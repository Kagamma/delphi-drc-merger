unit UnDrcMerger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8Classes, FileUtil, lconvencoding, gvector;

type
  TTokenType = (
      tkEOF, tkSharp, tkNumber, tkIdent, tkString, tkComment, tkUnknown,
      tkBegin, tkEnd);

  TDRCItem = record
    ID: cardinal;
    Key,
    Value: unicodestring;
  end;

  TDRCItemList = specialize TVector<TDRCItem>;

  TDRCMerger = class
  private
    FContent: TDRCItemList;
    FRawDRC: unicodestring;
    FCurPos: integer;
    FLog: TStrings;

    function NextChar: widechar;  
    function PeekAtNextChar: widechar;
    function NextToken(out AOutput: unicodestring): TTokenType;  
    function PeekAtNextToken(out AOutput: unicodestring): TTokenType;
    procedure Parse;
  public
    constructor Create;
    destructor Destroy; override; 

    procedure LoadFromFile(const AFileName: string);
    procedure MergeFrom(const AMerger: TDRCMerger);
    procedure SaveToFile(const AFileName: string);
    function FindByKey(const AKey: unicodestring): integer;

    property Content: TDRCItemList read FContent;
    property Log: TStrings read FLog;
  end;

implementation

function TDRCMerger.FindByKey(const AKey: unicodestring): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FContent.Size-1 do
  begin
    if FContent.Items[i].Key = AKey then
      exit(i);
  end;
end;

function TDRCMerger.NextChar: widechar;
begin
  Result := #0;
  Inc(FCurPos);
  if FCurPos > Length(FRawDRC) then
    exit;
  Result := FRawDRC[FCurPos];
end;

function TDRCMerger.PeekAtNextChar: widechar;
begin
  Result := NextChar;
  Dec(FCurPos);
end;

function TDRCMerger.NextToken(out AOutput: unicodestring): TTokenType;
var
  c: widechar;
begin
  AOutput := '';
  repeat
    c := NextChar;
  until not (c in [#1..#32, ',']);
  case c of
    #0:
      Result := tkEOF;
    '#':
      Result := tkSharp;
    '0'..'9':
      begin
        Result := tkNumber; 
        Dec(FCurPos);
        repeat
          c := NextChar;
          AOutput := AOutput + c;
        until not (PeekAtNextChar in ['0'..'9']);
      end;
    'A'..'Z', 'a'..'z', '_':
      begin
        Result := tkIdent;
        Dec(FCurPos);
        repeat
          c := NextChar;
          AOutput := AOutput + c;
        until not (PeekAtNextChar in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
        if UpCase(AOutput) = 'BEGIN' then
          Result := tkBegin
        else 
        if UpCase(AOutput) = 'END' then
          Result := tkEnd;
      end;
    '/':
      begin
        if PeekAtNextChar in ['/'] then
        begin
          Result := tkComment;
          repeat
            c := NextChar;
          until c in [#0, #10, #13];
        end
        else
        if PeekAtNextChar in ['*'] then
        begin
          Result := tkComment;
          repeat
            c := NextChar;
          until ((c in ['*']) and (PeekAtNextChar in ['/'])) or (c in [#0]);
          NextChar;
        end
      end;
    '"':
      begin
        Result := tkString;
        while true do
        begin
          c := NextChar;
          if c in ['\'] then
          begin
            AOutput := AOutput + c;
            if not (PeekAtNextChar in [#0]) then
              AOutput := AOutput + NextChar;
          end
          else
          if not (c in ['"']) then
          begin
            AOutput := AOutput + c;
          end
          else
            break;
        end;
      end;
    else
      Result := tkUnknown;
  end;
end;

function TDRCMerger.PeekAtNextToken(out AOutput: unicodestring): TTokenType;
var
  OldPos: integer;
begin
  OldPos := FCurPos;
  Result := NextToken(AOutput);
  FCurPos := OldPos;
end;

procedure TDRCMerger.Parse;
  procedure ParseHeader;
  var
    Item: TDRCItem;
    TokenType: TTokenType;
    Data: unicodestring;
  begin
    while true do
    begin
      TokenType := NextToken(Data);
      case TokenType of
        tkSharp:
          begin
            if PeekAtNextToken(Data) = tkIdent then
            begin
              NextToken(Data);
              Item.Key := Data;
              if PeekAtNextToken(Data) = tkNumber then
              begin
                NextToken(Data);
                Item.ID := StrToInt(UTF8Encode(Data));
                FContent.PushBack(Item);
              end;
            end;
          end;
        tkBegin:
          exit;
      end;
    end;
  end;

  procedure ParseBody;
  var
    i: integer;
    Key: unicodestring;
    TokenType: TTokenType;
    Data: unicodestring;
    Item: TDRCItem;
  begin
    while true do
    begin
      TokenType := NextToken(Data);
      case TokenType of
        tkIdent:
          begin
            Key := Data;
            if PeekAtNextToken(Data) = tkString then
            begin
              NextToken(Data);
              i := FindByKey(Key);
              if i >= 0 then
              begin
                Item := FContent.Items[i];
                Item.Value := Data;
                FContent.Items[i] := Item;
              end;
            end;
          end;
        tkEnd:
          exit;
      end;
    end;
  end;

begin
  ParseHeader;
  ParseBody;
end;

constructor TDRCMerger.Create;
begin
  inherited;
  FContent := TDRCItemList.Create;  
  FLog := TStringList.Create;
end;

destructor TDRCMerger.Destroy;
begin
  FreeAndNil(FContent);   
  FreeAndNil(FLog);
  inherited;
end;

procedure TDRCMerger.LoadFromFile(const AFileName: string);
var
  FS: TFileStreamUTF8;
  SS: TStringStream;
begin
  FS := TFileStreamUTF8.Create(AFileName, fmOpenRead);
  SS := TStringStream.Create('');
  try
    FS.Position := 0;
    SS.CopyFrom(FS, FS.Size);
    SS.Position := 0;
    FRawDRC := UTF8Decode(CP932ToUTF8(SS.DataString));
    FCurPos := 0;
    Parse;
  finally
    FreeAndNil(FS);
    FreeAndNil(SS);
  end;
end;

procedure TDRCMerger.MergeFrom(const AMerger: TDRCMerger);
var
  i, j: integer;
  FromContent: TDRCItemList;
  Key: unicodestring;
  Item: TDRCItem;
begin
  FromContent := AMerger.Content;
  for i := 0 to FContent.Size-1 do
  begin
    Key := FContent.Items[i].Key;
    j := AMerger.FindByKey(Key);
    if j >= 0 then
    begin
      Item := FContent.Items[i];
      Item.Value := FromContent.Items[j].Value;
      FContent.Items[i] := Item;
    end;
  end;
end;

procedure TDRCMerger.SaveToFile(const AFileName: string);
var
  Header,
  Body: unicodestring;
  SS: TStringStream;
  FS: TFileStreamUTF8;
  i: integer;
begin
  FS := TFileStreamUTF8.Create(AFileName, fmCreate);
  SS := TStringStream.Create('');
  try
    Header := '';
    Body := '';
    for i := 0 to FContent.Size - 1 do
    begin
      Header :=
          Header + '#' + FContent.Items[i].Key + #8 +
          UTF8Decode(IntToStr(FContent.Items[i].ID)) + #10#13;
      Body :=
          Body + #8 + FContent.Items[i].Key + ',' + #8 +
          FContent.Items[i].Value + #10#13;
    end;
    SS.WriteString(
        UTF8ToCP932(UTF8Encode(Header + 'BEGIN' + #10#13 + Body + 'END')));
    SS.Position := 0;
    FS.CopyFrom(SS, SS.Size);
  finally
    FreeAndNil(FS);
    FreeAndNil(SS);
  end;
end;

end.

