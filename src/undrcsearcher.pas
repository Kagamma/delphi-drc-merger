unit UnDrcSearcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gvector;

type
  TDRCSearcherItem = record
    FromFile,
    ToFile: string;
  end;

  TDRCSearcherItemList = specialize TVector<TDRCSearcherItem>;

  TDRCSearcher = class
  private
    FSrcPath,
    FDstPath: string;
    FLog: TStrings;
    FContent: TDRCSearcherItemList;
  public
    constructor Create(const ASrcPath, ADstPath: string);
    destructor Destroy; override;
    procedure Search;

    property Content: TDRCSearcherItemList read FContent;
    property Log: TStrings read FLog;
  end;

implementation

constructor TDRCSearcher.Create(const ASrcPath, ADstPath: string);
begin
  inherited Create;
  FContent := TDRCSearcherItemList.Create;
  FLog := TStringList.Create;
  FSrcPath := ASrcPath;
  FDstPath := ADstPath;
end;

destructor TDRCSearcher.Destroy;
begin
  FreeAndNil(FContent);
  FreeAndNil(FLog);
  inherited;
end;

procedure TDRCSearcher.Search;
begin

end;

end.

