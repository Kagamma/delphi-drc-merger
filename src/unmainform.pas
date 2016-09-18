unit UnMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    btStart: TButton;
    edSrcPath: TEdit;
    edDstPath: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    mmOutput: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btStartClick(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  UnDrcMerger, UnDrcSearcher;

{ TMainForm }

procedure TMainForm.btStartClick(Sender: TObject);
var
  Searcher: TDRCSearcher;
  FromMerger,
  ToMerger: TDRCMerger;
  FromFile,
  ToFile: string;
begin
  Searcher := TDRCSearcher.Create(edSrcPath.Text, edDstPath.Text);
  try
    Searcher.Search;

    begin
      FromMerger := TDRCMerger.Create;
      ToMerger := TDRCMerger.Create;
      try
        FromFile := edSrcPath.Text;     
        ToFile := edDstPath.Text;

        FromMerger.LoadFromFile(FromFile);    
        ToMerger.LoadFromFile(ToFile);

        ToMerger.MergeFrom(FromMerger);

        ToMerger.SaveToFile(ToFile);
      finally
        FreeAndNil(FromMerger);  
        FreeAndNil(ToMerger);
      end;
    end;
  finally
    FreeAndNil(Searcher);
  end;
end;

end.

