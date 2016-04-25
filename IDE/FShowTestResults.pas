unit FShowTestResults;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, StdCtrls, uTestDefs, uProjectGen, ExtCtrls,
  uBuildOutputParser;

type
  TfrmShowTestresults = class(TForm)
    pgMain: TPageControl;
    tbCompileErrors: TTabSheet;
    tbTestErrors: TTabSheet;
    mmCompilation: TMemo;
    mmTestresult: TMemo;
    mmCommand: TMemo;
    tbRunlog: TTabSheet;
    mmRunlog: TMemo;
    pnlButtons: TPanel;
    btnShowInCode: TButton;
    procedure btnShowInCodeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pgMainChange(Sender: TObject);
  private
    { Private declarations }
    FBuildError: TBuildOutputParser;
    FRunError: TRunErrorParser;

    FPath: string;
  public
    { Public declarations }
    property Path: string
      read FPath write FPath;

    procedure InitWithTest( const t: TInputTest );
    procedure InitWithFunc( const f: TInputFunction );
  end;


implementation

uses
  FShowTestCode;

{$R *.dfm}

{ TfrmShowTestresults }

procedure TfrmShowTestresults.btnShowInCodeClick(Sender: TObject);
var
  frm: TfrmShowTestcode;
  oError: TBuildError;
begin
  oError := nil;

  if FBuildError.ErrorCode <> '' then
  begin
    oError := FBuildError;
  end
  else if FRunError.ErrorCode <> '' then
  begin
    oError := FRunError;
  end;

  if Assigned(oError) then
  begin
    frm := TfrmShowTestcode.Create(nil);
    try
      frm.PopupParent := Self;
      frm.PopupMode := pmExplicit;

      frm.HandleBuildError(oError);

      frm.ShowModal;
    finally
      frm.Free;
    end;
  end;
end;

procedure TfrmShowTestresults.FormCreate(Sender: TObject);
begin
  FBuildError := TBuildOutputParser.Create;
  FRunError := TRunErrorParser.Create;
end;

procedure TfrmShowTestresults.InitWithFunc(const f: TInputFunction);
begin
  mmCommand.Text     := f.CompileCommand.Text;
  mmCompilation.Text := f.CompileResult.Text;

  mmTestresult.Text  := f.TotalTestResult.Text;
  mmRunlog.Text      := f.RunLog.Text;

  FBuildError.Parse(f.CompileResult.Text, FPath);
  FRunError.Parse(f.RunLog.Text, FPath);

  pnlButtons.Visible := (FBuildError.ErrorCode <> '') or (FRunError.ErrorCode <> '');

  if mmTestresult.Text <> '' then
  begin
    pgMain.ActivePage := tbTestErrors;
  end
  else
  begin
    pgMain.ActivePage := tbCompileErrors;
  end;
end;

procedure TfrmShowTestresults.InitWithTest(const t: TInputTest);
begin
  mmTestresult.Text := t.TestResultString;

  if t.TestResultString <> '' then
  begin
    pgMain.ActivePage := tbTestErrors;
  end
  else
  begin
    pgMain.ActivePage := tbCompileErrors;
  end;
end;

procedure TfrmShowTestresults.pgMainChange(Sender: TObject);
begin
  btnShowInCode.Enabled :=
    ((pgMain.ActivePage = tbCompileErrors) and (FBuildError.ErrorCode <> '')) or
    ((pgMain.ActivePage = tbTestErrors) and (FRunError.ErrorCode <> ''));
end;

end.
