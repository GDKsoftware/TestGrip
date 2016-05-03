unit FShowTestCode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GDCSynEdit, GDCSynMemo, GDCSynEditHighlighter, GDCSynHighlighterPas, uTestDefs,
  uBuildOutputParser, ExtCtrls, StdCtrls;

type
  TfrmShowTestcode = class(TForm)
    memoCode: TSynMemo;
    SynHighlighter: TSynPasSyn;
    pnlBottom: TPanel;
    memoHints: TMemo;
    procedure memoCodeSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FErrorLine: integer;
  public
    { Public declarations }
    function LoadTestCode( const pInputTest: TInputTest; const sUnit: string): string; overload;
    function LoadTestCode(const AInputTestFunction: TInputFunction; const AUnit: string): string; overload;

    procedure HandleBuildError(const oError: TBuildError);
  end;

implementation

uses uTestGen, StrUtils, uTestGripInstall;

{$R *.dfm}

{ TfrmShowTestcode }

procedure TfrmShowTestcode.FormCreate(Sender: TObject);
begin
  FErrorLine := -1;
end;

procedure TfrmShowTestcode.HandleBuildError(const oError: TBuildError);
var
  sFile: string;
begin
  sFile := oError.BuildUnitname;
  if not ContainsText(oError.BuildUnitname, ':') then
  begin
    // relative path
    sFile := IncludeTrailingPathDelimiter(oError.FilePath) + oError.BuildUnitname;
  end;
  memoCode.Lines.LoadFromFile(sFile);

  FErrorLine := oError.Line;
  memoCode.GotoLineAndCenter(oError.Line);

  memoHints.Clear;
  memoHints.Lines.Add(sFile);
  memoHints.Lines.Add(oError.ErrorCode + ': ' + oError.ErrorText);

  pnlBottom.Visible := True;
end;

function TfrmShowTestcode.LoadTestCode(const AInputTestFunction: TInputFunction; const AUnit: string): string;
var
  sTestCode : string;
  objTest   : TTestGen;
begin
  objTest := TTestGen.Create();
  try
    objTest.UseTestFrameWork := GetUseTestFramework;

    sTestCode := objTest.GetTestCode(AUnit, AInputTestFunction.Parent, AInputTestFunction);
  finally
    objTest.Free;
  end;

  memoCode.Text := sTestCode;

  Result := sTestCode;
end;

function TfrmShowTestcode.LoadTestCode(
  const pInputTest: TInputTest; const sUnit: string): string;
var
  sTestCode : string;
  objTest   : TTestGen;
begin
  objTest := TTestGen.Create();
  try
    objTest.UseTestFrameWork := GetUseTestFramework;

    sTestCode := objTest.GetTestCode( sUnit, pInputTest.Parent.Parent, pInputTest.Parent );
  finally
    objTest.Free;
  end;

  memoCode.Text := sTestCode;

  Result := sTestCode;
end;

procedure TfrmShowTestcode.memoCodeSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if (FErrorLine = Line) then
  begin
    Special := True;
    BG := clRed;
  end;
end;

end.
