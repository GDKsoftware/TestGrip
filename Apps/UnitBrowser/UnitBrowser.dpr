program UnitBrowser;

uses
  Forms,
  FUnitBrowserMain in 'FUnitBrowserMain.pas' {FrmUnitBrowserMain},
  uPascalDefs in '..\..\Core\uPascalDefs.pas',
  uUnitParser in '..\..\Core\uUnitParser.pas',
  uCommonFunctions in '..\..\Shared\uCommonFunctions.pas',
  uD7Functions in '..\..\Shared\uD7Functions.pas',
  uUTF8Functions in '..\..\Shared\uUTF8Functions.pas',
  uXmlFuncs in '..\..\Shared\uXmlFuncs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmUnitBrowserMain, FrmUnitBrowserMain);
  Application.Run;
end.
