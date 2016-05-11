program UnitSearch;

uses
  Forms,
  FUnitSearchMain in 'FUnitSearchMain.pas' {FrmUnitSearchMain},
  uCodeHelpers in '..\..\Core\uCodeHelpers.pas',
  uDefinitionSearch in '..\..\Core\uDefinitionSearch.pas',
  uPascalDefs in '..\..\Core\uPascalDefs.pas',
  uProjectParser in '..\..\Core\uProjectParser.pas',
  uUnitParser in '..\..\Core\uUnitParser.pas',
  uUsedUnitSearch in '..\..\Core\uUsedUnitSearch.pas',
  uCommonFunctions in '..\..\Shared\uCommonFunctions.pas',
  uD7Functions in '..\..\Shared\uD7Functions.pas',
  uUTF8Functions in '..\..\Shared\uUTF8Functions.pas',
  uXmlFuncs in '..\..\Shared\uXmlFuncs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmUnitSearchMain, FrmUnitSearchMain);
  Application.Run;
end.
