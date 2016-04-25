program impgen;

uses
  Vcl.Forms,
  fmain in 'fmain.pas' {frmMain},
  uCodeHelpers in '..\..\Core\uCodeHelpers.pas',
  uDefinitionSearch in '..\..\Core\uDefinitionSearch.pas',
  uPascalDefs in '..\..\Core\uPascalDefs.pas',
  uProjectParser in '..\..\Core\uProjectParser.pas',
  uUnitParser in '..\..\Core\uUnitParser.pas',
  uUsedUnitSearch in '..\..\Core\uUsedUnitSearch.pas',
  uCommonFunctions in '..\..\Shared\uCommonFunctions.pas',
  uD7Functions in '..\..\Shared\uD7Functions.pas',
  uUTF8Functions in '..\..\Shared\uUTF8Functions.pas',
  uXmlFuncs in '..\..\Shared\uXmlFuncs.pas',
  uInheritGen in '..\..\Generation\uInheritGen.pas',
  uPascalFileGen in '..\..\Generation\uPascalFileGen.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
