program TestIDE;

uses
  Vcl.Forms,
  FIDEMain in 'FIDEMain.pas' {frmIDEMain},
  DockForm in 'DockForm.pas',
  ToolsAPI in 'ToolsAPI.pas',
  Deskutil in 'Deskutil.pas',
  FAboutTestgrip in '..\..\IDE\FAboutTestgrip.pas' {FrmAboutTestgrip},
  FDetailsEdit in '..\..\IDE\FDetailsEdit.pas' {frmDetailsEdit},
  FInputTest in '..\..\IDE\FInputTest.pas',
  FMultitestEdit in '..\..\IDE\FMultitestEdit.pas' {frmTestsMatrixEdit},
  FParamEditor in '..\..\IDE\FParamEditor.pas' {frmParamEditor},
  FPropertyEditor in '..\..\IDE\FPropertyEditor.pas' {GDC_frmPropertyEditor},
  FRename in '..\..\IDE\FRename.pas' {frmRename},
  FShowTestCode in '..\..\IDE\FShowTestCode.pas',
  FShowTestResults in '..\..\IDE\FShowTestResults.pas' {frmShowTestresults},
  FWarnings in '..\..\IDE\FWarnings.pas' {frmWarnings},
  uHelp in '..\..\IDE\uHelp.pas',
  uIdeEditorManager in '..\..\IDE\uIdeEditorManager.pas',
  uIDEGarbageCollector in '..\..\IDE\uIDEGarbageCollector.pas',
  uIdeHelper in '..\..\IDE\uIdeHelper.pas',
  uTestgripUIFuncs in '..\..\IDE\uTestgripUIFuncs.pas',
  uCodeHelpers in '..\..\Core\uCodeHelpers.pas',
  uDefinitionSearch in '..\..\Core\uDefinitionSearch.pas',
  uPascalDefs in '..\..\Core\uPascalDefs.pas',
  uProjectParser in '..\..\Core\uProjectParser.pas',
  uUnitParser in '..\..\Core\uUnitParser.pas',
  uUsedUnitSearch in '..\..\Core\uUsedUnitSearch.pas',
  uConst in '..\..\Definition\uConst.pas',
  uInputParser in '..\..\Definition\uInputParser.pas',
  uTestDefs in '..\..\Definition\uTestDefs.pas',
  uCommonFunctions in '..\..\Shared\uCommonFunctions.pas',
  uD7Functions in '..\..\Shared\uD7Functions.pas',
  uUTF8Functions in '..\..\Shared\uUTF8Functions.pas',
  uXmlFuncs in '..\..\Shared\uXmlFuncs.pas',
  uDelphiRegistry in '..\..\Generation\uDelphiRegistry.pas',
  uDUnitTestGen in '..\..\Generation\uDUnitTestGen.pas',
  uDUnitXTestGen in '..\..\Generation\uDUnitXTestGen.pas',
  uInheritGen in '..\..\Generation\uInheritGen.pas',
  uPascalFileGen in '..\..\Generation\uPascalFileGen.pas',
  uProjectGen in '..\..\Generation\uProjectGen.pas',
  uProjectTesting in '..\..\Generation\uProjectTesting.pas',
  uTestGen in '..\..\Generation\uTestGen.pas',
  uTestgripInstall in '..\..\Generation\uTestgripInstall.pas',
  uUnitTestGenBase in '..\..\Generation\uUnitTestGenBase.pas',
  uUnitTestGenIntf in '..\..\Generation\uUnitTestGenIntf.pas',
  uBuildOutputParser in '..\..\Execution\uBuildOutputParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmIDEMain, frmIDEMain);
  Application.Run;
end.
