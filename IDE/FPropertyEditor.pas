unit FPropertyEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DockForm, StdCtrls, Types,
  Menus, Grids, DBGrids, ComCtrls, ExtCtrls, uIdeEditorManager,uPascalDefs, uTestDefs,
  ActnList, ActnMan, ToolWin, ActnCtrls,
  ActnMenus, ImgList, XPStyleActnCtrls, GDCVirtualTrees, Buttons, ToolsApi,uTestGen, FMultitestEdit, XPMan,
  FShowTestResults, FInputTest, Contnrs, uProjectParser, uBuildOutputParser, uUnitParser,
  uDefinitionSearch;

var
  NotifierIndex: Integer;

type
  ENotInImplementation = Exception;

  TKeyboardBinding = Class(TNotifierObject, IOTAKeyboardBinding)
  Private
    Procedure AddBreakpoint(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
  Protected
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  Protected
  Public
  End;


  rTreeData = record
    Name: string;
    Level: integer;
    Data: Pointer;
    ResultText: string; // Only if level = 0
  end;
  PTreeData = ^rTreeData;

  TGDC_frmPropertyEditor = class(TDockableForm)
    pnlTop: TPanel;
    pnlMain: TPanel;
    Timer1: TTimer;
    ActionManager1: TActionManager;
    acDeleteTest: TAction;
    acRunTest: TAction;
    ToolBar1: TToolBar;
    tbRun: TToolButton;
    acAddTest: TAction;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    Runcurrentfunction1: TMenuItem;
    Runalltests1: TMenuItem;
    lblInfo: TLabel;
    acOpenTestProject: TAction;
    pnlBottom: TPanel;
    ProgressBar1: TProgressBar;
    pnlResult: TPanel;
    lblResult: TLabel;
    lblProgress: TLabel;
    Image1: TImage;
    acClean: TAction;
    tbAddTest: TToolButton;
    tbRemoveTest: TToolButton;
    tbDebug: TToolButton;
    tbClean: TToolButton;
    XPManifest1: TXPManifest;
    Panel1: TPanel;
    tmrInit: TTimer;
    popFunction: TPopupMenu;
    popTest: TPopupMenu;
    popClass: TPopupMenu;
    miPopFunction_AddTest: TMenuItem;
    miPopFunction_Default: TMenuItem;
    Deletealltests1: TMenuItem;
    miPopTest_Default: TMenuItem;
    miPopTest_RunTest: TMenuItem;
    miPopFunction_RunAllTests: TMenuItem;
    Runalltests3: TMenuItem;
    miPopTest_DeleteTest: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    miPopTest_ShowLatestTestResult: TMenuItem;
    miPopTest_CopyTest: TMenuItem;
    acRunAllTests: TAction;
    miRenameFunction: TMenuItem;
    GCTimer: TTimer;
    miJumpToFunction: TMenuItem;
    miPopTest_JumpToFunction: TMenuItem;
    miShowlatesttestresultFunc: TMenuItem;
    tbWarnings: TToolButton;
    acGenerateOverrides: TAction;
    tbGenOver: TToolButton;
    miGenerateCodeForClass: TMenuItem;
    miGenerateCodeForFunction: TMenuItem;
    N3: TMenuItem;
    procedure btnLoadTestFileClick(Sender: TObject);
    procedure btnCreateUnitTestClick(Sender: TObject);
    procedure lblAddTestClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure acDeleteTestExecute(Sender: TObject);
    procedure acRunTestExecute(Sender: TObject);
    procedure acAddTestExecute(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure acOpenTestProjectExecute(Sender: TObject);
    procedure Runcurrentfunction1Click(Sender: TObject);
    procedure Runalltests1Click(Sender: TObject);
    procedure lblResultClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure acTestMatrixEditExecute(Sender: TObject);
    procedure acCleanExecute(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
    procedure tmrInitTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miPopFunction_DefaultClick(Sender: TObject);
    procedure miPopTest_DefaultClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miPopTest_ShowLatestTestResultClick(Sender: TObject);
    procedure acRunAllTestsExecute(Sender: TObject);
    procedure miRenameFunctionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miPopTest_CopyTestClick(Sender: TObject);
    procedure GCTimerTimer(Sender: TObject);
    procedure miJumpToFunctionClick(Sender: TObject);
    procedure miPopTest_JumpToFunctionClick(Sender: TObject);
    procedure miShowlatesttestresultFuncClick(Sender: TObject);
    procedure acGenerateOverridesExecute(Sender: TObject);
    procedure miGenerateCodeForFunctionClick(Sender: TObject);
  private
    CurrentFile: string;
    FErrorResult: string;
    Treeview1: TVirtualStringTree;
    frmShowTestResults: TfrmShowTestresults;
    miToggleExceptionHandling: TMenuItem;
    miToggleUseDUnit: TMenuItem;
    miToggleUseDUnitX: TMenuItem;

    FDefinitionSearch: TDefinitionSearch;

    procedure Treeview1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure Treeview1PaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);

    // Versie verschillen in D2007 en XE componenten
{$ifdef VER180}
    procedure Treeview1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure Treeview1GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
{$else}
  {$ifdef VER150}
    procedure Treeview1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure Treeview1GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
  {$else}
    procedure Treeview1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure Treeview1GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
  {$endif}
{$endif}

    procedure SetFontColorBasedOnTestResult(const ACanvas: TCanvas; const ATestResult: Boolean);

    procedure InitProgressBar;
    procedure DisableProgressBar;
    procedure ShowCompileProgress(CompilerText: string; Position: integer; Max: integer);
    procedure SetTestResult(ShowError: boolean; ErrorText: string='');


    procedure UpdateTreeView(PascalFile: TPascalFile);
    procedure OpenSelectedNodeTest;
    procedure OpenInputForm(InputTest: TInputTest=nil);

    procedure RunAllTests;
    function RunTests(RunOnlyFocusedNode: boolean; DeleteTestProject: boolean=true; bDontActuallyRunTest: boolean = false): string;
    function RunFunctionTest(const ProjectFileName: string; const oTestGen: TTestGen; const oFunctionTest: TInputFunction; DeleteTestProject: boolean; bDontActuallyRunTest: boolean; const sUnitFilename: string):boolean;
    procedure AddTest;

    function GetActiveFunctionOrProcedure: TMethodDefinition;
    function IsFileModfied: boolean;

    function GetCurrentTestFilename: string;
    function GetProjectFileName(const aProjectParser: TProjectParser): string;

    procedure SyncIdeManager;

    procedure FindAndSelectFunctionInTree( const sClass: string; const sFunction: string );
    procedure DeleteSelectedTest;
    procedure SetLabelInfo(aText: string);

    procedure CleanTestProjects;

    procedure SelectNodeInTree(aNode: PVirtualNode); overload;
    procedure SelectNodeInTree(aTest: TInputTest); overload;

    procedure ClearCurrentFolder;

    class procedure miShowTestgripClick(Sender: TObject);
    procedure miAboutTestgripClick(Sender: TObject);
    procedure miFaqClick(Sender: TObject);
    procedure miOpenDemoProjectClick(Sender: TObject);
    procedure miToggleExceptionHandlingClick(Sender: TObject);
    procedure miToggleUseDUnitClick(Sender: TObject);
    procedure miToggleUseDUnitXClick(Sender: TObject);

    procedure miHelpClick(Sender: TObject);
    procedure RemoveAndFreeTest(const AInputTestForm: TfrmInputTest);
    procedure AddTestNodesToFunctionNode(var NodeData: PTreeData; InputTestFunction: TInputFunction; NodeFunctions: PVirtualNode);
    procedure AddTestFunctionNodesToClassNode(t: TInputFunction; var NodeData: PTreeData; InputTestClass: TInputTestClass; NodeClass: PVirtualNode);
    procedure ShowBuildError;
    procedure InitDefinitionSearch(sProjectFile: string);
    procedure AddTestgripMenusToIDE(const AINTAServices: INTAServices);
    procedure RemoveTestgripMenusFromIDE(const AINTAServices: INTAServices);
    procedure SetInfoForMethod(const AMethodDefinition: TMethodDefinition);
    function FindClosestMethodDefinitionForMethodSignature(const ASignature: string; oUnitParser: TUnitParser): TMethodDefinition;
    procedure EditTestsWithMatrixEdit(APascalFile: TPascalFile; AInputFunction: TInputFunction; AUnitParser: TUnitParser; AMethodDefinition: TMethodDefinition; AInputTestClass: TInputTestClass);
  protected
    IdeEditorManager: TIdeEditorManager;
    miShowTestgrip: TMenuItem;

    FLastTestPath: string;
    FBuildOutputParser: TBuildOutputParser;
    FRunErrorParser: TRunErrorParser;

    function GetShortTestResultString(const aTest: TInputTest): string;

    procedure OnSaveExistingTest(Sender: TObject);
    procedure OnSaveNewTest(Sender: TObject);

    function GetSelectedTestFunction: TInputFunction;
    function GetMethodDefinitionFromExistingFunction(const aTestFunc: TInputFunction): TMethodDefinition;

    procedure ListAllFilesInIDE(const AProject: IOTAProject; const AFiles: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CallBackProgress( const Sender: TTestGen; currentstep: integer; maxsteps: integer; const sShortDesc: string );
  end;


procedure PropertyEditorComponentsRegister;
procedure PropertyEditorComponentsUnregister;

procedure Register;

var
  GDC_frmPropertyEditor: TGDC_frmPropertyEditor;


implementation

{$R *.dfm}

uses
  Deskutil, uInputParser,
  IniFiles, uCommonFunctions,
  TypInfo, StrUtils, Math, FAboutTestgrip, uProjectTesting, FRename, uTestgripUIFuncs,
  uD7Functions, uHelp, ShellApi, uTestgripInstall, uConst,
  uProjectGen, FShowTestCode, uIdeHelper, Clipbrd,
  uCodeHelpers, uIDEGarbageCollector;

const
  DpkFileExtension = '.dpk';
  DprFileExtension = '.dpr';
  DprojFileExtension = '.dproj';
  PasFileExtension = '.pas';
  TestgripFileExtension = '.test';
  TestgripPrefix = 'TESTGRIP_';

var
  iKeyBindingIndex : Integer = 0;
  GTrialErrorShown: boolean = false;

type
  TIdeNotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  protected
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
  end;

procedure PropertyEditorComponentsRegister;
var
  Services: IOTAServices;
begin
  // Create the form
  if GDC_frmPropertyEditor = nil then
  begin
    GDC_frmPropertyEditor := TGDC_frmPropertyEditor.Create(Application);
    if GDC_frmPropertyEditor.Floating then
    begin
      GDC_frmPropertyEditor.Show;
    end
    else
    begin
      GDC_frmPropertyEditor.ForceShow;
      FocusWindow(GDC_frmPropertyEditor);
    end;

    // Register to save position with the IDE
    RegisterDesktopFormClass(TGDC_frmPropertyEditor, 'GDC_IntegratedEditor', GDC_frmPropertyEditor.Name);
    if (@RegisterFieldAddress <> nil) then
      RegisterFieldAddress(GDC_frmPropertyEditor.Name, @GDC_frmPropertyEditor);
  end;

  // Register the IDE Notifier
  Services := BorlandIDEServices as IOTAServices;
  Assert(Assigned(Services), 'IOTAServices not available');
  NotifierIndex := Services.AddNotifier(TIdeNotifier.Create);

  iKeyBindingIndex := (BorlandIDEServices As IOTAKeyboardServices).AddKeyboardBinding(TKeyboardBinding.Create);
end;

procedure Register;
begin
  PropertyEditorComponentsRegister;
end;

procedure RemoveNotifier;
var
  Services: IOTAServices;
begin
  if NotifierIndex <> -1 then
  begin
    Services := BorlandIDEServices as IOTAServices;
    if assigned(Services) then
      Services.RemoveNotifier(NotifierIndex);
  end;
end;

function MsgServices: IOTAMessageServices;
begin
  Result := (BorlandIDEServices as IOTAMessageServices);
  Assert(Result <> nil, 'IOTAMessageServices not available');
end;


function GetCurrentProject: IOTAProject;
var
  ModServices: IOTAModuleServices;
  Module: IOTAModule;
  Project: IOTAProject;
  ProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;
  ModServices := BorlandIDEServices as IOTAModuleServices;
  for i := 0 to ModServices.ModuleCount - 1 do
  begin
    Module := ModServices.Modules[i];
    if Supports(Module, IOTAProjectGroup, ProjectGroup) then
    begin
      Result := ProjectGroup.ActiveProject;
      Exit;
    end
    else if Supports(Module, IOTAProject, Project) then
    begin // In the case of unbound packages, return the 1st
      if Result = nil then
        Result := Project;
    end;
  end;
  
  Assert(Result <> nil, 'IOTAProject not available');
end;

{ TIdeNotifier }

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
//  MsgServices.AddTitleMessage('After Compile');
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
//  MsgServices.AddTitleMessage('Before Compile');
end;

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
 // TODO met openen iets?
end;



{ TMyDockForm }

function TGDC_frmPropertyEditor.GetCurrentTestFilename: string;
begin
  Result := StringReplace(TIDEHelper.GetCurrentUnitName, PasFileExtension, TestgripFileExtension, [rfIgnoreCase]);
end;

function TGDC_frmPropertyEditor.GetMethodDefinitionFromExistingFunction(
  const aTestFunc: TInputFunction): TMethodDefinition;
var
  PascalFile: TPascalFile;
  sUnit: string;
begin
  sUnit := TIDEHelper.GetCurrentUnitName;
  if not IdeEditorManager.GetCurrentPascalFile(sUnit, PascalFile) then
  begin
    PascalFile := IdeEditorManager.AddManagedPascalFile(sUnit, true);
  end;

  if Assigned(aTestFunc.Parent) then
  begin
    Result := GetFirstMethodDefinitionFromSourceBySignature( PascalFile.PascalFileName, aTestFunc.GuessSignature, true );
  end
  else
  begin
    Result := GetFirstMethodDefinitionFromSourceBySignature( PascalFile.PascalFileName, aTestFunc.GuessSignature, true );
  end;
end;

function TGDC_frmPropertyEditor.GetProjectFileName(const aProjectParser: TProjectParser): string;
begin
  result := ExtractFileName(GetActiveProject.FileName);
  if ExtractFileExt(result) = DprojFileExtension then
  begin
    if Assigned(aProjectParser) then
    begin
      aProjectParser.ParseDProj(GetActiveProject.FileName, true);

      result := ExtractFileName(aProjectParser.EntryFile);
    end
    else
    begin
      if FileExists(IncludeTrailingPathDelimiter(ExtractFileDir(GetActiveProject.FileName)) +
        ChangeFileExt(ExtractFileName(result), DpkFileExtension)) then
      begin
        result := ChangeFileExt(result, DpkFileExtension);
      end
      else
      begin
        result := ChangeFileExt(result, DprFileExtension);
      end;
    end;
  end;
end;

function TGDC_frmPropertyEditor.GetSelectedTestFunction: TInputFunction;
var
  EditView: IOTAEditView;
  EditorServices: IOTAEditorServices;
  node: PTreeData;
begin
  Result := nil;

  EditorServices := BorlandIDEServices as IOTAEditorServices;
  EditView := EditorServices.TopView;

  if Assigned(EditView) then
  begin
    if assigned(Treeview1.FocusedNode) then
    begin
      Node := Treeview1.GetNodeData(Treeview1.FocusedNode);
      if assigned(Node) then
      begin
        if Node.Level = 1 then
        begin
          Result := TInputFunction(node.Data);
        end
        else if Node.Level = 2 then
        begin
          Result := TInputTest(node.Data).Parent;
        end;
      end;
    end;
  end;
end;

function TGDC_frmPropertyEditor.GetShortTestResultString(
  const aTest: TInputTest): string;
var
  i, c: integer;
  implier: TInputImplier;
begin
  Result := '';

  if (aTest.TestResult_EqualsWas <> '') or (aTest.TestResult_EqualsExpected <> '') then
  begin
    Result := 'was <' + aTest.TestResult_EqualsWas + '> instead of <' + aTest.TestResult_EqualsExpected + '>';
  end
  else
  begin

    c := aTest.Implies.Count - 1;
    for i := 0 to c do
    begin
      implier := TInputImplier(aTest.Implies[i]);

      if (implier.TestResultExpected <> '') or (implier.TestResultWas <> '') then
      begin
        Result := 'implier #' + IntToStr(i+1) + ' was <' + implier.TestResultWas + '> instead of <' + implier.TestResultExpected + '>';
        break;
      end;
    end;

  end;
end;

procedure TGDC_frmPropertyEditor.Image1Click(Sender: TObject);
begin
  pnlResult.Visible := false;
end;

procedure TGDC_frmPropertyEditor.InitProgressBar;
begin
  ProgressBar1.Position := 0;
  lblProgress.Caption := '';
  pnlBottom.Visible := true;
  Application.ProcessMessages;
end;

function TGDC_frmPropertyEditor.IsFileModfied: boolean;
var
  EditorServices: IOTAEditorServices;
  EditView: IOTAEditView;
begin
  Result := false;
   // Get access to editor services
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  // Get access to editor of active unit
  EditView := EditorServices.TopView;

  if Assigned(EditView) then
  begin
    result := EditView.Buffer.Modified
  end;
end;

procedure TGDC_frmPropertyEditor.acAddTestExecute(Sender: TObject);
begin
  if GetActiveProject <> nil then
  begin
    AddTest;
  end;
end;

procedure TGDC_frmPropertyEditor.acCleanExecute(Sender: TObject);
begin
  CleanTestProjects;
end;

procedure TGDC_frmPropertyEditor.acDeleteTestExecute(Sender: TObject);
begin
  if GetActiveProject <> nil then
  begin
    DeleteSelectedTest;
  end;
end;

procedure TGDC_frmPropertyEditor.acGenerateOverridesExecute(Sender: TObject);
var
  sProjectFile: string;
  sSearchFor: string;
  sCode: string;
  sCurrentUnitname: string;
begin
  // preparation to get the right projectfile and search starting
  sProjectFile := GetActiveProject.FileName;

  InitDefinitionSearch(sProjectFile);

  // find out select class/interface name
  sSearchFor := TIDEHelper.GetWordUnderCaret;

  sCurrentUnitname := TIDEHelper.GetCurrentUnitName;

  // to clipboard
  sCode := TCodeHelperA.GetInterfaceMethodDefinitions(FDefinitionSearch, sCurrentUnitname, sSearchFor);
  clipboard.AsText := sCode;

  if sCode <> '' then
  begin
    TIDEHelper.AddDelphiMessage('Copied interface methods for ' + sSearchFor + ' to clipboard');
  end
  else
  begin
    TIDEHelper.AddDelphiMessage('Interface methods for ' + sSearchFor + ' not found');
  end;
end;

procedure TGDC_frmPropertyEditor.acOpenTestProjectExecute(Sender: TObject);
var
  ProjectFile: string;
begin
  ProjectFile := RunTests(true, false, true);
  if ProjectFile <> '' then
    TCommonExecutionFunctions.ExecuteFile(ExtractFileName(ProjectFile),ExtractFileDir(ProjectFile))
  else
    ShowMessage('Cannot compile test-project!');
end;

procedure TGDC_frmPropertyEditor.acRunAllTestsExecute(Sender: TObject);
var
  proj: IOTAProject;
  lstFiles: TStrings;
  i, c: integer;
  j, d: integer;
  k, e: integer;
  s: string;
  sTestFile: string;
  PascalFile: TPascalFile;
  sTestResult: string;
  sProjectFile: string;
  aProjTester: TProjectTesting;
  aProjParser: TProjectParser;
begin
  proj := GetCurrentProject;
  aProjParser := TProjectParser.Create;
  try
    sProjectFile := IncludeTrailingPathDelimiter(ExtractFileDir(GetActiveProject.FileName)) + GetProjectFileName(aProjParser);
  finally
    aProjParser.Free;
  end;

  lstFiles := TStringList.Create;
  try
    ListAllFilesInIDE(proj, lstFiles);

    i := 0;
    c := lstFiles.Count;
    while i < c do
    begin
      s := lstFiles[i];
      if SameText( ExtractFileExt(s), PasFileExtension) then
      begin
        sTestFile := Copy(s,1,Length(s)-4) + TestgripFileExtension;
        if FileExists(sTestFile) then
        begin
          lstFiles[i] := s;

          if not IdeEditorManager.GetCurrentPascalFile(s, PascalFile) then
          begin
            PascalFile := IdeEditorManager.AddManagedPascalFile(s, True);
          end;
        end
        else
        begin
          lstFiles.Delete(i);
          Dec(i);
        end;
      end
      else
      begin
        lstFiles.Delete(i);
        Dec(i);
      end;

      c := lstFiles.Count;
      Inc(i);
    end;


    aProjTester := TProjectTesting.Create;
    try
      c := IdeEditorManager.PascalFileList.Count - 1;
      for i := 0 to c do
      begin
        PascalFile := TPascalFile(IdeEditorManager.PascalFileList[i]);

        TIDEHelper.AddDelphiMessage('Testing ' + ExtractFileName(PascalFile.PascalFileName) + '...');

        d := PascalFile.TestClassList.Count - 1;
        for j := 0 to d do
        begin
          sTestResult := aProjTester.RunAllTestsInClass( PascalFile.TestClassList[j], PascalFile.PascalFileName, sProjectFile, CallbackProgress );

          e := aProjTester.ShortListOfFailures.Count - 1;
          for k := 0 to e do
          begin
            TIDEHelper.AddDelphiMessage( aProjTester.ShortListOfFailures[k] );
          end;
        end;
      end;
    finally
      aProjTester.Free;
    end;

  finally
    lstFiles.Free;
  end;
end;

procedure TGDC_frmPropertyEditor.acRunTestExecute(Sender: TObject);
begin
  if GetActiveProject <> nil then
  begin
    if IsFileModfied then
      if (MessageDlg('Current file is modified. Run anyway?', mtWarning, [mbYes, mbNo], 0) <> mrYes) then
        Exit;

    RunTests(true);
  end;
end;

procedure TGDC_frmPropertyEditor.acTestMatrixEditExecute(Sender: TObject);
var
  mDef: TMethodDefinition;
  PascalFile: TPascalFile;
  InputTestClass: TInputTestClass;
  oUnitParser: TUnitParser;
  sUnit: string;
  InputTestFunction: TInputFunction;
begin
  try
    mDef := GetActiveFunctionOrProcedure;
    if assigned(mDef) then
    begin
      try
        // Check if pascalfile exists in the manager
        sUnit := TIDEHelper.GetCurrentUnitName;

        PascalFile := IdeEditorManager.GetOrAddPascalFile(sUnit);

        // Find the current class
        InputTestClass := PascalFile.GetOrAddInputTestClass(mDef.InClass);

        // Find current function
        InputTestFunction := InputTestClass.GetOrAddInputFunction(mdef);

        oUnitParser := TUnitParser.Create;
        try
          oUnitParser.ParseFromFile(sUnit, True);

          EditTestsWithMatrixEdit(PascalFile, InputTestFunction, oUnitParser, mDef, InputTestClass);
        finally
          oUnitParser.Free;
        end;
      finally
        mdef.Free;
      end;
    end;

  except
    on E: Exception do
    begin
      // ??
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.btnCleanClick(Sender: TObject);
begin
  ClearCurrentFolder;
end;

procedure TGDC_frmPropertyEditor.btnCreateUnitTestClick(Sender: TObject);
begin
  IdeEditorManager.AddManagedPascalFile(TIDEHelper.GetCurrentUnitName, true);
end;


procedure TGDC_frmPropertyEditor.btnLoadTestFileClick(Sender: TObject);
begin
  SyncIdeManager;
end;

procedure TGDC_frmPropertyEditor.CallBackProgress(const Sender: TTestGen;
  currentstep, maxsteps: integer; const sShortDesc: string);
begin
  ShowCompileProgress(sShortDesc,currentstep,maxsteps);
end;

procedure TGDC_frmPropertyEditor.CleanTestProjects;
var
  CurrentFolder: string;
  aProj: IOTAProject;
begin
  // Clean all test file in project
  aProj := GetActiveProject;
  if Assigned(aProj) then
  begin
    CurrentFolder := ExtractFileDir(aProj.FileName);

    ShowMessage(IntToStr(TCommonFileSystemFunctions.CleanFolder(CurrentFolder, TestgripPrefix) )+' file(s) are deleted' );
  end;
end;

procedure TGDC_frmPropertyEditor.ClearCurrentFolder;
begin
  //
end;

constructor TGDC_frmPropertyEditor.Create(AOwner: TComponent);
var
  col: TVirtualTreeColumn;
begin
  inherited Create(AOwner);

  FBuildOutputParser := TBuildOutputParser.Create;
  FRunErrorParser := TRunErrorParser.Create;
  FLastTestPath := '';

  GCTimer.Enabled := True;

  DeskSection := Name;
  AutoSave := true;

  frmShowTestResults := TfrmShowTestresults.Create(Self);

  IdeEditorManager := TIdeEditorManager.Create;

  // Instruct TDockableForm to save state
  SaveStateNecessary := true;

  Treeview1 := TVirtualStringTree.Create(Self);
  Treeview1.Parent := pnlMain;
  Treeview1.Align := alClient;
  Treeview1.Header.AutoSizeIndex := 0;
  Treeview1.Header.DefaultHeight := 10;
  Treeview1.Header.Font.Charset := DEFAULT_CHARSET;
  Treeview1.Header.Font.Color := clWindowText;
  Treeview1.Header.Font.Height := -11;
  Treeview1.Header.Font.Name := 'Tahoma';
  Treeview1.Header.Font.Style := [];
  Treeview1.Header.Height := 10;
  Treeview1.Header.Options := [hoColumnResize, hoDrag, hoShowHint, hoShowSortGlyphs];
  Treeview1.HintAnimation := hatNone;
  Treeview1.HintMode := hmHint;
  Treeview1.ParentShowHint := False;
  Treeview1.ShowHint := True;
  Treeview1.TabOrder := 0;
  Treeview1.OnClick := TreeView1Click;
  Treeview1.OnDblClick := Treeview1DblClick;
  Treeview1.OnGetText := Treeview1GetText;
  Treeview1.OnPaintText := Treeview1PaintText;
  Treeview1.OnGetHint := Treeview1GetHint;

  TreeView1.OnMouseDown := Treeview1MouseDown;
  TreeView1.PopupMenu := nil;

  col := Treeview1.Header.Columns.Add;
  col.Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coWrapCaption, coSmartResize];
  col.Position := 0;
  col.Width := 200;

  col := Treeview1.Header.Columns.Add;
  col.Position := 1;
  col.Style := vsOwnerDraw;
  col.Width := Treeview1.ClientWidth - Treeview1.Header.Columns[0].Width;
  col.Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coWrapCaption, coSmartResize];

  Treeview1.NodeDataSize := SizeOf(rTreeData);
end;

procedure TGDC_frmPropertyEditor.miAboutTestgripClick(Sender: TObject);
var
  frmAbout: TfrmAboutTestgrip;
begin
  frmAbout := TfrmAboutTestgrip.Create(nil);
  try
    frmAbout.ShowModal;
  finally
    frmAbout.Free;
  end;
end;

procedure TGDC_frmPropertyEditor.miFaqClick(Sender: TObject);
begin
  // go to http://www.gdcsoftware.com/index.php/testgrip/f-a-q/
  TCommonExecutionFunctions.OpenURL('http://www.gdcsoftware.com/index.php/testgrip/f-a-q/');
end;

procedure TGDC_frmPropertyEditor.miGenerateCodeForFunctionClick(Sender: TObject);
var
  frmShowTestCode: TfrmShowTestcode;
begin
  frmShowTestCode := TfrmShowTestcode.Create(nil);
  try
    frmShowTestCode.LoadTestCode(
      GetSelectedTestFunction,
      TIDEHelper.GetCurrentUnitName
    );

    frmShowTestCode.ShowModal;
  finally
    frmShowTestCode.Free;
  end;
end;

procedure TGDC_frmPropertyEditor.miHelpClick(Sender: TObject);
var
  sTestgripPath: string;
begin
  sTestgripPath := TestgripInstallPath;
  if FileExists(sTestgripPath + C_HELPFILE) then
  begin
    ShellExecute(0, 'open', PChar(sTestgripPath + C_HELPFILE), nil, nil, SW_SHOW);
  end;
end;

procedure TGDC_frmPropertyEditor.miJumpToFunctionClick(Sender: TObject);
var
  EditView: IOTAEditView;
  EditorServices: IOTAEditorServices;
  TestFunc: TInputFunction;
  node: PTreeData;
  PascalFile: TPascalFile;
  mdef: TMethodDefinition;
  sUnit: string;
begin
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  EditView := EditorServices.TopView;
  TestFunc := nil;

  if Assigned(EditView) then
  begin
    if assigned(Treeview1.FocusedNode) then
    begin
      Node := Treeview1.GetNodeData(Treeview1.FocusedNode);
      if assigned(Node) then
      begin
        if Node.Level = 1 then
        begin
          TestFunc := TInputFunction(node.Data);
        end
        else if Node.Level = 2 then
        begin
          TestFunc := TInputTest(node.Data).Parent;
        end;
      end;
    end;

    if Assigned(TestFunc) then
    begin
      sUnit := TIDEHelper.GetCurrentUnitName;
      if not IdeEditorManager.GetCurrentPascalFile(sUnit,PascalFile) then
      begin
        PascalFile := IdeEditorManager.AddManagedPascalFile(sUnit, true);
      end;

      // TODO: eigenlijk niet GetFirstMethodDefinitionFromSourceByName() gebruiken
      if Assigned(TestFunc.Parent) then
      begin
        mdef := GetFirstMethodDefinitionFromSourceByName( PascalFile.PascalFileName, TestFunc.MethodName, TestFunc.Parent.Name, true );
      end
      else
      begin
        mdef := GetFirstMethodDefinitionFromSourceByName( PascalFile.PascalFileName, TestFunc.MethodName, '', true );
      end;

      if Assigned(mdef) then
      begin
        EditView.GetEditWindow.Form.SetFocus;

        EditView.Position.GotoLine(mdef.LineNumber);

        EditView.Center(mDef.LineNumber, 0);
      end
      else
      begin
        TestgripShowError('Can''t find function or procedure by this name');
      end;
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.miOpenDemoProjectClick(Sender: TObject);
var
  sDemoProjPath: string;
begin
  sDemoProjPath := TCommonAppFunctions.GetAppDataFolder + 'GDC Software\Testgrip\demo\TheWorld.dproj';
  if FileExists(sDemoProjPath) then
  begin
    TCommonExecutionFunctions.ExecuteFile(ExtractFileName(sDemoProjPath),ExtractFileDir(sDemoProjPath));
  end;
end;

procedure TGDC_frmPropertyEditor.miPopFunction_DefaultClick(Sender: TObject);
begin
  TreeView1DblClick(TreeView1);
end;

procedure TGDC_frmPropertyEditor.miPopTest_CopyTestClick(Sender: TObject);
var
  t: TInputTest;
  tf: TInputFunction;
  NodeData: PTreeData;
  PascalFile: TPascalFile;
  tcopy: TInputTest;
begin
  if Assigned(TreeView1.FocusedNode) then
  begin
    NodeData := Treeview1.GetNodeData(Treeview1.FocusedNode);
    if NodeData.Level = 2 then
    begin
      IdeEditorManager.GetCurrentPascalFile(TIDEHelper.GetCurrentUnitName, PascalFile);

      t := TInputTest(NodeData.Data);
      tf := t.Parent;

      tcopy := TInputTest.Create(tf);
      tcopy.CloneFrom(t);
      tcopy.CopyAllImpliersFrom(t);

      tcopy.DisplayName := 'Copy of ' + t.DisplayName;

      tf.TestList.Add( tcopy );

      PascalFile.SaveToFile( GetCurrentTestFilename );
      UpdateTreeView(PascalFile);
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.miPopTest_DefaultClick(Sender: TObject);
begin
  TreeView1DblClick(Treeview1);
end;

procedure TGDC_frmPropertyEditor.miPopTest_JumpToFunctionClick(Sender: TObject);
begin
  miJumpToFunctionClick(Sender);
end;

procedure TGDC_frmPropertyEditor.miPopTest_ShowLatestTestResultClick(
  Sender: TObject);
var
  NodeData: PTreeData;
  t: TInputTest;
begin
  if Assigned(TreeView1.FocusedNode) then
  begin
    NodeData := Treeview1.GetNodeData(Treeview1.FocusedNode);

    if NodeData.Level = 2 then
    begin
      t := TInputTest(NodeData.Data);

      frmShowTestResults.Path := FLastTestPath;
      frmShowTestResults.InitWithTest(t);

      frmShowTestResults.ShowModal;
    end;
  end;
end;

class procedure TGDC_frmPropertyEditor.miShowTestgripClick(Sender: TObject);
begin
  PropertyEditorComponentsUnregister;

  PropertyEditorComponentsRegister;
end;

procedure TGDC_frmPropertyEditor.miToggleExceptionHandlingClick(
  Sender: TObject);
begin
  miToggleExceptionHandling.Checked := not miToggleExceptionHandling.Checked;

  SetUse3rdPartyExceptionSetting(miToggleExceptionHandling.Checked);
end;

procedure TGDC_frmPropertyEditor.miToggleUseDUnitClick(Sender: TObject);
begin
  miToggleUseDUnit.Checked := True;
  miToggleUseDUnitX.Checked := False;

  SetUseTestFramework(tfwDUnit);
end;

procedure TGDC_frmPropertyEditor.miToggleUseDUnitXClick(Sender: TObject);
begin
  miToggleUseDUnit.Checked := False;
  miToggleUseDUnitX.Checked := True;

  SetUseTestFramework(tfwDUnitX);
end;

procedure TGDC_frmPropertyEditor.DeleteSelectedTest;
var
  t: TInputTest;
  tf: TInputFunction;
  tfc: TInputTestClass;
  NodeData: PTreeData;
  PascalFile: TPascalFile;
begin
  if Assigned(TreeView1.FocusedNode) then
  begin
    NodeData := Treeview1.GetNodeData(Treeview1.FocusedNode);
    if NodeData.Level = 2 then
    begin
      IdeEditorManager.GetCurrentPascalFile(TIDEHelper.GetCurrentUnitName,PascalFile);

      t := TInputTest(NodeData.Data);
      tf := t.Parent;

      t.Free;

      tf.TestList.Remove(t);

      if tf.TestList.Count = 0 then // delete function also
      begin
        tfc := tf.Parent;
        tfc.FunctionList.Remove(tf);
        tf.Free;

        if tfc.FunctionList.Count = 0 then
        begin
          // delete object
          PascalFile.TestClassList.Remove(tfc);
          tfc.Free;

          // Delete node
          Treeview1.DeleteNode(Treeview1.FocusedNode.Parent.Parent);
        end
        else
        begin
          Treeview1.DeleteNode(Treeview1.FocusedNode.Parent);
        end;

      end
      else
      begin
        Treeview1.DeleteNode(Treeview1.FocusedNode);
      end;


      // Save tests
      PascalFile.SaveToFile(GetCurrentTestFilename);


      SyncIdeManager;
    end;
  end;
end;

destructor TGDC_frmPropertyEditor.Destroy;
begin
  IDEGarbageCollector.GCCycle(-1, true);

  // Instruct TDockableForm to save state
  SaveStateNecessary := true;

  IdeEditorManager.Free;

  FBuildOutputParser.Free;
  FRunErrorParser.Free;

  inherited;

  GDC_frmPropertyEditor := nil;
end;

procedure TGDC_frmPropertyEditor.DisableProgressBar;
begin
  pnlBottom.Visible := false;
end;

procedure TGDC_frmPropertyEditor.FindAndSelectFunctionInTree(const sClass,
  sFunction: string);
var
  nodeClass,
  NodeFunction: PVirtualNode;
  NodeData: PTreeData;
begin
  nodeClass := Treeview1.RootNode.FirstChild;
  while assigned(nodeClass) do
  begin
    NodeData := Treeview1.GetNodeData(nodeClass);
    if SameText(NodeData.Name, sClass) then
    begin
      nodeFunction := nodeClass.FirstChild;

      while assigned(nodeFunction) do
      begin
        nodeData := Treeview1.GetNodeData(nodeFunction);

        if SameText(nodeData.Name, sFunction) then
        begin
          Treeview1.ClearSelection;

          Treeview1.FocusedNode := nodeFunction;
          Treeview1.Selected[nodeFunction] := True;
          Treeview1.Expanded[nodeFunction] := True;
        end;
        nodeFunction := nodeFunction.NextSibling;
      end;
    end;
    nodeClass := nodeClass.NextSibling;
  end;
end;

procedure TGDC_frmPropertyEditor.FormCreate(Sender: TObject);
begin
  tmrInit.Enabled := True;

  FDefinitionSearch := nil;
end;

procedure TGDC_frmPropertyEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDefinitionSearch);
end;

procedure TGDC_frmPropertyEditor.FormResize(Sender: TObject);
begin
  if Assigned(Treeview1) then
  begin
    Treeview1.Header.Columns[1].Width := Math.Max(Treeview1.ClientWidth - Treeview1.Header.Columns[0].Width, 200);
  end;
end;

procedure TGDC_frmPropertyEditor.EditTestsWithMatrixEdit(APascalFile: TPascalFile; AInputFunction: TInputFunction; AUnitParser: TUnitParser; AMethodDefinition: TMethodDefinition; AInputTestClass: TInputTestClass);
var
  frmEdit: TfrmTestsMatrixEdit;
begin
  frmEdit := TfrmTestsMatrixEdit.Create(nil);
  try
    frmEdit.LoadFunction(AMethodDefinition, AInputTestClass, AInputFunction, AUnitParser);
    if frmEdit.ShowModal = mrOk then
    begin
      frmEdit.SaveTests;
      UpdateTreeView(APascalFile);
      APascalFile.SaveToFile(GetCurrentTestFilename);
    end;
  finally
    frmEdit.Free;
  end;
end;

function TGDC_frmPropertyEditor.FindClosestMethodDefinitionForMethodSignature(const ASignature: string; oUnitParser: TUnitParser): TMethodDefinition;
var
  testmdef: TMethodDefinition;
begin
  testmdef := TMethodDefinition.Create(ASignature, True, csUnknown);
  try
    Result := oUnitParser.FindClosestMethodDefinition(testmdef);
  finally
    testmdef.Free;
  end;
end;

procedure TGDC_frmPropertyEditor.SetInfoForMethod(const AMethodDefinition: TMethodDefinition);
begin
  if AMethodDefinition.InClass <> '' then
  begin
    SetLabelInfo(AMethodDefinition.InClass + '.' + AMethodDefinition.DefMethodName);
  end
  else
  begin
    SetLabelInfo(AMethodDefinition.DefMethodName);
  end;
end;

procedure TGDC_frmPropertyEditor.RemoveTestgripMenusFromIDE(const AINTAServices: INTAServices);
var
  c: Integer;
  i: Integer;
begin
  c := AINTAServices.MainMenu.Items.Count - 1;
  for i := 0 to c do
  begin
    if SameText('Testgrip', AINTAServices.MainMenu.Items[i].Hint) then
    begin
      // remove existing menu, referes to old memory (should also be removed on uninstall, but how?)
      AINTAServices.MainMenu.Items.Delete(i);
      break;
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.AddTestgripMenusToIDE(const AINTAServices: INTAServices);
var
  miTestgrip: TMenuItem;
  miRunAllTests: TMenuItem;
  miOpenDemoProject: TMenuItem;
  miFaqTestgrip: TMenuItem;
  miHelp: TMenuItem;
  miAboutTestgrip: TMenuItem;
  ActiveTestframework: TTestFrameWork;
begin
  miTestgrip := TMenuItem.Create(AINTAServices.MainMenu);
  miTestgrip.Caption := 'Testgrip';
  miTestgrip.Hint := 'Testgrip';

  // hint is not visible, but ensures we can still identify the menu item (the caption is automatically altered for a dynamically assigned shortcutkey)
  miShowTestgrip := TMenuItem.Create(miTestgrip);
  miShowTestgrip.Caption := '&Show';
  miShowTestgrip.Enabled := True;
  miShowTestgrip.OnClick := miShowTestgripClick;
  miTestgrip.Add(miShowTestgrip);

  miRunAllTests := TMenuItem.Create(miTestgrip);
  miRunAllTests.Action := acRunAllTests;
  acRunAllTests.Enabled := True;
  miTestgrip.Add(miRunAllTests);

  miOpenDemoProject := TMenuItem.Create(miTestgrip);
  miOpenDemoProject.Caption := '&Demo project';
  miOpenDemoProject.Enabled := True;
  miOpenDemoProject.OnClick := miOpenDemoProjectClick;
  miTestgrip.Add(miOpenDemoProject);

  miFaqTestgrip := TMenuItem.Create(miTestgrip);
  miFaqTestgrip.Caption := 'Frequently asked &questions';
  miFaqTestgrip.Enabled := True;
  miFaqTestgrip.OnClick := miFaqClick;
  miTestgrip.Add(miFaqTestgrip);

  miToggleExceptionHandling := TMenuItem.Create(miTestgrip);
  miToggleExceptionHandling.Caption := 'Use 3rd party &exception handler';
  miToggleExceptionHandling.Enabled := True;
  miToggleExceptionHandling.Checked := GetUse3rdPartyExceptionSetting;
  miToggleExceptionHandling.OnClick := miToggleExceptionHandlingClick;
  miTestgrip.Add(miToggleExceptionHandling);

  ActiveTestframework := GetUseTestFramework;

  miToggleUseDUnit := TMenuItem.Create(miTestgrip);
  miToggleUseDUnit.Caption := 'Use DUnit';
  miToggleUseDUnit.Enabled := True;
  miToggleUseDUnit.Checked := (ActiveTestframework = tfwDUnit);
  miToggleUseDUnit.OnClick := miToggleUseDUnitClick;
  miTestgrip.Add(miToggleUseDUnit);

  miToggleUseDUnitX := TMenuItem.Create(miTestgrip);
  miToggleUseDUnitX.Caption := 'Use DUnitX';
  miToggleUseDUnitX.Enabled := True;
  miToggleUseDUnitX.Checked := (ActiveTestframework = tfwDUnitX);
  miToggleUseDUnitX.OnClick := miToggleUseDUnitXClick;
  miTestgrip.Add(miToggleUseDUnitX);

  miHelp := TMenuItem.Create(miTestgrip);
  miHelp.Caption := '&Help';
  miHelp.Enabled := True;
  miHelp.OnClick := miHelpClick;
  miTestgrip.Add(miHelp);

  miAboutTestgrip := TMenuItem.Create(miTestgrip);
  miAboutTestgrip.Caption := '&About...';
  miAboutTestgrip.Enabled := True;
  miAboutTestgrip.OnClick := miAboutTestgripClick;
  miTestgrip.Add(miAboutTestgrip);

  AINTAServices.MainMenu.Items.Add(miTestgrip);
end;

procedure PropertyEditorComponentsUnregister;
begin
  RemoveNotifier;

  // Cleanup dockable form instance
  if @UnregisterFieldAddress <> nil then
    UnregisterFieldAddress(@GDC_frmPropertyEditor);

  FreeAndNil(GDC_frmPropertyEditor);

  If iKeyBindingIndex > 0 Then
    (BorlandIDEServices As IOTAKeyboardServices).RemoveKeyboardBinding(iKeyBindingIndex);
end;

procedure TGDC_frmPropertyEditor.InitDefinitionSearch(sProjectFile: string);
begin
  if Assigned(FDefinitionSearch) then
  begin
    if FDefinitionSearch.ProjectFile <> sProjectFile then
    begin
      FreeAndNil(FDefinitionSearch);
    end;
  end;

  if not Assigned(FDefinitionSearch) then
  begin
    FDefinitionSearch := TDefinitionSearch.Create(sProjectFile);
  end;
end;

procedure TGDC_frmPropertyEditor.ShowBuildError;
var
  frmCode: TfrmShowTestcode;
begin
  frmCode := TfrmShowTestcode.Create(nil);
  try
    frmCode.HandleBuildError(FBuildOutputParser);
    frmCode.ShowModal;
  finally
    frmCode.Free;
  end;
end;

procedure TGDC_frmPropertyEditor.AddTestFunctionNodesToClassNode(t: TInputFunction; var NodeData: PTreeData; InputTestClass: TInputTestClass; NodeClass: PVirtualNode);
var
  j: Integer;
  InputTestFunction: TInputFunction;
  NodeFunctions: PVirtualNode;
begin
  for j := 0 to InputTestClass.FunctionList.Count - 1 do
  begin
    InputTestFunction := TInputFunction(InputTestClass.FunctionList[j]);

    NodeFunctions := TreeView1.Insertnode(NodeClass, amAddChildLast);

    NodeData := Treeview1.GetNodeData(NodeFunctions);
    NodeData.name := InputTestFunction.MethodName;
    NodeData.Data := InputTestFunction;
    NodeData.Level := 1;

    AddTestNodesToFunctionNode(NodeData, InputTestFunction, NodeFunctions);
    if (t = InputTestFunction) then
    begin
      SelectNodeInTree(NodeFunctions);
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.AddTestNodesToFunctionNode(var NodeData: PTreeData; InputTestFunction: TInputFunction; NodeFunctions: PVirtualNode);
var
  k: Integer;
  InputTest: TInputTest;
  NodeTest: PVirtualNode;
begin
  for k := 0 to InputTestFunction.TestList.Count - 1 do
  begin
    InputTest := TInputTest(InputTestFunction.TestList.Items[k]);

    NodeTest := TreeView1.InsertNode(NodeFunctions, amAddChildLast);

    NodeData := Treeview1.GetNodeData(NodeTest);
    NodeData.name := InputTest.DisplayName;
    NodeData.Data := InputTest;
    NodeData.Level := 2;
  end;
end;

procedure TGDC_frmPropertyEditor.RemoveAndFreeTest(const AInputTestForm: TfrmInputTest);
var
  InputFunction: TInputFunction;
  InputTest: TInputTest;
begin
  InputTest := AInputTestForm.LoadedTest;
  InputFunction := InputTest.Parent;
  if InputFunction.TestList.Count = 1 then
  begin
    InputFunction.Parent.FunctionList.Remove(AInputTestForm.LoadedTest.Parent);
    InputFunction.Free;
  end
  else
  begin
    InputFunction.TestList.Remove(InputTest);
    InputTest.Free;
  end;
end;

procedure TGDC_frmPropertyEditor.GCTimerTimer(Sender: TObject);
begin
  GCTimer.Enabled := False;
  try
    try
      IDEGarbageCollector.GCCycle(1, false);
    except
      // ignore
    end;
  finally
    GCTimer.Enabled := True;
  end;
end;

function TGDC_frmPropertyEditor.GetActiveFunctionOrProcedure: TMethodDefinition;
var
  selectedfunc: TInputFunction;
begin
  // if the Testgrip properties window was focussed and a test or method was selected, return that selected method
  if Treeview1.Focused then
  begin
    selectedfunc := GetSelectedTestFunction;
    if Assigned(selectedfunc) then
    begin
      Result := GetMethodDefinitionFromExistingFunction(selectedfunc);

      Exit;
    end;
  end;

  // otherwise, try to get it from the code editor
  Result := TIDEHelper.GetActiveFunctionOrProcedure;
end;

procedure TGDC_frmPropertyEditor.lblAddTestClick(Sender: TObject);
begin
  AddTest;
end;

procedure TGDC_frmPropertyEditor.lblResultClick(Sender: TObject);
begin
  if FBuildOutputParser.ErrorCode <> '' then
  begin
    ShowBuildError;
  end
  else
  begin
    ShowMessage(lblResult.Caption + #13#10 + FErrorResult);
  end;
end;

procedure TGDC_frmPropertyEditor.ListAllFilesInIDE(const AProject: IOTAProject; const AFiles: TStrings);
var
  i, c: Integer;
begin
  {$ifdef VER200}
    c := AProject.GetModuleCount - 1;
    for i := 0 to c do
    begin
      AFiles.Add(AProject.GetModule(i).FileName);
    end;
  {$else}
    {$ifdef VER180}
      c := AProject.GetModuleCount - 1;
      for i := 0 to c do
      begin
        AFiles.Add(AProject.GetModule(i).FileName);
      end;
    {$else}
      {$ifdef VER150}
        c := AProject.GetModuleCount - 1;
        for i := 0 to c do
        begin
          AFiles.Add(AProject.GetModule(i).FileName);
        end;
      {$else}
        AProject.GetCompleteFileList(AFiles);
      {$endif}
    {$endif}
  {$endif}
end;

procedure TGDC_frmPropertyEditor.OnSaveExistingTest(Sender: TObject);
var
  frm: TfrmInputTest;
  pasfile: TPascalFile;
begin
  frm := TfrmInputTest(Sender);
  if frm.ModalResult = mrOK then
  begin
    frm.SaveToTest(frm.LoadedTest);

    pasfile := TPascalFile(frm.CurrentPascalFile);
    UpdateTreeView(pasfile);

    pasfile.SaveToFile(pasfile.TestFileName);
  end;
end;

procedure TGDC_frmPropertyEditor.OnSaveNewTest(Sender: TObject);
var
  InputTestForm: TfrmInputTest;
  InputTest: TInputTest;
  PasFile: TPascalFile;
begin
  InputTestForm := TfrmInputTest(Sender);

  if InputTestForm.ModalResult = mrOk then
  begin
    InputTest := InputTestForm.LoadedTest;
    InputTestForm.SaveToTest( InputTest );

    PasFile := TPascalFile(InputTestForm.CurrentPascalFile);
    PasFile.SaveToFile(PasFile.TestFileName);

    UpdateTreeView(PasFile);

    if Assigned(InputTest) then
    begin
      SelectNodeInTree(InputTest);
    end;
  end
  else
  begin
    RemoveAndFreeTest(InputTestForm);
  end;
end;

procedure TGDC_frmPropertyEditor.OpenInputForm(InputTest: TInputTest);
var
  PascalFile: TPascalFile;
  frm: TfrmInputTest;
  sUnit: string;
begin
  sUnit := TIDEHelper.GetCurrentUnitName;
  PascalFile := IdeEditorManager.GetOrAddPascalFile(sUnit);

  // Present a form to give the params
  frm := TfrmInputTest.Create(nil);
  frm.LoadTest(InputTest, sUnit);
  frm.SetFormView;
  frm.CurrentPascalFile := PascalFile;
  frm.OnSave := OnSaveExistingTest;

  frm.Show;
  IDEGarbageCollector.Add(frm);
end;

procedure TGDC_frmPropertyEditor.AddTest;
var
  mDef: TMethodDefinition;
  PascalFile: TPascalFile;
  InputTestClass: TInputTestClass;
  InputFunction: TInputFunction;
  InputTest: TInputTest;
  frm: TfrmInputTest;
  i: integer;
  oUnitParser: TUnitParser;
  fsPascalFile: TFileStream;
  bFunctionExists: boolean;
  aProjParser: TProjectParser;

  EditorServices: IOTAEditorServices;
  EditView: IOTAEditView;
  iStartOfImplementation: integer;
  sUnit: string;
begin
  try
    mDef := GetActiveFunctionOrProcedure;
    try
      if assigned(mDef) then
      begin
        // Check if pascalfile exists in the manager
        sUnit := TIDEHelper.GetCurrentUnitName;
        PascalFile := IdeEditorManager.GetOrAddPascalFile(sUnit);

        // UnitParser
        oUnitParser := TUnitParser.Create;
        try
          fsPascalFile := TFileStream.Create(sUnit, fmOpenRead);
          try
            oUnitParser.ParseUnit(fsPascalFile, true, false, false, false, true);

            iStartOfImplementation := oUnitParser.StartOfImplementation;
          finally
            fsPascalFile.Free;
          end;

          // Find the current class
          InputTestClass := PascalFile.GetOrAddInputTestClass(mDef.InClass);

          // Present a form to give the params
          frm := TfrmInputTest.Create(nil);

          i := 0;
          bFunctionExists := InputTestClass.GetInputFunction(mDef,InputFunction);
          if bFunctionExists then
          begin
            i := InputFunction.TestCount;
          end
          else
          begin
            EditorServices := BorlandIDEServices as IOTAEditorServices;
            EditView := EditorServices.TopView;

            if EditView.CursorPos.Line < iStartOfImplementation then
            begin
              raise ENotInImplementation.Create('To add new tests, your cursor needs to be inside the implementation of the procedure or function.');
            end;

            InputFunction := InputTestClass.AddInputFunction(mDef);
          end;


          // Add new test
          InputTest := InputFunction.AddInputTest(frm.edTestName.Text);

          aProjParser := TProjectParser.Create;
          try
            frm.LoadProjectFiles(IncludeTrailingPathDelimiter(ExtractFileDir(GetActiveProject.FileName)) + GetProjectFileName(aProjParser));
          finally
            aProjParser.Free;
          end;

          frm.LoadTest(InputTest, sUnit, mdef, i + 1);
          //frm.SetMethod(mDef, i + 1, oUnitParser);
          frm.SetFormView;
          frm.CurrentPascalFile := PascalFile;
          frm.OnSave := OnSaveNewTest;
          frm.Show;
          IDEGarbageCollector.Add(frm);
        finally
          oUnitParser.Free;
        end;
      end;
    finally
      mDef.Free;
    end;
  except
    on E: ENotInImplementation do
    begin
      TestgripShowError(E.Message);
    end
    else
    begin
      raise;
    end;
  end;
end;


procedure TGDC_frmPropertyEditor.OpenSelectedNodeTest;
var
  Node: PTreeData;
  InputTest: TInputTest;
  TestFunc: TInputFunction;
  TestClass: TInputTestClass;
  mdef: TMethodDefinition;
  PascalFile: TPascalFile;
  oUnitParser: TUnitParser;
  sUnit: string;
begin
  if assigned(Treeview1.FocusedNode) then
  begin
    Node := Treeview1.GetNodeData(Treeview1.FocusedNode);
    if assigned(Node) then
    begin
      if Node.Level = 2 then
      begin
        InputTest := TInputTest(node.Data);

        if assigned(InputTest) then
          OpenInputForm(InputTest);
      end
      else if Node.Level = 1 then
      begin
        // Check if pascalfile exists in the manager
        sUnit := TIDEHelper.GetCurrentUnitName;
        PascalFile := IdeEditorManager.GetOrAddPascalFile(sUnit);

        TestFunc := TInputFunction(node.Data);

        oUnitParser := TUnitParser.Create;
        try
          oUnitParser.ParseFromFile(sUnit, True);

          mDef := FindClosestMethodDefinitionForMethodSignature(TestFunc.GuessSignature, oUnitParser);
          if assigned(mDef) then
          begin
            TestClass := TestFunc.Parent;

            EditTestsWithMatrixEdit(PascalFile, TestFunc, oUnitParser, mdef, TestClass);
          end;
        finally
          oUnitParser.Free;
        end;

        // lookup and set cursor to function:
        if TestFunc.Parent.ClassName <> '' then
        begin

          //TestFunc.Parent.ClassName + '.' + TestFunc.Name;
        end
        else
        begin
          //TestFunc.Name;
        end;
      end;
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.miRenameFunctionClick(Sender: TObject);
var
  sOldName: string;
  sNewName: string;
  Node: PTreeData;
  TestFunc: TInputFunction;
  frmRename: TfrmRename;
  b: boolean;
  PascalFile: TPascalFile;
  sUnit: string;
begin
  sOldName := '';
  sNewName := '';
  b := false;

  if assigned(Treeview1.FocusedNode) then
  begin
    Node := Treeview1.GetNodeData(Treeview1.FocusedNode);
    if assigned(Node) then
    begin
      if Node.Level = 1 then
      begin
        TestFunc := TInputFunction(node.Data);
        sOldName := TestFunc.MethodName;

        frmRename := TfrmRename.Create(nil);
        try
          frmRename.Data := sOldName;
          if frmRename.ShowModal = mrOk then
          begin
            sNewName := frmRename.Data;

            b := true;

            TestFunc.MethodName := sNewName;
          end;
        finally
          frmRename.Free;
        end;
      end;
    end;
  end;

  if b then
  begin
    sUnit := TIDEHelper.GetCurrentUnitName;
    PascalFile := IdeEditorManager.GetOrAddPascalFile(sUnit);

    UpdateTreeView(PascalFile);

    PascalFile.SaveToFile(GetCurrentTestFilename);
  end;
end;

procedure TGDC_frmPropertyEditor.RunAllTests;
begin
  RunTests(false);
end;

procedure TGDC_frmPropertyEditor.Runalltests1Click(Sender: TObject);
begin
  RunAllTests;
end;

procedure TGDC_frmPropertyEditor.Runcurrentfunction1Click(Sender: TObject);
begin
  acRunTest.Execute;
end;

function TGDC_frmPropertyEditor.RunFunctionTest(const ProjectFileName: string; const oTestGen: TTestGen; const oFunctionTest: TInputFunction; DeleteTestProject: boolean; bDontActuallyRunTest: boolean; const sUnitFilename: string): boolean;
var
  i, c: integer;
  aTest: TInputTest;
  sError: string;
begin
  oTestGen.DisableExceptionLib := not miToggleExceptionHandling.Checked;
  oTestGen.UseTestFrameWork := GetUseTestFramework;

  result := oTestGen.RunFunctionTests( ProjectFilename,
    sUnitFilename,
    oFunctionTest.Parent,oFunctionTest,true,DeleteTestProject, bDontActuallyRunTest);

  // compilation command used for testproject
  TIDEHelper.AddDelphiMessage( oTestGen.ProjectGen.CompileCommand.Text );

  // write legible testresults to messages panel
  c := oFunctionTest.TestList.Count - 1;
  for i := 0 to c do
  begin
    aTest := TInputTest(oFunctionTest.TestList[i]);

    sError := GetShortTestResultString(aTest);
    if sError <> '' then
    begin
      TIDEHelper.AddDelphiMessage( aTest.DisplayName + ': ' + sError );
    end;
  end;
end;

function TGDC_frmPropertyEditor.RunTests(RunOnlyFocusedNode: boolean; DeleteTestProject: boolean; bDontActuallyRunTest: boolean): string;
var
  PascalFile: TPascalFile;
  aTestGen: TTestGen;
  ProjectFileName: string;
  aSelectedFunction: TInputFunction;
  lstErrors: TStringlist;
  NodeDataClass,
  NodeData: PTreeData;
  nodeClass,
  NodeFunction: PVirtualNode;

  iTestPassed,iTestFailed: integer;

  bOneOrMoreTestFailed: boolean;
  aProjParser: TProjectParser;
begin

  // Cleanup errorresult
  FErrorResult := '';

  TIDEHelper.AddDelphiMessage('Compiling test projects...');
  bOneOrMoreTestFailed := false;

  result := '';
  if RunOnlyFocusedNode then
  begin
    if not Assigned(TreeView1.FocusedNode) then
    begin
      RunOnlyFocusedNode := false;
    end;
  end;

  InitProgressBar;
  try
    if FileExists(GetCurrentTestFilename) then
    begin
      IdeEditorManager.GetCurrentPascalFile(TIDEHelper.GetCurrentUnitName,PascalFile);

      FBuildOutputParser.Clear;
      FRunErrorParser.Clear;

      aProjParser := TProjectParser.Create;
      try
        ProjectFileName := GetProjectFileName(aProjParser);

        aTestGen := TTestGen.Create;
        aTestGen.OnProgress := CallBackProgress;
        try
          FLastTestPath := ExtractFileDir(GetActiveProject.FileName);
          aTestGen.ProjectPath := FLastTestPath;
          aTestGen.ProjectParser := aProjParser;

          // Check if root node is selected, if true -> test all functions inside this class
          if assigned(Treeview1.FocusedNode) then
          begin
            NodeData := Treeview1.GetNodeData(Treeview1.FocusedNode);
            if NodeData.Level = 0 then
            begin
              RunOnlyFocusedNode := false;
            end;
          end;

          if RunOnlyFocusedNode then
          begin
            if assigned(Treeview1.FocusedNode) then
            begin
              aSelectedFunction := nil;
              NodeData := Treeview1.GetNodeData(Treeview1.FocusedNode);
              if NodeData.Level = 1 then  // (2=test, 1=function, 0=class)
              begin
                aSelectedFunction := TInputFunction(NodeData.Data);
              end
              else if NodeData.Level = 2 then
              begin
                aSelectedFunction := TInputTest(NodeData.Data).Parent;
              end;

              if Assigned(aSelectedFunction.Parent) then
              begin
                try
                  if NodeData.Level = 2 then // Test node, find parent
                  begin
                    NodeData := Treeview1.GetNodeData(Treeview1.FocusedNode.Parent);
                  end;
                  NodeData.ResultText := '';

                  if not RunFunctionTest(ProjectFileName,aTestGen,aSelectedFunction, False, bDontActuallyRunTest, TIDEHelper.GetCurrentUnitName) then
                  begin
                    bOneOrMoreTestFailed := true;
                    result := aTestGen.GetProjectFile;
                    if aTestGen.ProjectCompiled then
                    begin
                      SetTestResult(true, aSelectedFunction.TotalTestResult.Text);

                      FRunErrorParser.Parse(aSelectedFunction.TotalTestResult.Text, aTestGen.ProjectPath);
                    end
                    else
                    begin
                      lstErrors := TStringlist.Create;
                      try
                        aTestGen.ProjectGen.ListCompileErrorsAndFatals(lstErrors);

                        FBuildOutputParser.Parse(lstErrors.Text, aTestGen.ProjectPath);

                        SetTestResult(true, lstErrors.Text);

                        TIDEHelper.AddDelphiMessage( lstErrors.Text );

                        NodeData.ResultText := lstErrors.Text;

                        // exit town when the going gets tough
                        Exit;
                      finally
                        lstErrors.Free;
                      end;
                    end;
                  end
                  else
                  begin
                    if DeleteTestProject then
                    begin
                      aTestGen.DeleteTestProjectFiles;
                    end;
                    result := aTestGen.GetProjectFile;
                    SetTestResult(false);
                  end;
                finally
                  Treeview1.Refresh;
                end;
              end;
            end;
          end
          else
          begin
            // Run all function test

            nodeClass := Treeview1.RootNode.FirstChild;
            while assigned(nodeClass) do
            begin
              NodeDataClass := Treeview1.GetNodeData(nodeClass);

              NodeDataClass.ResultText := '';

              iTestPassed := 0;
              iTestFailed := 0;

              nodeFunction := nodeClass.FirstChild;

              while assigned(nodeFunction) do
              begin
                nodeData := Treeview1.GetNodeData(nodeFunction);
                nodeData.ResultText := '';

                try
                  aSelectedFunction := TInputFunction(nodeData.Data);
                  if not RunFunctionTest(ProjectFileName,aTestGen, aSelectedFunction, False, bDontActuallyRunTest, TIDEHelper.GetCurrentUnitName) then
                  begin
                    inc(iTestFailed);

                    if not aTestGen.ProjectCompiled then
                    begin
                      lstErrors := TStringlist.Create;
                      try
                        aTestGen.ProjectGen.ListCompileErrorsAndFatals(lstErrors);

                        // todo: what to do with non-deleted failed projects...

                        FBuildOutputParser.Parse(lstErrors.Text, aTestGen.ProjectPath);

                        SetTestResult(true, lstErrors.Text);

                        TIDEHelper.AddDelphiMessage( lstErrors.Text );

                        NodeData.ResultText := lstErrors.Text;

                        break;
                      finally
                        lstErrors.Free;
                      end;
                    end
                    else
                    begin
                      NodeData.ResultText := aSelectedFunction.TotalTestResult.Text;

                      FRunErrorParser.Parse(aSelectedFunction.TotalTestResult.Text, aTestGen.ProjectPath);
                    end;
                  end
                  else
                  begin
                    if DeleteTestProject then
                    begin
                      aTestGen.DeleteTestProjectFiles;
                    end;

                    if TInputFunction(nodeData.Data).TestResult then
                    begin
                      inc(iTestPassed);
                      nodeData.ResultText := '';
                    end
                    else
                    begin
                      inc(iTestFailed);
                      nodeData.ResultText := TInputFunction(nodeData.Data).TotalTestResult.Text;
                    end;
                  end;
                except
                  on E: Exception do
                  begin
                    inc(iTestFailed);
                    TInputFunction(nodeData.Data).TotalTestResult.Text := E.Message;
                    nodeData.ResultText := E.Message;
                  end;
                end;

                nodeFunction := nodeFunction.NextSibling;
              end;
              if iTestFailed > 0 then
                bOneOrMoreTestFailed := true;

              NodeDataClass.ResultText := 'Passed: ' + inttostr(iTestPassed) +
                ' Failed: '+IntToStr(iTestFailed);

              nodeClass := nodeClass.NextSibling;
            end;

            if bOneOrMoreTestFailed then
              SetTestResult(true,'One or more tests did not pass!' );



          end;

        finally
          aTestGen.Free;
        end;

      finally
        aProjParser.Free;
      end;

    end;
  finally
    DisableProgressBar;
    if bOneOrMoreTestFailed then
    begin
      TIDEHelper.AddDelphiMessage('One or more tests did not pass!');
    end
    else
    begin
      TIDEHelper.AddDelphiMessage('Success');
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.SelectNodeInTree(aNode: PVirtualNode);
begin
  Treeview1.FocusedNode := aNode;
  Treeview1.Expanded[aNode] := True;
  Treeview1.Selected[aNode] := True;

  Treeview1.FullExpand(aNode);
end;

procedure TGDC_frmPropertyEditor.SelectNodeInTree(aTest: TInputTest);
var
  nodeClass,
  NodeFunction,
  NodeTest: PVirtualNode;
  NodeData: PTreeData;
begin
  nodeClass := Treeview1.RootNode.FirstChild;
  while assigned(nodeClass) do
  begin
    nodeFunction := nodeClass.FirstChild;

    while assigned(nodeFunction) do
    begin
      nodeTest := nodeFunction.FirstChild;

      while assigned(nodeTest) do
      begin
        nodeData := Treeview1.GetNodeData(nodeTest);

        if TInputTest(nodeData) = aTest then
        begin
          SelectNodeInTree(nodeTest);
          exit;
        end;

        nodeTest := nodeTest.NextSibling;
      end;

      nodeFunction := nodeFunction.NextSibling;
    end;
    nodeClass := nodeClass.NextSibling;
  end;
end;

procedure TGDC_frmPropertyEditor.SetFontColorBasedOnTestResult(const ACanvas: TCanvas; const ATestResult: Boolean);
begin
  if ATestResult then
  begin
    ACanvas.Font.Color := clGreen;
  end
  else
  begin
    ACanvas.Font.Color := clRed;
    ACanvas.Font.Style := [fsBold];
  end;
end;

procedure TGDC_frmPropertyEditor.SetLabelInfo(aText: string);
begin
  lblInfo.Caption := aText;
end;

procedure TGDC_frmPropertyEditor.SetTestResult(ShowError: boolean;
  ErrorText: string);
begin
  Application.ProcessMessages;
  pnlResult.Visible := true;

  if ShowError then
  begin
    lblResult.Font.Color := clRed;
    lblResult.Caption := 'Tests failed! Click here to find out why.';
    FErrorResult := FErrorResult + #13#10 + ErrorText
  end
  else
  begin
    lblResult.Font.Color := clGreen;
    lblResult.Caption := 'All tests ok!';
    FErrorResult := '';
  end;
  Application.ProcessMessages;
end;

procedure TGDC_frmPropertyEditor.ShowCompileProgress(CompilerText: string; Position,
  Max: integer);
begin
  Application.ProcessMessages;
  lblProgress.Caption := CompilerText;
  ProgressBar1.Position := Position;
  ProgressBar1.Max := Max;
  Application.ProcessMessages;
end;

procedure TGDC_frmPropertyEditor.miShowlatesttestresultFuncClick(Sender: TObject);
var
  NodeData: PTreeData;
  f: TInputFunction;
begin
  if Assigned(TreeView1.FocusedNode) then
  begin
    NodeData := Treeview1.GetNodeData(Treeview1.FocusedNode);

    if NodeData.Level = 1 then
    begin
      f := TInputFunction(NodeData.Data);

      frmShowTestResults.Path := FLastTestPath;
      frmShowTestResults.InitWithFunc(f);

      frmShowTestResults.ShowModal;
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.SpeedButton1Click(Sender: TObject);
begin
  AddTest;
end;

procedure TGDC_frmPropertyEditor.SyncIdeManager;
var
  PascalFile: TPascalFile;
  sUnit: string;
begin
  Treeview1.Clear;

  sUnit := TIDEHelper.GetCurrentUnitName;
  IdeEditorManager.AddManagedPascalFile(sUnit,false);

  if IdeEditorManager.GetCurrentPascalFile(sUnit,PascalFile) then
  begin
    UpdateTreeView(PascalFile);
  end;
end;

procedure TGDC_frmPropertyEditor.Timer1Timer(Sender: TObject);
var
  mDef: TMethodDefinition;
  aProj: IOTAProject;
  UnitFilename: string;
begin
  aProj := GetActiveProject;
  if not Assigned(aProj) then
  begin
    SetLabelInfo('');
    UpdateTreeView(nil);

    Exit;
  end;

  if TreeView1.Focused then
  begin
    Exit;
  end;

  try
    UnitFilename := TIDEHelper.GetCurrentUnitName;
    if UnitFilename <> CurrentFile then
    begin
      SyncIdeManager;
      CurrentFile := UnitFilename;
    end;

    mDef := GetActiveFunctionOrProcedure;
    try
      if assigned(mDef) then
      begin
        SetInfoForMethod(mDef);

        FindAndSelectFunctionInTree( mDef.InClass, mDef.DefMethodName );
      end
      else
      begin
        SetLabelInfo('');
      end;
    finally
      mDef.Free;
    end;

  except
    // Do not show an exception here
  end;
end;

procedure TGDC_frmPropertyEditor.tmrInitTimer(Sender: TObject);
var
  EditorServices: IOTAEditorServices;
  EditView: IOTAEditView;
  inta: INTAServices;
begin
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  EditView := EditorServices.TopView;

  if Assigned(EditView) then
  begin
    tmrInit.Enabled := False;

    inta := (BorlandIDEServices as INTAServices);

    RemoveTestgripMenusFromIDE(inta);
    AddTestgripMenusToIDE(inta);
  end;
end;

procedure TGDC_frmPropertyEditor.TreeView1Click(Sender: TObject);
var
  t: TInputTest;
  f: TInputFunction;
  c: TInputTestClass;
  NodeData: PTreeData;
begin
  TreeView1.PopupMenu := nil;

  if Assigned(TreeView1.FocusedNode) then
  begin
    NodeData := Treeview1.GetNodeData(Treeview1.FocusedNode);

    if NodeData.Level = 2 then
    begin
      t := TInputTest(NodeData.Data);
      TreeView1.PopupMenu := popTest;

      f := t.Parent;
      c := f.Parent;

      if c.ClassName <> '' then
      begin
        SetLabelInfo(c.ClassName +'.'+ f.MethodName);
      end
      else
      begin
        SetLabelInfo(f.MethodName);
      end;
    end
    else if NodeData.Level = 1 then
    begin
      f := TInputFunction(NodeData.Data);
      TreeView1.PopupMenu := popFunction;

      c := f.Parent;

      if c.ClassName <> '' then
      begin
        SetLabelInfo(c.ClassName +'.'+ f.MethodName);
      end
      else
      begin
        SetLabelInfo(f.MethodName);
      end;
    end
    else
    begin
      TreeView1.PopupMenu := popClass;
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.TreeView1DblClick(Sender: TObject);
begin
  OpenSelectedNodeTest;
end;

procedure TGDC_frmPropertyEditor.UpdateTreeView(PascalFile: TPascalFile);
var
  i: Integer;
  InputTestClass: TInputTestClass;
  SelectedNodeData: pointer;
  NodeData: PTreeData;
  NodeClass: PVirtualNode;
  t: TInputFunction;
begin
  t := nil;
  if Treeview1.FocusedNode <> nil then
  begin
    NodeData := Treeview1.GetNodeData(Treeview1.FocusedNode);
    SelectedNodeData := NodeData.Data;
    if NodeData.Level = 1 then
    begin
      t := TInputFunction(SelectedNodeData);
    end
    else if NodeData.Level = 2 then
    begin
      t := TInputTest(SelectedNodeData).Parent;
    end;
  end;

  TreeView1.Clear;

  if Assigned(PascalFile) then
  begin
    for i := 0 to PascalFile.TestClassList.Count -1 do
    begin
      InputTestClass := TInputTestClass(PascalFile.TestClassList.Items[i]);

      if InputTestClass.Name = '' then // is dummy class
        NodeClass := TreeView1.InsertNode(Treeview1.RootNode,amAddChildFirst)
      else
        NodeClass := TreeView1.InsertNode(Treeview1.RootNode,amAddChildLast);

      NodeData := Treeview1.GetNodeData(NodeClass);
      NodeData.name   := InputTestClass.ClassName;
      NodeData.Data   := InputTestClass;
      NodeData.Level  := 0;
      NodeData.ResultText := '';

      AddTestFunctionNodesToClassNode(t, NodeData, InputTestClass, NodeClass);
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.Treeview1GetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle;
  {$ifdef VER180}var HintText: widestring{$else}
  {$ifdef VER150}var HintText: widestring{$else}
  var HintText: string{$endif}
  {$endif});
var
  NodeData: PTreeData;
begin
  NodeData := Treeview1.GetNodeData(Node);

  if (NodeData.Level = 2) or (NodeData.Level = 1) then
  begin
    HintText := NodeData.ResultText;
  end
  else
  begin
    HintText := '';
  end;
end;


procedure TGDC_frmPropertyEditor.Treeview1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  {$ifdef VER180}var CellText: widestring{$else}
  {$ifdef VER150}var CellText: widestring{$else}
  var CellText: string{$endif}
  {$endif});
var
  NodeData: PTreeData;
  t: TInputTest;
begin
  CellText := '';
  NodeData := Treeview1.GetNodeData(Node);
  if Column = 0 then
  begin
    if NodeData.Name ='' then
      CellText := 'Functions without a class'
    else
      CellText := NodeData.Name;
  end
  else
  begin
    // Column 1 is result column
    if NodeData.Level = 0 then
    begin
      CellText := NodeData.ResultText
    end
    else if NodeData.Level = 1 then
    begin
      CellText := NodeData.ResultText
    end
    else if NodeData.Level = 2 then
    begin
      t := TInputTest(NodeData.Data);
      if not t.TestResult then
      begin
        CellText := GetShortTestResultString(t);
        if CellText = '' then
        begin
          CellText := t.TestResultString;
        end;
      end;
    end;
  end;
end;

procedure TGDC_frmPropertyEditor.Treeview1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    // simulate left mouse click before right mousebutton code is executed (shows context-popup menu) in order to initialize things properly
    PostMessage(TreeView1.Handle, WM_LBUTTONDOWN, MK_LBUTTON, MAKELPARAM(X,Y));
    PostMessage(TreeView1.Handle, WM_LBUTTONUP, MK_LBUTTON, MAKELPARAM(X,Y));
  end;
end;

procedure TGDC_frmPropertyEditor.Treeview1PaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  t: TInputTest;
  tf: TInputFunction;
  NodeData: PTreeData;
begin
  if assigned(Node) then
  begin
    NodeData := Treeview1.GetNodeData(Node);
    if NodeData.Level = 1 then
    begin
      tf := TInputFunction(NodeData.Data);

      SetFontColorBasedOnTestResult(TargetCanvas, tf.TestResult);
    end
    else if NodeData.Level = 2 then
    begin
      t := TInputTest(NodeData.Data);

      SetFontColorBasedOnTestResult(TargetCanvas, t.TestResult);
    end;
  end;
end;

{ TKeyboardBinding }

procedure TKeyboardBinding.AddBreakpoint(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  if KeyCode = TextToShortCut('Ctrl+Alt+R') then
  begin
    GDC_frmPropertyEditor.acRunTest.Execute;
    BindingResult := krHandled;
  end
  else if KeyCode = TextToShortCut('Ctrl+Alt+T') then
  begin
    GDC_frmPropertyEditor.acAddTest.Execute;
    BindingResult := krHandled;
  end
  else if KeyCode = TextToShortCut('Ctrl+Alt+G') then
  begin
    GDC_frmPropertyEditor.acGenerateOverrides.Execute;
    BindingResult := krHandled;
  end;
end;

procedure TKeyboardBinding.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([TextToShortcut('Ctrl+Alt+R')], AddBreakpoint, Nil);
  BindingServices.AddKeyBinding([TextToShortcut('Ctrl+Alt+T')], AddBreakpoint, Nil);

  BindingServices.AddKeyBinding([TextToShortcut('Ctrl+Alt+G')], AddBreakpoint, Nil);
end;

function TKeyboardBinding.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TKeyboardBinding.GetDisplayName: string;
begin
  result := 'Integrated Unit test bindings';
end;

function TKeyboardBinding.GetName: string;
begin
  result := 'IUTBindings';
end;

initialization

finalization

  PropertyEditorComponentsUnregister;

end.
