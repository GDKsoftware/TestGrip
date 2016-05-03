unit uTestGen;

interface

uses
  uTestDefs, uInheritGen, uPascalFileGen, uProjectGen, uUnitParser, Classes, Contnrs, uProjectParser,
  uUnitTestGenIntf;

type
  TTestGen = class;

  TTestGenProgressFunc = procedure( const Sender: TTestGen; currentstep: integer; maxsteps: integer; const sShortDesc: string ) of object;

  { TTestGen }
  TTestGen = class(TObject)
  protected
    FProjectPath: string;
    FTestResultPath: string;
    FUnitTestClassName: string;

    FUseTestFrameWork: TTestFrameWork;
    FInheritedGen: TInheritGen;
    FUnitTestGen: IUnitTestClassFileGen;
    FProjectGen: TProjectGen;

    FProjectParser: TProjectParser;

    FInheritedTestFile: string;
    FTestUnitFile: string;
    FProjectCompiled: boolean;

    FCurrentStep: integer;
    FProgressFunc: TTestGenProgressFunc;
    FTotalTestTime: string;

    FSetupCode: string;

    FDisableExceptionLib: boolean;

    procedure InitSetupCode(const aClass: TInputTestClass; const aFunctionToTest: TInputFunction);
    procedure InitProjectUsesAndRequires(const sProjectFile: string);
    procedure SetUseTestFrameWork(const Value: TTestFrameWork);

    procedure SetProjectPath( const s: string );
    procedure SetSetupCode(const aFunctionToTest: TInputFunction);

    procedure PrepareInherit( const sUnit: string; const aClass: TInputTestClass; const bSave: boolean = True );
    procedure PrepareTestProject( const sProjectFile: string; const aClass: TInputTestClass = nil; const aFunctionToTest: TInputFunction = nil );
    procedure PrepareTestUnit( const aClass: TInputTestClass; const aFunctionToTest: TInputFunction; const bSave : boolean = true );

    procedure InformAboutNextStep( const sShortDesc: string );

    procedure InitFunctionTestResults(bAllFail: Boolean; const aFunctionToTest: TInputFunction);
    procedure SaveFunctionTestResultsFromRunlogDUnit(const aFunctionToTest: TInputFunction);
    procedure SaveFunctionTestResultsFromRunlogDUnitX(const aFunctionToTest: TInputFunction);
    procedure SaveTestResultsToXml(const aClass: TInputTestClass);
  public
    property ProjectCompiled: boolean
      read FProjectCompiled;
    property ProjectPath: string
      read FProjectPath write SetProjectPath;
    property ProjectGen: TProjectGen
      read FProjectGen;
    property UnitTestClassName: string
      read FUnitTestClassName write FUnitTestClassName;
    property OnProgress: TTestGenProgressFunc
      read FProgressFunc write FProgressFunc;
    property TotalTestTime: string
      read FTotalTestTime;
    property DisableExceptionLib: boolean
      read FDisableExceptionLib write FDisableExceptionLib;

    property ProjectParser: TProjectParser
      read FProjectParser write FProjectParser;

    property InheritedTestFile: string
      read FInheritedTestFile;

    property UseTestFrameWork: TTestFrameWork
      read FUseTestFrameWork write SetUseTestFrameWork;

    constructor Create;
    destructor Destroy; override;

    function GetProjectFile: string;

    function RunFunctionTests( const sProjectFile: string; const sUnit: string; const aClass: TInputTestClass; const aFunctionToTest: TInputFunction; bGenerateTestResultFiles: Boolean = True; bDeleteTestProject: boolean = True; bDontActuallyRunTest: boolean = false ): boolean;
    function GetTestCode( const sUnit: string; const aClass: TInputTestClass; const aFunctionToTest: TInputFunction ): string;

    procedure DeleteTestProjectFiles;
  end;

implementation

uses
  StrUtils, SysUtils, uCommonFunctions, Variants, uPascalDefs, VarUtils,
  Windows, uD7Functions, uTestgripInstall, uDUnitTestGen, uDUnitXTestGen;

const
  c_inhfileprefix = 'TESTGRIP_inhtest';
  c_unitfileprefix = 'TESTGRIP_unittest';


{ TTestGen }

constructor TTestGen.Create;
var
  sTimeStr: string;
begin
  inherited Create;

  FProjectParser := nil;

  FProgressFunc := nil;
  FCurrentStep := 0;

  FInheritedGen := nil;
  FUnitTestGen := nil;
  FProjectGen := TProjectGen.Create;

  FProjectPath := '';
  FTestResultPath := '';

  FTotalTestTime := '';

  FSetupCode := '';

  FProjectCompiled := False;

  sTimeStr := FormatDateTime('mmddhhnnss',Now);
  FInheritedTestFile := c_inhfileprefix + sTimeStr + '.pas';
  FTestUnitFile := c_unitfileprefix + sTimeStr + '.pas';

  FUseTestFrameWork := tfwDUnit;
  FUnitTestClassName := c_inhclassname;
end;

function TTestGen.GetTestCode( const sUnit: string; const aClass: TInputTestClass; const aFunctionToTest: TInputFunction ): string;
begin
  SetSetupCode( aFunctionToTest );

  PrepareInherit( sUnit, aClass, False );
  PrepareTestUnit( aClass, aFunctionToTest, False );

  Result := FUnitTestGen.GetTemplateCode;
end;

procedure TTestGen.SetSetupCode(const aFunctionToTest: TInputFunction);
begin
  FSetupCode := '';
  if (assigned(aFunctionToTest)) then
  begin
    if (aFunctionToTest.UseCustomSetupCode) then
      FSetupCode := aFunctionToTest.CustomSetupCode
    else if assigned(aFunctionToTest.Parent) and aFunctionToTest.Parent.UseCustomSetupCode then
      FSetupCode := aFunctionToTest.Parent.CustomSetupCode;
  end;
end;

procedure TTestGen.SetUseTestFrameWork(const Value: TTestFrameWork);
begin
  FUseTestFrameWork := Value;

  if Assigned(FProjectGen) then
    FProjectGen.UseTestFramework := Value;
end;

procedure TTestGen.DeleteTestProjectFiles;
var
  iError: Cardinal;
  iRetriesLeft: integer;
  ProjectPathAndName: string;
begin
  iRetriesLeft := 2;

  if FileExists(FProjectGen.ExeOutputPath + FProjectGen.ProjectName + '.exe') then
  begin
    if not SysUtils.DeleteFile(FProjectGen.ExeOutputPath + FProjectGen.ProjectName + '.exe') then
    begin
      // wait a second in case the file is still in use
      iError := GetLastError;
      while (iError = 5) and (iRetriesLeft > 0) do
      begin
        Sleep(500);
        if SysUtils.DeleteFile(FProjectGen.ExeOutputPath + FProjectGen.ProjectName + '.exe') then
        begin
          break;
        end;
        Dec(iRetriesLeft);
      end;
    end;
  end;

  ProjectPathAndName := FProjectGen.ProjectPath + FProjectGen.ProjectName;

  if FileExists(ProjectPathAndName + '.dpr') then
  begin
    SysUtils.DeleteFile(ProjectPathAndName + '.dpr');
  end;
  if FileExists(ProjectPathAndName + '.drc') then
  begin
    SysUtils.DeleteFile(ProjectPathAndName + '.drc');
  end;
  if FileExists(ProjectPathAndName + '.map') then
  begin
    SysUtils.DeleteFile(ProjectPathAndName + '.map');
  end;
  if FileExists(ProjectPathAndName + '.tds') then
  begin
    SysUtils.DeleteFile(ProjectPathAndName + '.tds');
  end;

  if FileExists(FProjectGen.ProjectPath + FTestUnitFile) then
  begin
    SysUtils.DeleteFile(FProjectGen.ProjectPath + FTestUnitFile);
  end;
  if FileExists(FProjectGen.ProjectPath + ChangeFileExt(FTestUnitFile,'.dcu')) then
  begin
    SysUtils.DeleteFile(FProjectGen.ProjectPath + ChangeFileExt(FTestUnitFile,'.dcu'));
  end;
  if FileExists(FProjectGen.ProjectPath + FInheritedTestFile) then
  begin
    SysUtils.DeleteFile(FProjectGen.ProjectPath + FInheritedTestFile);
  end;
  if FileExists(FProjectGen.ProjectPath + ChangeFileExt(FInheritedTestFile,'.dcu')) then
  begin
    SysUtils.DeleteFile(FProjectGen.ProjectPath + ChangeFileExt(FInheritedTestFile,'.dcu'));
  end;
 end;

procedure TTestGen.InitProjectUsesAndRequires(const sProjectFile: string);
var
  projsource: TUnitParser;
  bIsDpk: Boolean;
begin
  projsource := TUnitParser.Create;
  try
    bIsDpk := SameText(ExtractFileExt(sProjectFile), '.dpk');
    projsource.ParseFromFile(sProjectFile, False, True, bIsDpk);
    FProjectGen.UsesFiles.AddStrings(projsource.OuterUsesList);
    if bIsDpk then
    begin
      FProjectGen.Requires.AddStrings(projsource.Requires);
    end;
  finally
    projsource.Free;
  end;
end;

procedure TTestGen.InitSetupCode(const aClass: TInputTestClass; const aFunctionToTest: TInputFunction);
begin
  FSetupCode := '';

  if Assigned(aClass) then
  begin
    if aClass.UseCustomSetupCode then
    begin
      FSetupCode := aClass.CustomSetupCode;
    end;
  end;

  if Assigned(aFunctionToTest) then
  begin
    if aFunctionToTest.UseCustomSetupCode then
    begin
      FSetupCode := aFunctionToTest.CustomSetupCode;
    end;
  end;
end;

procedure TTestGen.SaveFunctionTestResultsFromRunlogDUnit(const aFunctionToTest: TInputFunction);
var
  c: Integer;
  iTestNumber: Integer;
  i: Integer;
  sLine: string;
  sPrefix: string;
  iPrefixLen: Integer;
  p1: Integer;
  p3: Integer;
  p4: Integer;
  p5: Integer;
  sTestResult: string;
  p2: Integer;
begin
  iPrefixLen := 0;

  c := FProjectGen.RunLog.Count - 1;
  iTestNumber := 0;
  for i := 0 to c do
  begin
    sLine := FProjectGen.RunLog[i];
    if StartsText('Time: ', sLine) then
    begin
      FTotalTestTime := Trim(Copy(sLine, 7));
    end;

    sPrefix := ') Test_';
    iPrefixLen := 0;
    p1 := Pos(sPrefix, sLine);
    if p1 <> 0 then
    begin
      iPrefixLen := Length(') Test_');
    end
    else
    begin
      sPrefix := '] ' + FUnitTestClassName + '.Test_';
      p1 := Pos(sPrefix, sLine);
      if p1 <> 0 then
      begin
        iPrefixLen := Length(sPrefix);
      end;
    end;

    if p1 <> 0 then
    begin
      if iTestNumber <> 0 then
      begin
        p3 := iPrefixLen + Length(IntToStr(iTestNumber) + ': ');
        p4 := Pos(sPrefix + IntToStr(iTestNumber) + ': ', FProjectGen.RunLogText);
        p5 := Pos(sPrefix + IntToStr(iTestNumber + 1) + ': ', FProjectGen.RunLogText);
        sTestResult := Copy(FProjectGen.RunLogText, p4 + p3, p5 - p4 + p3);
        aFunctionToTest.MarkTestResultString(iTestNumber, sTestResult);
      end;

      sTestResult := '';
      p2 := PosEx(':', sLine, p1);
      if p2 <> 0 then
      begin
        iTestNumber := StrToIntDef(Copy(sLine, p1 + iPrefixLen, p2 - p1 - iPrefixLen), -1);
        if iTestNumber <> -1 then
        begin
          aFunctionToTest.MarkTestResult(iTestNumber, False);
        end;
      end;
    end;
  end;

  if (iTestNumber <> 0) then
  begin
    p3 := iPrefixLen + Length(IntToStr(iTestNumber) + ': ');
    p4 := Pos(sPrefix + IntToStr(iTestNumber) + ': ', FProjectGen.RunLogText);
    sTestResult := Copy(FProjectGen.RunLogText, p4 + p3);
    aFunctionToTest.MarkTestResultString(iTestNumber, sTestResult);
  end;

  if (iTestNumber = 0) and (ContainsText(FProjectGen.RunLogText, 'Runtime error ')) then
  begin
    c := aFunctionToTest.TestCount;
    for i := 1 to c do
    begin
      aFunctionToTest.MarkTestResult(i, False);
      aFunctionToTest.MarkTestResultString(i, '');
    end;
    aFunctionToTest.MarkTestResultString(1, FProjectGen.RunLogText);
  end;
end;

procedure TTestGen.SaveFunctionTestResultsFromRunlogDUnitX(const aFunctionToTest: TInputFunction);
begin
  // todo: write different parser for DUnitX logging (or for the XML output?)
  SaveFunctionTestResultsFromRunlogDUnit(aFunctionToTest);
end;

procedure TTestGen.InitFunctionTestResults(bAllFail: Boolean; const aFunctionToTest: TInputFunction);
var
  i, c: Integer;
begin
  // default = test passed
  c := aFunctionToTest.TestCount;
  for i := 1 to c do
  begin
    if bAllFail then
    begin
      aFunctionToTest.MarkTestResult(i, false);
      aFunctionToTest.MarkTestResultString(i, '');
    end
    else
    begin
      aFunctionToTest.MarkTestResult(i, FProjectCompiled);
      aFunctionToTest.MarkTestResultString(i, '');
    end;
  end;
end;

procedure TTestGen.SaveTestResultsToXml(const aClass: TInputTestClass);
var
  sTestgripPath: string;
begin
  try
    // Save to test folder
    SaveTestResultFile(FTestResultPath + 'result_' + FProjectGen.ProjectName + '.xml', FTestUnitFile, aClass, FTotalTestTime);

    // Add layout files if needed
    if not FileExists(FTestResultPath + 'output.css') then
    begin
      sTestgripPath := TestgripInstallPath;
      CopyFile(PChar(sTestgripPath + 'templates\output.xsl'), PChar(FTestResultPath + 'output.xsl'), false);
      CopyFile(PChar(sTestgripPath + 'templates\output.js'), PChar(FTestResultPath + 'output.js'), false);
      CopyFile(PChar(sTestgripPath + 'templates\jquery.js'), PChar(FTestResultPath + 'jquery.js'), false);
      CopyFile(PChar(sTestgripPath + 'templates\output.css'), PChar(FTestResultPath + 'output.css'), false);
    end;
  except
    // ignore
  end;
end;

destructor TTestGen.Destroy;
begin
  FInheritedGen.Free;
  FUnitTestGen := nil;
  FProjectGen.Free;

  inherited;
end;

function TTestGen.GetProjectFile: string;
begin
  Result := '';
  if Assigned(FProjectGen) then
  begin
    Result := FProjectPath + FProjectGen.ProjectName + '.dpr';
  end;
end;

procedure TTestGen.InformAboutNextStep(const sShortDesc: string);
begin
  Inc(FCurrentStep);

  if Assigned(FProgressFunc) then
  begin
    FProgressFunc( Self, FCurrentStep, 5, sShortDesc );
  end;
end;

procedure TTestGen.PrepareInherit(const sUnit: string;
  const aClass: TInputTestClass; const bSave: boolean);
begin
  if Assigned(aClass) then
  begin
    FInheritedGen := TInheritGen.Create( sUnit, aClass.ClassName );
  end
  else
  begin
    FInheritedGen := TInheritGen.Create( sUnit, '' );
  end;

  if bSave then
  begin
    FInheritedGen.SaveAs( FProjectPath + FInheritedTestFile );
  end;
end;

procedure TTestGen.PrepareTestProject(const sProjectFile: string; const aClass: TInputTestClass; const aFunctionToTest: TInputFunction);
var
  sDprDpkFile: string;
begin
  FProjectGen.ProjectPath := FProjectPath;
  if FDisableExceptionLib then
  begin
    FProjectGen.MadExceptInstalled := False;
    FProjectGen.JCLInstalled := False;
  end;

  sDprDpkFile := sProjectFile;
  if Assigned(FProjectParser) then
  begin
    sDprDpkFile := FProjectParser.EntryFile;

    if FProjectParser.OutputDir <> '' then
    begin
      if StartsStr('.', FProjectParser.OutputDir) then
      begin
        FProjectGen.ExeOutputPath := FProjectGen.ProjectPath + FProjectParser.OutputDir;
      end
      else
      begin
        FProjectGen.ExeOutputPath := FProjectParser.OutputDir;
      end;
    end;
  end;

  if sProjectFile <> '' then
  begin
    InitProjectUsesAndRequires(sProjectFile);
  end;

  InitSetupCode(aClass, aFunctionToTest);

  FProjectGen.UsesFiles.Add( TCommonFileNameFunctions.RemoveFileExtension(FInheritedTestFile) + ' in ''' + FInheritedTestFile + '''' );
  FProjectGen.UsesFiles.Add( TCommonFileNameFunctions.RemoveFileExtension(FTestUnitFile) + ' in ''' + FTestUnitFile + '''' );

  if Assigned(FProjectParser) then
  begin
    FProjectGen.ResourcePaths.DelimitedText := FProjectParser.BRCC_IncludePath.DelimitedText + ';' + FProjectParser.BRCC_OutputDir.DelimitedText;

    FProjectGen.ProjectBrowsePaths.DelimitedText := FProjectParser.BRCC_IncludePath.DelimitedText + ';' + FProjectParser.SearchPath.DelimitedText;

    if FProjectParser.Namespaces.DelimitedText <> '' then
    begin
      FProjectGen.Namespaces := FProjectParser.Namespaces.DelimitedText;
    end;

    FProjectGen.ExtraAlias.AddStrings( FProjectParser.ExtraAlias );
  end;

  FProjectGen.SetupCode := FSetupCode;
  FProjectGen.SaveProjectFile;
end;

procedure TTestGen.PrepareTestUnit(const aClass: TInputTestClass;
  const aFunctionToTest: TInputFunction; const bSave : boolean = true);
var
  i, c: integer;
  test: TInputTest;
begin
  if FUseTestFrameWork = tfwDUnit then
  begin
    FUnitTestGen := TDUnitTestClassFileGen.Create;
  end
  else if FUseTestFrameWork = tfwDUnitX then
  begin
    FUnitTestGen := TDUnitXTestClassFileGen.Create;
  end;

  Assert(Assigned(FUnitTestGen), 'Unknown TestFramework selected');

  if Assigned(aClass) and (aClass.ClassName <> '') then
  begin
    FUnitTestClassName := aClass.ClassName + 'Test';

    if not StartsText('T', FUnitTestClassName) then
      FUnitTestClassName := 'T' + FUnitTestClassName;
  end;

  FUnitTestGen.UnitTestClassName := FUnitTestClassName;

  // if the inherited unit uses utf8, we should probably use it too
  if Assigned(FInheritedGen) then
  begin
    FUnitTestGen.WriteUTF8BOM := FInheritedGen.WriteUTF8BOM;
  end;

  FUnitTestGen.SetupCode := FSetupCode;

  FUnitTestGen.OuterUses.Add( TCommonFileNameFunctions.RemoveFileExtension(FInheritedTestFile) );
  FUnitTestGen.OuterUses.AddStrings( FInheritedGen.UnitParser.OuterUsesList );
  FUnitTestGen.OuterUses.AddStrings( FInheritedGen.UnitParser.InnerUsesList );

  if Assigned(aClass) then
  begin
    FUnitTestGen.OuterUses.Text := Trim(FUnitTestGen.OuterUses.Text) + #13#10 + aClass.ExtraUses;
  end
  else
  begin
    FUnitTestGen.OuterUses.Text := Trim(FUnitTestGen.OuterUses.Text);
  end;

  if Assigned(aFunctionToTest) then
  begin
    c := aFunctionToTest.TestList.Count - 1;
    for i := 0 to c do
    begin
      test := TInputTest(aFunctionToTest.TestList[i]);

      FUnitTestGen.GenerateTest( aClass, aFunctionToTest, test, 'TinhTest', FInheritedGen.UnitParser.MethodList );
    end;
  end;

  if bSave then
    FUnitTestGen.SaveAs( FProjectPath + FTestUnitFile );
end;

function TTestGen.RunFunctionTests(const sProjectFile: string; const sUnit: string; const aClass: TInputTestClass; const aFunctionToTest: TInputFunction; bGenerateTestResultFiles: Boolean; bDeleteTestProject: boolean; bDontActuallyRunTest: boolean ): boolean;
var
  sLastCurDir: string;
  sCompileResult: string;
  s: ansistring;
  bAllFail: boolean;
begin
  FTotalTestTime := '';
  FCurrentStep   := 0;
  FSetupCode     := '';
  sLastCurDir    := GetCurrentDir;
  bAllFail       := False;

  SetSetupCode( aFunctionToTest );
  if Assigned(aFunctionToTest) then
  begin
    aFunctionToTest.TotalTestResult.Text := '';
  end;
  try
    SetCurrentDir( FProjectPath );

    InformAboutNextStep('Writing baseclass');
    PrepareInherit( sUnit, aClass );
    InformAboutNextStep('Writing tests');
    PrepareTestUnit( aClass, aFunctionToTest );
    InformAboutNextStep('Writing project');
    PrepareTestProject( sProjectFile, aClass, aFunctionToTest );

    FProjectGen.ExtraDefines := '';
    if Assigned(aClass) then
    begin
      FProjectGen.ExtraDefines := aClass.Defines;
    end;

    if Assigned(aFunctionToTest) then
    begin
      if aFunctionToTest.Defines <> '' then
      begin
        FProjectGen.ExtraDefines := aFunctionToTest.Defines;
      end;
    end;

    InformAboutNextStep('Compiling tests');
    try
      FProjectGen.CompileProject;
    except
      on E: ENotSupportedException do
      begin
        // ignore trial version error 'this version does not supposed command line compiling'
        FProjectGen.CompileLog.Text := E.Message;
      end;
    end;

    if Assigned(aFunctionToTest) then
    begin
      sCompileResult := FProjectGen.CompileLog.Text;

      aFunctionToTest.CompileResult.Text := sCompileResult;
      aFunctionToTest.CompileCommand.Text := FProjectGen.CompileCommand.Text;
      aFunctionToTest.CompileResult.Text  := FProjectGen.CompileLog.Text;
    end;

    if bDontActuallyRunTest then
    begin
      Result := True;
    end
    else
    begin

      if not FileExists( FProjectGen.ExeOutputPath + FProjectGen.ProjectName + '.exe') then
      begin
        Result := False;
        FProjectCompiled := false;
      end
      else
      begin
        FProjectCompiled := true;
        InformAboutNextStep('Running tests');
        if FProjectGen.RunProject = 0 then
        begin
          Result := True;
        end
        else
        begin
          Result := False;
        end;
      end;

      if Assigned(aFunctionToTest) then
      begin
        s := Trim(FProjectGen.RunLogText);
        if s = '' then
        begin
          bAllFail := True;
        end;

        aFunctionToTest.RunLog.Text := s;

        InitFunctionTestResults(bAllFail, aFunctionToTest);

        if FUseTestFrameWork = tfwDUnit then
        begin
          SaveFunctionTestResultsFromRunlogDUnit(aFunctionToTest);
        end
        else if FUseTestFrameWork = tfwDUnitX then
        begin
          SaveFunctionTestResultsFromRunlogDUnitX(aFunctionToTest);
        end;
      end;

      if bGenerateTestResultFiles then
      begin
        SaveTestResultsToXml(aClass);
      end;

    end;

    if bDeleteTestProject then
    begin
      DeleteTestProjectFiles;
    end;

  finally
    SetCurrentDir(sLastCurDir);
  end;
end;

procedure TTestGen.SetProjectPath(const s: string);
begin
  FProjectPath := ReplaceStr( s, '/', '\' );

  if FProjectPath <> '' then
  begin
    FProjectPath := IncludeTrailingPathDelimiter(FProjectPath);

    FTestResultPath := IncludeTrailingPathDelimiter(FProjectPath + 'testresults');
    if not DirectoryExists(FTestResultPath) then
    begin
      MkDir(FTestResultPath);
    end;
  end;
end;

end.

