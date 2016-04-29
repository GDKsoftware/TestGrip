unit uProjectTesting;

interface

uses
  Classes, uTestDefs, uTestGen, SysUtils, uProjectParser, Contnrs;

type
  TProjectTesting = class
  protected
    FProjectParser: TProjectParser;
    FProjectPath: string;
    FTestsPassed: integer;
    FTestsFailed: integer;

    FDeleteTestProject: boolean;
    FDisableExceptionLib: boolean;

    FShortListOfFailures: TStrings;

    FSkipList: TStrings;
    FExtraAlias: TStrings;
    FUndefList: TStrings;

    function InSkipList(const sFile: string): boolean;
  public
    property TestsFailed: integer
      read FTestsFailed;
    property TestsPassed: integer
      read FTestsPassed;
    property ShortListOfFailures: TStrings
      read FShortListOfFailures;
    property DisableExceptionLib: boolean
      read FDisableExceptionLib write FDisableExceptionLib;
    property DeleteTestProject: boolean
      read FDeleteTestProject write FDeleteTestProject;

    property SkipList: TStrings
      read FSkipList;
    property ExtraAlias: TStrings
      read FExtraAlias;
    property UndefList: TStrings
      read FUndefList;

    constructor Create;
    destructor Destroy; override;

    function ListAllTestableFilesInProject( const sProjectFile: string; const lstFiles: TStrings; var sEntryFile: string ): integer;

    function RunAllTestsInClass( const aTestClass: TInputTestClass; const sUnitFilename: string; const sProjectFilename: string; aCallBackProgress: TTestGenProgressFunc ): string;

  end;

implementation

uses
  StrUtils, uD7Functions;

{ TProjectTesting }

constructor TProjectTesting.Create;
begin
  FProjectParser := TProjectParser.Create;
  FShortListOfFailures := TStringList.Create;
  FSkipList := TStringList.Create;
  FExtraAlias := TStringList.Create;
  FUndefList := TStringList.Create;

  FDisableExceptionLib := False;
  FDeleteTestProject := True;
end;

destructor TProjectTesting.Destroy;
begin
  FUndefList.Free;
  FExtraAlias.Free;
  FSkipList.Free;
  FShortListOfFailures.Free;
  FProjectParser.Free;

  inherited;
end;

function TProjectTesting.InSkipList(const sFile: string): boolean;
var
  sFolder: string;
  i, c: integer;
begin
  Result := False;

  sFolder := ExtractFilePath(sFile);

  c := FSkipList.Count - 1;
  for i := 0 to c do
  begin
    Result := StartsText( FSkipList[i], sFolder );
    if Result then
    begin
      break;
    end;
  end;
end;

function TProjectTesting.ListAllTestableFilesInProject(const sProjectFile: string; const lstFiles: TStrings; var sEntryFile: string): integer;
var
  i, c: integer;
  sDProj: string;
  sUnit, sTestFile: string;
begin
  Result := 0;

  FProjectPath := ExtractFilePath(sProjectFile);
  sDProj := ChangeFileExt(sProjectFile, '.dproj');

  FProjectParser.ParseDProj( sDProj, false );

  FProjectParser.ExtraAlias.AddStrings(FExtraAlias);

  sEntryFile := FProjectParser.EntryFile;

  c := FProjectParser.Files.Count - 1;
  for i := 0 to c do
  begin
    sUnit := FProjectParser.Files[i];
    sTestFile := ChangeFileExt(sUnit, '.test');

    if not InSkipList(sTestFile) then
    begin

      if FileExists( FProjectPath + sTestFile ) then
      begin
        lstFiles.Add(sUnit);

        Inc(Result);
      end;

    end;
  end;
end;

function TProjectTesting.RunAllTestsInClass(
  const aTestClass: TInputTestClass; const sUnitFilename: string; const sProjectFilename: string; aCallBackProgress: TTestGenProgressFunc): string;
var
  func: TInputFunction;
  i, c: integer;
  aTestGen: TTestGen;
  bDeleteTestProject, bDontActuallyRunTest: boolean;
  lstErrors: TStrings;
  sTestResult: string;
  sTestErrors: string;
begin
  FTestsPassed := 0;
  FTestsFailed := 0;

  FShortListOfFailures.Text := '';

  bDeleteTestProject := FDeleteTestProject;
  bDontActuallyRunTest := false;

  sTestResult := '';
  Result := '';

  c := aTestClass.FunctionList.Count - 1;
  for i := 0 to c do
  begin
    func := TInputFunction(aTestClass.FunctionList[i]);

    aTestGen := TTestGen.Create;
    try
      aTestGen.OnProgress := aCallBackProgress;
      aTestGen.ProjectPath := ExtractFileDir(sProjectFilename);
      aTestGen.DisableExceptionLib := FDisableExceptionLib;
      aTestGen.ProjectParser := FProjectParser;
      aTestGen.ProjectGen.UndefList.AddStrings(FUndefList);
      try

        if func.TestCount > 0 then
        begin

          if not aTestGen.RunFunctionTests( sProjectFilename, sUnitFilename, aTestClass, func, true, bDeleteTestProject, bDontActuallyRunTest) then
          begin
            inc(FTestsFailed);

            if not aTestGen.ProjectCompiled then
            begin
              lstErrors := TStringlist.Create;
              try
                aTestGen.ProjectGen.ListCompileErrorsAndFatals(lstErrors);

                sTestErrors := lstErrors.Text;
              finally
                lstErrors.Free;
              end;
            end
            else
            begin
              sTestResult := func.TotalTestResult.Text;
            end;
          end
          else
          begin
            if func.TestResult then
            begin
              inc(FTestsPassed);
              sTestResult := func.TotalTestResult.Text;
            end
            else
            begin
              inc(FTestsFailed);
              sTestResult := func.TotalTestResult.Text;
            end;
          end;

          if aTestGen.ProjectCompiled then
          begin
            if aTestClass.Name <> '' then
            begin
              if func.TestResult then
              begin
                FShortListOfFailures.Add(aTestClass.Name + '.' + func.MethodName + ' - All tests passed');
              end
              else
              begin
                FShortListOfFailures.Add(aTestClass.Name + '.' + func.MethodName + ' - Some tests failed');
              end;

              Result := Result + #13#10#13#10 + aTestClass.Name + '.' + func.MethodName + #13#10 + aTestGen.ProjectGen.RunLog.Text;
            end
            else
            begin
              if func.TestResult then
              begin
                FShortListOfFailures.Add(func.MethodName + ' - All tests passed');
              end
              else
              begin
                FShortListOfFailures.Add(func.MethodName + ' - Some tests failed');
              end;

              Result := Result + #13#10#13#10 + func.MethodName + #13#10 + aTestGen.ProjectGen.RunLog.Text;
            end;
          end
          else
          begin
            if aTestClass.Name <> '' then
            begin
              Result :=
              Result + #13#10#13#10 + aTestClass.Name + '.' + func.MethodName + #13#10 +
//              aTestGen.ProjectGen.CompileCommand.Text + #13#10 +
              sTestErrors;

              FShortListOfFailures.Add(func.MethodName + ' - Tests failed to compile');
            end
            else
            begin
              Result := Result + #13#10#13#10 + func.MethodName + #13#10 +
//              aTestGen.ProjectGen.CompileCommand.Text + #13#10 +
              sTestErrors;

              FShortListOfFailures.Add(func.MethodName + ' - Tests failed to compile');
            end;
          end;
        end;
      except
        on E: Exception do
        begin
          Inc(FTestsFailed);

          Result := Result + #13#10 + E.Message;

          FShortListOfFailures.Add('Exception: ' + E.Message);
        end;
      end;

    finally
      aTestGen.Free;
    end;

  end;
end;

end.
