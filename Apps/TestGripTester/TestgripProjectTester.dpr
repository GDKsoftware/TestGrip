program TestgripProjectTester;

{$APPTYPE CONSOLE}


uses
  SysUtils,
  Classes,
  StrUtils,
  Contnrs,
  uProjectParser in '..\..\Core\uProjectParser.pas',
  uInputParser in '..\..\Definition\uInputParser.pas',
  uTestDefs in '..\..\Definition\uTestDefs.pas',
  uConst in '..\..\Definition\uConst.pas',
  uTestGen in '..\..\Generation\uTestGen.pas',
  uPascalFileGen in '..\..\Generation\uPascalFileGen.pas',
  uProjectGen in '..\..\Generation\uProjectGen.pas',
  uInheritGen in '..\..\Generation\uInheritGen.pas',
  uUnitParser in '..\..\Core\uUnitParser.pas',
  uPascalDefs in '..\..\Core\uPascalDefs.pas',
  uDelphiRegistry in '..\..\Generation\uDelphiRegistry.pas',
  uProjectTesting in '..\..\Generation\uProjectTesting.pas',
  uTestgripInstall in '..\..\Generation\uTestgripInstall.pas',
  uCommonFunctions in '..\..\Shared\uCommonFunctions.pas',
  uD7Functions in '..\..\Shared\uD7Functions.pas',
  uUTF8Functions in '..\..\Shared\uUTF8Functions.pas',
  uXmlFuncs in '..\..\Shared\uXmlFuncs.pas',
  uUnitTestGenBase in '..\..\Generation\uUnitTestGenBase.pas',
  uUnitTestGenIntf in '..\..\Generation\uUnitTestGenIntf.pas',
  uDUnitTestGen in '..\..\Generation\uDUnitTestGen.pas',
  uDUnitXTestGen in '..\..\Generation\uDUnitXTestGen.pas';

var
  ProjectTester: TProjectTesting;
  TestableFiles: TStrings;
  ProjectFile: string;
  ProjectPath: string;
  CurrentAppPath: string;
  ProjectEntryFile: string;
  MayOutputMore: boolean;

procedure LoadAllTestableFiles;
begin
  ProjectTester.ListAllTestableFilesInProject( ProjectFile, TestableFiles, ProjectEntryFile );
end;

function RunClassTests( const sUnit: string; const aTestClass: TInputTestClass ): integer;
var
  s: string;
begin
  s := ProjectTester.RunAllTestsInClass( aTestClass, sUnit, ProjectEntryFile, nil );

  WriteLn(s);

  Result := ProjectTester.TestsFailed;
end;

function LoadAndRunAllTests( const sUnit: string; const sTestFile: string ): integer;
var
  lstTestClasses: TObjectList;
  aTestParser: TInputParser;
  i, c: integer;
begin
  Result := 0;

  lstTestClasses := TObjectList.Create(True);
  try
    aTestParser := TInputParser.Create( sTestFile, lstTestClasses);
    try
      if not aTestParser.ParseTestFile then
      begin
        Result := 1;
      end
      else
      begin

        c := lstTestClasses.Count - 1;
        for i := 0 to c do
        begin
          Result := Result + RunClassTests( sUnit, TInputTestClass(lstTestClasses[i]) );
        end;

      end;
    finally
      aTestParser.Free;
    end;
  finally
    lstTestClasses.Free;
  end;
end;

function RunTests: integer;
var
  i, c: integer;
  sFullPath: string;
  sTestFilePath: string;
begin
  Result := 0;

  c := TestableFiles.Count - 1;
  for i := 0 to c do
  begin
    sFullPath := ProjectPath + TestableFiles[i];
    sTestFilePath := ChangeFileExt(sFullPath, '.test');

    Writeln( sFullPath );
    Result := Result + LoadAndRunAllTests( sFullPath, sTestFilePath );
  end;
end;

function ProcessParams: boolean;
var
  i, c: integer;
  s: string;
begin
  Result := False;
  CurrentAppPath := ParamStr(0);

  ProjectTester.DisableExceptionLib := True;

  c := ParamCount;
  for i := 1 to c do
  begin
    s := ParamStr(i);
    if StartsStr('/', s) then
    begin
      if SameText(s, '/exceptlib') then
      begin
        ProjectTester.DisableExceptionLib := False;
      end
      else if SameText(s, '/istrial') then
      begin
        writeln('NO');
        MayOutputMore := False;
        Result := False;
        Exit;
      end
      else if SameText(s, '/dver') then
      begin
        WriteLn( TDelphiRegistry.GetDelphiVersion );
        MayOutputMore := False;
        Result := False;
        Exit;
      end
      else if SameText(s, '/dpath') then
      begin
        WriteLn( TDelphiRegistry.GetDelphiPath );
        MayOutputMore := False;
        Result := False;
        Exit;
      end
      else if StartsText('/undef:', s) then
      begin
        ProjectTester.UndefList.Add(Copy(s, 8));
      end
      else if StartsText('/skip:', s) then
      begin
        ProjectTester.SkipList.Add( Copy(s, 7) );
      end
      else if StartsText('/alias:', s) then
      begin
        ProjectTester.ExtraAlias.Add( Copy(s, 8) );
      end
      else if SameText('/keep', s) then
      begin
        ProjectTester.DeleteTestProject := False;
      end;
    end
    else
    begin
      ProjectFile := s;
      Result := True;
    end;
  end;
end;

begin
  MayOutputMore := True;
  
  TestableFiles := TStringList.Create;
  ProjectTester := TProjectTesting.Create;
  try
    try
      if ParamCount >= 1 then
      begin
        if ProcessParams then
        begin
          if FileExists(ProjectFile) then
          begin
            ProjectPath := ExtractFilePath(ProjectFile);

            LoadAllTestableFiles;

            ExitCode := RunTests;
          end
          else
          begin
            writeln('Projectfile does not exist (' + ProjectFile + ')');
            ExitCode := 1;
          end;
        end
        else
        begin
          if MayOutputMore then
          begin
            writeln('Testgrip Project Tester');
            writeln('testgripprojecttester.exe <path\to\project.dproj>');
          end;
        end;
      end
      else
      begin
        writeln('Testgrip Project Tester');
        writeln('testgripprojecttester.exe <path\to\project.dproj>');
      end;

    except
      on E: Exception do
      begin
        Writeln(E.ClassName, ': ', E.Message);
      end;
    end;
  finally
    TestableFiles.Free;
    ProjectTester.Free;
  end;
end.
