unit uProjectGen;

interface

uses
  Classes;

type
  TProjectGen = class
  protected
    FLatestDelphiRegistryKey: string;
    FProjectPath: string;

    FProjectName: string;
    FUsesFiles: TStringList;

    FRequires: TStrings;

    FDcc32Path: string;

    FCompileCommand: TStrings;
    FCompileLog: TStrings;
    FRunLog: TStrings;
    FFindError: TStrings;

    FExtraLogging: string;

    FBrowsePaths: TStrings;
    FLibraryPaths: TStrings;
    FResourcePaths: TStrings;
    FExtraAlias: TStrings;
    FUndefList: TStrings;

    FProjectBrowsePaths: TStrings;

    FDelphiPath: string;
    FDUnitSourcePath: string;

    FMadExceptInstalled: boolean;
    FJCLInstalled: boolean;
    FIgnoreUncaughtExceptions: boolean;

    FExtraDefines: string;

    FSetupCode: string;

    FRunLogText: ansistring;

    FNamespaces: string;

    FExeOutputPath: string;

    FMadExceptToolsPath: string;

    procedure SetProjectPath( const s: string );
    procedure SetExeOutputPath( const s: string );

    procedure RemoveDuplicateUses;

    function GetTestProjectDPRCode: string;
    function GetMadExceptSettings: string;
    function GetMSBuildBatchContent: string;
    procedure MadExceptPatch;

    procedure DetermineLatestDelphiRegistryKey;

    function ExpandVariables(const s: string): string;
  public
    property ProjectPath: string
      read FProjectPath write SetProjectPath;
    property LibraryPaths: TStrings
      read FLibraryPaths;

    property ProjectBrowsePaths: TStrings
      read FProjectBrowsePaths;

    property ResourcePaths: TStrings
      read FResourcePaths;

    property ExtraDefines: string
      read FExtraDefines write FExtraDefines;

    property CompileCommand: TStrings
      read FCompileCommand;
    property CompileLog: TStrings
      read FCompileLog;
    property RunLog: TStrings
      read FRunLog;
    property RunLogText: ansistring
      read FRunLogText;
    property FindErrorLog: TStrings
      read FFindError;

    property ExtraLogging: string
      read FExtraLogging;

    property UsesFiles: TStringList
      read FUsesFiles;
    property Requires: TStrings
      read FRequires;

    property Namespaces: string
      read FNamespaces write FNamespaces;
    property ExtraAlias: TStrings
      read FExtraAlias;

    property ProjectName: string
      read FProjectName;

    property SetupCode: string
      read FSetupCode write FSetupCode;

    property MadExceptInstalled: boolean
      read FMadExceptInstalled write FMadExceptInstalled;
    property JCLInstalled: boolean
      read FJCLInstalled write FJCLInstalled;

    property MadExceptToolsPath: string
      read FMadExceptToolsPath write FMadExceptToolsPath;

    property IgnoreUncaughtExceptions: boolean
      read FIgnoreUncaughtExceptions write FIgnoreUncaughtExceptions;

    property ExeOutputPath: string
      read FExeOutputPath write SetExeOutputPath;

    property UndefList: TStrings
      read FUndefList;

    constructor Create;
    destructor Destroy; override;

    procedure SaveProjectFile;
    procedure CompileProject;
    function RunProject: integer;


    function ListCompileErrorsAndFatals( const lst: TStrings ): integer;

  end;

implementation

uses
  DateUtils, StrUtils, SysUtils, ShellApi, uCommonFunctions, Windows, Registry,
  uDelphiRegistry, uD7Functions, uTestgripInstall;

{$I version.inc}

{ TProjectGen }


procedure TProjectGen.CompileProject;
var
  i, c: integer;
  j, d: integer;
  sCmd: string;
  sOut: string;
  sTraceDefine: string;
  sExtraDefines: string;
  sRequires: string;
  sNoConfig: string;
  sBrowsePaths: string;
  sResourcePaths: string;
  sLibPaths: string;
  s: string;
  sAliases: string;
begin
  FExtraLogging := '';

  if FExeOutputPath = '' then
  begin
    FExeOutputPath := FProjectPath;
  end;

  FExeOutputPath := ExpandVariables(FExeOutputPath);

  if FExeOutputPath <> '' then
  begin
    try
      ForceDirectories(FExeOutputPath);
    except
      on E: Exception do
      begin
        FExtraLogging := FExtraLogging + 'Creating path "' + FExeOutputPath + '" resulted in error: ' + E.Message + #13#10 + 'Reverting to projectpath for output: "' + FProjectPath + '"' + #13#10;

        FExeOutputPath := FProjectPath;
      end;
    end;
  end;

  sTraceDefine := '';
  if FMadExceptInstalled then
  begin
    sTraceDefine := ';madExcept';
  end
  else if FJCLInstalled then
  begin
    sTraceDefine := ';USE_JEDI_JCL';
  end;

  sExtraDefines := 'DEBUG;_CONSOLE_TEST_RUNNER' + sTraceDefine;
  if FExtraDefines <> '' then
  begin
    sExtraDefines := sExtraDefines + ';' + FExtraDefines;
  end;

  if not FMadExceptInstalled then
  begin
    // explicitly remove all madexcept references, it could be that they were set on the project or the extra defines on the test
  	FUndefList.Add('madExcept');
  end;
  
  c := FUndefList.Count - 1;
  for i := 0 to c do
  begin
    // todo: this messes up defines that are substrings of longer defines
    sExtraDefines := ReplaceText(sExtraDefines, FUndefList[i], '');
    sExtraDefines := ReplaceText(sExtraDefines, ';;', ';');
  end;

  sRequires := '';
  c := FRequires.Count - 1;
  for i := 0 to c do
  begin
    d := FLibraryPaths.Count - 1;
    for j := 0 to d do
    begin
      if FileExists(IncludeTrailingPathDelimiter(TCommonStringFunctions.RemoveDoubleQuotes(FLibraryPaths[j])) + FRequires[i] + '.dcp') then
      begin
        sRequires := sRequires + ' -LU' + '"' + IncludeTrailingPathDelimiter(TCommonStringFunctions.RemoveDoubleQuotes(FLibraryPaths[j])) + FRequires[i] + '.dcp"';
      end;
    end;
  end;

  c := FProjectBrowsePaths.Count - 1;
  for i := 0 to c do
  begin
    s := FProjectBrowsePaths[i];
    if s <> '' then
    begin
      FProjectBrowsePaths[i] := '"' + ExcludeTrailingPathDelimiter(TCommonStringFunctions.RemoveDoubleQuotes(s)) + '"';
    end;
  end;

  c := FLibraryPaths.Count - 1;
  for i := 0 to c do
  begin
    s := FLibraryPaths[i];
    if s <> '' then
    begin
      FLibraryPaths[i] := '"' + ExcludeTrailingPathDelimiter(TCommonStringFunctions.RemoveDoubleQuotes(s)) + '"';
    end;
  end;

  c := FResourcePaths.Count - 1;
  for i := 0 to c do
  begin
    s := FResourcePaths[i];
    if s <> '' then
    begin
      FResourcePaths[i] := '"' + ExcludeTrailingPathDelimiter(TCommonStringFunctions.RemoveDoubleQuotes(s)) + '"';
    end;
  end;

  sLibPaths := ReplaceText( Trim(FLibraryPaths.Text), #13#10, ';' );
  sBrowsePaths := ReplaceText( Trim(Trim(FProjectBrowsePaths.Text) + #13#10 + Trim(FLibraryPaths.Text)), #13#10, ';' );
  sResourcePaths := ReplaceText( Trim(Trim(FResourcePaths.Text) + #13#10 + Trim(FLibraryPaths.Text)), #13#10, ';' );

  sCmd :=
    IncludeTrailingPathDelimiter(FDelphiPath) + 'bin\' + 'dcc32'  +
    ' -D' + sExtraDefines +
    ' -V';

  sCmd := sCmd +
    ' -E"' + ExcludeTrailingPathDelimiter(FExeOutputPath) + '"' +
    ' -N0"' + ExcludeTrailingPathDelimiter(FExeOutputPath) + '"' +
    ' -NO"' + ExcludeTrailingPathDelimiter(FExeOutputPath) + '"';

  sCmd := sCmd +
    ' -I' + sBrowsePaths + '';

  // namespaces for Delphi XE2
  if FNamespaces <> '' then
  begin
    sCmd := sCmd +
      ' -NS"' + FNamespaces + '"';
  end;

  sAliases := '';

  {$ifdef VER230}
  sAliases := sAliases +
    ' -AGenerics.Collections=System.Generics.Collections;Generics.Defaults=System.Generics.Defaults;WinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE';
  {$endif}

  {$ifdef VER240}
  sAliases := sAliases +
    ' -AGenerics.Collections=System.Generics.Collections;Generics.Defaults=System.Generics.Defaults;WinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE';
  {$endif}

  if FExtraAlias.Count > 0 then
  begin
    if sAliases <> '' then
    begin
      sAliases := sAliases + ';' + ReplaceText( Trim(FExtraAlias.Text), #13#10, ';' );
    end
    else
    begin
      sAliases := ' -A' + ReplaceText( Trim(FExtraAlias.Text), #13#10, ';' );
    end;
  end;

  sCmd := sCmd + sAliases;

  sNoConfig := ''; // --no-config

  sCmd := sCmd +
    ' -O' + sLibPaths + '' +
    ' -U' + sBrowsePaths + '' +
    ' -R' + sResourcePaths + '' +
    sRequires +
    ' -$O-' +
    ' -$C-' +
    sNoConfig +
    ' ' + FProjectName + '.dpr';

  sOut := '';
  TCommonExecutionFunctions.ExecuteAndWaitAndGetStdOut(sCmd, SW_HIDE, sOut);

  if StartsText('This version of the product does not support command line compiling', sOut) then
  begin
    // user has a trial version that can't compile from command line
    raise ENotSupportedException.Create(Trim(sOut));
  end;

  FCompileCommand.Text := sCmd;
  FCompileLog.Text := sOut;
end;

constructor TProjectGen.Create;
begin
  FLatestDelphiRegistryKey := '';

  FUsesFiles := TStringList.Create;
  FUsesFiles.Duplicates := dupIgnore;
  FCompileCommand := TStringList.Create;
  FCompileLog := TStringList.Create;
  FRunLog := TStringList.Create;

  FRequires := TStringList.Create;
  FRequires.Delimiter := ';';

  FLibraryPaths := TStringList.Create;
  FLibraryPaths.Delimiter := ';';
  FBrowsePaths := TStringList.Create;
  FBrowsePaths.Delimiter := ';';
  FProjectBrowsePaths := TStringList.Create;
  FProjectBrowsePaths.Delimiter := ';';

  FExtraAlias := TStringList.Create;
  FExtraAlias.Delimiter := ';';
  
  FUndefList := TStringList.Create;
  FUndefList.Delimiter := ';';

  FResourcePaths := TStringList.Create;
  FResourcePaths.Delimiter := ';';

  FFindError := TStringList.Create;

  FProjectPath := '';

  FUsesFiles.Add('SysUtils');

  FUsesFiles.Add('TestFrameWork in ''' + TestgripInstallPath + 'Compatibility\TestFrameWork.pas' + '''');
  FUsesFiles.Add('TextTestRunner');
  FUsesFiles.Add('Forms');

  FProjectName := 'TESTGRIP_Test' + FormatDateTime('mmddhhnnss',Now);

  FMadExceptInstalled := False;
  FJCLInstalled := False;
  FIgnoreUncaughtExceptions := True;
  FSetupCode := '';
  FExeOutputPath := '';

  DetermineLatestDelphiRegistryKey;

  if ContainsText(FLibraryPaths.Text, 'madExcept') or ContainsText(FBrowsePaths.Text, 'madExcept') then
  begin
    FMadExceptInstalled := True;
  end;
  if ContainsText(FLibraryPaths.Text, 'jcl') then
  begin
    FJCLInstalled := True;
  end;

  // todo: find madexcept path some another way
  FMadExceptToolsPath := 'C:\Program Files\madCollection\madExcept\Tools\';

  FNamespaces := '';

  {$ifdef DVERXE}
  FNamespaces := 'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;Data.Win;System.Win;';
  {$endif}
end;

destructor TProjectGen.Destroy;
begin
  FreeAndNil(FUndefList);
  FreeAndNil(FExtraAlias);
  FreeAndNil(FResourcePaths);
  FreeAndNil(FFindError);
  FreeAndNil(FRunLog);
  FreeAndNil(FCompileLog);
  FreeAndNil(FCompileCommand);
  FreeAndNil(FRequires);
  FreeAndNil(FUsesFiles);
  FreeAndNil(FLibraryPaths);
  FreeAndNil(FBrowsePaths);
  FreeAndNil(FProjectBrowsePaths);

  inherited;
end;

function TProjectGen.ExpandVariables(const s: string): string;
var
  lstEnvVars: TStrings;
  j, d: integer;
begin
  lstEnvVars := TStringList.Create;
  try
    TCommonAppFunctions.GetAllEnvVars( lstEnvVars );

    lstEnvVars.Add('BDS=' + ExcludeTrailingPathDelimiter(FDelphiPath));
    lstEnvVars.Add('DELPHI=' + ExcludeTrailingPathDelimiter(FDelphiPath));

    if FDelphiPath <> '' then
    begin
      FDUnitSourcePath := IncludeTrailingPathDelimiter(FDelphiPath) + 'source\dUnit\src';
      if DirectoryExists(FDUnitSourcePath) then
      begin
        FLibraryPaths.Insert(0, FDUnitSourcePath);
      end;
    end;

    if lstEnvVars.IndexOfName('Platform') = -1 then
    begin
      lstEnvVars.Add('Platform=WIN32');
    end;
    if lstEnvVars.IndexOfName('Config') = -1 then
    begin
      lstEnvVars.Add('Config=DEBUG');
    end;

    Result := s;

    d := lstEnvVars.Count - 1;
    for j := 0 to d do
    begin
      if lstEnvVars.Names[j] <> '' then
      begin
        Result := ReplaceStr( Result, '$(' + lstEnvVars.Names[j] + ')', lstEnvVars.ValueFromIndex[j] );
      end;
    end;
  finally
    lstEnvVars.Free;
  end;
end;

procedure TProjectGen.DetermineLatestDelphiRegistryKey;
var
  reg: TRegistry;
  i, c: integer;
  sBrowsePathStr: string;
  sLibPathStr: string;
  lstEnvVars: TStringList;
  j, d: integer;
  bDebugPaths: boolean;
begin
  FLatestDelphiRegistryKey := '';

  sBrowsePathStr := '';
  sLibPathStr := '';

  bDebugPaths := false;

  FDelphiPath := '';
  FDUnitSourcePath := '';

  FDcc32Path := 'dcc32';

  reg := TRegistry.Create(KEY_READ);
  try
	reg.RootKey := HKEY_CURRENT_USER;
	
    if (FLatestDelphiRegistryKey = '') and reg.KeyExists(TDelphiRegistry.GetActiveDelphiRegistryKey) then
    begin
      if reg.OpenKeyReadOnly(TDelphiRegistry.GetActiveDelphiRegistryKey) then
      begin
        FDelphiPath := reg.ReadString('RootDir');

        FDcc32Path := IncludeTrailingPathDelimiter(FDelphiPath) + 'bin\dcc32.exe';
        if not FileExists(FDcc32Path) then
        begin
          raise Exception.Create('Couldn''t find dcc32.exe [' + FDcc32Path + ']');
        end;

        reg.CloseKey;
      end;

      // Delphi > XE2 has subdirectories per platform,
      if reg.OpenKeyReadOnly(TDelphiRegistry.GetActiveDelphiRegistryKey + '\Library\Win32') then
      begin
         FLatestDelphiRegistryKey := TDelphiRegistry.GetActiveDelphiRegistryKey;

         sBrowsePathStr := reg.ReadString('Browsing Path');
         if bDebugPaths then
         begin
           sLibPathStr := reg.ReadString('Debug DCU Path');
         end
         else
         begin
           sLibPathStr := reg.ReadString('Search Path');
         end;

         reg.CloseKey;
      end;

      if reg.OpenKeyReadOnly(TDelphiRegistry.GetActiveDelphiRegistryKey + '\Library') then
      begin
         FLatestDelphiRegistryKey := TDelphiRegistry.GetActiveDelphiRegistryKey;

         if sBrowsePathStr = '' then
         begin
           sBrowsePathStr := reg.ReadString('Browsing Path');
         end;

         if sLibPathStr = '' then
         begin
           if bDebugPaths then
           begin
             sLibPathStr := reg.ReadString('Debug DCU Path');
           end
           else
           begin
             sLibPathStr := reg.ReadString('Search Path');
           end;
         end;
      end;


    end;
  finally
    reg.Free;
  end;

  ExtractStrings([';'],[],PChar(sBrowsePathStr), FBrowsePaths);
  ExtractStrings([';'],[],PChar(sLibPathStr), FLibraryPaths);

  FLibraryPaths.Text := ReplaceStr(FLibraryPaths.Text,'"', '');
  FBrowsePaths.Text := ReplaceStr(FBrowsePaths.Text,'"', '');
  
  lstEnvVars := TStringList.Create;
  try
    TCommonAppFunctions.GetAllEnvVars( lstEnvVars );

    lstEnvVars.Add('BDS=' + ExcludeTrailingPathDelimiter(FDelphiPath));
    lstEnvVars.Add('DELPHI=' + ExcludeTrailingPathDelimiter(FDelphiPath));

    if FDelphiPath <> '' then
    begin
      FDUnitSourcePath := IncludeTrailingPathDelimiter(FDelphiPath) + 'source\dUnit\src';
      if DirectoryExists(FDUnitSourcePath) then
      begin
        FLibraryPaths.Insert(0, FDUnitSourcePath);
      end;
    end;

    if lstEnvVars.IndexOfName('Platform') = -1 then
    begin
      lstEnvVars.Add('Platform=WIN32');
    end;
    if lstEnvVars.IndexOfName('Config') = -1 then
    begin
      lstEnvVars.Add('Config=DEBUG');
    end;

    FLibraryPaths.Insert(0,IncludeTrailingPathDelimiter(FDelphiPath) + 'bin');
    c := FLibraryPaths.Count - 1;
    for i := 0 to c do
    begin
      // fix for XE, inconsistent variables
      if ContainsText(FLibraryPaths[i], '$(Platform)') then
      begin
        FLibraryPaths[i] := ReplaceStr( FLibraryPaths[i], '$(BDSLIB)\$(Platform)', '$(Platform)' );
        FLibraryPaths[i] := ReplaceStr( FLibraryPaths[i], '$(Platform)', '$(BDSLIB)\$(Platform)' );
      end;

      d := lstEnvVars.Count - 1;
      for j := 0 to d do
      begin
        if lstEnvVars.Names[j] <> '' then
        begin
          FLibraryPaths[i] := ReplaceStr( FLibraryPaths[i], '$(' + lstEnvVars.Names[j] + ')', lstEnvVars.ValueFromIndex[j] );
        end;
      end;

      FLibraryPaths[i] := '"' + ExcludeTrailingPathDelimiter( FLibraryPaths[i] ) + '"';
    end;

    c := FBrowsePaths.Count - 1;
    for i := 0 to c do
    begin
      d := lstEnvVars.Count - 1;
      for j := 0 to d do
      begin
        if lstEnvVars.Names[j] <> '' then
        begin
          FBrowsePaths[i] := ReplaceStr( FBrowsePaths[i], '$(' + lstEnvVars.Names[j] + ')', lstEnvVars.ValueFromIndex[j] );
        end;
      end;

      FBrowsePaths[i] := '"' + ExcludeTrailingPathDelimiter(FBrowsePaths[i]) + '"';
    end;

  finally
    lstEnvVars.Free;
  end;
end;

function TProjectGen.GetTestProjectDPRCode: string;
begin
  Result :=
    'program <TestProjectName>;' + #13#10 +
    '{$APPTYPE CONSOLE}' + #13#10 +
    'uses' + #13#10 +
    '<UsesFiles>;' + #13#10 +
    #13#10 +
    'type' + #13#10 +
    '  TTestGripExcept = class' + #13#10 +
    '  public' + #13#10 +
    '    procedure TestGripExceptionHandler( Sender : TObject; E : Exception );' + #13#10 +
    '  end;' + #13#10 +
    #13#10 +
    'procedure TTestGripExcept.TestGripExceptionHandler( Sender : TObject; E : Exception );' + #13#10 +
    'begin' + #13#10;

  if FIgnoreUncaughtExceptions then
  begin
    Result := Result +
      '  // ignore uncaught exceptions' + #13#10;
  end
  else
  begin
    Result := Result +
      '  System.WriteLn(''Exception: '' + E.Message );' + #13#10 +
      '  ExitCode := ExitCode + 1;' + #13#10;
  end;

  Result := Result +
    'end;' + #13#10 +
    #13#10 +
    'var R: TTestResult;' + #13#10 +
    'begin' + #13#10 +
//    '  ReportMemoryLeaksOnShutdown := True;' + #13#10 +
    '  R := nil;' + #13#10 +
    '  try' + #13#10 +
    '    Application.Initialize;' + #13#10 +
    '    Application.OnException := (TTestGripExcept.Create).TestGripExceptionHandler;' + #13#10 +
    #13#10 +
    '<SetupCode>' + #13#10 +
    #13#10 +
    '    R := TextTestRunner.RunRegisteredTests(rxbHaltOnFailures);' + #13#10 +
    '    ExitCode := R.ErrorCount + R.FailureCount;' + #13#10 +
    '  except' + #13#10 +
    '    on E: Exception do' + #13#10 +
    '    begin' + #13#10 +
    '      if Assigned(R) then' + #13#10 +
    '      begin' + #13#10 +
    '        ExitCode := R.ErrorCount + R.FailureCount + 1;' + #13#10 +
    '      end' + #13#10 +
    '      else' + #13#10 +
    '      begin' + #13#10 +
    '        ExitCode := 1;' + #13#10 +
    '      end;' + #13#10 +
    '      System.Writeln( E.ClassName + '': '' + E.Message );' + #13#10 +
    '    end;' + #13#10 +
    '  end;' + #13#10 +
    'end.' + #13#10;
end;

function TProjectGen.GetMadExceptSettings: string;
begin
  Result :=
  '[GeneralSettings]' + #13#10 +
  'HandleExceptions=1' + #13#10 +
  'AppendMapFileToBinary=1' + #13#10 +
  'NoOwnMadExceptSettings=0' + #13#10 +
  'CheckFileCrc=1' + #13#10 +
  'CheckForFrozenMainThread=0' + #13#10 +
  'FreezeTimeout=60000' + #13#10 +
  'AutomaticallySaveBugReport=1' + #13#10 +
  'AutoSaveBugReportIfNotSent=1' + #13#10 +
  'AutomaticallyMailBugReport=0' + #13#10 +
  'AutoMailProgressBox=0' + #13#10 +
  'CopyBugReportToClipboard=0' + #13#10 +
  'SuspendAllRunningThreads=0' + #13#10 +
  'ShowPleaseWaitBox=1' + #13#10 +
  'PleaseWaitIcon=plwait1' + #13#10 +
  'AutomaticallyContinueApplication=0' + #13#10 +
  'AutomaticallyRestartApplication=0' + #13#10 +
  'AutomaticallyCloseApplication=0' + #13#10 +
  'MailAddress=' + #13#10 +
  'SendInBackground=1' + #13#10 +
  'Send32Icon=send321' + #13#10 +
  'MailAsSmtpServer=0' + #13#10 +
  'MailAsSmtpClient=0' + #13#10 +
  'UploadViaHttp=0' + #13#10 +
  'MailViaMapi=1' + #13#10 +
  'MailViaMailto=1' + #13#10 +
  'SmtpServer=' + #13#10 +
  'SmtpPort=25' + #13#10 +
  'SmtpAccount=' + #13#10 +
  'SmtpPassword=' + #13#10 +
  'HttpServer=' + #13#10 +
  'HttpPort=80' + #13#10 +
  'HttpAccount=' + #13#10 +
  'HttpPassword=' + #13#10 +
  'BugReportFile=bugreport.txt' + #13#10 +
  'AttachBugReport=1' + #13#10 +
  'AttachBugReportFile=1' + #13#10 +
  'DeleteBugReportFile=1' + #13#10 +
  'BugReportSendAs=bugreport.txt' + #13#10 +
  'BugReportZip=' + #13#10 +
  'ScreenShotDepth=8' + #13#10 +
  'ScreenShotAppOnly=0' + #13#10 +
  'ScreenShotSendAs=screenshot.png' + #13#10 +
  'ScreenShotZip=' + #13#10 +
  'AdditionalAttachments=' + #13#10 +
  'AppendBugReports=1' + #13#10 +
  'BugReportFileSize=100000' + #13#10 +
  'DontSaveDuplicateExceptions=1' + #13#10 +
  'DontSaveDuplicateFreezings=1' + #13#10 +
  'DuplicateExceptionDefinition=1' + #13#10 +
  'DuplicateFreezeDefinition=2' + #13#10 +
  'ShowExceptionBox=1' + #13#10 +
  'OkBtnText=&OK' + #13#10 +
  'DetailsBtnText=&Details' + #13#10 +
  'PleaseWaitTitle=Information' + #13#10 +
  'PleaseWaitText=Please wait a moment...' + #13#10 +
  'MailSubject=bug report' + #13#10 +
  'MailBody=please find the bug report attached' + #13#10 +
  'SendBoxTitle=Sending bug report...' + #13#10 +
  'PrepareAttachMsg=Preparing attachments...' + #13#10 +
  'MxLookupMsg=Searching for mail server...' + #13#10 +
  'ConnectMsg=Connecting to server...' + #13#10 +
  'AuthMsg=Authentication...' + #13#10 +
  'SendMailMsg=Sending mail...' + #13#10 +
  'FieldsMsg=Setting fields...' + #13#10 +
  'SendAttachMsg=Sending attachments...' + #13#10 +
  'SendFinalizeMsg=Finalizing...' + #13#10 +
  'MailFailureMsg=Sorry, sending the bug report didn''t work.' + #13#10 +
  'VersionVariable=' + #13#10 +
  '[BugReport]' + #13#10 +
  'ListThreads=1' + #13#10 +
  'ListModules=1' + #13#10 +
  'ListHardware=1' + #13#10 +
  'ShowCpuRegisters=1' + #13#10 +
  'ShowStackDump=1' + #13#10 +
  'Disassembly=1' + #13#10 +
  'HideUglyItems=0' + #13#10 +
  'ShowRelativeAddrs=1' + #13#10 +
  'ShowRelativeLines=1' + #13#10 +
  'FormatDisassembly=0' + #13#10 +
  'LimitDisassembly=5' + #13#10 +
  'EnabledPlugins=modules|processes|hardware';
end;

function TProjectGen.GetMSBuildBatchContent: string;
begin
  Result :=
//    'set DCC_Quiet=true' +
    'set BDS=D:\Program Files\CodeGear\RAD Studio\6.0' + #13#10 +
    'set MSBuildBinPath=%WinDir%\Microsoft.NET\Framework\v3.5' + #13#10 +
    'call %MSBuildBinPath%\msbuild ' + FProjectName + '.dproj /nologo /t:rebuild /p:config=Debug %1 %2 %3 %4 %5' + #13#10;
    // 2007: /p:Configuration=Debug
end;

function TProjectGen.ListCompileErrorsAndFatals( const lst: TStrings ): integer;
var
  i, c: integer;
begin
  Result := 0;

  c := FCompileLog.Count - 1;
  for i := 0 to c do
  begin
    if (Pos('Error: ', FCompileLog[i]) <> 0) or (Pos('Fatal: ', FCompileLog[i]) <> 0) then
    begin
      lst.Add( FCompileLog[i] );
      Inc(Result);
    end;
  end;
end;

procedure TProjectGen.MadExceptPatch;
var
  sCmd: string;
  sOut: string;
begin
  sCmd :=
    FMadExceptToolsPath + 'madExceptPatch.exe "' + FProjectPath + FProjectName + '.exe"';

  sOut := '';
  TCommonExecutionFunctions.ExecuteAndWaitAndGetStdOut(sCmd, SW_HIDE, sOut);

  FCompileLog.Text := sOut;
end;

procedure TProjectGen.RemoveDuplicateUses;
var
  i, c: integer;
  p: integer;
  s: string;
begin
  i := 0;
  c := FUsesFiles.Count - 1;
  while i <= c do
  begin
    p := FUsesFiles.IndexOf( FUsesFiles[i] );
    while (p <> -1) and (p < i) do
    begin
      FUsesFiles.Delete(i);
      Dec(i);
      Dec(c);
      break;
    end;

    if (i <= c) and StartsText('vcl.', FUsesFiles[i]) then
    begin
      s := Copy( FUsesFiles[i], 5 );
      p := FUsesFiles.IndexOf(s);
      while (p <> -1) and (p < i) do
      begin
        FUsesFiles.Delete(i);
        Dec(i);
        Dec(c);
        break;
      end;
    end
    else if (i <= c) and StartsText('system.', FUsesFiles[i]) then
    begin
      s := Copy( FUsesFiles[i], 8 );
      p := FUsesFiles.IndexOf(s);
      while (p <> -1) and (p < i) do
      begin
        FUsesFiles.Delete(i);
        Dec(i);
        Dec(c);
        break;
      end;
    end;

    Inc(i);
  end;
end;

function TProjectGen.RunProject: integer;
const
  c_accessviolationstr = 'Access violation at address ';
var
  sOut: string;
  sError: string;
  p: integer;
begin
  sOut := '';

  Result := TCommonExecutionFunctions.ExecuteAndWaitAndGetStdOut('' + FExeOutputPath + FProjectName + '.exe', SW_HIDE, sOut);

  FRunLogText := sOut;
  FRunLog.Text := sOut;

  p := Pos(c_accessviolationstr, sOut);
  if p <> 0 then
  begin
    sError := Copy(sOut, p + Length(c_accessviolationstr), 8 );
  end;
end;

procedure TProjectGen.SaveProjectFile;
var
  sContent: string;
  f: TextFile;
  sUsesStr: string;
  i, c: integer;
begin
  sContent := GetTestProjectDPRCode;

  sContent := ReplaceText( sContent, '<TestProjectName>', FProjectName );
  
  RemoveDuplicateUses;

  sUsesStr := '';
  c := FUsesFiles.Count - 1;
  for i := 0 to c do
  begin
    if (i <> 0) then
    begin
      if ((i-1) >= 0) and StartsText('{$', FUsesFiles[i-1]) then
      begin
        // don't add , to compiler directives
        sUsesStr := sUsesStr + #13#10;
      end
      else
      begin
        sUsesStr := sUsesStr + ',' + #13#10;
      end;
    end;
    sUsesStr := sUsesStr + '  ' + FUsesFiles[i];
  end;

  sContent := ReplaceText( sContent, '<UsesFiles>', sUsesStr );
  sContent := ReplaceText( sContent, '<SetupCode>', FSetupCode );

  Assign( f, FProjectPath + FProjectName + '.dpr' );
  try
    Rewrite( f );

    Write( f, sContent );
  finally
    Close( f );
  end;
end;

procedure TProjectGen.SetExeOutputPath(const s: string);
begin
  FExeOutputPath := ReplaceStr( s, '/', '\' );

  if FExeOutputPath <> '' then
  begin
    FExeOutputPath := IncludeTrailingPathDelimiter(FExeOutputPath);
    FExeOutputPath := ReplaceStr(FExeOutputPath, '\.\', '\');
  end;
end;

procedure TProjectGen.SetProjectPath(const s: string);
begin
  FProjectPath := ReplaceStr( s, '/', '\' );

  if FProjectPath <> '' then
  begin
    FProjectPath := IncludeTrailingPathDelimiter(FProjectPath);
  end;
end;

end.
