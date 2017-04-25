program WindmillUnitTests;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  Forms,
  Feeder.Interfaces in 'feeder\Feeder.Interfaces.pas',
  Runner.Interfaces in 'runner\Runner.Interfaces.pas',
  Output.Interfaces in 'output\Output.Interfaces.pas',
  Rules.Interfaces in 'rules\Rules.Interfaces.pas',
  uCodeHelpers in '..\..\Core\uCodeHelpers.pas',
  uDefinitionSearch in '..\..\Core\uDefinitionSearch.pas',
  uPascalDefs in '..\..\Core\uPascalDefs.pas',
  uProjectParser in '..\..\Core\uProjectParser.pas',
  uUnitParser in '..\..\Core\uUnitParser.pas',
  uUsedUnitSearch in '..\..\Core\uUsedUnitSearch.pas',
  uCommonFunctions in '..\..\Shared\uCommonFunctions.pas',
  uD7Functions in '..\..\Shared\uD7Functions.pas',
  WindmillPP.CmdLine in 'runner\WindmillPP.CmdLine.pas',
  Output.CmdLine in 'output\Output.CmdLine.pas',
  Feeder.Project in 'feeder\Feeder.Project.pas',
  Rules.Without in 'rules\Rules.Without.pas',
  Rules.Without.Tests in 'rules\Rules.Without.Tests.pas',
  Runner.Tests in 'runner\Runner.Tests.pas',
  Runner.Default in 'runner\Runner.Default.pas',
  Rules.Method.All in 'rules\Rules.Method.All.pas',
  uUnitLines in '..\..\Core\uUnitLines.pas',
  WindmillPP.Tests in 'runner\WindmillPP.Tests.pas',
  Input.Params.Interfaces in 'input\Input.Params.Interfaces.pas',
  Input.Params.CmdLine in 'input\Input.Params.CmdLine.pas',
  Rules.Overridden in 'rules\Rules.Overridden.pas',
  Rules.Overridden.Tests in 'rules\Rules.Overridden.Tests.pas',
  Rules.QueryStrings in 'rules\Rules.QueryStrings.pas',
  Rules.QueryStrings.Tests in 'rules\Rules.QueryStrings.Tests.pas';

type
  TTestGripExcept = class
  public
    procedure TestGripExceptionHandler( Sender : TObject; E : Exception );
  end;

procedure TTestGripExcept.TestGripExceptionHandler( Sender : TObject; E : Exception );
begin
  // ignore uncaught exceptions
end;


var
  runner : ITestRunner;
  TestResults : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
  TestResults := nil;
  try
    // Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    // Create the test runner
    Runner := TDUnitX.CreateRunner;
    // Tell the runner to use RTTI to find Fixtures
    Runner.UseRTTI := True;
    // Tell the runner how we will log things
    // Log to the console window
    Logger := TDUnitXConsoleLogger.Create(true);
    Runner.AddLogger(Logger);
    //Generate an NUnit compatible XML File
    NUnitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    Runner.AddLogger(NUnitLogger);



    //Run tests
    TestResults := Runner.Execute;
    if not TestResults.AllPassed then
      System.ExitCode := TestResults.ErrorCount + TestResults.FailureCount
    else
      System.ExitCode := 0;

  except
    on E: Exception do
    begin
      if Assigned(TestResults) then
      begin
        ExitCode := TestResults.ErrorCount + TestResults.FailureCount + 1;
      end
      else
      begin
        ExitCode := 1;
      end;
      System.Writeln( E.ClassName + ': ' + E.Message );
    end;
  end;

end.
