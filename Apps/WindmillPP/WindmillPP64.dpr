program WindmillPP64;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  uCodeHelpers in '..\..\Core\uCodeHelpers.pas',
  uDefinitionSearch in '..\..\Core\uDefinitionSearch.pas',
  uPascalDefs in '..\..\Core\uPascalDefs.pas',
  uProjectParser in '..\..\Core\uProjectParser.pas',
  uUnitParser in '..\..\Core\uUnitParser.pas',
  uUsedUnitSearch in '..\..\Core\uUsedUnitSearch.pas',
  uCommonFunctions in '..\..\Shared\uCommonFunctions.pas',
  uD7Functions in '..\..\Shared\uD7Functions.pas',
  Rules.Interfaces in 'rules\Rules.Interfaces.pas',
  Rules.Method.All in 'rules\Rules.Method.All.pas',
  Rules.Overridden in 'rules\Rules.Overridden.pas',
  Rules.Without in 'rules\Rules.Without.pas',
  Feeder.Interfaces in 'feeder\Feeder.Interfaces.pas',
  Feeder.Project in 'feeder\Feeder.Project.pas',
  Output.CmdLine in 'output\Output.CmdLine.pas',
  Output.Interfaces in 'output\Output.Interfaces.pas',
  Runner.Default in 'runner\Runner.Default.pas',
  Runner.Interfaces in 'runner\Runner.Interfaces.pas',
  WindmillPP.CmdLine in 'runner\WindmillPP.CmdLine.pas',
  uUnitLines in '..\..\Core\uUnitLines.pas',
  Input.Params.CmdLine in 'input\Input.Params.CmdLine.pas',
  Input.Params.Interfaces in 'input\Input.Params.Interfaces.pas',
  Rules.QueryStrings in 'rules\Rules.QueryStrings.pas';

var
  Mill: IWindmillPP;
  Output: IOutput;
begin
  Output := TOutputCmdLine.Create;

  Mill := TWindmillPPCmdLine.Create(TInputParamsCmdLine.Create, Output);
  Mill.Go;

  ExitCode := Output.WarningCount + Output.ErrorCount;
end.
