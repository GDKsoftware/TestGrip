unit WindmillPP.Tests;

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  Runner.Interfaces,
  Output.Interfaces,
  Input.Params.Interfaces;

type
  [TestFixture]
  TWindmillPPTests = class
  private
    FWindmill: IWindMillPP;
    FOutput: TMock<IOutput>;
    FInput: TMock<IInputParams>;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure ProjectTheWorld;
  end;

implementation

uses
  WindmillPP.CmdLine,
  System.Rtti;

procedure TWindmillPPTests.Setup;
begin
  FOutput := TMock<IOutput>.Create;
  FInput := TMock<IInputParams>.Create;

  FWindmill := TWindmillPPCmdLine.Create(FInput, FOutput);
end;

procedure TWindmillPPTests.ProjectTheWorld;
begin
  FInput.Setup.WillExecute(
    'Count',
    function(const args: TArray<TValue>; const ReturnType: TRttiType): TValue
    begin
      Result := 1;
    end);

  FInput.Setup.WillExecute(
    'Str',
    function(const args: TArray<TValue>; const ReturnType: TRttiType): TValue
    var
      Idx: Integer;
    begin
      Idx := args[1].AsType<Integer>;
      if Idx = 1 then
      begin
        Result := '../install/demoproject/TheWorld.dproj';
      end
      else
      begin
        Result := '';
      end;
    end);

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Exactly('Warning', 3);
  FOutput.Setup.Expect.Never('Error');

  FWindmill.Go;

  FOutput.Verify;
end;

end.
