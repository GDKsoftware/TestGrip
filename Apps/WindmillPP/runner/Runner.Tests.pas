unit Runner.Tests;

interface

uses
  uPascalDefs,
  System.Classes,
  DUnitX.TestFramework,
  Delphi.Mocks,
  Output.Interfaces,
  Runner.Interfaces;

type
  [TestFixture]
  TRunnerTests = class
  private
    FOutput: TMock<IOutput>;
    FRunner: IRunner;

  public
    [Setup]
    procedure Setup;

    [Teardown]
    procedure Teardown;

    [Test]
    procedure Test1;
  end;


implementation

uses
  Runner.Default,
  System.Rtti;

procedure TRunnerTests.Setup;
begin
  FOutput := TMock<IOutput>.Create;
//  FRunner := T
end;

procedure TRunnerTests.Teardown;
begin

end;

procedure TRunnerTests.Test1;
var
  CallCount: Integer;
begin
  CallCount := 0;

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Exactly('Warning', 3);
  FOutput.Setup.Expect.Never('Error');

  FOutput.Setup.WillExecute(
    'Warning',
    function(const args: TArray<TValue>; const ReturnType: TRttiType): TValue
    var
      Filepath: string;
      LineNumber: Integer;
      TextMessage: string;
    begin
      Inc(CallCount);
      Filepath := args[1].AsType<string>;
      LineNumber := args[2].AsType<Integer>;
      TextMessage := args[3].AsType<string>;
      case callCount of
        1:
        begin
          Assert.AreEqual(LineNumber, 301);
        end;
        2:
        begin
          Assert.AreEqual(LineNumber, 337);
        end;
        3:
        begin
          Assert.AreEqual(LineNumber, 350);
        end;
      end;
    end);

  FRunner := TRunnerDefault.Create(FOutput);
  FRunner.Execute('../install/demoproject/uWorld.pas');

  FOutput.Verify;
end;

end.
