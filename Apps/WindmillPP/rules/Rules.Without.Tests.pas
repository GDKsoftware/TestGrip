unit Rules.Without.Tests;

interface

uses
  uPascalDefs,
  System.Classes,
  DUnitX.TestFramework,
  Delphi.Mocks,
  Output.Interfaces,
  Rules.Interfaces;

type
  [TestFixture]
  TRulesWithoutTests = class
  private
    FOutput: TMock<IOutput>;
    FRulesWithout: IRulesOnMethod;

    FCurrentMethod: TMethodDefinition;
    FCurrentCode: TStringList;

    FCurrentFile: string;
    FCurrentClass: TClassDefinition;

  public
    [Setup]
    procedure Setup;

    [Teardown]
    procedure Teardown;

    [Test]
    procedure Flawlesscode;

    [Test]
    procedure Badcode;

    [Test]
    procedure Comments;
  end;

implementation

uses
  System.SysUtils,
  Rules.Without;

procedure TRulesWithoutTests.Setup;
begin
  FOutput := TMock<IOutput>.Create;
  FRulesWithout := TRulesWithout.Create(FOutput);
  FCurrentCode := TStringList.Create;
  FCurrentMethod := TMethodDefinition.Create('procedure DoSomethingWithMyObject', True, TClassScope.csPublic);
end;

procedure TRulesWithoutTests.Teardown;
begin
  FreeAndNil(FCurrentCode);
  FreeAndNil(FCurrentMethod);
end;

procedure TRulesWithoutTests.Flawlesscode;
begin
  FCurrentCode.Text :=
    'var'#13#10 +
    '  MyObject: TMyObject;'#13#10 +
    'begin'#13#10 +
    '  MyObject := TMyObject.Create;'#13#10 +
    '  try'#13#10 +
    '    MyObject.DoSomething;'#13#10 +
    '  finally'#13#10 +
    '    MyObject.Free;'#13#10 +
    '  end;'#13#0 +
    'end;';

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Never('Warning');
  FOutput.Setup.Expect.Never('Error');

  FRulesWithout.Process(FCurrentFile, FCurrentClass, FCurrentMethod, FCurrentCode);

  FOutput.Verify;
end;

procedure TRulesWithoutTests.Badcode;
begin
  FCurrentCode.Text :=
    'begin'#13#10 +
    '  with TMyObject.Create do'#13#10 +
    '    MyObject.DoSomething;'#13#10 +
    'end;';

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Once('Warning');
  FOutput.Setup.Expect.Never('Error');

  FRulesWithout.Process(FCurrentFile, FCurrentClass, FCurrentMethod, FCurrentCode);

  FOutput.Verify;
end;

procedure TRulesWithoutTests.Comments;
begin
  FCurrentCode.Text :=
    'begin'#13#10 +
    '  // something something With something'#13#10 +
    'end;';

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Never('Warning');
  FOutput.Setup.Expect.Never('Error');

  FRulesWithout.Process(FCurrentFile, FCurrentClass, FCurrentMethod, FCurrentCode);

  FOutput.Verify;
end;

end.
