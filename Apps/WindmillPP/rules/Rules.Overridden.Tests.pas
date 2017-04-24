unit Rules.Overridden.Tests;

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
  TRulesOverriddenTests = class
  private
    FOutput: TMock<IOutput>;
    FRules: IRulesOnMethod;

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
    procedure OkCode;

    [Test]
    procedure UnassignedInheritedCode;

    [Test]
    procedure BadCode;
  end;

implementation

uses
  System.SysUtils,
  Rules.Overridden;

procedure TRulesOverriddenTests.Setup;
begin
  FOutput := TMock<IOutput>.Create;
  FRules := TRulesOverridden.Create(FOutput);
  FCurrentCode := TStringList.Create;

  FCurrentMethod := TMethodDefinition.Create('function DoSomethingWithMyObject: Integer', True, TClassScope.csPublic);
  FCurrentMethod.IsOverride := True;
end;

procedure TRulesOverriddenTests.Teardown;
begin
  FreeAndNil(FCurrentCode);
  FreeAndNil(FCurrentMethod);
end;

procedure TRulesOverriddenTests.OkCode;
begin
  FCurrentCode.Text :=
    'var'#13#10 +
    '  MyObject: TMyObject;'#13#10 +
    'begin'#13#10 +
    '  MyObject := TMyObject.Create;'#13#10 +
    '  try'#13#10 +
    '    Result := inherited + MyObject.DoSomething;'#13#10 +
    '  finally'#13#10 +
    '    MyObject.Free;'#13#10 +
    '  end;'#13#0 +
    'end;';

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Never('Warning');
  FOutput.Setup.Expect.Never('Error');

  FRules.Process(FCurrentFile, FCurrentClass, FCurrentMethod, FCurrentCode);

  FOutput.Verify;
end;

procedure TRulesOverriddenTests.UnassignedInheritedCode;
begin
  FCurrentCode.Text :=
    'var'#13#10 +
    '  MyObject: TMyObject;'#13#10 +
    'begin'#13#10 +
    '  MyObject := TMyObject.Create;'#13#10 +
    '  try'#13#10 +
    '    inherited;'#13#10 +
    '    Result := MyObject.DoSomething;'#13#10 +
    '  finally'#13#10 +
    '    MyObject.Free;'#13#10 +
    '  end;'#13#0 +
    'end;';

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Once('Warning');
  FOutput.Setup.Expect.Never('Error');

  FRules.Process(FCurrentFile, FCurrentClass, FCurrentMethod, FCurrentCode);

  FOutput.Verify;
end;

procedure TRulesOverriddenTests.BadCode;
begin
  FCurrentCode.Text :=
    'var'#13#10 +
    '  MyObject: TMyObject;'#13#10 +
    'begin'#13#10 +
    '  MyObject := TMyObject.Create;'#13#10 +
    '  try'#13#10 +
    '    Result := MyObject.DoSomething;'#13#10 +
    '  finally'#13#10 +
    '    MyObject.Free;'#13#10 +
    '  end;'#13#0 +
    'end;';

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Once('Warning');
  FOutput.Setup.Expect.Never('Error');

  FRules.Process(FCurrentFile, FCurrentClass, FCurrentMethod, FCurrentCode);

  FOutput.Verify;
end;

end.
