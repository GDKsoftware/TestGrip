unit Rules.QueryStrings.Tests;

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
  TRulesQueryStringsTests = class
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
    procedure BasicTest;

    [Test]
    procedure BasicTestWithIfElseEnd;

    [Test]
    procedure SQLAddTest;

    [Test]
    procedure OkSQLAddTest;
  end;

implementation

uses
  System.SysUtils,
  Rules.QueryStrings;

procedure TRulesQueryStringsTests.Setup;
begin
  FOutput := TMock<IOutput>.Create;
  FRules := TRulesQueryStrings.Create(FOutput);
  FCurrentCode := TStringList.Create;
  FCurrentMethod := TMethodDefinition.Create('procedure DoSomethingWithMyObject', True, TClassScope.csPublic);
end;

procedure TRulesQueryStringsTests.Teardown;
begin
  FreeAndNil(FCurrentCode);
  FreeAndNil(FCurrentMethod);
end;

procedure TRulesQueryStringsTests.BasicTest;
begin
  FCurrentCode.Text :=
    'var'#13#10 +
    '  Param1: string;'#13#10 +
    'begin'#13#10 +
    '  MyObject.SQL.Text := ''select * from mytable where param='' + Param1;'#13#10 +
    'end;';

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Never('Warning');
  FOutput.Setup.Expect.Once('Error');

  FRules.Process(FCurrentFile, FCurrentClass, FCurrentMethod, FCurrentCode);

  FOutput.Verify;
end;

procedure TRulesQueryStringsTests.BasicTestWithIfElseEnd;
begin
  FCurrentCode.Text :=
    'var'#13#10 +
    '  Param1: string;'#13#10 +
    'begin'#13#10 +
    '  if true then begin'#13#10 +
    '    MyObject.SQL.Text := ''select * from mytable where param=:Param1'''#13#10 +
    '  end if False then'#13#10 +
    '    MyObject.SQL.Text := ''select * from mytable where param=:Param1'''#13#10 +
    '  else'#13#10 +
    '    MyObject.SQL.Text := ''select * from mytable where param=:Param1'';'#13#10 +
    'end;';

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Never('Warning');
  FOutput.Setup.Expect.Never('Error');

  FRules.Process(FCurrentFile, FCurrentClass, FCurrentMethod, FCurrentCode);

  FOutput.Verify;
end;

procedure TRulesQueryStringsTests.SQLAddTest;
begin
  FCurrentCode.Text :=
    'begin'#13#10 +
    '  FDataset.SQL.Clear;'#13#10 +
    '  FDataset.SQL.Text := FSql_org.Text;'#13#10 +
    '  if pos(''WHERE'', UpperCase(FSql_org.Text)) > 0 then'#13#10 +
    '    FDataset.SQL.Add('' AND '' + volgorde.Strings[0] + '' >= '' + Edit_Van.Text +'#13#10 +
    '                     '' AND '' + volgorde.Strings[0] + '' <= '' + Edit_TotEnMet.Text + '')'#13#10 +
    '  else'#13#10 +
    '    FDataset.SQL.Add(''WHERE '' + volgorde.Strings[0] + '' >= '' + Edit_Van.Text +'#13#10 +
    '                     '' AND '' + volgorde.Strings[0] + '' <= '' + Edit_TotEnMet.Text + '');'#13#10 +
    ''#13#10 +
    '  if Edit_Volgorde.Text <> '' then'#13#10 +
    '     FDataset.SQL.Add(''Order By '' + Edit_Volgorde.Text);'#13#10 +
    ''#13#10 +
    '  FDataset.Active := True;' +
    'end;';

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.AtLeastOnce('Warning');
  FOutput.Setup.Expect.Exactly('Error', 2);

  FRules.Process(FCurrentFile, FCurrentClass, FCurrentMethod, FCurrentCode);

  FOutput.Verify;
end;

procedure TRulesQueryStringsTests.OkSQLAddTest;
begin
  FCurrentCode.Text :=
    'begin'#13#10 +
    '  Query.Sql.Add(''Delete from Bev_Special_rechten'');'#13#10 +
    'end;';

  FOutput.Setup.Expect.Never('Info');
  FOutput.Setup.Expect.Never('Warning');
  FOutput.Setup.Expect.Never('Error');

  FRules.Process(FCurrentFile, FCurrentClass, FCurrentMethod, FCurrentCode);

  FOutput.Verify;
end;

end.
