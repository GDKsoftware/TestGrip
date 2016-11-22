unit uDUnitXTestGen;

interface

uses
  Classes, uTestDefs,
  uPascalFileGen,
  uUnitTestGenIntf, uUnitTestGenBase, uCommonFunctions;

type
  TDUnitXTestClassFileGen = class(TUnitTestClassFileGenBase, IUnitTestClassFileGen)
  private
    procedure AddImpliesChecks(const ComposedTestFunction: TUnitTestFunctionGen; const ATest: TInputTest);
  protected
    function RequoteValueForCode(const AValue: string): string;

    procedure AddPreImpliesCode(const ComposedTestFunction: TUnitTestFunctionGen; const ATest: TInputTest);
    procedure AddCheckForTestResult(const ComposedTestFunction: TUnitTestFunctionGen; const AFunction: TInputFunction; const ATest: TInputTest; const sTestResultVar: string);
    procedure AddCustomInitializationCode(const ComposedTestFunction: TUnitTestFunctionGen; const AFunction: TInputFunction; const sTestClassName: string; const AClass: TInputTestClass; const ATest: TInputTest);
    procedure AddParameterInitializations(const ComposedTestFunction: TUnitTestFunctionGen; const ATest: TInputTest);
    procedure AddCustomVars(const ComposedTestFunction: TUnitTestFunctionGen; const AFunction: TInputFunction; const AClass: TInputTestClass; const ATest: TInputTest);
    function AddTestResultVarIfNeeded(const ComposedTestFunction: TUnitTestFunctionGen; const AFunction: TInputFunction; IsProcedure: Boolean; const AClass: TInputTestClass): string;
    procedure FixParameterType(const Parameter: TInputParam);

    function GenerateTemplate: string; override;

    function GenerateTestInterfaceSection: string;
    function GenerateTestImplementationSection: string;
  public
    procedure GenerateTest(const AClass: TInputTestClass; const AFunction: TInputFunction; const ATest: TInputTest; const sTestClassName: string; const lstFunctionRef: TList = nil); override;
    function GetTemplateCode: string; override;
  end;

implementation

uses
  StrUtils, SysUtils, Variants, uD7Functions;

{ TDUnitXTestClassFileGen }

function TDUnitXTestClassFileGen.GenerateTemplate: string;
var
  sTypeDef: string;
  sImplementation: string;
begin
  FOuterUses.Add('DUnitX.TestFramework');

  Result := inherited GenerateTemplate;

  // TypeDefs -> class
  sTypeDef := GenerateTestInterfaceSection;
  sImplementation := GenerateTestImplementationSection;

  // fill in template
  Result := ReplaceText( Result, '<TypeDefs>', sTypeDef );
  Result := ReplaceText( Result, '<Implementation>', sImplementation );
end;

procedure TDUnitXTestClassFileGen.GenerateTest(const AClass: TInputTestClass; const AFunction: TInputFunction;
  const ATest: TInputTest; const sTestClassName: string; const lstFunctionRef: TList);
var
  ComposedTestFunction: TUnitTestFunctionGen;
  ReturnType: string;
  TestIdx: Integer;
  IsProcedure: Boolean;
  c: Integer;
  Parameters: string;
  i: Integer;
  Parameter: TInputParam;
  sCall: string;
  sTestResultVar: string;
  CurrentTestMethodName: string;
begin
  ComposedTestFunction := TUnitTestFunctionGen.Create;
  FTests.Add(ComposedTestFunction);

  TestIdx := FTests.Count;

  CurrentTestMethodName := 'Test_' + AFunction.MethodName + IntToStr(TestIdx);
  ComposedTestFunction.Def := 'procedure ' + CurrentTestMethodName;

  ReturnType := GetReturnType(lstFunctionRef, AFunction);
  if ReturnType <> '' then
    AFunction.CachedType := ReturnType;

  IsProcedure := SameStr(ReturnType, 'void');

  ComposedTestFunction.Imp :=
    'procedure ' + FUnitTestClassName + '.' + CurrentTestMethodName + ';' + #13#10 +
    'var'#13#10 +
    '  TestObj: ' + sTestClassName + ';' + #13#10;

  sTestResultVar := AddTestResultVarIfNeeded(ComposedTestFunction, AFunction, IsProcedure, AClass);

  Parameters := '';
  c := aTest.ParamList.Count - 1;
  for i := 0 to c do
  begin
    Parameter := TInputParam(aTest.ParamList[i]);
    FixParameterType(Parameter);

    ComposedTestFunction.Imp := ComposedTestFunction.Imp + '  ' + Parameter.ParamName + ': ' + Parameter.ParamType + ';' + #13#10;

    if i <> 0 then
    begin
      Parameters := Parameters + ', ' + Parameter.ParamName;
    end
    else
    begin
      Parameters := Parameter.ParamName;
    end;
  end;

  AddCustomVars(ComposedTestFunction, AFunction, AClass, ATest);

  ComposedTestFunction.Imp := ComposedTestFunction.Imp +
    'begin'#13#10;

  ComposedTestFunction.Imp := ComposedTestFunction.Imp + '  TestObj := nil;' + #13#10;
  ComposedTestFunction.Imp := ComposedTestFunction.Imp + '  // start assignments' + #13#10;

  AddParameterInitializations(ComposedTestFunction, aTest);

  ComposedTestFunction.Imp := ComposedTestFunction.Imp + #13#10;

  ComposedTestFunction.Imp := ComposedTestFunction.Imp + '  // start initcode' + #13#10;

  AddCustomInitializationCode(ComposedTestFunction, AFunction, sTestClassName, AClass, ATest);

  ComposedTestFunction.Imp := ComposedTestFunction.Imp +
    '  Assert.IsNotNull(TestObj, ''TestObj has not been created yet'');' + #13#10;

  ComposedTestFunction.Imp := ComposedTestFunction.Imp +
  '  try' + #13#10;

  sCall := 'TestObj' + '.' + 'TEST_' + aFunction.MethodName + '(' + Parameters + ')';

  ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    // call' + #13#10;
  if not IsProcedure then
  begin
    ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    ' + sTestResultVar + ' := ' + sCall + ';' + #13#10 + #13#10;

    ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    // start test equals' + #13#10;

    AddCheckForTestResult(ComposedTestFunction, AFunction, ATest, sTestResultVar);
  end
  else
  begin
    ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    ' + sCall + ';' + #13#10;
  end;

  ComposedTestFunction.Imp := ComposedTestFunction.Imp + '' + #13#10;

  AddPreImpliesCode(ComposedTestFunction, ATest);
  AddImpliesChecks(ComposedTestFunction, ATest);

  ComposedTestFunction.Imp := ComposedTestFunction.Imp + '' + #13#10;

  ComposedTestFunction.Imp := ComposedTestFunction.Imp +
  '  finally' + #13#10 +
  '    TestObj.Free;' + #13#10 +
  '  end;'+ #13#10;

  ComposedTestFunction.Imp := ComposedTestFunction.Imp +
    'end;'#13#10;
end;

function TDUnitXTestClassFileGen.GetTemplateCode: string;
begin
  Result := GenerateTemplate;
end;

procedure TDUnitXTestClassFileGen.AddImpliesChecks(const ComposedTestFunction: TUnitTestFunctionGen; const ATest: TInputTest);
var
  i, c: Integer;
  implies: TInputImplier;
  p: Integer;
  sTmp: string;
  TrimmedEval: string;
begin
  ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    // start test implies'#13#10;
  c := aTest.Implies.Count - 1;
  for i := 0 to c do
  begin
    implies := TInputImplier(aTest.Implies[i]);

    TrimmedEval := implies.Eval;
    if StartsText('Assert.', TrimmedEval) then
    begin
      if not EndsText(';', TrimmedEval) then
        TrimmedEval := TrimmedEval + ';';

      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    ' + TrimmedEval + #13#10;
    end
    else
    begin
      if not ContainsStr(implies.Eval, '(') and not ContainsStr(implies.Eval, ')') and ContainsStr(implies.Eval, '=') then
      begin
        p := Pos('=', implies.Eval);
        sTmp := Copy(implies.Eval, p + 1);
        if not ContainsStr(implies.Eval, '''') then
        begin
          if SameText(Trim(sTmp), 'nil') then
          begin
            ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.IsNull(' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i + 1) + ''');'#13#10;
          end
          else
          begin
            ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual(' + Copy(implies.Eval, p + 1) + ', ' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i + 1) + ''');'#13#10;
          end;
        end
        else
        begin
          ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual(' + Copy(implies.Eval, p + 1) + ', ' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i + 1) + ''');'#13#10;
        end;
      end
      else if not ContainsStr(implies.Eval, '(') and not ContainsStr(implies.Eval, ')') and ContainsStr(implies.Eval, '<>') then
      begin
        p := Pos('<>', implies.Eval);
        sTmp := Copy(implies.Eval, p + 2);
        if not ContainsStr(implies.Eval, '''') then
        begin
          if SameText(Trim(sTmp), 'nil') then
          begin
            ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.IsNotNull(' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i + 1) + ''');'#13#10;
          end
          else
          begin
            ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual(' + Copy(implies.Eval, p + 2) + ', ' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i + 1) + ''');'#13#10;
          end;
        end
        else
        begin
          ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual(' + Copy(implies.Eval, p + 2) + ', ' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i + 1) + ''');'#13#10;
        end;
      end
      else
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.IsTrue( ' + implies.Eval + ', ''test_implies_' + IntToStr(i + 1) + ''' );'#13#10;
      end;
    end;
  end;
end;

procedure TDUnitXTestClassFileGen.AddPreImpliesCode(const ComposedTestFunction: TUnitTestFunctionGen; const ATest: TInputTest);
var
  lstInitCode: TStringList;
  LineIdx, LineCount: Integer;
begin
  if ATest.PreImpliesCode <> '' then
  begin
    ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    // start pre-implies code'#13#10;

    lstInitCode := TStringList.Create;
    try
      lstInitCode.Text := ATest.PreImpliesCode;

      LineCount := lstInitCode.Count - 1;
      for LineIdx := 0 to LineCount do
      begin
        lstInitCode.Strings[LineIdx] := '    ' + lstInitCode.Strings[LineIdx];
      end;

      ComposedTestFunction.Imp := ComposedTestFunction.Imp + lstInitCode.Text;
    finally
      lstInitCode.Free;
    end;

    ComposedTestFunction.Imp := ComposedTestFunction.Imp + #13#10;
  end;
end;

procedure TDUnitXTestClassFileGen.AddCheckForTestResult(const ComposedTestFunction: TUnitTestFunctionGen; const AFunction: TInputFunction; const ATest: TInputTest; const sTestResultVar: string);
var
  sQuotedStr: string;
begin
  if SameText(aFunction.CachedType, 'string') or SameText(aFunction.CachedType, 'ansistring') or SameText(aFunction.CachedType, 'widestring') then
  begin
    if VarIsNull(aTest.Equals) then
    begin
      aTest.Equals := '';
    end;

    sQuotedStr := RequoteValueForCode(aTest.Equals);
    if aTest.EqualsNot then
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ''' + sQuotedStr + ''', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end
    else
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ''' + sQuotedStr + ''', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end;
  end
  else if SameText(aFunction.CachedType, 'boolean') then
  begin
    if SameText('true', aTest.Equals) then
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.IsTrue( ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end
    else if SameText('false', aTest.Equals) then
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.IsFalse( ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end
    else
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end;
  end
  else if SameText(aFunction.CachedType, 'integer') or SameText(aFunction.CachedType, 'longint') or SameText(aFunction.CachedType, 'int64') or SameText(aFunction.CachedType, 'double') or SameText(aFunction.CachedType, 'float') or SameText(aFunction.CachedType, 'extended') then
  begin
    if TCommonStringFunctions.IsNumeric('' + aTest.Equals) then
    begin
      if aTest.EqualsNot then
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end
      else
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end;
    end
    else
    begin
      // hex, oct, etc????
      if aTest.EqualsNot then
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end
      else
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end;
    end;
  end
  else if SameText(aFunction.CachedType, 'char') or SameText(aFunction.CachedType, 'ansichar') or SameText(aFunction.CachedType, 'widechar') then
  begin
    if StartsText('#', '' + aTest.Equals) then
    begin
      if aTest.EqualsNot then
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end
      else
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end;
    end
    else
    begin
      if aTest.EqualsNot then
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual( ''' + aTest.Equals + ''', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end
      else
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ''' + aTest.Equals + ''', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end;
    end;
  end
  else if SameText(aFunction.CachedType, 'tdatetime') then
  begin
    if TCommonStringFunctions.IsNumeric(aTest.Equals) then
    begin
      if aTest.EqualsNot then
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual( ' + aTest.Equals + ' * 1.0, Double(' + sTestResultVar + '), 0, ''test_equals'' );' + ''#13''#10'';
      end
      else
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ' + aTest.Equals + ' * 1.0, Double(' + sTestResultVar + '), 0, ''test_equals'' );' + ''#13''#10'';
      end;
    end
    else if aTest.EqualsNot then
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual( ''' + aTest.Equals + ''', FormatDateTime(''yyyy-mm-dd hh:nn:ss'', ' + sTestResultVar + '), ''test_equals'' );' + ''#13''#10'';
    end
    else
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ''' + aTest.Equals + ''', FormatDateTime(''yyyy-mm-dd hh:nn:ss'', ' + sTestResultVar + '), ''test_equals'' );' + ''#13''#10'';
    end;
  end
  else if VarIsNull(aTest.Equals) then
  begin
    if aTest.EqualsNot then
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual( '''', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end
    else
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( '''', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end;
  end
  else
  begin
    if TCommonStringFunctions.IsNumeric('' + aTest.Equals) then
    begin
      if aTest.EqualsNot then
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end
      else
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end;
    end
    else if StartsText('nil', '' + aTest.Equals) then
    begin
      if aTest.EqualsNot then
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.IsNotNull( ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end
      else
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.IsNull( ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
      end;
    end
    else if SameText('true', aTest.Equals) then
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.IsTrue( ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end
    else if SameText('false', aTest.Equals) then
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.IsFalse( ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end
    else if aTest.EqualsNot then
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreNotEqual( ''' + Copy('' + aTest.Equals, 2) + ''', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end
    else
    begin
      ComposedTestFunction.Imp := ComposedTestFunction.Imp + '    Assert.AreEqual( ''' + aTest.Equals + ''', ' + sTestResultVar + ', ''test_equals'' );' + ''#13''#10'';
    end;
  end;
end;

function TDUnitXTestClassFileGen.RequoteValueForCode(const AValue: string): string;
begin
  Result := ReplaceStr('' + AValue, '''', '''''');
  Result := ReplaceStr(Result, #13#10, ''' + #13#10 + ' + #1 + '''');
  Result := ReplaceStr(Result, #10, ''' + #10 + ' + #1 + '''');
  Result := ReplaceStr(Result, #13, ''' + #13 + ' + #1 + '''');
  Result := ReplaceStr(Result, #1, #13#10);
end;

procedure TDUnitXTestClassFileGen.AddCustomInitializationCode(const ComposedTestFunction: TUnitTestFunctionGen; const AFunction: TInputFunction; const sTestClassName: string; const AClass: TInputTestClass; const ATest: TInputTest);
var
  CustomInitializationCode: string;
  lstInitCode: TStringList;
  InitLineCount: Integer;
  InitLineIdx: Integer;
begin
  CustomInitializationCode := '';
  if aClass.InitCode <> '' then
  begin
    CustomInitializationCode := aClass.InitCode + ''#13''#10'';
  end;
  if aFunction.InitCode <> '' then
  begin
    CustomInitializationCode := aFunction.InitCode + ''#13''#10'';
  end;
  if aTest.InitCode <> '' then
  begin
    CustomInitializationCode := aTest.InitCode + ''#13''#10'';
  end;
  if aClass.ClassName <> '' then
  begin
    CustomInitializationCode := ReplaceText(CustomInitializationCode, aClass.ClassName + '.Create', sTestClassName + '.Create');
  end;

  lstInitCode := TStringList.Create;
  try
    lstInitCode.Text := CustomInitializationCode;

    InitLineCount := lstInitCode.Count - 1;
    for InitLineIdx := 0 to InitLineCount do
    begin
      lstInitCode.Strings[InitLineIdx] := '  ' + lstInitCode.Strings[InitLineIdx];
    end;

    ComposedTestFunction.Imp := ComposedTestFunction.Imp + lstInitCode.Text;
  finally
    lstInitCode.Free;
  end;
end;

procedure TDUnitXTestClassFileGen.FixParameterType(const Parameter: TInputParam);
begin
  if SameText(Parameter.ParamValue, 'nil') and SameText(Parameter.ParamType, 'Variant') then
  begin
    Parameter.ParamType := 'pointer';
  end;
end;

procedure TDUnitXTestClassFileGen.AddParameterInitializations(const ComposedTestFunction: TUnitTestFunctionGen; const ATest: TInputTest);
var
  Parameter: TInputParam;
  ParamIdx, ParamCount: Integer;
  InitializationValueInCode: string;
  QuotedStr : string;
begin
  ParamCount := aTest.ParamList.Count - 1;
  for ParamIdx := 0 to ParamCount do
  begin
    Parameter := TInputParam(ATest.ParamList[ParamIdx]);

    InitializationValueInCode := '';
    if SameText(Parameter.ParamType, 'string') or SameText(Parameter.ParamType, 'ansistring') or SameText(Parameter.ParamType, 'widestring') then
    begin
      QuotedStr := RequoteValueForCode(Parameter.ParamValue);

      InitializationValueInCode := '''' + QuotedStr + '''';
    end
    else if SameText(Parameter.ParamType, 'char') or SameText(Parameter.ParamType, 'ansichar') or SameText(Parameter.ParamType, 'widechar') then
    begin
      if StartsStr('''', Parameter.ParamValue) and EndsStr('''', Parameter.ParamValue) then
      begin
        InitializationValueInCode := Parameter.ParamValue;
      end
      else if StartsText( '#', '' + Parameter.ParamValue ) then
      begin
        InitializationValueInCode := Parameter.ParamValue;
      end
      else
      begin
        InitializationValueInCode := '''' + Parameter.ParamValue + '''';
      end;
    end
    else if SameText(Parameter.ParamType, 'tdatetime') then
    begin
      if TCommonStringFunctions.IsNumeric(Parameter.ParamValue) then
      begin
        InitializationValueInCode := 'TDateTime(' + Parameter.ParamValue + ')';
      end
      else if StartsStr('''', Parameter.ParamValue) and EndsStr('''', Parameter.ParamValue) then
      begin
        InitializationValueInCode := 'StrToDateTime(''' + Parameter.ParamValue + ''')';
      end
      else
      begin
        InitializationValueInCode := 'StrToDateTime(''' + Parameter.ParamValue + ''')';
      end;
    end
    else
    begin
      InitializationValueInCode := Parameter.ParamValue;
    end;

    ComposedTestFunction.Imp := ComposedTestFunction.Imp +
      '  ' + Parameter.ParamName + ' := ' + InitializationValueInCode + ';'#13#10;
  end;
end;

function TDUnitXTestClassFileGen.AddTestResultVarIfNeeded(const ComposedTestFunction: TUnitTestFunctionGen; const AFunction: TInputFunction; IsProcedure: Boolean; const AClass: TInputTestClass): string;
var
  TestResultVar: string;
begin
  TestResultVar := 'TestResult';

  if not IsProcedure then
  begin
    ComposedTestFunction.Imp := ComposedTestFunction.Imp + '  ' + TestResultVar + ': ' + AFunction.CachedType + ';' + ''#13''#10'';
  end;

  Result := TestResultVar;
end;

procedure TDUnitXTestClassFileGen.AddCustomVars(const ComposedTestFunction: TUnitTestFunctionGen; const AFunction: TInputFunction; const AClass: TInputTestClass; const ATest: TInputTest);
var
  Vars: string;
  VarsCode: TStringList;
  C: Integer;
  VarIdx: Integer;
begin
  Vars := '';

  if trim(aClass.Vars) <> '' then
    Vars := aClass.Vars;
  if trim(aFunction.Vars) <> '' then
    Vars := aFunction.Vars;
  if trim(aTest.Vars) <> '' then
    Vars := aTest.Vars;

  if Vars <> '' then
  begin
    VarsCode := TStringList.Create;
    try
      VarsCode.Text := Vars;

      C := VarsCode.Count - 1;
      for VarIdx := 0 to C do
      begin
        ComposedTestFunction.Imp := ComposedTestFunction.Imp + '  ' + Trim(VarsCode[VarIdx]) + #13#10;
      end;
    finally
      VarsCode.Free;
    end;
  end;
end;

function TDUnitXTestClassFileGen.GenerateTestImplementationSection: string;
var
  c: Integer;
  i: Integer;
  functiontest: TUnitTestFunctionGen;
  SetupProc: string;
  TearDownProc: string;
begin
  SetupProc :=
    'procedure ' + FUnitTestClassName + '.Setup;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10;

  TearDownProc :=
    'procedure ' + FUnitTestClassName + '.TearDown;'#13#10 +
    'begin'#13#10 +
    'end;'#13#10;

  Result := SetupProc + #13#10 + TearDownProc + #13#10;
  c := FTests.Count - 1;
  for i := 0 to c do
  begin
    functiontest := FTests[i];
    Result := Result + functiontest.Imp + #13#10;
  end;

  Result := Result + #13#10 +
    'initialization' + #13#10 +
    '  TDUnitX.RegisterTestFixture(' + FUnitTestClassName + ');';
end;

function TDUnitXTestClassFileGen.GenerateTestInterfaceSection: string;
var
  c, TestIdx: Integer;
  functiontest: TUnitTestFunctionGen;
begin
  Result :=
    '  [TestFixture]'#13#10 +
    '  ' + FUnitTestClassName + ' = class'#13#10 +
    '  protected'#13#10 +
    '  public'#13#10 +
    '    [Setup]'#13#10 +
    '    procedure Setup;'#13#10 +
    '    [TearDown]'#13#10 +
    '    procedure TearDown;'#13#10 +
    ''#13#10;

  c := FTests.Count - 1;
  for TestIdx := 0 to c do
  begin
    functiontest := FTests[TestIdx];

    Result := Result +
      '    [Test]'#13#10 +
      '    ' + functiontest.Def + ';' + #13#10;
  end;

  Result := Result + '  end;'#13#10;
end;

end.
