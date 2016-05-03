unit uDUnitTestGen;

interface

uses
  Classes,
  uUnitTestGenIntf, uUnitTestGenBase,
  uPascalFileGen, uTestDefs;

type
  { TDUnitTestClassFileGen }
  TDUnitTestClassFileGen = class(TUnitTestClassFileGenBase, IUnitTestClassFileGen)
  protected
    function GenerateTemplate: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GenerateTest( const aClass: TInputTestClass; const aFunction: TInputFunction; const aTest: TInputTest; const sTestClassName: string; const lstFunctionRef: TList = nil ); override;
    function GetTemplateCode() : string; override;
  end;

implementation

uses
  Contnrs, StrUtils, SysUtils, uPascalDefs, uCommonFunctions, Variants, uD7Functions;


{ TDUnitTestClassFileGen }

constructor TDUnitTestClassFileGen.Create;
begin
  inherited;

  FOuterUses.Add('TestFramework');
end;

destructor TDUnitTestClassFileGen.Destroy;
begin
  inherited;
end;

function TDUnitTestClassFileGen.GenerateTemplate: string;
var
  sTypeDef: string;
  sImplementation: string;
  i, c: Integer;
  functiontest: TUnitTestFunctionGen;
  sSetupProc: string;
begin
  Result := Inherited GenerateTemplate;

  // TypeDefs -> class

  sTypeDef := '';
  sImplementation := '';

  sTypeDef := sTypeDef +
    FUnitTestClassName + ' = class(TTestCase)' + #13#10 +
    'protected' + #13#10 +
    'public' + #13#10 +
    '  procedure SetUp; override;' + #13#10 +
    'published' + #13#10;

  c := FTests.Count - 1;
  for i := 0 to c do
  begin
    functiontest := FTests[i];

    sTypeDef := sTypeDef + '  ' + functiontest.Def + ';' + #13#10;
  end;

  sTypeDef := sTypeDef +
    'end;' + #13#10;

  // Implementation -> functions + testsuite initialization

  sSetupProc :=
    'procedure ' + FUnitTestClassName + '.SetUp;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10;

  sImplementation := sImplementation + sSetupProc + #13#10;


  c := FTests.Count - 1;
  for i := 0 to c do
  begin
    functiontest := FTests[i];

    sImplementation := sImplementation + functiontest.Imp + #13#10;
  end;

  sImplementation := sImplementation + #13#10 + 'initialization' + #13#10 + 'RegisterTest(' + FUnitTestClassName + '.Suite);';

  // fill in template

  Result := ReplaceText( Result, '<TypeDefs>', sTypeDef );
  Result := ReplaceText( Result, '<Implementation>', sImplementation );
end;

procedure TDUnitTestClassFileGen.GenerateTest(const aClass: TInputTestClass;
  const aFunction: TInputFunction; const aTest: TInputTest; const sTestClassName: string; const lstFunctionRef: TList);
var
  t: TUnitTestFunctionGen;
  iTestIndex: integer;
  i, c: integer;
  param: TInputParam;
  sCall: string;
  sParams: string;
  implies: TInputImplier;
  bIsProcedure: boolean;
  p: integer;
  sInitCode: string;
  sTestResultVar: string;
  sVars: string;
  lstInitCode: TStrings;
  lstVarsCode: TStrings;
  sQuotedStr: string;
  sTmp: string;
  ReturnType: string;
begin

  t := TUnitTestFunctionGen.Create;
  FTests.Add( t );

  iTestIndex := FTests.Count;

  t.Def := 'procedure Test_' + IntToStr(iTestIndex);

  t.Imp :=
    'procedure ' + FUnitTestClassName + '.Test_' + IntToStr(iTestIndex) + ';' + #13#10;

  ReturnType := GetReturnType(lstFunctionRef, aFunction);
  if ReturnType <> '' then
    aFunction.CachedType := ReturnType;

  bIsProcedure := SameStr(ReturnType, 'void');

  sTestResultVar := '';

  t.Imp := t.Imp + 'var' + #13#10;

  c := aTest.ParamList.Count - 1;
  if (c <> -1) or (aClass.ClassName <> '') or not SameText(aFunction.CachedType, 'void') then
  begin
    if not SameText(aFunction.CachedType, 'void') then
    begin
      sTestResultVar := 'testResult';
      t.Imp := t.Imp + '  ' + sTestResultVar + ': ' + aFunction.CachedType + ';' + #13#10;
    end;
  end;

  t.Imp := t.Imp + '  TestObj: ' + sTestClassName + ';' + #13#10;

  sParams := '';
  c := aTest.ParamList.Count - 1;
  for i := 0 to c do
  begin
    param := TInputParam(aTest.ParamList[i]);
    if SameText(param.ParamValue, 'nil') and SameText(param.ParamType, 'Variant') then
    begin
      param.ParamType := 'pointer';
    end;

    t.Imp := t.Imp + '  ' + param.ParamName + ': ' + param.ParamType + ';' + #13#10;
    if i <> 0 then
    begin
      sParams := sParams + ', ' + param.ParamName;
    end
    else
    begin
      sParams := param.ParamName;
    end;
  end;

  // begin custom vars
  if trim(aClass.Vars) <> '' then
    sVars := aClass.Vars;
  if trim(aFunction.Vars) <> '' then
    sVars := aFunction.Vars;
  if trim(aTest.Vars) <> '' then
    sVars := aTest.Vars;

  lstVarsCode := TStringList.Create;
  try
    lstVarsCode.Text := sVars;

    c := lstVarsCode.Count - 1;
    for i := 0 to c do
    begin
      t.Imp := t.Imp + '  ' + Trim(lstVarsCode[i]) + #13#10;
    end;
  finally
    lstVarsCode.Free;
  end;

  t.Imp := t.Imp +
    'begin' + #13#10;

  t.Imp := t.Imp + '  TestObj := nil;' + #13#10;
  t.Imp := t.Imp + '  // start assignments' + #13#10;


  c := aTest.ParamList.Count - 1;
  for i := 0 to c do
  begin
    param := TInputParam(aTest.ParamList[i]);

    if SameText(param.ParamType, 'string') or SameText(param.ParamType, 'ansistring') or SameText(param.ParamType, 'widestring') then
    begin
      sQuotedStr := ReplaceStr(param.ParamValue, '''', '''''');
      sQuotedStr := ReplaceStr(sQuotedStr, #13#10, ''' + #13#10 + ' + #1 + '''');
      sQuotedStr := ReplaceStr(sQuotedStr, #10, ''' + #10 + ' + #1 + '''');
      sQuotedStr := ReplaceStr(sQuotedStr, #13, ''' + #13 + ' + #1 + '''');
      sQuotedStr := ReplaceStr(sQuotedStr, #1, #13#10);

      t.Imp := t.Imp +
        '  ' + param.ParamName + ' := ''' + sQuotedStr + ''';' + #13#10;
    end
    else if SameText(param.ParamType, 'char') or SameText(param.ParamType, 'ansichar') or SameText(param.ParamType, 'widechar') then
    begin
      if StartsStr('''', param.ParamValue) and EndsStr('''', param.ParamValue) then
      begin
        t.Imp := t.Imp +
          '  ' + param.ParamName + ' := ' + param.ParamValue + ';' + #13#10;
      end
      else if StartsText( '#', '' + param.ParamValue ) then
      begin
        t.Imp := t.Imp +
          '  ' + param.ParamName + ' := ' + param.ParamValue + ';' + #13#10;
      end
      else
      begin
        t.Imp := t.Imp +
          '  ' + param.ParamName + ' := ''' + param.ParamValue + ''';' + #13#10;
      end;
    end
    else if SameText(param.ParamType, 'tdatetime') then
    begin
      if TCommonStringFunctions.IsNumeric(param.ParamValue) then
      begin
        t.Imp := t.Imp +
          '  ' + param.ParamName + ' := TDateTime(' + param.ParamValue + ');' + #13#10;
      end
      else if StartsStr('''', param.ParamValue) and EndsStr('''', param.ParamValue) then
      begin
        t.Imp := t.Imp +
          '  ' + param.ParamName + ' := StrToDateTime(' + param.ParamValue + ');' + #13#10;
      end
      else
      begin
        t.Imp := t.Imp +
          '  ' + param.ParamName + ' := StrToDateTime(''' + param.ParamValue + ''');' + #13#10;
      end;
    end
    else
    begin
      t.Imp := t.Imp +
        '  ' + param.ParamName + ' := ' + param.ParamValue + ';' + #13#10;
    end;
  end;

  t.Imp := t.Imp + #13#10;

  sInitCode := '';

  t.Imp := t.Imp + '  // start initcode' + #13#10;
  if aClass.InitCode <> '' then
  begin
    sInitCode := aClass.InitCode + #13#10;
  end;
  if aFunction.InitCode <> '' then
  begin
    sInitCode := aFunction.InitCode + #13#10;
  end;
  if aTest.InitCode <> '' then
  begin
    sInitCode := aTest.InitCode + #13#10;
  end;

  if aClass.ClassName <> '' then
  begin
    sInitCode := ReplaceText(sInitCode, aClass.ClassName + '.Create', sTestClassName + '.Create');
  end;

  lstInitCode := TStringList.Create;
  try
    lstInitCode.Text := sInitCode;

    c := lstInitCode.Count - 1;
    for i := 0 to c do
    begin
      lstInitCode.Strings[i] := '  ' + lstInitCode.Strings[i];
    end;

    t.Imp := t.Imp + lstInitCode.Text;
  finally
    lstInitCode.Free;
  end;

  t.Imp := t.Imp +
    '  Check(TestObj <> nil, ''TestObj has not been created yet'');' + #13#10;

  t.Imp := t.Imp +
  '  try' + #13#10;

  sCall := 'TestObj' + '.' + 'TEST_' + aFunction.MethodName + '(' + sParams + ')';

  t.Imp := t.Imp + '    // call' + #13#10;
  if not bIsProcedure then
  begin
    t.Imp := t.Imp + '    ' + sTestResultVar + ' := ' + sCall + ';' + #13#10 + #13#10;

    t.Imp := t.Imp + '    // start test equals' + #13#10;

    if SameText(aFunction.CachedType, 'string') or SameText(aFunction.CachedType, 'ansistring') or SameText(aFunction.CachedType, 'widestring') then
    begin
      if VarIsNull(aTest.Equals) then
      begin
        aTest.Equals := '';
      end;

      sQuotedStr := ReplaceStr('' + aTest.Equals, '''', '''''');
      sQuotedStr := ReplaceStr(sQuotedStr, #13#10, ''' + #13#10 + ' + #1 + '''');
      sQuotedStr := ReplaceStr(sQuotedStr, #10, ''' + #10 + ' + #1 + '''');
      sQuotedStr := ReplaceStr(sQuotedStr, #13, ''' + #13 + ' + #1 + '''');
      sQuotedStr := ReplaceStr(sQuotedStr, #1, #13#10);

      if aTest.EqualsNot then
      begin
        t.Imp := t.Imp + '    CheckNotEqualsString( ''' + sQuotedStr + ''', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end
      else
      begin
        t.Imp := t.Imp + '    CheckEqualsString( ''' + sQuotedStr + ''', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end;
    end
    else if SameText(aFunction.CachedType, 'boolean') then
    begin
      if SameText( 'true', aTest.Equals ) then
      begin
        t.Imp := t.Imp + '    CheckTrue( ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end
      else if SameText( 'false', aTest.Equals ) then
      begin
        t.Imp := t.Imp + '    CheckFalse( ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end
      else
      begin
        t.Imp := t.Imp + '    CheckEquals( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end;
    end
    else if SameText(aFunction.CachedType, 'integer') or SameText(aFunction.CachedType, 'longint') or SameText(aFunction.CachedType, 'int64') or  SameText(aFunction.CachedType, 'double') or SameText(aFunction.CachedType, 'float') or SameText(aFunction.CachedType, 'extended') then
    begin
      if TCommonStringFunctions.IsNumeric('' + aTest.Equals) then
      begin
        if aTest.EqualsNot then
        begin
          t.Imp := t.Imp + '    CheckNotEquals( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end
        else
        begin
          t.Imp := t.Imp + '    CheckEquals( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end;
      end
      else
      begin
        // hex, oct, etc????
        if aTest.EqualsNot then
        begin
          t.Imp := t.Imp + '    CheckNotEquals( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end
        else
        begin
          t.Imp := t.Imp + '    CheckEquals( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end;
      end;
    end
    else if SameText(aFunction.CachedType, 'char') or SameText(aFunction.CachedType, 'ansichar') or SameText(aFunction.CachedType, 'widechar') then
    begin
      if StartsText( '#', '' + aTest.Equals ) then
      begin
        if aTest.EqualsNot then
        begin
          t.Imp := t.Imp + '    CheckNotEquals( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end
        else
        begin
          t.Imp := t.Imp + '    CheckEquals( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end;
      end
      else
      begin
        if aTest.EqualsNot then
        begin
          t.Imp := t.Imp + '    CheckNotEquals( ''' + aTest.Equals + ''', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end
        else
        begin
          t.Imp := t.Imp + '    CheckEquals( ''' + aTest.Equals + ''', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end;
      end;
    end
    else if SameText(aFunction.CachedType, 'tdatetime') then
    begin
      if TCommonStringFunctions.IsNumeric(aTest.Equals) then
      begin
        if aTest.EqualsNot then
        begin
          t.Imp := t.Imp + '    CheckNotEquals( ' + aTest.Equals + ' * 1.0, Double(' + sTestResultVar + '), 0, ''test_equals'' );' + #13#10;
        end
        else
        begin
          t.Imp := t.Imp + '    CheckEquals( ' + aTest.Equals + ' * 1.0, Double(' + sTestResultVar + '), 0, ''test_equals'' );' + #13#10;
        end;
      end
      else if aTest.EqualsNot then
      begin
        t.Imp := t.Imp + '    CheckNotEqualsString( ''' + aTest.Equals + ''', FormatDateTime(''yyyy-mm-dd hh:nn:ss'', ' + sTestResultVar + '), ''test_equals'' );' + #13#10;
      end
      else
      begin
        t.Imp := t.Imp + '    CheckEqualsString( ''' + aTest.Equals + ''', FormatDateTime(''yyyy-mm-dd hh:nn:ss'', ' + sTestResultVar + '), ''test_equals'' );' + #13#10;
      end;
    end
    else if VarIsNull(aTest.Equals) then
    begin
      if aTest.EqualsNot then
      begin
        t.Imp := t.Imp + '    CheckNotEqualsString( '''', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end
      else
      begin
        t.Imp := t.Imp + '    CheckEqualsString( '''', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end;
    end
    else
    begin
      if TCommonStringFunctions.IsNumeric('' + aTest.Equals) then
      begin
        if aTest.EqualsNot then
        begin
          t.Imp := t.Imp + '    CheckNotEquals( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end
        else
        begin
          t.Imp := t.Imp + '    CheckEquals( ' + aTest.Equals + ', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end;
      end
      else if StartsText( 'nil', '' + aTest.Equals ) then
      begin
        if aTest.EqualsNot then
        begin
          t.Imp := t.Imp + '    CheckNotNull( ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end
        else
        begin
          t.Imp := t.Imp + '    CheckNull( ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
        end;
      end
      else if SameText( 'true', aTest.Equals ) then
      begin
        t.Imp := t.Imp + '    CheckTrue( ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end
      else if SameText( 'false', aTest.Equals ) then
      begin
        t.Imp := t.Imp + '    CheckFalse( ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end
      else if aTest.EqualsNot then
      begin
        t.Imp := t.Imp + '    CheckNotEqualsString( ''' + Copy('' + aTest.Equals,2) + ''', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end
      else
      begin
        t.Imp := t.Imp + '    CheckEqualsString( ''' + aTest.Equals + ''', ' + sTestResultVar + ', ''test_equals'' );' + #13#10;
      end;
    end;
  end
  else
  begin
    t.Imp := t.Imp + '    ' + sCall + ';' + #13#10;
  end;

  t.Imp := t.Imp + '' + #13#10;

  if aTest.PreImpliesCode <> '' then
  begin
    t.Imp := t.Imp + '    // start pre-implies code' + #13#10;

    lstInitCode := TStringList.Create;
    try
      lstInitCode.Text := aTest.PreImpliesCode;

      c := lstInitCode.Count - 1;
      for i := 0 to c do
      begin
        lstInitCode.Strings[i] := '    ' + lstInitCode.Strings[i];
      end;

      t.Imp := t.Imp + lstInitCode.Text;
    finally
      lstInitCode.Free;
    end;

    t.Imp := t.Imp + '' + #13#10;
  end;

  t.Imp := t.Imp + '    // start test implies' + #13#10;
  c := aTest.Implies.Count - 1;
  for i := 0 to c do
  begin
    implies := TInputImplier(aTest.Implies[i]);

    if not ContainsStr(implies.Eval, '(') and not ContainsStr(implies.Eval, ')') and ContainsStr(implies.Eval, '=') then
    begin
      p := Pos('=', implies.Eval);
      sTmp := Copy(implies.Eval, p + 1);
      if not ContainsStr(implies.Eval, '''') then
      begin
        if SameText(Trim(sTmp), 'nil') then
        begin
          t.Imp := t.Imp + '    CheckNull(' +  Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i+1) + ''');' + #13#10;
        end
        else
        begin
          t.Imp := t.Imp + '    CheckEquals(' + Copy(implies.Eval, p + 1) + ', ' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i+1) + ''');' + #13#10;
        end;
      end
      else
      begin
        t.Imp := t.Imp + '    CheckEqualsString(' + Copy(implies.Eval, p + 1) + ', ' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i+1) + ''');' + #13#10;
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
          t.Imp := t.Imp + '    CheckNotNull(' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i+1) + ''');' + #13#10;
        end
        else
        begin
          t.Imp := t.Imp + '    CheckNotEquals(' + Copy(implies.Eval, p + 2) + ', ' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i+1) + ''');' + #13#10;
        end;
      end
      else
      begin
        t.Imp := t.Imp + '    CheckNotEqualsString(' + Copy(implies.Eval, p + 2) + ', ' + Copy(implies.Eval, 1, p - 1) + ', ''test_implies_' + IntToStr(i+1) + ''');' + #13#10;
      end;
    end
    else
    begin
      t.Imp := t.Imp + '    CheckTrue( ' + implies.Eval + ', ''test_implies_' + IntToStr(i+1) + ''' );' + #13#10;
    end;
  end;

  t.Imp := t.Imp + '' + #13#10;

  t.Imp := t.Imp +
  '  finally' + #13#10 +
  '    TestObj.Free;' + #13#10 +
  '  end;'+ #13#10;


  t.Imp := t.Imp +
    'end;' + #13#10;
end;

function TDUnitTestClassFileGen.GetTemplateCode: string;
begin
  Result := GenerateTemplate;
end;

end.
