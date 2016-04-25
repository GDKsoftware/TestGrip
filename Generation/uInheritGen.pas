unit uInheritGen;

interface

uses
  uUnitParser, Classes, Contnrs, uPascalFileGen, uPascalDefs;

type
  TInheritGen = class(TPascalFileGen)
  protected
    FUnitParser: TUnitParser;

    FInheritedClass: string;
    FOriginalUnit: string;
    FCustomClassname: string;

    FOverloadList: TStrings;

    FTestFunctions: TObjectList;

    FInterfaceMethods: TList;

    function GenerateTemplate: string; override;
    procedure ParsedMethodsToTest;

    function HasTestFunction(const sName: string): boolean;

    procedure CalcClassname;
  public
    property TestFunctions: TObjectList
      read FTestFunctions;
    property UnitParser: TUnitParser
      read FUnitParser;
    property CustomClassname: string
      read FCustomClassname write FCustomClassname;

    function AddFunction( const sName: string; const ft: string ): TMethodDefinition;
    function GetTemplateCode() : string;

    procedure IncludeInterfaceMethod(const AMethod: TMethodDefinition);
  
    constructor Create( const sFilename: string; const sClassToInherit: string );
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils, SysUtils, uCommonFunctions, uD7Functions;

const
  c_classinheritdef =
    '  <InhClassName> = class(<ParentClassName>)' + #13#10 +
    '  public' + #13#10 +
    '<InhFunctionDefinitions>' + #13#10 +
    '  end;' + #13#10;

  c_classinheritimpl =
    '<InhFunctionImplementations>' + #13#10;


{ TInheritGen }

function TInheritGen.AddFunction(const sName: string; const ft: string): TMethodDefinition;
begin
  Result := TMethodDefinition.Create( sName, ft );
  FTestFunctions.Add( Result );
end;

procedure TInheritGen.CalcClassname;
begin
  FPossibleClassname := 'TinhTest';

  FPossibleClassname := FPossibleClassname + DetermineClassSigniture(FInheritedClass, False);
end;

constructor TInheritGen.Create( const sFilename: string; const sClassToInherit: string );
var
  stream: TFileStream;
  sfname: string;
begin
  inherited Create;

  FCustomClassname := '';

  FInterfaceMethods := TList.Create;

  FOverloadList := TStringList.Create;

  FInheritedClass := sClassToInherit;

  FTestFunctions := TObjectList.Create(True);

  FUnitParser := TUnitParser.Create;

  sfname := ExtractFileName( ReplaceStr(sFilename,'/','\') );
  FOriginalUnit := TCommonFileNameFunctions.RemoveFileExtension(sfname);

  if sFilename <> '' then
  begin
    stream := TFileStream.Create( sFilename, fmOpenRead );
    try
      FUnitParser.ParseUnit( stream, True );

      // if inherited unit has utf8 characters in it, we should support it by using the same BOM
      FWriteUTF8BOM := FUnitParser.HasUTF8BOM;
    finally
      stream.Free;
    end;
  end;

  ParsedMethodsToTest;

  FOuterUses.AddStrings( FUnitParser.OuterUsesList );
  FOuterUses.Add(FOriginalUnit);
  FInnerUses.AddStrings( FUnitParser.InnerUsesList );
end;

destructor TInheritGen.Destroy;
begin
  FInterfaceMethods.Free;

  FOverloadList.Free;

  FTestFunctions.Free;

  FUnitParser.Free;
  
  inherited;
end;

function TInheritGen.GenerateTemplate: string;
var
  sFuncDefs: string;
  sFuncImpls: string;
  i, c: integer;
  sImpl: string;
  func: TMethodDefinition;
  bIsVoid: boolean;
begin
  Result := inherited GenerateTemplate;

  if FCustomClassname <> '' then
  begin
    FPossibleClassname := FCustomClassname;
  end
  else
  begin
    CalcClassname;
  end;

  Result := ReplaceText( Result, '<TypeDefs>', c_classinheritdef  );
  Result := ReplaceText( Result, '<Implementation>', c_classinheritimpl  );

  FUnitParser.OuterUsesList.Add( FOriginalUnit );

  if FInheritedClass <> '' then
  begin
    Result := ReplaceText( Result, '<ParentClassName>', FInheritedClass );
  end
  else
  begin
    Result := ReplaceText( Result, '<ParentClassName>', 'TObject' );
  end;
  Result := ReplaceText( Result, '<InhClassName>', FPossibleClassname );

  sFuncDefs := '';
  sFuncImpls := '';

  c := FInterfaceMethods.Count - 1;
  for i := 0 to c do
  begin
    func := FInterfaceMethods[i];

    sFuncDefs := sFuncDefs + '    ' + func.RawMethod;
    if i < c then
    begin
      sFuncDefs := sFuncDefs + #13#10;
    end;
  end;

  c := FTestFunctions.Count - 1;
  for i := 0 to c do
  begin
    func := TMethodDefinition(FTestFunctions[i]);

    bIsVoid := SameText(func.Functype, 'void');
    if bIsVoid then
    begin
      if FOverloadList.IndexOf(LowerCase(func.DefMethodName)) <> -1 then
      begin
        sFuncDefs := sFuncDefs + '    procedure TEST_' + func.DefMethodName + '(' + func.RawParamDefinition + '); overload;' + #13#10;
      end
      else
      begin
        sFuncDefs := sFuncDefs + '    procedure TEST_' + func.DefMethodName + '(' + func.RawParamDefinition + ');' + #13#10;
      end;
      sImpl := 'procedure ' + FPossibleClassname + '.TEST_' + func.DefMethodName + '(' + func.RawParamDefinition + ');';
    end
    else
    begin
      if FOverloadList.IndexOf(LowerCase(func.DefMethodName)) <> -1 then
      begin
        sFuncDefs := sFuncDefs + '    function TEST_' + func.DefMethodName + '(' + func.RawParamDefinition + '): ' + func.Functype + '; overload;' + #13#10;
      end
      else
      begin
        sFuncDefs := sFuncDefs + '    function TEST_' + func.DefMethodName + '(' + func.RawParamDefinition + '): ' + func.Functype + ';' + #13#10;
      end;
      sImpl := 'function ' + FPossibleClassname + '.TEST_' + func.DefMethodName + '(' + func.RawParamDefinition + '): ' + func.Functype + ';';
    end;

    sFuncImpls :=
      sFuncImpls +
      sImpl + #13#10 +
      'begin' + #13#10;

    if bIsVoid then
    begin
      sFuncImpls := sFuncImpls +
        '  ' + func.DefMethodName + '(' + func.ParamImplString + ');' + #13#10;
    end
    else
    begin
      sFuncImpls := sFuncImpls +
        '  Result := ' + func.DefMethodName + '(' + func.ParamImplString + ');' + #13#10;
    end;
    sFuncImpls := sFuncImpls +
      'end;' + #13#10 + #13#10;
  end;
  
  
  Result := ReplaceText( Result, '<InhFunctionDefinitions>', sFuncDefs );
  Result := ReplaceText( Result, '<InhFunctionImplementations>', sFuncImpls );
end;

function TInheritGen.GetTemplateCode: string;
begin
  Result := GenerateTemplate;
end;

function TInheritGen.HasTestFunction(const sName: string): boolean;
var
  i, c: integer;
  mdef: TMethodDefinition;
begin
  Result := False;
  
  c := FTestFunctions.Count - 1;
  for i := 0 to c do
  begin
    mdef := TMethodDefinition(FTestFunctions[i]);
    if SameText(mdef.DefMethodName, sName) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TInheritGen.IncludeInterfaceMethod(const AMethod: TMethodDefinition);
begin
  FInterfaceMethods.Add(AMethod);
end;

procedure TInheritGen.ParsedMethodsToTest;
var
  i, c: integer;
  mdef: TMethodDefinition;
begin
  FTestFunctions.OwnsObjects := False;

  c := FUnitParser.MethodList.Count - 1;
  for i := 0 to c do
  begin
    mdef := TMethodDefinition(FUnitParser.MethodList[i]);

    if SameText( mdef.InClass, FInheritedClass ) and not SameText( mdef.Functype, 'constructor' ) and not SameText( mdef.Functype, 'destructor' ) then
    begin
      if FUnitParser.NonPrivateMethods.IndexOf(mdef.Signature) <> -1 then
      begin
        if (FOverloadList.IndexOf(LowerCase(mdef.DefMethodName)) = -1) and HasTestFunction(mdef.DefMethodName) then
        begin
          FOverloadList.Add(LowerCase(mdef.DefMethodName));
        end;

        FTestFunctions.Add( mdef );
      end;
    end;
  end;
end;

end.
