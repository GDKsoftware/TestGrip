unit uUnitParser;

interface

{$I ..\Testgrip.inc}

uses
  Classes, Contnrs, uPascalDefs;

type
  TFileBoundary = record
    Start: Integer;
    Stop: Integer;
  end;

  TUnitParser = class
  protected
    FRawOuterUsesList: TStrings;
    FRawInnerUsesList: TStrings;
    FOuterUsesList: TStrings;
    FInnerUsesList: TStrings;

    FInterfaceClassList: TStrings;

    FRequires: TStrings;

    FBufferSize: integer;

    FMethodList: TObjectList;
    FInterfaceMethodList: TObjectList;
    FNonPrivateMethods: TStrings;

    FPropertyList: TObjectList;
    FVariableList: TObjectList;

    FInitCode: TStrings;

    FHasUTF8BOM: boolean;

    FStartOfImplementation: integer;

    FOuterUsesBoundaries: TFileBoundary;
    FInnerUsesBoundaries: TFileBoundary;

    function GetHasOuterUses: Boolean;
    function GetHasInnerUses: Boolean;

    {$IFDEF DELPHI2009_UP}
    function ReadLoopUntilSemiColon(const sIn: string; iStart: integer; var sOut: string): boolean;
    {$ELSE}
    function ReadLoopUntilSemiColon(const sIn: ansistring; iStart: integer; var sOut: ansistring): boolean;
    {$ENDIF}

    procedure ParseUsesList( const lstIn: TStrings; const lstOut: TStrings; bIsDkp: boolean = False );

    procedure ExtractAllMethods( const s: TStream; bGetLineNumbers: boolean = False; iLineNumberOffset: integer = 0 );

    function ExtractParentClassFromDefinition( const s: string ): string;

    function FilterVariable( const s: string ): string;

    procedure UpdateMethodBasedOnDefinition(const aMethod: TMethodDefinition);
  public
    property HasUTF8BOM: boolean
      read FHasUTF8BOM;

    property BufferSize: integer
      read FBufferSize write FBufferSize;

    property Requires: TStrings
      read FRequires;

    property RawOuterUsesList: TStrings
      read FRawOuterUsesList;
    property RawInnerUsesList: TStrings
      read FRawInnerUsesList;

    property OuterUsesList: TStrings
      read FOuterUsesList;
    property InnerUsesList: TStrings
      read FInnerUsesList;

    property MethodList: TObjectList
      read FMethodList;
    property InterfaceMethodList: TObjectList
      read FInterfaceMethodList;

    property PropertyList: TObjectList
      read FPropertyList;
    property VariableList: TObjectList
      read FVariableList;

    property NonPrivateMethods: TStrings
      read FNonPrivateMethods;

    property InitCode: TStrings
      read FInitCode;

    property StartOfImplementation: integer
      read FStartOfImplementation;

    property InterfaceClassList: TStrings
      read FInterfaceClassList;

    property OuterUsesBoundaries: TFileBoundary
      read FOuterUsesBoundaries;
    property InnerUsesBoundaries: TFileBoundary
      read FInnerUsesBoundaries;

    property HasOuterUses: Boolean
      read GetHasOuterUses;
    property HasInnerUses: Boolean
      read GetHasInnerUses;

    constructor Create;
    destructor Destroy; override;

    procedure ParseUnit( const s: TStream; bExtractMethods: boolean = False; bExtractDprInit: boolean = False; bIsDpk: boolean = false; bExtractProperties: boolean = False; bGetLineNumbers: boolean = False );
    procedure ParseFromFile(const AFilename: string; bExtractMethods: Boolean; bExtractDprInit: Boolean = False; bIsDpk: Boolean = False; bExtractProperties: boolean = False; bGetLineNumbers: boolean = False);

    function GetCreateCodeFromClass( const sClassName: string): string;

    function FindClosestMethodDefinition(const aMDef: TMethodDefinition): TMethodDefinition;

    function FindNextMethodAfter(const aMethod: TMethodDefinition): TMethodDefinition;

    function IsNonPrivate(const sSigniture: string): boolean;

    function GetDefinition(const sSigniture: string): TMethodDefinition;

    /// <summary>Fill lst with methoddefinitions within given class, DO NOT FREE definitions</summary>
    procedure ListInterfaceMethods(const sClassname: string; const lst: TList);
  end;

  function GetFirstMethodDefinitionFromSourceByName( const sPascalFile: string; const sMethodName: string; const sClassName: string; bIncludeLineNumer: boolean = False ): TMethodDefinition;
  function GetFirstMethodDefinitionFromSourceBySignature( const sPascalFile: string; const sSignature: string; bIncludeLineNumer: boolean = False ): TMethodDefinition;

  function IsValidPascalIdentifier(const sIdent: string): boolean;

implementation

uses
  StrUtils, SysUtils, Math, uCommonFunctions{$ifdef VER150}, uD7Functions{$endif};


function IsValidPascalIdentifier(const sIdent: string): boolean;
begin
  {$ifdef VER150}
  Result := IsValidIdent(sIdent);
  {$else}
  Result := IsValidIdent(sIdent, false);
  {$endif}
end;


{ TUnitParser }

function TUnitParser.ExtractParentClassFromDefinition(const s: string): string;
var
  i, j: integer;
begin
  Result := '';

  i := Pos( '(', s );
  if i <> 0 then
  begin
    Inc(i);
    j := PosEx( ')', s, i );
    if j <> 0 then
    begin    
      Result := Trim( Copy(s, i, j - i ) );
    end;
  end;
end;

function TUnitParser.FilterVariable(const s: string): string;
var
  i, c: integer;
  state: integer;
begin
  Result := '';
  state := 0;

  c := Length(s);
  for i := c downto 1 do
  begin
    if (s[i] = ':') and (state = 0) then
    begin
      state := 1;
    end
    {$IFDEF DELPHI2009_UP}
    else if CharInSet(s[i], [#13,#10,#7,' ']) then
    {$ELSE}
    else if (s[i] in [#13,#10,#7,' ']) then
    {$ENDIF}
    begin
      // continue unless state=2
      if state = 2 then
      begin
        break;
      end;
    end
    else if (state = 1) then
    begin
      state := 2;
    end;

    Result := s[i] + Result;
  end;
end;

procedure TUnitParser.UpdateMethodBasedOnDefinition(const aMethod: TMethodDefinition);
var
  sSigniture: string;
  Definition: TMethodDefinition;
begin
  sSigniture := aMethod.Signature;
  Definition := GetDefinition(sSigniture);
  if Assigned(Definition) then
  begin
    aMethod.Scope := Definition.Scope;
    aMethod.IsOverride := Definition.IsOverride;
    aMethod.IsVirtual := Definition.IsVirtual;
    aMethod.IsOverloaded := Definition.IsOverloaded;
  end;
end;

function TUnitParser.FindClosestMethodDefinition(const aMDef: TMethodDefinition): TMethodDefinition;
var
  i, c: integer;
  j, d: integer;
  mdef: TMethodDefinition;
  aHighestMatched: TMethodDefinition;
  iHighestMatchScore: integer;
  iCurrentScore: integer;
  bMatchAll: boolean;
begin
  aHighestMatched := nil;
  iHighestMatchScore := 0;
  bMatchAll := False;

  c := FMethodList.Count - 1;
  for i := 0 to c do
  begin
    mdef := TMethodDefinition(FMethodList[i]);

    if SameText(aMDef.InClass, mdef.InClass) then
    begin
      if SameText(aMDef.DefMethodName, mdef.DefMethodName) then
      begin
        iCurrentScore := 1;

        d := aMDef.ParamTypes.Count - 1;
        for j := 0 to d do
        begin
          if (j < mdef.ParamTypes.Count) then
          begin
            if SameText(mdef.ParamTypes[j], aMDef.ParamTypes[j]) then
            begin
              Inc(iCurrentScore);

              if j = (mdef.ParamTypes.Count - 1) then
              begin
                bMatchAll := (mdef.ParamTypes.Count = aMDef.ParamTypes.Count);
                break;
              end;
            end
            else
            begin
              // zodra het niet meer overeenkomt, geen matchpunten meer
              break;
            end;
          end
          else
          begin
            break;
          end;
        end;

        if (iCurrentScore > iHighestMatchScore) then
        begin
          aHighestMatched     := mdef;
          iHighestMatchScore  := iCurrentScore;

          if bMatchAll then
          begin
            break;
          end;
        end;
      end;
    end;

  end;

  Result := aHighestMatched;
end;

function TUnitParser.FindNextMethodAfter(const aMethod: TMethodDefinition): TMethodDefinition;
var
  AnotherMethod: TMethodDefinition;
  MethodIdx, MethodCount: Integer;
begin
  Result := nil;

  MethodCount := FMethodList.Count - 1;
  for MethodIdx := 0 to MethodCount do
  begin
    AnotherMethod := TMethodDefinition(FMethodList[MethodIdx]);

    if Assigned(Result) then
    begin
      if (AnotherMethod.LineNumber > aMethod.LineNumber) and (AnotherMethod.LineNumber < Result.LineNumber) then
      begin
        Result := AnotherMethod;
      end;
    end
    else if AnotherMethod.LineNumber > aMethod.LineNumber then
    begin
      Result := AnotherMethod;
    end;
  end;
end;

constructor TUnitParser.Create;
begin
  FRawOuterUsesList := TStringList.Create;
  FRawInnerUsesList := TStringList.Create;
  FOuterUsesList := TStringList.Create;
  FInnerUsesList := TStringList.Create;
  FRequires := TStringList.Create;

  FMethodList := TObjectList.Create(True);
  FInterfaceMethodList := TObjectList.Create(True);
  FPropertyList := TObjectList.Create(True);
  FVariableList := TObjectList.Create(True);

  FInitCode := TStringList.Create;

  FInterfaceClassList := TStringList.Create;

  FNonPrivateMethods := TStringList.Create;

  FBufferSize := 2048;

  FStartOfImplementation := -1;
end;

destructor TUnitParser.Destroy;
var
  Idx: Integer;
begin
  for Idx := 0 to FNonPrivateMethods.Count - 1 do
    FNonPrivateMethods.Objects[Idx].Free;

  FreeAndNil(FNonPrivateMethods);

  for Idx := 0 to FInterfaceClassList.Count - 1 do
    FInterfaceClassList.Objects[Idx].Free;

  FreeAndNil(FInterfaceClassList);

  FreeAndNil(FInitCode);

  FreeAndNil(FVariableList);
  FreeAndNil(FPropertyList);
  FreeAndNil(FInterfaceMethodList);
  FreeAndNil(FMethodList);

  FreeAndNil(FRequires);

  FreeAndNil(FInnerUsesList);
  FreeAndNil(FOuterUsesList);
  FreeAndNil(FRawInnerUsesList);
  FreeAndNil(FRawOuterUsesList);

  inherited;
end;

procedure TUnitParser.ExtractAllMethods(const s: TStream; bGetLineNumbers: boolean; iLineNumberOffset: integer);
var
  bEof: boolean;
  iRead: integer;
  {$IFDEF DELPHI2009_UP}
  largeblock: string;
  largeblocklowercase: string;
  {$ENDIF}
  ansiblock: ansistring;
  ansilargeblock: ansistring;
  ansilargeblocklowercase: ansistring;
  sMethodDef: ansistring;
  i: integer;
  pc, pd, pp, pf: integer;
  bInParams: boolean;
  bInMethodDef: boolean;
  fdef: TMethodDefinition;
  iAnotherLineOffset: integer;
  iMethodLineNumber: integer;
  iMethodDefinitionLineCount: Integer;
  iSkip: integer;
begin
  bInParams := False;
  sMethodDef := '';
  {$IFDEF DELPHI2009_UP}
  largeblock := '';
  {$ENDIF}
  ansilargeblock := '';
  bInMethodDef := false;
  iAnotherLineOffset := iLineNumberOffset;

  i := 1;

  bEof := False;
  while not (bEof and (i >= Length(ansilargeblock))) do
  begin
    SetLength(ansiblock,FBufferSize);

    if i >= Length(ansilargeblock) then
    begin
      iRead := s.Read( ansiblock[1], FBufferSize );
      if iRead < FBufferSize then
      begin
        SetLength(ansiblock,iRead);
      end;

      if iRead = 0 then
      begin
        bEof := True;
      end;

      ansilargeblock := ansilargeblock + ansiblock;
    end;

    if not bInMethodDef then
    begin
      iSkip := Length(ansilargeblock);
      {$IFDEF DELPHI2009_UP}
      largeblock := string(ansilargeblock);
      largeblocklowercase := LowerCase(largeblock);
      pc := PosEx('constructor ', largeblocklowercase, Max(1, i - 11));
      pd := PosEx('destructor ', largeblocklowercase, Max(1, i - 10));
      pp := PosEx('procedure ', largeblocklowercase, Max(1, i - 9));
      pf := PosEx('function ', largeblocklowercase, Max(1, i - 8));
      largeblocklowercase := '';
      {$ELSE}
      ansilargeblocklowercase := LowerCase(ansilargeblock);
      pc := PosEx('constructor ', ansilargeblocklowercase, Max(1, i - 11));
      pd := PosEx('destructor ', ansilargeblocklowercase, Max(1, i - 10));
      pp := PosEx('procedure ', ansilargeblocklowercase, Max(1, i - 9));
      pf := PosEx('function ', ansilargeblocklowercase, Max(1, i - 8));
      ansilargeblocklowercase := '';
      {$ENDIF}

      {$IFDEF DELPHI2009_UP}
      if (pc > 2) and not CharInSet(ansilargeblock[pc - 1], [#$0A,#$0D,#$07,#$20]) then
      {$ELSE}
      if (pc > 2) and not (ansilargeblock[pc - 1] in [#$0A,#$0D,#$07,#$20]) then
      {$ENDIF}
      begin
        iSkip := Min(pc + 12, iSkip);
        pc := 0;
      end;

      {$IFDEF DELPHI2009_UP}
      if (pd > 2) and not CharInSet(ansilargeblock[pd - 1], [#$0A,#$0D,#$07,#$20]) then
      {$ELSE}
      if (pd > 2) and not (ansilargeblock[pd - 1] in [#$0A,#$0D,#$07,#$20]) then
      {$ENDIF}
      begin
        iSkip := Min(pd + 11, iSkip);
        pd := 0;
      end;

      {$IFDEF DELPHI2009_UP}
      if (pp > 2) and not CharInSet(ansilargeblock[pp - 1], [#$0A,#$0D,#$07,#$20]) then
      {$ELSE}
      if (pp > 2) and not (ansilargeblock[pp - 1] in [#$0A,#$0D,#$07,#$20]) then
      {$ENDIF}
      begin
        iSkip := Min(pp + 10, iSkip);
        pp := 0;
      end;

      {$IFDEF DELPHI2009_UP}
      if (pf > 2) and not CharInSet(ansilargeblock[pf - 1], [#$0A,#$0D,#$07,#$20]) then
      {$ELSE}
      if (pf > 2) and not (ansilargeblock[pf - 1] in [#$0A,#$0D,#$07,#$20]) then
      {$ENDIF}
      begin
        iSkip := Min(pf + 9, iSkip);
        pf := 0;
      end;

      i := 0;
      if (pc <> 0) then
      begin
        if i = 0 then
        begin
          i := pc;
        end
        else
        begin
          i := Min( i, pc );
        end;
      end;

      if pd <> 0 then
      begin
        if i = 0 then
        begin
          i := pd;
        end
        else
        begin
          i := Min( i, pd );
        end;
      end;

      if pp <> 0 then
      begin
        if i = 0 then
        begin
          i := pp;
        end
        else
        begin
          i := Min( i, pp );
        end;
      end;

      if pf <> 0 then
      begin
        if i = 0 then
        begin
          i := pf;
        end
        else
        begin
          i := Min( i, pf );
        end;
      end;

      if i <> 0 then
      begin
        bInMethodDef := True;
        sMethodDef := '';
      end
      else
      begin
        i := iSkip;
      end;
    end;

    if bInMethodDef then
    begin
      sMethodDef := sMethodDef + ansilargeblock[i];

      if bInParams then
      begin
        if ansilargeblock[i] = ')' then
        begin
          bInParams := False;
        end;
      end
      else
      begin
        if ansilargeblock[i] = '(' then
        begin
          bInParams := True;
        end
        else if ansilargeblock[i] = ';' then
        begin
          bInMethodDef := False;

          fdef := TMethodDefinition.Create(string(sMethodDef), false, csUnknown);
          if IsValidPascalIdentifier(fdef.DefMethodName) then
          begin
            if bGetLineNumbers then
            begin
              iMethodLineNumber := TCommonStringFunctions.CountLines(string(ansilargeblock), lfDos, i);
              iMethodDefinitionLineCount := TCommonStringFunctions.CountLines(string(sMethodDef), lfDos);

              fdef.LineNumber := iAnotherLineOffset + iMethodLineNumber - iMethodDefinitionLineCount;

              iAnotherLineOffset := iAnotherLineOffset + iMethodLineNumber;
            end;

            UpdateMethodBasedOnDefinition(fdef);

            FMethodList.Add( fdef );
          end
          else
          begin
            fdef.Free;

            sMethodDef := '';
          end;

          ansilargeblock := Copy(ansilargeblock, i + 1);
          i := 1;
        end;
      end;

      Inc(i);
    end;
  end;
end;

function TUnitParser.GetCreateCodeFromClass(const sClassName: string): string;
var
  i, c: integer;
  j, d: integer;
  m: TMethodDefinition;
  sParamType: string;
  sParamVal: string;
  sAllParams: string;
  sParentClass: string;
begin
  Result := '';
  sAllParams := '';

  c := FMethodList.Count - 1;
  for i := 0 to c do
  begin
    m := TMethodDefinition(FMethodList[i]);
    if SameText(m.InClass, sClassName) and SameText(m.Functype, 'constructor') then
    begin
      sAllParams := '';

      d := m.Parameters.Count - 1;
      for j := 0 to d do
      begin
        sParamType := m.ParamTypes[j];
        if SameText(sParamType, 'integer') then
        begin
          sParamVal := '0';
        end
        else if SameText(sParamType, 'string') then
        begin
          sParamVal := '''''';
        end
        else if SameText(sParamType, 'boolean') then
        begin
          sParamVal := 'false';
        end
        else if SameText(sParamType, 'tdatetime') then
        begin
          sParamVal := '0';
        end
        else
        begin
          sParamVal := 'nil';
        end;

        if sAllParams <> '' then
        begin
          sAllParams := sAllParams + ', ' + sParamVal;
        end
        else
        begin
          sAllParams := sParamVal;
        end;
      end;

      Result := m.InClass + '.' + m.DefMethodName + '(' + sAllParams + ')';
      break;
    end;
  end;

  if Result = '' then
  begin
    sParentClass := FInterfaceClassList.Values[sClassName];

    if SameText(sParentClass, 'TForm') or SameText(sParentClass, 'TComponent') then
    begin
      Result := sClassName + '.' + 'Create(nil)';    
    end
    else
    begin
      Result := sClassName + '.' + 'Create';
    end;
  end;
end;

function TUnitParser.IsNonPrivate(const sSigniture: string): boolean;
begin
  Result := (FNonPrivateMethods.IndexOf(sSigniture) <> -1);
end;

function TUnitParser.GetDefinition(const sSigniture: string): TMethodDefinition;
var
  Idx: Integer;
begin
  Idx := FNonPrivateMethods.IndexOf(sSigniture);
  if Idx <> -1 then
    Result := TMethodDefinition(FNonPrivateMethods.Objects[Idx])
  else
    Result := nil;
end;

procedure TUnitParser.ListInterfaceMethods(const sClassname: string; const lst: TList);
var
  mdef: TMethodDefinition;
  i, c: integer;
begin
  c := FInterfaceMethodList.Count - 1;
  for i := 0 to c do
  begin
    mdef := TMethodDefinition(FInterfaceMethodList[i]);
    if Assigned(mdef) then
    begin
      if SameText(sClassname, mdef.InClass) then
      begin
        lst.Add(mdef);
      end;
    end;
  end;
end;

procedure TUnitParser.ParseFromFile(const AFilename: string; bExtractMethods,
  bExtractDprInit, bIsDpk, bExtractProperties, bGetLineNumbers: boolean);
var
  fsPascalFile: TFileStream;
begin
  fsPascalFile := TFileStream.Create(AFilename,fmOpenRead);
  try
    ParseUnit(fsPascalFile, bExtractMethods, bExtractDprInit, bIsDpk, bExtractProperties, bGetLineNumbers);
  finally
    fsPascalFile.Free;
  end;
end;

procedure TUnitParser.ParseUnit(const s: TStream; bExtractMethods,
  bExtractDprInit, bIsDpk, bExtractProperties: boolean; bGetLineNumbers: boolean);
var
  bInCurlyBracketComment: boolean;
  bInParenthesisComment: boolean;
  bInLineComment: boolean;
  bInQuote: boolean;
  bInInterface: boolean;
  bInType: boolean;
  bInImplementation: boolean;
  bInPrivate: boolean;
  bInProtected: boolean;
  bInPublic: boolean;
  bInPublished: boolean;
  bInProperty: boolean;
  bInUses1: boolean;
  bInUses2: boolean;
  bInClassTypeDeclaration: boolean;
  bInAnnotation: boolean;
  sHelperWord: ansistring;
  bEof: boolean;
  {$IFDEF DELPHI2009_UP}
  sCurrentClass: string;
  sTypeName: string;
  sPreviousKeyWord: string;
  sCurrentKeyWord: string;
  block: string;
  ch: Char;
  sTempLine: string;
  {$ELSE}
  sCurrentClass: ansistring;
  sTypeName: ansistring;
  sPreviousKeyWord: ansistring;
  sCurrentKeyWord: ansistring;
  block: ansistring;
  ch: AnsiChar;
  sTempLine: ansistring;
  {$ENDIF}
  ansiblock: ansistring;
  iRead: integer;
  iTotalBytesDone: integer;
  i: Integer;
  x: integer;
  sUsesList1: string;
  sUsesList2: string;
  bWasInElse: boolean;
  bIsCreateForm: boolean;
  bInVar: boolean;
  bInMethod: boolean;
  sCurrentVar: string;
  vardef: TVariableDefinition;
  pdef: TPropertyDefinition;
  bInRecord: boolean;
  CurrentMethodDefinition: TMethodDefinition;
  bMethodBracketOpen: boolean;
  iInGenericTypes: integer;
  iLineNumberOffset: integer;
  iLinesInBlock: integer;
  aScope: TClassScope;
  bInInterfaceClass: boolean;
  CurrentAnnotation: string;
  CurrentAnnotations: TStringList;
  CurrentClassDef: TClassDefinition;
  TempIdx: Integer;
begin
  iTotalBytesDone := 0;
  sHelperWord := '';
  sCurrentKeyWord := '';
  sPreviousKeyWord := '';
  sCurrentClass := '';
  sTypeName := '';
  sUsesList1 := '';
  sUsesList2 := '';
  sTempLine := '';

  bInCurlyBracketComment := False;
  bInParenthesisComment := False;
  bInLineComment := False;
  bInQuote := False;
  bInInterface := False;
  bInType := False;
  bInImplementation := False;
  bInPrivate := False;
  bInProtected := False;
  bInPublic := False;
  bInPublished := False;
  bInProperty := False;
  bInUses1 := False;
  bInUses2 := False;
  bWasInElse := False;
  bIsCreateForm := False;
  bInVar := False;
  bInMethod := False;
  bInRecord := False;
  bInClassTypeDeclaration := False;
  bMethodBracketOpen := False;
  iInGenericTypes := 0;
  bInInterfaceClass := True;
  bInAnnotation := False;

  iLineNumberOffset := 0;
  iLinesInBlock := 0;

  CurrentMethodDefinition := nil;

  CurrentAnnotations := TStringList.Create;

  FOuterUsesList.Clear;
  FInnerUsesList.Clear;
  FMethodList.Clear;
  FNonPrivateMethods.Clear;
  FInterfaceMethodList.Clear;
  FInterfaceClassList.Clear;
  FRequires.Clear;
  FInitCode.Clear;
  FPropertyList.Clear;

  FInnerUsesBoundaries.Start := 0;
  FInnerUsesBoundaries.Stop := 0;
  FOuterUsesBoundaries.Start := 0;
  FOuterUsesBoundaries.Stop := 0;

  block := '';
  ansiblock := '';

  bEof := False;

  FHasUTF8BOM := False;

  s.Seek( 0, soFromBeginning );

  SetLength(ansiblock, 3);
  iRead := s.Read(ansiblock[1], 3);
  if iRead = 3 then
  begin
    FHasUTF8BOM := (ansiblock[1] = #$EF) and (ansiblock[2] = #$BB) and (ansiblock[3] = #$BF);
    if not FHasUTF8BOM then
    begin
      s.Seek( 0, soFromBeginning );
    end
    else
    begin
      iTotalBytesDone := 3;
    end;
  end
  else
  begin
    s.Seek( 0, soFromBeginning );
  end;

  while not bEof do
  begin
    SetLength(ansiblock, FBufferSize);
    iRead := s.Read(ansiblock[1], FBufferSize);
    iTotalBytesDone := iTotalBytesDone + iRead;
    if iRead < FBufferSize then
    begin
      SetLength(ansiblock, iRead);
      bEof := True;
    end;

    {$IFDEF DELPHI2009_UP}
    block := string(ansiblock);
    {$ELSE}
    block := ansiblock;
    {$ENDIF}

    if bGetLineNumbers then
    begin
      iLinesInBlock := TCommonStringFunctions.CountLines(block, lfDos);

      iLineNumberOffset := iLineNumberOffset + iLinesInBlock;
    end;

    if bInUses1 then
    begin
      bInUses1 := not ReadLoopUntilSemiColon(block, 1, sUsesList1);
    end;

    if bInUses2 then
    begin
      bInUses2 := not ReadLoopUntilSemiColon(block, 1, sUsesList2);
    end;

    for i := 1 to iRead do
    begin
      ch := block[i];

      sTempLine := sTempLine + ch;

      if bInLineComment then
      begin
        {$IFDEF DELPHI2009_UP}
        if CharInSet(ch, [#$0D, #$0A]) then
        {$ELSE}
        if (ch in [#$0D, #$0A]) then
        {$ENDIF}
        begin
          bInLineComment := false;
        end;
      end
      else if bInParenthesisComment then
      begin
        if ch = '*' then
        begin
          sCurrentKeyWord := ch;
        end
        else if (ch = ')') and (sCurrentKeyWord = '*') then
        begin
          bInParenthesisComment := False;
        end;
      end
      else if bInCurlyBracketComment then
      begin
        if ch = '}' then
        begin
          bInCurlyBracketComment := False;
        end;
      end
      else if not bInParenthesisComment and (ch = '{') then
      begin
        bInCurlyBracketComment := True;
      end
      else if (sCurrentKeyWord = '*') and (ch = '(') then
      begin
        sCurrentKeyWord := '';
        bInParenthesisComment := True;
      end
      else if not bInImplementation and not bInAnnotation and not bInVar and (ch = '[') then
      begin
        bInAnnotation := True;
        CurrentAnnotation := '';
        sTempLine := '';
      end
      else if bInAnnotation and (ch = ']') then
      begin
        bInAnnotation := False;
        if CurrentAnnotation <> '' then
        begin
          CurrentAnnotations.Add(CurrentAnnotation);
        end
        else if Assigned(CurrentClassDef) then
        begin
          sTempLine := Copy(sTempLine, 1, Length(sTempLine) - 1);
          CurrentClassDef.Annotations.Add(sTempLine);
        end;
        CurrentAnnotation := '';
        sTempLine := '';
      end
      else if bInAnnotation and ((CurrentAnnotation = '') or StartsStr('''', CurrentAnnotation)) and (ch = '''') then
      begin
        // is begin/end of GUID
      end
      else if bInAnnotation then
      begin
        CurrentAnnotation := CurrentAnnotation + ch;
        sTempLine := '';
      end
      else if ch = '=' then
      begin
        // check if this is an = without whitespace inbetween typename

        if bInInterface and bInType then
        begin
          if sPreviousKeyWord = '' then
          begin
            sPreviousKeyWord := sCurrentKeyWord;
          end;

          sTypeName := sPreviousKeyWord;
        end;
      end
      else if (ch = '<') and bInType then
      begin
        Inc(iInGenericTypes);
        sCurrentKeyWord := sCurrentKeyWord + '<';
      end
      else if (ch = '>') and bInType then
      begin
        Dec(iInGenericTypes);
        iInGenericTypes := Max(0, iInGenericTypes);
        sCurrentKeyWord := sCurrentKeyWord + '>';
      end
      else if (ch = ';') and (iInGenericTypes = 0) then
      begin
        bInClassTypeDeclaration := False;

        if SameText(sCurrentKeyWord, 'class') then
        begin
          // this is a prototype, not a real class
        end
        else if SameText(sCurrentKeyWord, 'end') then
        begin
          // end of ... anything
          CurrentAnnotations.Clear;
          bInAnnotation := False;

          bInPrivate := False;
          bInProtected := False;
          bInPublic := False;
          bInPublished := False;
          bInProperty := False;
          bInRecord := False;

          bInInterfaceClass := False;

          sCurrentClass := '';
        end;

        if bIsCreateForm then
        begin
          FInitCode.Add( sTempLine );
          bIsCreateForm := False;
        end;

        if not bInImplementation then
        begin
          if Assigned(CurrentMethodDefinition) then
          begin
            if SameText(sCurrentKeyWord, 'virtual') then
            begin
              CurrentMethodDefinition.IsVirtual := True;
            end
            else if SameText(sCurrentKeyWord, 'override') then
            begin
              CurrentMethodDefinition.IsOverride := True;
            end
            else if SameText(sCurrentKeyWord, 'overload') then
            begin
              CurrentMethodDefinition.IsOverloaded := True;
            end;
          end;

          if bInProperty and (bInPublic or bInPublished) then
          begin
            pdef := TPropertyDefinition.Create(sTempLine, sCurrentClass);
            if pdef.PropertyName <> '' then
            begin
              FPropertyList.Add( pdef );
            end
            else
            begin
              pdef.Free;
            end;
          end
          else if bInVar and (bInPublic or bInPublished) then
          begin
            vardef := TVariableDefinition.Create(Trim(sTempLine),sCurrentClass);
            if vardef.PropertyName <> '' then
            begin
              FVariableList.Add( vardef );
            end
            else
            begin
              vardef.Free;
            end;
          end
          else if bInMethod and not bMethodBracketOpen then
          begin
            bInMethod := False;

            if not bInPrivate and not bInImplementation then
            begin
              if bInPrivate then aScope := csPrivate;
              if bInProtected then aScope := csProtected;
              if bInPublic then aScope := csPublic;
              if bInPublished then aScope := csPublished;

              if bInInterfaceClass then
              begin
                aScope := csPublic;
              end;

              CurrentMethodDefinition := TMethodDefinition.Create(sTempLine, false, aScope);

              // classname is not inline here, so we get it from the currentclass var
              CurrentMethodDefinition.InClass := sCurrentClass;
              CurrentMethodDefinition.Annotations.AddStrings(CurrentAnnotations);

              if bInInterfaceClass then
              begin
                FInterfaceMethodList.Add(TMethodDefinition.Create(CurrentMethodDefinition));
              end;

              FNonPrivateMethods.AddObject(CurrentMethodDefinition.Signature, CurrentMethodDefinition);
            end;

            CurrentAnnotations.Clear;
            bInAnnotation := False;
          end

        end;

        bInProperty := False;
        bInVar := False;

        if bInMethod and not bInImplementation then
        begin
          // dont reset templine to preserve full methodline
        end
        else
        begin
          sTempLine := '';
        end;
      end
      else if (ch = ':') and not bInMethod and (iInGenericTypes = 0) then
      begin
        sCurrentVar := sTypeName;
        bInVar := True;

        sTempLine := FilterVariable(sTempLine);
      end
      {$IFDEF DELPHI2009_UP}
      else if CharInSet(ch, [#$20, #$09, #$0A, #$0D, '(']) and (iInGenericTypes = 0) then
      {$ELSE}
      else if (ch in [#$20, #$09, #$0A, #$0D, '(']) and (iInGenericTypes = 0) then
      {$ENDIF}
      begin
        if StartsText('class', sCurrentKeyWord) then
        begin
          bInPrivate := False;
          bInProtected := False;
          bInPublic := False;
          bInPublished := True;

          if sTypeName <> '' then
          begin
            bInClassTypeDeclaration := True;

            sCurrentClass := DetermineClassSigniture(sTypeName);

            CurrentClassDef := TClassDefinition.Create(sCurrentClass+' = class');
            CurrentClassDef.Annotations.AddStrings(CurrentAnnotations);
            CurrentAnnotations.Clear;

            FInterfaceClassList.AddObject(sCurrentClass + '=' + '', CurrentClassDef);
          end;

          sTypeName := '';
        end
        else if SameText(sCurrentKeyWord, 'interface') then
        begin
          if bInType then
          begin
            bInClassTypeDeclaration := True;

            sTempLine := Trim(sTempLine);
            sCurrentClass := Trim(TKeyValueFunctions.GetKey(sTempLine));

            CurrentClassDef := TClassDefinition.Create(sCurrentClass+' = interface');
            CurrentClassDef.Annotations.AddStrings(CurrentAnnotations);
            CurrentAnnotations.Clear;

            FInterfaceClassList.AddObject(sCurrentClass + '=' + '', CurrentClassDef);

            bInInterfaceClass := True;
          end
          else
          begin
            FOuterUsesBoundaries.Start := Max(0, iTotalBytesDone - iRead) + i - 1;
          end;

          bInInterface := True;
//          sTempLine := '';
        end
        else if SameText(sCurrentKeyWord, 'implementation') then
        begin
          bInInterface := False;
          bInImplementation := True;
          bInType := True;

          CurrentAnnotations.Clear;
          bInAnnotation := False;

          if bGetLineNumbers then
          begin
            // recalculate last block until i, so we don't count the extra lines beyond the "implementation" keyword
            iLineNumberOffset := iLineNumberOffset - iLinesInBlock + TCommonStringFunctions.CountLines(block, lfDos, i);

            FStartOfImplementation := iLineNumberOffset;
          end;

          FInnerUsesBoundaries.Start := Max(0, iTotalBytesDone - iRead) + i - 1;

          s.Seek( FInnerUsesBoundaries.Start, soBeginning );
          ExtractAllMethods(s, bGetLineNumbers, iLineNumberOffset);
          s.Seek( iTotalBytesDone, soBeginning );
        end
        else if SameText(sCurrentKeyWord, 'uses') then
        begin
          if bInImplementation then
          begin
            FInnerUsesBoundaries.Start := Max(0, iTotalBytesDone - iRead) + i - 4 - 1;
            bInUses2 := not ReadLoopUntilSemiColon(block, i - 4, sUsesList2);
          end
          else
          begin
            FOuterUsesBoundaries.Start := Max(0, iTotalBytesDone - iRead) + i - 4 - 1;
            bInUses1 := not ReadLoopUntilSemiColon(block, i - 4, sUsesList1);
          end;
        end
        else if bIsDpk and SameText(sCurrentKeyWord, 'contains') then
        begin
          FOuterUsesBoundaries.Start := Max(0, iTotalBytesDone - iRead) + i - 9 - 1;
          bInUses1 := not ReadLoopUntilSemiColon(block, i - 9, sUsesList1);
        end
        else if bIsDpk and SameText(sCurrentKeyWord, 'requires') then
        begin
          FInnerUsesBoundaries.Start := Max(0, iTotalBytesDone - iRead) + i - 9 - 1;
          bInUses2 := not ReadLoopUntilSemiColon(block, i - 9, sUsesList2);
        end
        else if SameText(sCurrentKeyWord, 'private') then
        begin
          bInPrivate := True;
          bInProtected := False;
          bInPublic := False;
          bInPublished := False;
          bInClassTypeDeclaration := false;
          sTempLine := '';
        end
        else if SameText(sCurrentKeyWord, 'protected') then
        begin
          bInPrivate := False;
          bInProtected := True;
          bInPublic := False;
          bInPublished := False;
          bInClassTypeDeclaration := false;
          sTempLine := '';
        end
        else if SameText(sCurrentKeyWord, 'public') then
        begin
          bInPrivate := False;
          bInProtected := False;
          bInPublic := True;
          bInPublished := False;
          bInClassTypeDeclaration := false;
          sTempLine := '';
        end
        else if SameText(sCurrentKeyWord, 'published') then
        begin
          bInPrivate := False;
          bInProtected := False;
          bInPublic := False;
          bInPublished := True;
          bInClassTypeDeclaration := false;
          sTempLine := '';
        end
        else if SameText(sCurrentKeyWord, 'record') then
        begin
          bInRecord := True;
          sTempLine := '';
        end
        else if SameText(sCurrentKeyWord, 'const') then
        begin

        end
        else if SameText(sCurrentKeyWord, 'var') then
        begin

        end
        else if SameText(sCurrentKeyWord, 'end.') then
        begin
          bEof := True;
        end
        else if SameText(sCurrentKeyWord, 'type') then
        begin
          bInType := True;
          sTempLine := '';
        end
        else if SameText(sCurrentKeyWord, 'property') then
        begin
          bInProperty := True;
          bInClassTypeDeclaration := false;
          sTempLine := sCurrentKeyWord + ch;
        end
        else if SameText(sCurrentKeyWord, 'procedure') then
        begin
          bInMethod := true;
          bInClassTypeDeclaration := false;
          sTempLine := sCurrentKeyWord + ch;
        end
        else if SameText(sCurrentKeyWord, 'function') then
        begin
          bInMethod := true;
          bInClassTypeDeclaration := false;
          sTempLine := sCurrentKeyWord + ch;
        end
        else if SameText(sCurrentKeyWord, 'constructor') then
        begin
          bInMethod := true;
          bInClassTypeDeclaration := false;
          sTempLine := sCurrentKeyWord + ch;
        end
        else if SameText(sCurrentKeyWord, 'destructor') then
        begin
          bInMethod := true;
          bInClassTypeDeclaration := false;
          sTempLine := sCurrentKeyWord + ch;
        end
        else if SameText(sCurrentKeyWord, 'application.createform') then
        begin
          sTempLine := sCurrentKeyWord + ch;

          bIsCreateForm := True;
        end
        else if (ch = '(') then
        begin
          sCurrentKeyWord := '(';
          bMethodBracketOpen := True;
        end
        else
        begin

        end;

        sPreviousKeyWord := sCurrentKeyWord;
        sCurrentKeyWord := '';
      end
      else if ch = '/' then
      begin
        if sHelperWord = '/' then
        begin
          bInLineComment := True;
        end
        else
        begin
          sHelperWord := '/';
        end;
      end
      else if (ch = ')') and not bIsCreateForm and (iInGenericTypes = 0) then
      begin
        if bInClassTypeDeclaration then
        begin
          bInClassTypeDeclaration := False;

          TempIdx := FInterfaceClassList.IndexOfName(sCurrentClass);
          if Assigned(FInterfaceClassList.Objects[TempIdx]) then
          begin
            TClassDefinition(FInterfaceClassList.Objects[TempIdx]).Reparse(sTempLine);
          end;

          sTempLine := ExtractParentClassFromDefinition( sTempLine );
          if sTempLine <> '' then
          begin
            FInterfaceClassList.Values[sCurrentClass] := sTempLine;
          end;
        end
        else if bInMethod then
        begin
          bMethodBracketOpen := false;
        end
        else
        begin
          bInMethod := False;
          sTempLine := '';
        end;
      end
      else
      begin
        if ch = '''' then
        begin
          bInQuote := not bInQuote;
        end;

        sCurrentKeyWord := sCurrentKeyWord + ch;
        bWasInElse := True;
      end;

      if not bWasInElse then
      begin
        sCurrentKeyWord := '';
      end;
    end;

  end;

  CurrentAnnotations.Free;

  FOuterUsesBoundaries.Stop := FOuterUsesBoundaries.Start + Length(sUsesList1);
  FInnerUsesBoundaries.Stop := FInnerUsesBoundaries.Start + Length(sUsesList2);

  FRawOuterUsesList.Text := sUsesList1;
  FRawInnerUsesList.Text := sUsesList2;

  ParseUsesList( FRawOuterUsesList, FOuterUsesList, bIsDpk );
  if bIsDpk then
  begin
    ParseUsesList( FRawInnerUsesList, FRequires, bIsDpk );
  end
  else
  begin
    ParseUsesList( FRawInnerUsesList, FInnerUsesList, bIsDpk );
  end;
end;

procedure TUnitParser.ParseUsesList(const lstIn, lstOut: TStrings; bIsDkp: boolean);
var
  i, c: integer;
  j, d: integer;
  k: integer;
  s: string;
  bInCurlyBracketComment: boolean;
  bInParenthesisComment: boolean;
  bInQuote: boolean;
  sCurrentUnit: string;
  bUsesSkipped: boolean;
  bIsDprInUses: boolean;
  iSep: integer;
begin
  // remove uses
  // remove ;

  // seperate at ,
  // try to filter comments (but not in "file.pas" notation)

  bInQuote := False;
  bInCurlyBracketComment := False;
  bInParenthesisComment := False;
  bUsesSkipped := False;

  bIsDprInUses := False;

  sCurrentUnit := '';

  c := lstIn.Count - 1;
  for i := 0 to c do
  begin
    s := lstIn[i];

    k := 1;
    if bIsDkp then
    begin
      if not bUsesSkipped and (Pos( 'contains', Trim(s) ) = 1) then
      begin
        k := Pos('contains', s) + 9;

        bUsesSkipped := True;
      end
      else if not bUsesSkipped and (Pos( 'requires', Trim(s) ) = 1) then
      begin
        k := Pos('requires', s) + 9;

        bUsesSkipped := True;
      end;
    end
    else
    begin
      if not bUsesSkipped and (Pos( 'uses', Trim(s) ) = 1) then
      begin
        k := Pos('uses', s) + 5;

        bUsesSkipped := True;
      end;
    end;

    d := Length(s);
    for j := k to d do
    begin

      if bInCurlyBracketComment or bInParenThesisComment then
      begin

        if (s[j] = '$') then
        begin
          bInCurlyBracketComment := False;

          sCurrentUnit := '{$';
        end
        else if (s[j] = '}') then
        begin
          bInCurlyBracketComment := False;
        end
        else if (s[j] = '*') and (d > j + 1) and (s[j+1] = ')') then    // lazy eval if !!
        begin
          bInParenthesisComment := False;
        end;

      end
      else
      begin

        if bInQuote then
        begin
          if s[j] = '''' then
          begin
            bInQuote := False;
          end;
        end
        else if not bIsDprInUses and (s[j] = '''') then
        begin
          bInQuote := True;
        end
        else
        begin
          if (s[j] = '/') and (d > j + 1) and (s[j+1] = '/') then
          begin
            break; // comment -> skip te remainder of this line
          end;

          if not bIsDprInUses and (s[j] = '{') then
          begin
            bInCurlyBracketComment := True;
          end
          else if (s[j] = '(') and (d > j + 1) and (s[j+1] = '*') then
          begin
            bInParenthesisComment := True;
          end
          else
          begin

            if (s[j] = ',') or (s[j] = ';') then
            begin
              bIsDprInUses := False;

              iSep := Pos('}', sCurrentUnit);
              if iSep <> 0 then
              begin
                lstOut.Add(Trim(Copy(sCurrentUnit, 1, iSep)));
                sCurrentUnit := Copy(sCurrentUnit, iSep + 1);
              end;

              sCurrentUnit := Trim(sCurrentUnit);
              if sCurrentUnit <> '' then
              begin
                lstOut.Add(sCurrentUnit);
              end;
              sCurrentUnit := '';
            end
            else
            begin
              sCurrentUnit := sCurrentUnit + s[j];

              if EndsText(' in ', sCurrentUnit) then
              begin
                bIsDprInUses := True;
              end;
            end;

          end;

        end;

      end;

      if not bIsDprInUses and (s[j] = '{') and not bInCurlyBracketComment then
      begin
        bInCurlyBracketComment := True;
      end

    end;

  end;

end;

{$IFDEF DELPHI2009_UP}
function TUnitParser.ReadLoopUntilSemiColon(const sIn: string; iStart: integer; var sOut: string): boolean;
{$ELSE}
function TUnitParser.ReadLoopUntilSemiColon(const sIn: ansistring; iStart: integer; var sOut: ansistring): boolean;
{$ENDIF}
var
  i: integer;
  StartIdx: Integer;
begin
  Result := False;

  if iStart >= 1 then
  begin
    StartIdx := iStart;
  end
  else
  begin
    StartIdx := 1;
  end;

  // todo: skip ; in comments

  i := PosEx(';', sIn, StartIdx);
  if i <> 0 then
  begin
    sOut := sOut + Copy(sIn, StartIdx, i - StartIdx + 1);

    Result := True;
  end
  else
  begin
    sOut := sOut + Copy(sIn, StartIdx);
  end;
end;

function TUnitParser.GetHasOuterUses: Boolean;
begin
  Result := FOuterUsesBoundaries.Start <> FOuterUsesBoundaries.Stop;
end;

function TUnitParser.GetHasInnerUses: Boolean;
begin
  Result := FInnerUsesBoundaries.Start <> FInnerUsesBoundaries.Stop
end;

//----------


function GetFirstMethodDefinitionFromSourceByName(const sPascalFile, sMethodName: string; const sClassName: string; bIncludeLineNumer: boolean): TMethodDefinition;
var
  parser: TUnitParser;
  s: TFileStream;
  i, c: integer;
  mdef: TMethodDefinition;
begin
  Result := nil;

  parser := TUnitParser.Create;
  try
    s := TFileStream.Create( sPascalFile, fmOpenRead );
    try
      parser.ParseUnit(s, true, false, false, false, bIncludeLineNumer);

      c := parser.MethodList.Count - 1;
      for i := 0 to c do
      begin
        mdef := TMethodDefinition(parser.MethodList[i]);
        if SameText( mdef.DefMethodName, sMethodName ) and SameText( mdef.InClass, sClassName ) then
        begin
          Result := mdef;
          parser.MethodList[i] := nil;  // zodat object niet automatisch gefreed word
          Exit;
        end;
      end;
    finally
      s.Free;
    end;
  finally
    parser.Free;
  end;
end;

function GetFirstMethodDefinitionFromSourceBySignature( const sPascalFile: string; const sSignature: string; bIncludeLineNumer: boolean = False ): TMethodDefinition;
var
  parser: TUnitParser;
  s: TFileStream;
  sigmdef: TMethodDefinition;
  refmdef: TMethodDefinition;
begin
  parser := TUnitParser.Create;
  try
    s := TFileStream.Create( sPascalFile, fmOpenRead );
    try
      parser.ParseUnit(s, true, false, false, false, bIncludeLineNumer);
    finally
      s.Free;
    end;

    sigmdef := TMethodDefinition.Create(sSignature, true, csUnknown);
    try
      refmdef := parser.FindClosestMethodDefinition(sigmdef);

      Result := TMethodDefinition.Create(refmdef);  // kopie maken, word anders gefreed met parser.Free
    finally
      sigmdef.Free;
    end;
  finally
    parser.Free;
  end;
end;

end.
