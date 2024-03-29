unit uPascalDefs;

interface

{$I ..\Testgrip.inc}

uses
  Classes;

type
  TClassScope = (csUnknown = 0, csPrivate, csProtected, csPublic, csPublished);

  TVariableDefinition = class
  protected
    FRawDefinition: string;
    FInClass: string;
    FName: string;
    FType: string;

    procedure ParseRawDefinition; virtual;
  public
    property InClass: string
      read FInClass write FInClass;
    property PropertyName: string
      read FName;
    property PropertyType: string
      read FType;

    constructor Create( const sRawDefinition: string; const sClassName: string ); virtual;
    destructor Destroy; override;
  end;

  TPropertyDefinition = class(TVariableDefinition)
  protected
    procedure ParseRawDefinition; override;
  public
    constructor Create( const sRawDefinition: string; const sClassName: string ); override;
    destructor Destroy; override;
  end;


  TMethodDefinition = class
  protected
    FInClass: string;
    FDefMethodName: string;
    FParameters: TStrings;
    FParamTypes: TStrings;
    FFunctype: string;
    FRawParamDefinition: string;
    FRawMethod: string;
    FParamDefaultValues: TStrings;

    FLineNumber: integer;

    FScope: TClassScope;

    FAnnotations: TStrings;

    FIsVirtual: Boolean;
    FIsOverride: Boolean;
    FIsOverloaded: Boolean;

    procedure ParseRawMethodString;
    procedure ParseRawParamDefinition;

    procedure ParseAsSignature;

    function GetSignature: string;

    procedure Clear;
  public
    property InClass: string
      read FInClass write FInClass;
    property DefMethodName: string
      read FDefMethodName;
    property Functype: string
      read FFunctype;
    property Parameters: TStrings
      read FParameters;
    property ParamTypes: TStrings
      read FParamTypes;
    property ParamDefaultValues: TStrings
      read FParamDefaultValues;
    property Annotations: TStrings
      read FAnnotations;

    property RawParamDefinition: string
      read FRawParamDefinition write FRawParamDefinition;

    property RawMethod: string
      read FRawMethod;

    property Signature: string
      read GetSignature;

    property LineNumber: integer
      read FLineNumber write FLineNumber;

    property Scope: TClassScope
      read FScope write FScope;

    property IsVirtual: Boolean
      read FIsVirtual write FIsVirtual;
    property IsOverride: Boolean
      read FIsOverride write FIsOverride;
    property IsOverloaded: Boolean
      read FIsOverloaded write FIsOverloaded;

    constructor Create( const sName: string; const sFuncType: string ); overload;
    constructor Create( const sRawDefinition: string; bIsSignature: boolean; aClassScope: TClassScope ); overload;
    constructor Create( const aSourceMDef: TMethodDefinition ); overload;
    destructor Destroy; override;

    function ParamImplString: string;
    function ParamsAndTypes: string;
  end;

  TClassDefinition = class
  protected
    FRawDefinition: string;
    FInherits: TStrings;
    FName: string;
    FIsInterface: boolean;

    // FInterfaceGUID: string;
    // FInterfaceParents: string;
    FAnnotations: TStrings;

    /// <summary>parse all inherited class/interface names into the FInherits list</summary>
    procedure ParseInheritDef(const s: string);

    /// <summary>parse a raw class definition into a name, class/interface type, and what classes/interfaces it inherits</summary>
    procedure Parse(const sRawDefinition: string);

    procedure SetType(const s: string);
  public
    property Name: string
      read FName;
    property IsInterface: boolean
      read FIsInterface;
    property Inherits: TStrings
      read FInherits;
    property Annotations: TStrings
      read FAnnotations;

    constructor Create(const ADefinition: TClassDefinition); overload;
    constructor Create(const sRawDefinition: string); overload;
    destructor Destroy; override;

    procedure Reparse(const sRawDefinition: string);

    function HasAnnotation(const AAnnotation: string): Boolean;
  end;

  function DetermineClassSigniture(const s: string; bIncludeClassname: boolean = True): string;

implementation

uses
  StrUtils,
  SysUtils, uCommonFunctions, uD7Functions;

//  uCommonFunctions;

{ TMethodDefinition }

constructor TMethodDefinition.Create( const sName: string; const sFuncType: string );
begin
  inherited Create;

  FLineNumber := -1;

  FDefMethodName := sName;
  FFunctype := sFuncType;
  FParameters := TStringList.Create;
  FParamTypes := TStringList.Create;
  FParamDefaultValues := TStringList.Create;
  FAnnotations := TStringList.Create;
end;

procedure TMethodDefinition.Clear;
begin
  FAnnotations.Clear;
  FParameters.Clear;
  FParamTypes.Clear;
  FParamDefaultValues.Clear;
  FInClass := '';
  FDefMethodName := '';
  FRawParamDefinition := '';
  FLineNumber := 0;
  FFunctype := '';
end;

constructor TMethodDefinition.Create(const sRawDefinition: string; bIsSignature: boolean; aClassScope: TClassScope );
begin
  inherited Create;

  FLineNumber := -1;

  FScope := aClassScope;

  FRawMethod := sRawDefinition;
  FParameters := TStringList.Create;
  FParamTypes := TStringList.Create;
  FParamDefaultValues := TStringList.Create;
  FAnnotations := TStringList.Create;

  if bIsSignature then
  begin
    ParseAsSignature;
  end
  else
  begin
    ParseRawMethodString;
  end;
end;

constructor TMethodDefinition.Create(const aSourceMDef: TMethodDefinition);
begin
  inherited Create;

  FParameters := TStringList.Create;
  FParamTypes := TStringList.Create;
  FParamDefaultValues := TStringList.Create;
  FAnnotations := TStringList.Create;

  if Assigned(aSourceMDef) then
  begin
    FRawMethod := aSourceMDef.FRawMethod;
    FRawParamDefinition := aSourceMDef.FRawParamDefinition;

    FParamDefaultValues.AddStrings(aSourceMDef.FParamDefaultValues);
    FParameters.AddStrings(aSourceMDef.FParameters);
    FParamTypes.AddStrings(aSourceMDef.FParamTypes);
    FAnnotations.AddStrings(aSourceMDef.FAnnotations);

    FInClass := aSourceMDef.FInClass;
    FDefMethodName := aSourceMDef.FDefMethodName;
    FLineNumber := aSourceMDef.FLineNumber;
    FFunctype := aSourceMDef.FFunctype;
  end;
end;

destructor TMethodDefinition.Destroy;
begin
  FreeAndNil(FParamDefaultValues);
  FreeAndNil(FParamTypes);
  FreeAndNil(FParameters);
  FreeAndNil(FAnnotations);

  inherited;
end;

function TMethodDefinition.GetSignature: string;
var
  i, c: integer;
begin
  Result := '';

  c := FParameters.Count - 1;
  for i := 0 to c do
  begin
    if i <> 0 then
    begin
      Result := Result + ',';
    end;

    if FParamTypes.Count > I then
    begin
      Result := Result + FParamTypes[i];
    end;
  end;

  if FInClass <> '' then
  begin
    Result := FFunctype + ' ' + FInClass + '.' + FDefMethodName + '(' + Result +  ')';
  end
  else
  begin
    Result := FFunctype + ' ' + FDefMethodName + '(' + Result +  ')';
  end;

  Result := LowerCase(Result);
end;

function TMethodDefinition.ParamImplString: string;
var
  i, c: integer;
begin
  Result := '';

  c := FParameters.Count - 1;
  for i := 0 to c do
  begin
    if i <> 0 then
    begin
      Result := Result + ', ';
    end;

    Result := Result + FParameters[i];
  end;
end;

function TMethodDefinition.ParamsAndTypes: string;
var
  i, c: integer;
begin
  Result := '';

  c := FParameters.Count - 1;
  for i := 0 to c do
  begin
    if i <> 0 then
    begin
      Result := Result + '; ';
    end;

    Result := Result + FParameters[i] + ': ' + FParamTypes[i];
  end;
end;

procedure TMethodDefinition.ParseAsSignature;
var
  i,c: integer;
  bInResult, bInClassName, bInMethodName, bInParams: boolean;
  s: string;
  ch: Char;
begin
  Clear;

  bInResult     := True;
  bInClassName  := False;
  bInMethodName := False;
  bInParams     := False;

  s := '';

  c := Length(FRawMethod);
  i := 1;
  while i <= c do
  begin
    ch := FRawMethod[i];

    if ch = ' ' then
    begin
      if bInResult then
      begin
        FFunctype := s;
      end;

      bInResult := False;
      bInClassName := True;

      s := '';
    end
    else if ch = '.' then
    begin
      if bInClassName or bInResult then
      begin
        FInClass := s;
      end;

      bInResult := False;
      bInClassName := False;

      bInMethodName := True;

      s := '';
    end
    else if ch = '(' then
    begin
      if bInMethodName or bInResult then
      begin
        FDefMethodName := s;
      end;

      bInMethodName := False;
      bInResult := False;
      bInParams := True;

      s := '';
    end
    else if ch = ',' then
    begin
      if bInParams then
      begin
        FParameters.Add('');
        FParamTypes.Add(s);
      end;

      s := '';
    end
    else if ch = ')' then
    begin
      if bInParams then
      begin
        FParameters.Add('');
        FParamTypes.Add(s);
      end;

      bInParams := False;

      s := '';
    end
    else
    begin
      s := s + ch;
    end;

    Inc(i);
  end;
end;

procedure TMethodDefinition.ParseRawMethodString;
var
  ch: Char;
  i, c: integer;
  iOldLen: integer;
  bInType, bInClassName, bInMethodName, bInParams, bInResult: boolean;
begin
  Clear;

  FRawMethod := ReplaceStr( FRawMethod, #9, ' ' );
  FRawMethod := ReplaceStr( FRawMethod, #13#10, ' ' );
  FRawMethod := ReplaceStr( FRawMethod, #13, ' ' );
  FRawMethod := ReplaceStr( FRawMethod, #10, ' ' );

  iOldLen := Length(FRawMethod);
  FRawMethod := ReplaceStr( FRawMethod, '  ', ' ' );
  while Length(FRawMethod) <> iOldLen do
  begin
    iOldLen := Length(FRawMethod);
    FRawMethod := ReplaceStr( FRawMethod, '  ', ' ' );
  end;

  bInType := True;
  bInClassName := False;
  bInMethodName := False;
  bInParams := False;
  bInResult := False;

  c := Length(FRawMethod);
  for i := 1 to c do
  begin
    ch := FRawMethod[i];

    if bInType then
    begin
      FFunctype := FFuncType + ch;

      if (Trim(FFunctype) <> '') and (ch = ' ') then
      begin
        FFuncType := LowerCase( Trim(FFunctype) );
        bInType := False;
        bInClassName := True;

        if SameText(FFuncType, 'procedure') then
        begin
          FFuncType := 'void';
        end
        else if SameText(FFuncType, 'function' ) then
        begin
          FFunctype := '';
        end
        else if SameText(FFuncType, 'constructor') then
        begin
          FFuncType := 'constructor';
        end
        else if SameText(FFuncType, 'destructor') then
        begin
          FFunctype := 'destructor';
        end;
      end;
    end
    else if bInClassName then
    begin
      if ch = '.' then
      begin
        bInClassName := False;
        bInMethodName := True;
      end
      else if ch = '(' then
      begin
        bInClassName := False;
        bInParams := True;
        FDefMethodName := FInClass;
        FInClass := '';
      end
      else if ch = ':' then
      begin
        bInClassName := False;
        bInResult := True;
        FDefMethodName := FInClass;
        FInClass := '';
      end
      else if ch = ';' then
      begin
        FDefMethodName := FInClass;
        FInClass := '';

        break;
      end
      else
      begin
        FInClass := FInClass + ch;
      end;
    end
    else if bInMethodName then
    begin
      if ch = '(' then
      begin
        bInMethodName := False;
        bInParams := True;
      end
      else if ch = ':' then
      begin
        bInMethodName := False;
        bInResult := True;
      end
      else if ch = ';' then
      begin
        break;
      end
      else
      begin
        FDefMethodName := FDefMethodName + ch;
      end;
    end
    else if bInParams then
    begin
      if ch = ')' then
      begin
        bInParams := False;
      end
      else
      begin
        FRawParamDefinition := FRawParamDefinition + ch;
      end;
    end
    else if bInResult then
    begin
      if ch = ';' then
      begin
        FFunctype := Trim(FFunctype);

        break;
      end
      else
      begin
        FFunctype := FFunctype + ch;
      end;
    end
    else if ch = ':' then
    begin
      bInResult := True;
    end
    else if not bInParams and (ch = ';') then
    begin
      // the end!
      break;
    end;

  end;

  FDefMethodName := Trim(FDefMethodName);

  FInClass := DetermineClassSigniture(FInClass);

  ParseRawParamDefinition;
end;

procedure TMethodDefinition.ParseRawParamDefinition;
var
  i, c: integer;
  ch: char;
  sCurrentParam: string;
  sCurrentType: string;
  p: integer;
  j: integer;
  pDefault: integer;
begin
  sCurrentParam := '';
  sCurrentType := '';

  c := Length(FRawParamDefinition);
  for i := 1 to c do
  begin
    ch := FRawParamDefinition[i];

    if ch = ';' then
    begin
      p := Pos(':', sCurrentParam);
      if p <> 0 then
      begin
        sCurrentType := Trim(Copy(sCurrentParam, p + 1));
        sCurrentParam := Copy(sCurrentParam, 1, p - 1 );

        if Trim(sCurrentParam) <> '' then
        begin
          sCurrentParam := Trim(sCurrentParam);

          p := TCommonStringFunctions.RPos(' ', sCurrentParam);
          if p <> 0 then
          begin
            FParameters.Add( Trim(Copy(sCurrentParam,p + 1)) );
          end
          else
          begin
            FParameters.Add( sCurrentParam );
          end;
        end;

        for j := FParamTypes.Count to FParameters.Count - 1 do
        begin
          pDefault := Pos('=', sCurrentType);
          if pDefault <> 0 then
          begin
            FParamTypes.Add( Trim(Copy(sCurrentType,1,pDefault-1)) );
            FParamDefaultValues.Add( Trim(Copy(sCurrentType,pDefault+1)) );
          end
          else
          begin
            FParamTypes.Add( Trim(sCurrentType) );
            FParamDefaultValues.Add( '' );
          end;
        end;
      end;

      sCurrentParam := '';
    end
    else if ch = ',' then
    begin
      if Trim(sCurrentParam) <> '' then
      begin
        sCurrentParam := Trim(sCurrentParam);

        p := TCommonStringFunctions.RPos(' ', sCurrentParam);
        if p <> 0 then
        begin
          FParameters.Add( Trim(Copy(sCurrentParam,p + 1)) );
        end
        else
        begin
          FParameters.Add( sCurrentParam );
        end;
      end;

      sCurrentParam := '';
    end
    else
    begin
      sCurrentParam := sCurrentParam + ch;
    end;
  end;

  p := Pos(':', sCurrentParam);
  if p <> 0 then
  begin
    sCurrentType := Trim(Copy(sCurrentParam, p + 1));
    sCurrentParam := Copy(sCurrentParam, 1, p - 1 );
    if Trim(sCurrentParam) <> '' then
    begin
      sCurrentParam := Trim(sCurrentParam);

      p := TCommonStringFunctions.RPos(' ', sCurrentParam);
      if p <> 0 then
      begin
        FParameters.Add( Trim(Copy(sCurrentParam,p + 1)) );
      end
      else
      begin
        FParameters.Add( sCurrentParam );
      end;
    end;

    for j := FParamTypes.Count to FParameters.Count - 1 do
    begin
      pDefault := Pos('=', sCurrentType);
      if pDefault <> 0 then
      begin
        FParamTypes.Add( Trim(Copy(sCurrentType,1,pDefault-1)) );
        FParamDefaultValues.Add( Trim(Copy(sCurrentType,pDefault+1)) );
      end
      else
      begin
        FParamTypes.Add( Trim(sCurrentType) );
        FParamDefaultValues.Add( '' );
      end;
    end;
  end;
end;

{ TPropertyDefinition }

constructor TPropertyDefinition.Create(const sRawDefinition: string; const sClassName: string);
begin
  inherited Create(sRawDefinition, sClassName);
end;

destructor TPropertyDefinition.Destroy;
begin

  inherited;
end;

procedure TPropertyDefinition.ParseRawDefinition;
var
  p1, p2, p3: integer;
begin
  // different implementation from inherited;

  p1 := Pos( 'property ', FRawDefinition );
  if (p1 <> 0) then
  begin
    p2 := PosEx( ':', FRawDefinition, p1 + 9 );
    if (p2 <> 0) then
    begin
      FName := Trim( Copy(FRawDefinition, p1 + 9, p2 - p1 - 9) );

      p3 := PosEx( ' read ', FRawDefinition, p2 + 1 );
      if p3 <> 0 then
      begin
        FType := Trim( Copy(FRawDefinition, p2 + 1, p3 - p2 - 1 ) );
      end;
    end;
  end;
end;

{ TVariableDefinition }

constructor TVariableDefinition.Create(const sRawDefinition,
  sClassName: string);
begin
  FRawDefinition := sRawDefinition;
  FRawDefinition := ReplaceStr(FRawDefinition, '' + #$9, ' ');
  FRawDefinition := ReplaceStr(FRawDefinition, '' + #$13, ' ');
  FRawDefinition := ReplaceStr(FRawDefinition, '' + #$10, ' ');

  FInClass := sClassName;

  Self.ParseRawDefinition;
end;

destructor TVariableDefinition.Destroy;
begin

  inherited;
end;

procedure TVariableDefinition.ParseRawDefinition;
var
  p1, p2: integer;
begin
  p1 := PosEx( ':', FRawDefinition, 1 );
  if (p1 <> 0) then
  begin
    FName := Trim( Copy(FRawDefinition, 1, p1 - 1) );

    p2 := PosEx(';', FRawDefinition, p1 + 1);
    if p2 <> 0 then
    begin
      FType := Trim( Copy(FRawDefinition, p1 + 1, p2 - p1 - 1) );
    end;
  end;
end;


// -----------------------------------------------------------------------------

function DetermineClassSigniture(const s: string; bIncludeClassname: boolean): string;
var
  p1, p2: integer;
  i: integer;
  bConstraint: boolean;
begin
  bConstraint := False;

  p1 := PosEx('<', s);
  if p1 <> 0 then
  begin
    p2 := PosEx('>', s, p1 + 1);
    if p2 <> 0 then
    begin
      if bIncludeClassname then
      begin
        Result := Trim(Copy(s, 1, p1 - 1)) + '<';
      end
      else
      begin
        Result := '<';
      end;
      for i := p1 + 1 to p2 - 1 do
      begin
        if s[i] = ':' then
        begin
          // generic type constraint should be ignored for signiture because it may be missing in the implementation section
          bConstraint := True;
        end
        else if s[i] = ';' then
        begin
          bConstraint := False;
          Result := Result + ',';
        end
        else if s[i] = ',' then
        begin
          bConstraint := False;
          Result := Result + ',';
        end
        {$IFDEF DELPHI2009_UP}
        else if not bConstraint and not CharInSet(s[i], [#$20,#$9]) then
        {$ELSE}
        else if not bConstraint and not (s[i] in [#$20,#$9]) then
        {$ENDIF}
        begin
          Result := Result + s[i];
        end;
      end;
      Result := Result + '>';
    end
    else
    begin
      if bIncludeClassname then
      begin
        Result := s;  // geen correcte classe eigenlijk
      end
      else
      begin
        Result := '';
      end;
    end;
  end
  else
  begin
    if bIncludeClassname then
    begin
      Result := s;
    end
    else
    begin
      Result := '';
    end;
  end;
end;


{ TClassDefinition }

constructor TClassDefinition.Create(const sRawDefinition: string);
begin
  inherited Create;

  FRawDefinition := '';
  FInherits := TStringList.Create;
  FAnnotations := TStringList.Create;

  Parse(sRawDefinition);
end;

constructor TClassDefinition.Create(const ADefinition: TClassDefinition);
begin
  inherited Create;

  FInherits := TStringList.Create;
  FAnnotations := TStringList.Create;

  FRawDefinition := ADefinition.FRawDefinition;
  FIsInterface := ADefinition.IsInterface;
  FName := ADefinition.FName;

  FInherits.AddStrings(ADefinition.FInherits);
  FAnnotations.AddStrings(ADefinition.FAnnotations);
end;

destructor TClassDefinition.Destroy;
begin
  FreeAndNil(FInherits);
  FreeAndNil(FAnnotations);

  inherited;
end;

function TClassDefinition.HasAnnotation(const AAnnotation: string): Boolean;
var
  I, C: Integer;
begin
  Result := False;

  C := FAnnotations.Count - 1;
  for I := 0 to C do
  begin
    if SameText(AAnnotation, FAnnotations[I]) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TClassDefinition.Parse(const sRawDefinition: string);
var
  i, c: integer;
  bNamePart, bTypePart, bInhPart: Boolean;
  sTemp: string;
begin
  FRawDefinition := Trim(sRawDefinition);

  bNamePart := True;
  bTypePart := False;
  bInhPart := False;

  sTemp := '';
  c := Length(FRawDefinition);
  i := 1;
  while i <= c do
  begin

    if bNamePart then
    begin
      if FRawDefinition[i] = '=' then
      begin
        sTemp := Trim(sTemp);
        FName := sTemp;

        sTemp := '';
        bNamePart := False;
        bTypePart := True;
      end
      else
      begin
        sTemp := sTemp + FRawDefinition[i];
      end;
    end
    else if bTypePart then
    begin
      if FRawDefinition[i] = '(' then
      begin
        sTemp := Trim(sTemp);
        SetType(sTemp);

        sTemp := '';
        bInhPart := True;
        bTypePart := False;
      end
      else if FRawDefinition[i] = ';' then
      begin
        sTemp := Trim(sTemp);
        SetType(sTemp);

        sTemp := '';
        Exit;
      end
      else
      begin
        sTemp := sTemp + FRawDefinition[i];
      end;
    end
    else if bInhPart then
    begin
      if FRawDefinition[i] = ')' then
      begin
        ParseInheritDef(sTemp);

        sTemp := '';
        Exit;
      end
      else if FRawDefinition[i] = ';' then
      begin
        sTemp := '';
        Exit;
      end
      else
      begin
        sTemp := sTemp + FRawDefinition[i];
      end;
    end;

    Inc(i);
  end;

  sTemp := Trim(sTemp);
  if (sTemp <> '') then
  begin
    if bNamePart then
    begin
      FName := sTemp;
    end
    else if bTypePart then
    begin
      SetType(sTemp);
    end
    else if bInhPart then
    begin
      ParseInheritDef(sTemp);
    end;
  end;
end;

procedure TClassDefinition.ParseInheritDef(const s: string);
var
  i, c: integer;
  iLevel: integer;
  sTemp: string;
begin
  iLevel := 0;
  sTemp := '';

  i := 1;
  c := Length(s);
  while i <= c do
  begin
    if (iLevel = 0) and (s[i] = ',') then
    begin
      sTemp := Trim(sTemp);
      FInherits.Add(sTemp);
      sTemp := '';
    end
    else
    begin
      sTemp := sTemp + s[i];

      if s[i] = '>' then
      begin
        Dec(iLevel);
      end
      else if s[i] = '<' then
      begin
        Inc(iLevel);
      end;
    end;

    Inc(i);
  end;

  sTemp := Trim(sTemp);
  if sTemp <> '' then
  begin
    FInherits.Add(sTemp);
  end;
end;

procedure TClassDefinition.Reparse(const sRawDefinition: string);
begin
  Parse(sRawDefinition);
end;

procedure TClassDefinition.SetType(const s: string);
begin
  FIsInterface := SameText(s, 'interface');
end;

end.
