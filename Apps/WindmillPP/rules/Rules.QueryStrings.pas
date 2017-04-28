unit Rules.QueryStrings;

interface

uses
  Rules.Interfaces,
  Output.Interfaces,
  System.Classes,
  System.RegularExpressions,
  System.Generics.Collections,
  uPascalDefs;

type
  {$SCOPEDENUMS ON}
  TQueryStringPartType = (
    Unknown = 0,
    QuotedString = 1,
    ConcatPlus = 2,
    CRLF = 3,
    ParameterInString = 4,
    VariableOutside = 5,
    TheEnd = 6
  );
  {$SCOPEDENUMS OFF}

  TQueryStringPart = class
  public
    PartType: TQueryStringPartType;
    Content: string;
  end;

  TRulesQueryStrings = class(TInterfacedObject, IRulesOnMethod)
  private
    FOutput: IOutput;
    FSourceCode: TStrings;
    FSQLTextAssignmentRegex: TRegEx;
    FSQLAssignmentRegex: TRegEx;
    FSQLAddRegex: TRegEx;

    FCurrentQuery: TList<TQueryStringPart>;

    function HasVariableOutside: Boolean;
    function DefinitelyHasVariablesThatShouldBeParameters: Boolean;

    procedure UnwrapStringFrom(const StartIdx: Integer);
    function HasSQLAssignments: Boolean;

    procedure ProcessWithRegex(const Regex: TRegex; const Filepath: string; const Classdef: TClassDefinition; const Method: TMethodDefinition);
  public
    constructor Create(const Output: IOutput);
    destructor Destroy; override;

    procedure Process(const Filepath: string; const Classdef: TClassDefinition; const Method: TMethodDefinition; const Lines: TStrings);
  end;

implementation

uses
  System.StrUtils, System.SysUtils, uCommonFunctions;

constructor TRulesQueryStrings.Create(const Output: IOutput);
begin
  FOutput := Output;
  FSQLTextAssignmentRegex := TRegEx.Create('sql.text\s*:=', [roIgnoreCase]);
  FSQLAssignmentRegex := TRegEx.Create('sql\s*:=', [roIgnoreCase]);
  FSQLAddRegex := TRegEx.Create('sql.add\(', [roIgnoreCase]);
end;

destructor TRulesQueryStrings.Destroy;
begin
  FreeAndNil(FCurrentQuery);

  inherited;
end;

function TRulesQueryStrings.HasSQLAssignments: Boolean;
begin
  Result :=
    FSQLTextAssignmentRegex.IsMatch(FSourceCode.Text) or
    FSQLAssignmentRegex.IsMatch(FSourceCode.Text) or
    FSQLAddRegex.IsMatch(FSourceCode.Text);
end;

procedure TRulesQueryStrings.UnwrapStringFrom(const StartIdx: Integer);
var
  Idx, Len: Integer;
  CurrentChar: Char;
  CurrentPart: TQueryStringPart;
  CheckEndElse: string;
begin
  FreeAndNil(FCurrentQuery);
  FCurrentQuery := TList<TQueryStringPart>.Create;

  CurrentPart := TQueryStringPart.Create;
  FCurrentQuery.Add(CurrentPart);

  Idx := StartIdx;

  Len := FSourceCode.Text.Length;
  while Idx <= Len do
  begin
    CurrentChar := FSourceCode.Text[Idx];
    if (CurrentPart.PartType = TQueryStringPartType.Unknown) then
    begin
      if CurrentChar = '''' then
      begin
        CurrentPart.PartType := TQueryStringPartType.QuotedString;
      end
      else if CurrentChar = '+' then
      begin
        CurrentPart.PartType := TQueryStringPartType.ConcatPlus;
      end
      else if CurrentChar = '#' then
      begin
        CurrentPart.PartType := TQueryStringPartType.CRLF;
        CurrentPart.Content := '#';
      end
      else if CurrentChar = ';' then
      begin
        CurrentPart.PartType := TQueryStringPartType.TheEnd;
        Break;
      end
      else if CharInSet(CurrentChar, [' ',#9,#13,#10]) then
      begin
        // ignore spacing
      end
      else if CurrentChar = ')' then
      begin
        CurrentPart.PartType := TQueryStringPartType.TheEnd;
        CurrentPart.Content := CurrentPart.Content + CurrentChar;
      end
      else
      begin
        CurrentPart.PartType := TQueryStringPartType.VariableOutside;
        CurrentPart.Content := CurrentPart.Content + CurrentChar;
      end;
    end
    else if (CurrentPart.PartType = TQueryStringPartType.QuotedString) then
    begin
      if CurrentChar = '''' then
      begin
        if ((Idx+1) <= Len) and (FSourceCode.Text[Idx+1] = '''') then
        begin
          // just an escaped single quote
          CurrentPart.Content := CurrentPart.Content + '''';
          Inc(Idx);
        end
        else
        begin
          CurrentPart := TQueryStringPart.Create;
          FCurrentQuery.Add(CurrentPart);
        end;
      end
      else
      begin
        CurrentPart.Content := CurrentPart.Content + CurrentChar;
      end;
    end
    else if (CurrentChar = '+') then
    begin
      CurrentPart := TQueryStringPart.Create;
      CurrentPart.PartType := TQueryStringPartType.ConcatPlus;
      FCurrentQuery.Add(CurrentPart);
    end
    else if CurrentChar = ';' then
    begin
      CurrentPart := TQueryStringPart.Create;
      CurrentPart.PartType := TQueryStringPartType.TheEnd;
      FCurrentQuery.Add(CurrentPart);
      Break;
    end
    else if CurrentChar = ')' then
    begin
      CurrentPart := TQueryStringPart.Create;
      CurrentPart.PartType := TQueryStringPartType.Unknown;
      FCurrentQuery.Add(CurrentPart);
    end
    else if CurrentPart.PartType = TQueryStringPartType.ConcatPlus then
    begin
      CurrentPart := TQueryStringPart.Create;
      FCurrentQuery.Add(CurrentPart);
    end
    else
    begin
      CurrentPart.Content := CurrentPart.Content + CurrentChar;

      CheckEndElse := CurrentPart.Content.TrimLeft;
      if (CheckEndElse.StartsWith('else', True) and (CheckEndElse.Length > 4) and (CharInSet(CheckEndElse[5], [';',' ',#13,#10,#9]))) or
         (CheckEndElse.StartsWith('end', True) and (CheckEndElse.Length > 3) and (CharInSet(CheckEndElse[4], [';',' ',#13,#10,#9]))) then
      begin
        CurrentPart.PartType := TQueryStringPartType.TheEnd;
        Break;
      end;
    end;

    Inc(Idx);
  end;
end;

function TRulesQueryStrings.HasVariableOutside: Boolean;
var
  Part: TQueryStringPart;
  HasQuotedString: Boolean;
  HasVars: Boolean;
begin
  HasQuotedString := False;
  HasVars := False;

  for Part in FCurrentQuery do
  begin
    if Part.PartType = TQueryStringPartType.QuotedString then
    begin
      HasQuotedString := True;
    end
    else if Part.PartType = TQueryStringPartType.VariableOutside then
    begin
      HasVars := True;
    end;
  end;

  Result := HasQuotedString and HasVars;
end;

function TRulesQueryStrings.DefinitelyHasVariablesThatShouldBeParameters: Boolean;
var
  Part: TQueryStringPart;
  PreviousString: string;
  VariablePrecededByComparison: Boolean;
begin
  PreviousString := '';
  VariablePrecededByComparison := False;

  Result := False;

  for Part in FCurrentQuery do
  begin
    if Part.PartType = TQueryStringPartType.QuotedString then
    begin
      if VariablePrecededByComparison then
      begin
        if Part.Content.TrimLeft.StartsWith('.') then
        begin
          VariablePrecededByComparison := False;
        end;
      end;

      if VariablePrecededByComparison then
      begin
        Break;
      end;

      PreviousString := Part.Content.TrimRight;
    end
    else if Part.PartType = TQueryStringPartType.VariableOutside then
    begin
      VariablePrecededByComparison :=
        PreviousString.EndsWith('like', True) or
        PreviousString.EndsWith('=') or
        PreviousString.EndsWith('>') or
        PreviousString.EndsWith('<') or
        PreviousString.EndsWith('''');
    end
    else if Part.PartType = TQueryStringPartType.ConcatPlus then
    begin
      // skip the glue
    end
    else if VariablePrecededByComparison then
    begin
      Break;
    end;
  end;

  if VariablePrecededByComparison then
    Result := True;
end;

procedure TRulesQueryStrings.ProcessWithRegex(const Regex: TRegex; const Filepath: string; const Classdef: TClassDefinition; const Method: TMethodDefinition);
var
  Matches: TMatchCollection;
  Match: TMatch;
  StartLineOfQuery: Integer;
begin
  Matches := Regex.Matches(FSourceCode.Text, 1);
  for Match in Matches do
  begin
    if Match.Success then
    begin
      UnwrapStringFrom(Match.Index + Match.Length);

      if HasVariableOutside then
      begin
        StartLineOfQuery := TCommonStringFunctions.CountLines(FSourceCode.Text, lfDos, Match.Index + Match.Length);

        if DefinitelyHasVariablesThatShouldBeParameters then
        begin
          FOutput.Error(Filepath, Method.LineNumber + StartLineOfQuery + 1, 'SQL Query has concattenated variables that SHOULD be parameters');
        end
        else
        begin
          FOutput.Warning(Filepath, Method.LineNumber + StartLineOfQuery + 1, 'SQL Query has concattenated variables that MIGHT be parameters, check to be sure');
        end;
      end;
    end;
  end;
end;

procedure TRulesQueryStrings.Process(const Filepath: string; const Classdef: TClassDefinition; const Method: TMethodDefinition; const Lines: TStrings);
begin
  FSourceCode := Lines;

  if HasSQLAssignments then
  begin
    ProcessWithRegex(FSQLTextAssignmentRegex, Filepath, Classdef, Method);

    ProcessWithRegex(FSQLAssignmentRegex, Filepath, Classdef, Method);

    ProcessWithRegex(FSQLAddRegex, Filepath, Classdef, Method);
  end;
end;

end.
