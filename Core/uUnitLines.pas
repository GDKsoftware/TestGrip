unit uUnitLines;

interface

uses
  System.Classes,
  uUnitParser,
  uPascalDefs;

type
  TUnitLines = class
  private
    FUnitParser: TUnitParser;
    FFullListing: TStrings;
  public
    constructor Create(const UnitStream: TStream; const UnitParser: TUnitParser);
    destructor Destroy; override;

    function GetLinesForMethod(const Method: TMethodDefinition): TStrings;
  end;

implementation

uses
  StrUtils, SysUtils;

constructor TUnitLines.Create(const UnitStream: TStream; const UnitParser: TUnitParser);
var
  Stream: TStringStream;
begin
  FUnitParser := UnitParser;

  FFullListing := TStringList.Create;

  UnitStream.Seek(0, soBeginning);

  Stream := TStringStream.Create;
  try
    Stream.LoadFromStream(UnitStream);

    FFullListing.Text := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

destructor TUnitLines.Destroy;
begin
  FreeAndNil(FFullListing);
end;

function TUnitLines.GetLinesForMethod(const Method: TMethodDefinition): TStrings;
var
  NextMethod: TMethodDefinition;
  Idx: Integer;
begin
  Result := TStringList.Create;

  NextMethod := FUnitParser.FindNextMethodAfter(Method);
  if Assigned(NextMethod) then
  begin
    for Idx := Method.LineNumber to NextMethod.LineNumber - 2 do
    begin
      Result.Add(FFullListing[Idx]);
    end;
  end
  else
  begin
    for Idx := Method.LineNumber to FFullListing.Count - 1 do
    begin
      if ContainsText(FFullListing[Idx], 'end.') then
        break;

      Result.Add(FFullListing[Idx]);
    end;
  end;

  Result.Text := TrimRight(Result.Text);
end;

end.
