unit uDefinitionSearch;

interface

uses
  Classes,
  uUnitParser,
  uProjectParser;

type
  TDefinitionSearch = class
  protected
    FProjectFile: string;

    FAllUnits: TStringList;
    FExtraFiles: TStringList;

    FProject: TProjectParser;

    FSearchFor: string;

    procedure SetUnitParser(const sUnitname: string; const oParser: TUnitParser);

    procedure InitSearch;
    function DoSearch(const sUnit: string): TUnitParser;
    procedure FiniSearch;
  public
    property ProjectFile: string
      read FProjectFile;

    constructor Create(const sProjectFile: string);
    destructor Destroy; override;

    function Search(const lstUses: TStrings; const sClassName: string): TUnitParser;
  end;

implementation

uses
  SysUtils;

{ TDefinitionSearch }

constructor TDefinitionSearch.Create(const sProjectFile: string);
begin
  FProjectFile := sProjectFile;

  FProject := TProjectParser.Create;
  FExtraFiles := TStringList.Create;
  FAllUnits := TStringList.Create;
end;

destructor TDefinitionSearch.Destroy;
begin
  FreeAndNil(FAllUnits);
  FreeAndNil(FExtraFiles);
  FreeAndNil(FProject);

  inherited;
end;

function TDefinitionSearch.DoSearch(const sUnit: string): TUnitParser;
var
  sFile: string;
  oParser: TUnitParser;
  stream: TFileStream;
  i, c: integer;
  sClass: string;
begin
  Result := nil;

  sFile := FProject.GetUnitFilename(sUnit);
  if sFile = '' then
  begin
    // file not found in unitlisting, try to refresh Project and try again..
    FProject.Refresh;

    sFile := FProject.GetUnitFilename(sUnit);

    // todo: if it's still missing, try to search through the SearchPaths
  end;

  if sFile <> '' then
  begin
    oParser := TUnitParser.Create;

    stream := TFileStream.Create(sFile, fmOpenRead);
    try
      oParser.ParseUnit(stream, True, False, False, False, False);

      SetUnitParser(sUnit, oParser);

      c := oParser.InterfaceClassList.Count - 1;
      for i := 0 to c do
      begin
        sClass := oParser.InterfaceClassList.Names[i];
        if SameText(sClass, FSearchFor) then
        begin
          Result := oParser;
        end;
      end;
    finally
      stream.Free;
    end;
  end
  else
  begin
    // todo: alternative search for unit
  end;
end;

procedure TDefinitionSearch.FiniSearch;
begin
  //
end;

procedure TDefinitionSearch.InitSearch;
begin
  FProject.ParseDProj(FProjectFile, False);
end;

function TDefinitionSearch.Search(const lstUses: TStrings; const sClassName: string): TUnitParser;
var
  i, c: integer;
  oUnitParser: TUnitParser;
begin
  Assert(Assigned(lstUses), 'Need Uses hint');

  Result := nil;

  FSearchFor := sClassName;

  // lookup classname through all uses -> detailed search by doing UnitParse, link parse to allfiles.objects
  InitSearch;
  try
    c := lstUses.Count - 1;
    for i := 0 to c do
    begin
      oUnitParser := DoSearch(lstUses[i]);
      if Assigned(oUnitParser) then
      begin
        Result := oUnitParser;
      end;
    end;
  finally
    FiniSearch;
  end;
end;

procedure TDefinitionSearch.SetUnitParser(const sUnitname: string; const oParser: TUnitParser);
var
  p: integer;
begin
  p := FAllUnits.IndexOf(sUnitname);
  if p <> -1 then
  begin
    FAllUnits.Objects[p].Free;

    FAllUnits.Objects[p] := oParser;
  end
  else
  begin
    FAllUnits.AddObject(sUnitname, oParser);
  end;
end;

end.
