unit uUsedUnitSearch;

interface

uses
  Classes,
  uProjectParser;

type
  TUnitNode = class
  protected
    FPath: string;
    FUnitNodeName: string;
    FIsTraversed: Boolean;
    FLinkedUnits: TStrings;
  public
    constructor Create(const AUnitNodeName: string; const APath: string = ''); overload;
    constructor Create(const AUnitNode: TUnitNode); overload;
    destructor Destroy; override;

    function IsLinked(const ANode:TUnitNode): Boolean;
    procedure Link(const ANode: TUnitNode);

    property NodeName: string read FUnitNodeName;
    property Path: string read FPath write FPath;
    property LinkedUnits: TStrings read FLinkedUnits;
    property IsTraversed: Boolean read FIsTraversed write FIsTraversed;
  end;

  TUsedUnitSearch = class
  protected
    FAllUnits: TStrings;
    FProject: TProjectParser;

    procedure LoadUnitsFromProject;

    function GetExistingUnitNode(const AUnit: string): TUnitNode;

    function GetFilepathForUnitInLibraryPaths(const AUnitName: string): string;
    function GetFilepathForUnitInSearchPaths(const AUnitName: string): string;
    function GetFullpathForUnit(const AUnitName: string; const APathHint: string): string;

    function GetOrAddUnitNode(const AUnitName: string; const APathHint: string): TUnitNode;
    function NewUnitNode(const AUnitName: string; FullPath: string): TUnitNode;

    procedure TravelAndExpandProjectUnits;
    procedure TravelAndExpandPascalFile(const ACurrentNode: TUnitNode);
    procedure TravelAndExpandList(const AParentNode: TUnitNode; const AList: TStrings); inline;
    procedure TravelAndExpand(const AParentNode: TUnitNode; const AUnit: string; const APathHint: string);

    procedure FreeAllUnitNodes;
  public
    constructor Create(const AProjectPath: string);
    destructor Destroy; override;

    function Search(const AUnitName: string): TStrings;
  end;

implementation

{ TUsedUnitSearch }

uses
  uUnitParser, SysUtils, uCommonFunctions;

const
  PasFileExtension = '.pas';

constructor TUsedUnitSearch.Create(const AProjectPath: string);
begin
  FAllUnits := TStringList.Create;

  FProject := TProjectParser.Create;
  FProject.ParseDProj(AProjectPath, False);

  LoadUnitsFromProject;
end;

destructor TUsedUnitSearch.Destroy;
begin
  FreeAllUnitNodes;
  FreeAndNil(FAllUnits);
  FreeAndNil(FProject);

  inherited;
end;

procedure TUsedUnitSearch.FreeAllUnitNodes;
var
  I, C: Integer;
  Obj: TUnitNode;
begin
  C := FAllUnits.Count - 1;
  for I := 0 to C do
  begin
    Obj := TUnitNode(FAllUnits.Objects[I]);
    FAllUnits.Objects[I] := nil;
    Obj.Free;
  end;

  FAllUnits.Clear;
end;

function TUsedUnitSearch.GetExistingUnitNode(const AUnit: string): TUnitNode;
var
  UnitIdx: Integer;
begin
  Result := nil;

  UnitIdx := FAllUnits.IndexOf(AUnit);
  if UnitIdx <> -1 then
  begin
    Result := TUnitNode(FAllUnits.Objects[UnitIdx]);
  end;
end;

function TUsedUnitSearch.GetOrAddUnitNode(const AUnitName: string; const APathHint: string): TUnitNode;
var
  FullPath: string;
begin
  Result := GetExistingUnitNode(AUnitName);
  if not Assigned(Result) then
  begin
    FullPath := GetFullpathForUnit(AUnitName, APathHint);

    Result := NewUnitNode(AUnitName, FullPath);
  end;
end;

procedure TUsedUnitSearch.LoadUnitsFromProject;
var
  I, C: Integer;
  UnitName: string;
begin
  C := FProject.Files.Count - 1;
  for I := 0 to C do
  begin
    UnitName := TCommonFileNameFunctions.RemoveFileExtension(FProject.Files[I]);
    GetOrAddUnitNode(UnitName, FProject.GetUnitFilename(UnitName));
  end;

  TravelAndExpandProjectUnits;
end;

function TUsedUnitSearch.GetFullpathForUnit(const AUnitName: string; const APathHint: string): string;
var
  FullPath: string;
begin
  Result := '';

  if APathHint <> '' then
  begin
    if APathHint.EndsWith(PasFileExtension, True) then
    begin
      FullPath := APathHint;
    end
    else
    begin
      FullPath := IncludeTrailingPathDelimiter(APathHint) + AUnitName + PasFileExtension;
    end;

    if FileExists(FullPath) then
    begin
      Result := FullPath;
    end;
  end
  else if Assigned(FProject) then
  begin
    Result := GetFilepathForUnitInSearchPaths(AUnitName);
    if Result = '' then
    begin
      Result := GetFilepathForUnitInLibraryPaths(AUnitName);
    end;
  end;
end;

function TUsedUnitSearch.Search(const AUnitName: string): TStrings;
var
  Node: TUnitNode;
  I, C: Integer;
  ListedUnit: TUnitNode;
begin
  Node := GetExistingUnitNode(AUnitName);
  if Assigned(Node) then
  begin
    Result := TStringList.Create;

    C := FAllUnits.Count - 1;
    for I := 0 to C do
    begin
      ListedUnit := TUnitNode(FAllUnits.Objects[I]);
      if ListedUnit.IsLinked(Node) then
      begin
        Result.Add(ListedUnit.Path);
      end;
    end;
  end
  else
  begin
    raise Exception.Create('Unit ''' + AUnitName + ''' not found');
  end;
end;

procedure TUsedUnitSearch.TravelAndExpandPascalFile(const ACurrentNode: TUnitNode);
var
  UnitParser: TUnitParser;
begin
  UnitParser := TUnitParser.Create;
  try
    // todo: actually still parses method definitions, maybe parameterize that to speed up parsing?
    UnitParser.ParseFromFile(ACurrentNode.Path, False, False, False, False);

    TravelAndExpandList(ACurrentNode, UnitParser.OuterUsesList);
    TravelAndExpandList(ACurrentNode, UnitParser.InnerUsesList);
  finally
    UnitParser.Free;
  end;
end;

function TUsedUnitSearch.GetFilepathForUnitInLibraryPaths(const AUnitName: string): string;
begin
  Result := '';

  // todo: lookup file in library paths? of which Delphi installation?
end;

function TUsedUnitSearch.GetFilepathForUnitInSearchPaths(const AUnitName: string): string;
var
  C: Integer;
  I: Integer;
  FullPath: string;
begin
  Result := '';

  C := FProject.SearchPath.Count - 1;
  for I := 0 to C do
  begin
    FullPath := IncludeTrailingPathDelimiter(FProject.SearchPath[I]) + AUnitName + PasFileExtension;
    if FileExists(FullPath) then
    begin
      Result := FullPath;
      Exit;
    end;
  end;
end;

function TUsedUnitSearch.NewUnitNode(const AUnitName: string; FullPath: string): TUnitNode;
begin
  Result := TUnitNode.Create(AUnitName, FullPath);
  FAllUnits.AddObject(AUnitName, Result);
end;

procedure TUsedUnitSearch.TravelAndExpandProjectUnits;
var
  I, C: Integer;
  UnitName: string;
begin
  C := FProject.Files.Count - 1;
  for I := 0 to C do
  begin
    UnitName := TCommonFileNameFunctions.RemoveFileExtension(FProject.Files[I]);

    TravelAndExpand(nil, UnitName, FProject.GetUnitFilename(UnitName));
  end;
end;

procedure TUsedUnitSearch.TravelAndExpand(const AParentNode: TUnitNode; const AUnit: string; const APathHint: string);
var
  CurrentNode: TUnitNode;
begin
  CurrentNode := GetOrAddUnitNode(AUnit, APathHint);
  if Assigned(AParentNode) then
  begin
    AParentNode.Link(CurrentNode);
  end;

  if not CurrentNode.IsTraversed then
  begin
    CurrentNode.IsTraversed := True;

    if CurrentNode.Path <> '' then
    begin
      TravelAndExpandPascalFile(CurrentNode);
    end;
  end;
end;

procedure TUsedUnitSearch.TravelAndExpandList(const AParentNode: TUnitNode; const AList: TStrings);
var
  I, C: Integer;
begin
  C := AList.Count - 1;
  for I := 0 to C do
  begin
    TravelAndExpand(AParentNode, AList[I], '');
  end;
end;

{ TUnitNode }

constructor TUnitNode.Create(const AUnitNodeName: string; const APath: string = '');
begin
  inherited Create;

  FLinkedUnits := TStringList.Create;
  FUnitNodeName := AUnitNodeName;
  FPath := APath;
end;

constructor TUnitNode.Create(const AUnitNode: TUnitNode);
begin
  inherited Create;

  FLinkedUnits := nil;

  FUnitNodeName := AUnitNode.NodeName;
  FPath := AUnitNode.Path;
  FIsTraversed := AUnitNode.IsTraversed;
end;

destructor TUnitNode.Destroy;
begin
  FreeAndNil(FLinkedUnits);

  inherited;
end;

function TUnitNode.IsLinked(const ANode: TUnitNode): Boolean;
begin
  Result := (FLinkedUnits.IndexOfObject(ANode) <> -1);
end;

procedure TUnitNode.Link(const ANode: TUnitNode);
begin
  if not IsLinked(ANode) then
  begin
    FLinkedUnits.AddObject(ANode.UnitName, ANode);
  end;
end;

end.
