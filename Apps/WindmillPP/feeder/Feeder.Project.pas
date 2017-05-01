unit Feeder.Project;

interface

uses
  Feeder.Interfaces,
  uProjectParser;

type
  TFeederProject = class(TInterfacedObject, IFeeder)
  private
    FCurrentFileIdx: Integer;
    FProjectParser: TProjectParser;
    FProjectRoot: string;
  public
    constructor Create(const Projectfilepath: string);
    destructor Destroy; override;

    function Eof: Boolean;
    function NextFile: string;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils;

constructor TFeederProject.Create(const Projectfilepath: string);
begin
  inherited Create;

  FProjectParser := TProjectParser.Create;
  FProjectParser.ParseDProj(Projectfilepath, False);

  FProjectRoot := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(TPath.GetFullPath(Projectfilepath)));

  FCurrentFileIdx := 0;
end;

destructor TFeederProject.Destroy;
begin
  FreeAndNil(FProjectParser);

  inherited;
end;

function TFeederProject.Eof: Boolean;
begin
  Result := FCurrentFileIdx >= FProjectParser.Files.Count;
end;

function TFeederProject.NextFile: string;
begin
  if not Eof then
  begin
    Result :=
      FProjectRoot +
      FProjectParser.Files[FCurrentFileIdx];

    Inc(FCurrentFileIdx);
  end
  else
  begin
    raise EEndOfFiles.Create('We''re at the end of filelist');
  end;
end;

end.
