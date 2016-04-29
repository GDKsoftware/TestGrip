unit Main.Generator;

interface

uses
  uProjectParser, uUnitParser, Classes, uPascalDefs, Service.Generator, Index.Generator;

type
  TMainGenerator = class(TObject)
  private
    FProjectFile: string;
    FDestination: string;
    FProjectFolder: string;

    FProjectParser: TProjectParser;

    FIndexGenerator: TIndexGenerator;

    procedure DocumentUnit(const AUnitFilename: string);
    procedure AddByClassDefinition(const AUnitParser: TUnitParser; const AClassDefObj: TClassDefinition);
    procedure AddMethodsToServiceGenerator(const AClassDefObj: TClassDefinition; const AServiceGenerator: TServiceGenerator; const AUnitParser: TUnitParser);
  public
    constructor Create(const AProjectFile: string; const ADestination: string);
    destructor Destroy; override;

    procedure Run;
  end;

implementation

uses
  System.SysUtils;

{ TMainGenerator }

constructor TMainGenerator.Create(const AProjectFile, ADestination: string);
begin
  inherited Create;
  FProjectFile := AProjectFile;
  FDestination := ADestination;
  FProjectFolder := ExtractFilePath(AProjectFile);

  FIndexGenerator := TIndexGenerator.Create(FDestination);

  FProjectParser := TProjectParser.Create;
  FProjectParser.ParseDProj(AProjectFile, False);
end;

destructor TMainGenerator.Destroy;
begin
  FreeAndNil(FProjectParser);
  FreeAndNil(FIndexGenerator);

  inherited;
end;

procedure TMainGenerator.DocumentUnit(const AUnitFilename: string);
var
  UnitParser: TUnitParser;
  ClassDefObj: TClassDefinition;
  I, C: Integer;
begin
  UnitParser := TUnitParser.Create;
  try
    UnitParser.ParseFromFile(AUnitFilename, True);

    C := UnitParser.InterfaceClassList.Count - 1;
    for I := 0 to C do
    begin
      ClassDefObj := TClassDefinition(UnitParser.InterfaceClassList.Objects[I]);
      if Assigned(ClassDefObj) then
      begin
        AddByClassDefinition(UnitParser, ClassDefObj);
      end;
    end;
  finally
    UnitParser.Free;
  end;
end;

procedure TMainGenerator.Run;
var
  UnitName: string;
begin
  for UnitName in FProjectParser.Files do
  begin
    try
      DocumentUnit(FProjectFolder + UnitName);
    except
      on E: Exception do
      begin
        // if a file is missing, mention it, but keep going
        System.Writeln(E.ClassName, ': ', E.Message);
      end;
    end;
  end;

  FIndexGenerator.Generate;
end;

procedure TMainGenerator.AddMethodsToServiceGenerator(const AClassDefObj: TClassDefinition; const AServiceGenerator: TServiceGenerator; const AUnitParser: TUnitParser);
var
  Methods: TList;
  C: Integer;
  I: Integer;
begin
  Methods := TList.Create;
  try
    AUnitParser.ListInterfaceMethods(AClassDefObj.Name, Methods);
    C := Methods.Count - 1;
    for I := 0 to C do
    begin
      AServiceGenerator.AddMethod(Methods[I]);
    end;
  finally
    Methods.Free;
  end;
end;

procedure TMainGenerator.AddByClassDefinition(const AUnitParser: TUnitParser; const AClassDefObj: TClassDefinition);
var
  ServiceGenerator: TServiceGenerator;
begin
  if AClassDefObj.IsInterface and AClassDefObj.HasAnnotation('ServiceContract') then
  begin
    ServiceGenerator := TServiceGenerator.Create(AClassDefObj, FDestination);
    try
      AddMethodsToServiceGenerator(AClassDefObj, ServiceGenerator, AUnitParser);

      FIndexGenerator.AddLink(ServiceGenerator.Generate);
    finally
      ServiceGenerator.Free;
    end;
  end;
end;

end.
