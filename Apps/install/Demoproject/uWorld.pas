unit uWorld;

interface

uses
  classes, SysUtils, Math, DateUtils;

type

  {
  The following classes are created to give a list of possible usages for
  TestGrip. Browse thought all the tests, to get an overview of all the
  possibilities. Don't forget to open the advanced view, some tests are
  more complex.
  }

  // Forward declaration for using TWorldObject in TWorld
  TWorldObject = class;

  // Our main class
  TWorld = class
  strict private
    FWorldObjectList: TList;

    FWorldName: string;
  public
    constructor Create;
    destructor Destroy; override;

    // Functions to add and delete objects
    function AddWorldObject(aWorldObject: TWorldObject): integer;
    function DeleteWorldObject(WorldObjectIndex: integer): boolean;

    // and to get a specific object
    function GetWorldObjectItem(WorldObjectIndex: integer): TWorldObject;
    
    function WorldObjectCount: integer;

    // To demonstrate the functionality of the class parameters in TestGrip,
    // we have added this procedure. Please check the unit test in advanced view
    // and click on "Class settings".
    function GetWorldName: string;
    property Name: string
      read FWorldName write FWorldName;
  end;

  // All objects derive form this class
  TWorldObject = class
  private
    FName: string;
  public
    property Name: string
      read FName write FName;
  end;

  // Some objects are alive
  TLivingWorldObject = class(TWorldObject)
  private
    FBirthDate: TDateTime;
    FParent: TLivingWorldObject;
  public
    constructor Create;

    // We could use properties here, but instead we use overloaded functions
    // to demo the unit tests of this type of functions
    procedure SetBirthDate(aDate: TDateTime); overload;
    procedure SetBirthDate(YearsOld: integer); overload;

    function Age: integer;
    function IsBorn: boolean;

    // Use the reproduce function to create another instance of a TLivingWorldObject
    // with the same name, but a birthdate of now()
    function Reproduce: TLivingWorldObject;

    // Only available when the instance is created via the function Reproduce
    function Parent: TLivingWorldObject;
  end;

  TPerson = class(TLivingWorldObject)
  private
    FLength: integer;
    FWeight: double;
  protected
    procedure SetLength(const Value: integer);
    procedure SetWeight(const Value: double);
  public
    property Length: integer
      read FLength write SetLength;
    property Weight: double
      read FWeight write SetWeight;
  end;

  // To show the usage of this type in unit tests, check out the tests in the
  // function SetLocation
  TTreeLocation = (tlForest,tlBackyard);

  TTree = class(TLivingWorldObject)
  private
    FLocation: TTreeLocation;
  protected
    procedure SetLocation(const Value: TTreeLocation);
    function GetRingCount: integer;
  public
    property Location: TTreeLocation
      read FLocation write SetLocation;
    property RingCount: integer
      read GetRingCount;
  end;

  IHello = interface
    function Hello(const A, B: Integer): Integer;
  end;

  {
  A couple of functions without a class. Unit tests for this functions will be
  located in TestGrip under the item "Functions without a class".
  }

  // BMI calculation: BMI = weight / (Length * Length). Ideal BMI should be
  // between 20 and 25 :)
  function CalculateBMI(Length: double; Weight: double): double;

  // Converts Inches to Meters, meters with 2 decimals
  function InchToMeters(Inch: double): double;

  // Custom round function
  function Rounder(Value: Extended; Decimals: Integer): Extended;

implementation

{ TWorld }

function TWorld.AddWorldObject(aWorldObject: TWorldObject): integer;
begin
  result := FWorldObjectList.Add(aWorldObject);
end;

constructor TWorld.Create;
begin
  FWorldObjectList := TList.Create;
end;

function TWorld.DeleteWorldObject(WorldObjectIndex: integer): boolean;
begin
  try
    FWorldObjectList.Delete(WorldObjectIndex);
    result := true;
  except
    // For the purpose of testing, we don't raise the exception
    result := false;
  end;
end;

destructor TWorld.Destroy;
var
  i: integer;
begin
  // First, delete all the objects inside our list, then free the list
  // to avoid memory leaks
  for I := 0 to FWorldObjectList.Count -1 do
  begin
    TWorldObject(FWorldObjectList.Items[i]).Free;
  end;
  FWorldObjectList.Free;

  inherited;
end;

function TWorld.GetWorldObjectItem(WorldObjectIndex: integer): TWorldObject;
var
  WorldObject: TWorldObject;
begin
  if WorldObjectIndex > FWorldObjectList.Count -1 then
  begin
    result := nil
  end
  else
  begin
    WorldObject := TWorldObject(FWorldObjectList.Items[WorldObjectIndex]);

    if assigned(WorldObject) then
     result := WorldObject
    else
      result := nil;
  end;

end;

function TWorld.GetWorldName: string;
begin
  result := FWorldName;
end;

function TWorld.WorldObjectCount: integer;
begin
  result := FWorldObjectList.Count -1;
end;

{ TLivingWorldObject }

function TLivingWorldObject.Age: integer;
begin
  // A birthdate of 0 means no assigned, so -1 as result
  if FBirthDate = 0 then
    result := -1
  else
    result := YearsBetween(Date,FBirthDate);
end;

constructor TLivingWorldObject.Create;
begin
  FBirthDate := 0;
end;

function TLivingWorldObject.IsBorn: boolean;
begin
  // We assume 0 is not assigned yet
  result := FBirthDate > 0;
end;

function TLivingWorldObject.Parent: TLivingWorldObject;
begin
  if assigned(FParent) then
    result := FParent
  else
    result := nil;
end;

function TLivingWorldObject.Reproduce: TLivingWorldObject;
begin
  // Create a new instance of TLivingWorldObject and set the name
  result := TLivingWorldObject.Create;
  result.FParent := self;
  result.Name := self.Name;
  result.FBirthDate := date();
end;

procedure TLivingWorldObject.SetBirthDate(YearsOld: integer);
begin
  // Calculate the birthdate (not exactly, but for our demo it's good enough
  SetBirthDate(IncYear(Date(),YearsOld*-1));
end;

procedure TLivingWorldObject.SetBirthDate(aDate: TDateTime);
begin
  FBirthDate := aDate;
end;

{ TPerson }

procedure TPerson.SetLength(const Value: integer);
begin
  // We do have a max en minimal value (self defined( of the length variabele
  // Max = 250, min = 0. See the unit tests for the tests
  if Value < 0 then
    FLength := 0
  else if Value > 250 then
    FLength := 250
  else
    FLength := Value;
end;

procedure TPerson.SetWeight(const Value: double);
begin
  FWeight := Value;
end;

{ TTree }

function TTree.GetRingCount: integer;
begin
  // Each year one ring is added to our tree
  if FBirthDate = 0 then
    result := 0
  else
    result := YearsBetween(Date(),FBirthDate);
end;

procedure TTree.SetLocation(const Value: TTreeLocation);
begin
  FLocation := Value;
end;

function CalculateBMI(Length: double; Weight: double): double;
begin
  result := round(Weight / (Length * Length));
end;

function InchToMeters(Inch: double): double;
begin
  result := Rounder((Inch / 39.37),2);
end;

function Rounder(Value: Extended; Decimals: Integer): Extended;
var
  i: Integer;
  A: Extended;
begin
  if Decimals <= 0 then
    A := 10
  else
  begin
    A := 1;
    for i := 1 to Decimals do
      A := A * 10;
  end;

  Result := Math.Floor((Value * A) + 0.5) / A;
end;


end.
