unit Service.Generator;

interface

uses
  uPascalDefs,
  System.Generics.Collections,
  XML.Generator;

type
  TServiceGenerator = class(TXMLGenerator)
  private
    FTargetFolder: string;
    FClassDefinition: TClassDefinition;
    FMethods: TObjectList<TMethodDefinition>;

    procedure AddAnnotationsToXML(var XML: string; Method: TMethodDefinition);
    procedure AddParametersToXML(var XML: string; Method: TMethodDefinition);
    procedure AddMethodToXML(var XML: string; Method: TMethodDefinition);
    procedure AddServiceToXML(EscapedServiceName: string; var XML: string);
  public
    constructor Create(const AClassDefiniton: TClassDefinition; const ATargetFolder: string);
    destructor Destroy; override;

    procedure AddMethod(const AMethodDefinition: TMethodDefinition);

    function Generate: string;
  end;

implementation

uses
  System.SysUtils, uXmlFuncs, System.Classes;

{ TServiceGenerator }

procedure TServiceGenerator.AddMethod(const AMethodDefinition: TMethodDefinition);
begin
  inherited Create;

  // add a copy
  FMethods.Add(TMethodDefinition.Create(AMethodDefinition));
end;

constructor TServiceGenerator.Create(const AClassDefiniton: TClassDefinition; const ATargetFolder: string);
begin
  inherited Create;

  FTargetFolder := ATargetFolder;
  FClassDefinition := TClassDefinition.Create(AClassDefiniton);
  FMethods := TObjectList<TMethodDefinition>.Create;

  FDirectXSLLink := 'output.xsl';
end;

destructor TServiceGenerator.Destroy;
begin
  FreeAndNil(FMethods);

  inherited;
end;

function TServiceGenerator.Generate: string;
var
  XML: string;
  EscapedServiceName: string;
begin
  EscapedServiceName := XmlFuncs_Escape(Copy(FClassDefinition.Name, 2));

  XML := '';

  AddServiceToXML(EscapedServiceName, XML);

  Result := EscapedServiceName + '.xml';
  WriteXMLStringToFile(FTargetFolder + Result, XML);
end;

procedure TServiceGenerator.AddServiceToXML(EscapedServiceName: string; var XML: string);
var
  Method: TMethodDefinition;
begin
  XML := XML + '<service name="' + EscapedServiceName + '">';
  for Method in FMethods do
  begin
    AddMethodToXML(XML, Method);
  end;
  XML := XML + '</service>';
end;

procedure TServiceGenerator.AddMethodToXML(var XML: string; Method: TMethodDefinition);
var
  ParametersXML: string;
  EscapedMethodName: string;
  AnnotationXML: string;
begin
  ParametersXML := '';
  AddParametersToXML(ParametersXML, Method);

  AnnotationXML := '';
  AddAnnotationsToXML(AnnotationXML, Method);

  EscapedMethodName := XmlFuncs_Escape(Method.DefMethodName);

  if (ParametersXML <> '') or (AnnotationXML <> '') then
    XML := XML + '<method name="' + EscapedMethodName + '">' + AnnotationXML + ParametersXML + '</method>'
  else
    XML := XML + '<method name="' + EscapedMethodName + '" />'
end;

procedure TServiceGenerator.AddAnnotationsToXML(var XML: string; Method: TMethodDefinition);
var
  Annotation: string;
begin
  for Annotation in Method.Annotations do
  begin
    XML := XML + '<annotation>' + XmlFuncs_Escape(Annotation) + '</annotation>';
  end;
end;

procedure TServiceGenerator.AddParametersToXML(var XML: string; Method: TMethodDefinition);
var
  I, C: Integer;
begin
  C := Method.Parameters.Count - 1;
  for I := 0 to C do
  begin
    XML := XML + '<param name="' + XmlFuncs_Escape(Method.Parameters[I]) + '" type="' + XmlFuncs_Escape(Method.ParamTypes[I]) + '" />';
  end;
end;

end.
