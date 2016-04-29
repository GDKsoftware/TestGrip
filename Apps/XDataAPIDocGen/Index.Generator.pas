unit Index.Generator;

interface

uses
  System.Classes, XML.Generator;

type
  TIndexGenerator = class(TXMLGenerator)
  private
    FTargetFolder: string;
    FLinks: TStrings;
    procedure AddLinksToXML(var XML: string);
    procedure AddIndexToXML(var XML: string);
  public
    constructor Create(const ATargetFolder: string);
    destructor Destroy; override;

    procedure AddLink(const ARelativePath: string);

    procedure Generate;
  end;

implementation

uses
  System.SysUtils, uCommonFunctions;

{ TIndexGenerator }

procedure TIndexGenerator.AddLink(const ARelativePath: string);
begin
  FLinks.Add(ARelativePath);
end;

constructor TIndexGenerator.Create(const ATargetFolder: string);
begin
  inherited Create;

  FLinks := TStringList.Create;
  FTargetFolder := ATargetFolder;
end;

destructor TIndexGenerator.Destroy;
begin
  FreeAndNil(FLinks);

  inherited;
end;

procedure TIndexGenerator.Generate;
var
  XML: string;
begin
  XML := '';

  AddIndexToXML(XML);

  WriteXMLStringToFile(FTargetFolder + 'index.xml', XML);
end;

procedure TIndexGenerator.AddIndexToXML(var XML: string);
begin
  XML := XML + '<index>';
  AddLinksToXML(XML);
  XML := XML + '</index>';
end;

procedure TIndexGenerator.AddLinksToXML(var XML: string);
var
  Link: string;
  LinkCaption: string;
begin
  for Link in FLinks do
  begin
    LinkCaption := TCommonFileNameFunctions.RemoveFileExtension(ExtractFileName(Link));
    XML := XML + '<link url="' + Link + '">' + LinkCaption + '</link>';
  end;
end;

end.
