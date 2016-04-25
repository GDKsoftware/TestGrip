unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uProjectParser, System.StrUtils,
  Vcl.ExtCtrls;

type
  TClassRef = class
  protected
    FFilename: string;
    FParentClasses: string;
    FInterfaceClassname: string;
  public
    constructor Create(const AFilename, AInterfaceClassname, AParentClasses: string);

    property Filename: string read FFilename;
    property InterfaceClassname: string read FInterfaceClassname;
    property ParentClasses: string read FParentClasses;
  end;

  TfrmMain = class(TForm)
    edProject: TEdit;
    cmbInterfaces: TComboBox;
    edImplementationClassname: TEdit;
    mmExample: TMemo;
    tmrFocus: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnCopyToClipboard: TButton;
    btnBrowseForProjectFile: TButton;
    procedure edProjectChange(Sender: TObject);
    procedure cmbInterfacesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edImplementationClassnameChange(Sender: TObject);
    procedure tmrFocusTimer(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnBrowseForProjectFileClick(Sender: TObject);
  private
    { Private declarations }
    FProjectParser: TProjectParser;
    FTryHintedInterface: string;

    procedure ReloadImplementation;

    procedure ReloadInterfaces;

    procedure ReloadProject;

    procedure LoadInterfacesFromFile(const AFilename: string; const ARootPath: string);

    function IsInterfaceClass(const AClassName: string): boolean; inline;

    procedure AddInterfaceClassesFromList(const AUnitfile: string; const AClasslist: TStrings);

    procedure DeleteListObjects(const AList: TStrings);

    procedure LoadImplementation(const AClassref: TClassref);

    procedure GenerateImplementationFromMethodList(const ASelectedInterface: TClassRef; const AMethods: TList);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses uUnitParser, uPascalDefs, uInheritGen;

procedure TfrmMain.AddInterfaceClassesFromList(const AUnitfile: string; const AClasslist: TStrings);
var
  i, c: Integer;
begin
  c := AClasslist.Count - 1;
  for I := 0 to c do
  begin
    if IsInterfaceClass(AClassList.Names[I]) then
    begin
      cmbInterfaces.AddItem(
        AClassList.Names[I],
        TClassRef.Create(AUnitFile, AClassList.Names[I], AClassList.ValueFromIndex[I])
      );
    end;
  end;
end;

procedure TfrmMain.btnBrowseForProjectFileClick(Sender: TObject);
var
  OpenFileDialog: TOpenDialog;
begin
  OpenFileDialog := TOpenDialog.Create(nil);
  try
    OpenFileDialog.Filter := '*.dproj|*.dproj';

    if OpenFileDialog.Execute then
    begin
      edProject.Text := OpenFileDialog.FileName;
    end;
  finally
    OpenFileDialog.Free;
  end;
end;

procedure TfrmMain.btnCopyToClipboardClick(Sender: TObject);
begin
  mmExample.SelectAll;
  mmExample.CopyToClipboard;
end;

procedure TfrmMain.cmbInterfacesChange(Sender: TObject);
begin
  ReloadImplementation;
end;

procedure TfrmMain.DeleteListObjects(const AList: TStrings);
var
  i, c: Integer;
  obj: TObject;
begin
  c := AList.Count - 1;
  for I := 0 to c do
  begin
    obj := AList.Objects[I];
    AList.Objects[I] := nil;
    obj.Free;
  end;
end;

procedure TfrmMain.edImplementationClassnameChange(Sender: TObject);
begin
  ReloadImplementation;
end;

procedure TfrmMain.edProjectChange(Sender: TObject);
begin
  ReloadProject;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FTryHintedInterface := '';

  if ParamCount > 0 then
  begin
    edProject.Text := ParamStr(1);

    if ParamCount > 1 then
    begin
      FTryHintedInterface := ParamStr(2);
    end;
  end;

  ReloadProject;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  tmrFocus.Enabled := True;
end;

procedure TfrmMain.GenerateImplementationFromMethodList(const ASelectedInterface: TClassRef; const AMethods: TList);
var
  i, c: integer;
  Method: TMethodDefinition;
  Generator: TInheritGen;
  Filename: string;
begin
  Filename := ExtractFileName(ASelectedInterface.Filename);

  Generator := TInheritGen.Create(ASelectedInterface.Filename, 'TInterfacedObject, ' + cmbInterfaces.Text);
  try
    Generator.CustomClassname := edImplementationClassname.Text;
    Generator.Filename := Copy(edImplementationClassname.Text, 2) + '.pas';

    // unitname
    Generator.OuterUses.Add(Copy(Filename, 1, Length(Filename) - 4));

    c := AMethods.Count - 1;
    for i := 0 to c do
    begin
      Method := TMethodDefinition(AMethods[i]);

      Generator.IncludeInterfaceMethod(Method);
    end;

    mmExample.Text :=
      Generator.GetTemplateCode;

  finally
    Generator.Free;
  end;
end;

function TfrmMain.IsInterfaceClass(const AClassName: string): boolean;
begin
  Result := StartsText('I', AClassName);
end;

procedure TfrmMain.LoadImplementation(const AClassref: TClassref);
var
  UnitParser: TUnitParser;
  FileStream: TFileStream;
  Methods: TList;
begin
  UnitParser := TUnitParser.Create;
  try
    Methods := TList.Create;
    try
      FileStream := TFileStream.Create(AClassref.Filename, fmOpenRead);
      try
        UnitParser.ParseUnit(FileStream, True);
        UnitParser.ListInterfaceMethods(AClassref.InterfaceClassname, Methods);
      finally
        FileStream.Free;
      end;

      GenerateImplementationFromMethodList(AClassref, Methods);
    finally
      Methods.Free;
    end;
  finally
    UnitParser.Free;
  end;
end;

procedure TfrmMain.LoadInterfacesFromFile(const AFilename: string; const ARootPath: string);
var
  UnitParser: TUnitParser;
  FileStream: TFileStream;
begin
  try
    UnitParser := TUnitParser.Create;
    try
      if ContainsText(AFileName, ':') then
      begin
        // absolute path
        FileStream := TFileStream.Create(AFilename, fmOpenRead);
      end
      else
      begin
        // relative from project root
        FileStream := TFileStream.Create(ARootPath + AFilename, fmOpenRead);
      end;

      try
        UnitParser.ParseUnit(FileStream, True);

        //cmbInterfaces.Items.AddStrings(UnitParser.InterfaceClassList);
        AddInterfaceClassesFromList(ARootPath + AFilename, UnitParser.InterfaceClassList);
      finally
        FileStream.Free;
      end;
    finally
      UnitParser.Free;
    end;
  except
    on E: EFileStreamError do
    begin
      // ignore file read errors, just skip file
      //  (aparantly XE8 requests exclusive locks for files?)
    end;
  end;
end;

procedure TfrmMain.ReloadImplementation;
var
  SelectedInterface: TClassRef;
begin
  mmExample.Text := '';

  if cmbInterfaces.ItemIndex <> -1 then
  begin
    SelectedInterface := TClassRef(cmbInterfaces.Items.Objects[cmbInterfaces.ItemIndex]);
    if Assigned(SelectedInterface) then
    begin
      LoadImplementation(SelectedInterface);
    end;
  end;
end;

procedure TfrmMain.ReloadInterfaces;
var
  i, c: Integer;
  Filename: string;
  RootPath: string;
begin
  DeleteListObjects(cmbInterfaces.Items);
  cmbInterfaces.Clear;

  RootPath := ExtractFilePath(edProject.Text);

  c := FProjectParser.Files.Count - 1;
  for i := 0 to c do
  begin
    Filename := FProjectParser.Files[i];

    LoadInterfacesFromFile(Filename, RootPath);
  end;
end;

procedure TfrmMain.ReloadProject;
begin
  if FileExists(edProject.Text) then
  begin
    if not Assigned(FProjectParser) then
    begin
      FProjectParser := TProjectParser.Create;
    end;

    FProjectParser.ParseDProj(edProject.Text, False);

    ReloadInterfaces;
  end;
end;

procedure TfrmMain.tmrFocusTimer(Sender: TObject);
begin
  tmrFocus.Enabled := False;

  try
    cmbInterfaces.SetFocus;
  except
  end;

  if FTryHintedInterface <> '' then
  begin
    cmbInterfaces.ItemIndex := cmbInterfaces.Items.IndexOf(FTryHintedInterface);
    if cmbInterfaces.ItemIndex <> -1 then
    begin
      cmbInterfacesChange(cmbInterfaces);
    end;

    FTryHintedInterface := '';
  end;
end;

{ TClassRef }

constructor TClassRef.Create(const AFilename, AInterfaceClassname, AParentClasses: string);
begin
  FFilename := AFilename;
  FInterfaceClassname := AInterfaceClassname;
  FParentClasses := AParentClasses;
end;

end.
