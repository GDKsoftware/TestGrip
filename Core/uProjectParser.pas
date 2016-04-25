unit uProjectParser;

interface

uses
  Classes;

type
  TProjectParser = class
  protected
    FFilename: string;
    FDeleteUnwantedDefines: Boolean;

    FDefines: TStrings;
    FFiles: TStrings;
    FNamespaces: TStrings;
    FExtraAlias: TStrings;

    FEntryFile: string;

    FSearchPath: TStrings;
    FBRCC_IncludePath: TStrings;
    FBRCC_OutputDir: TStrings;

    FOutputDir: string;

    procedure RemoveDuplicateEntriesInList( const lst: TStrings );

    procedure RemoveDuplicateDefines;
    procedure RemoveDuplicateNamespaces;
  public
    property Defines: TStrings
      read FDefines;
    property Files: TStrings
      read FFiles;
    property EntryFile: string
      read FEntryFile;
    property Namespaces: TStrings
      read FNamespaces;
    property SearchPath: TStrings
      read FSearchPath;
    property BRCC_IncludePath: TStrings
      read FBRCC_IncludePath;
    property BRCC_OutputDir: TStrings
      read FBRCC_OutputDir;
    property OutputDir: string
      read FOutputDir;
    property ExtraAlias: TStrings
      read FExtraAlias;

    constructor Create;
    destructor Destroy; override;

    procedure ParseDProj(const sFilename: string; bDeleteUnwantedDefines: boolean);
    procedure Refresh;

    /// <summary>Tries to find the full path for the given unitname among the files that have been added to the projectfile</summary>
    /// <returns>empty string if not found</returns>
    function GetUnitFilename(const sUnit: string): string;
  end;

implementation

uses
  XmlIntf, XmlDoc, ActiveX, StrUtils, Variants, SysUtils, uD7Functions;

{ TProjectParser }


procedure TProjectParser.RemoveDuplicateEntriesInList(const lst: TStrings);
var
  i, c: integer;
  p: integer;
begin
  c := lst.Count;
  i := 0;
  while i < c do
  begin
    p := lst.IndexOf( lst[i] );
    if (p <> -1) and (p < i) then
    begin
      lst.Delete(i);
      Dec(c);
    end
    else
    begin
      Inc(i);
    end;
  end;

  i := 0;
  c := FDefines.Count;
  while i < c do
  begin
    if FDefines[i] = '' then
    begin
      FDefines.Delete(i);
      Dec(c);
    end
    else
    begin
      Inc(i);
    end;
  end;
end;

procedure TProjectParser.Refresh;
begin
  ParseDProj(FFilename, False);
end;

procedure TProjectParser.RemoveDuplicateDefines;
begin
  RemoveDuplicateEntriesInList(FDefines);
end;

procedure TProjectParser.RemoveDuplicateNamespaces;
begin
  RemoveDuplicateEntriesInList(FNamespaces);
end;

constructor TProjectParser.Create;
begin
  FFilename := '';
  FDeleteUnwantedDefines := False;

  FFiles := TStringList.Create;

  FDefines := TStringList.Create;
  FDefines.Delimiter := ';';

  FNamespaces := TStringList.Create;
  FNamespaces.Delimiter := ';';

  FSearchPath := TStringList.Create;
  FSearchPath.Delimiter := ';';
  FSearchPath.StrictDelimiter := True;

  FBRCC_IncludePath := TStringList.Create;
  FBRCC_IncludePath.Delimiter := ';';
  FBRCC_IncludePath.StrictDelimiter := True;

  FBRCC_OutputDir := TStringList.Create;
  FBRCC_OutputDir.Delimiter := ';';

  FExtraAlias := TStringList.Create;
  FExtraAlias.Delimiter := ';';

  FOutputDir := '';
end;

destructor TProjectParser.Destroy;
begin
  FExtraAlias.Free;
  FBRCC_OutputDir.Free;
  FBRCC_IncludePath.Free;
  FSearchPath.Free;
  FNamespaces.Free;
  FDefines.Free;
  FFiles.Free;

  inherited;
end;

function TProjectParser.GetUnitFilename(const sUnit: string): string;
var
  i,c: Integer;
begin
  Result := '';

  c := FFiles.Count - 1;
  for i := 0 to c do
  begin
    if EndsText(sUnit + '.pas', FFiles[i]) then
    begin
      Result := IncludeTrailingPathDelimiter(ExtractFilePath(FFilename)) + FFiles[i];
      Exit;
    end;
  end;
end;

procedure TProjectParser.ParseDProj(const sFilename: string; bDeleteUnwantedDefines: boolean);
var
  doc: IXMLDocument;
  rootnode: IXmlNode;
  propnode: IXmlNode;
  i, c: integer;
  j, d: integer;
  defnode: IXmlNode;
  subnode: IXmlNode;
  sPropertyGroupCondition: string;
begin
  FFilename := sFilename;
  FDeleteUnwantedDefines := bDeleteUnwantedDefines;

  FFiles.Clear;

  CoInitialize(nil);

  doc := TXMLDocument.Create(nil);

  doc.LoadFromFile( sFilename );

  rootnode := doc.Node.ChildNodes.FindNode('Project');

  c := rootnode.ChildNodes.Count - 1;
  for i := 0 to c do
  begin
    propnode := rootnode.ChildNodes.Get(i);
    if SameText( propnode.NodeName, 'PropertyGroup' ) then
    begin
      defnode := propnode.ChildNodes.FindNode('DCC_Define');
      if Assigned(defnode) then
      begin
        if (FDefines.DelimitedText <> '') and not EndsStr(';',FDefines.DelimitedText) then
        begin
          FDefines.DelimitedText := Trim(FDefines.DelimitedText + ';' + ReplaceText( defnode.NodeValue, '$(DCC_Define)', '' ));
        end
        else
        begin
          FDefines.DelimitedText := Trim(FDefines.DelimitedText + ReplaceText( defnode.NodeValue, '$(DCC_Define)', '' ));
        end;
      end;

      defnode := propnode.ChildNodes.FindNode('MainSource');
      if Assigned(defnode) then
      begin
        FEntryFile := IncludeTrailingPathDelimiter(ExtractFilePath(sFilename)) + defnode.NodeValue;
      end;

      if not VarIsNull(propnode.Attributes['Condition']) then
      begin
        sPropertyGroupCondition := propnode.Attributes['Condition'];
        if SameText(sPropertyGroupCondition, '''$(Base)''!=''''') then
        begin
          // ...
        end
        else if SameText(sPropertyGroupCondition, '''$(Base_Win32)''!=''''') then
        begin
          // ...
        end
        else if SameText(sPropertyGroupCondition, '''$(Base_Win64)''!=''''') then
        begin
          // ...
        end;
      end;

      defnode := propnode.ChildNodes.FindNode('DCC_Namespace');
      if Assigned(defnode) then
      begin
        if (FNamespaces.DelimitedText <> '') and not EndsStr(';',FNamespaces.DelimitedText) then
        begin
          FNamespaces.DelimitedText := Trim(FNamespaces.DelimitedText + ';' + ReplaceText( defnode.NodeValue, '$(DCC_Namespace)', '' ));
        end
        else
        begin
          FNamespaces.DelimitedText := Trim(FNamespaces.DelimitedText + ReplaceText( defnode.NodeValue, '$(DCC_Namespace)', '' ));
        end;

        if EndsStr(';',FNamespaces.DelimitedText) then
        begin
          FNamespaces.DelimitedText := Copy(FNamespaces.DelimitedText,1, Length(FNamespaces.DelimitedText) - 1);
        end;
      end;

      defnode := propnode.ChildNodes.FindNode('DCC_UnitSearchPath');
      if Assigned(defnode) then
      begin
        if (FSearchPath.DelimitedText <> '') and not EndsStr(';',FSearchPath.DelimitedText) then
        begin
          FSearchPath.DelimitedText := Trim(FSearchPath.DelimitedText + ';' + ReplaceText( defnode.NodeValue, '$(DCC_UnitSearchPath)', '' ));
        end
        else
        begin
          FSearchPath.DelimitedText := Trim(FSearchPath.DelimitedText + ReplaceText( defnode.NodeValue, '$(DCC_UnitSearchPath)', '' ));
        end;

        if EndsStr(';',FSearchPath.DelimitedText) then
        begin
          FSearchPath.DelimitedText := Copy(FSearchPath.DelimitedText,1, Length(FSearchPath.DelimitedText) - 1);
        end;
      end;

      defnode := propnode.ChildNodes.FindNode('BRCC_IncludePath');
      if Assigned(defnode) then
      begin
        if (FBRCC_IncludePath.DelimitedText <> '') and not EndsStr(';',FBRCC_IncludePath.DelimitedText) then
        begin
          FBRCC_IncludePath.DelimitedText := Trim(FBRCC_IncludePath.DelimitedText + ';' + ReplaceText( defnode.NodeValue, '$(BRCC_IncludePath)', '' ));
        end
        else
        begin
          FBRCC_IncludePath.DelimitedText := Trim(FBRCC_IncludePath.DelimitedText + ReplaceText( defnode.NodeValue, '$(BRCC_IncludePath)', '' ));
        end;

        if EndsStr(';',FBRCC_IncludePath.DelimitedText) then
        begin
          FBRCC_IncludePath.DelimitedText := Copy(FBRCC_IncludePath.DelimitedText,1, Length(FBRCC_IncludePath.DelimitedText) - 1);
        end;
      end;


      defnode := propnode.ChildNodes.FindNode('BRCC_OutputDir');
      if Assigned(defnode) then
      begin
        if (FBRCC_OutputDir.DelimitedText <> '') and not EndsStr(';',FBRCC_OutputDir.DelimitedText) then
        begin
          FBRCC_OutputDir.DelimitedText := Trim(FBRCC_OutputDir.DelimitedText + ';' + ReplaceText( defnode.NodeValue, '$(BRCC_OutputDir)', '' ));
        end
        else
        begin
          FBRCC_OutputDir.DelimitedText := Trim(FBRCC_OutputDir.DelimitedText + ReplaceText( defnode.NodeValue, '$(BRCC_OutputDir)', '' ));
        end;

        if EndsStr(';',FBRCC_OutputDir.DelimitedText) then
        begin
          FBRCC_OutputDir.DelimitedText := Copy(FBRCC_OutputDir.DelimitedText,1, Length(FBRCC_OutputDir.DelimitedText) - 1);
        end;
      end;

      defnode := propnode.ChildNodes.FindNode('DCC_ExeOutput');
      if Assigned(defnode) then
      begin
        FOutputDir := defnode.NodeValue;
        if StartsStr('.', FOutputDir) then
        begin
          FOutputDir := IncludeTrailingPathDelimiter(ExtractFilePath(sFilename)) + FOutputDir;
        end;
      end;
    end
    else if SameText( propnode.NodeName, 'ItemGroup' ) then
    begin
      d := propnode.ChildNodes.Count - 1;
      for j := 0 to d do
      begin
        subnode := propnode.ChildNodes[j];
        if SameText( subnode.NodeName, 'DCCReference' ) then
        begin
          FFiles.Add( subnode.Attributes['Include'] );
        end;
      end;
    end;

  end;

  RemoveDuplicateDefines;
  RemoveDuplicateNamespaces;

  if bDeleteUnwantedDefines then
  begin
    i := FDefines.IndexOf('RELEASE');
    if i <> -1 then
    begin
      FDefines.Delete( i );
    end;

    i := FDefines.IndexOf('DEBUG');
    if i <> -1 then
    begin
      FDefines.Delete( i );
    end;
  end;
end;

end.
