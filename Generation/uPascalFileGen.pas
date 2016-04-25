unit uPascalFileGen;

interface

uses
  Classes;

type
  TPascalFileGen = class(TInterfacedPersistent)
  protected
    FFilename: string;
    FUnitname: string;
    FPossibleClassname: string;

    FInnerUses: TStringList;
    FOuterUses: TStringList;

    FWriteUTF8BOM: boolean;

    function GetOuterUses: TStringList;
    procedure SetOuterUses(const Value: TStringList);
    function GetWriteUTF8BOM: boolean;
    procedure SetWriteUTF8BOM(const Value: boolean);

    procedure RemoveDuplicateUses(const AUsesList: TStrings);
    procedure RemoveDuplicatesComparedToOtherList(const AUses: TStrings; const ACompareAgainst: TStrings);
    procedure RemoveItemsByValue(const AValue: string; const AUses: TStrings);
    procedure RemoveNamespacedDuplicatesFromInnerUses;
    procedure RemoveDuplicates;
    procedure RemoveNonIfDefines(const AList: TStrings);

    function GenerateTemplate: string; virtual;
  public
    property WriteUTF8BOM: boolean
      read GetWriteUTF8BOM write SetWriteUTF8BOM;

    property OuterUses: TStringList
      read GetOuterUses write SetOuterUses;
    property InnerUses: TStringList
      read FInnerUses;

    property PossibleClassname: string
      read FPossibleClassname write FPossibleClassname;
    property Filename: string
      read FFilename write FFilename;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure SaveAs(const sFilename: string); virtual;

  end;

implementation

uses
  SysUtils, StrUtils, uD7Functions;

const
  c_pascaltemplate = 'unit <UnitName>;' + #13#10 + #13#10 + 'interface' + #13#10
    + #13#10 + '<OuterUses>' + #13#10 + 'type' + #13#10 + '<TypeDefs>' + #13#10
    + 'implementation' + #13#10 + #13#10 + '<InnerUses>' + #13#10 +
    '<Implementation>' + #13#10 + 'end.' + #13#10;

  { TPascalFileGen }

constructor TPascalFileGen.Create;
begin
  FWriteUTF8BOM := false;

  FFilename := 'pasfile.pas';

  FInnerUses := TStringList.Create;
  FOuterUses := TStringList.Create;

  FInnerUses.Duplicates := dupIgnore;
  FOuterUses.Duplicates := dupIgnore;
end;

destructor TPascalFileGen.Destroy;
begin
  FOuterUses.Free;
  FInnerUses.Free;

  inherited;
end;

function TPascalFileGen.GenerateTemplate: string;
var
  sfname, sfext: string;
  sOuterUses, sInnerUses: string;
  i, c: integer;
begin
  Result := c_pascaltemplate;

  sfname := ExtractFileName(ReplaceStr(FFilename, '/', '\'));
  sfext := ExtractFileExt(sfname);

  FUnitname := Copy(sfname, 1, Length(sfname) - Length(sfext));
  FPossibleClassname := 'T' + FUnitname;

  Result := ReplaceText(Result, '<UnitName>', FUnitname);

  RemoveDuplicates;
  RemoveNonIfDefines(FOuterUses);
  RemoveNonIfDefines(FInnerUses);

  sOuterUses := '';
  c := FOuterUses.Count - 1;
  for i := 0 to c do
  begin
    if i <> 0 then
    begin
      sOuterUses := sOuterUses + ',' + #13#10;
    end;
    sOuterUses := sOuterUses + '  ' + FOuterUses[i];
  end;

  sInnerUses := '';
  c := FInnerUses.Count - 1;
  for i := 0 to c do
  begin
    if i <> 0 then
    begin
      sInnerUses := sInnerUses + ',' + #13#10;
    end;
    sInnerUses := sInnerUses + '  ' + FInnerUses[i];
  end;

  if sOuterUses <> '' then
  begin
    Result := ReplaceText(Result, '<OuterUses>', 'uses' + #13#10 + sOuterUses +
      ';' + #13#10);
  end
  else
  begin
    Result := ReplaceText(Result, '<OuterUses>', '');
  end;

  if sInnerUses <> '' then
  begin
    Result := ReplaceText(Result, '<InnerUses>', 'uses' + #13#10 + sInnerUses +
      ';' + #13#10);
  end
  else
  begin
    Result := ReplaceText(Result, '<InnerUses>', '');
  end;
end;

function TPascalFileGen.GetOuterUses: TStringList;
begin
  Result := FOuterUses;
end;

function TPascalFileGen.GetWriteUTF8BOM: boolean;
begin
  Result := FWriteUTF8BOM;
end;

procedure TPascalFileGen.RemoveDuplicates;
begin
  RemoveDuplicateUses(FInnerUses);
  RemoveNamespacedDuplicatesFromInnerUses;

  RemoveDuplicateUses(FOuterUses);
  RemoveDuplicatesComparedToOtherList(FInnerUses, FOuterUses);
end;

procedure TPascalFileGen.RemoveNonIfDefines(const AList: TStrings);
var
  i, c: integer;
begin
  c := AList.Count - 1;
  i := 0;
  while i < c do
  begin
    if StartsText('{$', AList[i]) and not StartsText('{$if', AList[i]) then
    begin
      AList.Delete(i);
      Dec(c);
    end
    else
    begin
      Inc(i);
    end;
  end;
end;

procedure TPascalFileGen.SaveAs(const sFilename: string);
var
  s: string;
  f: TextFile;
begin
  FFilename := sFilename;

  s := GenerateTemplate;

  AssignFile(f, sFilename);
  try
    Rewrite(f);

    if FWriteUTF8BOM then
    begin
      Write(f, '' + #$EF + #$BB + #$BF);
    end;

    Write(f, s);
  finally
    Close(f);
  end;
end;

procedure TPascalFileGen.SetOuterUses(const Value: TStringList);
begin
  FOuterUses := Value;
end;

procedure TPascalFileGen.SetWriteUTF8BOM(const Value: boolean);
begin
  FWriteUTF8BOM := Value;
end;

procedure TPascalFileGen.RemoveNamespacedDuplicatesFromInnerUses;
var
  c: integer;
  i: integer;
  s: string;
  p: integer;
begin
  c := FInnerUses.Count;
  i := 0;
  while i < c do
  begin
    s := FInnerUses[i];
    if StartsText('vcl.', s) then
    begin
      s := Copy(s, 5);
    end
    else if StartsText('system.', s) then
    begin
      s := Copy(s, 8);
    end;
    p := FOuterUses.IndexOf(s);
    if p <> -1 then
    begin
      FInnerUses.Delete(i);
      c := FInnerUses.Count;
    end
    else
    begin
      Inc(i);
    end;
  end;
end;

procedure TPascalFileGen.RemoveItemsByValue(const AValue: string;
  const AUses: TStrings);
var
  ItemIdx: integer;
begin
  ItemIdx := AUses.IndexOf(AValue);
  while ItemIdx <> -1 do
  begin
    AUses.Delete(ItemIdx);
    ItemIdx := AUses.IndexOf(AValue);
  end;
end;

procedure TPascalFileGen.RemoveDuplicatesComparedToOtherList
  (const AUses: TStrings; const ACompareAgainst: TStrings);
var
  i, c: integer;
  Item: string;
begin
  c := ACompareAgainst.Count;
  i := 0;
  while i < c do
  begin
    Item := ACompareAgainst[i];

    RemoveItemsByValue(Item, AUses);

    // vcl/system namespaced duplicates
    if StartsText('vcl.', Item) then
    begin
      Item := Copy(Item, 5);
      RemoveItemsByValue(Item, AUses);
    end
    else if StartsText('system.', Item) then
    begin
      Item := Copy(Item, 8);
      RemoveItemsByValue(Item, AUses);
    end;

    Inc(i);
    c := ACompareAgainst.Count;
  end;
end;

procedure TPascalFileGen.RemoveDuplicateUses(const AUsesList: TStrings);
var
  i, j, c: integer;
  s: string;
  truncs: string;
begin
  c := AUsesList.Count;
  i := 0;
  while i < c do
  begin
    s := AUsesList[i];
    truncs := '';
    if StartsText('vcl.', s) then
    begin
      truncs := Copy(s, 5);
    end
    else if StartsText('system.', s) then
    begin
      truncs := Copy(s, 8);
    end;

    j := 0;
    while j < c do
    begin
      if i <> j then
      begin
        if SameText(AUsesList[j], s) or
          ((truncs <> '') and SameText(AUsesList[j], truncs)) then
        begin
          AUsesList.Delete(j);
          if i > j then
          begin
            Dec(i);
          end;
          Dec(j);
          Dec(c);
        end;
      end;
      Inc(j);
    end;
    Inc(i);
  end;
end;

end.
