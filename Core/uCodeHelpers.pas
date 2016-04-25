unit uCodeHelpers;

interface

uses
  uDefinitionSearch;

type
  TCodeHelperA = class
  public
    class function GetInterfaceMethodDefinitions(const oDefinitionSearch: TDefinitionSearch; const sCurrentUnitname: string; const sSearchFor: string): string;
  end;

implementation

uses
  uUnitParser, Classes, SysUtils, uPascalDefs;

{ TCodeHelperA }

class function TCodeHelperA.GetInterfaceMethodDefinitions(
  const oDefinitionSearch: TDefinitionSearch; const sCurrentUnitname,
  sSearchFor: string): string;
var
  lstUses: TStringList;
  oParser: TUnitParser;
  oStream: TFileStream;
  lstMethods: TList;
  i, c: integer;
  mdef: TMethodDefinition;
begin
  // parse current file's outer uses into lstUses
  lstUses := TStringList.Create;
  try
    oParser := TUnitParser.Create;
    try
      oStream := TFileStream.Create(sCurrentUnitname, fmOpenRead);
      try
        oParser.ParseUnit(oStream, False, False, False, False, False);

        lstUses.AddStrings(oParser.OuterUsesList);
      finally
        oStream.Free;
      end;
    finally
      oParser.Free;
    end;

    // find out the methods to implement
    lstMethods := TList.Create;
    try
      oParser := oDefinitionSearch.Search(lstUses, sSearchFor);
      if Assigned(oParser) then
      begin
        oParser.ListInterfaceMethods(sSearchFor, lstMethods);
      end;

      // copy those methods
      Result := '';
      c := lstMethods.Count - 1;
      for i := 0 to c do
      begin
        mdef := lstMethods[i];
        if Assigned(mdef) then
        begin
          Result := Result + mdef.RawMethod + #13#10;
        end;
      end;
    finally
      lstMethods.Free;
    end;
  finally
    lstUses.Free;
  end;
end;

end.
