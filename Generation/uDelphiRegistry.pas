unit uDelphiRegistry;

interface

uses
  Registry;

{
Delphi XE6 	27 	VER270
Delphi XE5 	26 	VER260
Delphi XE4 	25 	VER250
Delphi XE3 	24 	VER240
Delphi XE2 	23 	VER230
Delphi XE 	22 	VER220
Delphi 2010 	21 	VER210
Delphi 2009 	20 	VER200
Delphi 2007 	18.5 	VER185
Delphi 2006 	18 	VER180
Delphi 2005 	17 	VER170
Delphi 8 	16 	VER160
Delphi 7 	15 	VER150
Delphi 6 	14 	VER140
Delphi 5 	13 	VER130
Delphi 4 	12 	VER120
Delphi 3 	10 	VER100
Delphi 2 	9 	VER90
Delphi 1 	8 	VER80
}

const
  cntInstalledName = 'TestGrip';

  REGKEY_D7    = 'Software\Borland\Delphi\7.0';
  REGKEY_D2005 = 'Software\Borland\BDS\3.0';
  REGKEY_D2007 = 'Software\Borland\BDS\5.0';
  REGKEY_D2009 = 'Software\CodeGear\BDS\6.0';
  REGKEY_D2010 = 'Software\CodeGear\BDS\7.0';
  REGKEY_DXE   = 'Software\Embarcadero\BDS\8.0';
  REGKEY_DXE2  = 'Software\Embarcadero\BDS\9.0';
  REGKEY_DXE3  = 'Software\Embarcadero\BDS\10.0';
  REGKEY_DXE4  = 'Software\Embarcadero\BDS\11.0';
  REGKEY_DXE5  = 'Software\Embarcadero\BDS\12.0';
  REGKEY_DXE6  = 'Software\Embarcadero\BDS\14.0';
  REGKEY_DXE7  = 'Software\Embarcadero\BDS\15.0';
  REGKEY_DXE8  = 'Software\Embarcadero\BDS\16.0';
  REGKEY_DXE10 = 'Software\Embarcadero\BDS\17.0';
  REGKEY_DXE11 = 'Software\Embarcadero\BDS\18.0';

type

  TDelphiRegistry = class
  private
    class function GetRegistryKeyNameIfItExists(const ARegistry: TRegistry; const AKeyName: string): string;
  public
    class function GetInstallFolder: string;
    class function GetActiveDelphiRegistryKey: string;
    class function GetDelphiPath: string;
	  class function GetDelphiVersion: string;
  end;

implementation

uses
  Windows, classes;

{ TDelphiRegistry }

class function TDelphiRegistry.GetDelphiPath: string;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create( HKEY_CURRENT_USER );
  try
    if reg.KeyExists(GetActiveDelphiRegistryKey) then
    begin
      if reg.OpenKeyReadOnly(GetActiveDelphiRegistryKey) then
      begin
        result := reg.ReadString('RootDir');

        reg.CloseKey;
      end;
    end;
  finally
    reg.Free;
  end;
end;

class function TDelphiRegistry.GetInstallFolder: string;
var
  reg: TRegistry;
  lstNames: TStringlist;
  i: Integer;
  sValue: string;
begin
  result := '';
  reg := TRegistry.Create( HKEY_CURRENT_USER );

  try
    if reg.KeyExists(GetActiveDelphiRegistryKey + '\Known Packages') then
      if reg.OpenKeyReadOnly(GetActiveDelphiRegistryKey + '\Known Packages') then
      begin
        lstNames := TStringlist.create;
        try
          reg.GetValueNames(lstNames);

          for i := 0 to lstNames.Count -1 do
          begin
            sValue := Reg.ReadString(lstNames[i]);

            if sValue = cntInstalledName then
              result := lstNames[i];
          end;
        finally
          lstNames.Free;
        end;
      end;
  finally
    reg.Free;
  end;

end;

class function TDelphiRegistry.GetDelphiVersion: string;
begin
  Result := 'Unknown';

  {$ifdef VER310}
  Result := 'DXE11';
  {$endif}
  {$ifdef VER300}
  Result := 'DXE10';
  {$endif}
  {$ifdef VER290}
  Result := 'DXE8';
  {$endif}
  {$ifdef VER280}
  Result := 'DXE7';
  {$endif}
  {$ifdef VER270}
  Result := 'DXE6';
  {$endif}
  {$ifdef VER260}
  Result := 'DXE5';
  {$endif}
  {$ifdef VER250}
  Result := 'DXE4';
  {$endif}
  {$ifdef VER240}
  Result := 'DXE3';
  {$endif}
  {$ifdef VER230}
  Result := 'DXE2';
  {$endif}
  {$ifdef VER220}
  Result := 'DXE';
  {$endif}
  {$ifdef VER210}
  Result := 'D2010';
  {$endif}
  {$ifdef VER200}
  Result := 'D2009';
  {$endif}
  {$ifdef VER180}
  Result := 'D2007';
  {$endif}
  {$ifdef VER150}
  Result := 'D7';
  {$endif}
end;

class function TDelphiRegistry.GetRegistryKeyNameIfItExists(const ARegistry: TRegistry; const AKeyName: string): string;
begin
  Result := '';

  if ARegistry.KeyExists(AKeyName) then
  begin
    if ARegistry.OpenKeyReadOnly(AKeyName) then
    begin
      result := AKeyName;

      ARegistry.CloseKey;
    end;
  end;
end;

class function TDelphiRegistry.GetActiveDelphiRegistryKey: string;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create( HKEY_CURRENT_USER );
  try
    // Delphi 10.1 Berlin
    {$ifdef VER310}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_DXE11);
    {$endif}

    // Delphi 10 seattle
    {$ifdef VER300}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_DXE10);
    {$endif}

    // Delphi XE8
    {$ifdef VER290}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_DXE8);
    {$endif}

    // Delphi XE7
    {$ifdef VER280}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_DXE7);
    {$endif}

    // Delphi XE6
    {$ifdef VER270}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_DXE6);
    {$endif}

    // Delphi XE5
    {$ifdef VER260}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_DXE5);
    {$endif}

    // Delphi XE4
    {$ifdef VER250}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_DXE4);
    {$endif}

    // Delphi XE3
    {$ifdef VER240}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_DXE3);
    {$endif}

    // Delphi XE2
    {$ifdef VER230}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_DXE2);
    {$endif}

    // Delphi XE
    {$ifdef VER220}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_DXE);
    {$endif}

    // D2010
    {$ifdef VER210}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_D2010);
    {$endif}

    // Delphi 2009
    {$ifdef VER200}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_D2009);
    {$endif}

    // Delphi 2007
    {$ifdef VER180}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_D2007);
    {$endif}

    // Delphi 2005
    {$ifdef VER170}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_D2005);
    {$endif}

  	// Delphi 7
    {$ifdef VER150}
    Result := GetRegistryKeyNameIfItExists(reg, REGKEY_D7);
    {$endif}
  finally
    reg.Free;
  end;
end;

end.
