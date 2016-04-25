unit uTestgripInstall;

interface

const
  C_TESTGRIP_REGISTRYKEY = 'Software\GDC Software\Testgrip';

  function TestgripInstallPath: string;
//  function HasAskedForEmail: boolean;
//  procedure SetEmailAsked(b: boolean = True);

  procedure SetUse3rdPartyExceptionSetting(b: boolean);
  function GetUse3rdPartyExceptionSetting: boolean;

implementation

uses
  Registry, uCommonFunctions, Windows, SysUtils;

function TestgripInstallPath: string;
var
  reg: TRegistry;
  s: string;
begin
  Result := TCommonAppFunctions.GetAppPath;

  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    if reg.OpenKeyReadOnly(C_TESTGRIP_REGISTRYKEY) then
    begin
      s := reg.ReadString('InstallPath');
      if s <> '' then
      begin
        Result := IncludeTrailingPathDelimiter(s);
      end;

      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;


procedure SetUse3rdPartyExceptionSetting(b: boolean);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create(KEY_WRITE);
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.OpenKey(C_TESTGRIP_REGISTRYKEY, true) then
    begin
      try
        reg.WriteBool('Use3rdPartyExcept', b);
      except
        // if this fails (should not be possible, really) .. how can we know not to bother the user anymore?
      end;

      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

function GetUse3rdPartyExceptionSetting: boolean;
var
  reg: TRegistry;
begin
  Result := False;

  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.OpenKey(C_TESTGRIP_REGISTRYKEY, true) then
    begin
      try
        Result := reg.ReadBool('Use3rdPartyExcept');
      except
        Result := False;

        SetUse3rdPartyExceptionSetting(false);
      end;

      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;


end.
