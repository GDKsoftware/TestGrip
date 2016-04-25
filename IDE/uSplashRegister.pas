unit uSplashRegister;

interface

implementation

{$R SPLASHREGISTER.RES}

uses
  Classes, Windows, Graphics, ToolsAPI,DesignIntf;

ResourceString
  strSplashScreenName = 'GDC Testgrip © 2011';

procedure AddSplashText;         
var
  bmp: HBITMAP;
begin
  bmp := LoadBitmap(FindResourceHInstance(HInstance), 'LOGOGEUZE');
  {$ifndef VER150}
  if assigned(SplashScreenServices) then
  begin
    SplashScreenServices.AddPluginBitmap(strSplashScreenName,bmp,false,'Registered','');
  end;
  {$endif}
end;


initialization
   AddSplashText;
end.

