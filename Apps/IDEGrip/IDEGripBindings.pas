unit IDEGripBindings;

interface

uses
  Classes, ToolsAPI;

type
  TIDEGripBinding = class(TNotifierObject, IOTAKeyboardBinding)
  private
    procedure AddBreakpoint(const Context: IOTAKeyContext; KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
  public
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
  end;

procedure RegisterIDEGripBindings;
procedure UnregisterIDEGripBindings;

procedure register;

var
  IDEGripKeyBindingIndex: Integer;

implementation

uses
  Menus, IDEGripActions;

{ TIDEGripBinding }

procedure TIDEGripBinding.AddBreakpoint(const Context: IOTAKeyContext; KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  if KeyCode = TextToShortCut('Ctrl+Alt+G') then
  begin
    TIDEGripActions.CopyInterface;
    BindingResult := krHandled;
  end;
end;

procedure TIDEGripBinding.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([TextToShortcut('Ctrl+Alt+G')], AddBreakpoint, Nil);
end;

function TIDEGripBinding.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TIDEGripBinding.GetDisplayName: string;
begin
  result := 'IDEGrip key bindings';
end;

function TIDEGripBinding.GetName: string;
begin
  result := 'IDEGripKeyBindings';
end;


procedure RegisterIDEGripBindings;
var
  KeyboardServices: IOTAKeyboardServices;
begin
  KeyboardServices := (BorlandIDEServices As IOTAKeyboardServices);
  IDEGripKeyBindingIndex := KeyboardServices.AddKeyboardBinding(TIDEGripBinding.Create);
end;

procedure UnregisterIDEGripBindings;
var
  KeyboardServices: IOTAKeyboardServices;
begin
  If IDEGripKeyBindingIndex > 0 Then
  begin
    KeyboardServices := (BorlandIDEServices As IOTAKeyboardServices);
    KeyboardServices.RemoveKeyboardBinding(IDEGripKeyBindingIndex);
  end;
end;

procedure register;
begin
  RegisterIDEGripBindings;
end;

initialization
  IDEGripKeyBindingIndex := 0;
finalization
  UnregisterIDEGripBindings;
end.
