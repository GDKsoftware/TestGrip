unit Deskutil;

interface

uses
  Forms;

type
  PRegisterFieldAddress = procedure(const AFieldName: string; const APointer: Pointer);
  PUnregisterFieldAddress = procedure(const APointer: Pointer);

procedure FocusWindow(const AForm: TCustomForm);
procedure RegisterDesktopFormClass(const AClass: TClass; const AClassName: string; const AObjectName: string);


var
  RegisterFieldAddress: PRegisterFieldAddress;
  UnregisterFieldAddress: PUnregisterFieldAddress;

implementation

procedure FocusWindow(const AForm: TCustomForm);
begin

end;

procedure RegisterDesktopFormClass(const AClass: TClass; const AClassName: string; const AObjectName: string);
begin

end;

end.
