unit DockForm;

interface

uses
  Forms, Classes;

type
  TDockableForm = class(TForm)
  private
    FDeskSection: string;
    FAutoSave: Boolean;
    FSaveStateNecessary: Boolean;
    procedure SetAutoSave(const Value: Boolean);
    procedure SetDeskSection(const Value: string);
    procedure SetSaveStateNecessary(const Value: Boolean);
  protected

  public
    constructor Create(AOwner: TComponent); override;

    procedure ForceShow;

    property DeskSection: string read FDeskSection write SetDeskSection;
    property AutoSave: Boolean read FAutoSave write SetAutoSave;
    property SaveStateNecessary: Boolean read FSaveStateNecessary write SetSaveStateNecessary;
  end;

implementation

uses
  Controls;

{ TDockableForm }

constructor TDockableForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BorderStyle := bsNone;
  Align := TAlign.alClient;
end;

procedure TDockableForm.ForceShow;
begin
  Show;
end;

procedure TDockableForm.SetAutoSave(const Value: Boolean);
begin
  FAutoSave := Value;
end;

procedure TDockableForm.SetDeskSection(const Value: string);
begin
  FDeskSection := Value;
end;

procedure TDockableForm.SetSaveStateNecessary(const Value: Boolean);
begin
  FSaveStateNecessary := Value;
end;

end.
