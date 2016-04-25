unit uIDEGarbageCollector;

interface

uses
  Classes;

type
  TIDEGarbageCollector = class
  protected
    FFormsToGarbageCollect: TList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure GCCycle( iCycles: integer; bForceClose: boolean );
    procedure Add(const AObject: TObject);
    procedure Remove(const AObject: TObject);

  end;

var
  IDEGarbageCollector: TIDEGarbageCollector;

implementation

uses
  Forms, SysUtils;

{ TIDEGarbageCollector }

procedure TIDEGarbageCollector.Add(const AObject: TObject);
begin
  if AObject is TForm then
    FFormsToGarbageCollect.Add(AObject);
end;

constructor TIDEGarbageCollector.Create;
begin
  inherited Create;

  FFormsToGarbageCollect := TList.Create;

end;

destructor TIDEGarbageCollector.Destroy;
begin
  FreeAndNil(FFormsToGarbageCollect);

  inherited;
end;

procedure TIDEGarbageCollector.GCCycle(iCycles: integer; bForceClose: boolean);
var
  i, c: integer;
  frm: TForm;
  iCyclesLeft: integer;
begin
  iCyclesLeft := iCycles;

  try

    c := FFormsToGarbageCollect.Count - 1;
    for i := 0 to c do
    begin
      frm := TForm(FFormsToGarbageCollect[i]);
      if Assigned(frm) then
      begin
        if not frm.Visible or bForceClose then
        begin
          FFormsToGarbageCollect[i] := nil;
          frm.Free;

          if iCyclesLeft <> -1 then
          begin
            Dec(iCyclesLeft);

            if iCyclesLeft = 0 then
            begin
              break;
            end;
          end;
        end;
      end;
    end;
  except
    // ignore
  end;

  FFormsToGarbageCollect.Pack;
end;


procedure TIDEGarbageCollector.Remove(const AObject: TObject);
var
  Idx: Integer;
begin
  if AObject is TForm then
  begin
    Idx := FFormsToGarbageCollect.IndexOf(AObject);
    if Idx <> -1 then
    begin
      FFormsToGarbageCollect[Idx] := nil;
    end;
  end;
end;

initialization
  IDEGarbageCollector := TIDEGarbageCollector.Create;

end.
