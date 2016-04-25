unit uBuildOutputParser;

interface

uses
  Classes;

type
  TBuildError = class
  protected
    FBuildUnitname: string;
    FLine: integer;
    FErrorCode: string;
    FErrorText: string;

    FFilePath: string;
  public
    property ErrorText: string
      read FErrorText;
    property ErrorCode: string
      read FErrorCode;
    property Line: integer
      read FLine;
    property BuildUnitname: string
      read FBuildUnitname;

    property FilePath: string
      read FFilePath write FFilePath;

    constructor Create;
    procedure Clear;
  end;

  TRunError = class(TBuildError)
  protected
  public

  end;

  TRunErrorParser = class(TRunError)
  protected
  public
    constructor Create;

    procedure Parse(const s: string; const sUnitPath: string);
  end;

  TBuildOutputParser = class(TBuildError)
  protected
    FBuildOutput: TStrings;

  public

    constructor Create;
    destructor Destroy; override;

    procedure Parse(const s: string; const sUnitPath: string);
  end;

implementation

uses
  StrUtils, SysUtils;

{ TBuildOutputParser }

constructor TBuildOutputParser.Create;
begin
  inherited Create;

  FBuildOutput := TStringList.Create;
end;

destructor TBuildOutputParser.Destroy;
begin
  FBuildOutput.Free;

  inherited;
end;

procedure TBuildOutputParser.Parse(const s: string; const sUnitPath: string);
var
  i, c: integer;
  p1, p2, p3: integer;
begin
  FBuildOutput.Text := s;
  FFilePath := sUnitPath;

  // for each line, when you hit the first error, parse unit and line number
  c := FBuildOutput.Count - 1;
  for i := 0 to c do
  begin
    // format we need 'unitname.pas(123) Error: E1234 Blablabla'
    p1 := Pos(') Error: ', FBuildOutput[i]);
    if p1 > 0 then
    begin
      p2 := Pos('(', FBuildOutput[i]);
      FBuildUnitname := Copy(FBuildOutput[i], 1, p2 - 1);
      FLine := StrToIntDef(Copy(FBuildOutput[i], p2 + 1, p1 - p2 - 1), 0);
      p3 := PosEx(' ', FBuildOutput[i], p1+9);
      FErrorCode := Copy(FBuildOutput[i], p1 + 9, p3 - p1 - 9);
      FErrorText := Copy(FBuildOutput[i], p3 + 1);

      Exit;
    end;
  end;
end;

{ TBuildError }

procedure TBuildError.Clear;
begin
  FBuildUnitname := '';
  FLine := 0;
  FErrorCode := '';
  FErrorText := '';
  FFilePath := '';
end;

constructor TBuildError.Create;
begin
  Clear;
end;

{ TRunErrorParser }

constructor TRunErrorParser.Create;
begin
  inherited Create;
end;

procedure TRunErrorParser.Parse(const s, sUnitPath: string);
var
  p1, p2, p3, p4, p5: integer;
begin
(*
  1) Test_1: ETestFailure
     at 0048412c TESTGRIP_Test0409033653.exe TESTGRIP_unittest0409033653 48 TIntegratedTest.Test_1
      "TestObj has not been created yet"
*)

  FFilePath := sUnitPath;

  p1 := Pos('.exe ', s);
  if p1 <> 0 then
  begin
    p2 := PosEx(' ', s, p1 + 5);
    if p2 <> 0 then
    begin
      FBuildUnitname := Copy(s, p1 + 5, p2 - p1 - 5) + '.pas';
      FErrorCode := 'Exception';

      p3 := PosEx(' ', s, p2 + 1);
      if p3 <> 0 then
      begin
        FLine := StrToIntDef(Copy(s, p2 + 1, p3 - p2 - 1), 0);

        p4 := PosEx('' + #13#10, s, p3);
        if p4 <> 0 then
        begin
          p5 := PosEx('' + #13#10, s, p4 + 2);
          if p5 <> 0 then
          begin
            FErrorText := Trim(Copy(s, p4 + 2, p5 - p4 - 2));
          end
          else
          begin
            FErrorText := Trim(Copy(s, p4 + 2));
          end;
        end;
      end;
    end;
  end;
end;

end.
