unit uUnitRegen;

interface

uses
  uUnitParser, Classes;

type
  TUnitRegen = class(TUnitParser)
  protected
    procedure WriteUsesToStream(const AWriter: TStream; const AUses: TStrings);
    function GetFormattedUses(const AUses: TStrings): string;
    procedure CopyCrlf(const AReader: TStream; const AWriter: TStream);
  public
    procedure WriteUsesChanges(const AInFilename: string; const AOutFilename: string);
  end;

implementation

uses
  SysUtils;

{ TUnitRegen }

procedure TUnitRegen.CopyCrlf(const AReader, AWriter: TStream);
var
  Buffer: AnsiString;
  CurrentPos: Integer;
  BytesRead: Integer;
begin
  SetLength(Buffer, 2);

  CurrentPos := AReader.Position;

  BytesRead := AReader.Read(Buffer[1], 2);
  if BytesRead = 2 then
  begin
    if (Buffer[1] = #13) and (Buffer[2] = #10) then
    begin
      AWriter.Write(Buffer[1], 2);
    end
    else
    begin
      AReader.Seek(soFromBeginning, CurrentPos);
    end;
  end
  else
  begin
    AReader.Seek(soFromBeginning, CurrentPos);
  end;
end;

function TUnitRegen.GetFormattedUses(const AUses: TStrings): string;
var
  I, C: Integer;
  UsedUnit: string;
begin
  Result := '';

  C := AUses.Count - 1;
  for I := 0 to C do
  begin
    UsedUnit := AUses[I];
    if UsedUnit <> '' then
    begin
      if Result <> '' then
      begin
        Result := Result + ','#13#10;
      end;

      Result := Result + '  ' + UsedUnit;
    end;
  end;

  if Result <> '' then
    Result :=
      'uses'#13#10 + Result + ';'#13#10;
end;

procedure TUnitRegen.WriteUsesChanges(const AInFilename: string; const AOutFilename: string);
var
  Reader, Writer: TStream;
  TempStream: TStream;
begin
  Assert(FOuterUsesBoundaries.Start <> 0);
  Assert(FInnerUsesBoundaries.Start <> 0);

  if AInFilename <> AOutFilename then
  begin
    Reader := TFileStream.Create(AInFilename, fmOpenRead);
  end
  else
  begin
    Reader := TStringStream.Create;

    TempStream := TFileStream.Create(AInFilename, fmOpenRead);
    try
      Reader.CopyFrom(TempStream, TempStream.Size);
    finally
      TempStream.Free;
    end;

    Reader.Seek(soFromBeginning, 0);
  end;

  try
    Writer := TFileStream.Create(AOutFilename, fmCreate or fmOpenWrite);
    try
      // read/write until outeruses
      Writer.CopyFrom(Reader, FOuterUsesBoundaries.Start);

      CopyCrlf(Reader, Writer);

      // write new outeruses
      WriteUsesToStream(Writer, FOuterUsesList);

      // skip original uses
      Reader.Seek(FOuterUsesBoundaries.Stop - FOuterUsesBoundaries.Start, soFromCurrent);

      // read/write until inneruses
      Writer.CopyFrom(Reader, FInnerUsesBoundaries.Start - FOuterUsesBoundaries.Stop - 2);

      CopyCrlf(Reader, Writer);

      // write new inneruses
      WriteUsesToStream(Writer, FInnerUsesList);

      // skip original uses
      if FInnerUsesBoundaries.Stop - FInnerUsesBoundaries.Start <> 0 then
        Reader.Seek(FInnerUsesBoundaries.Stop - FInnerUsesBoundaries.Start, soFromCurrent);

      // read/write until end
      Writer.CopyFrom(Reader, Reader.Size - Reader.Position);
    finally
      Writer.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TUnitRegen.WriteUsesToStream(const AWriter: TStream; const AUses: TStrings);
var
  UsesStream: TStringStream;
begin
  UsesStream := TStringStream.Create(GetFormattedUses(AUses));
  try
    if UsesStream.Size <> 0 then
    begin
      AWriter.CopyFrom(UsesStream, UsesStream.Size);
    end;
  finally
    UsesStream.Free;
  end;
end;

end.
