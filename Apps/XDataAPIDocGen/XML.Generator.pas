unit XML.Generator;

interface

type
  TXMLGenerator = class(TObject)
  protected
    FDirectXSLLink: string;

    procedure WriteXMLStringToFile(const AFilename, AData: string);
  public

  end;

implementation

uses
  System.Classes, System.SysUtils;

const
  C_DefaultXMLHeader = '<?xml version="1.0" encoding="utf-8"?>'#13#10;


procedure TXMLGenerator.WriteXMLStringToFile(const AFilename, AData: string);
var
  FileStream: TFileStream;
  StringStream: TStringStream;
  Headers: string;
begin
  FileStream := TFileStream.Create(AFilename, fmCreate or fmOpenWrite);
  try
    Headers := C_DefaultXMLHeader;
    if FDirectXSLLink <> '' then
      Headers := Headers + '<?xml-stylesheet type="text/xsl" href="' + FDirectXSLLink + '"?>'#13#10;

    StringStream := TStringStream.Create(Headers + AData, TEncoding.UTF8);
    try
      StringStream.SaveToStream(FileStream);
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
