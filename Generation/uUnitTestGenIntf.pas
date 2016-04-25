unit uUnitTestGenIntf;

interface

uses
  Classes,
  uTestDefs;

type
  TUnitTestFunctionGen = class(TObject)
  protected
    FDef: string;
    FImp: string;
  public
    property Def: string
      read FDef write FDef;
    property Imp: string
      read FImp write FImp;
  end;

  IUnitTestClassFileGen = interface
    ['{C7B2C5E9-BE39-4FC3-A277-D5A135DB1516}']

    function GetUnitTestClassName: string;
    procedure SetUnitTestClassName(const Value: string);
    function GetOuterUses: TStringList;
    procedure SetOuterUses(const Value: TStringList);
    function GetWriteUTF8BOM: boolean;
    procedure SetWriteUTF8BOM(const Value: boolean);
    function GetSetupCode: string;
    procedure SetSetupCode(const Value: string);

    procedure GenerateTest(const aClass: TInputTestClass; const aFunction: TInputFunction; const aTest: TInputTest; const sTestClassName: string; const lstFunctionRef: TList = nil);
    function GetTemplateCode: string;

    procedure SaveAs(const sFilename: string);

    property UnitTestClassName: string
      read GetUnitTestClassName write SetUnitTestClassName;
    property SetupCode: string
      read GetSetupCode write SetSetupCode;
    property WriteUTF8BOM: boolean
      read GetWriteUTF8BOM write SetWriteUTF8BOM;
    property OuterUses: TStringList
      read GetOuterUses write SetOuterUses;
  end;

const
  c_inhclassname = 'TIntegratedTest';

implementation

end.
