object FrmUnitBrowserMain: TFrmUnitBrowserMain
  Left = 0
  Top = 0
  Caption = 'UnitBrowser v1.0'
  ClientHeight = 490
  ClientWidth = 855
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object treeMain: TTreeView
    Left = 0
    Top = 0
    Width = 670
    Height = 490
    Align = alClient
    Indent = 19
    TabOrder = 0
  end
  object pnlUses: TPanel
    Left = 670
    Top = 0
    Width = 185
    Height = 490
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object edInnerUses: TMemo
      Left = 0
      Top = 201
      Width = 185
      Height = 289
      Align = alClient
      TabOrder = 0
    end
    object edOuterUses: TMemo
      Left = 0
      Top = 0
      Width = 185
      Height = 201
      Align = alTop
      TabOrder = 1
    end
  end
end
