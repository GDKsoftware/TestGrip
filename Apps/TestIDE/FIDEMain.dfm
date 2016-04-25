object frmIDEMain: TfrmIDEMain
  Left = 0
  Top = 0
  Caption = 'Delphi IDE Simulator'
  ClientHeight = 666
  ClientWidth = 964
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 241
    Top = 0
    Height = 550
    ExplicitLeft = 247
    ExplicitTop = -16
    ExplicitHeight = 553
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 550
    Width = 964
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 113
  end
  object pnlTestgripContainer: TPanel
    Left = 0
    Top = 0
    Width = 241
    Height = 550
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlMessages: TPanel
    Left = 0
    Top = 553
    Width = 964
    Height = 113
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object edMessages: TMemo
      Left = 0
      Top = 0
      Width = 964
      Height = 113
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object pcUnits: TPageControl
    Left = 244
    Top = 0
    Width = 720
    Height = 550
    Align = alClient
    TabOrder = 2
  end
  object MainMenu1: TMainMenu
    Left = 480
    Top = 296
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
  end
  object ActionManager1: TActionManager
    Left = 656
    Top = 224
    StyleName = 'Platform Default'
    object acSave: TAction
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = acSaveExecute
    end
  end
end
