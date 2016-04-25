object frmParamEditor: TfrmParamEditor
  Left = 0
  Top = 0
  Caption = 'Param editor'
  ClientHeight = 409
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 368
    Width = 478
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      478
      41)
    object btnSave: TButton
      Left = 341
      Top = 6
      Width = 121
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save and close'
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 260
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 478
    Height = 368
    Align = alClient
    TabOrder = 1
    DesignSize = (
      478
      368)
    object Label1: TLabel
      Left = 8
      Top = 120
      Width = 73
      Height = 13
      Caption = 'Param init code'
    end
    object lblVariable: TLabel
      Left = 8
      Top = 8
      Width = 97
      Height = 13
      Caption = 'Optional extra vars:'
    end
    object Memo1: TMemo
      Left = 8
      Top = 139
      Width = 457
      Height = 223
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
    end
    object mmVars: TMemo
      Left = 8
      Top = 27
      Width = 457
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
end
