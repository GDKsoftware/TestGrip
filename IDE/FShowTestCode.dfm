object frmShowTestcode: TfrmShowTestcode
  Left = 0
  Top = 0
  Caption = 'Show testcode'
  ClientHeight = 515
  ClientWidth = 883
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object memoCode: TSynMemo
    Left = 0
    Top = 0
    Width = 883
    Height = 456
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = SynHighlighter
    OnSpecialLineColors = memoCodeSpecialLineColors
    ExplicitHeight = 433
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 456
    Width = 883
    Height = 59
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object memoHints: TMemo
      Left = 0
      Top = 0
      Width = 883
      Height = 59
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitHeight = 42
    end
  end
  object SynHighlighter: TSynPasSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    StringAttri.Foreground = clBlue
    CharAttri.Foreground = clBlue
    Left = 816
    Top = 8
  end
end
