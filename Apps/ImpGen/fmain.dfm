object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Implement Interface'
  ClientHeight = 539
  ClientWidth = 769
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    769
    539)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 52
    Height = 13
    Caption = 'Projectfile:'
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 49
    Height = 13
    Caption = 'Interface:'
  end
  object Label3: TLabel
    Left = 8
    Top = 107
    Width = 78
    Height = 13
    Caption = 'Implementation:'
  end
  object edProject: TEdit
    Left = 92
    Top = 8
    Width = 504
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edProjectChange
    ExplicitWidth = 533
  end
  object cmbInterfaces: TComboBox
    Left = 92
    Top = 35
    Width = 504
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = cmbInterfacesChange
    ExplicitWidth = 533
  end
  object edImplementationClassname: TEdit
    Left = 92
    Top = 76
    Width = 504
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'TMyImplementation'
    OnChange = edImplementationClassnameChange
    ExplicitWidth = 533
  end
  object mmExample: TMemo
    Left = 92
    Top = 104
    Width = 669
    Height = 400
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 3
    ExplicitWidth = 698
    ExplicitHeight = 403
  end
  object btnCopyToClipboard: TButton
    Left = 686
    Top = 510
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Clipboard'
    TabOrder = 4
    OnClick = btnCopyToClipboardClick
    ExplicitLeft = 715
    ExplicitTop = 513
  end
  object btnBrowseForProjectFile: TButton
    Left = 602
    Top = 6
    Width = 34
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 5
    OnClick = btnBrowseForProjectFileClick
    ExplicitLeft = 631
  end
  object tmrFocus: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrFocusTimer
    Left = 40
    Top = 208
  end
end
