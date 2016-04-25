object FrmUnitSearchMain: TFrmUnitSearchMain
  Left = 0
  Top = 0
  Caption = 'Unit Search'
  ClientHeight = 336
  ClientWidth = 543
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    543
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 34
    Height = 13
    Caption = 'Project'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 50
    Height = 13
    Caption = 'Search for'
  end
  object edSearchUnit: TEdit
    Left = 72
    Top = 37
    Width = 382
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'UnitName'
  end
  object btnSearch: TButton
    Left = 460
    Top = 35
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Search'
    TabOrder = 1
    OnClick = btnSearchClick
  end
  object edProject: TEdit
    Left = 72
    Top = 8
    Width = 382
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    TextHint = 'Projectfile (dproj)'
  end
  object lstSearchResults: TListBox
    Left = 8
    Top = 72
    Width = 525
    Height = 256
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
    OnDblClick = lstSearchResultsDblClick
  end
  object btnBrowseForProjectFile: TButton
    Left = 460
    Top = 5
    Width = 34
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 4
    OnClick = btnBrowseForProjectFileClick
  end
end
