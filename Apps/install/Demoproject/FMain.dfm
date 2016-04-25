object frmWorld: TfrmWorld
  Left = 0
  Top = 0
  Caption = 'The World'
  ClientHeight = 431
  ClientWidth = 675
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
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 37
    Height = 13
    Caption = 'Objects'
  end
  object Label6: TLabel
    Left = 194
    Top = 328
    Width = 262
    Height = 13
    Caption = 'Take a look at the uWorld.pas unit for all the unit tests'
  end
  object lbObjects: TListBox
    Left = 8
    Top = 32
    Width = 289
    Height = 257
    ItemHeight = 13
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 336
    Top = 32
    Width = 249
    Height = 153
    Caption = 'Create a person'
    TabOrder = 1
    object Label2: TLabel
      Left = 16
      Top = 35
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object Label3: TLabel
      Left = 16
      Top = 62
      Width = 33
      Height = 13
      Caption = 'Length'
    end
    object Label4: TLabel
      Left = 16
      Top = 89
      Width = 34
      Height = 13
      Caption = 'Weight'
    end
    object edPersonName: TEdit
      Left = 112
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edLength: TEdit
      Left = 112
      Top = 59
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object edWeight: TEdit
      Left = 112
      Top = 86
      Width = 121
      Height = 21
      TabOrder = 2
    end
    object btnAddPerson: TButton
      Left = 160
      Top = 113
      Width = 75
      Height = 25
      Caption = 'AddPerson'
      TabOrder = 3
      OnClick = btnAddPersonClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 336
    Top = 191
    Width = 249
    Height = 98
    Caption = 'Create a tree'
    TabOrder = 2
    object Label5: TLabel
      Left = 16
      Top = 35
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object edTreeName: TEdit
      Left = 112
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object btnAddTree: TButton
      Left = 160
      Top = 59
      Width = 75
      Height = 25
      Caption = 'AddPerson'
      TabOrder = 1
      OnClick = btnAddTreeClick
    end
  end
end
