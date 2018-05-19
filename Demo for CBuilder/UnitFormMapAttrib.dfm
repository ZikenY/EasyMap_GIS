object FormMapAttrib: TFormMapAttrib
  Left = 192
  Top = 110
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #22320#22270#23646#24615
  ClientHeight = 264
  ClientWidth = 234
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 19
    Width = 48
    Height = 13
    Caption = #22320#22270#21517#31216
  end
  object Label4: TLabel
    Left = 16
    Top = 51
    Width = 60
    Height = 13
    Caption = #21442#32771#27604#20363#23610
  end
  object Button1: TButton
    Left = 48
    Top = 224
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 136
    Top = 224
    Width = 75
    Height = 25
    Cancel = True
    Caption = #21462#28040
    TabOrder = 4
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 80
    Top = 16
    Width = 129
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 80
    Top = 48
    Width = 129
    Height = 21
    TabOrder = 1
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 83
    Width = 201
    Height = 129
    Caption = #20854#23427#20449#24687
    TabOrder = 2
    object Memo1: TMemo
      Left = 16
      Top = 24
      Width = 169
      Height = 89
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
end
