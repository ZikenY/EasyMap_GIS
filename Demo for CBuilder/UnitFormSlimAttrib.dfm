object FormSlimAttrib: TFormSlimAttrib
  Left = 196
  Top = 122
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #22270#23618#23646#24615
  ClientHeight = 382
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 24
    Top = 12
    Width = 48
    Height = 12
    Caption = #22270#23618#21517#31216
  end
  object Label4: TLabel
    Left = 16
    Top = 44
    Width = 60
    Height = 12
    Caption = #21442#32771#27604#20363#23610
  end
  object Label6: TLabel
    Left = 248
    Top = 12
    Width = 36
    Height = 12
    Caption = #36879#26126#24230
  end
  object Label7: TLabel
    Left = 21
    Top = 319
    Width = 48
    Height = 12
    Caption = #26631#27880#23383#27573
  end
  object Button1: TButton
    Left = 256
    Top = 344
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 8
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 344
    Top = 344
    Width = 75
    Height = 25
    Cancel = True
    Caption = #21462#28040
    TabOrder = 9
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 88
    Top = 8
    Width = 129
    Height = 20
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 72
    Width = 73
    Height = 25
    Caption = #22270#23618#26174#31034
    TabOrder = 2
  end
  object CheckBox2: TCheckBox
    Left = 112
    Top = 72
    Width = 57
    Height = 25
    Caption = #21487#36873#25321
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 104
    Width = 201
    Height = 83
    Caption = #26174#31034#27604#20363#23610#33539#22260
    TabOrder = 4
    object Label2: TLabel
      Left = 17
      Top = 27
      Width = 24
      Height = 12
      Caption = #26368#22823
    end
    object Label3: TLabel
      Left = 17
      Top = 51
      Width = 24
      Height = 12
      Caption = #26368#23567
    end
    object Edit2: TEdit
      Left = 56
      Top = 24
      Width = 129
      Height = 20
      TabOrder = 0
    end
    object Edit3: TEdit
      Left = 56
      Top = 48
      Width = 129
      Height = 20
      TabOrder = 1
    end
  end
  object Edit4: TEdit
    Left = 88
    Top = 40
    Width = 129
    Height = 20
    TabOrder = 1
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 200
    Width = 201
    Height = 105
    Caption = #20854#23427#20449#24687
    TabOrder = 5
    object Memo1: TMemo
      Left = 16
      Top = 24
      Width = 169
      Height = 65
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object GroupBox3: TGroupBox
    Left = 232
    Top = 38
    Width = 193
    Height = 291
    Caption = #23646#24615#23383#27573#21517#31216#21015#34920
    TabOrder = 7
    object Label5: TLabel
      Left = 16
      Top = 263
      Width = 48
      Height = 12
      Caption = #20027#26174#23383#27573
    end
    object Memo2: TMemo
      Left = 16
      Top = 24
      Width = 161
      Height = 220
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object Edit5: TEdit
      Left = 72
      Top = 259
      Width = 105
      Height = 20
      TabOrder = 1
    end
  end
  object Edit6: TEdit
    Left = 296
    Top = 8
    Width = 129
    Height = 20
    TabOrder = 6
  end
  object CBLabelLayerField: TComboBox
    Left = 72
    Top = 315
    Width = 145
    Height = 20
    Style = csDropDownList
    ItemHeight = 12
    TabOrder = 10
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      '10'
      '11'
      '12'
      '13'
      '14'
      '15'
      '16'
      '17'
      '18'
      '19'
      '20')
  end
  object Button3: TButton
    Left = 16
    Top = 344
    Width = 105
    Height = 25
    Caption = #29983#25104#23646#24615#26631#27880#23618
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    TabOrder = 11
    OnClick = Button3Click
  end
end
