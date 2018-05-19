object FormCreateSlim: TFormCreateSlim
  Left = 192
  Top = 110
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #26032#24314#30690#37327#22270#23618
  ClientHeight = 454
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 20
    Top = 200
    Width = 251
    Height = 201
    Caption = 'Spatial Info'
    TabOrder = 1
    object Label6: TLabel
      Left = 20
      Top = 29
      Width = 53
      Height = 15
      Caption = #24038#19979#35282'X'
    end
    object Label7: TLabel
      Left = 20
      Top = 60
      Width = 53
      Height = 15
      Caption = #24038#19979#35282'Y'
    end
    object Label8: TLabel
      Left = 20
      Top = 91
      Width = 53
      Height = 15
      Caption = #21491#19978#35282'X'
    end
    object Label10: TLabel
      Left = 10
      Top = 160
      Width = 75
      Height = 15
      Caption = #22235#21449#26641#32423#25968
    end
    object Label9: TLabel
      Left = 20
      Top = 121
      Width = 53
      Height = 15
      Caption = #21491#19978#35282'Y'
    end
    object EditLeft: TEdit
      Left = 90
      Top = 25
      Width = 131
      Height = 23
      TabOrder = 0
      Text = '0'
    end
    object EditBottom: TEdit
      Left = 90
      Top = 56
      Width = 131
      Height = 23
      TabOrder = 1
      Text = '0'
    end
    object EditTop: TEdit
      Left = 90
      Top = 88
      Width = 131
      Height = 23
      TabOrder = 2
      Text = '1000'
    end
    object ComboBox2: TComboBox
      Left = 90
      Top = 155
      Width = 131
      Height = 23
      Style = csDropDownList
      ItemHeight = 15
      ItemIndex = 5
      TabOrder = 4
      Text = '5'
      Items.Strings = (
        '0'
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8'
        '9'
        '10')
    end
    object EditRight: TEdit
      Left = 90
      Top = 118
      Width = 131
      Height = 23
      TabOrder = 3
      Text = '1000'
    end
  end
  object Button1: TButton
    Left = 390
    Top = 410
    Width = 94
    Height = 31
    Caption = #30830#23450
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 510
    Top = 410
    Width = 94
    Height = 31
    Cancel = True
    Caption = #21462#28040
    TabOrder = 6
    OnClick = Button2Click
  end
  object GroupBox2: TGroupBox
    Left = 290
    Top = 10
    Width = 321
    Height = 311
    Caption = #23646#24615#23383#27573#21517#31216#21015#34920
    TabOrder = 2
    object Label5: TLabel
      Left = 21
      Top = 271
      Width = 15
      Height = 60
      Caption = #20027#26174#23383#27573
      WordWrap = True
    end
    object Memo1: TMemo
      Left = 20
      Top = 30
      Width = 281
      Height = 231
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object Edit5: TEdit
      Left = 60
      Top = 275
      Width = 241
      Height = 23
      TabOrder = 1
      Text = '-1'
    end
  end
  object GBFileName: TGroupBox
    Left = 290
    Top = 330
    Width = 321
    Height = 71
    Caption = #25968#25454#25991#20214#21517
    TabOrder = 3
    object SBFileName: TSpeedButton
      Left = 20
      Top = 24
      Width = 33
      Height = 27
      Hint = #26032#24314#25991#20214#20301#32622
      Enabled = False
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000FF00FF00FF00
        FF00008484000084840000848400008484000084840000848400008484000084
        8400008484000084840000848400008484000000000000000000FF00FF00FF00
        FF0000848400FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
        FF0000FFFF0000FFFF0000FFFF00008484000000000000000000FF00FF000084
        8400FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
        FF0000FFFF0000FFFF0000FFFF00000000000084840000000000FF00FF000084
        8400FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
        FF0000FFFF0000FFFF000084840000000000008484000000000000848400FFFF
        FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
        FF0000FFFF0000FFFF0000000000FFFF0000FFFF00000000000000848400FFFF
        FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
        FF0000FFFF0000FFFF0000000000FFFF0000FFFF000000000000008484000084
        8400008484000084840000848400008484000084840000848400008484000084
        84000084840000848400FFFF0000FFFF0000FFFF000000000000FF00FF000084
        8400FFFFFF00FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
        0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000FF00FF000084
        8400FFFFFF00FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
        0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF000000000000FF00FF000084
        8400FFFFFF00FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFFFF000084
        84000084840000848400008484000084840000848400FF00FF00FF00FF00FF00
        FF0000848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000848400FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000084840000848400008484000084840000848400FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      ParentShowHint = False
      ShowHint = True
      OnClick = SBFileNameClick
    end
    object EditFileName: TEdit
      Left = 60
      Top = 25
      Width = 241
      Height = 23
      Enabled = False
      TabOrder = 0
    end
  end
  object CheckMDS: TCheckBox
    Left = 30
    Top = 410
    Width = 111
    Height = 21
    Caption = #20869#23384#25968#25454#28304
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckMDSClick
  end
  object GroupBox4: TGroupBox
    Left = 20
    Top = 10
    Width = 251
    Height = 181
    Caption = #22522#26412#20449#24687
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 24
      Width = 60
      Height = 15
      Caption = #22270#23618#21517#31216
    end
    object Label2: TLabel
      Left = 10
      Top = 85
      Width = 60
      Height = 15
      Caption = #22352#26631#21333#20301
    end
    object Label3: TLabel
      Left = 8
      Top = 114
      Width = 75
      Height = 15
      Caption = #22522#26412#27604#20363#23610
    end
    object Label4: TLabel
      Left = 10
      Top = 144
      Width = 60
      Height = 15
      Caption = #22352#26631#31934#24230
    end
    object Label11: TLabel
      Left = 10
      Top = 55
      Width = 60
      Height = 15
      Caption = #22270#23618#31867#22411
    end
    object EditName: TEdit
      Left = 90
      Top = 20
      Width = 131
      Height = 23
      TabOrder = 0
      Text = 'New SlimLayer'
    end
    object ComboBox1: TComboBox
      Left = 90
      Top = 80
      Width = 131
      Height = 23
      Style = csDropDownList
      ItemHeight = 15
      ItemIndex = 0
      TabOrder = 1
      Text = 'UNIT_M'
      Items.Strings = (
        'UNIT_M'
        'UNIT_KM'
        'UNIT_MILE'
        'UNIT_NAUTICALMILE'
        'UNIT_DEGREE')
    end
    object Edit2: TEdit
      Left = 90
      Top = 110
      Width = 131
      Height = 23
      TabOrder = 2
      Text = '2000'
    end
    object Edit3: TEdit
      Left = 90
      Top = 140
      Width = 131
      Height = 23
      TabOrder = 3
      Text = '0.0001'
    end
    object CBGeoType: TComboBox
      Left = 90
      Top = 50
      Width = 131
      Height = 23
      Style = csDropDownList
      ItemHeight = 15
      ItemIndex = 2
      TabOrder = 4
      Text = 'Polyline'
      Items.Strings = (
        'Point'
        'MultiPoint'
        'Polyline'
        'Polygon'
        'Annotation')
    end
  end
  object NewDlg: TSaveDialog
    Filter = 'TabbyMap SlimData(*.tsd)|*.tsd'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = #26032#24314#25968#25454#25991#20214
    Left = 264
    Top = 296
  end
end
