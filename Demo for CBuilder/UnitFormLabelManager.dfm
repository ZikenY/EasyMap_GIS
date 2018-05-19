object FormLabelManager: TFormLabelManager
  Left = 271
  Top = 110
  Width = 571
  Height = 492
  BorderIcons = [biSystemMenu]
  Caption = #26631#27880#31649#29702#22120
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #24494#36719#38597#40657
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 17
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 217
    Height = 401
    Caption = #26631#27880#23618#21015#34920
    TabOrder = 0
    object LBLayerList: TListBox
      Left = 16
      Top = 24
      Width = 185
      Height = 329
      ItemHeight = 17
      TabOrder = 0
      OnClick = LBLayerListClick
    end
    object Button1: TButton
      Left = 16
      Top = 360
      Width = 57
      Height = 25
      Caption = #19978#31227
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 80
      Top = 360
      Width = 57
      Height = 25
      Caption = #19979#31227
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 144
      Top = 360
      Width = 57
      Height = 25
      Caption = #21024#38500
      TabOrder = 3
      OnClick = Button3Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 240
    Top = 8
    Width = 313
    Height = 401
    Caption = #26631#27880#23646#24615
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 28
      Width = 60
      Height = 17
      Caption = #26631#35760#22270#23618#21517
    end
    object Label2: TLabel
      Left = 16
      Top = 108
      Width = 48
      Height = 17
      Caption = #26631#27880#23383#27573
    end
    object Label3: TLabel
      Left = 16
      Top = 228
      Width = 48
      Height = 17
      Caption = #23383#20307#39068#33394
    end
    object Label4: TLabel
      Left = 16
      Top = 148
      Width = 48
      Height = 17
      Caption = #23383#20307#39640#24230
    end
    object Label5: TLabel
      Left = 16
      Top = 188
      Width = 48
      Height = 17
      Caption = #23383#20307#23485#24230
    end
    object Label6: TLabel
      Left = 16
      Top = 68
      Width = 60
      Height = 17
      Caption = #30690#37327#22270#23618#21517
    end
    object Label7: TLabel
      Left = 16
      Top = 267
      Width = 60
      Height = 17
      Caption = #26368#22823#27604#20363#23610
    end
    object Label8: TLabel
      Left = 16
      Top = 307
      Width = 60
      Height = 17
      Caption = #26368#23567#27604#20363#23610
    end
    object Label9: TLabel
      Left = 16
      Top = 347
      Width = 60
      Height = 17
      Caption = #21442#32771#27604#20363#23610
    end
    object EditLayerName: TEdit
      Left = 80
      Top = 24
      Width = 217
      Height = 25
      TabOrder = 0
      OnChange = EditLayerNameChange
    end
    object CBFieldIndex: TComboBox
      Left = 80
      Top = 104
      Width = 217
      Height = 25
      Style = csDropDownList
      ItemHeight = 17
      TabOrder = 1
      OnChange = CBFieldIndexChange
    end
    object PanelFontColor: TPanel
      Left = 80
      Top = 224
      Width = 217
      Height = 25
      TabOrder = 2
      OnClick = PanelFontColorClick
    end
    object EditFontHeight: TEdit
      Left = 80
      Top = 144
      Width = 217
      Height = 25
      TabOrder = 3
      Text = '0'
      OnChange = EditFontHeightChange
    end
    object EditFontWidth: TEdit
      Left = 80
      Top = 184
      Width = 217
      Height = 25
      TabOrder = 4
      Text = '0'
      OnChange = EditFontWidthChange
    end
    object EditVectorLayer: TEdit
      Left = 80
      Top = 64
      Width = 217
      Height = 25
      Color = 13303807
      ReadOnly = True
      TabOrder = 5
    end
    object CBVisible: TCheckBox
      Left = 208
      Top = 376
      Width = 81
      Height = 17
      Caption = #26631#35760#21487#35270
      TabOrder = 6
      OnClick = CBVisibleClick
    end
    object EditMaxScale: TEdit
      Left = 80
      Top = 264
      Width = 217
      Height = 25
      TabOrder = 7
      Text = '0'
      OnChange = EditMaxScaleChange
    end
    object EditMinScale: TEdit
      Left = 80
      Top = 304
      Width = 217
      Height = 25
      TabOrder = 8
      Text = '0'
      OnChange = EditMinScaleChange
    end
    object EditRefScale: TEdit
      Left = 80
      Top = 344
      Width = 217
      Height = 25
      TabOrder = 9
      Text = '0'
      OnChange = EditRefScaleChange
    end
  end
  object Button4: TButton
    Left = 456
    Top = 416
    Width = 97
    Height = 33
    Caption = #20851#38381
    TabOrder = 2
    OnClick = Button4Click
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    Left = 512
    Top = 232
  end
end
