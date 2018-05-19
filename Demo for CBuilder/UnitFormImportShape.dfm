object FormImportShape: TFormImportShape
  Left = 278
  Top = 182
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #21152#36733'ArcView ShapeFile'
  ClientHeight = 274
  ClientWidth = 441
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 441
    Height = 234
    ActivePage = TabSheet1
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #21442#25968#35774#32622
      object RGMapUnit: TRadioGroup
        Left = 16
        Top = 80
        Width = 265
        Height = 49
        Caption = #22352#26631#21333#20301
        Columns = 5
        ItemIndex = 0
        Items.Strings = (
          ' '#31859
          #20844#37324
          #33521#37324
          #28023#37324
          ' '#24230)
        TabOrder = 0
      end
      object GroupBox2: TGroupBox
        Left = 16
        Top = 8
        Width = 113
        Height = 57
        Caption = #27604#20363#23610
        TabOrder = 1
        object EditScale: TEdit
          Left = 16
          Top = 21
          Width = 81
          Height = 23
          AutoSelect = False
          BiDiMode = bdLeftToRight
          ParentBiDiMode = False
          TabOrder = 0
          Text = '2000'
        end
      end
      object GroupBox3: TGroupBox
        Left = 144
        Top = 8
        Width = 113
        Height = 57
        Caption = #22352#26631#31934#24230
        TabOrder = 2
        object EditPrecision: TEdit
          Left = 16
          Top = 21
          Width = 81
          Height = 23
          AutoSelect = False
          BiDiMode = bdLeftToRight
          ParentBiDiMode = False
          TabOrder = 0
          Text = '0.0001'
        end
      end
      object GroupBox4: TGroupBox
        Left = 272
        Top = 8
        Width = 145
        Height = 57
        Caption = #31354#38388#32034#24341
        TabOrder = 3
        object Label1: TLabel
          Left = 24
          Top = 24
          Width = 24
          Height = 12
          Caption = #32423#25968
        end
        object CBIndexLevel: TComboBox
          Left = 72
          Top = 21
          Width = 57
          Height = 20
          Style = csDropDownList
          BiDiMode = bdLeftToRight
          ItemHeight = 12
          ItemIndex = 5
          ParentBiDiMode = False
          TabOrder = 0
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
      end
      object GroupBox5: TGroupBox
        Left = -200
        Top = -56
        Width = 233
        Height = 73
        TabOrder = 4
        Visible = False
        object MemoSR: TMemo
          Left = 16
          Top = 16
          Width = 145
          Height = 47
          Enabled = False
          TabOrder = 0
        end
        object BTSR: TButton
          Left = 168
          Top = 16
          Width = 49
          Height = 47
          Caption = 'FT!!'
          Enabled = False
          TabOrder = 1
          OnClick = BTSRClick
        end
        object RBEnterSR: TRadioButton
          Left = 8
          Top = -2
          Width = 97
          Height = 17
          Caption = #25351#23450#31354#38388#21442#32771
          TabOrder = 2
          OnClick = RBEnterSRClick
        end
        object RBReadSR: TRadioButton
          Left = 112
          Top = -2
          Width = 97
          Height = 17
          Caption = #20174#25991#20214#20013#35835#21462
          Checked = True
          TabOrder = 3
          TabStop = True
          OnClick = RBEnterSRClick
        end
      end
      object GroupBox6: TGroupBox
        Left = 16
        Top = 144
        Width = 401
        Height = 49
        TabOrder = 5
        object EditSlimDir: TEdit
          Left = 16
          Top = 17
          Width = 369
          Height = 23
          Enabled = False
          TabOrder = 1
          Text = 'c:\esdout'
        end
        object CBSlim: TCheckBox
          Left = 8
          Top = -2
          Width = 217
          Height = 17
          Caption = #36716#25442#20026'SlimData      '#65293#23384#25918#20301#32622' '#65293
          TabOrder = 0
          OnClick = CBSlimClick
        end
      end
      object GroupBox7: TGroupBox
        Left = 296
        Top = 80
        Width = 121
        Height = 49
        TabOrder = 6
        object Label2: TLabel
          Left = 16
          Top = 24
          Width = 36
          Height = 12
          Caption = #23383#27573#21495
        end
        object CBAA: TComboBox
          Left = 56
          Top = 20
          Width = 49
          Height = 20
          Style = csDropDownList
          Enabled = False
          ItemHeight = 12
          ItemIndex = 0
          TabOrder = 1
          Text = '1'
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
        object CheckAA: TCheckBox
          Left = 8
          Top = 0
          Width = 89
          Height = 17
          Caption = #23646#24615#36716#27880#35760
          Enabled = False
          TabOrder = 0
          OnClick = CheckAAClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = #25991#20214#21015#34920
      ImageIndex = 1
      object GroupBox1: TGroupBox
        Left = 16
        Top = 8
        Width = 401
        Height = 185
        Caption = 'ShapeFile'#21015#34920
        TabOrder = 0
        object LBShapeFiles: TListBox
          Left = 16
          Top = 24
          Width = 369
          Height = 113
          ItemHeight = 15
          MultiSelect = True
          TabOrder = 0
        end
        object Button3: TButton
          Left = 216
          Top = 144
          Width = 75
          Height = 25
          Caption = #28155#21152#25991#20214
          TabOrder = 1
          OnClick = Button3Click
        end
        object Button4: TButton
          Left = 304
          Top = 144
          Width = 75
          Height = 25
          Caption = #21024#38500
          TabOrder = 2
          OnClick = Button4Click
        end
        object Button6: TButton
          Left = 24
          Top = 144
          Width = 75
          Height = 25
          Caption = #28155#21152#30446#24405
          Enabled = False
          TabOrder = 3
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 234
    Width = 441
    Height = 40
    Align = alBottom
    BevelInner = bvRaised
    TabOrder = 1
    object Panel2: TPanel
      Left = 246
      Top = 2
      Width = 193
      Height = 36
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Button1: TButton
        Left = 8
        Top = 5
        Width = 75
        Height = 25
        Caption = #30830#23450
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 94
        Top = 5
        Width = 75
        Height = 25
        Cancel = True
        Caption = #21462#28040
        TabOrder = 1
        OnClick = Button2Click
      end
    end
  end
  object OpenShapeFileDlg: TOpenDialog
    Filter = 'ArcView ShapeFile(*.shp)|*.shp'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 280
    Top = 152
  end
end
