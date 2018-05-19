object FormRendererUI: TFormRendererUI
  Left = 292
  Top = 110
  Width = 700
  Height = 449
  Caption = '专题图设置'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 630
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '微软雅黑'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 17
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 692
    Height = 384
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 130
      Height = 382
      Align = alLeft
      BevelInner = bvRaised
      BevelOuter = bvNone
      TabOrder = 0
      object GBRendererType: TGroupBox
        Left = 8
        Top = 160
        Width = 113
        Height = 54
        Caption = '专题图选择'
        TabOrder = 0
        object CBUniqueValue: TCheckBox
          Left = 11
          Top = 22
          Width = 48
          Height = 25
          Caption = '单值'
          TabOrder = 0
          OnClick = CBUniqueValueClick
        end
        object CBGrade: TCheckBox
          Left = 58
          Top = 22
          Width = 45
          Height = 25
          Caption = '分级'
          TabOrder = 1
          OnClick = CBGradeClick
        end
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 224
        Width = 113
        Height = 62
        Caption = '字段选择'
        TabOrder = 1
        object CBField: TComboBox
          Left = 10
          Top = 24
          Width = 95
          Height = 25
          Style = csDropDownList
          ItemHeight = 17
          TabOrder = 0
          OnChange = CBFieldChange
        end
      end
      object GroupBox3: TGroupBox
        Left = 8
        Top = 8
        Width = 113
        Height = 145
        Caption = '缺省符号'
        TabOrder = 2
        object CBShowDefaultSymbol: TCheckBox
          Left = 17
          Top = 113
          Width = 74
          Height = 25
          Caption = '显示缺省'
          TabOrder = 0
        end
        object PanelDefaultSymbol: TPanel
          Left = 8
          Top = 20
          Width = 97
          Height = 65
          TabOrder = 1
          OnMouseMove = PanelDefaultSymbolMouseMove
        end
        object BtnDefaultSymbol: TButton
          Left = 8
          Top = 86
          Width = 97
          Height = 24
          Caption = '选择缺省符号'
          TabOrder = 2
          OnClick = BtnDefaultSymbolClick
        end
      end
    end
    object PanelRendererBase: TPanel
      Left = 131
      Top = 1
      Width = 560
      Height = 382
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvNone
      TabOrder = 1
      object PanelGradeTop: TPanel
        Left = 8
        Top = 81
        Width = 529
        Height = 72
        TabOrder = 0
        object GroupBox4: TGroupBox
          Left = 184
          Top = 3
          Width = 129
          Height = 57
          Caption = '颜色模板'
          TabOrder = 0
          object CBColorGrade: TComboBox
            Left = 16
            Top = 21
            Width = 97
            Height = 25
            Style = csDropDownList
            ItemHeight = 17
            TabOrder = 0
            Items.Strings = (
              '随机乱给颜色')
          end
          object CBGradeColor: TCheckBox
            Left = 8
            Top = 0
            Width = 73
            Height = 17
            Caption = '颜色分级'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
        end
        object GroupBox5: TGroupBox
          Left = 96
          Top = 3
          Width = 81
          Height = 57
          Caption = '级数'
          TabOrder = 1
          object CBGradeLevel: TComboBox
            Left = 16
            Top = 21
            Width = 49
            Height = 25
            Style = csDropDownList
            ItemHeight = 17
            TabOrder = 0
          end
        end
        object GroupBox6: TGroupBox
          Left = 16
          Top = 3
          Width = 73
          Height = 57
          Caption = '符号模板'
          TabOrder = 2
          object Button7: TButton
            Left = 13
            Top = 20
            Width = 48
            Height = 26
            Caption = '选择'
            TabOrder = 0
            OnClick = Button1Click
          end
        end
        object GroupBox7: TGroupBox
          Left = 320
          Top = 3
          Width = 153
          Height = 57
          Caption = '符号尺寸'
          TabOrder = 3
          object Label1: TLabel
            Left = 11
            Top = 24
            Width = 24
            Height = 17
            Caption = '最小'
          end
          object Label2: TLabel
            Left = 77
            Top = 24
            Width = 24
            Height = 17
            Caption = '最大'
          end
          object Edit1: TEdit
            Left = 37
            Top = 21
            Width = 37
            Height = 25
            TabOrder = 0
            Text = '0.2'
          end
          object Edit2: TEdit
            Left = 103
            Top = 21
            Width = 37
            Height = 25
            TabOrder = 1
            Text = '1.5'
          end
          object CBGradeSize: TCheckBox
            Left = 8
            Top = 0
            Width = 97
            Height = 17
            Caption = '符号尺寸分级'
            TabOrder = 2
          end
        end
      end
      object PanelUVTop: TPanel
        Left = 9
        Top = 9
        Width = 528
        Height = 72
        TabOrder = 1
        object Button4: TButton
          Left = 115
          Top = 35
          Width = 98
          Height = 26
          Caption = '清除分类'
          TabOrder = 3
          OnClick = Button4Click
        end
        object Button3: TButton
          Left = 16
          Top = 35
          Width = 98
          Height = 26
          Caption = '增加所有分类'
          TabOrder = 2
          OnClick = Button3Click
        end
        object Button2: TButton
          Left = 115
          Top = 8
          Width = 98
          Height = 26
          Caption = '增加分类'
          TabOrder = 1
          OnClick = Button2Click
        end
        object Button1: TButton
          Left = 16
          Top = 8
          Width = 98
          Height = 26
          Caption = '选择符号模板'
          TabOrder = 0
          OnClick = Button1Click
        end
        object GroupBox2: TGroupBox
          Left = 224
          Top = 3
          Width = 193
          Height = 57
          Caption = '颜色模板'
          TabOrder = 4
          object CBColorUV: TComboBox
            Left = 16
            Top = 21
            Width = 161
            Height = 25
            Style = csDropDownList
            ItemHeight = 17
            TabOrder = 0
            OnClick = CBColorUVClick
            Items.Strings = (
              '随机乱给颜色')
          end
        end
      end
      object PanelSymbolMap: TPanel
        Left = -63
        Top = 168
        Width = 549
        Height = 257
        TabOrder = 2
        object Panel3: TPanel
          Left = 1
          Top = 1
          Width = 547
          Height = 9
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
        end
        object Panel4: TPanel
          Left = 1
          Top = 247
          Width = 547
          Height = 9
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
        end
        object Panel5: TPanel
          Left = 1
          Top = 10
          Width = 8
          Height = 237
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 2
        end
        object Panel6: TPanel
          Left = 539
          Top = 10
          Width = 9
          Height = 237
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 3
        end
        object LVItems: TListView
          Left = 56
          Top = 32
          Width = 417
          Height = 153
          Columns = <>
          Font.Charset = GB2312_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = '微软雅黑'
          Font.Style = []
          GridLines = True
          LargeImages = ImageList1
          OwnerDraw = True
          ParentFont = False
          TabOrder = 4
          OnAdvancedCustomDrawItem = LVItemsAdvancedCustomDrawItem
          OnDblClick = LVItemsDblClick
        end
      end
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 384
    Width = 692
    Height = 35
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvNone
    TabOrder = 1
    object Button5: TButton
      Left = 8
      Top = 5
      Width = 73
      Height = 25
      Caption = '确定'
      TabOrder = 0
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 88
      Top = 5
      Width = 73
      Height = 25
      Caption = '取消'
      TabOrder = 1
      OnClick = Button6Click
    end
  end
  object Timer1: TTimer
    Interval = 300
    OnTimer = Timer1Timer
    Left = 129
    Top = 1
  end
  object ImageList1: TImageList
    Height = 72
    Width = 120
    Left = 164
    Top = 209
  end
end
