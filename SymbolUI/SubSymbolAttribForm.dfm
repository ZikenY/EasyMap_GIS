object SubSymbolAttrib: TSubSymbolAttrib
  Left = 380
  Top = 337
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = '图元属性'
  ClientHeight = 252
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object PanelSimplePoint: TPanel
    Left = -184
    Top = 152
    Width = 396
    Height = 202
    TabOrder = 0
    TabStop = True
    Visible = False
    object Label1: TLabel
      Left = 43
      Top = 44
      Width = 24
      Height = 12
      Caption = '颜色'
    end
    object Label2: TLabel
      Left = 40
      Top = 124
      Width = 30
      Height = 12
      Caption = 'X偏移'
    end
    object Label3: TLabel
      Left = 216
      Top = 124
      Width = 30
      Height = 12
      Caption = 'Y偏移'
    end
    object Label12: TLabel
      Left = 43
      Top = 84
      Width = 24
      Height = 12
      Caption = '尺寸'
    end
    object Label13: TLabel
      Left = 200
      Top = 84
      Width = 48
      Height = 12
      Caption = '边线宽度'
    end
    object PanelSPColor: TPanel
      Left = 80
      Top = 40
      Width = 97
      Height = 21
      TabOrder = 5
      OnClick = PanelSPColorClick
    end
    object EditSPOffsetX: TEdit
      Left = 80
      Top = 120
      Width = 97
      Height = 20
      TabOrder = 3
      OnChange = EditSPOffsetXChange
    end
    object EditSPOffsetY: TEdit
      Left = 256
      Top = 120
      Width = 97
      Height = 20
      TabOrder = 4
      OnChange = EditSPOffsetXChange
    end
    object EditSPSize: TEdit
      Left = 80
      Top = 80
      Width = 97
      Height = 20
      TabOrder = 1
      OnChange = EditSPSizeChange
    end
    object EditSPLineWidth: TEdit
      Left = 256
      Top = 80
      Width = 97
      Height = 20
      TabOrder = 2
      OnChange = EditSPLineWidthChange
    end
    object CBSPLockColor: TCheckBox
      Left = 200
      Top = 40
      Width = 105
      Height = 17
      Alignment = taLeftJustify
      Caption = '锁定颜色'
      TabOrder = 0
      OnClick = CBSPLockColorClick
    end
  end
  object PanelEnvelopePoint: TPanel
    Left = 0
    Top = 0
    Width = 396
    Height = 202
    TabOrder = 1
    TabStop = True
    Visible = False
    object Label4: TLabel
      Left = 40
      Top = 28
      Width = 24
      Height = 12
      Caption = '颜色'
    end
    object Label5: TLabel
      Left = 40
      Top = 68
      Width = 30
      Height = 12
      Caption = 'X偏移'
    end
    object Label6: TLabel
      Left = 216
      Top = 68
      Width = 30
      Height = 12
      Caption = 'Y偏移'
    end
    object Label7: TLabel
      Left = 24
      Top = 148
      Width = 48
      Height = 12
      Caption = '旋转角度'
    end
    object Label8: TLabel
      Left = 43
      Top = 108
      Width = 24
      Height = 12
      Caption = '宽度'
    end
    object Label9: TLabel
      Left = 219
      Top = 108
      Width = 24
      Height = 12
      Caption = '高度'
    end
    object Label16: TLabel
      Left = 200
      Top = 148
      Width = 48
      Height = 12
      Caption = '边线宽度'
    end
    object PanelEPColor: TPanel
      Left = 80
      Top = 24
      Width = 97
      Height = 21
      TabOrder = 0
      OnClick = PanelEPColorClick
    end
    object EditEPOffsetX: TEdit
      Left = 80
      Top = 64
      Width = 97
      Height = 20
      TabOrder = 2
      OnChange = EditEPOffsetXChange
    end
    object EditEPOffsetY: TEdit
      Left = 256
      Top = 64
      Width = 97
      Height = 20
      TabOrder = 3
      OnChange = EditEPOffsetXChange
    end
    object EditEPAngle: TEdit
      Left = 80
      Top = 144
      Width = 97
      Height = 20
      TabOrder = 6
      OnChange = EditEPAngleChange
    end
    object EditEPWidth: TEdit
      Left = 80
      Top = 104
      Width = 97
      Height = 20
      TabOrder = 4
      OnChange = EditEPWidthChange
    end
    object EditEPHeight: TEdit
      Left = 256
      Top = 104
      Width = 97
      Height = 20
      TabOrder = 5
      OnChange = EditEPHeightChange
    end
    object CBEPLockColor: TCheckBox
      Left = 216
      Top = 26
      Width = 105
      Height = 17
      Alignment = taLeftJustify
      Caption = '锁定颜色'
      TabOrder = 1
      OnClick = CBEPLockColorClick
    end
    object EditEPLineWidth: TEdit
      Left = 256
      Top = 144
      Width = 97
      Height = 20
      TabOrder = 7
      OnChange = EditEPLineWidthChange
    end
  end
  object PanelShapePoint: TPanel
    Left = 272
    Top = 136
    Width = 396
    Height = 249
    TabOrder = 2
    TabStop = True
    Visible = False
    object Label10: TLabel
      Left = 40
      Top = 20
      Width = 24
      Height = 12
      Caption = '颜色'
    end
    object Label11: TLabel
      Left = 200
      Top = 20
      Width = 48
      Height = 12
      Caption = '旋转角度'
    end
    object Label14: TLabel
      Left = 40
      Top = 52
      Width = 30
      Height = 12
      Caption = 'X偏移'
    end
    object Label15: TLabel
      Left = 216
      Top = 52
      Width = 30
      Height = 12
      Caption = 'Y偏移'
    end
    object Label17: TLabel
      Left = 24
      Top = 84
      Width = 48
      Height = 12
      Caption = '边线宽度'
    end
    object PanelPPColor: TPanel
      Left = 80
      Top = 16
      Width = 97
      Height = 21
      TabOrder = 0
      OnClick = PanelPPColorClick
    end
    object EditPPAngle: TEdit
      Left = 256
      Top = 16
      Width = 97
      Height = 20
      TabOrder = 1
      OnChange = EditPPAngleChange
    end
    object GroupBox1: TGroupBox
      Left = 40
      Top = 109
      Width = 313
      Height = 129
      Caption = '节点坐标'
      TabOrder = 6
      object StringGrid1: TStringGrid
        Left = 16
        Top = 20
        Width = 281
        Height = 93
        ColCount = 3
        DefaultColWidth = 80
        DefaultRowHeight = 15
        RowCount = 2
        TabOrder = 0
      end
    end
    object CBPPLockColor: TCheckBox
      Left = 200
      Top = 80
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = '锁定颜色'
      TabOrder = 5
      OnClick = CBPPLockColorClick
    end
    object EditPPOffsetX: TEdit
      Left = 80
      Top = 48
      Width = 97
      Height = 20
      TabOrder = 2
      OnChange = EditPPOffsetXChange
    end
    object EditPPOffsetY: TEdit
      Left = 256
      Top = 48
      Width = 97
      Height = 20
      TabOrder = 3
      OnChange = EditPPOffsetXChange
    end
    object EditPPLineWidth: TEdit
      Left = 80
      Top = 80
      Width = 97
      Height = 20
      TabOrder = 4
      OnChange = EditPPLineWidthChange
    end
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    Left = 64
    Top = 16
  end
end
