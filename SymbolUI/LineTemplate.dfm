object FormLineTemplate: TFormLineTemplate
  Left = 207
  Top = 178
  BorderStyle = bsNone
  ClientHeight = 176
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object Label6: TLabel
    Left = 24
    Top = 143
    Width = 72
    Height = 12
    Caption = '模板缩放因子'
  end
  object Label5: TLabel
    Left = 10
    Top = 16
    Width = 361
    Height = 25
    AutoSize = False
    Caption = 
      '这个模板用来设计线符号的表现。拖动蓝色方块调整线符号的长度，点击' +
      '左边的黑色或白色方块改变线符号的形状。'
    WordWrap = True
  end
  object BTClearTempl: TButton
    Left = 240
    Top = 136
    Width = 89
    Height = 25
    Caption = '清除'
    TabOrder = 0
    OnClick = BTClearTemplClick
  end
  object EditTemplFactor: TEdit
    Left = 104
    Top = 139
    Width = 41
    Height = 20
    BiDiMode = bdLeftToRight
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = '宋体'
    Font.Style = []
    ParentBiDiMode = False
    ParentFont = False
    TabOrder = 1
    Text = '1'
    OnChange = EditTemplFactorChange
  end
  object UpDown4: TUpDown
    Left = 145
    Top = 139
    Width = 16
    Height = 20
    Associate = EditTemplFactor
    Min = 0
    Position = 1
    TabOrder = 2
    Wrap = False
  end
end
