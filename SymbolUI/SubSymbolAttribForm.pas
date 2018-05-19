unit SubSymbolAttribForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Grids, InterfaceObj, InterfaceDisplay, InterfaceSymbol,
  WKSStructs;

type
    TEVENTSymbolModified = procedure() of Object;

type
  TSubSymbolAttrib = class(TForm)
    PanelSimplePoint: TPanel;
    PanelEnvelopePoint: TPanel;
    PanelShapePoint: TPanel;
    Label1: TLabel;
    PanelSPColor: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    EditSPOffsetX: TEdit;
    EditSPOffsetY: TEdit;
    Label4: TLabel;
    PanelEPColor: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    EditEPOffsetX: TEdit;
    EditEPOffsetY: TEdit;
    Label7: TLabel;
    EditEPAngle: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    EditEPWidth: TEdit;
    EditEPHeight: TEdit;
    Label10: TLabel;
    PanelPPColor: TPanel;
    Label11: TLabel;
    EditPPAngle: TEdit;
    GroupBox1: TGroupBox;
    StringGrid1: TStringGrid;
    Label12: TLabel;
    EditSPSize: TEdit;
    EditSPLineWidth: TEdit;
    Label13: TLabel;
    CBSPLockColor: TCheckBox;
    CBEPLockColor: TCheckBox;
    CBPPLockColor: TCheckBox;
    Label14: TLabel;
    Label15: TLabel;
    EditPPOffsetX: TEdit;
    EditPPOffsetY: TEdit;
    Label16: TLabel;
    EditEPLineWidth: TEdit;
    EditPPLineWidth: TEdit;
    Label17: TLabel;
    ColorDialog1: TColorDialog;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PanelSPColorClick(Sender: TObject);
    procedure PanelEPColorClick(Sender: TObject);
    procedure PanelPPColorClick(Sender: TObject);
    procedure CBSPLockColorClick(Sender: TObject);
    procedure CBEPLockColorClick(Sender: TObject);
    procedure CBPPLockColorClick(Sender: TObject);
    procedure EditSPLineWidthChange(Sender: TObject);
    procedure EditEPLineWidthChange(Sender: TObject);
    procedure EditPPLineWidthChange(Sender: TObject);
    procedure EditSPOffsetXChange(Sender: TObject);
    procedure EditEPOffsetXChange(Sender: TObject);
    procedure EditPPOffsetXChange(Sender: TObject);
    procedure EditEPAngleChange(Sender: TObject);
    procedure EditPPAngleChange(Sender: TObject);
    procedure EditSPSizeChange(Sender: TObject);
    procedure EditEPWidthChange(Sender: TObject);
    procedure EditEPHeightChange(Sender: TObject);
  private
    { Private declarations }

    m_Symbol: ISymbol;
    m_pSimplePointSymbol: ISimplePointSymbol;
    m_pEnvelopePointSymbol: IEnvelopePointSymbol;
    m_pPolyPointSymbol: IPolyPointSymbol;

    m_InProgress: Boolean;
  public
    { Public declarations }
    SymbolModified: TEVENTSymbolModified;

    procedure SetSymbol(pSymbol: ISymbol);
    procedure RefreshStuff();
  end;

implementation

{$R *.DFM}

procedure TSubSymbolAttrib.SetSymbol(pSymbol: ISymbol);
begin
    m_pSimplePointSymbol := nil;
    m_pEnvelopePointSymbol := nil;
    m_pPolyPointSymbol := nil;
    m_Symbol := pSymbol;
    Self.RefreshStuff();
end;

procedure TSubSymbolAttrib.RefreshStuff();
var
    size, angle: Double;
    offset: WKSPoint;
    color: COLORREF;
    colorlock: Boolean;
    symbolname: AnsiString;
begin
    if (not Self.Visible) then Exit;

    m_pSimplePointSymbol := nil;
    m_pEnvelopePointSymbol := nil;
    m_pPolyPointSymbol := nil;

    PanelSimplePoint.Visible := False;
    PanelEnvelopePoint.Visible := False;
    PanelShapePoint.Visible := False;
    Self.Caption := '图元属性';

    if (not Assigned(m_Symbol)) then begin
        Exit;
    end;

    m_InProgress := True;

    m_Symbol.GetColor(color);
    PanelSPColor.Color := color;
    PanelEPColor.Color := color;
    PanelPPColor.Color := color;
    m_Symbol.GetColorLock(colorlock);
    CBSPLockColor.Checked := colorlock;
    CBEPLockColor.Checked := colorlock;
    CBPPLockColor.Checked := colorlock;
    symbolname := m_Symbol.GetName();

    m_pSimplePointSymbol := ISimplePointSymbol(GotoInterface(m_Symbol, 'ISimplePointSymbol'));
    if (Assigned(m_pSimplePointSymbol)) then begin
        m_pSimplePointSymbol.GetDiameter(size);
        EditSPSize.Text := FloatToStr(size);
        m_pSimplePointSymbol.GetLineWidth(size);
        EditSPLineWidth.Text := FloatToStr(size);
        m_pSimplePointSymbol.GetOffset(offset.x, offset.y);
        EditSPOffsetX.Text := FloatToStr(offset.x);
        EditSPOffsetY.Text := FloatToStr(offset.y);

        PanelSimplePoint.Visible := True;
        Self.Caption := '图元属性 - ' + symbolname;
        m_InProgress := False;
        Exit;
    end;

    m_pEnvelopePointSymbol := IEnvelopePointSymbol(GotoInterface(m_Symbol, 'IEnvelopePointSymbol'));
    if (Assigned(m_pEnvelopePointSymbol)) then begin
        m_pEnvelopePointSymbol.GetOffset(offset.x, offset.y);
        EditEPOffsetX.Text := FloatToStr(offset.x);
        EditEPOffsetY.Text := FloatToStr(offset.y);
        m_pEnvelopePointSymbol.GetAngle(angle);
        EditEPAngle.Text := FloatToStr(angle);
        m_pEnvelopePointSymbol.GetWidth(offset.x);
        m_pEnvelopePointSymbol.GetHeight(offset.y);
        EditEPWidth.Text := FloatToStr(offset.x);
        EditEPHeight.Text := FloatToStr(offset.y);
        m_pEnvelopePointSymbol.GetLineWidth(size);
        EditEPLineWidth.Text := FloatToStr(size);

        PanelEnvelopePoint.Visible := True;
        Self.Caption := '图元属性 - ' + symbolname;
        m_InProgress := False;
        Exit;
    end;

    m_pPolyPointSymbol := IPolyPointSymbol(GotoInterface(m_Symbol, 'IPolyPointSymbol'));
    if (Assigned(m_pPolyPointSymbol)) then begin
        m_pPolyPointSymbol.GetOffset(offset.x, offset.y);
        EditPPOffsetX.Text := FloatToStr(offset.x);
        EditPPOffsetY.Text := FloatToStr(offset.y);
        m_pPolyPointSymbol.GetAngle(angle);
        EditPPAngle.Text := FloatToStr(angle);
        m_pPolyPointSymbol.GetLineWidth(size);
        EditPPLineWidth.Text := FloatToStr(size);

        PanelShapePoint.Visible := True;
        Self.Caption := '图元属性 - ' + symbolname;
        m_InProgress := False;
        Exit;
    end;

end;

procedure TSubSymbolAttrib.FormShow(Sender: TObject);
begin
    PanelSimplePoint.Align := alClient;
    PanelEnvelopePoint.Align := alClient;
    PanelShapePoint.Align := alClient;

    Self.RefreshStuff();
end;

procedure TSubSymbolAttrib.FormCreate(Sender: TObject);
begin
    m_Symbol := nil;
    m_InProgress := False;
end;

procedure TSubSymbolAttrib.PanelSPColorClick(Sender: TObject);
begin
    ColorDialog1.Color := PanelSPColor.Color;
    if (ColorDialog1.Execute()) then begin
        PanelSPColor.Color := ColorDialog1.Color;
    end;

    m_pSimplePointSymbol.SetColor(PanelSPColor.Color);
    Self.SymbolModified();
end;

procedure TSubSymbolAttrib.PanelEPColorClick(Sender: TObject);
begin
    ColorDialog1.Color := PanelEPColor.Color;
    if (ColorDialog1.Execute()) then begin
        PanelEPColor.Color := ColorDialog1.Color;
    end;

    m_pEnvelopePointSymbol.SetColor(PanelEPColor.Color);
    Self.SymbolModified();
end;

procedure TSubSymbolAttrib.PanelPPColorClick(Sender: TObject);
begin
    ColorDialog1.Color := PanelPPColor.Color;
    if (ColorDialog1.Execute()) then begin
        PanelPPColor.Color := ColorDialog1.Color;
    end;

    m_pPolyPointSymbol.SetColor(PanelPPColor.Color);
    Self.SymbolModified();
end;

procedure TSubSymbolAttrib.CBSPLockColorClick(Sender: TObject);
begin
    if (m_InProgress) then Exit;
    m_pSimplePointSymbol.SetColorLock(CBSPLockColor.Checked);
    Self.SymbolModified();
end;

procedure TSubSymbolAttrib.CBEPLockColorClick(Sender: TObject);
begin
    if (m_InProgress) then Exit;
    m_pEnvelopePointSymbol.SetColorLock(CBEPLockColor.Checked);
    Self.SymbolModified();
end;

procedure TSubSymbolAttrib.CBPPLockColorClick(Sender: TObject);
begin
    if (m_InProgress) then Exit;
    m_pPolyPointSymbol.SetColorLock(CBPPLockColor.Checked);
    Self.SymbolModified();
end;

procedure TSubSymbolAttrib.EditSPLineWidthChange(Sender: TObject);
var
    linewidth: Double;
begin
    if (m_InProgress) then Exit;
    try
        linewidth := StrToFloat(EditSPLineWidth.Text);
        m_pSimplePointSymbol.SetLineWidth(linewidth);
        Self.SymbolModified();
    except
    end;
end;

procedure TSubSymbolAttrib.EditEPLineWidthChange(Sender: TObject);
var
    linewidth: Double;
begin
    if (m_InProgress) then Exit;
    try
        linewidth := StrToFloat(EditEPLineWidth.Text);
        m_pEnvelopePointSymbol.SetLineWidth(linewidth);
        Self.SymbolModified();
    except
    end;
end;

procedure TSubSymbolAttrib.EditPPLineWidthChange(Sender: TObject);
var
    linewidth: Double;
begin
    if (m_InProgress) then Exit;
    try
        linewidth := StrToFloat(EditPPLineWidth.Text);
        m_pPolyPointSymbol.SetLineWidth(linewidth);
        Self.SymbolModified();
    except
    end;
end;

procedure TSubSymbolAttrib.EditSPOffsetXChange(Sender: TObject);
var
    OffsetX, OffsetY: Double;
begin
    if (m_InProgress) then Exit;
    try
        OffsetX := StrToFloat(EditSPOffsetX.Text);
        OffsetY := StrToFloat(EditSPOffsetY.Text);
        m_pSimplePointSymbol.SetOffset(OffsetX, OffsetY);
        Self.SymbolModified();
    except
    end;
end;

procedure TSubSymbolAttrib.EditEPOffsetXChange(Sender: TObject);
var
    OffsetX, OffsetY: Double;
begin
    if (m_InProgress) then Exit;
    try
        OffsetX := StrToFloat(EditEPOffsetX.Text);
        OffsetY := StrToFloat(EditEPOffsetY.Text);
        m_pEnvelopePointSymbol.SetOffset(OffsetX, OffsetY);
        Self.SymbolModified();
    except
    end;
end;

procedure TSubSymbolAttrib.EditPPOffsetXChange(Sender: TObject);
var
    OffsetX, OffsetY: Double;
begin
    if (m_InProgress) then Exit;
    try
        OffsetX := StrToFloat(EditPPOffsetX.Text);
        OffsetY := StrToFloat(EditPPOffsetX.Text);
        m_pPolyPointSymbol.SetOffset(OffsetX, OffsetY);
        Self.SymbolModified();
    except
    end;
end;

procedure TSubSymbolAttrib.EditEPAngleChange(Sender: TObject);
var
    angle: Double;
begin
    if (m_InProgress) then Exit;
    try
        angle := StrToFloat(EditEPAngle.Text);
        m_pEnvelopePointSymbol.SetAngle(angle);
        Self.SymbolModified();
    except
    end;
end;

procedure TSubSymbolAttrib.EditPPAngleChange(Sender: TObject);
var
    angle: Double;
begin
    if (m_InProgress) then Exit;
    try
        angle := StrToFloat(EditPPAngle.Text);
        m_pPolyPointSymbol.SetAngle(angle);
        Self.SymbolModified();
    except
    end;
end;

procedure TSubSymbolAttrib.EditSPSizeChange(Sender: TObject);
var
    diameter: Double;
begin
    if (m_InProgress) then Exit;
    try
        diameter := StrToFloat(EditSPSize.Text);
        m_pSimplePointSymbol.SetDiameter(diameter);
        Self.SymbolModified();
    except
    end;
end;

procedure TSubSymbolAttrib.EditEPWidthChange(Sender: TObject);
var
    width: Double;
begin
    if (m_InProgress) then Exit;
    try
        width := StrToFloat(EditEPWidth.Text);
        m_pEnvelopePointSymbol.SetWidth(width);
        Self.SymbolModified();
    except
    end;
end;

procedure TSubSymbolAttrib.EditEPHeightChange(Sender: TObject);
var
    height: Double;
begin
    if (m_InProgress) then Exit;
    try
        height := StrToFloat(EditEPHeight.Text);
        m_pEnvelopePointSymbol.SetHeight(height);
        Self.SymbolModified();
    except
    end;
end;

end.
