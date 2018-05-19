unit slimrendererui;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, easylib, InterfaceObj, InterfaceDisplay,
  InterfaceLayer, InterfaceLayerAgent, InterfaceFields, InterfaceGeometry,
  InterfaceDisplayTransformation, WKSStructs, selectuniquevalue, ImgList;

type
    TSymbolUISetMainWnd = procedure(wnd: HWND); stdcall;
    TSymbolUISelectSymbol = function(var pSymbol: ISymbol): Boolean; stdcall;

type
  TFormRendererUI = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    GBRendererType: TGroupBox;
    CBUniqueValue: TCheckBox;
    CBGrade: TCheckBox;
    PanelRendererBase: TPanel;
    PanelGradeTop: TPanel;
    PanelUVTop: TPanel;
    GroupBox1: TGroupBox;
    CBField: TComboBox;
    PanelSymbolMap: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    PanelBottom: TPanel;
    Button5: TButton;
    Button6: TButton;
    LVItems: TListView;
    GroupBox2: TGroupBox;
    CBColorUV: TComboBox;
    Timer1: TTimer;
    ImageList1: TImageList;
    GroupBox3: TGroupBox;
    CBShowDefaultSymbol: TCheckBox;
    PanelDefaultSymbol: TPanel;
    BtnDefaultSymbol: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PanelDefaultSymbolMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure CBUniqueValueClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CBGradeClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure LVItemsAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure CBFieldChange(Sender: TObject);
    procedure BtnDefaultSymbolClick(Sender: TObject);
    procedure LVItemsDblClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    m_pDefaultSymbol: ISymbol;
    m_pTemplateSymbol: ISymbol;
    m_pFields: IFields;
    m_SymbolMap: IStringMapObj;
    m_pDefaultSymbolDisplay: IDisplay;
    m_pSymbolListDisplay: IDisplay;

    procedure AddSymbolToMap(key: AnsiString; pSymbol: ISymbol);
    procedure PrepareDefaultSymbolDisplay();
    procedure DefaultSymbolPreview();
    procedure PrepareSymbolListDisplay(rect: TRect);
    procedure SymbolListDraw(const rect: TRect; const pSymbol: ISymbol);       
    procedure RefreshItemList();
    function GetRandomColorSymbol(): ISymbol;

  public
    { Public declarations }

    m_pVLA: IVectorLayerAgent;
    m_OK: Boolean;

  end;

var
    //symbolui
    hSymbolUI: HMODULE;
    SymbolUISetMainWnd: TSymbolUISetMainWnd;
    SymbolUISelectSymbol: TSymbolUISelectSymbol;

implementation

{$R *.DFM}


procedure TFormRendererUI.FormShow(Sender: TObject);
var
    i, fieldcount, count: Longint;
    pField: IField;
    fieldname: AnsiString;
    pSymbol: ISymbol;
    pObj: IObj;
    dc: HDC;
    renderertype: TSlimRendererType;
    pKey: IAnsiString;
    sKey: AnsiString;
begin
    m_OK := False;
    PanelUVTop.Align := alTop;
    PanelGradeTop.Align := alTop;
    PanelSymbolMap.Align := alClient;
    LVItems.Align := alClient;
    PanelRendererBase.Visible := False;
    CBField.Items.Clear();
    LVItems.Items.Clear();
    CBUniqueValue.Checked := False;
    CBGrade.Checked := False;
    m_SymbolMap.Clear();
    Self.RefreshItemList();
    if (CBColorUV.ItemIndex < 0) then begin
        CBColorUV.ItemIndex := 0;
    end;

    dc := GetDC(LVItems.Handle);
    m_pSymbolListDisplay.SetDC(dc);

    if (Assigned(m_pVLA)) then begin
        CBShowDefaultSymbol.Checked := m_pVLA.GetShowDefaultSymbol();
        m_pVLA.GetDefaultSymbol(pSymbol);
        pObj := nil;
        pSymbol.Clone(pObj);
        m_pDefaultSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
        pObj := nil;
        pSymbol.Clone(pObj);
        m_pTemplateSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
        m_pVLA.GetFields(m_pFields);
        fieldcount := m_pFields.GetFieldCount();
        for i := 0 to fieldcount - 1 do begin
            m_pFields.GetField(i, pField);
            fieldname := pField.GetFieldName();
            CBField.Items.Add(fieldname);
        end;

        if (m_pVLA.GetRendererField() >= 0) then begin
            CBField.ItemIndex := m_pVLA.GetRendererField();
        end
        else begin
            CBField.ItemIndex := -1;
        end;

        renderertype := m_pVLA.GetRendererType();
        if (renderertype = SLIMRENDERERTYPE_UNIQUEVALUE) then begin
            CBUniqueValue.Checked := True;
        end
        else if (renderertype = SLIMRENDERERTYPE_GRADE) then begin
            CBGrade.Checked := True;
        end;

        count := m_pVLA.GetSymbolCount();
        for i := 0 to count - 1 do begin
            pKey := nil;
            pSymbol := nil;
            m_pVLA.GetSymbolByIndex(i, pKey, pSymbol);
            sKey := pKey.GetText();
            Self.AddSymbolToMap(PChar(sKey), pSymbol);
        end;

        Self.RefreshItemList();

        Self.PrepareDefaultSymbolDisplay();
    end;

end;

procedure TFormRendererUI.AddSymbolToMap(key: AnsiString; pSymbol: ISymbol);
var
    pObj: IObj;
begin
    pObj := IStringMapObj(GotoInterface(pSymbol, 'ISymbol'));
    m_SymbolMap._Set(PChar(key), pObj);
end;

procedure TFormRendererUI.FormCreate(Sender: TObject);
var
    pObj: IObj;
begin
    CreateObj('CStringMapObj', pObj);
    m_SymbolMap := IStringMapObj(GotoInterface(pObj, 'IStringMapObj'));

    CreateObj('CMiniDisplay', pObj);
    m_pDefaultSymbolDisplay := IDisplay(GotoInterface(pObj, 'IDisplay'));

    CreateObj('CMiniDisplay', pObj);
    m_pSymbolListDisplay := IDisplay(GotoInterface(pObj, 'IDisplay'));
    
end;

procedure TFormRendererUI.PrepareDefaultSymbolDisplay();
var
    dc: HDC;
    rect: TRect;
    pTrans: IDisplayTransformation;
    f: Double;
begin
    m_pDefaultSymbolDisplay.GetDisplayTransformation(pTrans);
    pTrans.SetMapUnit(UNIT_MM);
    dc := GetDC(PanelDefaultSymbol.Handle);
    m_pDefaultSymbolDisplay.SetDC(dc);
    rect.Left := 0;
    rect.Right := PanelDefaultSymbol.Width;
    rect.Top := 0;
    rect.Bottom := PanelDefaultSymbol.Height;
    pTrans.SetDeviceRect(rect);
    f := 1;
    pTrans.SetMapScale(f);
    f := 1;
    pTrans.SetReferenceScale(f);
    m_pDefaultSymbolDisplay.SetBackgroundColor(COLORREF($00F3F3F3));
end;

procedure TFormRendererUI.DefaultSymbolPreview();
var
    x, y: Double;
    pPnt: IPoint;
    pLine: IPolyline;
    pPath: IPath;
    pPolygon: IPolygon;
    pRing: IRing;
    pGeo: IGeometry;
    wkspnt: WKSPoint;
    wkspntz: WKSPointZ;
    pnt: tagPOINT;
    rect: TRect;
    pTrans: IDisplayTransformation;
    symboltype: TSymbolType;
begin
    PanelDefaultSymbol.Refresh();
    
    if (not Assigned(m_pDefaultSymbolDisplay)) then begin
        Exit;
    end;

    if (not Assigned(m_pDefaultSymbol)) then begin
        Exit;
    end;

    m_pDefaultSymbolDisplay.SetSymbol(m_pDefaultSymbol);
    m_pDefaultSymbolDisplay.GetDisplayTransformation(pTrans);
    m_pDefaultSymbolDisplay.GetRect(rect);


    CreateGeometry(GEOMETRYTYPE_POINT, pGeo);
    pPnt := IPoint(GotoInterface(pGeo, 'IPoint'));
    pnt.x := (rect.Right + rect.Left) div 2;
    pnt.y := (rect.Top + rect.Bottom) div 2;
    pTrans.Device2Map(pnt, wkspnt);
    pPnt.SetX(wkspnt.x);
    pPnt.SetY(wkspnt.y);

    CreateGeometry(GEOMETRYTYPE_PATH, pGeo);
    pPath := IPath(GotoInterface(pGeo, 'IPath'));
    pnt.x := rect.Left + 3;
    pnt.y := (rect.Top + rect.Bottom) div 2;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pPath.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Right - 3;
    pnt.y := (rect.Top + rect.Bottom) div 2;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pPath.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    CreateGeometry(GEOMETRYTYPE_POLYLINE, pGeo);
    pLine := IPolyline(GotoInterface(pGeo, 'IPolyline'));
    pLine.AddPathRef(pPath);

    CreateGeometry(GEOMETRYTYPE_RING, pGeo);
    pRing := IRing(GotoInterface(pGeo, 'IRing'));
    pnt.x := rect.Left + 5;
    pnt.y := rect.Top + 5;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    x := wkspntz.x;
    y := wkspntz.y;
    pRing.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Right - 5;
    pnt.y := rect.Top + 5;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pRing.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Right - 5;
    pnt.y := rect.Bottom - 5;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pRing.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Left + 5;
    pnt.y := rect.Bottom - 5;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pRing.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Left + 5;
    pnt.y := rect.Top + 5;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pRing.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    CreateGeometry(GEOMETRYTYPE_POLYGON, pGeo);
    pPolygon := IPolygon(GotoInterface(pGeo, 'IPolygon'));
    pPolygon.AddRingRef(pRing);

    m_pDefaultSymbolDisplay.StartDraw();
    symboltype :=  m_pDefaultSymbol.GetSymbolType();
    if (symboltype = SYMBOLTYPE_POINT) then begin
        m_pDefaultSymbolDisplay.DrawGeometry(pPnt);
    end
    else if (symboltype = SYMBOLTYPE_LINE) then begin
        m_pDefaultSymbolDisplay.DrawGeometry(pLine);
    end
    else if (symboltype = SYMBOLTYPE_FILL) then begin
        m_pDefaultSymbolDisplay.DrawGeometry(pPolygon);
    end
    else begin
        m_pDefaultSymbolDisplay.DrawTextXY(x, y, '毛猫', rect);
    end;
    m_pDefaultSymbolDisplay.FinishDraw();
    m_pDefaultSymbolDisplay.RefreshWindow1();
end;

procedure TFormRendererUI.FormPaint(Sender: TObject);
begin
    Self.DefaultSymbolPreview();
    m_pDefaultSymbolDisplay.RefreshWindow1();
end;

procedure TFormRendererUI.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    dc: HDC;
begin
    m_pDefaultSymbolDisplay.GetDC(dc);
    ReleaseDC(PanelDefaultSymbol.Handle, dc);

    Action := caHide;
end;

procedure TFormRendererUI.PanelDefaultSymbolMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
    Self.DefaultSymbolPreview();
end;

procedure TFormRendererUI.CBUniqueValueClick(Sender: TObject);
begin
    PanelRendererBase.Visible := False;
    if (CBUniqueValue.Checked) then begin
        CBGrade.Checked := False;
        PanelRendererBase.Visible := True;
        PanelUVTop.Visible := True;
        PanelGradeTop.Visible := False;
    end;

    m_SymbolMap.Clear();
    LVItems.Items.Clear();
end;

procedure TFormRendererUI.Timer1Timer(Sender: TObject);
begin
    Self.DefaultSymbolPreview();
end;

procedure TFormRendererUI.CBGradeClick(Sender: TObject);
begin
    PanelRendererBase.Visible := False;
    if (CBGrade.Checked) then begin
        CBUniqueValue.Checked := False;
        PanelRendererBase.Visible := True;
        PanelUVTop.Visible := False;
        PanelGradeTop.Visible := True;
    end;

    m_SymbolMap.Clear();
    LVItems.Items.Clear();
end;

procedure TFormRendererUI.RefreshItemList();
var
    i, count: Longint;
    key: IAnsiString;
    sKey: AnsiString;
    pObj: IObj;
    item: TListItem;
begin
    LVItems.Items.Clear();
    count := m_SymbolMap.GetSize();
    for i := 0 to count - 1 do begin
        m_SymbolMap.GetAt(i, key, pObj);
        sKey := key.GetText();
        item := LVItems.Items.Add();
        item.Caption := sKey;
    end;
end;

function FieldValue2String(const pFieldValue: IFieldValue): AnsiString;
var
    n: Longint;
    f: Double;
begin
    case pFieldValue.GetFieldType() of
        FIELDTYPE_SHORT,
        FIELDTYPE_LONG: begin
            pFieldValue.GetInteger(n);
            Result := IntToStr(n);
        end;

        FIELDTYPE_SINGLE,
        FIELDTYPE_DOUBLE: begin
            pFieldValue.GetFloat(f);
            Result := FloatToStr(f);
        end;

        FIELDTYPE_STRING: begin
            Result := pFieldValue.GetText();
        end;

        else begin
            Result := '死了';
        end;
    end;
end;

function TFormRendererUI.GetRandomColorSymbol(): ISymbol;
var
    pObj: IObj;
    color: TColor;
begin
    m_pTemplateSymbol.Clone(pObj);
    Result := ISymbol(GotoInterface(pObj, 'ISymbol'));
    color := RGB(Random(255), Random(255), Random(255));
    Result.SetColor(color);
end;

procedure TFormRendererUI.Button3Click(Sender: TObject);
var
    pFids: IIntArray;
    i, count, fid: Longint;
    pFeature: IVectorFeature;
    pFieldValue: IFieldValue;
    svalue: AnsiString;
begin
    m_SymbolMap.Clear();
    LVItems.Items.Clear();

    m_pVLA.GetFids(pFids);
    count := pFids.GetSize();
    for i := 0 to count - 1 do begin
        pFids.GetAt(i, fid);
        m_pVLA.GetFeature(fid, pFeature);
        if (CBField.ItemIndex >= 0) then begin
            pFeature.GetFieldValue(CBField.ItemIndex, pFieldValue);
            svalue := FieldValue2String(pFieldValue);
        end
        else begin
            svalue := IntToStr(pFeature.GetFID());
        end;

        m_SymbolMap._Set(PChar(svalue), Self.GetRandomColorSymbol());
    end;

    Self.RefreshItemList();
end;

procedure TFormRendererUI.Button4Click(Sender: TObject);
begin
    m_SymbolMap.Clear();
    LVItems.Items.Clear();
end;

procedure TFormRendererUI.LVItemsAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
    pKey: IAnsiString;
    pValue: IObj;
    pSymbol: ISymbol;
    index: Longint;
    rect: TRect;
begin
    rect := Item.DisplayRect(drIcon);
    index := Item.Index;

    m_SymbolMap.GetAt(index, pKey, pValue);
    pSymbol := ISymbol(GotoInterface(pValue, 'ISymbol'));

    Self.PrepareSymbolListDisplay(rect);
    Self.SymbolListDraw(rect, pSymbol);
end;

procedure TFormRendererUI.PrepareSymbolListDisplay(rect: TRect);
var
    pTrans: IDisplayTransformation;
begin
    m_pSymbolListDisplay.GetDisplayTransformation(pTrans);
    pTrans.SetMapUnit(UNIT_MM);

    pTrans.SetDeviceRect(rect);
    pTrans.SetMapScale(1.0);
    pTrans.SetReferenceScale(1.0);
    m_pSymbolListDisplay.SetBackgroundColor(clWhite);
end;

function _DrawEnvelope(dc: HDC; envelope: TRect; linecolor: COLORREF;
    linewidth: Longint; hollow: Boolean; fillcolor: COLORREF): Boolean; overload;
var
    tagBrush: tagLOGBRUSH;
    BrushNew, BrushOld: HBRUSH;
    PenNew, PenOld: HPEN;
begin

    PenNew := CreatePen(PS_SOLID, linewidth, linecolor);
    PenOld := SelectObject(dc, PenNew);

    if (hollow) then begin
        tagBrush.lbStyle := BS_HOLLOW;
    end
    else begin
        tagBrush.lbStyle := BS_SOLID;
        tagBrush.lbColor := fillcolor
    end;
    BrushNew := CreateBrushIndirect(tagBrush);
    BrushOld := SelectObject(dc, BrushNew);

    Rectangle(dc, envelope.Left, envelope.Top, envelope.Right, envelope.Bottom);

    //恢复以前的hdc
    SelectObject(dc, BrushOld);
    SelectObject(dc, PenOld);
    DeleteObject(BrushNew);
    DeleteObject(PenNew);

    Result := True;
end;

procedure TFormRendererUI.SymbolListDraw(const rect: TRect; const pSymbol: ISymbol);
var
    pPnt: IPoint;
    pLine: IPolyline;
    pPath: IPath;
    pPolygon: IPolygon;
    pRing: IRing;
    pGeo: IGeometry;
    wkspnt: WKSPoint;
    wkspntz: WKSPointZ;
    pnt: tagPOINT;
    pTrans: IDisplayTransformation;
    pDrawSymbol: ISymbol;
    pObj: IObj;
    drawsymtype: TSymbolType;
    dc: HDC;
    recttmp: TRect;
begin
    recttmp.Left := rect.Left + 2;
    recttmp.Top := rect.Top + 2;
    recttmp.Right := rect.Right - 1;
    recttmp.Bottom := rect.Bottom - 1;
    m_pSymbolListDisplay.GetDC(dc);
    _DrawEnvelope(dc, recttmp, $00EFE0E0, 1, True, clWhite);

    if (not Assigned(pSymbol)) then Exit;
    pSymbol.Clone(pObj);
    pDrawSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));

    m_pSymbolListDisplay.SetSymbol(pDrawSymbol);
    m_pSymbolListDisplay.GetDisplayTransformation(pTrans);

    CreateGeometry(GEOMETRYTYPE_POINT, pGeo);
    pPnt := IPoint(GotoInterface(pGeo, 'IPoint'));
    pnt.x := (rect.Right + rect.Left) div 2;
    pnt.y := (rect.Top + rect.Bottom) div 2;
    pTrans.Device2Map(pnt, wkspnt);
    pPnt.SetX(wkspnt.x);
    pPnt.SetY(wkspnt.y);

    CreateGeometry(GEOMETRYTYPE_PATH, pGeo);
    pPath := IPath(GotoInterface(pGeo, 'IPath'));
    pnt.x := rect.Left + 5;
    pnt.y := (rect.Top + rect.Bottom) div 2;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pPath.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Right - 5;
    pnt.y := (rect.Top + rect.Bottom) div 2;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pPath.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    CreateGeometry(GEOMETRYTYPE_POLYLINE, pGeo);
    pLine := IPolyline(GotoInterface(pGeo, 'IPolyline'));
    pLine.AddPathRef(pPath);

    CreateGeometry(GEOMETRYTYPE_RING, pGeo);
    pRing := IRing(GotoInterface(pGeo, 'IRing'));
    pnt.x := rect.Left + 10;
    pnt.y := rect.Top + 10;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pRing.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Right - 10;
    pnt.y := rect.Top + 10;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pRing.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Right - 10;
    pnt.y := rect.Bottom - 10;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pRing.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Left + 10;
    pnt.y := rect.Bottom - 10;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pRing.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Left + 10;
    pnt.y := rect.Top + 10;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pRing.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    CreateGeometry(GEOMETRYTYPE_POLYGON, pGeo);
    pPolygon := IPolygon(GotoInterface(pGeo, 'IPolygon'));
    pPolygon.AddRingRef(pRing);

    m_pSymbolListDisplay.StartDraw();
    drawsymtype := pDrawSymbol.GetSymbolType();
    case drawsymtype of
        SYMBOLTYPE_POINT: m_pSymbolListDisplay.DrawGeometry(pPnt);
        SYMBOLTYPE_LINE: m_pSymbolListDisplay.DrawGeometry(pLine);
        SYMBOLTYPE_FILL: m_pSymbolListDisplay.DrawGeometry(pPolygon);
    end;
    m_pSymbolListDisplay.FinishDraw();
    m_pSymbolListDisplay.RefreshWindow1();
end;

procedure TFormRendererUI.Button1Click(Sender: TObject);
var
    i, count: Longint;
    pKey: IAnsiString;
    sKey: AnsiString;
    pObj: IObj;
    pSymbol: ISymbol;
    color: COLORREF;
begin
    if (nil = @SymbolUISelectSymbol) then begin
        ShowMessage('no symbolui.dll');
        Exit;
    end;

    SymbolUISelectSymbol(m_pTemplateSymbol);

    count := m_SymbolMap.GetSize();
    for i := 0 to count - 1 do begin
        pObj := nil;
        m_SymbolMap.GetAt(i, pKey, pObj);
        pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
        pSymbol.GetColor(color);
        pObj := nil;
        m_pTemplateSymbol.Clone(pObj);
        pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
        pSymbol.SetColor(color);
        pObj := IObj(GotoInterface(pSymbol, 'IObj'));
        sKey := pKey.GetText();
        m_SymbolMap._Set(PChar(sKey), pObj);
    end;
end;

procedure TFormRendererUI.CBFieldChange(Sender: TObject);
begin
    m_SymbolMap.Clear();
    LVItems.Items.Clear();
end;

procedure TFormRendererUI.BtnDefaultSymbolClick(Sender: TObject);
begin
    if (nil = @SymbolUISelectSymbol) then begin
        ShowMessage('no symbolui.dll');
        Exit;
    end;

    SymbolUISelectSymbol(m_pDefaultSymbol);
end;

procedure TFormRendererUI.LVItemsDblClick(Sender: TObject);
var
    pKey: IAnsiString;
    sKey: AnsiString;
    pObj: IObj;
    pSymbol: ISymbol;
begin
    if (not Assigned(LVItems.Selected)) then begin
        Exit;
    end;

    if (nil = @SymbolUISelectSymbol) then begin
        ShowMessage('no symbolui.dll');
        Exit;
    end;

    m_SymbolMap.GetAt(LVItems.Selected.Index, pKey, pObj);
    pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
    SymbolUISelectSymbol(pSymbol);
    sKey := pKey.GetText();
    m_SymbolMap._Set(PChar(sKey), pSymbol);
    LVItems.Refresh();
end;

procedure TFormRendererUI.Button5Click(Sender: TObject);
var
    renderertype: TSlimRendererType;
    pKey: IAnsiString;
    sKey: AnsiString;
    pObj: IObj;
    pSymbol: ISymbol;
    i, count: Longint;
begin
    if (not Assigned(m_pVLA)) then begin
        Exit;
    end;

    renderertype := SLIMRENDERERTYPE_SIMPLE;
    if (CBUniqueValue.Checked) then begin
        renderertype := SLIMRENDERERTYPE_UNIQUEVALUE;
    end;
    if (CBGrade.Checked) then begin
        renderertype := SLIMRENDERERTYPE_GRADE;
    end;

    m_pVLA.SetDefaultSymbol(m_pDefaultSymbol);
    m_pVLA.SetRendererType(renderertype);
    m_pVLA.SetRendererField(CBField.ItemIndex);
    m_pVLA.ClearSymbol();
    count := m_SymbolMap.GetSize();
    for i := 0 to count - 1 do begin
        pObj := nil;
        m_SymbolMap.GetAt(i, pKey, pObj);
        sKey := pKey.GetText();
        pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
        m_pVLA.SetSymbol(PChar(sKey), pSymbol);
    end;
    m_pVLA.SetShowDefaultSymbol(CBShowDefaultSymbol.Checked);

    m_OK := True;
    Self.Close();
end;

procedure TFormRendererUI.Button6Click(Sender: TObject);
begin
    Self.Close();
end;

procedure TFormRendererUI.Button2Click(Sender: TObject);
var
    selectuv: TFormSelectUniqueValue;
    pFids: IIntArray;
    i, count, fid: Longint;
    pFeature: IVectorFeature;
    pFieldValue: IFieldValue;
    svalue: AnsiString;
    item: TListItem;
    pStringMap: IStringMapObj;
    pObj: IObj;
    pKey: IAnsiString;
begin
    selectuv := TFormSelectUniqueValue.Create(Self);
    CreateObj('CStringMapObj', pObj);
    pStringMap := IStringMapObj(GotoInterface(pObj, 'IStringMapObj'));

    m_pVLA.GetFids(pFids);
    count := pFids.GetSize();
    for i := 0 to count - 1 do begin
        pFids.GetAt(i, fid);
        m_pVLA.GetFeature(fid, pFeature);
        if (CBField.ItemIndex >= 0) then begin
            pFeature.GetFieldValue(CBField.ItemIndex, pFieldValue);
            svalue := FieldValue2String(pFieldValue);
        end
        else begin
            svalue := IntToStr(pFeature.GetFID());
        end;

        pStringMap._Set(PChar(svalue), nil);
    end;

    count := pStringMap.GetSize();
    for i := 0 to count - 1 do begin
        pKey := nil;
        pObj := nil;
        pStringMap.GetAt(i, pKey, pObj);
        svalue := pKey.GetText();
        item := selectuv.ListView1.Items.Add();
        item.Caption := svalue;
    end;

    selectuv.ShowModal();
    if (selectuv.m_OK and (selectuv.ListView1.SelCount > 0)) then begin
        count := selectuv.ListView1.Items.Count;
        for i := 0 to count - 1 do begin
            if (selectuv.ListView1.Items.Item[i].Selected) then begin
                svalue := selectuv.ListView1.Items.Item[i].Caption;
                Self.AddSymbolToMap(svalue, Self.GetRandomColorSymbol());
            end;
        end;

        Self.RefreshItemList();
    end;
    
    selectuv.Free();
end;

initialization
    if (not InitialEasyLib('easylib.dll')) then begin
        MessageBox(0, 'no easylib.dll', '', MB_OK);
        Application.Terminate();
    end;

    hSymbolUI := LoadLibrary('symbolui.dll');
    if (0 <> hSymbolUI) then begin
        SymbolUISetMainWnd := TSymbolUISetMainWnd(GetProcAddress(hSymbolUI, 'SetMainWnd'));
        SymbolUISelectSymbol := TSymbolUISelectSymbol(GetProcAddress(hSymbolUI, 'SelectSymbol'));
    end
    else begin
        SymbolUISetMainWnd := nil;
        SymbolUISelectSymbol := nil;
    end;

finalization
    if (0 <> easylibmodule) then
        FreeLibrary(easylibmodule);
        
    FreeEasyLib();

end.
