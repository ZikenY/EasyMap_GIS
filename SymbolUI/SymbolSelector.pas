unit SymbolSelector;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, ImgList, easylib, InterfaceObj, InterfaceGeometry,
  InterfaceDisplayTransformation, InterfaceDisplay, InterfaceSymbol, WKSStructs,
  Buttons;

procedure SetMainWnd(wnd: HWND); stdcall;
function SelectSymbol(var pSymbol: ISymbol): Boolean; stdcall;

type
  TFormSymbolSelector = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    BTLoadSymbolLib: TButton;
    PanelFileList: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    LVSymbolList: TListView;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    ODSymbolLib: TOpenDialog;
    ImageList1: TImageList;
    Panel16: TPanel;
    BTOK: TButton;
    BTCancel: TButton;
    PanelSymbolPreview: TPanel;
    CBFileList: TComboBox;
    PanelSymbolProperty: TPanel;
    PanelPointProperty: TPanel;
    PanelLineProperty: TPanel;
    PanelPointColor: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    EditPointSize: TEdit;
    Label3: TLabel;
    EditPointAngle: TEdit;
    UpDown2: TUpDown;
    BTPointAttrib: TButton;
    PanelLineColor: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    EditLineWidth: TEdit;
    UpDown3: TUpDown;
    BTLineAttrib: TButton;
    ColorDialog1: TColorDialog;
    PanelFillProperty: TPanel;
    Label6: TLabel;
    PanelFillColor: TPanel;
    BTFillAttrib: TButton;
    TimerRefresh: TTimer;
    PanelTextProperty: TPanel;
    Label7: TLabel;
    PanelTextColor: TPanel;
    BTTextAttrib: TButton;
    Label8: TLabel;
    EditTextHeight: TEdit;
    UpDown4: TUpDown;
    Label9: TLabel;
    EditTextWidth: TEdit;
    UpDown5: TUpDown;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure BTLoadSymbolLibClick(Sender: TObject);
    procedure LVSymbolListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BTOKClick(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
    procedure LVSymbolListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure Panel3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel16MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PanelFileListResize(Sender: TObject);
    procedure CBFileListChange(Sender: TObject);
    procedure PanelPointPropertyMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPointColorClick(Sender: TObject);
    procedure PanelLineColorClick(Sender: TObject);
    procedure EditPointAngleChange(Sender: TObject);
    procedure EditPointSizeChange(Sender: TObject);
    procedure EditLineWidthChange(Sender: TObject);
    procedure TimerRefreshTimer(Sender: TObject);
    procedure PanelFillColorClick(Sender: TObject);
    procedure PanelTextColorClick(Sender: TObject);
    procedure EditTextHeightChange(Sender: TObject);
    procedure EditTextWidthChange(Sender: TObject);
    procedure BTPointAttribClick(Sender: TObject);
    procedure BTFillAttribClick(Sender: TObject);
    procedure BTLineAttribClick(Sender: TObject);
    procedure BTTextAttribClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }

    m_ListFile: AnsiString;
    m_SymbolType: TSymbolType;
    m_pSymbolLib: ISymbolLib;
    m_pSymbol: ISymbol;
    m_pSymbolListDisplay: IDisplay;
    m_pPreviewDisplay: IDisplay;
    m_Progress: Boolean;
    m_FormAlready: Boolean;

    procedure RefreshSymbolList();
    procedure PrepareSymbolListDisplay(rect: TRect);
    procedure SymbolListDraw(const rect: TRect; const pSymbol: ISymbol);
    procedure PreparePreviewDisplay();
    procedure SymbolPreviewDraw();
    procedure RefreshSymbolGot();
    function LoadLib(const filename: AnsiString): Boolean;
    function LoadSymbolLibFromFile(const filename: AnsiString): Boolean;
    procedure UpdateData2Form();

  public
    { Public declarations }

    function SetSymbolType(const symboltype: TSymbolType): Boolean;
    function SetSymbol(const pSymbol: ISymbol): Boolean;
    function GetSymbol(out pSymbol: ISymbol): Boolean;
    procedure ClearSymbols();
  end;

implementation

uses MainForm;

{$R *.DFM}

procedure SetMainWnd(wnd: HWND);
begin
    Application.Handle := wnd;
end;

function SelectSymbol(var pSymbol: ISymbol): Boolean;
var
    FormSymbolSelector: TFormSymbolSelector;
    pSymbolGot: ISymbol;
begin
    Result := False;
    if (not Assigned(pSymbol)) then Exit;
    FormSymbolSelector := TFormSymbolSelector.Create(nil);
    FormSymbolSelector.SetSymbol(pSymbol);
    FormSymbolSelector.ShowModal();
    FormSymbolSelector.GetSymbol(pSymbolGot);
    FormSymbolSelector.Free();
    if (Assigned(pSymbolGot)) then begin
        pSymbol := pSymbolGot;
        Result := True;
    end;
end;

function GetModuleDirectory(): AnsiString;
var
    pcs: array[0..1999] of Char;
    filename: AnsiString;
    i, size: Longint;
begin
    Result := '.';

    GetModuleFileName(easylibmodule, pcs, 2000);
    filename := Trim(AnsiString(pcs));
    size := Length(filename);
    for i := size - 1 downto 0 do begin
        if (filename[i] = '\') then begin
            Result := Copy(filename, 0, i-1);
            Break;
        end;
    end;
end;

function MakeUpper(str: AnsiString; AllowTrim: Boolean): AnsiString;
begin
    if (AllowTrim) then begin
        Result := AnsiString(StrUpper(PChar(Trim(str))));
    end
    else begin
        Result := AnsiString(StrUpper(PChar(str)));
    end;
end;

function MakeLower(str: AnsiString; AllowTrim: Boolean): AnsiString;
begin
    if (AllowTrim) then begin
        Result := AnsiString(StrLower(PChar(Trim(str))));
    end
    else begin
        Result := AnsiString(StrLower(PChar(str)));
    end;
end;

procedure TFormSymbolSelector.FormCreate(Sender: TObject);
var
    i, count: Longint;
    pObj: IObj;
    FindFileData: WIN32_FIND_DATA;
    strlist: TStringList;
    s: AnsiString;
    loadsuccess: Boolean;
begin
    m_FormAlready := False;
    m_Progress := False;

    CreateObj('CSymbolLib', pObj);
    m_pSymbolLib := ISymbolLib(GotoInterface(pObj, 'ISymbolLib'));

    CreateObj('CMiniDisplay', pObj);
    m_pSymbolListDisplay := IDisplay(GotoInterface(pObj, 'IDisplay'));

    CreateObj('CMiniDisplay', pObj);
    m_pPreviewDisplay := IDisplay(GotoInterface(pObj, 'IDisplay'));

    m_SymbolType := SYMBOLTYPE_POINT;
    m_pSymbol := nil;

    try
        m_ListFile := GetModuleDirectory() + '\symbolliblist.lst';
        if (FindFirstFile(PChar(m_ListFile), FindFileData) = INVALID_HANDLE_VALUE) then begin
            CBFileList.Items.SaveToFile(m_ListFile);
        end;

        strlist := TStringList.Create();
        strlist.LoadFromFile(m_ListFile);
        count := strlist.Count;
        for i := 0 to count - 1 do begin
            s := Trim(strlist.Strings[i]);
            if (s <> '') then begin
                CBFileList.Items.Add(s);
            end;
        end;
        strlist.Free();
        if (CBFileList.Items.Count > 0) then begin
            loadsuccess := Self.LoadSymbolLibFromFile(CBFileList.Items.Strings[0]);
            if (loadsuccess) then begin
                CBFileList.ItemIndex := 0;
            end
            else begin
                CBFileList.Items.Delete(0);
                CBFileList.Items.SaveToFile(m_ListFile);
            end;
        end;
    except
        m_ListFile := '';
    end;
end;

procedure TFormSymbolSelector.BTLoadSymbolLibClick(Sender: TObject);
var
    filename: AnsiString;
begin
    if (not ODSymbolLib.Execute()) then Exit;

    filename := Trim(ODSymbolLib.FileName);
    self.LoadLib(filename);

    if (LVSymbolList.Items.Count > 0) then begin
        LVSymbolList.Items[0].Selected := True;
    end;
end;

function TFormSymbolSelector.LoadLib(const filename: AnsiString): Boolean;
var
    i, count: Longint;
    s1, s2: AnsiString;
begin
    Result := Self.LoadSymbolLibFromFile(filename);

    count := CBFileList.Items.Count;
    for i := 0 to count - 1 do begin
        s1 := MakeUpper(CBFileList.Items.Strings[i], True);
        s2 := MakeUpper(filename, True);
        if (s1 = s2) then begin
            if (Result) then begin
                // 移到最上面
                CBFileList.Items.Delete(i);
                CBFileList.Items.Add(filename);
                CBFileList.Items.Move(CBFileList.Items.Count-1, 0);
                CBFileList.ItemIndex := 0;
                if (Trim(m_ListFile) <> '') then begin
                    CBFileList.Items.SaveToFile(m_ListFile);
                end;
                Exit;
            end
            else begin
                CBFileList.Items.Delete(i);
                Break;
            end;
        end;
    end;

    if (Result) then begin
        CBFileList.Items.Add(filename);
        CBFileList.Items.Move(CBFileList.Items.Count-1, 0);
        CBFileList.ItemIndex := 0;
    end;

    if (Trim(m_ListFile) <> '') then CBFileList.Items.SaveToFile(m_ListFile);
end;

procedure TFormSymbolSelector.RefreshSymbolList();
var
    i, symbolcount: Longint;
    pSymbol: ISymbol;
    item: TListItem;
    symbolname: AnsiString;
begin
    LVSymbolList.Items.Clear();
    symbolcount := m_pSymbolLib.GetSymbolCount();
    for i := 0 to symbolcount - 1 do begin
        m_pSymbolLib.GetSymbolRef(pSymbol, i);

        symbolname := pSymbol.GetName();
        item := LVSymbolList.Items.Add();
        item.Caption := symbolname;
    end;
end;

function TFormSymbolSelector.SetSymbolType(const symboltype: TSymbolType): Boolean;
begin
    Result := True;
    m_SymbolType := symboltype;
    if (Assigned(m_pSymbol)) then begin
        if (m_pSymbol.GetSymbolType() <> symboltype) then begin
            m_pSymbol := nil;
        end;
    end;

    if (not Self.LoadSymbolLibFromFile(CBFileList.Text)) then begin
        Self.ClearSymbols();
    end;

    PanelPointProperty.Visible := False;
    PanelLineProperty.Visible := False;
    PanelFillProperty.Visible := False;
    PanelTextProperty.Visible := False;
    if (SYMBOLTYPE_POINT = symboltype) then begin
        PanelPointProperty.Visible := True;
    end
    else if (SYMBOLTYPE_LINE = symboltype) then begin
        PanelLineProperty.Visible := True;
    end
    else if (SYMBOLTYPE_FILL = symboltype) then begin
        PanelFillProperty.Visible := True;
    end
    else if (SYMBOLTYPE_TEXT = symboltype) then begin
        PanelTextProperty.Visible := True;
    end
    else begin
        Result := False;
    end;

end;

function TFormSymbolSelector.SetSymbol(const pSymbol: ISymbol): Boolean;
var
    pObj: IObj;
begin
    Result := False;
    if (not Assigned(pSymbol)) then Exit;

    Self.SetSymbolType(pSymbol.GetSymbolType());
    pSymbol.Clone(pObj);
    m_pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
    Self.UpdateData2Form();
    if (m_FormAlready) then begin
        Self.SymbolPreviewDraw();
    end;
    Result := True;
end;

function TFormSymbolSelector.GetSymbol(out pSymbol: ISymbol): Boolean;
var
    pObj: IObj;
begin
    Result := False;
    pSymbol := nil;
    if (not Assigned(m_pSymbol)) then Exit;

    m_pSymbol.Clone(pObj);
    pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
    Result := True;
end;

procedure TFormSymbolSelector.LVSymbolListAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
    pSymbol: ISymbol;
    index: Longint;
    rect: TRect;
begin
    rect := Item.DisplayRect(drIcon);
    index := Item.Index;
    m_pSymbolLib.GetSymbolRef(pSymbol, index);
    if (not Assigned(pSymbol)) then Exit;

    Self.PrepareSymbolListDisplay(rect);
    Self.SymbolListDraw(rect, pSymbol);

end;

procedure TFormSymbolSelector.PrepareSymbolListDisplay(rect: TRect);
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

procedure TFormSymbolSelector.SymbolListDraw(const rect: TRect; const pSymbol: ISymbol);
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
    recttmp.Right := rect.Right - 2;
    recttmp.Bottom := rect.Bottom - 2;
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

procedure TFormSymbolSelector.UpdateData2Form();
label
    exitpoint;
var
    pMultiPointSymbol: IMultiPointSymbol;
    pMultiLineSymbol: IMultiLineSymbol;
    pMultiFillSymbol: IMultiFillSymbol;
    pPointSymbol: IPointSymbol;
    pLineSymbol: ILineSymbol;
    pFillSymbol: IFillSymbol;
    pSimpleTextSymbol: ISimpleTextSymbol;
    color: COLORREF;
    angle, size: Double;
begin
    if (not Assigned(m_pSymbol)) then Exit;
    m_Progress := True;
    m_pSymbol.GetColor(color);

    pMultiPointSymbol := IMultiPointSymbol(GotoInterface(m_pSymbol, 'IMultiPointSymbol'));
    if (Assigned(pMultiPointSymbol)) then begin
        PanelPointColor.Color := color;
        pMultiPointSymbol.GetSize(size);
        EditPointSize.Text := FloatToStr(size);
        pMultiPointSymbol.GetAngle(angle);
        EditPointAngle.Text := FloatToStr(angle);
        goto exitpoint;
    end;

    pPointSymbol := IPointSymbol(GotoInterface(m_pSymbol, 'IPointSymbol'));
    if (Assigned(pPointSymbol)) then begin
        CreateMultiPointSymbol(pMultiPointSymbol);
        pMultiPointSymbol.AddSymbol(pPointSymbol);
        m_pSymbol := pMultiPointSymbol;
        PanelPointColor.Color := color;
        pMultiPointSymbol.GetSize(size);
        EditPointSize.Text := FloatToStr(size);
        pMultiPointSymbol.GetAngle(angle);
        EditPointAngle.Text := FloatToStr(angle);
        goto exitpoint;
    end;

    pMultiLineSymbol := IMultiLineSymbol(GotoInterface(m_pSymbol, 'IMultiLineSymbol'));
    if (Assigned(pMultiLineSymbol)) then begin
        PanelLineColor.Color := color;
        pMultiLineSymbol.GetSize(size);
        EditLineWidth.Text := FloatToStr(size);
        goto exitpoint;
    end;

    pLineSymbol := ILineSymbol(GotoInterface(m_pSymbol, 'ILineSymbol'));
    if (Assigned(pLineSymbol)) then begin
        CreateMultiLineSymbol(pMultiLineSymbol);
        pMultiLineSymbol.AddSymbol(pLineSymbol);
        m_pSymbol := pMultiLineSymbol;
        PanelLineColor.Color := color;
        pMultiLineSymbol.GetSize(size);
        EditLineWidth.Text := FloatToStr(size);
        goto exitpoint;
    end;

    pMultiFillSymbol := IMultiFillSymbol(GotoInterface(m_pSymbol, 'IMultiFillSymbol'));
    if (Assigned(pMultiFillSymbol)) then begin
        PanelFillColor.Color := color;
        goto exitpoint;
    end;

    pFillSymbol := IFillSymbol(GotoInterface(m_pSymbol, 'IFillSymbol'));
    if (Assigned(pFillSymbol)) then begin
        CreateMultiFillSymbol(pMultiFillSymbol);
        pMultiFillSymbol.AddSymbol(pFillSymbol);
        m_pSymbol := pMultiFillSymbol;
        PanelFillColor.Color := color;
        goto exitpoint;
    end;

    pSimpleTextSymbol := ISimpleTextSymbol(GotoInterface(m_pSymbol, 'ISimpleTextSymbol'));
    if (Assigned(pSimpleTextSymbol)) then begin
        m_pSymbol := pSimpleTextSymbol;
        PanelTextColor.Color := color;
        pSimpleTextSymbol.GetWidth(size);
        EditTextWidth.Text := FloatToStr(size);
        pSimpleTextSymbol.GetHeight(size);
        EditTextHeight.Text := FloatToStr(size);
        goto exitpoint;
    end;

exitpoint:
    m_Progress := False;
end;

procedure TFormSymbolSelector.RefreshSymbolGot();
var
    Item: TListItem;
    pSymbol: ISymbol;
    pObj: IObj;
begin
    Item := LVSymbolList.Selected;
    if (not Assigned(Item)) then Exit;

    m_pSymbolLib.GetSymbolRef(pSymbol, Item.Index);
    pSymbol.Clone(pObj);
    m_pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
    Self.UpdateData2Form();
end;

function TFormSymbolSelector.LoadSymbolLibFromFile(const filename: AnsiString): Boolean;
begin
    Result := False;
    if (not LoadSymbolLib(PChar(filename), m_pSymbolLib, m_SymbolType)) then Exit;
    Self.RefreshSymbolList();

    Result := True;
end;

procedure TFormSymbolSelector.PreparePreviewDisplay();
var
    dc: HDC;
    rect: TRect;
    pTrans: IDisplayTransformation;
    f: Double;
begin
    m_pPreviewDisplay.GetDisplayTransformation(pTrans);
    pTrans.SetMapUnit(UNIT_MM);
    dc := GetDC(PanelSymbolPreview.Handle);
    m_pPreviewDisplay.SetDC(dc);
    rect.Left := 0;
    rect.Right := PanelSymbolPreview.Width;
    rect.Top := 0;
    rect.Bottom := PanelSymbolPreview.Height;
    pTrans.SetDeviceRect(rect);
    f := 1;
    pTrans.SetMapScale(f);
    f := 1;
    pTrans.SetReferenceScale(f);
    m_pPreviewDisplay.SetBackgroundColor(COLORREF($00F3F3F3));
end;

procedure TFormSymbolSelector.SymbolPreviewDraw();
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
    drawsymtype: Longint;
begin
    m_pPreviewDisplay.SetBackgroundColor(clWhite);
    m_pPreviewDisplay.RefreshWindow1();
    if (not Assigned(m_pSymbol)) then begin
        Exit;
    end;

    m_pPreviewDisplay.SetSymbol(m_pSymbol);
    m_pPreviewDisplay.GetDisplayTransformation(pTrans);
    m_pPreviewDisplay.GetRect(rect);

    CreateGeometry(GEOMETRYTYPE_POINT, pGeo);
    pPnt := IPoint(GotoInterface(pGeo, 'IPoint'));
    pnt.x := (rect.Right + rect.Left) div 2;
    pnt.y := (rect.Top + rect.Bottom) div 2;
    pTrans.Device2Map(pnt, wkspnt);
    pPnt.SetX(wkspnt.x);
    pPnt.SetY(wkspnt.y);

    CreateGeometry(GEOMETRYTYPE_PATH, pGeo);
    pPath := IPath(GotoInterface(pGeo, 'IPath'));
    pnt.x := rect.Left;
    pnt.y := (rect.Top + rect.Bottom) div 2;
    pTrans.Device2Map(pnt, wkspnt);
    wkspntz.x := wkspnt.x;
    wkspntz.y := wkspnt.y;
    pPath.AddPoint(wkspntz, VERTEXTYPE_COMMON);
    pnt.x := rect.Right;
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

    m_pPreviewDisplay.StartDraw();
    drawsymtype := m_pSymbol.GetSymbolType();
    case drawsymtype of
        SYMBOLTYPE_POINT: m_pPreviewDisplay.DrawGeometry(pPnt);
        SYMBOLTYPE_LINE: m_pPreviewDisplay.DrawGeometry(pLine);
        SYMBOLTYPE_FILL: m_pPreviewDisplay.DrawGeometry(pPolygon);
        SYMBOLTYPE_TEXT: m_pPreviewDisplay.DrawTextXY(x, y, '毛猫', rect);
    end;
    m_pPreviewDisplay.FinishDraw();
    m_pPreviewDisplay.RefreshWindow1();
end;

procedure TFormSymbolSelector.ClearSymbols();
begin
    m_pSymbolLib.ClearSymbols();
    Self.RefreshSymbolList();
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.FormShow(Sender: TObject);
var
    dc: HDC;
begin
    Self.Tag := 0;
    m_Progress := True;
    PanelPointProperty.Align := alClient;
    PanelLineProperty.Align := alClient;
    PanelFillProperty.Align := alClient;
    PanelTextProperty.Align := alClient;

    dc := GetDC(LVSymbolList.Handle);
    m_pSymbolListDisplay.SetDC(dc);

    Self.UpdateData2Form();
    Self.PreparePreviewDisplay();
    m_Progress := False;
    m_FormAlready := True;
end;

procedure TFormSymbolSelector.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    dc: HDC;
begin
    m_FormAlready := False;
    m_pSymbolListDisplay.GetDC(dc);
    ReleaseDC(LVSymbolList.Handle, dc);

    if (Self.Tag <> 1) then begin
        m_pSymbol := nil;
    end;
end;

procedure TFormSymbolSelector.BTOKClick(Sender: TObject);
begin
    Self.Tag := 1;
    Self.Close();
end;

procedure TFormSymbolSelector.BTCancelClick(Sender: TObject);
begin
    Self.Close();
end;

procedure TFormSymbolSelector.LVSymbolListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
    if (not Selected) then Exit;

    Self.RefreshSymbolGot();
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.FormPaint(Sender: TObject);
begin  
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.Panel3MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.Panel16MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.PanelFileListResize(Sender: TObject);
begin
    CBFileList.Width := PanelFileList.Width - 15;
end;

procedure TFormSymbolSelector.CBFileListChange(Sender: TObject);
begin
    Self.LoadLib(CBFileList.Text);
end;

procedure TFormSymbolSelector.PanelPointPropertyMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.PanelPointColorClick(Sender: TObject);
begin
    if (not Assigned(m_pSymbol)) then Exit;
    ColorDialog1.Color := PanelPointColor.Color;
    if (not ColorDialog1.Execute()) then Exit;
    PanelPointColor.Color := ColorDialog1.Color;

    m_pSymbol.SetColor(PanelPointColor.Color);
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.PanelLineColorClick(Sender: TObject);
begin
    if (not Assigned(m_pSymbol)) then Exit;
    ColorDialog1.Color := PanelLineColor.Color;
    if (not ColorDialog1.Execute()) then Exit;
    PanelLineColor.Color := ColorDialog1.Color;

    m_pSymbol.SetColor(PanelLineColor.Color);
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.EditPointAngleChange(Sender: TObject);
var
    angle: Double;
    pPointSymbol: IPointSymbol;
begin
    if (not m_FormAlready) then Exit;
    if (m_Progress) then Exit;
    if (not Assigned(m_pSymbol)) then Exit;
    pPointSymbol := IPointSymbol(GotoInterface(m_pSymbol, 'IPointSymbol'));
    if (not Assigned(pPointSymbol)) then Exit;

    try
        angle := StrToFloat(EditPointAngle.Text);
    except
        Exit;
    end;

    pPointSymbol.SetAngle(angle);
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.EditPointSizeChange(Sender: TObject);
var
    size: Double;
    pMPS: IMultiPointSymbol;
begin
    if (not m_FormAlready) then Exit;
    if (m_Progress) then Exit;
    if (not Assigned(m_pSymbol)) then Exit;
    pMPS := IMultiPointSymbol(GotoInterface(m_pSymbol, 'IMultiPointSymbol'));
    if (not Assigned(pMPS)) then Exit;

    try
        size := StrToFloat(EditPointSize.Text);
    except
        Exit;
    end;

    pMPS.SetSize(size);
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.EditLineWidthChange(Sender: TObject);
var
    size: Double;
    pMLS: IMultiLineSymbol;
begin
    if (not m_FormAlready) then Exit;
    if (m_Progress) then Exit;
    if (not Assigned(m_pSymbol)) then Exit;

    pMLS := IMultiLineSymbol(GotoInterface(m_pSymbol, 'IMultiLineSymbol'));
    if (not Assigned(pMLS)) then Exit;

    try
        size := StrToFloat(EditLineWidth.Text);
    except
        Exit;
    end;

    pMLS.SetSize(size);
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.TimerRefreshTimer(Sender: TObject);
begin
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.PanelFillColorClick(Sender: TObject);
begin
    if (not Assigned(m_pSymbol)) then Exit;
    ColorDialog1.Color := PanelFillColor.Color;
    if (not ColorDialog1.Execute()) then Exit;
    PanelFillColor.Color := ColorDialog1.Color;

    m_pSymbol.SetColor(PanelFillColor.Color);
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.PanelTextColorClick(Sender: TObject);
begin
    if (not Assigned(m_pSymbol)) then Exit;
    ColorDialog1.Color := PanelTextColor.Color;
    if (not ColorDialog1.Execute()) then Exit;
    PanelTextColor.Color := ColorDialog1.Color;

    m_pSymbol.SetColor(PanelTextColor.Color);
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.EditTextHeightChange(Sender: TObject);
var
    size: Double;
    pSTS: ISimpleTextSymbol;
begin
    if (not m_FormAlready) then Exit;
    if (m_Progress) then Exit;
    if (not Assigned(m_pSymbol)) then Exit;
    pSTS := ISimpleTextSymbol(GotoInterface(m_pSymbol, 'ISimpleTextSymbol'));
    if (not Assigned(pSTS)) then Exit;

    try
        size := StrToFloat(EditTextHeight.Text);
    except
        Exit;
    end;
    
    pSTS.SetHeight(size);
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.EditTextWidthChange(Sender: TObject);
var
    size: Double;
    pSTS: ISimpleTextSymbol;
begin
    if (not m_FormAlready) then Exit;
    if (m_Progress) then Exit;
    if (not Assigned(m_pSymbol)) then Exit;
    pSTS := ISimpleTextSymbol(GotoInterface(m_pSymbol, 'ISimpleTextSymbol'));
    if (not Assigned(pSTS)) then Exit;

    try
        size := StrToFloat(EditTextWidth.Text);
    except
        Exit;
    end;

    pSTS.SetWidth(size);
    Self.SymbolPreviewDraw();
end;

procedure TFormSymbolSelector.BTPointAttribClick(Sender: TObject);
begin
    SingleSymbolEditDoModal(m_pSymbol, SYMBOLTYPE_POINT);
end;

procedure TFormSymbolSelector.BTFillAttribClick(Sender: TObject);
begin
    SingleSymbolEditDoModal(m_pSymbol, SYMBOLTYPE_FILL);
end;

procedure TFormSymbolSelector.BTLineAttribClick(Sender: TObject);
begin
    SingleSymbolEditDoModal(m_pSymbol, SYMBOLTYPE_LINE);
end;

procedure TFormSymbolSelector.BTTextAttribClick(Sender: TObject);
begin
    SingleSymbolEditDoModal(m_pSymbol, SYMBOLTYPE_TEXT);
end;

procedure TFormSymbolSelector.SpeedButton1Click(Sender: TObject);
var
    size: Double;
    pMPS: IMultiPointSymbol;
begin
    if (not m_FormAlready) then Exit;
    if (m_Progress) then Exit;
    if (not Assigned(m_pSymbol)) then Exit;
    pMPS := IMultiPointSymbol(GotoInterface(m_pSymbol, 'IMultiPointSymbol'));
    if (not Assigned(pMPS)) then Exit;

    pMPS.GetSize(size);
    size := size + 0.1;
    EditPointSize.Text := FloatToStr(size);
end;

procedure TFormSymbolSelector.SpeedButton2Click(Sender: TObject);
var
    size: Double;
    pMPS: IMultiPointSymbol;
begin
    if (not m_FormAlready) then Exit;
    if (m_Progress) then Exit;
    if (not Assigned(m_pSymbol)) then Exit;
    pMPS := IMultiPointSymbol(GotoInterface(m_pSymbol, 'IMultiPointSymbol'));
    if (not Assigned(pMPS)) then Exit;

    pMPS.GetSize(size);
    size := size - 0.1;
    EditPointSize.Text := FloatToStr(size);
end;

initialization
    if (not InitialEasyLib('easylib.dll')) then begin
        MessageBox(0, 'no easylib.dll', '', MB_OK);
    end;

finalization
    FreeEasyLib();
end.
