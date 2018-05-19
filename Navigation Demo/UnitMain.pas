unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  InterfaceObj, InterfaceGeometry, InterfaceDisplayTransformation, WKSStructs,
  InterfaceDisplay, InterfaceSymbol, InterfaceLayer, InterfaceLayerAgent,
  InterfaceBitmapLayer, InterfaceMap, InterfaceActiveView, InterfaceTracker,
  InterfaceLabelLayer, easylib, ExtCtrls, ImgList, ComCtrls, UnitNavigating,
  ToolWin, StdCtrls;

type TMouseAction = Longword;
const MA_NORMOL         = TMouseAction(0);
const MA_ZOOM           = TMouseAction(1);
const MA_PAN            = TMouseAction(2);
const MA_ADDROUTE       = TMouseAction(3);
const MA_ADDBARRIER     = TMouseAction(4);

type
  TFormMain = class(TForm)
    PageControl1: TPageControl;
    TSPreview: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelSpace: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    ToolBarBrowse: TToolBar;
    ToolButton4: TToolButton;
    ToolButton12: TToolButton;
    ToolButton11: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ImageListPreview: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    OpenDialog1: TOpenDialog;
    ToolButton3: TToolButton;
    SaveDialog1: TSaveDialog;
    TimerRefreshSpace: TTimer;
    ToolButton5: TToolButton;
    OpenDialog2: TOpenDialog;
    ToolBarEdit: TToolBar;
    TBAddRoute: TToolButton;
    TBAddBarrier: TToolButton;
    TBDoBestPath: TToolButton;
    TBClearResult: TToolButton;
    CBShortestLayer: TComboBox;
    Panel5: TPanel;
    EditTolerance: TEdit;
    Panel6: TPanel;
    TimerNavigating: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PanelSpaceResize(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure TimerRefreshSpaceTimer(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure PanelSpaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelSpaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PanelSpaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolButton5Click(Sender: TObject);
    procedure CBShortestLayerChange(Sender: TObject);
    procedure EditToleranceChange(Sender: TObject);
    procedure TBAddRouteClick(Sender: TObject);
    procedure TBAddBarrierClick(Sender: TObject);
    procedure TBDoBestPathClick(Sender: TObject);
    procedure TBClearResultClick(Sender: TObject);
    procedure TimerNavigatingTimer(Sender: TObject);
  private
    { Private declarations }

    m_InitOK: Boolean;
    m_pMap: IMap;
    m_pAV: IActiveView;
    m_pDisplay: IDisplay;
    m_pDT: IDisplayTransformation;
    m_SpaceDC: HDC;
    m_MouseAction: TMouseAction;
    m_MouseDown: Boolean;
    m_MouseDownPos: array[0..1000] of TPoint;
    m_MouseDownCount: Longint;
    m_LastNavPoint: WKSPoint;
    m_pCarSymbol: ISymbol;

    m_pShortLayer: IVectorLayerAgent;
    m_pBestPath: IPolyline;
    m_NavProgress: Double;

    m_Navigating: TFormNavigating;

    procedure ReloadMap();
    procedure MainSpaceResize();
    procedure UpdateView();
    procedure ViewFullMap();
    procedure UpdateNavigating();
    procedure UpdateShortLayers();

    procedure AddNetRoute(route: WKSPoint);
    function AddNetBarrier(barrier: WKSPoint): Boolean;
    procedure AddNetBlock(blocked: WKSPoint);
    procedure PrepareBestPath();

    procedure LoadCarSymbol();
    procedure OpenWorkspace(ews: AnsiString);

  public
    { Public declarations }
  end;

const MOUSECURSOR_DEFAULT       = 1;
const MOUSECURSOR_ARROW1        = MOUSECURSOR_DEFAULT + 1;
const MOUSECURSOR_ARROW2        = MOUSECURSOR_ARROW1 + 1;
const MOUSECURSOR_CROSS1        = MOUSECURSOR_ARROW2 + 1;
const MOUSECURSOR_CROSS2        = MOUSECURSOR_CROSS1 + 1;
const MOUSECURSOR_CROSSBOX      = MOUSECURSOR_CROSS2 + 1;
const MOUSECURSOR_PICKUP        = MOUSECURSOR_CROSSBOX + 1;
const MOUSECURSOR_MOVE          = MOUSECURSOR_PICKUP + 1;
const MOUSECURSOR_COPY          = MOUSECURSOR_MOVE + 1;
const MOUSECURSOR_DRAG          = MOUSECURSOR_COPY + 1;
const MOUSECURSOR_BEAM          = MOUSECURSOR_DRAG + 1;
const MOUSECURSOR_CANCEL        = MOUSECURSOR_BEAM + 1;
const MOUSECURSOR_MEASURE       = MOUSECURSOR_CANCEL + 1;
const MOUSECURSOR_HAND          = MOUSECURSOR_MEASURE + 1;
const MOUSECURSOR_ZOOM          = MOUSECURSOR_HAND + 1;
const MOUSECURSOR_ZOOMIN        = MOUSECURSOR_ZOOM + 1;
const MOUSECURSOR_ZOOMOUT       = MOUSECURSOR_ZOOMIN + 1;
const MOUSECURSOR_BUSYWAIT      = MOUSECURSOR_ZOOMOUT + 1;
const MOUSECURSOR_BARRIER       = MOUSECURSOR_BUSYWAIT + 1;
const MOUSECURSOR_FOX           = MOUSECURSOR_BARRIER + 1;
const MOUSECURSOR_PIGGY         = MOUSECURSOR_FOX + 1;
const MOUSECURSOR_HOLD          = MOUSECURSOR_PIGGY + 1;
const MOUSECURSOR_CATCH         = MOUSECURSOR_HOLD + 1;
const MOUSECURSOR_PENCIL        = MOUSECURSOR_CATCH + 1;
const MOUSECURSOR_BULLSEYE      = MOUSECURSOR_PENCIL + 1;
const MOUSECURSOR_TERMINATOR    = MOUSECURSOR_BULLSEYE + 1;
const MOUSECURSOR_PALETTE       = MOUSECURSOR_TERMINATOR + 1;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}
{$R cursors.res}

procedure TFormMain.ReloadMap();
begin
    m_pAV := IActiveView(GotoInterface(m_pMap, 'IActiveView'));
    m_pAV.GetDisplay(m_pDisplay);
    m_pDisplay.GetDisplayTransformation(m_pDT);
end;

procedure TFormMain.MainSpaceResize();
var
    rect: TRect;
begin
    if (not m_InitOK) then Exit;

    rect.Left := 0;
    rect.Top := 0;
    rect.Right := PanelSpace.Width;
    rect.Bottom := PanelSpace.Height;

    m_pAV.LostFocus();
    m_pAV.GainFocus(m_SpaceDC, rect);

    Self.UpdateView();
end;

procedure TFormMain.UpdateView();
begin
    m_pAV.UpdateData(nil);
    m_pAV.UpdateSelection(nil);

    m_pAV.RefreshWindow();
end;

procedure TFormMain.ViewFullMap();
var
    extent: WKSRect;
begin
    m_pMap.GetFullExtent(extent);
    m_pDT.SetVisibleExtent(extent);
    Self.UpdateView();
end;

procedure TFormMain.UpdateNavigating();
var
    i, count: Longint;
    pLayer: ILayer;
    centerpoint: WKSPoint;
    scale: Double;
    mapunit: TMapUnits;
    pLLM: ILabelLayerManager;
    pLL: ILabelLayer;
begin
    m_Navigating.ClearAll();
    count := m_pMap.GetLayerCount();
    for i := count - 1 downto 0 do begin
        m_pMap.GetLayer(pLayer, i);
        m_Navigating.AddLayer(pLayer);
    end;

    pLLM := ILabelLayerManager(GotoInterface(m_pMap, 'ILabelLayerManager'));
    count := pLLM.GetLabelLayerCount();
    for i := 0 to count - 1 do begin
        pLLM.GetLabelLayer(pLL, i);
        m_Navigating.AddLabelLayer(pLL);
    end;

    mapunit := m_pDT.GetMapUnit();
    m_Navigating.SetMapUnit(mapunit);
    m_pDT.GetMapScale(scale);
    scale := scale / 5;
    m_Navigating.SetScale(scale);
    m_pDT.GetRotateCenter(centerpoint);
    m_Navigating.SetCenter(centerpoint);
    m_Navigating.UpdateView();

end;


procedure DrawEnvelope(dc: HDC; envelope: TRect; linecolor: COLORREF;
    linewidth: Longint; hollow: Boolean; fillcolor: COLORREF);
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

    SelectObject(dc, BrushOld);
    SelectObject(dc, PenOld);
    DeleteObject(BrushNew);
    DeleteObject(PenNew);
end;

procedure TFormMain.FormShow(Sender: TObject);
var
    pObj: IObj;
    currdir: AnsiString;
begin
    pObj := nil;
    m_SpaceDC := GetDC(PanelSpace.Handle);

    CreateObj('CGeoMap', pObj);
    m_pMap := IMap(GotoInterface(pObj, 'IMap'));
    pObj := nil;
    Self.ReloadMap();

    m_MouseAction := MA_NORMOL;
    PanelSpace.Cursor := MOUSECURSOR_DEFAULT;
    m_MouseDown := False;
    m_MouseDownCount := 0;

    m_InitOK := True;
    Self.MainSpaceResize();

    m_Navigating := TFormNavigating.Create(Self);
    m_Navigating.Show();
    m_Navigating.Left := Self.Left + Self.Width - m_Navigating.Width - 35;
    m_Navigating.Top := Self.Top + Self.Height - m_Navigating.Height - 80;

    currdir := GetCurrentDir();
    SetCurrentDir('.\导航示例数据');
    Self.OpenWorkspace('map.ews');
    SetCurrentDir(currdir);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
    m_InitOK := False;
    //load cursor resource 鼠标光标资源
    Screen.Cursors[MOUSECURSOR_DEFAULT]     := LoadCursor(HInstance, 'IDC_DEFAULT');
    Screen.Cursors[MOUSECURSOR_ARROW1]      := LoadCursor(HInstance, 'IDC_ARROW1');
    Screen.Cursors[MOUSECURSOR_ARROW2]      := LoadCursor(HInstance, 'IDC_ARROW2');
    Screen.Cursors[MOUSECURSOR_CROSS1]      := LoadCursor(HInstance, 'IDC_CROSS1');
    Screen.Cursors[MOUSECURSOR_CROSS2]      := LoadCursor(HInstance, 'IDC_CROSS2');
    Screen.Cursors[MOUSECURSOR_CROSSBOX]    := LoadCursor(HInstance, 'IDC_CROSSBOX');
    Screen.Cursors[MOUSECURSOR_PICKUP]      := LoadCursor(HInstance, 'IDC_PICKUP');
    Screen.Cursors[MOUSECURSOR_MOVE]        := LoadCursor(HInstance, 'IDC_MOVE');
    Screen.Cursors[MOUSECURSOR_COPY]        := LoadCursor(HInstance, 'IDC_COPY');
    Screen.Cursors[MOUSECURSOR_DRAG]        := LoadCursor(HInstance, 'IDC_DRAG');
    Screen.Cursors[MOUSECURSOR_BEAM]        := LoadCursor(HInstance, 'IDC_BEAM');
    Screen.Cursors[MOUSECURSOR_CANCEL]      := LoadCursor(HInstance, 'IDC_CANCEL');
    Screen.Cursors[MOUSECURSOR_MEASURE]     := LoadCursor(HInstance, 'IDC_MEASURE');
    Screen.Cursors[MOUSECURSOR_HAND]        := LoadCursor(HInstance, 'IDC_HAND');
    Screen.Cursors[MOUSECURSOR_ZOOM]        := LoadCursor(HInstance, 'IDC_ZOOM');
    Screen.Cursors[MOUSECURSOR_ZOOMIN]      := LoadCursor(HInstance, 'IDC_ZOOMIN');
    Screen.Cursors[MOUSECURSOR_ZOOMOUT]     := LoadCursor(HInstance, 'IDC_ZOOMOUT');
    Screen.Cursors[MOUSECURSOR_BUSYWAIT]    := LoadCursor(HInstance, 'IDC_BUSYWAIT');
    Screen.Cursors[MOUSECURSOR_BARRIER]     := LoadCursor(HInstance, 'IDC_BARRIER');
    Screen.Cursors[MOUSECURSOR_FOX]         := LoadCursor(HInstance, 'IDC_FOX');
    Screen.Cursors[MOUSECURSOR_PIGGY]       := LoadCursor(HInstance, 'IDC_PIGGY');
    Screen.Cursors[MOUSECURSOR_HOLD]        := LoadCursor(HInstance, 'IDC_HOLD');
    Screen.Cursors[MOUSECURSOR_CATCH]       := LoadCursor(HInstance, 'IDC_CATCH');
    Screen.Cursors[MOUSECURSOR_PENCIL]      := LoadCursor(HInstance, 'IDC_PENCIL');
    Screen.Cursors[MOUSECURSOR_BULLSEYE]    := LoadCursor(HInstance, 'IDC_BULLSEYE');
    Screen.Cursors[MOUSECURSOR_TERMINATOR]  := LoadCursor(HInstance, 'IDC_TERMINATOR');
    Screen.Cursors[MOUSECURSOR_PALETTE]     := LoadCursor(HInstance, 'IDC_PALETTE');

    Self.LoadCarSymbol();    
end;

procedure TFormMain.PanelSpaceResize(Sender: TObject);
begin
    Self.MainSpaceResize();
end;

procedure TFormMain.ToolButton1Click(Sender: TObject);
begin
    m_pMap.ClearAllData();
    Self.UpdateView();

    m_Navigating.ClearAll();
    m_Navigating.UpdateView();
    Self.UpdateShortLayers();
    m_pShortLayer := nil;
end;

procedure TFormMain.OpenWorkspace(ews: AnsiString);
var
    pObj: IObj;
    pStream: IStreamX;
    pPersist: IPersist;
begin
    CBShortestLayer.Items.Clear();

    m_pMap.ClearAllData();
    CreateObj('CMemoryStream', pObj);
    pStream := IStreamX(GotoInterface(pObj, 'IStreamX'));
    if (not pStream.LoadFromFile(PChar(ews))) then begin
        Exit;
    end;

    Instantiate(pStream, pPersist);
    if (not Assigned(pPersist)) then Exit;
    m_pMap := IMap(GotoInterface(pPersist, 'IMap'));
    Self.ReloadMap();
    Self.MainSpaceResize();

    Self.UpdateNavigating();
    Self.UpdateShortLayers();

    Self.CBShortestLayerChange(nil);
end;

procedure TFormMain.ToolButton2Click(Sender: TObject);
begin
    if (not OpenDialog1.Execute()) then begin
        Exit;
    end;

    Self.OpenWorkspace(OpenDialog1.FileName)
end;

procedure TFormMain.ToolButton3Click(Sender: TObject);
var
    pObj: IObj;
    pStream: IStreamX;
    pPersist: IPersist;
    filename: AnsiString;
begin
    if (not SaveDialog1.Execute()) then begin
        Exit;
    end;

    CreateObj('CMemoryStream', pObj);
    pStream := IStreamX(GotoInterface(pObj, 'IStreamX'));

    pPersist := IPersist(GotoInterface(m_pMap, 'IPersist'));
    pPersist.Dump(pStream);

    filename := SaveDialog1.FileName;
    pStream.SaveToFile(PChar(filename));
end;

procedure TFormMain.FormPaint(Sender: TObject);
begin
    m_pAV.RefreshWindow();
end;

procedure TFormMain.TimerRefreshSpaceTimer(Sender: TObject);
begin
    if (not m_InitOK) then begin
        Exit;
    end;

    m_pAV.RefreshWindow();
end;

procedure TFormMain.ToolButton8Click(Sender: TObject);
begin
    Self.UpdateView();
end;

procedure TFormMain.ToolButton7Click(Sender: TObject);
begin
    Self.ViewFullMap();
end;

procedure TFormMain.ToolButton6Click(Sender: TObject);
begin
    m_MouseAction := MA_PAN;
    PanelSpace.Cursor := MOUSECURSOR_HAND;
    m_MouseDown := False;
    m_MouseDownCount := 0;
end;

procedure TFormMain.PanelSpaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
    pScreenBrowser: IScreenBrowser;
    pDT: IDisplayTransformation;
    map_point: WKSPoint;
begin
    m_MouseDown := True;
    pScreenBrowser := IScreenBrowser(GotoInterface(m_pDisplay, 'IScreenBrowser'));

    m_pDisplay.GetDisplayTransformation(pDT);
    pDT.Device2MapXY(X, Y, map_point.x, map_point.y);

    case m_MouseAction of
        MA_ZOOM, MA_PAN: begin
            m_MouseDownPos[0].x := X;
            m_MouseDownPos[0].y := Y;
            m_MouseDownCount := 0;
            if (MA_PAN = m_MouseAction) then begin
                pScreenBrowser.PanStart(m_MouseDownPos[0]);
            end;
        end;

        MA_ADDROUTE: begin
            Self.AddNetRoute(map_point);
            m_MouseDownCount := 0;
            Self.PrepareBestPath();
            m_pAV.RefreshWindow();
        end;

        MA_ADDBARRIER: begin
            if (not Self.AddNetBarrier(map_point)) then begin
                Self.AddNetBlock(map_point);
            end;
            m_MouseDownCount := 0;
            Self.PrepareBestPath();
            m_pAV.RefreshWindow();
        end;

        else begin
            m_MouseDownCount := 0;
        end;
    end;

end;

procedure TFormMain.PanelSpaceMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
    rect: TRect;
    pnt: TPoint;
    pScreenBrowser: IScreenBrowser;
begin
    pScreenBrowser := IScreenBrowser(GotoInterface(m_pDisplay, 'IScreenBrowser'));
    pnt.x := X;
    pnt.y := Y;
    rect.TopLeft := m_MouseDownPos[0];
    rect.Right := X;
    rect.Bottom := Y;
    case m_MouseAction of
        MA_ZOOM: begin
            if (m_MouseDown) then begin
                m_pAV.RefreshWindow();
                DrawEnvelope(m_SpaceDC, rect, clBlue, 1, True, 0);
            end;
        end;

        MA_PAN: begin
            pScreenBrowser.PanMoveTo(pnt);
        end;
    end;

end;

procedure TFormMain.PanelSpaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
    rect: TRect;
    pScreenBrowser: IScreenBrowser;
begin
    m_MouseDown := False;
    rect.TopLeft := m_MouseDownPos[0];
    rect.Right := X;
    rect.Bottom := Y;
    pScreenBrowser := IScreenBrowser(GotoInterface(m_pDisplay, 'IScreenBrowser'));

    case m_MouseAction of
        MA_ZOOM: begin
            if (Button = mbLeft) then begin
                pScreenBrowser.VisibleExtentIn(rect);
            end
            else begin
                pScreenBrowser.VisibleExtentOut(rect);
            end;
            Self.UpdateView();

            m_MouseDownCount := 0;
        end;

        MA_PAN: begin
            pScreenBrowser.PanStop();
            Self.UpdateView();
            m_MouseDownCount := 0;
        end;
    end;

    Self.TimerRefreshSpaceTimer(nil);

end;

procedure TFormMain.ToolButton4Click(Sender: TObject);
begin
    m_MouseAction := MA_ZOOM;
    PanelSpace.Cursor := MOUSECURSOR_ZOOMIN;

    m_MouseDown := False;
    m_MouseDownCount := 0;
end;

procedure TFormMain.ToolButton12Click(Sender: TObject);
var
    scale: Double;
begin
    m_pDT.GetMapScale(scale);
    scale := scale / 2;
    m_pDT.SetMapScale(scale);
    Self.UpdateView();
end;

procedure TFormMain.ToolButton11Click(Sender: TObject);
var
    scale: Double;
begin
    m_pDT.GetMapScale(scale);
    scale := scale * 2;
    m_pDT.SetMapScale(scale);
    Self.UpdateView();
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
    
    m_Navigating.Close();
    m_Navigating.Free();
end;

procedure TFormMain.ToolButton5Click(Sender: TObject);
var
    pObj: IObj;
    pVLA: IVectorLayerAgent;
    pLayer: ILayer;
    filename: AnsiString;
    flag: Boolean;
begin
    if (not OpenDialog2.Execute()) then begin
        Exit;
    end;

    filename := OpenDialog2.FileName;

    CreateObj('CVectorLayerAgent', pObj);
    pVLA := IVectorLayerAgent(GotoInterface(pObj, 'IVectorLayerAgent'));

    if (OpenDialog2.FilterIndex = 1) then begin
        //esd
        flag := pVLA.LoadSlimLayer(PChar(filename), True, True);
    end
    else begin
        //shp
        flag := pVLA.LoadShapeLayer(PChar(filename), UNIT_M, 2000, 0.01, 5, True);
    end;

    if (not flag) then begin
        Exit;
    end;

    pVLA.GetLayer(pLayer);
    m_pMap.AddLayer(pLayer);
    if (m_pMap.GetAllCount() = 1) then begin
        Self.ViewFullMap();
    end
    else begin
        Self.UpdateView();
    end;

    Self.UpdateNavigating();
    Self.UpdateShortLayers();
end;

procedure TFormMain.UpdateShortLayers();
var
    i, count: Longint;
    pLayer: ILayer;
    layername: AnsiString;
begin
    CBShortestLayer.Items.Clear();
    count := m_pMap.GetLayerCount();
    for i := 0 to count - 1 do begin
        m_pMap.GetLayer(pLayer, i);
        layername := pLayer.GetName();
        CBShortestLayer.Items.Add(layername);
    end;

    for i := 0 to count - 1 do begin
        if (CBShortestLayer.Items[i] = '路径分析') then begin
            CBShortestLayer.ItemIndex := i;
            break;
        end;
    end;

end;



procedure TFormMain.CBShortestLayerChange(Sender: TObject);
var
    pLayer: ILayer;
    pObj: IObj;
    pShortLayer: IVectorLayerAgent;
    colinfo: TGeometryColumnInfo;
begin
    m_pShortLayer := nil;
    if (CBShortestLayer.ItemIndex < 0) then begin
        Exit;
    end;

    m_pMap.GetLayer(pLayer, CBShortestLayer.ItemIndex);

    CreateObj('CVectorLayerAgent', pObj);
    pShortLayer := IVectorLayerAgent(GotoInterface(pObj, 'IVectorLayerAgent'));
    if (not pShortLayer.SetLayer(pLayer)) then begin
        Exit;
    end;

    pShortLayer.GetGeometryColumnInfo(colinfo);
    if (colinfo.ShpType <> SHAPETYPE_POLYLINE) then begin
        Exit;
    end;

    m_pShortLayer := pShortLayer;

    if (not m_pShortLayer.RestoreNetTopo()) then begin
        m_pShortLayer.CreateNetTopo(-1, True);
    end;
    
    m_pShortLayer.SetNetTolerance(StrToFloat(EditTolerance.Text));

    Self.TBDoBestPathClick(nil);
    Self.PrepareBestPath();
end;

procedure TFormMain.EditToleranceChange(Sender: TObject);
begin
    if (Assigned(m_pShortLayer)) then begin
        m_pShortLayer.SetNetTolerance(StrToFloat(EditTolerance.Text));
    end;
end;

procedure TFormMain.TBAddRouteClick(Sender: TObject);
begin
    m_MouseAction := MA_ADDROUTE;
    PanelSpace.Cursor := MOUSECURSOR_ARROW2;

    m_MouseDown := False;
    m_MouseDownCount := 0;
end;

procedure TFormMain.TBAddBarrierClick(Sender: TObject);
begin
    m_MouseAction := MA_ADDBARRIER;
    PanelSpace.Cursor := MOUSECURSOR_ARROW1;

    m_MouseDown := False;
    m_MouseDownCount := 0;
end;

procedure TFormMain.AddNetRoute(route: WKSPoint);
begin
    if (not Assigned(m_pShortLayer)) then begin
        Exit;
    end;

    m_pShortLayer.AddNetRoute(route);
    TimerRefreshSpace.Enabled := True;
end;

function TFormMain.AddNetBarrier(barrier: WKSPoint): Boolean;
begin
    Result := False;
    if (not Assigned(m_pShortLayer)) then begin
        Exit;
    end;

    Result := m_pShortLayer.AddNetBarrierPoint(barrier);
end;

procedure TFormMain.AddNetBlock(blocked: WKSPoint);
var
    pFids: IIntArray;
    fid: Longint;
    rect: WKSRect;
    point_dev: TPoint;
    rect_dev: TRect;
    pFeature: IVectorFeature;
    pGeometry: IGeometry;
    pPolyline: IPolyline;
    pPath: IPath;
    pntcount: Longword;
    pnt_from, pnt_to: WKSPointZ;
    _from, _to: WKSPoint;
begin
    if (not Assigned(m_pShortLayer)) then begin
        Exit;
    end;

    m_pDT.Map2Device(blocked, point_dev);
    rect_dev.Left := point_dev.x - 1;
    rect_dev.Right := point_dev.x + 1;
    rect_dev.Top := point_dev.y - 1;
    rect_dev.Bottom := point_dev.y + 1;
    m_pDT.Device2MapXY(rect_dev.Left, rect_dev.Bottom, rect.left, rect.bottom);
    m_pDT.Device2MapXY(rect_dev.Right, rect_dev.top, rect.right, rect.top);
    m_pShortLayer.Identify(pFids, rect, true);
    if (pFids.GetSize() = 0) then begin
        Exit;
    end;
    pFids.GetAt(0, fid);
    m_pShortLayer.GetFeature(fid, pFeature);
    pFeature.GetGeometryRef(pGeometry);
    pPolyline := IPolyline(GotoInterface(pGeometry, 'IPolyline'));
    if (not Assigned(pPolyline)) then begin
        Exit;
    end
    else if (pPolyline.GetPathCount() = 0) then begin
        Exit;
    end;
    pPolyline.GetPathRef(pPath, 0);
    pntcount := pPath.GetPointCount();
    if (pntcount < 2) then begin
        Exit;
    end;
    pPath.GetPoint1(0, pnt_from);
    pPath.GetPoint1(pntcount - 1, pnt_to);
    _from := Z2Point(pnt_from);
    _to := Z2Point(pnt_to);

    m_pShortLayer.AddNetBlockedSingleEdge(_from, _to);
    m_pShortLayer.AddNetBlockedSingleEdge(_to, _from);
end;

procedure TFormMain.PrepareBestPath();
var
    pMP: IMultiPoint;
    pnt: WKSPointZ;
    pGeometry: IGeometry;
    pES: IEnvelopePointSymbol;
    pSLS: ISimpleLineSymbol;
    pObj: IObj;
    blocked_fids: IIntArray;
    i, count: Longint;
    fid: Longint;
    pFeature: IVectorFeature;

    pRapidDraw: IRapidDraw;
    pLayer: ILayer;
    pELA: IElementLayerAgent;
    pElement: IElement;
    layername: AnsiString;
begin
    CreateObj('CElementLayerAgent', pObj);
    pELA := IElementLayerAgent(GotoInterface(pObj, 'IElementLayerAgent'));

    pRapidDraw := IRapidDraw(GotoInterface(m_pMap, 'IRapidDraw'));
    count := pRapidDraw.RD_GetLayerCount();
    for i := 0 to count - 1 do begin
        pLayer := nil;
        pRapidDraw.RD_GetLayer(pLayer, i);
        layername := pLayer.GetName();
        if (layername = 'shortest_path') then begin
            pRapidDraw.RD_RemoveLayerEx(pLayer);
            break;
        end;
    end;

    if (not Assigned(m_pShortLayer)) then begin
        Exit;
    end;

    pELA.CreateElementLayer('shortest_path');
    pELA.GetLayer(pLayer);
    pRapidDraw.RD_AddLayer(pLayer);

    if (Assigned(m_pBestPath)) then begin
        CreateObj('CSimpleLineSymbol', pObj);
        pSLS := ISimpleLineSymbol(GotoInterface(pObj, 'ISimpleLineSymbol'));
        pSLS.SetColor(clBlue);
        pSLS.SetWidth(1.5);

        CreateObj('CGeometryElement', pObj);
        pElement := IElement(GotoInterface(pObj, 'IElement'));
        pElement.SetSymbol(pSLS);
        pElement.SetGeometry(m_pBestPath);
        pELA.AddElement(pElement);
    end;

    m_pShortLayer.GetNetBlockedEdgeIDs(blocked_fids);
    count := blocked_fids.GetSize();
    if (count > 0) then begin
        for i := 0 to count - 1 do begin
            blocked_fids.GetAt(i, fid);
            m_pShortLayer.GetFeature(fid, pFeature);
            pFeature.GetGeometryRef(pGeometry);

            CreateObj('CSimpleLineSymbol', pObj);
            pSLS := ISimpleLineSymbol(GotoInterface(pObj, 'ISimpleLineSymbol'));
            pSLS.SetColor(clRed);
            pSLS.SetWidth(1.0);

            CreateObj('CGeometryElement', pObj);
            pElement := IElement(GotoInterface(pObj, 'IElement'));
            pElement.SetSymbol(pSLS);
            pElement.SetGeometry(pGeometry);
            pELA.AddElement(pElement);
        end;
    end;

    CreateObj('CEnvelopePointSymbol', pObj);
    pES := IEnvelopePointSymbol(GotoInterface(pObj, 'IEnvelopePointSymbol'));
    pES.SetWidth(2.8);
    pES.SetHeight(2.8);
    pES.SetSolid(True);
    pES.SetColor(clGreen);
    m_pShortLayer.GetNetRoutes(pMP);
    pGeometry := IGeometry(GotoInterface(pMP, 'IGeometry'));
    CreateObj('CGeometryElement', pObj);
    pElement := IElement(GotoInterface(pObj, 'IElement'));
    pElement.SetSymbol(pES);
    pElement.SetGeometry(pGeometry);
    pELA.AddElement(pElement);

    m_Navigating.ClearNavPoints();
    count := pMP.GetPointCount();
    for i := 0 to count - 1 do begin
        pMP.GetPoint(pnt, i);
        m_Navigating.AddNavPoint(pnt);
    end;

    pObj := nil;
    pES.Clone(pObj);
    pES := IEnvelopePointSymbol(GotoInterface(pObj, 'IEnvelopePointSymbol'));

    pES.SetColor(clRed);
    pMP := nil;
    m_pShortLayer.GetNetBarrierPoints(pMP);
    pGeometry := IGeometry(GotoInterface(pMP, 'IGeometry'));
    CreateObj('CGeometryElement', pObj);
    pElement := IElement(GotoInterface(pObj, 'IElement'));
    pElement.SetSymbol(pES);
    pElement.SetGeometry(pGeometry);
    pELA.AddElement(pElement);

end;

procedure TFormMain.LoadCarSymbol();
var
    pObj: IObj;
    pES: IEnvelopePointSymbol;
    pMPS: IMultiPointSymbol;
    pSymbolLib: ISymbolLib;
    filename: AnsiString;
begin
    CreateObj('CEnvelopePointSymbol', pObj);
    pES := IEnvelopePointSymbol(GotoInterface(pObj, 'IEnvelopePointSymbol'));
    pES.SetWidth(3.8);
    pES.SetHeight(3.8);
    pES.SetSolid(True);
    pES.SetColor(clFuchsia);
    m_pCarSymbol := pES;

    pObj := nil;
    CreateObj('CSymbolLib', pObj);
    pSymbolLib := ISymbolLib(GotoInterface(pObj, 'ISymbolLib'));

    filename := 'cars.sym';
    if (LoadSymbolLib(PChar(filename), pSymbolLib, SYMBOLTYPE_POINT)) then begin
        if (pSymbolLib.GetSymbolCount() >= 2) then begin
            pSymbolLib.GetSymbolRef(m_pCarSymbol, 0);
            pMPS := IMultiPointSymbol(GotoInterface(m_pCarSymbol, 'IMultiPointSymbol'));
            if (Assigned(pMPS)) then begin
                pMPS.SetSize(0.8);
            end;
        end;
    end;
end;


procedure TFormMain.TBDoBestPathClick(Sender: TObject);
var
    i, j, k, count, fid: Longint;
    pFids: IIntArray;
    pPolylineTmp: IPolyline;
    pFeature: IVectorFeature;
    pGeometry: IGeometry;
    pPath, pPath1, pPath2: IPath;
    startpoint, point1: WKSPointZ;
begin
    if (not Assigned(m_pShortLayer)) then begin
        Exit;
    end;

    CreateGeometry(GEOMETRYTYPE_POLYLINE, pGeometry);
    m_pBestPath := IPolyline(GotoInterface(pGeometry, 'IPolyline'));

    m_pShortLayer.DoBestPath(pPath, pFids);

    count := pFids.GetSize();
    if (count = 0) then begin
        Exit;
    end;

    for i := 0 to count - 1 do begin
        pFids.GetAt(i, fid);
        pFeature := nil;
        m_pShortLayer.GetFeature(fid, pFeature);
        pGeometry := nil;
        pFeature.GetGeometryRef(pGeometry);
        pPolylineTmp := IPolyline(GotoInterface(pGeometry, 'IPolyline'));

        pPath.GetPoint1(i, startpoint);

        for j := 0 to pPolylineTmp.GetPathCount() - 1 do begin
            pPath1 := nil;
            pPolylineTmp.GetPathRef(pPath1, j);
            pPath1.GetPoint1(0, point1);
            if ((abs(point1.x - startpoint.x) < 0.0000001)
                and (abs(point1.y - startpoint.y) < 0.0000001)) then begin
                pPath2 := pPath1;
            end
            else begin
                CreateGeometry(GEOMETRYTYPE_PATH, pGeometry);
                pPath2 := IPath(GotoInterface(pGeometry, 'IPath'));
                for k := pPath1.GetPointCount() - 1 downto 0 do begin
                    pPath1.GetPoint1(k, point1);
                    pPath2.AddPoint(point1, VERTEXTYPE_COMMON);
                end;
            end;

            m_pBestPath.AddPathRef(pPath2);
        end;
    end;                    

    Self.PrepareBestPath();
end;

procedure TFormMain.TBClearResultClick(Sender: TObject);
begin
    m_pBestPath := nil;

    if (not Assigned(m_pShortLayer)) then begin
        Exit;
    end;

    m_pShortLayer.ClearNetRoutes();
    m_pShortLayer.ClearNetBarrierPoints();
    m_pShortLayer.ClearNetBlockedEdges();
    Self.PrepareBestPath();
end;

function ComputeAngle(var pointcenter: WKSPoint; var pointmove: WKSPoint): Double;
var
    x, y, angle: Double;
begin
    x := pointmove.x - pointcenter.x;
    y := pointmove.y - pointcenter.y;
    y := -y;

    if ((abs(x) < 0.0000001) or (abs(y) < 0.0000001)) then begin
        Result := 0;
        Exit;
    end;

    angle := arctan(y / x);

    if ((x > 0) and (y > 0)) then begin
        angle := 2*3.1415927 + angle
    end;
    if ((x < 0) and (y > 0)) then begin
        angle := 3.1415927 + angle;
    end;
    if ((x < 0) and (y < 0)) then begin
        angle := 3.1415927 + angle;
    end;
    if ((x > 0) and (y < 0)) then begin
        angle := angle;
    end;

    Result := angle / 3.1415927 / 2.0 * 360.0;
end;

procedure TFormMain.TimerNavigatingTimer(Sender: TObject);
var
    degree, length: Double;
    position: WKSPoint;

    pObj: IObj;
    i, count: Longint;
    pRapidDraw: IRapidDraw;
    pLayer: ILayer;
    pELA: IElementLayerAgent;
    pElement: IElement;
    layername: AnsiString;
    pPositionPoint: IPoint;
    pPointSymbol: IPointSymbol;
begin
    if (not Assigned(m_pBestPath)) then begin
        Exit;
    end;

    if (m_pBestPath.GetPathCount() = 0) then begin
        Exit;
    end;

    m_pBestPath.GetLength(length);

    if (length < m_NavProgress) then begin
        m_NavProgress := 0;
    end
    else begin
        m_NavProgress := m_NavProgress + (length / 1000);
        m_pBestPath.GetPositionByDistance(m_NavProgress, position);
    end;

    pRapidDraw := IRapidDraw(GotoInterface(m_pMap, 'IRapidDraw'));
    count := pRapidDraw.RD_GetLayerCount();
    for i := 0 to count - 1 do begin
        pLayer := nil;
        pRapidDraw.RD_GetLayer(pLayer, i);
        layername := pLayer.GetName();
        if (layername = 'navigating_point') then begin
            break;
        end;

        pLayer := nil;
    end;

    CreateObj('CElementLayerAgent', pObj);
    pELA := IElementLayerAgent(GotoInterface(pObj, 'IElementLayerAgent'));

    if (not Assigned(pLayer)) then begin
        pELA.CreateElementLayer('navigating_point');
        pELA.GetLayer(pLayer);
        pRapidDraw.RD_AddLayer(pLayer);
    end
    else begin
        pELA.SetLayer(pLayer);
        pELA.ClearElements();
    end;

    CreateObj('CPoint', pObj);
    pPositionPoint := IPoint(GotoInterface(pObj, 'IPoint'));
    pPositionPoint.SetX(position.x);
    pPositionPoint.SetY(position.y);

    degree := ComputeAngle(m_LastNavPoint, position) + 90;
    m_LastNavPoint := position;
    m_Navigating.SetCenter(position);
    m_Navigating.Rotate(degree);
    m_Navigating.UpdateView();

    pPointSymbol := IPointSymbol(GotoInterface(m_pCarSymbol, 'IPointSymbol'));
    pPointSymbol.SetAngle(degree);

    CreateObj('CGeometryElement', pObj);
    pElement := IElement(GotoInterface(pObj, 'IElement'));
    pElement.SetSymbol(pPointSymbol);
    pElement.SetGeometry(pPositionPoint);
    pELA.AddElement(pElement);

    m_pAV.RefreshWindow();
end;


initialization
    if (not InitialEasyLib('easylib.dll')) then begin
        MessageBox(0, 'no easylib.dll', '', MB_OK);
        Application.Terminate();
    end;

finalization
    FreeEasyLib();

end.
