unit mainform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, ComCtrls, ImgList, Grids, Outline, DirOutln, StdCtrls, ToolWin,
  easylib, InterfaceObj, InterfaceGeometry, InterfaceDisplayTransformation,
  InterfaceDisplay, InterfaceSymbol, InterfaceLayer, InterfaceLayerAgent,
  InterfaceBitmapLayer, InterfaceMap, InterfaceActiveView, InterfaceTracker,
  InterfaceLabelLayer, WKSStructs;

type TMouseAction = Longword;
const MA_NORMOL         = TMouseAction(0);
const MA_ZOOM           = TMouseAction(1);
const MA_PAN            = TMouseAction(2);
const MA_SELECTFEATURE  = TMouseAction(3);
const MA_ADDPOINT       = TMouseAction(4);
const MA_ADDLINE        = TMouseAction(5);
const MA_ADDPOLYGON     = TMouseAction(6);
const MA_ADDANNOTATION  = TMouseAction(7);
const MA_ADDROUTE       = TMouseAction(8);
const MA_ADDBARRIER     = TMouseAction(9);

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    StatusBar1: TStatusBar;
    PanelLeft: TPanel;
    PanelMain: TPanel;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    TSPreview: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelLeftBottom: TPanel;
    Panel7: TPanel;
    Panel6: TPanel;
    Panel5: TPanel;
    Panel8: TPanel;
    Splitter2: TSplitter;
    PanelLeftTop: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    DOMain: TDirectoryOutline;
    CBDriverList: TComboBox;
    TVFileList: TTreeView;
    ImageListFileList: TImageList;
    TimerRefreshSpace: TTimer;
    ImageListPreview: TImageList;
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
    ToolBarEdit: TToolBar;
    TBEditSave: TToolButton;
    TBEditCancel: TToolButton;
    TBEditUndo: TToolButton;
    TBEditRedo: TToolButton;
    TBAddFeature: TToolButton;
    TBSelect: TToolButton;
    TBDelete: TToolButton;
    TBClearSelect: TToolButton;
    TimerEditBar: TTimer;
    TBSymbolUI: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TBCreateNetTopo: TToolButton;
    TBAddRoute: TToolButton;
    TBAddBarrier: TToolButton;
    TBDoBestPath: TToolButton;
    TBClearResult: TToolButton;
    Panel15: TPanel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    EditNewBookmark: TEdit;
    Panel16: TPanel;
    EditTolerance: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel9Resize(Sender: TObject);
    procedure CBDriverListChange(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure DOMainChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PanelSpaceResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure TimerRefreshSpaceTimer(Sender: TObject);
    procedure TVFileListClick(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure PanelSpaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelSpaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PanelSpaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure N4Click(Sender: TObject);
    procedure TBSelectClick(Sender: TObject);
    procedure TBDeleteClick(Sender: TObject);
    procedure TBClearSelectClick(Sender: TObject);
    procedure TBEditSaveClick(Sender: TObject);
    procedure TBEditCancelClick(Sender: TObject);
    procedure TBEditUndoClick(Sender: TObject);
    procedure TBEditRedoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TBAddFeatureClick(Sender: TObject);
    procedure TimerEditBarTimer(Sender: TObject);
    procedure TBSymbolUIClick(Sender: TObject);
    procedure TBCreateNetTopoClick(Sender: TObject);
    procedure TBAddRouteClick(Sender: TObject);
    procedure TBAddBarrierClick(Sender: TObject);
    procedure TBDoBestPathClick(Sender: TObject);
    procedure TBClearResultClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure EditToleranceChange(Sender: TObject);
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
    m_pMoveTracker: IMoveTracker;
    m_pMoveFeatures: IObjArray;
    m_BestPath: IPolyline;

    procedure ReloadMap();
    procedure MainSpaceResize();
    procedure UpdateView();
    procedure UpdateMapLayers(const filename: AnsiString; const filetype: Longint);
    procedure ViewFullMap();
    function GetEditVLA(): IVectorLayerAgent;
    procedure NewFeature(const pointcount: Longint);
    procedure AddNetRoute(route: WKSPoint);
    function AddNetBarrier(barrier: WKSPoint): Boolean;
    procedure AddNetBlock(blocked: WKSPoint);
    procedure DrawBestPath();

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

type
    TSymbolUISetMainWnd = procedure(wnd: HWND); stdcall;
    TSymbolUISelectSymbol = function(var pSymbol: ISymbol): Boolean; stdcall;

type
    TRendererUISetMainWnd = procedure(wnd: HWND); stdcall;
    TRendererUISelectRenderer = function(pVLA: IVectorLayerAgent): Boolean; stdcall;

var
    FormMain: TFormMain;

    //symbolui
    hSymbolUI: HMODULE;
    SymbolUISetMainWnd: TSymbolUISetMainWnd;
    SymbolUISelectSymbol: TSymbolUISelectSymbol;

    //rendererui
    hRendererUI: HMODULE;
    RendererUISetMainWnd: TRendererUISetMainWnd;
    RendererUISelectRenderer: TRendererUISelectRenderer;

implementation

{$R *.DFM}
{$R cursors.res}

function SeekFileByExt(pathname: AnsiString; seekext: AnsiString;
  filenames: TStringList): Integer;
var
    sFileName       : AnsiString;
    FindFileData    : WIN32_FIND_DATA;
    hFound          : THandle;
    count           : Integer;
    r               : LongBool;
begin
    count := 0;

    sFileName := PChar(pathname) + '\*.' + seekext;
    hFound := Windows.FindFirstFile(PChar(sFileName), FindFileData);
    if (hFound = INVALID_HANDLE_VALUE) then begin
        Result := count;
        Exit;
    end;

    repeat
        if ((FILE_ATTRIBUTE_DIRECTORY and FindFileData.dwFileAttributes) = 0) then begin
            //不是子目录
            if (filenames <> nil) then begin
                filenames.Add(AnsiString(FindFileData.cFileName));
            end;
            Inc(count);
        end;

        r := Windows.FindNextFile(hFound, FindFileData);

    until(not r);
    Windows.FindClose(hFound);

    Result := count;

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

function GetVLA(const pLayer: ILayer): IVectorLayerAgent;
var
    pObj: IObj;
    pVLA: IVectorLayerAgent;
    pCheck: ILayer;
begin
    Result := nil;
    CreateObj('CVectorLayerAgent', pObj);
    pVLA := IVectorLayerAgent(GotoInterface(pObj, 'IVectorLayerAgent'));
    pVLA.SetLayer(pLayer);
    pVLA.GetLayer(pCheck);
    if (Assigned(pCheck)) then begin
        Result := pVLA;
        Exit;
    end;
end;

function TFormMain.GetEditVLA(): IVectorLayerAgent;
var
    pLayer: ILayer;
begin
    m_pMap.GetLayer(pLayer, 0);
    Result := GetVLA(pLayer);
end;

procedure TFormMain.ReloadMap();
begin
    m_pAV := IActiveView(GotoInterface(m_pMap, 'IActiveView'));
    m_pAV.GetDisplay(m_pDisplay);
    m_pDisplay.GetDisplayTransformation(m_pDT);
    m_pMoveTracker.SetDisplay(m_pDisplay);
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
var
    pObj: IObj;
    center: WKSPoint;
    pPoint: IPoint;
begin
    m_pAV.UpdateData(nil);
    m_pAV.UpdateSelection(nil);

    CreateObj('CPoint', pObj);
    pPoint := IPoint(GotoInterface(pObj, 'IPoint'));
    m_pDT.GetRotateCenter(center);
    pPoint.SetX(center.x);
    pPoint.SetY(center.y);
    m_pDisplay.StartDraw();
    m_pDisplay.DrawGeometry(pPoint);
    m_pDisplay.FinishDraw();

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

procedure TFormMain.UpdateMapLayers(const filename: AnsiString; const filetype: Longint);
var
    pObj: IObj;
    pStream: IStreamX;
    pPersist: IPersist;
    pVLA: IVectorLayerAgent;
    pBMPLayer: IBitmapLayer;
    pLayer: ILayer;
//    pLabelLayer: ILabelLayer;
//    pLabelManager: ILabelLayerManager;
begin
    m_pMap.ClearAllData();
    case filetype of
        0: begin
            TSPreview.Visible := True;
            CreateObj('CMemoryStream', pObj);
            pStream := IStreamX(GotoInterface(pObj, 'IStreamX'));
            pStream.LoadFromFile(PChar(filename));
            Instantiate(pStream, pPersist);
            if (not Assigned(pPersist)) then Exit;
            m_pMap := IMap(GotoInterface(pPersist, 'IMap'));
            Self.ReloadMap();
            Self.MainSpaceResize();
        end;

        1, 2: begin
            TSPreview.Visible := True;
            CreateObj('CVectorLayerAgent', pObj);
            pVLA := IVectorLayerAgent(GotoInterface(pObj, 'IVectorLayerAgent'));
            if (filetype = 1) then begin
                if (not pVLA.LoadSlimLayer(PChar(filename), False, True)) then begin
                    pVLA.LoadSlimLayer(PChar(filename), True, True);
                end;
            end
            else begin
                if (not pVLA.LoadShapeLayer(PChar(filename), UNIT_M, 2000, 0.01, 5, False)) then begin
                    pVLA.LoadShapeLayer(PChar(filename), UNIT_M, 2000, 0.01, 5, True);
                end;
            end;

            pVLA.RestoreNetTopo();

            pVLA.GetLayer(pLayer);
            m_pMap.AddLayer(pLayer);

(*
            //测试自动标注
            CreateObj('CLabelLayer', pObj);
            pLabelLayer := ILabelLayer(GotoInterface(pObj, 'ILabelLayer'));
            pLabelLayer.SetVectorLayer(pLayer);
            pLabelLayer.SetFieldIndex(1);
//            pLabelLayer.SetRefScale(10000000);

//            m_pMap.AddLayer(pLabelLayer);

            pLabelManager := ILabelLayerManager(GotoInterface(m_pMap, 'ILabelLayerManager'));
            pLabelManager.ClearLabelLayers();
            pLabelManager.AddLabelLayer(pLabelLayer);
*)
            Self.ViewFullMap();
        end;

        3: begin
            TSPreview.Visible := True;
            CreateObj('CBitmapLayer', pObj);
            pBMPLayer := IBitmapLayer(GotoInterface(pObj, 'IBitmapLayer'));
            pBMPLayer.LoadBmpFile(PChar(filename), True);
            pLayer := ILayer(GotoInterface(pBMPLayer, 'ILayer'));
            m_pMap.AddLayer(pBMPLayer);
            Self.ViewFullMap();
        end;

        else begin
            Self.UpdateView();
            TSPreview.Visible := False;
        end
    end;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
    drivenames, p: PChar;
    drivename: AnsiString;
    pObj: IObj;
begin
    PanelMain.Align := alClient;
    PageControl1.Align := alClient;
    PanelLeftTop.Align := alClient;
    DOMain.Align := alClient;
    PanelSpace.Align := alClient;
    TVFileList.Align := alClient;

    CreateObj('CObjArray', pObj);
    m_pMoveFeatures := IObjArray(GotoInterface(pObj, 'IObjArray'));

    pObj := nil;
    m_SpaceDC := GetDC(PanelSpace.Handle);
    CreateObj('CGeoMap', pObj);
    m_pMap := IMap(GotoInterface(pObj, 'IMap'));
    pObj := nil;
    CreateObj('CMoveTracker', pObj);
    m_pMoveTracker := IMoveTracker(GotoInterface(pObj, 'IMoveTracker'));
    Self.ReloadMap();

    GetMem(drivenames, 2000);
    GetLogicalDriveStrings(2000, drivenames);
    p := drivenames;

    drivename := AnsiString(p);
    drivename := Copy(drivename, 1, Length(drivename) - 1);
    CBDriverList.Items.Add(drivename);
    Inc(p);
    while (True) do begin
        if (Byte(p^) = 0) then begin
            Inc(p);
            if (Byte(p^) = 0) then begin
                Break;
            end;
            drivename := AnsiString(p);
            drivename := Copy(drivename, 1, Length(drivename) - 1);
            CBDriverList.Items.Add(drivename);
        end;
        Inc(p);
    end;

    if (CBDriverList.Items.Count > 0) then begin
        CBDriverList.ItemIndex := 0;
        try
            Self.CBDriverListChange(Self);
        except
        end;
    end;

    FreeMem(drivenames, 2000);

    m_MouseAction := MA_NORMOL;
    PanelSpace.Cursor := MOUSECURSOR_DEFAULT;
    m_MouseDown := False;
    m_MouseDownCount := 0;

    if (nil <> @SymbolUISetMainWnd) then
        SymbolUISetMainWnd(Self.Handle);

    if (nil <> @RendererUISetMainWnd) then
        RendererUISetMainWnd(Self.Handle);

    m_InitOK := True;
    Self.MainSpaceResize();
    TSPreview.Visible := False;
    TimerEditBar.Enabled := True;
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
end;

procedure TFormMain.Panel9Resize(Sender: TObject);
begin
    CBDriverList.Width := Panel9.Width - 15;
end;

procedure TFormMain.CBDriverListChange(Sender: TObject);
begin
    DOMain.Directory := CBDriverList.Text;
end;

procedure TFormMain.N2Click(Sender: TObject);
begin
    Self.Close();
end;

procedure TFormMain.DOMainChange(Sender: TObject);
var
    filelist: TStringList;
    node: TTreeNode;
    i: Longint;
begin
    TVFileList.Items.Clear();
    filelist := TStringList.Create();

    filelist.Clear();
    SeekFileByExt(DOMain.Directory, 'ews', filelist);
    for i := 0 to filelist.Count - 1 do begin
        node := TVFileList.Items.AddChild(nil, filelist.Strings[i]);
        node.ImageIndex := 0;
        node.SelectedIndex := 0;
    end;

    filelist.Clear();
    SeekFileByExt(DOMain.Directory, 'esd', filelist);
    for i := 0 to filelist.Count - 1 do begin
        node := TVFileList.Items.AddChild(nil, filelist.Strings[i]);
        node.ImageIndex := 1;
        node.SelectedIndex := 1;
    end;

    filelist.Clear();
    SeekFileByExt(DOMain.Directory, 'shp', filelist);
    for i := 0 to filelist.Count - 1 do begin
        node := TVFileList.Items.AddChild(nil, filelist.Strings[i]);
        node.ImageIndex := 2;
        node.SelectedIndex := 2;
    end;

    filelist.Clear();
    SeekFileByExt(DOMain.Directory, 'bmp', filelist);
    for i := 0 to filelist.Count - 1 do begin
        node := TVFileList.Items.AddChild(nil, filelist.Strings[i]);
        node.ImageIndex := 3;
        node.SelectedIndex := 3;
    end;

    filelist.Free();
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    ReleaseDC(PanelSpace.Handle, m_SpaceDC);
end;

procedure TFormMain.PanelSpaceResize(Sender: TObject);
begin
    Self.MainSpaceResize();
end;

procedure TFormMain.FormPaint(Sender: TObject);
begin
    TimerRefreshSpace.Enabled := True;
end;

procedure TFormMain.TimerRefreshSpaceTimer(Sender: TObject);
begin
    TimerRefreshSpace.Enabled := False;
    m_pAV.RefreshWindow();
    Self.DrawBestPath();
end;

procedure TFormMain.DrawBestPath();
var
    pLayer: ILayer;
    pVLA: IVectorLayerAgent;
    pMP: IMultiPoint;
    pGeometry: IGeometry;
    pES: IEnvelopePointSymbol;
    pSLS: ISimpleLineSymbol;
    pObj: IObj;
    pAV: IActiveView;
    pDisplay: IDisplay;
    pDT: IDisplayTransformation;
    blocked_fids: IIntArray;
    i, count: Longword;
    fid: Longint;
    pFeature: IVectorFeature;
begin
    pAV := IActiveView(GotoInterface(m_pMap, 'IActiveView'));
    pAV.GetDisplay(pDisplay);
    pDisplay.GetDisplayTransformation(pDT);

    if (Assigned(m_BestPath)) then begin
        CreateObj('CSimpleLineSymbol', pObj);
        pSLS := ISimpleLineSymbol(GotoInterface(pObj, 'ISimpleLineSymbol'));
        pSLS.SetColor(clBlue);
        pSLS.SetWidth(1.5);
        pSLS.Prepare(m_SpaceDC, pDT, R2_COPYPEN);
        pSLS.Draw(m_BestPath);
    end;

    m_pMap.GetLayer(pLayer, 0);
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        Exit;
    end;

    pVLA.GetNetBlockedEdgeIDs(blocked_fids);
    count := blocked_fids.GetSize();
    if (count > 0) then begin
        for i := 0 to count - 1 do begin
            blocked_fids.GetAt(i, fid);
            pVLA.GetFeature(fid, pFeature);
            pFeature.GetGeometryRef(pGeometry);

            CreateObj('CSimpleLineSymbol', pObj);
            pSLS := ISimpleLineSymbol(GotoInterface(pObj, 'ISimpleLineSymbol'));
            pSLS.SetColor(clRed);
            pSLS.SetWidth(1.0);
            pSLS.Prepare(m_SpaceDC, pDT, R2_COPYPEN);
            pSLS.Draw(pGeometry);
        end;
    end;
    
    CreateObj('CEnvelopePointSymbol', pObj);
    pES := IEnvelopePointSymbol(GotoInterface(pObj, 'IEnvelopePointSymbol'));
    pES.SetWidth(2.8);
    pES.SetHeight(2.8);
    pES.SetSolid(True);

    pES.Prepare(m_SpaceDC, pDT, R2_COPYPEN);
    pES.SetColor(clGreen);
    pVLA.GetNetRoutes(pMP);
    pGeometry := IGeometry(GotoInterface(pMP, 'IGeometry'));
    pES.Draw(pGeometry);

    pES.SetColor(clRed);
    pMP := nil;
    pVLA.GetNetBarrierPoints(pMP);
    pGeometry := IGeometry(GotoInterface(pMP, 'IGeometry'));
    pES.Draw(pGeometry);
end;

procedure TFormMain.TVFileListClick(Sender: TObject);
var
    filename: AnsiString;
    r: Longint;
begin
    if (m_pMap.IsDirty()) then begin
        r := MessageDlg('编辑没有保存，保存否？', mtConfirmation, [mbYes, mbNo], 0);
        if (r = mrYes) then begin
            m_pMap.SaveData();
        end
        else begin
            m_pMap.EditCancel();
        end;
    end;

    m_pMap.ClearAllData();
    if (not Assigned(TVFileList.Selected)) then begin
        Self.UpdateView();
        TSPreview.Visible := False;
        Exit;
    end;

    filename := DOMain.Directory + '\' + TVFileList.Selected.Text;
    Self.UpdateMapLayers(filename, TVFileList.Selected.ImageIndex);
    TimerRefreshSpace.Enabled := True;
    m_MouseAction := MA_NORMOL;
    PanelSpace.Cursor := MOUSECURSOR_DEFAULT;
end;

procedure TFormMain.ToolButton7Click(Sender: TObject);
begin
    Self.ViewFullMap();
    Self.DrawBestPath();
end;

procedure TFormMain.ToolButton8Click(Sender: TObject);
begin
    Self.UpdateView();
    Self.DrawBestPath();
end;

procedure TFormMain.ToolButton12Click(Sender: TObject);
var
    scale: Double;
begin
    m_pDT.GetMapScale(scale);
    scale := scale / 2;
    m_pDT.SetMapScale(scale);
    Self.UpdateView();
    Self.DrawBestPath();
end;

procedure TFormMain.ToolButton11Click(Sender: TObject);
var
    scale: Double;
begin
    m_pDT.GetMapScale(scale);
    scale := scale * 2;
    m_pDT.SetMapScale(scale);
    Self.UpdateView();
    Self.DrawBestPath();
end;

procedure TFormMain.ToolButton4Click(Sender: TObject);
begin
    m_MouseAction := MA_ZOOM;
    PanelSpace.Cursor := MOUSECURSOR_ZOOMIN;

    m_MouseDown := False;
    m_MouseDownCount := 0;
    m_pMoveTracker.Finish();
    m_pMoveFeatures.Clear();
    m_pMoveTracker.ClearGeometry();
end;

procedure TFormMain.ToolButton6Click(Sender: TObject);
begin
    m_MouseAction := MA_PAN;
    PanelSpace.Cursor := MOUSECURSOR_HAND;

    m_MouseDown := False;
    m_MouseDownCount := 0;
    m_pMoveTracker.Finish();
    m_pMoveFeatures.Clear();
    m_pMoveTracker.ClearGeometry();
end;

procedure TFormMain.PanelSpaceMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    pScreenBrowser: IScreenBrowser;
    pVLA: IVectorLayerAgent;
    pIntArray: IIntArray;
    fid, i, count: Longint;
    pFeature: IVectorFeature;
    pObj: IObj;
    pGeometry, pGeoClone: IGeometry;
    pDT: IDisplayTransformation;
    map_point: WKSPoint;
begin
    m_MouseDown := True;
    pScreenBrowser := IScreenBrowser(GotoInterface(m_pDisplay, 'IScreenBrowser'));
    m_pMoveFeatures.Clear();
    m_pMoveTracker.ClearGeometry();

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

        MA_SELECTFEATURE: begin
            m_MouseDownCount := 0;
            m_MouseDownPos[0].x := X;
            m_MouseDownPos[0].y := Y;
            if (Button = mbRight) then begin
                //移动选择的地物
                m_MouseDown := False;
                pVLA := Self.GetEditVLA();
                pVLA.GetSelection(pIntArray);
                if (not Assigned(pVLA)) then
                    Exit;

                count := pIntArray.GetSize();
                if (count > 0) then begin
                    for i := 0 to count - 1 do begin
                        pIntArray.GetAt(i, fid);
                        pVLA.GetFeature(fid, pFeature);
                        m_pMoveFeatures.Add(pFeature);
                        pFeature.GetGeometryRef(pGeometry);
                        pGeometry.Clone(pObj);
                        pGeoClone := IGeometry(GotoInterface(pObj, 'IGeometry'));
                        m_pMoveTracker.AddGeometryRef(pGeoClone);
                        pFeature := nil;
                        pGeometry := nil;
                        pObj := nil;
                    end;
                    m_pMoveTracker.Start(X, Y);
                end;
            end;
        end;

        MA_ADDPOINT: begin
            m_MouseDownPos[0].x := X;
            m_MouseDownPos[0].y := Y;
            m_MouseDownCount := 0;
            Self.NewFeature(1);
        end;

        MA_ADDLINE, MA_ADDPOLYGON: begin
            if (Button = mbLeft) then begin
                m_MouseDownPos[m_MouseDownCount].x := X;
                m_MouseDownPos[m_MouseDownCount].y := Y;
                Inc(m_MouseDownCount);
            end
            else begin
                Self.NewFeature(m_MouseDownCount);
                m_MouseDownCount := 0;
                m_pAV.RefreshWindow();
            end;
        end;

        MA_ADDROUTE: begin
            Self.AddNetRoute(map_point);
            m_MouseDownCount := 0;
            m_pAV.RefreshWindow();
        end;

        MA_ADDBARRIER: begin
            if (not Self.AddNetBarrier(map_point)) then begin
                Self.AddNetBlock(map_point);
            end;
            m_MouseDownCount := 0;
            m_pAV.RefreshWindow();
        end;

        else begin
            m_MouseDownCount := 0;
        end;
    end;
end;

procedure TFormMain.AddNetRoute(route: WKSPoint);
var
    pLayer: ILayer;
    pVLA: IVectorLayerAgent;
begin
    m_pMap.GetLayer(pLayer, 0);
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        Exit;
    end;

    pVLA.AddNetRoute(route);
    TimerRefreshSpace.Enabled := True;
end;

function TFormMain.AddNetBarrier(barrier: WKSPoint): Boolean;
var
    pLayer: ILayer;
    pVLA: IVectorLayerAgent;
begin
    Result := False;
    m_pMap.GetLayer(pLayer, 0);
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        Exit;
    end;

    Result := pVLA.AddNetBarrierPoint(barrier);
end;

procedure TFormMain.AddNetBlock(blocked: WKSPoint);
var
    pLayer: ILayer;
    pVLA: IVectorLayerAgent;
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
    m_pMap.GetLayer(pLayer, 0);
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        Exit;
    end;

    m_pDT.Map2Device(blocked, point_dev);
    rect_dev.Left := point_dev.x - 1;
    rect_dev.Right := point_dev.x + 1;
    rect_dev.Top := point_dev.y - 1;
    rect_dev.Bottom := point_dev.y + 1;
    m_pDT.Device2MapXY(rect_dev.Left, rect_dev.Bottom, rect.left, rect.bottom);
    m_pDT.Device2MapXY(rect_dev.Right, rect_dev.top, rect.right, rect.top);
    pVLA.Identify(pFids, rect, true);
    if (pFids.GetSize() = 0) then begin
        Exit;
    end;
    pFids.GetAt(0, fid);
    pVLA.GetFeature(fid, pFeature);
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

    pVLA.AddNetBlockedSingleEdge(_from, _to);
    pVLA.AddNetBlockedSingleEdge(_to, _from);
end;

procedure TFormMain.PanelSpaceMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
    rect: TRect;
    pnt: TPoint;
    pScreenBrowser: IScreenBrowser;
    i: Longint;
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

        MA_SELECTFEATURE: begin
            if (m_pMoveTracker.Started) then begin
                m_pMoveTracker.MouseMove(X, Y);
            end
            else if (m_MouseDown) then begin
                m_pAV.RefreshWindow();
                DrawEnvelope(m_SpaceDC, rect, clBlue, 1, True, 0);
            end;
        end;

        MA_ADDLINE, MA_ADDPOLYGON: begin
            if (m_MouseDownCount > 0) then begin
                m_pAV.RefreshWindow();
                MoveToEx(m_SpaceDC, m_MouseDownPos[0].x, m_MouseDownPos[0].y, nil);
                for i := 1 to m_MouseDownCount-1 do begin
                    LineTo(m_SpaceDC, m_MouseDownPos[i].x, m_MouseDownPos[i].y);
                end;

                LineTo(m_SpaceDC, X, Y);
                if (MA_ADDPOLYGON = m_MouseAction) then begin
                    LineTo(m_SpaceDC, m_MouseDownPos[0].x, m_MouseDownPos[0].y);
                end;
            end;
        end;

        MA_PAN: begin
            pScreenBrowser.PanMoveTo(pnt);
        end;
    end;
    
end;

procedure TFormMain.PanelSpaceMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    rect: TRect;
    pScreenBrowser: IScreenBrowser;
    wrect: WKSRect;
    i, count: Longint;
    pFeature: IVectorFeature;
    pGeometry: IGeometry;
    pObj: IObj;
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

        MA_SELECTFEATURE: begin
            if (Button = mbLeft) then begin
                //选择
//                m_pDT.Device2MapXY(rect.Left, rect.Bottom, wrect.left, wrect.bottom);
//                m_pDT.Device2MapXY(rect.Right, rect.top, wrect.right, wrect.top);
                m_pDT.Device2Map_Envelope(rect, wrect);
                m_pMap.Select(wrect, True, False);
            end
            else if (m_pMoveTracker.Started) then begin
                //移动
                m_pMoveTracker.MouseMove(X, Y);
                m_pMoveTracker.Finish();
                m_pMoveTracker.ResetIterator();
                count := m_pMoveFeatures.GetSize();
                for i := 0 to count - 1 do begin
                    m_pMoveTracker.NextGeometryRef(pGeometry);
                    m_pMoveFeatures.GetAt(i, pObj);
                    pFeature := IVectorFeature(GotoInterface(pObj, 'IVectorFeature'));
                    pFeature.SetGeometryRef(pGeometry);
                    pFeature.Update();
                end;
                m_pMap.SetUndoPoint('move');
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

    m_pMoveTracker.Finish();
    m_pMoveFeatures.Clear();
    m_pMoveTracker.ClearGeometry();
    Self.TimerRefreshSpaceTimer(nil);
end;

procedure TFormMain.N4Click(Sender: TObject);
begin
    showmessage('Oops');
end;

procedure TFormMain.TBSelectClick(Sender: TObject);
begin
    m_MouseAction := MA_SELECTFEATURE;
    PanelSpace.Cursor := MOUSECURSOR_CROSS2;

    m_MouseDown := False;
    m_MouseDownCount := 0;
    m_pMoveTracker.Finish();
    m_pMoveFeatures.Clear();
    m_pMoveTracker.ClearGeometry();
end;

procedure TFormMain.TBDeleteClick(Sender: TObject);
var
    pLayer: ILayer;
    pVLA: IVectorLayerAgent;
    fids: IIntArray;
    fid, i, fidcount: Longint;
begin
    m_pMap.GetLayer(pLayer, 0);
    pVLA := GetVLA(pLayer);
    pVLA.GetSelection(fids);
    fidcount := fids.GetSize();
    for i := 0 to fidcount-1 do begin
        fids.GetAt(i, fid);
        pVLA.DeleteFeature(fid);
    end;

    m_pMap.SetUndoPoint('oops');
    Self.UpdateView();
end;


procedure TFormMain.TBClearSelectClick(Sender: TObject);
begin
    m_pMap.ClearSelection();
    Self.UpdateView();
end;

procedure TFormMain.TBEditSaveClick(Sender: TObject);
begin
    m_pMap.SaveData();
    Self.UpdateView();
end;

procedure TFormMain.TBEditCancelClick(Sender: TObject);
begin
    m_pMap.EditCancel();
    Self.UpdateView();
end;

procedure TFormMain.TBEditUndoClick(Sender: TObject);
begin
    m_pMap.EditUndo();
    Self.UpdateView();
end;

procedure TFormMain.TBEditRedoClick(Sender: TObject);
begin
    m_pMap.EditRedo();
    Self.UpdateView();
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
    r: Longint;
begin
    CanClose := False;
    if (m_pMap.IsDirty()) then begin
        r := MessageDlg('编辑没有保存，保存否？', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
        if (r = mrYes) then begin
            m_pMap.SaveData();
            CanClose := True;
        end
        else if (r = mrNo) then begin
            CanClose := True;
        end;
    end
    else begin
        CanClose := True;
    end;
end;

procedure TFormMain.NewFeature(const pointcount: Longint);
var
    pLayer: ILayer;
    pVLA: IVectorLayerAgent;
    pFeature: IVectorFeature;
    wpnt: WKSPoint;
    pGeometry: IGeometry;
    pPoint: IPoint;
    pPath: IPath;
    pRing: IRing;
    pPolyline: IPolyline;
    pPolygon: IPolygon;
    wpntz: WKSPointZ;
    i: Longint;
begin
    m_pMap.GetLayer(pLayer, 0);
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        Exit;
    end;
    if (pVLA.ReadOnly()) then begin
        Exit;
    end;

    pVLA.CreateFeature(pFeature);
    case m_MouseAction of
        MA_ADDPOINT: begin
            m_pDT.Device2Map(m_MouseDownPos[0], wpnt);
            CreateGeometry(GEOMETRYTYPE_POINT, pGeometry);
            pPoint := IPoint(GotoInterface(pGeometry, 'IPoint'));
            pPoint.SetX(wpnt.x);
            pPoint.SetY(wpnt.y);
            pFeature.SetGeometryRef(pGeometry);
            pFeature.Update();
        end;

        MA_ADDLINE: begin
            if (pointcount < 2) then
                Exit;

            CreateGeometry(GEOMETRYTYPE_PATH, pGeometry);
            pPath := IPath(GotoInterface(pGeometry, 'IPath'));
            for i := 0 to pointcount-1 do begin
                m_pDT.Device2MapXY(m_MouseDownPos[i].x, m_MouseDownPos[i].y,
                    wpntz.x, wpntz.y);
                wpntz.z := 0;
                pPath.AddPoint(wpntz, VERTEXTYPE_COMMON);
            end;

            pGeometry := nil;
            CreateGeometry(GEOMETRYTYPE_POLYLINE, pGeometry);
            pPolyline := IPolyline(GotoInterface(pGeometry, 'IPolyline'));
            pPolyline.AddPathRef(pPath);
            pFeature.SetGeometryRef(pGeometry);
            pFeature.Update();
        end;

        MA_ADDPOLYGON: begin
            if (pointcount < 3) then
                Exit;

            CreateGeometry(GEOMETRYTYPE_RING, pGeometry);
            pRing := IRing(GotoInterface(pGeometry, 'IRing'));
            for i := 0 to pointcount-1 do begin
                m_pDT.Device2MapXY(m_MouseDownPos[i].x, m_MouseDownPos[i].y,
                    wpntz.x, wpntz.y);
                wpntz.z := 0;
                pRing.AddPoint(wpntz, VERTEXTYPE_COMMON);
            end;

            m_pDT.Device2MapXY(m_MouseDownPos[0].x, m_MouseDownPos[0].y,
                wpntz.x, wpntz.y);
            wpntz.z := 0;
            pRing.AddPoint(wpntz, VERTEXTYPE_COMMON);

            pGeometry := nil;
            CreateGeometry(GEOMETRYTYPE_POLYGON, pGeometry);
            pPolygon := IPolygon(GotoInterface(pGeometry, 'IPolygon'));
            pPolygon.AddRingRef(pRing);
            pFeature.SetGeometryRef(pGeometry);
            pFeature.Update();
        end;

        else begin
            Exit;
        end;
    end;

    m_pMap.SetUndoPoint('add feature');
    Self.UpdateView();
end;

procedure TFormMain.TBAddFeatureClick(Sender: TObject);
var
    pLayer: ILayer;
    pVLA: IVectorLayerAgent;
    geocolinfo: TGeometryColumnInfo;
begin
    m_MouseDown := False;
    m_MouseDownCount := 0;
    m_pMoveTracker.Finish();
    m_pMoveFeatures.Clear();
    m_pMoveTracker.ClearGeometry();
    m_MouseAction := MA_NORMOL;
    PanelSpace.Cursor := MOUSECURSOR_DEFAULT;

    m_pMap.GetLayer(pLayer, 0);
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        Exit;
    end;
    if (pVLA.ReadOnly()) then begin
        Exit;
    end;

    pVLA.GetGeometryColumnInfo(geocolinfo);
    if (geocolinfo.FeatureType = VECTORFEATURETYPE_TEXT) then begin
        m_MouseAction := MA_ADDANNOTATION;
    end
    else begin
        case geocolinfo.ShpType of
            SHAPETYPE_POINT: begin
                m_MouseAction := MA_ADDPOINT;
                PanelSpace.Cursor := MOUSECURSOR_CROSS1;
            end;

            SHAPETYPE_POLYLINE: begin
                m_MouseAction := MA_ADDLINE;
                PanelSpace.Cursor := MOUSECURSOR_CROSS1;
            end;

            SHAPETYPE_POLYGON: begin
                m_MouseAction := MA_ADDPOLYGON;
                PanelSpace.Cursor := MOUSECURSOR_CROSS1;
            end;
        end;
    end;
end;

procedure TFormMain.TimerEditBarTimer(Sender: TObject);
var
    pLayer: ILayer;
    pVLA: IVectorLayerAgent;
begin
    if (m_pMap.GetLayerCount() < 1) then begin
        ToolBarEdit.Visible := False;
        Exit;
    end;

    m_pMap.GetLayer(pLayer, 0);
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        ToolBarEdit.Visible := False;
        Exit;
    end;

    if (pVLA.ReadOnly()) then begin
        ToolBarEdit.Visible := False;
    end;

    if (m_pMap.IsDirty()) then begin
        TBEditSave.Enabled := True;
        TBEditCancel.Enabled := True;
        if (m_pMap.EditUndoable()) then begin
            TBEditUndo.Enabled := True;
        end
        else begin
            TBEditUndo.Enabled := False;
        end;

        if (m_pMap.EditRedoable()) then begin
            TBEditRedo.Enabled := True;
        end
        else begin
            TBEditRedo.Enabled := False;
        end;
    end
    else begin
        TBEditSave.Enabled := False;
        TBEditCancel.Enabled := False;
        TBEditUndo.Enabled := False;
        TBEditRedo.Enabled := False;
    end;

    ToolBarEdit.Visible := True;
end;

procedure TFormMain.TBSymbolUIClick(Sender: TObject);
var
    pLayer: ILayer;
    pVLA: IVectorLayerAgent;
begin
    m_pMap.GetLayer(pLayer, 0);
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then
        Exit;

    if (nil = @RendererUISelectRenderer) then begin
        ShowMessage('no rendererui.dll');
        Exit;
    end;

    RendererUISelectRenderer(pVLA);
    Self.UpdateView();
end;

procedure TFormMain.TBCreateNetTopoClick(Sender: TObject);
var
    pVLA: IVectorLayerAgent;
begin
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        Exit;
    end;

    pVLA.CreateNetTopo(-1, True);
    pVLA.SetNetTolerance(StrToFloat(EditTolerance.Text));

(*
    //测试拓扑网络的存储
    pVLA.StoreNetTopo();
    pVLA.ClearNetTopo();
    pVLA.RestoreNetTopo();
*)
end;

procedure TFormMain.TBAddRouteClick(Sender: TObject);
begin
    m_MouseAction := MA_ADDROUTE;
    PanelSpace.Cursor := MOUSECURSOR_ARROW2;

    m_MouseDown := False;
    m_MouseDownCount := 0;
    m_pMoveTracker.Finish();
    m_pMoveFeatures.Clear();
    m_pMoveTracker.ClearGeometry();
end;

procedure TFormMain.TBAddBarrierClick(Sender: TObject);
begin
    m_MouseAction := MA_ADDBARRIER;
    PanelSpace.Cursor := MOUSECURSOR_ARROW1;

    m_MouseDown := False;
    m_MouseDownCount := 0;
    m_pMoveTracker.Finish();
    m_pMoveFeatures.Clear();
    m_pMoveTracker.ClearGeometry();
end;

procedure TFormMain.TBDoBestPathClick(Sender: TObject);
var
    pVLA: IVectorLayerAgent;
    pFids: IIntArray;
    i, j, count: Longword;
    fid: Longint;
    pFeature: IVectorFeature;
    pGeometry: IGeometry;
    pPath: IPath;
    pPolylineTmp: IPolyline;
begin
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        Exit;
    end;

    pVLA.DoBestPath(pPath, pFids);
    CreateGeometry(GEOMETRYTYPE_POLYLINE, pGeometry);
    m_BestPath := IPolyline(GotoInterface(pGeometry, 'IPolyline'));
    count := pFids.GetSize();
    if (count = 0) then begin
        Self.TimerRefreshSpaceTimer(nil);
        Exit;
    end;
    
    for i := 0 to count - 1 do begin
        pFids.GetAt(i, fid);
        pFeature := nil;
        pVLA.GetFeature(fid, pFeature);
        pGeometry := nil;
        pFeature.GetGeometryRef(pGeometry);
        pPolylineTmp := IPolyline(GotoInterface(pGeometry, 'IPolyline'));

        for j := 0 to pPolylineTmp.GetPathCount() - 1 do begin
            pPath := nil;
            pPolylineTmp.GetPathRef(pPath, j);
            m_BestPath.AddPathRef(pPath);
        end;
    end;

    Self.TimerRefreshSpaceTimer(nil);
end;

procedure TFormMain.TBClearResultClick(Sender: TObject);
var
    pVLA: IVectorLayerAgent;
begin
    m_BestPath := nil;

    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        Exit;
    end;

    pVLA.ClearNetRoutes();
    pVLA.ClearNetBarrierPoints();
    pVLA.ClearNetBlockedEdges();
    Self.TimerRefreshSpaceTimer(nil);
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
    m_pDT.SetPlaneRotate(TrackBar1.Max - TrackBar1.Position);
    UpdateView();
end;

procedure TFormMain.TrackBar2Change(Sender: TObject);
begin
    m_pDT.SetAttitude(TrackBar2.Position);
    UpdateView();
end;

procedure TFormMain.Button1Click(Sender: TObject);
var
    pPB: IPlaceBookmark;
begin
    pPB := IPlaceBookmark(GotoInterface(m_pMap, 'IPlaceBookmark'));
    pPB.AddBookmark(PChar(EditNewBookmark.Text));
//    pPB.DisableActiveBookmarkShow();
    m_pAV.RefreshWindow();
end;

procedure TFormMain.Button2Click(Sender: TObject);
var
    pPB: IPlaceBookmark;
    extent: WKSRect;
begin
    m_pDT.GetVisibleExtent(extent);
    pPB := IPlaceBookmark(GotoInterface(m_pMap, 'IPlaceBookmark'));
    pPB.ClearBookmarks();
    m_pAV.RefreshWindow();
end;

procedure TFormMain.Button3Click(Sender: TObject);
var
    pPB: IPlaceBookmark;
    extent: WKSRect;
begin
    m_pDT.GetVisibleExtent(extent);
    pPB := IPlaceBookmark(GotoInterface(m_pMap, 'IPlaceBookmark'));
    pPB.PreviousBookmark();
    pPB.SetViewToCurrentBookmark();
    Self.UpdateView();
end;

procedure TFormMain.Button4Click(Sender: TObject);
var
    pPB: IPlaceBookmark;
    extent: WKSRect;
begin
    m_pDT.GetVisibleExtent(extent);
    pPB := IPlaceBookmark(GotoInterface(m_pMap, 'IPlaceBookmark'));
    pPB.NextBookmark();
    pPB.SetViewToCurrentBookmark();
    Self.UpdateView();
end;

procedure TFormMain.EditToleranceChange(Sender: TObject);
var
    pVLA: IVectorLayerAgent;
begin
    pVLA := Self.GetEditVLA();
    if (not Assigned(pVLA)) then begin
        Exit;
    end;

    pVLA.SetNetTolerance(StrToFloat(EditTolerance.Text));
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

    hRendererUI := LoadLibrary('rendererui.dll');
    if (0 <> hRendererUI) then begin
        RendererUISetMainWnd := TRendererUISetMainWnd(GetProcAddress(hRendererUI, 'SetMainWnd'));
        RendererUISelectRenderer := TRendererUISelectRenderer(GetProcAddress(hRendererUI, 'SelectRenderer'));
    end
    else begin
        RendererUISetMainWnd := nil;
        RendererUISelectRenderer := nil;
    end;

finalization
    if (0 <> hSymbolUI) then
        FreeLibrary(hSymbolUI);

    if (0 <> hRendererUI) then
        FreeLibrary(hRendererUI);

    FreeEasyLib();

end.
