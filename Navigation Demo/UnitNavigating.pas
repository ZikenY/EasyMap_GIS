unit UnitNavigating;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  InterfaceObj, InterfaceGeometry, InterfaceDisplayTransformation, WKSStructs,
  InterfaceDisplay, InterfaceSymbol, InterfaceLayer, InterfaceLayerAgent,
  InterfaceBitmapLayer, InterfaceMap, InterfaceActiveView, InterfaceTracker,
  InterfaceLabelLayer, easylib, ComCtrls, ToolWin, ExtCtrls, ImgList;

type
  TFormNavigating = class(TForm)
    PanelSpace: TPanel;
    Panel14: TPanel;
    ToolBarBrowse: TToolBar;
    ToolButton12: TToolButton;
    ToolButton11: TToolButton;
    ToolButton8: TToolButton;
    ImageListPreview: TImageList;
    PanelDirection: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure FormShow(Sender: TObject);
    procedure PanelSpaceResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolButton12Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
  private
    { Private declarations }

    m_pMap: IMap;
    m_pAV: IActiveView;
    m_pDisplay: IDisplay;
    m_pDT: IDisplayTransformation;
    m_SpaceDC: HDC;
    m_pCarSymbol: ISymbol;
    m_InitOK: Boolean;

    procedure ReloadMap();
    procedure LoadCarSymbol();

  public
    { Public declarations }

    procedure ClearAll();
    procedure UpdateView();
    procedure AddLayer(const pLayer: ILayer);
    procedure AddLabelLayer(const pLL: ILabelLayer);
    procedure SetMapUnit(const mapunit: TMapUnits);
    procedure SetScale(const scale: Double);
    procedure SetCenter(const centerpoint: WKSPoint);
    procedure Rotate(const degree: Double);
    procedure ClearNavPoints();
    procedure AddNavPoint(const point: WKSPointZ);
  end;

var
  FormNavigating: TFormNavigating;

implementation

{$R *.DFM}

procedure TFormNavigating.ReloadMap();
begin
    m_pAV := IActiveView(GotoInterface(m_pMap, 'IActiveView'));
    m_pAV.GetDisplay(m_pDisplay);
    m_pAV.DrawingHint(False);
    m_pDisplay.GetDisplayTransformation(m_pDT);
end;

procedure TFormNavigating.LoadCarSymbol();
var
    pObj: IObj;
    pSPS: ISimplePointSymbol;
    pMPS: IMultiPointSymbol;
    pSymbolLib: ISymbolLib;
    filename: AnsiString;
begin
    CreateObj('CSimplePointSymbol', pObj);
    pSPS := ISimplePointSymbol(GotoInterface(pObj, 'ISimplePointSymbol'));
    pSPS.SetColor(clFuchsia);
    m_pCarSymbol := pSPS;

    CreateObj('CSymbolLib', pObj);
    pSymbolLib := ISymbolLib(GotoInterface(pObj, 'ISymbolLib'));

    filename := 'cars.sym';
    if (LoadSymbolLib(PChar(filename), pSymbolLib, SYMBOLTYPE_POINT)) then begin
        if (pSymbolLib.GetSymbolCount() >= 2) then begin
            pSymbolLib.GetSymbolRef(m_pCarSymbol, 1);
            pMPS := IMultiPointSymbol(GotoInterface(m_pCarSymbol, 'IMultiPointSymbol'));
            if (Assigned(pMPS)) then begin
                pMPS.SetSize(0.6);
            end;
        end;
    end;
end;

procedure TFormNavigating.FormShow(Sender: TObject);
var
    pObj: IObj;
begin
    if (not m_InitOK) then begin
        pObj := nil;
        m_SpaceDC := GetDC(PanelSpace.Handle);

        CreateObj('CGeoMap', pObj);
        m_pMap := IMap(GotoInterface(pObj, 'IMap'));
        Self.ReloadMap();
        m_pDT.SetAttitude(70);

        m_InitOK := True;
    end;

    Self.PanelSpaceResize(Self);
end;

procedure TFormNavigating.PanelSpaceResize(Sender: TObject);
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

procedure TFormNavigating.ClearAll();
begin
    m_pMap.ClearAllData();
end;

procedure TFormNavigating.UpdateView();
var
    pObj: IObj;
    center: WKSPoint;
    pPoint: IPoint;
begin
    m_pAV.UpdateData(nil);
    m_pAV.UpdateSelection(nil);

    m_pDisplay.SetSymbol(m_pCarSymbol);

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

procedure TFormNavigating.AddLayer(const pLayer: ILayer);
begin
    m_pMap.AddLayer(pLayer);
end;

procedure TFormNavigating.AddLabelLayer(const pLL: ILabelLayer);
var
    pLLM: ILabelLayerManager;
begin
    pLLM := ILabelLayerManager(GotoInterface(m_pMap, 'ILabelLayerManager'));
    pLLM.AddLabelLayer(pLL);
end;

procedure TFormNavigating.SetMapUnit(const mapunit: TMapUnits);
begin
    m_pDT.SetMapUnit(mapunit);
end;

procedure TFormNavigating.SetScale(const scale: Double);
begin
    m_pDT.SetMapScale(scale);
end;

procedure TFormNavigating.SetCenter(const centerpoint: WKSPoint);
var
    center: WKSPoint;
begin
    center := centerpoint;
    m_pDT.SetRotateCenter(center);
end;

procedure TFormNavigating.Rotate(const degree: Double);
var
    deg: Longint;
begin
    m_pDT.SetPlaneRotate(degree);

    deg := Round(degree) mod 360;
    PanelDirection.Caption := '方位：' + FloatToStr(deg) + '度';
end;

procedure TFormNavigating.ClearNavPoints();
var
    pBookmark: IPlaceBookmark;
begin
    pBookmark := IPlaceBookmark(GotoInterface(m_pMap, 'IPlaceBookmark'));
    pBookmark.ClearBookmarks();
end;

procedure TFormNavigating.AddNavPoint(const point: WKSPointZ);
var
    pBookmark: IPlaceBookmark;
    count: Longint;
    sNav: AnsiString;
    extent: WKSRect;
    width, height: Double;
begin
    pBookmark := IPlaceBookmark(GotoInterface(m_pMap, 'IPlaceBookmark'));
    count := pBookmark.GetBookmarkCount();
    sNav := '导航点_' + IntToStr(count + 1);
    m_pDT.GetVisibleExtent(extent);
    width := extent.right - extent.left;
    height := extent.top - extent.bottom;
    extent.left := point.x - width/2;
    extent.right := point.x + width/2;
    extent.bottom := point.y - height/2;
    extent.top := point.y + height/2;

    pBookmark.AddBookmarkEx(extent, PChar(sNav));
end;

procedure TFormNavigating.FormCreate(Sender: TObject);
begin
    m_InitOK := False;
    Self.LoadCarSymbol();    
end;

procedure TFormNavigating.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
    Action := caHide;
end;

procedure TFormNavigating.ToolButton12Click(Sender: TObject);
var
    scale: Double;
begin
    m_pDT.GetMapScale(scale);
    scale := scale / 3;
    m_pDT.SetMapScale(scale);
    Self.UpdateView();
end;

procedure TFormNavigating.ToolButton11Click(Sender: TObject);
var
    scale: Double;
begin
    m_pDT.GetMapScale(scale);
    scale := scale * 3;
    m_pDT.SetMapScale(scale);
    Self.UpdateView();
end;

procedure TFormNavigating.ToolButton8Click(Sender: TObject);
var
    attitude: Double;
begin
    m_pDT.GetAttitude(attitude);
    if (attitude < 0.001) then begin
        attitude := 70;
    end
    else begin
        attitude := 0;
    end;

    m_pDT.SetAttitude(attitude);
    Self.UpdateView();
end;

end.
