unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, easylib, InterfaceObj, InterfaceGeometry, InterfaceDisplayTransformation,
  InterfaceDisplay, InterfaceSymbol, WKSStructs, yyspace, ExtCtrls, Buttons,
  Menus, ColorGrd, ComCtrls, ImgList, SubSymbolAttribForm, NewSymbolForm,
  LineTemplate, SymbolSelector;

type TPointSymbolType = Longint;
const POINTSYMBOLTYPE_UNSUPPORTED   = TPointSymbolType(-1);
const POINTSYMBOLTYPE_SIMPLE        = TPointSymbolType(0);
const POINTSYMBOLTYPE_ENVELOPE      = TPointSymbolType(1);
const POINTSYMBOLTYPE_POLY          = TPointSymbolType(2);
const POINTSYMBOLTYPE_TEXT          = TPointSymbolType(3);
const POINTSYMBOLTYPE_BITMAP        = TPointSymbolType(4);

type TEditorTask = Longint;
const TEDITORTASK_DEFAULT           = TEditorTask(0);
const TEDITORTASK_SELECT            = TEditorTask(1);
const TEDITORTASK_NEWCIRCLESOLID    = TEditorTask(2);
const TEDITORTASK_NEWCIRCLEHOLLOW   = TEditorTask(3);
const TEDITORTASK_NEWENVELOPESOLID  = TEditorTask(4);
const TEDITORTASK_NEWENVELOPEHOLLOW = TEditorTask(5);
const TEDITORTASK_NEWPATH           = TEditorTask(6);
const TEDITORTASK_NEWPOLYGONSOLID   = TEditorTask(7);
const TEDITORTASK_ROTATE            = TEditorTask(8);
const TEDITORTASK_VERTEXEDIT        = TEditorTask(9);

const MAX_UNDO = 10000;

type
  TFormMain = class(TForm)
    PanelMain: TPanel;
    PanelPointSymbolEditor: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    PanelSymbolEditor: TPanel;
    PanelDown: TPanel;
    PanelRight: TPanel;
    PanelRightPreview: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    PanelSymbolPreview: TPanel;
    PanelRightList: TPanel;
    Panel1: TPanel;
    Panel6: TPanel;
    SBAddCircleSolid: TSpeedButton;
    SBAddCircleHollow: TSpeedButton;
    SBAddEnvSolid: TSpeedButton;
    SBAddEnvHollow: TSpeedButton;
    Panel11: TPanel;
    PanelNewLyrStatus: TPanel;
    SBAddPath: TSpeedButton;
    SBAddSolidPolygon: TSpeedButton;
    PanelTop: TPanel;
    SBSelect: TSpeedButton;
    SBEditorZoomIn: TSpeedButton;
    SBEditorZoomOut: TSpeedButton;
    ColorGridDraw: TColorGrid;
    PanelDrawColor: TPanel;
    Panel17: TPanel;
    EditLineWidth: TEdit;
    Label1: TLabel;
    UpDown1: TUpDown;
    SBZoomFit: TSpeedButton;
    Panel18: TPanel;
    Panelx: TPanel;
    Panel20: TPanel;
    PanelLayerCount: TPanel;
    Panel22: TPanel;
    Label2: TLabel;
    EditLayerName: TEdit;
    SBNewSymbolLib: TSpeedButton;
    SBOpenSymbolLib: TSpeedButton;
    SBSaveSymbolLib: TSpeedButton;
    SBSaveAsSymbolLib: TSpeedButton;
    TVSubPointList: TTreeView;
    ImageList1: TImageList;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    PanelRightMiddle: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    Panel26: TPanel;
    Panel27: TPanel;
    PanelSymbolAttrib: TPanel;
    Label3: TLabel;
    EditSymbolCode: TEdit;
    Label4: TLabel;
    EditSymbolName: TEdit;
    SBNewSymbol: TSpeedButton;
    PanelRightBottom: TPanel;
    Panel23: TPanel;
    Panel28: TPanel;
    Panel29: TPanel;
    Panel30: TPanel;
    TVLayersSelected: TTreeView;
    ColorDialog1: TColorDialog;
    SBUndo: TSpeedButton;
    SBRedo: TSpeedButton;
    Panel31: TPanel;
    Panel33: TPanel;
    LVSymbolList: TListView;
    SBRotate: TSpeedButton;
    SBDeleteLayers: TSpeedButton;
    SBVertexEdit: TSpeedButton;
    Panel34: TPanel;
    SBClearSelection: TSpeedButton;
    ImageList2: TImageList;
    SBDeleteLayers2: TSpeedButton;
    Panel3: TPanel;
    CBColorLock: TCheckBox;
    SDSymbolLib: TSaveDialog;
    ODSymbolLib: TOpenDialog;
    SBSelectTree: TSpeedButton;
    SBLayerOrderDown: TSpeedButton;
    SBLayerOrderUp: TSpeedButton;
    PanelLineSymbolEditor: TPanel;
    PanelSubLineList: TPanel;
    Panel12: TPanel;
    Panel21: TPanel;
    Panel35: TPanel;
    SBSubLineUp: TSpeedButton;
    SBSubLineDown: TSpeedButton;
    Panel36: TPanel;
    Panel46: TPanel;
    PanelLineEditorBase: TPanel;
    GroupBox2: TGroupBox;
    PanelLineTemplate: TPanel;
    PanelSubLineColor: TPanel;
    ColorGridSubLineColor: TColorGrid;
    Panel39: TPanel;
    CBSubLineColorLock: TCheckBox;
    Panel40: TPanel;
    Label7: TLabel;
    EditSubLineWidth: TEdit;
    UpDown2: TUpDown;
    Panel41: TPanel;
    Label8: TLabel;
    EditSubLineName: TEdit;
    Panel42: TPanel;
    Label9: TLabel;
    EditSubLineOffset: TEdit;
    UpDown3: TUpDown;
    BTSelectPointSymbol: TButton;
    TVSubLine: TTreeView;
    ImageListLarge: TImageList;
    Panel32: TPanel;
    Panel19: TPanel;
    Label5: TLabel;
    Panel37: TPanel;
    SBSymbolUp: TSpeedButton;
    SBSymbolDown: TSpeedButton;
    Splitter1: TSplitter;
    Panel14: TPanel;
    Panel43: TPanel;
    Panel38: TPanel;
    Panel5: TPanel;
    Panel13: TPanel;
    LabelEditPosX: TLabel;
    LabelEditPosY: TLabel;
    SBClearSelect: TSpeedButton;
    SBModifySubPoint: TSpeedButton;
    SBDeleteSymbol: TSpeedButton;
    GroupBox1: TGroupBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Panel44: TPanel;
    BitBtn4: TBitBtn;
    PanelFillSymbolEditor: TPanel;
    PanelSubFillList: TPanel;
    Panel45: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Panel47: TPanel;
    Panel48: TPanel;
    Panel49: TPanel;
    TVSubFill: TTreeView;
    PanelFillEditorBase: TPanel;
    GBSimpleFill: TGroupBox;
    GroupBox4: TGroupBox;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    Panel58: TPanel;
    BitBtn8: TBitBtn;
    Panel52: TPanel;
    Panel54: TPanel;
    CBSubFillColorLock: TCheckBox;
    Panel56: TPanel;
    Label10: TLabel;
    EditSubFillName: TEdit;
    GBPointFill: TGroupBox;
    GroupBox5: TGroupBox;
    ColorGridSubFillColor: TColorGrid;
    PanelSubFillColor: TPanel;
    GroupBox6: TGroupBox;
    CBFillStyle: TComboBox;
    GroupBox7: TGroupBox;
    CBFillHatch: TComboBox;
    GroupBox8: TGroupBox;
    ColorGridSubBorderColor: TColorGrid;
    PanelSubBorderColor: TPanel;
    Panel55: TPanel;
    Label6: TLabel;
    EditBorderWidth: TEdit;
    UpDown4: TUpDown;
    BTSelectPointSymbol_PointFill: TButton;
    Panel57: TPanel;
    Label11: TLabel;
    EditFillPointSpaceX: TEdit;
    UpDown5: TUpDown;
    Label12: TLabel;
    EditFillPointSpaceY: TEdit;
    UpDown6: TUpDown;
    Panel59: TPanel;
    Label13: TLabel;
    Label14: TLabel;
    EditFillPointOffsetX: TEdit;
    UpDown7: TUpDown;
    EditFillPointOffsetY: TEdit;
    UpDown8: TUpDown;
    GroupBox3: TGroupBox;
    CBPointFillType: TComboBox;
    GroupBox9: TGroupBox;
    CBPointFillBorderAvailable: TCheckBox;
    BTSelectSymbol_PointFillBorder: TButton;
    PanelBottom: TPanel;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    Shape1: TShape;
    Image1: TImage;
    ImageCat1: TImage;
    Image3: TImage;
    TimerCat: TTimer;
    btnSingleSymbolOK: TButton;
    btnSingleSymbolCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PanelSymbolEditorResize(Sender: TObject);
    procedure SBAddCircleSolidClick(Sender: TObject);
    procedure SBAddCircleHollowClick(Sender: TObject);
    procedure SBSelectClick(Sender: TObject);
    procedure SBEditorZoomInClick(Sender: TObject);
    procedure SBEditorZoomOutClick(Sender: TObject);
    procedure SBAddEnvSolidClick(Sender: TObject);
    procedure SBAddEnvHollowClick(Sender: TObject);
    procedure ColorGridDrawChange(Sender: TObject);
    procedure EditLineWidthChange(Sender: TObject);
    procedure SBZoomFitClick(Sender: TObject);
    procedure SBAddPathClick(Sender: TObject);
    procedure SBAddSolidPolygonClick(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure EditLineWidthExit(Sender: TObject);
    procedure EditSymbolCodeExit(Sender: TObject);
    procedure EditSymbolCodeChange(Sender: TObject);
    procedure SBNewSymbolClick(Sender: TObject);
    procedure SBDeleteLayersClick(Sender: TObject);
    procedure PanelDrawColorClick(Sender: TObject);
    procedure SBUndoClick(Sender: TObject);
    procedure SBRedoClick(Sender: TObject);
    procedure TVSubPointListEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure TVSubPointListEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure SBVertexEditClick(Sender: TObject);
    procedure TVLayersSelectedDblClick(Sender: TObject);
    procedure SBNewSymbolLibClick(Sender: TObject);
    procedure LVSymbolListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TVSubPointListClick(Sender: TObject);
    procedure TVLayersSelectedClick(Sender: TObject);
    procedure SBSaveSymbolLibClick(Sender: TObject);
    procedure SBSaveAsSymbolLibClick(Sender: TObject);
    procedure SBOpenSymbolLibClick(Sender: TObject);
    procedure SBRotateClick(Sender: TObject);
    procedure SBSymbolUpClick(Sender: TObject);
    procedure SBSymbolDownClick(Sender: TObject);
    procedure SBSelectTreeClick(Sender: TObject);
    procedure TVSubPointListDblClick(Sender: TObject);
    procedure SBLayerOrderDownClick(Sender: TObject);
    procedure SBLayerOrderUpClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EditSymbolNameChange(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure TVSubLineClick(Sender: TObject);
    procedure EditSubLineNameChange(Sender: TObject);
    procedure EditSubLineWidthChange(Sender: TObject);
    procedure EditSubLineOffsetChange(Sender: TObject);
    procedure PanelSubLineColorClick(Sender: TObject);
    procedure CBSubLineColorLockClick(Sender: TObject);
    procedure ColorGridSubLineColorChange(Sender: TObject);
    procedure SBSubLineUpClick(Sender: TObject);
    procedure SBSubLineDownClick(Sender: TObject);
    procedure TVSubLineDblClick(Sender: TObject);
    procedure SBClearSelectClick(Sender: TObject);
    procedure SBModifySubPointClick(Sender: TObject);
    procedure SBDeleteSymbolClick(Sender: TObject);
    procedure LVSymbolListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BTSelectPointSymbolClick(Sender: TObject);
    procedure PanelSymbolPreviewMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure BitBtn7Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure TVSubFillDblClick(Sender: TObject);
    procedure TVSubFillClick(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BTSelectPointSymbol_PointFillClick(Sender: TObject);
    procedure BTSelectSymbol_PointFillBorderClick(Sender: TObject);
    procedure CBPointFillBorderAvailableClick(Sender: TObject);
    procedure ColorGridSubFillColorChange(Sender: TObject);
    procedure PanelSubFillColorClick(Sender: TObject);
    procedure ColorGridSubBorderColorChange(Sender: TObject);
    procedure CBFillStyleChange(Sender: TObject);
    procedure CBFillHatchChange(Sender: TObject);
    procedure EditBorderWidthChange(Sender: TObject);
    procedure PanelSubBorderColorClick(Sender: TObject);
    procedure EditFillPointSpaceXChange(Sender: TObject);
    procedure EditFillPointSpaceYChange(Sender: TObject);
    procedure EditFillPointOffsetXChange(Sender: TObject);
    procedure EditFillPointOffsetYChange(Sender: TObject);
    procedure CBPointFillTypeChange(Sender: TObject);
    procedure CBSubFillColorLockClick(Sender: TObject);
    procedure EditSubFillNameChange(Sender: TObject);
    procedure TimerCatTimer(Sender: TObject);
    procedure ImageCat1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure btnSingleSymbolOKClick(Sender: TObject);
    procedure btnSingleSymbolCancelClick(Sender: TObject);
  private
    { Private declarations }

    m_InitOK: Boolean;
    m_LibDirty: Boolean;
    m_EditorLBDown: Boolean;
    m_EditorLBDownPos: tagPOINT;
    m_EditorMoveStartPos: tagPOINT;
    m_MoveSelection: Boolean;
    m_pEditorSpace: TyySpace;
    m_pEditorPointSymbol: IMultiPointSymbol;
    m_pEditorLineSymbol: IMultiLineSymbol;
    m_pEditorFillSymbol: IMultiFillSymbol;
    m_pPreviewDisplay: IDisplay;
    m_pSymbolListDisplay: IDisplay;
    m_EditScale: Longint;
    m_EditorTask: TEditorTask;
    m_LineWidth: Double;
    m_selectedbox: array[0..500] of TRect;
    m_arrayofwkspoints: array[0..1000] of WKSPoint;
    m_pEditUndoList: ISymbolLib;
    m_EditListCurrent: Longint;
    m_SelectColor: COLORREF;
    m_VertexColor: COLORREF;
    m_RotateCenterColor: COLORREF;
    m_pSymbolLib: ISymbolLib;
    m_pSymbolLibFileName: AnsiString;
    m_ChangeSymbolProgress: Boolean;
    m_VertexEditSymbol: IPointSymbol;
    m_VertexEditIndex: Longint;
    m_VertexEditStart: Boolean;

    m_pBorderSymbolSaved: ILineSymbol;

    SubSymbolAttrib: TSubSymbolAttrib;
    NewSymbol: TNewSymbol;
    FormLineTemplate: TFormLineTemplate;
    FormSymbolSelector: TFormSymbolSelector;

    procedure NewSymbolLib(const firstsymboltype: TSymbolType); overload;
    procedure NewSymbolLib(const pDefaultSymbol: ISymbol); overload;
    procedure RefreshSymbolList();
    function GetNextSymbolCode(): Longint;
    function GetUndefinedSymbolName(): AnsiString;
    procedure UpdatePointSymbolEditor(const pMultiPointSymbol: IMultiPointSymbol);
    procedure UpdateLineSymbolEditor(const pMultiLineSymbol: IMultiLineSymbol);
    procedure UpdateFillSymbolEditor(const pMultiFillSymbol: IMultiFillSymbol);
    procedure UpdateTextSymbolEditor(const pSimpleTextSymbol: ISimpleTextSymbol);
    function PushEdit(): Boolean;
    function UndoEdit(): Boolean;
    function RedoEdit(): Boolean;
    procedure RefreshPointLayerList();
    procedure RefreshSubLineList();
    procedure RefreshSubFillList();
    function GetSubLineFromNodeIndex(const nodeindex: Longint;
        out pLineSymbol: ILineSymbol; out symbolindex: Longint): Boolean;
    function GetSelectedSubLine(out pLineSymbol: ILineSymbol;
        out symbolindex: Longint): Boolean;
    function GetSubFillFromNodeIndex(const nodeindex: Longint;
        out pFillSymbol: IFillSymbol; out symbolindex: Longint): Boolean;
    function GetSelectedSubFill(out pFillSymbol: IFillSymbol;
        out symbolindex: Longint): Boolean;
    procedure EnablePointSymbolStuff(const enable: Boolean);
    procedure EnableLineSymbolStuff(const enable: Boolean);
    procedure EnableFillSymbolStuff(const enable: Boolean);
    procedure CheckSelectedPointLayer(const index: Longint);
    procedure ClearSelectedPointLayers();
    function GetSelectedPointLayerByIndex(const index: Longint;
        out pPointSymbol: IPointsymbol; out parentindex: Longint): Boolean;
    function GetPointLayerIndexFromTree(): Longint;
    function GetPointLayerFromTree(): IPointSymbol;
    procedure RotatePointSelectedLayer(const delta: Longint);
    procedure BlowTheShape(pSymbol: IPolyPointSymbol);
    procedure RefreshEditSymbol();
    procedure PointLayerModerfied();
    procedure TemplateModified();
    procedure RefreshLineEditor(const pLineSymbol: ILineSymbol); overload;
    procedure RefreshLineEditor(); overload;
    procedure RefreshFillEditor(const pFillSymbol: IFillSymbol); overload;
    procedure RefreshFillEditor(); overload;


    procedure DrawUnderGrids();
    procedure DrawEditVertex(const vertex: tagPOINT);
    procedure DrawRotateCenter(const center: tagPOINT);
    procedure PointSymbolEditorDraw();
    procedure PrepareSymbolListDisplay(rect: TRect);
    procedure SymbolListDraw(const rect: TRect; const pSymbol: ISymbol);
    procedure PreparePreviewDisplay();
    procedure SymbolPreviewDraw();
    procedure DrawEditorSimplePoint(const pSymbol: IPointSymbol; const selected: Boolean; out mbr: TRect);
    procedure DrawEditorEnvelopePoint(const pSymbol: IPointSymbol; const selected: Boolean; out mbr: TRect);
    procedure DrawEditorBitmapPoint(const pSymbol: IPointSymbol; const selected: Boolean; out mbr: TRect);
    procedure DrawEditorTextPoint(const pSymbol: IPointSymbol; const selected: Boolean; out mbr: TRect);
    procedure DrawEditorPolyPoint(const pSymbol: IPointSymbol; const selected: Boolean; out mbr: TRect);

    procedure SelectLayers(const pnt: WKSPoint);
    procedure SelectSimplePoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
        out selected: Boolean);
    procedure SelectEnvelopePoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
        out selected: Boolean);
    procedure SelectBitmapPoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
        out selected: Boolean);
    procedure SelectTextPoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
        out selected: Boolean);
    procedure SelectPolyPoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
        out selected: Boolean);
    function CheckSelection(wkspnt: WKSPoint): Boolean;
    function CheckLayerSelected(layerindex: Longint): Boolean;
    procedure MoveSelection(offset: WKSPoint);

    procedure DrawEditorVertex(const pSymbol: IPointSymbol);
    procedure DrawEditorSimpleVertex(const pSymbol: IPointSymbol);
    procedure DrawEditorEnvelopeVertex(const pSymbol: IPointSymbol);
    procedure DrawEditorBitmapVertex(const pSymbol: IPointSymbol);
    procedure DrawEditorTextVertex(const pSymbol: IPointSymbol);
    procedure DrawEditorPolyVertex(const pSymbol: IPointSymbol);

    function CheckVertexHit(pnt: WKSPoint): Boolean;
    procedure CheckVertexHitSimplePoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
        out selected: Boolean);
    procedure CheckVertexHitEnvelopePoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
        out selected: Boolean);
    procedure CheckVertexHitBitmapPoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
        out selected: Boolean);
    procedure CheckVertexHitTextPoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
        out selected: Boolean);
    procedure CheckVertexHitPolyPoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
        out selected: Boolean);
    function CheckCursorHitVertex(const cursor: WKSPoint; const vertex: WKSPoint): Boolean;

    procedure MoveVertexSelected(const offset: WKSPoint; const newpos: WKSPoint);
    procedure MoveVertexSimplePoint(pSymbol: ISimplePointSymbol; const offset: WKSPoint);
    procedure MoveVertexEnvelopePoint(pSymbol: IEnvelopePointSymbol; const offset: WKSPoint);
    procedure MoveVertexPolyPoint(pSymbol: IPolyPointSymbol; const newpos: WKSPoint);

    procedure WinPos2EditorPos(x: Longint; y: Longint; out x1: Double; out y1: Double); overload;
    procedure WinPos2EditorPos(var point: tagPOINT; out point1: WKSPoint); overload;
    procedure EditorPos2WinPos(const x: Double; const y: Double; out x1: Longint; out y1: Longint); overload;
    procedure EditorPos2WinPos(var point: WKSPoint; out point1: tagPOINT); overload;

    procedure OnSpacePaint(dc: HDC);
    procedure OnSpaceSize(width, height: Longint);
    procedure OnSpaceMouseMove(Shift: TShiftState; X: Longint; Y: Longint);
    procedure OnSpaceMouseDown(Button: TMouseButton; Shift: TShiftState;
        X: Longint; Y: Longint);
    procedure OnSpaceMouseUp(Button: TMouseButton; Shift: TShiftState;
        X: Longint; Y: Longint);
    procedure OnSpacePan(X, Y: Integer);
    procedure OnCircleTracked(var circle: TyyCircleTracked);
    procedure OnEnvelopeTracked(var Rectangle: TyyRectTracked);
    procedure OnPolylineTracked(var Polyline: TyyShapeTracked);
    procedure OnPolygonTracked(var Polygon: TyyShapeTracked);

  public
    { Public declarations }

    m_SingleSymbolDlg: Boolean;
    m_SinleSymbolType: TSymbolType;
    m_pSingleSymbol: ISymbol;
  end;

var
  FormMain: TFormMain;

function SingleSymbolEditDoModal(var pSymbol: ISymbol; symboltype: TSymbolType): Boolean;

implementation
{$R *.DFM}

uses
    Math, MathLib;

function SingleSymbolEditDoModal(var pSymbol: ISymbol;
    symboltype: TSymbolType): Boolean;
var
    pFormMain: TFormMain;
    pObj: IObj;
begin
    Result := False;

    if (Assigned(pSymbol)) then begin
        pSymbol.Clone(pObj);
    end
    else begin
        pObj := nil;
    end;

    pFormMain := TFormMain.Create(nil);
    pFormMain.m_SingleSymbolDlg := True;
    pFormMain.m_pSingleSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
    pFormMain.m_SinleSymbolType := symboltype;
    pFormMain.ShowModal();

    if (Assigned(pFormMain.m_pSingleSymbol)) then begin
        Result := True;
        pSymbol := pFormMain.m_pSingleSymbol;
    end;

    pFormMain.Free();
end;

function CheckPointSymbolType(const pPntSym: IPointSymbol): TPointSymbolType;
var
    p: Pointer;
begin
    Result := POINTSYMBOLTYPE_UNSUPPORTED;
    if (not Assigned(pPntSym)) then Exit;

    p := GotoInterface(pPntSym, 'ISimplePointSymbol');
    if (Assigned(p)) then begin
        Result := POINTSYMBOLTYPE_SIMPLE;
        Exit;
    end;

    p := GotoInterface(pPntSym, 'IEnvelopePointSymbol');
    if (Assigned(p)) then begin
        Result := POINTSYMBOLTYPE_ENVELOPE;
        Exit;
    end;

    p := GotoInterface(pPntSym, 'ITextPointSymbol');
    if (Assigned(p)) then begin
        Result := POINTSYMBOLTYPE_TEXT;
        Exit;
    end;

    p := GotoInterface(pPntSym, 'IPolyPointSymbol');
    if (Assigned(p)) then begin
        Result := POINTSYMBOLTYPE_POLY;
        Exit;
    end;
end;

procedure TFormMain.WinPos2EditorPos(x: Longint; y: Longint; out x1: Double;
    out y1: Double);
begin
    x1 := (x - m_pEditorSpace.Width / 2) / m_EditScale;
    y1 := (m_pEditorSpace.Height / 2 - y) / m_EditScale;
end;

procedure TFormMain.WinPos2EditorPos(var point: tagPOINT; out point1: WKSPoint);
begin
    Self.WinPos2EditorPos(point.x, point.y, point1.x, point1.y);
end;

procedure TFormMain.EditorPos2WinPos(const x: Double; const y: Double;
    out x1: Longint; out y1: Longint);
var
    w, h: Double;
begin
    w := m_pEditorSpace.Width;
    h := m_pEditorSpace.Height;
    x1 := Round(w / 2.0 + x * m_EditScale);
    y1 := Round(h / 2.0 - y * m_EditScale);
end;

procedure TFormMain.EditorPos2WinPos(var point: WKSPoint; out point1: tagPOINT);
begin
    Self.EditorPos2WinPos(point.x, point.y, point1.x, point1.y);
end;

procedure TFormMain.DrawUnderGrids();
var
    winpnt: tagPOINT;
    editorpnt, editorpnt1, ep: WKSPoint;
    i, fromi, toi: Longint;
    dc: HDC;
    PenNew, PenOld: HPEN;
begin
    dc := m_pEditorSpace.GetBufferDC();
    PenNew := CreatePen(PS_SOLID, 1, clSilver);
    PenOld := SelectObject(dc, PenNew);

    winpnt.x := 0;
    winpnt.y := 0;
    WinPos2EditorPos(winpnt, editorpnt);
    winpnt.x := m_pEditorSpace.Width;
    winpnt.y := m_pEditorSpace.Height;
    WinPos2EditorPos(winpnt, editorpnt1);
    fromi := Round(editorpnt.x);
    toi := Round(editorpnt1.x);
    for i := fromi to toi do begin
        ep.x := i;
        EditorPos2WinPos(ep, winpnt);
        MoveToEx(dc, winpnt.x, 0, nil);
        LineTo(dc, winpnt.x, m_pEditorSpace.Height);
    end;
    fromi := Round(editorpnt.y);
    toi := Round(editorpnt1.y);
    for i := fromi downto toi do begin
        ep.y := i;
        EditorPos2WinPos(ep, winpnt);
        MoveToEx(dc, 0, winpnt.y, nil);
        LineTo(dc, m_pEditorSpace.Width, winpnt.y);
    end;

    DeleteObject(PenNew);
    PenNew := CreatePen(PS_SOLID, 1, clBlue);
    SelectObject(dc, PenNew);

    editorpnt.x := 0;
    editorpnt.y := 0;
    Self.EditorPos2WinPos(editorpnt, winpnt);
    MoveToEx(dc, 0, winpnt.y, nil);
    LineTo(dc, m_pEditorSpace.Width, winpnt.y);
    MoveToEx(dc, winpnt.x, 0, nil);
    LineTo(dc, winpnt.x, m_pEditorSpace.Height);

    SelectObject(dc, PenOld);
    DeleteObject(PenNew);
end;

procedure TFormMain.DrawEditVertex(const vertex: tagPOINT);
var
    envelope: TRect;
begin
    envelope.Left := vertex.x - 6;
    envelope.right := vertex.x + 6;
    envelope.Top := vertex.y - 6;
    envelope.Bottom := vertex.y + 6;

    m_pEditorSpace.DrawEnvelope(envelope, m_VertexColor, 2, True);
end;

procedure TFormMain.DrawRotateCenter(const center: tagPOINT);
var
    shape: TyyShapeTracked;
begin
    shape.m_PointCount := 2;
    shape.m_Points[0].x := center.x - 8;
    shape.m_Points[0].y := center.y;
    shape.m_Points[1].x := center.x + 8;
    shape.m_Points[1].y := center.y;
    m_pEditorSpace.DrawPolyline(shape, m_RotateCenterColor, 1);
    shape.m_Points[0].x := center.x;
    shape.m_Points[0].y := center.y - 8;
    shape.m_Points[1].x := center.x;
    shape.m_Points[1].y := center.y + 8;
    m_pEditorSpace.DrawPolyline(shape, m_RotateCenterColor, 1);
end;

procedure TFormMain.PointSymbolEditorDraw();
var
    pointsymboltype: TPointSymbolType;
    i, count: Longint;
    j, c, parentindex: Longint;
    pPntSym, pPSTmp: IPointSymbol;
    selected: Boolean;
    mbr: TRect;
    selectedcount: Longint;
    pPSs: array of IPointSymbol;
    length: Longint;
    wksoffset: WKSPoint;
    winoffset: tagPOINT;
begin
    SetLength(pPSs, 0);
    if (not m_InitOK) then Exit;

    m_pEditorSpace.SetBackColor(m_pEditorSpace.GetBackColor());
    m_pEditorSpace.ResetBuffer();
    Self.DrawUnderGrids();
    m_pEditorSpace.PostBuffer();
    if (not Assigned(m_pEditorPointSymbol)) then Exit;

    selectedcount := 0;
    count := m_pEditorPointSymbol.GetSymbolCount();
    c := TVLayersSelected.Items.Count;
    for i := 0 to count - 1 do begin
        selected := False;
        for j := 0 to c - 1 do begin
            GetSelectedPointLayerByIndex(j, pPSTmp, parentindex);
            if (i = parentindex) then begin
                selected := True;
                Break;
            end;
        end;

        m_pEditorPointSymbol.GetSymbolRef(pPntSym, i);
        pointsymboltype := CheckPointSymbolType(pPntSym);
        case pointsymboltype of
            POINTSYMBOLTYPE_SIMPLE: Self.DrawEditorSimplePoint(pPntSym, selected, mbr);
            POINTSYMBOLTYPE_ENVELOPE: Self.DrawEditorEnvelopePoint(pPntSym, selected, mbr);
            POINTSYMBOLTYPE_POLY: Self.DrawEditorPolyPoint(pPntSym, selected, mbr);
            POINTSYMBOLTYPE_TEXT: Self.DrawEditorTextPoint(pPntSym, selected, mbr);
            POINTSYMBOLTYPE_BITMAP: Self.DrawEditorBitmapPoint(pPntSym, selected, mbr);
            else begin
                Continue;
            end;
        end;

        if (selected) then begin
            m_selectedbox[selectedcount] := mbr;
            Inc(selectedcount);

            length := High(pPSs) + 1;
            SetLength(pPSs, length + 1);
            pPSs[length] := pPntSym;
        end;
    end;

    m_pEditorSpace.PostBuffer();

    for i := 0 to selectedcount - 1 do begin
        m_pEditorSpace.DrawSelectBox(m_selectedbox[i], COLORREF($0000E800));
    end;

    for i := 0 to High(pPSs) do begin
        if (TEDITORTASK_VERTEXEDIT = m_EditorTask) then begin
            Self.DrawEditorVertex(pPSs[i]);
        end
        else if ((TEDITORTASK_ROTATE = m_EditorTask)
            and (POINTSYMBOLTYPE_SIMPLE <> CheckPointSymbolType(pPSs[i]))) then begin
            pPSs[i].GetOffset(wksoffset.x, wksoffset.y);
            Self.EditorPos2WinPos(wksoffset, winoffset);
            Self.DrawRotateCenter(winoffset);
        end;
    end;

    SetLength(pPSs, 0);
    Self.SymbolPreviewDraw();
    m_pPreviewDisplay.RefreshWindow1();
end;

procedure TFormMain.DrawEditorSimplePoint(const pSymbol: IPointSymbol; const selected: Boolean;
    out mbr: TRect);
var
    pSPS: ISimplePointSymbol;
    diameter, linewidth: Double;
    offsetx: Double;
    offsety: Double;
    solid: Boolean;
    color: COLORREF;
    circle: TyyCircleTracked;
    lw: Longint;
begin
    pSPS := ISimplePointSymbol(GotoInterface(pSymbol, 'ISimplePointSymbol'));
    pSPS.GetDiameter(diameter);
    pSPS.GetLineWidth(linewidth);
    pSPS.GetOffset(offsetx, offsety);
    pSPS.GetSolid(solid);
    pSPS.GetColor(color);
    if (selected) then color := m_SelectColor;
    EditorPos2WinPos(offsetx, offsety, circle.m_Center.x, circle.m_Center.y);
    circle.m_Radius := Round((diameter / 2)*m_EditScale);

    lw := Round(linewidth*m_EditScale);
    m_pEditorSpace.DrawCircle(circle, color, lw, not solid, color);

    mbr.Left := circle.m_Center.x - circle.m_Radius - lw div 2 - 1;
    mbr.Right := circle.m_Center.x + circle.m_Radius + lw div 2 + 1;
    mbr.Top := circle.m_Center.y - circle.m_Radius - lw div 2 - 1;
    mbr.Bottom := circle.m_Center.y + circle.m_Radius + lw div 2 + 1;
end;

procedure TFormMain.DrawEditorEnvelopePoint(const pSymbol: IPointSymbol;
    const selected: Boolean; out mbr: TRect);
var
    pEPS: IEnvelopePointSymbol;
    width: Double;
    height: Double;
    linewidth: Double;
    offsetx: Double;
    offsety: Double;
    angle: Double;
    color: COLORREF;
    solid: Boolean;
    shape: TyyShapeTracked;
    wkspnt, origin: WKSPoint;
    i, lw: Longint;
begin
    pEPS := IEnvelopePointSymbol(GotoInterface(pSymbol, 'IEnvelopePointSymbol'));
    pEPS.GetWidth(width);
    pEPS.GetHeight(height);
    pEPS.GetLineWidth(linewidth);
    pEPS.GetOffset(offsetx, offsety);
    pEPS.GetAngle(angle);
    pEPS.GetSolid(solid);
    pEPS.GetColor(color);
    if (selected) then color := m_SelectColor;

    origin.x := 0; origin.y := 0;
    wkspnt.x := -width/2;
    wkspnt.y := -height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    EditorPos2WinPos(wkspnt, shape.m_Points[0]);

    wkspnt.x := -width/2;
    wkspnt.y := height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    EditorPos2WinPos(wkspnt, shape.m_Points[1]);

    wkspnt.x := width/2;
    wkspnt.y := height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    EditorPos2WinPos(wkspnt, shape.m_Points[2]);

    wkspnt.x := width/2;
    wkspnt.y := -height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    EditorPos2WinPos(wkspnt, shape.m_Points[3]);

    shape.m_Points[4] := shape.m_Points[0];
    shape.m_PointCount := 5;

    lw := Round(linewidth*m_EditScale);
    m_pEditorSpace.DrawShape(shape, color, lw, not solid, color);

    mbr.Left := shape.m_Points[0].x;
    mbr.Top := shape.m_Points[0].y;
    mbr.Right := shape.m_Points[1].x;
    mbr.Bottom := shape.m_Points[1].y;
    for i := 0 to 4 do begin
        mbr.Left := Min(mbr.Left, shape.m_Points[i].x);
        mbr.Bottom := Max(mbr.Bottom, shape.m_Points[i].y);
        mbr.Right := Max(mbr.Right, shape.m_Points[i].x);
        mbr.Top := Min(mbr.Top, shape.m_Points[i].y);
    end;
    lw := lw div 2 + 2;
    mbr.Left := mbr.Left - lw;
    mbr.Top := mbr.Top - lw;
    mbr.Right := mbr.Right + lw;
    mbr.Bottom := mbr.Bottom + lw;
end;

procedure TFormMain.DrawEditorBitmapPoint(const pSymbol: IPointSymbol;
    const selected: Boolean; out mbr: TRect);
begin

end;

procedure TFormMain.DrawEditorTextPoint(const pSymbol: IPointSymbol;
    const selected: Boolean; out mbr: TRect);
begin

end;

procedure TFormMain.DrawEditorPolyPoint(const pSymbol: IPointSymbol;
    const selected: Boolean; out mbr: TRect);
var
    pPPS: IPolyPointSymbol;
    offsetx: Double;
    offsety: Double;
    angle: Double;
    linewidth: Double;
    solid: Boolean;
    color: COLORREF;
    wkspnt, origin: WKSPoint;
    i, lw: Longint;
    shape: TyyShapeTracked;
begin
    pPPS := IPolyPointSymbol(GotoInterface(pSymbol, 'IPolyPointSymbol'));
    pPPS.GetOffset(offsetx, offsety);
    pPPS.GetAngle(angle);
    pPPS.GetLineWidth(linewidth);
    pPPS.GetSolid(solid);
    pPPS.GetColor(color);
    if (selected) then color := m_SelectColor;
    origin.x := 0;
    origin.y := 0;

    pPPS.GetNode(0, wkspnt);
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    EditorPos2WinPos(wkspnt, shape.m_Points[0]);
    mbr.Left := shape.m_Points[0].x;
    mbr.Top := shape.m_Points[0].y;
    mbr.Right := shape.m_Points[0].x;
    mbr.Bottom := shape.m_Points[0].y;

    shape.m_PointCount := pPPS.GetNodeCount();
    for i := 1 to shape.m_PointCount - 1 do begin
        pPPS.GetNode(i, wkspnt);
        RotateDegree(wkspnt, origin, angle);
        wkspnt.x := wkspnt.x + offsetx;
        wkspnt.y := wkspnt.y + offsety;
        EditorPos2WinPos(wkspnt, shape.m_Points[i]);

        mbr.Left := Min(mbr.Left, shape.m_Points[i].x);
        mbr.Bottom := Max(mbr.Bottom, shape.m_Points[i].y);
        mbr.Right := Max(mbr.Right, shape.m_Points[i].x);
        mbr.Top := Min(mbr.Top, shape.m_Points[i].y);
    end;

    lw := Round(linewidth*m_EditScale);
    m_pEditorSpace.DrawShape(shape, color, lw, not solid, color);

    lw := lw div 2 + 2;
    mbr.Left := mbr.Left - lw;
    mbr.Top := mbr.Top - lw;
    mbr.Right := mbr.Right + lw;
    mbr.Bottom := mbr.Bottom + lw;
end;

procedure TFormMain.DrawEditorVertex(const pSymbol: IPointSymbol);
var
    pointsymboltype: TPointSymbolType;
begin
    pointsymboltype := CheckPointSymbolType(pSymbol);
    case pointsymboltype of
        POINTSYMBOLTYPE_SIMPLE: Self.DrawEditorSimpleVertex(pSymbol);
        POINTSYMBOLTYPE_ENVELOPE: Self.DrawEditorEnvelopeVertex(pSymbol);
        POINTSYMBOLTYPE_POLY: Self.DrawEditorPolyVertex(pSymbol);
        POINTSYMBOLTYPE_TEXT: Self.DrawEditorTextVertex(pSymbol);
        POINTSYMBOLTYPE_BITMAP: Self.DrawEditorBitmapVertex(pSymbol);
        else begin
            
        end;
    end;

end;

procedure TFormMain.DrawEditorSimpleVertex(const pSymbol: IPointSymbol);
var
    pSPS: ISimplePointSymbol;
    diameter: Double;
    offsetx: Double;
    offsety: Double;
    radius: Longint;
    wndpnt, wndpnt1: tagPOINT;
begin
    pSPS := ISimplePointSymbol(GotoInterface(pSymbol, 'ISimplePointSymbol'));
    pSPS.GetDiameter(diameter);
    pSPS.GetOffset(offsetx, offsety);

    EditorPos2WinPos(offsetx, offsety, wndpnt.x, wndpnt.y);
    radius := Round((diameter / 2)*m_EditScale);

    wndpnt1 := wndpnt;
    wndpnt1.x := wndpnt.x + radius;
    Self.DrawEditVertex(wndpnt1);

    wndpnt1 := wndpnt;
    wndpnt1.x := wndpnt.x - radius;
    Self.DrawEditVertex(wndpnt1);

    wndpnt1 := wndpnt;
    wndpnt1.y := wndpnt.y + radius;
    Self.DrawEditVertex(wndpnt1);

    wndpnt1 := wndpnt;
    wndpnt1.y := wndpnt.y - radius;
    Self.DrawEditVertex(wndpnt1);
end;

procedure TFormMain.DrawEditorEnvelopeVertex(const pSymbol: IPointSymbol);
var
    pEPS: IEnvelopePointSymbol;
    width: Double;
    height: Double;
    offsetx: Double;
    offsety: Double;
    angle: Double;
    wkspnt, origin: WKSPoint;
    i: Longint;
    shape: array[0..3] of tagPOINT;
begin
    pEPS := IEnvelopePointSymbol(GotoInterface(pSymbol, 'IEnvelopePointSymbol'));
    pEPS.GetWidth(width);
    pEPS.GetHeight(height);
    pEPS.GetOffset(offsetx, offsety);
    pEPS.GetAngle(angle);

    origin.x := 0; origin.y := 0;
    wkspnt.x := -width/2;
    wkspnt.y := -height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    EditorPos2WinPos(wkspnt, shape[0]);

    wkspnt.x := -width/2;
    wkspnt.y := height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    EditorPos2WinPos(wkspnt, shape[1]);

    wkspnt.x := width/2;
    wkspnt.y := height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    EditorPos2WinPos(wkspnt, shape[2]);

    wkspnt.x := width/2;
    wkspnt.y := -height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    EditorPos2WinPos(wkspnt, shape[3]);

    for i := 0 to 3 do begin
        Self.DrawEditVertex(shape[i]);
    end;
end;

procedure TFormMain.DrawEditorBitmapVertex(const pSymbol: IPointSymbol);
begin

end;

procedure TFormMain.DrawEditorTextVertex(const pSymbol: IPointSymbol);
begin

end;

procedure TFormMain.DrawEditorPolyVertex(const pSymbol: IPointSymbol);
var
    pPPS: IPolyPointSymbol;
    offsetx: Double;
    offsety: Double;
    angle: Double;
    wkspnt, origin: WKSPoint;
    i: Longint;
    shape: TyyShapeTracked;
begin
    pPPS := IPolyPointSymbol(GotoInterface(pSymbol, 'IPolyPointSymbol'));
    pPPS.GetOffset(offsetx, offsety);
    pPPS.GetAngle(angle);
    origin.x := 0;
    origin.y := 0;
    shape.m_PointCount := pPPS.GetNodeCount();
    for i := 0 to shape.m_PointCount - 1 do begin
        pPPS.GetNode(i, wkspnt);
        RotateDegree(wkspnt, origin, angle);
        wkspnt.x := wkspnt.x + offsetx;
        wkspnt.y := wkspnt.y + offsety;
        EditorPos2WinPos(wkspnt, shape.m_Points[i]);
    end;

    for i := 0 to shape.m_PointCount - 1 do begin
        Self.DrawEditVertex(shape.m_Points[i]);
    end;
end;

procedure TFormMain.SelectLayers(const pnt: WKSPoint);
var
    i, count: Longint;
    pPntSym: IPointSymbol;
    symtype: TPointSymbolType;
    selected: Boolean;
begin
    if (not Assigned(m_pEditorPointSymbol)) then Exit;
    count := m_pEditorPointSymbol.GetSymbolCount();
    for i := 0 to count - 1 do begin
        m_pEditorPointSymbol.GetSymbolRef(pPntSym, i);
        symtype := CheckPointSymbolType(pPntSym);
        selected := False;
        case symtype of
            POINTSYMBOLTYPE_SIMPLE: Self.SelectSimplePoint(pPntSym, pnt, selected);
            POINTSYMBOLTYPE_ENVELOPE: Self.SelectEnvelopePoint(pPntSym, pnt, selected);
            POINTSYMBOLTYPE_POLY: Self.SelectPolyPoint(pPntSym, pnt, selected);
            POINTSYMBOLTYPE_TEXT: Self.SelectTextPoint(pPntSym, pnt, selected);
            POINTSYMBOLTYPE_BITMAP: Self.SelectBitmapPoint(pPntSym, pnt, selected);
            else begin
                Continue;
            end;
        end;

        if (selected) then begin
            Self.CheckSelectedPointLayer(i);
        end;
    end;
end;

procedure TFormMain.SelectSimplePoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
    out selected: Boolean);
var
    pSPS: ISimplePointSymbol;
    diameter, linewidth, r1, r2: Double;
    offsetx: Double;
    offsety: Double;
    solid: Boolean;
    pntsizetmp, linewidthtmp: Double;
begin
    pSPS := ISimplePointSymbol(GotoInterface(pSymbol, 'ISimplePointSymbol'));
    pSPS.GetDiameter(diameter);
    pSPS.GetLineWidth(linewidth);
    pSPS.GetOffset(offsetx, offsety);
    pSPS.GetSolid(solid);

    r1 := Abs(Sqrt(Sqr(offsetx - pnt.x) + Sqr(offsety - pnt.y)));
    selected := False;
    pntsizetmp := diameter;
    linewidthtmp := linewidth;
    if (0.2 > pntsizetmp) then pntsizetmp := 0.2;
    if (0.2 > linewidthtmp) then linewidthtmp := 0.2;

    if (solid) then begin
        r2 := pntsizetmp/2 + linewidthtmp/2;
        if (r2 > r1) then begin
            selected := True;
        end;
    end
    else begin
        r2 := diameter/2;
        if (Abs(r2 - r1) < linewidthtmp/2) then begin
            selected := True;
        end;
    end;
end;

procedure TFormMain.SelectEnvelopePoint(const pSymbol: IPointSymbol;
    const pnt: WKSPoint; out selected: Boolean);
var
    pEPS: IEnvelopePointSymbol;
    width: Double;
    height: Double;
    linewidth: Double;
    offsetx: Double;
    offsety: Double;
    angle: Double;
    solid: Boolean;
    wkspnts: array[0..4] of WKSPoint;
    origin: WKSPoint;
    envelope: WKSRect;
    r: Longint;
    linewidthtmp: Double;
begin
    selected := False;
    pEPS := IEnvelopePointSymbol(GotoInterface(pSymbol, 'IEnvelopePointSymbol'));
    pEPS.GetWidth(width);
    pEPS.GetHeight(height);
    pEPS.GetLineWidth(linewidth);
    pEPS.GetOffset(offsetx, offsety);
    pEPS.GetAngle(angle);
    pEPS.GetSolid(solid);

    origin.x := 0; origin.y := 0;
    wkspnts[0].x := -width/2;
    wkspnts[0].y := -height/2;
    RotateDegree(wkspnts[0], origin, angle);
    wkspnts[0].x := wkspnts[0].x + offsetx;
    wkspnts[0].y := wkspnts[0].y + offsety;

    wkspnts[1].x := -width/2;
    wkspnts[1].y := height/2;
    RotateDegree(wkspnts[1], origin, angle);
    wkspnts[1].x := wkspnts[1].x + offsetx;
    wkspnts[1].y := wkspnts[1].y + offsety;

    wkspnts[2].x := width/2;
    wkspnts[2].y := height/2;
    RotateDegree(wkspnts[2], origin, angle);
    wkspnts[2].x := wkspnts[2].x + offsetx;
    wkspnts[2].y := wkspnts[2].y + offsety;

    wkspnts[3].x := width/2;
    wkspnts[3].y := -height/2;
    RotateDegree(wkspnts[3], origin, angle);
    wkspnts[3].x := wkspnts[3].x + offsetx;
    wkspnts[3].y := wkspnts[3].y + offsety;

    wkspnts[4] := wkspnts[0];

    if (solid) then begin
        r := PointInPolygon(pnt, wkspnts, 5);
        if (0 <> r) then begin
            selected := True;
        end;
    end;

    linewidthtmp := linewidth/2;
    if (0.2 > linewidthtmp) then linewidthtmp := 0.2;

    if (not selected) then begin
        envelope.left := pnt.x - linewidthtmp;
        envelope.right := pnt.x + linewidthtmp;
        envelope.top := pnt.y + linewidthtmp;
        envelope.bottom := pnt.y - linewidthtmp;
        selected := EnvelopeCrossPath(envelope, wkspnts, 5);
    end;
end;

procedure TFormMain.SelectBitmapPoint(const pSymbol: IPointSymbol;
    const pnt: WKSPoint; out selected: Boolean);
begin
    selected := False;
end;

procedure TFormMain.SelectTextPoint(const pSymbol: IPointSymbol;
    const pnt: WKSPoint; out selected: Boolean);
begin
    selected := False;
end;

procedure TFormMain.SelectPolyPoint(const pSymbol: IPointSymbol;
    const pnt: WKSPoint; out selected: Boolean);
var
    pPPS: IPolyPointSymbol;
    offsetx: Double;
    offsety: Double;
    angle: Double;
    linewidth: Double;
    solid: Boolean;
    origin: WKSPoint;
    i, pointcount: Longint;
    envelope: WKSRect;
    r: Longint;
    linewidthtmp: Double;
begin
    pPPS := IPolyPointSymbol(GotoInterface(pSymbol, 'IPolyPointSymbol'));
    pPPS.GetOffset(offsetx, offsety);
    pPPS.GetAngle(angle);
    pPPS.GetLineWidth(linewidth);
    pPPS.GetSolid(solid);
    origin.x := 0;
    origin.y := 0;

    pointcount := pPPS.GetNodeCount();
    for i := 0 to pointcount - 1 do begin
        pPPS.GetNode(i, m_arrayofwkspoints[i]);
        RotateDegree(m_arrayofwkspoints[i], origin, angle);
        m_arrayofwkspoints[i].x := m_arrayofwkspoints[i].x + offsetx;
        m_arrayofwkspoints[i].y := m_arrayofwkspoints[i].y + offsety;
    end;

    if (solid) then begin
        r := PointInPolygon(pnt, m_arrayofwkspoints, pointcount);
        if (0 <> r) then begin
            selected := True;
        end;
    end;

    linewidthtmp := linewidth/2;
    if (0.2 > linewidthtmp) then linewidthtmp := 0.2;
    if (not selected) then begin
        envelope.left := pnt.x - linewidthtmp;
        envelope.right := pnt.x + linewidthtmp;
        envelope.top := pnt.y + linewidthtmp;
        envelope.bottom := pnt.y - linewidthtmp;
        selected := EnvelopeCrossPath(envelope, m_arrayofwkspoints, pointcount);
    end;
end;

function TFormMain.CheckSelection(wkspnt: WKSPoint): Boolean;
var
    i, count: Longint;
    pPntSym: IPointSymbol;
    symtype: TPointSymbolType;
    selected: Boolean;
begin
    Result := False;
    if (not Assigned(m_pEditorPointSymbol)) then Exit;
    count := m_pEditorPointSymbol.GetSymbolCount();
    for i := 0 to count - 1 do begin
        if (not Self.CheckLayerSelected(i)) then Continue;
        m_pEditorPointSymbol.GetSymbolRef(pPntSym, i);
        symtype := CheckPointSymbolType(pPntSym);
        selected := False;
        case symtype of
            POINTSYMBOLTYPE_SIMPLE: Self.SelectSimplePoint(pPntSym, wkspnt, selected);
            POINTSYMBOLTYPE_ENVELOPE: Self.SelectEnvelopePoint(pPntSym, wkspnt, selected);
            POINTSYMBOLTYPE_POLY: Self.SelectPolyPoint(pPntSym, wkspnt, selected);
            POINTSYMBOLTYPE_TEXT: Self.SelectTextPoint(pPntSym, wkspnt, selected);
            POINTSYMBOLTYPE_BITMAP: Self.SelectBitmapPoint(pPntSym, wkspnt, selected);
            else begin
                Continue;
            end;
        end;
        if (selected) then begin
            Result := True;
            Break;
        end;
    end;
end;

function TFormMain.CheckLayerSelected(layerindex: Longint): Boolean;
var
    i, c, parentindex: Longint;
    pPSTmp: IPointSymbol;
begin
    Result := False;
    c := TVLayersSelected.Items.Count;
    for i := 0 to c - 1 do begin
        GetSelectedPointLayerByIndex(i, pPSTmp, parentindex);
        if (parentindex = layerindex) then begin
            Result := True;
            Break;
        end;
    end;
end;

procedure TFormMain.MoveSelection(offset: WKSPoint);
var
    i, count: Longint;
    pPntSym: IPointSymbol;
    x, y: Double;
begin
    if (not Assigned(m_pEditorPointSymbol)) then Exit;
    count := m_pEditorPointSymbol.GetSymbolCount();
    for i := 0 to count - 1 do begin
        if (Self.CheckLayerSelected(i)) then begin
            m_pEditorPointSymbol.GetSymbolRef(pPntSym, i);
            pPntSym.GetOffset(x, y);
            x := x + offset.x;
            y := y + offset.y;
            pPntSym.SetOffset(x, y);
        end;
    end;
end;

procedure TFormMain.SymbolPreviewDraw();
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
    pSymbolToPreview: ISymbol;
    drawsymtype: Longint;
begin
    m_pPreviewDisplay.SetBackgroundColor(ColorGridDraw.BackgroundColor);
    m_pPreviewDisplay.RefreshWindow1();
    pSymbolToPreview := nil;
    if (Assigned(m_pEditorPointSymbol)) then begin
        pSymbolToPreview := m_pEditorPointSymbol;
        drawsymtype := 1;
    end
    else if (Assigned(m_pEditorLineSymbol)) then begin
        pSymbolToPreview := m_pEditorLineSymbol;
        drawsymtype := 2;
    end
    else if (Assigned(m_pEditorFillSymbol)) then begin
        pSymbolToPreview := m_pEditorFillSymbol;
        drawsymtype := 3;
    end
    else begin
        Exit;
    end;

    m_pPreviewDisplay.SetSymbol(pSymbolToPreview);
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

    m_pPreviewDisplay.StartDraw();
    case drawsymtype of
        1: m_pPreviewDisplay.DrawGeometry(pPnt);
        2: m_pPreviewDisplay.DrawGeometry(pLine);
        3: m_pPreviewDisplay.DrawGeometry(pPolygon);
        SYMBOLTYPE_TEXT: m_pPreviewDisplay.DrawTextXY(x, y, '毛猫', rect);
    end;
    m_pPreviewDisplay.FinishDraw();
    m_pPreviewDisplay.RefreshWindow1();
end;

procedure TFormMain.OnSpacePaint(dc: HDC);
begin
    Self.SymbolPreviewDraw();
    m_pPreviewDisplay.RefreshWindow1();
end;

procedure TFormMain.OnSpaceSize(width, height: Longint);
begin

end;

procedure TFormMain.OnSpaceMouseMove(Shift: TShiftState; X: Longint; Y: Longint);
var
    wkspnt: WKSPoint;
    offset: WKSPoint;
    r: Boolean;
begin
    if (not m_EditorLBDown) then begin
        m_VertexEditSymbol := nil;
        m_VertexEditStart := False;
        m_VertexEditIndex := -1;
    end;

    offset.x := (X - m_EditorMoveStartPos.x) / m_EditScale;
    offset.y := -(Y - m_EditorMoveStartPos.y) / m_EditScale;

    Self.WinPos2EditorPos(X, Y, wkspnt.x, wkspnt.y);
    LabelEditPosX.Caption := 'x: ' + FloatToStr(wkspnt.x);
    LabelEditPosY.Caption := 'y: ' + FloatToStr(wkspnt.y);

    case m_EditorTask of
        TEDITORTASK_SELECT: begin
            if (not m_EditorLBDown) then begin
                r := Self.CheckSelection(wkspnt);
                if (r) then begin
                    m_pEditorSpace.SetCursor(MOUSECURSOR_MOVE);
                end
                else begin
                    m_pEditorSpace.SetCursor(MOUSECURSOR_ARROW2);
                end;
            end
            else begin
                if (m_MoveSelection) then begin
                    if (m_EditorLBDown) then begin
//                        offset.x := (X - m_EditorMoveStartPos.x) / m_EditScale;
//                        offset.y := -(Y - m_EditorMoveStartPos.y) / m_EditScale;
                        Self.MoveSelection(offset);
                        Self.PointSymbolEditorDraw();
                        m_pEditorSpace.PostBuffer();

                        m_EditorMoveStartPos.x := X;
                        m_EditorMoveStartPos.y := Y;
                    end;
                end;
            end;
        end;

        TEDITORTASK_ROTATE: begin
            if (m_EditorLBDown) then begin
                Self.RotatePointSelectedLayer(Y - m_EditorMoveStartPos.y);

                Self.PointSymbolEditorDraw();
                m_pEditorSpace.PostBuffer();

                m_EditorMoveStartPos.x := X;
                m_EditorMoveStartPos.y := Y;
            end;
        end;

        TEDITORTASK_VERTEXEDIT: begin
            if (m_VertexEditStart) then begin
                offset.x := (X - m_EditorMoveStartPos.x) / m_EditScale;
                offset.y := -(Y - m_EditorMoveStartPos.y) / m_EditScale;

                Self.MoveVertexSelected(offset, wkspnt);
                Self.PointSymbolEditorDraw();
                m_pEditorSpace.PostBuffer();

                m_EditorMoveStartPos.x := X;
                m_EditorMoveStartPos.y := Y;
            end
            else begin
                if (Self.CheckVertexHit(wkspnt)) then begin
                    m_pEditorSpace.SetCursor(MOUSECURSOR_PENCIL);
                end
                else begin
                    m_pEditorSpace.SetCursor(MOUSECURSOR_ARROW1);
                end;
            end;
        end;

        else begin

        end;
    end;
end;

procedure TFormMain.OnSpaceMouseDown(Button: TMouseButton; Shift: TShiftState;
    X: Longint; Y: Longint);
var
    pnt: WKSPoint;
begin
    if (not m_EditorLBDown) then begin
        m_VertexEditSymbol := nil;
        m_VertexEditStart := False;
        m_VertexEditIndex := -1;
    end;

    m_EditorLBDown := (mbLeft = Button);
    m_EditorLBDownPos.x := X;
    m_EditorLBDownPos.y := Y;
    WinPos2EditorPos(m_EditorLBDownPos, pnt);

    m_EditorMoveStartPos.x := X;
    m_EditorMoveStartPos.y := Y;

    case m_EditorTask of
        TEDITORTASK_SELECT: begin
            if (mbLeft = Button) then begin
                if (Self.CheckSelection(pnt)) then begin
                    m_MoveSelection := True;
                end
                else begin
                    Self.SelectLayers(pnt);
                end;
            end
            else begin
                Self.SBClearSelectClick(nil);
            end;
        end;

        TEDITORTASK_ROTATE: begin
            if (mbLeft = Button) then begin
                if (0 = TVLayersSelected.Items.Count) then begin
                    Self.SelectLayers(pnt);
                end;
            end
            else begin
                Self.SBClearSelectClick(nil);
            end;
        end;

        TEDITORTASK_VERTEXEDIT: begin
            if (mbLeft = Button) then begin
                if (0 = TVLayersSelected.Items.Count) then begin
                    Self.SelectLayers(pnt);
                end
                else if (Self.CheckVertexHit(pnt)) then begin
                    m_VertexEditStart := True;
                end;
            end
            else begin
                Self.SBClearSelectClick(nil);
            end;
        end;

        else begin

        end;
    end;
end;

procedure TFormMain.OnSpaceMouseUp(Button: TMouseButton; Shift: TShiftState;
    X: Longint; Y: Longint);
var
    offset: WKSPoint;
    wkspnt: WKSPoint;
begin
    offset.x := (X - m_EditorMoveStartPos.x) / m_EditScale;
    offset.y := -(Y - m_EditorMoveStartPos.y) / m_EditScale;

    Self.WinPos2EditorPos(X, Y, wkspnt.x, wkspnt.y);

    case m_EditorTask of
        TEDITORTASK_SELECT: begin
            if ((mbLeft = Button) and m_EditorLBDown) then begin
                if (((X <> m_EditorLBDownPos.x) or (Y <> m_EditorLBDownPos.y))
                    and m_MoveSelection) then begin
//                    offset.x := (X - m_EditorMoveStartPos.x) / m_EditScale;
//                    offset.y := -(Y - m_EditorMoveStartPos.y) / m_EditScale;
                    Self.MoveSelection(offset);
                    Self.PushEdit();
                    Self.PointSymbolEditorDraw();
                    m_pEditorSpace.PostBuffer();
                end;
                m_MoveSelection := False;
            end;
        end;

        TEDITORTASK_ROTATE: begin
            if ((mbLeft = Button) and m_EditorLBDown and
                (0 < TVLayersSelected.Items.Count)) then begin
                Self.RotatePointSelectedLayer(Y - m_EditorMoveStartPos.y);
                Self.PushEdit();

                Self.PointSymbolEditorDraw();
                m_pEditorSpace.PostBuffer();
            end;
        end;

        TEDITORTASK_VERTEXEDIT: begin
            if (m_VertexEditStart) then begin
                offset.x := (X - m_EditorMoveStartPos.x) / m_EditScale;
                offset.y := -(Y - m_EditorMoveStartPos.y) / m_EditScale;

                Self.MoveVertexSelected(offset, wkspnt);
                Self.PushEdit();
                Self.PointSymbolEditorDraw();
                m_pEditorSpace.PostBuffer();
            end;
        end;

        else begin
        end;
    end;
    m_EditorLBDown := False;
    m_VertexEditStart := False;
    m_VertexEditSymbol := nil;
    m_VertexEditIndex := -1;
end;

procedure TFormMain.OnSpacePan(X, Y: Integer);
begin
    if (not m_InitOK) then Exit;
    Self.PointSymbolEditorDraw();
    m_pEditorSpace.PostBuffer();
end;

procedure TFormMain.OnCircleTracked(var circle: TyyCircleTracked);
var
    pSPS: ISimplePointSymbol;
    radius: Double;
    x, y: Double;
    name: AnsiString;
    root: TTreeNode;
begin
    WinPos2EditorPos(circle.m_Center.x, circle.m_Center.y, x, y);
    radius := circle.m_Radius / m_EditScale;

    if ((TEDITORTASK_NEWCIRCLESOLID = m_EditorTask)
        or (TEDITORTASK_NEWCIRCLEHOLLOW = m_EditorTask)) then begin
        if (0.3 > radius) then radius := 0.3;
        CreateSimplePointSymbol(radius*2, PanelDrawColor.Color, pSPS);
        pSPS.SetColorLock(CBColorLock.Checked);
        pSPS.SetLineWidth(m_LineWidth);
        pSPS.SetOffset(x, y);
        name := EditLayerName.Text;
        pSPS.SetName(PChar(name));
        case m_EditorTask of
            TEDITORTASK_NEWCIRCLESOLID: begin
                pSPS.SetSolid(True);
            end;

            TEDITORTASK_NEWCIRCLEHOLLOW: begin
                pSPS.SetSolid(False);
            end;
        end;
        m_pEditorPointSymbol.AddSymbol(pSPS);
        Self.PushEdit();

        Self.PointSymbolEditorDraw();
        Self.m_pEditorSpace.PostBuffer();
        Self.RefreshPointLayerList();

        root := TVSubPointList.Items.GetFirstNode();
        root.Item[0].Selected := true;
        TVSubPointListClick(nil);
    end;
end;

procedure TFormMain.OnEnvelopeTracked(var Rectangle: TyyRectTracked);
var
    pEPS: IEnvelopePointSymbol;
    rect: TRect;
    wrect: WKSRect;
    offset: WKSPoint;
    width, height: Double;
    name: AnsiString;
    root: TTreeNode;
begin
    if (Rectangle.m_Point1.x < Rectangle.m_Point2.x) then begin
        rect.Left := Rectangle.m_Point1.x;
        rect.Right := Rectangle.m_Point2.x;
    end
    else begin
        rect.Left := Rectangle.m_Point2.x;
        rect.Right := Rectangle.m_Point1.x;
    end;
    if (Rectangle.m_Point1.y < Rectangle.m_Point2.y) then begin
        rect.Top := Rectangle.m_Point1.y;
        rect.Bottom := Rectangle.m_Point2.y;
    end
    else begin
        rect.Top := Rectangle.m_Point2.y;
        rect.Bottom := Rectangle.m_Point1.y;
    end;

    WinPos2EditorPos(rect.Left, rect.Top, wrect.left, wrect.top);
    WinPos2EditorPos(rect.Right, rect.Bottom, wrect.right, wrect.bottom);
    offset.x := (wrect.left + wrect.right)/2;
    offset.y := (wrect.top + wrect.bottom)/2;
    width := Abs(wrect.left - wrect.right);
    height := Abs(wrect.top - wrect.bottom);

    if ((TEDITORTASK_NEWENVELOPESOLID = m_EditorTask)
        or (TEDITORTASK_NEWENVELOPEHOLLOW = m_EditorTask)) then begin
        if (0.3 > width) then width := 0.3;
        if (0.3 > height) then height := 0.3;
        CreateEnvelopePointSymbol(width, height, PanelDrawColor.Color, pEPS);
        pEPS.SetColorLock(CBColorLock.Checked);
        pEPS.SetLineWidth(m_LineWidth);
        pEPS.SetOffset(offset.x, offset.y);
        name := EditLayerName.Text;
        pEPS.SetName(PChar(name));
        case m_EditorTask of
            TEDITORTASK_NEWENVELOPESOLID: begin
                pEPS.SetSolid(True);
            end;

            TEDITORTASK_NEWENVELOPEHOLLOW: begin
                pEPS.SetSolid(False);
            end;
        end;

        m_pEditorPointSymbol.AddSymbol(pEPS);
        Self.PushEdit();

        Self.PointSymbolEditorDraw();
        Self.m_pEditorSpace.PostBuffer();
        Self.RefreshPointLayerList();

        root := TVSubPointList.Items.GetFirstNode();
        root.Item[0].Selected := true;
        TVSubPointListClick(nil);
    end;
end;

procedure TFormMain.OnPolygonTracked(var Polygon: TyyShapeTracked);
var
    pPPS: IPolyPointSymbol;
    wkspnt: WKSPoint;
    solid: Boolean;
    i: Longint;
    name: AnsiString;
    root: TTreeNode;
begin
    if (TEDITORTASK_NEWPOLYGONSOLID = m_EditorTask) then begin
        solid := True;
        CreatePolyPointSymbol(solid, m_LineWidth, PanelDrawColor.Color, pPPS);
        pPPS.SetColorLock(CBColorLock.Checked);
        for i := 0 to Polygon.m_PointCount - 1 do begin
            WinPos2EditorPos(Polygon.m_Points[i], wkspnt);
            pPPS.AddNode(wkspnt);
        end;
        WinPos2EditorPos(Polygon.m_Points[0], wkspnt);
        pPPS.AddNode(wkspnt);

        name := EditLayerName.Text;
        pPPS.SetName(PChar(name));
        m_pEditorPointSymbol.AddSymbol(pPPS);
        Self.BlowTheShape(pPPS);

        Self.PushEdit();

        Self.PointSymbolEditorDraw();
        Self.m_pEditorSpace.PostBuffer();
        Self.RefreshPointLayerList();

        root := TVSubPointList.Items.GetFirstNode();
        root.Item[0].Selected := true;
        TVSubPointListClick(nil);
    end;
end;

procedure TFormMain.OnPolylineTracked(var Polyline: TyyShapeTracked);
var
    pPPS: IPolyPointSymbol;
    wkspnt: WKSPoint;
    solid: Boolean;
    i: Longint;
    name: AnsiString;
    root: TTreeNode;          
begin
    if (TEDITORTASK_NEWPATH = m_EditorTask) then begin
        solid := False;
        CreatePolyPointSymbol(solid, m_LineWidth, PanelDrawColor.Color, pPPS);
        pPPS.SetColorLock(CBColorLock.Checked);
        for i := 0 to Polyline.m_PointCount - 1 do begin
            WinPos2EditorPos(Polyline.m_Points[i], wkspnt);
            pPPS.AddNode(wkspnt);
        end;

        name := EditLayerName.Text;
        pPPS.SetName(PChar(name));
        m_pEditorPointSymbol.AddSymbol(pPPS);
        Self.BlowTheShape(pPPS);

        Self.PushEdit();

        Self.PointSymbolEditorDraw();
        Self.m_pEditorSpace.PostBuffer();
        Self.RefreshPointLayerList();

        root := TVSubPointList.Items.GetFirstNode();
        root.Item[0].Selected := true;
        TVSubPointListClick(nil);
    end;
end;

procedure TFormMain.UpdatePointSymbolEditor(const pMultiPointSymbol: IMultiPointSymbol);
var
    pObj: IObj;
    pSymbol: ISymbol;
begin
    SubSymbolAttrib.SetSymbol(nil);

    m_pEditUndoList.ClearSymbols();
    m_EditListCurrent := 0;
    m_pEditorPointSymbol := nil;
    if (not Assigned(pMultiPointSymbol)) then begin
        Self.RefreshPointLayerList();
        Self.ClearSelectedPointLayers();
        Self.EnablePointSymbolStuff(False);
        Exit;
    end;

    Self.UpdateLineSymbolEditor(nil);

    m_pEditorPointSymbol := pMultiPointSymbol;
    m_pEditorPointSymbol.Clone(pObj);
    m_pEditUndoList.ClearSymbols();
    pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
    m_pEditUndoList.AddSymbol(pSymbol);
    Self.RefreshPointLayerList();
    Self.ClearSelectedPointLayers();
    Self.EnablePointSymbolStuff(True);
    Self.PointSymbolEditorDraw();
end;

procedure TFormMain.UpdateLineSymbolEditor(const pMultiLineSymbol: IMultiLineSymbol);
var
    pObj: IObj;
    pSymbol: ISymbol;
begin
    m_pEditUndoList.ClearSymbols();
    m_EditListCurrent := 0;
    m_pEditorLineSymbol := nil;
    if (not Assigned(pMultiLineSymbol)) then begin
        Self.EnableLineSymbolStuff(False);
        PanelLineSymbolEditor.Visible := False;
        Exit;
    end;

    Self.UpdatePointSymbolEditor(nil);
    SubSymbolAttrib.Close();

    m_pEditorLineSymbol := pMultiLineSymbol;
    m_pEditorLineSymbol.Clone(pObj);
    m_pEditUndoList.ClearSymbols();
    pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
    m_pEditUndoList.AddSymbol(pSymbol);
    Self.EnableLineSymbolStuff(True);
    Self.RefreshSubLineList();
    if (TVSubLine.Items.Count > 0) then begin
        TVSubLine.Items[0].Selected := True;
    end;
    Self.RefreshLineEditor();
    PanelLineSymbolEditor.Visible := True;
    Self.SymbolPreviewDraw();
    m_pPreviewDisplay.RefreshWindow1();
end;

procedure TFormMain.UpdateFillSymbolEditor(const pMultiFillSymbol: IMultiFillSymbol);
var
    pObj: IObj;
    pSymbol: ISymbol;
begin
    m_pEditUndoList.ClearSymbols();
    m_EditListCurrent := 0;
    m_pEditorFillSymbol := nil;
    if (not Assigned(pMultiFillSymbol)) then begin
        Self.EnableFillSymbolStuff(False);
        PanelFillSymbolEditor.Visible := False;
        Exit;
    end;

    Self.UpdatePointSymbolEditor(nil);
    SubSymbolAttrib.Close();
    Self.UpdateLineSymbolEditor(nil);

    m_pEditorFillSymbol := pMultiFillSymbol;
    m_pEditorFillSymbol.Clone(pObj);
    m_pEditUndoList.ClearSymbols();
    pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
    m_pEditUndoList.AddSymbol(pSymbol);
    Self.EnableFillSymbolStuff(True);
    PanelFillSymbolEditor.Visible := True;
    Self.RefreshSubFillList();
    if (TVSubFill.Items.Count > 0) then begin
        TVSubFill.Items[0].Selected := True;
    end;
    Self.RefreshFillEditor();
    Self.SymbolPreviewDraw();
    m_pPreviewDisplay.RefreshWindow1();
end;

procedure TFormMain.UpdateTextSymbolEditor(const pSimpleTextSymbol: ISimpleTextSymbol);
begin
    m_pEditUndoList.ClearSymbols();
    m_EditListCurrent := 0;

    Self.UpdatePointSymbolEditor(nil);
    SubSymbolAttrib.Close();
    Self.UpdateLineSymbolEditor(nil);
    Self.UpdateFillSymbolEditor(nil);
end;

function TFormMain.PushEdit(): Boolean;
var
    pObj: IObj;
    pSymbol: ISymbol;
    count: Longint;
begin
    Result := False;
    if (not Assigned(LVSymbolList.Selected)) then Exit;

    count := m_pEditUndoList.GetSymbolCount();
    while (count - 1 > m_EditListCurrent) do begin
        m_pEditUndoList.RemoveSymbol(count - 1);
        count := m_pEditUndoList.GetSymbolCount();
    end;

    if (Assigned(m_pEditorPointSymbol)) then begin
        //当前是点
        m_pEditorPointSymbol.Clone(pObj);
    end
    else if (Assigned(m_pEditorLineSymbol)) then begin
        //当前是线
        m_pEditorLineSymbol.Clone(pObj);
    end
    else if (Assigned(m_pEditorFillSymbol)) then begin
        //当前是面
        m_pEditorFillSymbol.Clone(pObj);
    end;

    pSymbol := ISymbol(GotoInterface(pObj, 'ISymbol'));
    m_pEditUndoList.AddSymbol(pSymbol);
    Inc(m_EditListCurrent);

    SubSymbolAttrib.RefreshStuff();
    LVSymbolList.Refresh();
    m_LibDirty := True;
    Result := True;
end;

function TFormMain.UndoEdit(): Boolean;
var
    pObj: IObj;
    pSymbol: ISymbol;
    pLineSymbol: ILineSymbol;
    pFillSymbol: IFillSymbol;
    sublineindex, subfillindex, n: Longint;
begin
    Result := False;
    if (not Assigned(LVSymbolList.Selected)) then Exit;
    if (0 = m_EditListCurrent) then Exit;

    Dec(m_EditListCurrent);
    m_pEditUndoList.GetSymbolRef(pSymbol, m_EditListCurrent);
    pSymbol.Clone(pObj);

    if (Assigned(m_pEditorPointSymbol)) then begin
        //当前是点
        m_pEditorPointSymbol := IMultiPointSymbol(GotoInterface(pObj, 'IMultiPointSymbol'));
        m_pSymbolLib.SetSymbolRef(m_pEditorPointSymbol, LVSymbolList.Selected.Index);

        Self.ClearSelectedPointLayers();
        Self.RefreshPointLayerList();
        Self.PointSymbolEditorDraw();
        m_pEditorSpace.PostBuffer();

        SubSymbolAttrib.SetSymbol(nil);
    end
    else if (Assigned(m_pEditorLineSymbol)) then begin
        //当前是线
        m_pEditorLineSymbol := IMultiLineSymbol(GotoInterface(pObj, 'IMultiLineSymbol'));
        m_pSymbolLib.SetSymbolRef(m_pEditorLineSymbol, LVSymbolList.Selected.Index);

        Self.RefreshLineEditor();
        Self.GetSelectedSubLine(pLineSymbol, sublineindex);
        Self.RefreshSubLineList();
        n := TVSubLine.Items.Count - 1 - sublineindex;
        if ((sublineindex >= 0) and (n >= 0) and (n < TVSubLine.Items.Count)) then begin
            TVSubLine.Items[n].Selected := True;
        end;
    end
    else if (Assigned(m_pEditorFillSymbol)) then begin
        //当前是面
        m_pEditorFillSymbol := IMultiFillSymbol(GotoInterface(pObj, 'IMultiFillSymbol'));
        m_pSymbolLib.SetSymbolRef(m_pEditorFillSymbol, LVSymbolList.Selected.Index);

        Self.RefreshFillEditor();
        Self.GetSelectedSubFill(pFillSymbol, subfillindex);
        Self.RefreshSubFillList();
        n := TVSubFill.Items.Count - 1 - subfillindex;
        if ((subfillindex >= 0) and (n >= 0) and (n < TVSubFill.Items.Count)) then begin
            TVSubFill.Items[n].Selected := True;
        end;
    end
    else begin
        Exit;
    end;

    Self.SymbolPreviewDraw();
    LVSymbolList.Refresh();
    m_LibDirty := True;
    Result := True;
end;

function TFormMain.RedoEdit(): Boolean;
var
    pObj: IObj;
    pSymbol: ISymbol;
    symbolcount: Longint;
    pLineSymbol: ILineSymbol;
    pFillSymbol: IFillSymbol;
    sublineindex, subfillindex, n: Longint;
begin
    Result := False;
    if (not Assigned(LVSymbolList.Selected)) then Exit;
    symbolcount := m_pEditUndoList.GetSymbolCount();
    if (symbolcount - 1 = m_EditListCurrent) then Exit;

    Inc(m_EditListCurrent);
    m_pEditUndoList.GetSymbolRef(pSymbol, m_EditListCurrent);
    pSymbol.Clone(pObj);

    if (Assigned(m_pEditorPointSymbol)) then begin
        //当前是点
        m_pEditorPointSymbol := IMultiPointSymbol(GotoInterface(pObj, 'IMultiPointSymbol'));
        m_pSymbolLib.SetSymbolRef(m_pEditorPointSymbol, LVSymbolList.Selected.Index);

        Self.ClearSelectedPointLayers();
        Self.RefreshPointLayerList();
        Self.PointSymbolEditorDraw();
        m_pEditorSpace.PostBuffer();

        SubSymbolAttrib.SetSymbol(nil);
    end
    else if (Assigned(m_pEditorLineSymbol)) then begin
        //当前是线
        m_pEditorLineSymbol := IMultiLineSymbol(GotoInterface(pObj, 'IMultiLineSymbol'));
        m_pSymbolLib.SetSymbolRef(m_pEditorLineSymbol, LVSymbolList.Selected.Index);

        Self.RefreshLineEditor();
        Self.GetSelectedSubLine(pLineSymbol, sublineindex);
        Self.RefreshSubLineList();
        n := TVSubLine.Items.Count - 1 - sublineindex;
        if ((sublineindex >= 0) and (n >= 0) and (n < TVSubLine.Items.Count)) then begin
            TVSubLine.Items[n].Selected := True;
        end;
    end
    else if (Assigned(m_pEditorFillSymbol)) then begin
        //当前是面
        m_pEditorFillSymbol := IMultiFillSymbol(GotoInterface(pObj, 'IMultiFillSymbol'));
        m_pSymbolLib.SetSymbolRef(m_pEditorFillSymbol, LVSymbolList.Selected.Index);

        Self.RefreshFillEditor();
        Self.GetSelectedSubFill(pFillSymbol, subfillindex);
        Self.RefreshSubFillList();
        n := TVSubFill.Items.Count - 1 - subfillindex;
        if ((subfillindex >= 0) and (n >= 0) and (n < TVSubFill.Items.Count)) then begin
            TVSubFill.Items[n].Selected := True;
        end;
    end
    else begin
        Exit;
    end;

    Self.SymbolPreviewDraw();
    LVSymbolList.Refresh();
    m_LibDirty := True;
    Result := True;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
    dc: HDC;
begin
    SubSymbolAttrib.SymbolModified := Self.PointLayerModerfied;
    FormLineTemplate.ModifyEvent := Self.TemplateModified;
//    SubSymbolAttrib.Parent := Self;

    m_EditorLBDown := False;
    m_MoveSelection := False;

    PanelSymbolEditor.Align := alClient;
    PanelSymbolPreview.Align := alClient;
//    PanelDown.Align := alBottom;
    PanelMain.Align := alClient;
    PanelRight.Align := alRight;
    PanelRightPreview.Align := alTop;
    PanelRightMiddle.Align := alTop;
    PanelRightBottom.Align := alBottom;
    PanelSymbolAttrib.Align := alClient;
    PanelRightList.Align := alClient;
    PanelPointSymbolEditor.Align := alClient;
    PanelLineSymbolEditor.Align := alClient;
    PanelFillSymbolEditor.Align := alClient;
    PanelSymbolAttrib.Align := alClient;
    TVLayersSelected.Align := alClient;
    TVSubPointList.Align := alClient;
    LVSymbolList.Align := alClient;
    PanelSubLineList.Align := alClient;
    PanelSubFillList.Align := alClient;
    TVSubLine.Align := alClient;
    TVSubFill.Align := alClient;
    PanelBottom.Align := alBottom;
    LabelEditPosX.Caption := 'x: ';
    LabelEditPosY.Caption := 'y: ';


    FormLineTemplate.Show();
    FormLineTemplate.Parent := PanelLineTemplate;
    FormLineTemplate.Left := 2;
    FormLineTemplate.Top := 2;

    CreateSpace(PanelSymbolEditor.Handle, m_pEditorSpace);
    m_pEditorSpace.SetBackColor(clWhite);
    m_pEditorSpace.InitialPaintEventHandle(OnSpacePaint);
    m_pEditorSpace.InitialSizeEventHandle(OnSpaceSize);
    m_pEditorSpace.InitialMouseMoveEventHandle(OnSpaceMouseMove);
    m_pEditorSpace.InitialMouseDownEventHandle(OnSpaceMouseDown);
    m_pEditorSpace.InitialMouseUpEventHandle(OnSpaceMouseUp);
    m_pEditorSpace.InitialPanEventHandle(OnSpacePan);
    m_pEditorSpace.InitialCircleEventHandle(OnCircleTracked);
    m_pEditorSpace.InitialRectEventHandle(OnEnvelopeTracked);
    m_pEditorSpace.InitialPolygonEventHandle(nil, OnPolygonTracked);
    m_pEditorSpace.InitialPolylineEventHandle(nil, OnPolylineTracked);
    m_pEditorSpace.ResetBuffer();
    m_InitOK := True;

    Self.PanelSymbolEditorResize(nil);
    Self.PreparePreviewDisplay();
    dc := GetDC(LVSymbolList.Handle);
    m_pSymbolListDisplay.SetDC(dc);

    EditLineWidth.Text := '0.5';
    Self.ColorGridDrawChange(nil);
    m_SelectColor := clRed;
    m_VertexColor := clBlue;
    m_RotateCenterColor := clBlue;

    CBFillStyle.ItemIndex := 0;
    CBFillHatch.ItemIndex := 0;
    CBPointFillType.ItemIndex := 0;

    Self.SBNewSymbolLibClick(nil);
    m_LibDirty := False;

    if (m_SingleSymbolDlg) then begin
        if (not Assigned(m_pSingleSymbol)) then begin
            Self.NewSymbolLib(m_SinleSymbolType);
        end
        else begin
            Self.NewSymbolLib(m_pSingleSymbol);
        end;

        PanelTop.Visible := False;
        PanelBottom.Visible := True;
        PanelDown.Visible := False;
    end;
end;

procedure TFormMain.RefreshPointLayerList();
var
    code: Longint;
    symname: AnsiString;
    root, lyr: TTreeNode;
    i, count: Longint;
    pPS: IPointSymbol;
    pSPS: ISimplePointSymbol;
    pEPS: IEnvelopePointSymbol;
    pPPS: IPolyPointSymbol;
    colorlock, solid: Boolean;
    symtype: TPointSymbolType;
begin
    TVSubPointList.Items.Clear();
    PanelLayerCount.Caption := '   图元个数';
    if (not Assigned(m_pEditorPointSymbol)) then begin
        Self.EnablePointSymbolStuff(False);
        Exit;
    end;

    code := -1;
    try
        code := StrToInt(EditSymbolCode.Text);
    except
    end;
    if (-1 > code) then begin
        code := -1;
    end;
    m_pEditorPointSymbol.SetCode(code);

    symname := EditSymbolName.Text;
    m_pEditorPointSymbol.SetName(PChar(symname));
    root := TVSubPointList.Items.AddChild(nil, symname);
    root.ImageIndex := 0;
    root.SelectedIndex := 0;

    count := m_pEditorPointSymbol.GetSymbolCount();
    for i := 0 to count - 1 do begin
        m_pEditorPointSymbol.GetSymbolRef(pPS, i);
        pPS.GetColorLock(colorlock);
        symname := pPS.GetName();
        lyr := TVSubPointList.Items.AddChildFirst(root, PChar(symname));
        symtype := CheckPointSymbolType(pPS);
        case symtype of
            POINTSYMBOLTYPE_SIMPLE: begin
                pSPS := ISimplePointSymbol(GotoInterface(pPS, 'ISimplePointSymbol'));
                pSPS.GetSolid(solid);
                if (solid) then begin
                    if (colorlock) then lyr.ImageIndex := 10
                    else lyr.ImageIndex := 2;
                end
                else begin
                    if (colorlock) then lyr.ImageIndex := 11
                    else lyr.ImageIndex := 3;
                end;
                lyr.SelectedIndex := lyr.ImageIndex;
            end;
            POINTSYMBOLTYPE_ENVELOPE: begin;
                pEPS := IEnvelopePointSymbol(GotoInterface(pPS, 'IEnvelopePointSymbol'));
                pEPS.GetSolid(solid);
                if (solid) then begin
                    if (colorlock) then lyr.ImageIndex := 12
                    else lyr.ImageIndex := 4;
                end
                else begin
                    if (colorlock) then lyr.ImageIndex := 13
                    else lyr.ImageIndex := 5;
                end;
                lyr.SelectedIndex := lyr.ImageIndex;
            end;
            POINTSYMBOLTYPE_POLY: begin;
                pPPS := IPolyPointSymbol(GotoInterface(pPS, 'IPolyPointSymbol'));
                pPPS.GetSolid(solid);
                if (not solid) then begin
                    if (colorlock) then lyr.ImageIndex := 14
                    else lyr.ImageIndex := 6;
                end
                else begin
                    if (colorlock) then lyr.ImageIndex := 15
                    else lyr.ImageIndex := 7;
                end;
                lyr.SelectedIndex := lyr.ImageIndex;
            end;
            POINTSYMBOLTYPE_TEXT: begin;

            end;
            POINTSYMBOLTYPE_BITMAP: begin;

            end;
            else begin
                lyr.ImageIndex := 1;
                lyr.SelectedIndex := 1;
            end;
        end;

    end;
    root.Expand(True);
    PanelLayerCount.Caption := '   图元个数：' + IntToStr(count);
    Self.EnablePointSymbolStuff(True);
end;

procedure TFormMain.RefreshSubLineList();
var
    i, symbolcount: Longword;
    colorlock: Boolean;
    pLineSymbol: ILineSymbol;
    pSimpleLineSymbol: ISimpleLineSymbol;
    pPointLineSymbol: IPointLineSymbol;
    name: AnsiString;
    pNode: TTreeNode;
begin
    TVSubLine.Items.Clear();
    if (not Assigned(m_pEditorLineSymbol)) then Exit;

    symbolcount := m_pEditorLineSymbol.GetSymbolCount();
    if (symbolcount <= 0) then Exit;

    for i := 0 to symbolcount-1 do begin
        m_pEditorLineSymbol.GetSymbolRef(pLineSymbol, i);
        pLineSymbol.GetColorLock(colorlock);
        name := pLineSymbol.GetName();
        pNode := TVSubLine.Items.AddChildFirst(nil, name);
        pSimpleLineSymbol := ISimpleLineSymbol(GotoInterface(pLineSymbol, 'ISimpleLineSymbol'));
        pPointLineSymbol := IPointLineSymbol(GotoInterface(pLineSymbol, 'IPointLineSymbol'));
        if (Assigned(pSimpleLineSymbol)) then begin
            if (not colorlock) then begin
                pNode.ImageIndex := 1;
                pNode.SelectedIndex := 1;
            end
            else begin
                pNode.ImageIndex := 2;
                pNode.SelectedIndex := 2;
            end;
        end
        else if (Assigned(pPointLineSymbol)) then begin
            if (not colorlock) then begin
                pNode.ImageIndex := 3;
                pNode.SelectedIndex := 3;
            end
            else begin
                pNode.ImageIndex := 4;
                pNode.SelectedIndex := 4;
            end;
        end
        else begin
            pNode.ImageIndex := 0;
            pNode.SelectedIndex := 0;
        end;
    end;
end;

procedure TFormMain.RefreshSubFillList();
var
    i, symbolcount: Longword;
    colorlock: Boolean;
    pFillSymbol: IFillSymbol;
    pSimpleFillSymbol: ISimpleFillSymbol;
    pPointFillSymbol: IPointFillSymbol;
    name: AnsiString;
    pNode: TTreeNode;
begin
    TVSubFill.Items.Clear();
    if (not Assigned(m_pEditorFillSymbol)) then Exit;

    symbolcount := m_pEditorFillSymbol.GetSymbolCount();
    if (symbolcount <= 0) then Exit;

    for i := 0 to symbolcount-1 do begin
        m_pEditorFillSymbol.GetSymbolRef(pFillSymbol, i);
        pFillSymbol.GetColorLock(colorlock);
        name := pFillSymbol.GetName();
        pNode := TVSubFill.Items.AddChildFirst(nil, name);
        pSimpleFillSymbol := ISimpleFillSymbol(GotoInterface(pFillSymbol, 'ISimpleFillSymbol'));
        pPointFillSymbol := IPointFillSymbol(GotoInterface(pFillSymbol, 'IPointFillSymbol'));
        if (Assigned(pSimpleFillSymbol)) then begin
            if (not colorlock) then begin
                pNode.ImageIndex := 5;
                pNode.SelectedIndex := 5;
            end
            else begin
                pNode.ImageIndex := 6;
                pNode.SelectedIndex := 6;
            end;
        end
        else if (Assigned(pPointFillSymbol)) then begin
            if (not colorlock) then begin
                pNode.ImageIndex := 7;
                pNode.SelectedIndex := 7;
            end
            else begin
                pNode.ImageIndex := 8;
                pNode.SelectedIndex := 8;
            end;
        end
        else begin
            pNode.ImageIndex := 0;
            pNode.SelectedIndex := 0;
        end;
    end;

end;

function TFormMain.GetSubLineFromNodeIndex(const nodeindex: Longint;
    out pLineSymbol: ILineSymbol; out symbolindex: Longint): Boolean;
var
    count: Longint;
begin
    Result := False;
    pLineSymbol := nil;
    symbolindex := -1;
    if (not Assigned(m_pEditorLineSymbol)) then begin
        Exit;
    end;

    count := TVSubLine.Items.Count;
    if ((0 > nodeindex) or (count <= nodeindex)) then begin
        Exit;
    end;

    symbolindex := count - 1 - nodeindex;
    Result := m_pEditorLineSymbol.GetSymbolRef(pLineSymbol, symbolindex);
end;

function TFormMain.GetSelectedSubLine(out pLineSymbol: ILineSymbol;
    out symbolindex: Longint): Boolean;
begin
    Result := False;
    if (not Assigned(TVSubLine.Selected)) then begin
        Exit;
    end;
    if (not Assigned(m_pEditorLineSymbol)) then begin
        Exit;
    end;

    Result := Self.GetSubLineFromNodeIndex(TVSubLine.Selected.Index,
        pLineSymbol, symbolindex);
end;

procedure TFormMain.RefreshLineEditor(const pLineSymbol: ILineSymbol);
var
    pSimpleLineSymbol: ISimpleLineSymbol;
    pPointLineSymbol: IPointLineSymbol;
    pTemplate: ILineSimpleTemplate;
    pObj: IObj;
    name: AnsiString;
    offset, width, factor: Double;
    color: COLORREF;
    colorlock, firstmark: Boolean;
    i, count, sector: Longword;
begin
    FormLineTemplate.m_InProgress := True;
    
    FormLineTemplate.ClearPattStore();
    EditSubLineWidth.Enabled := True;
    BTSelectPointSymbol.Enabled := False;
    EditSubLineWidth.Text := '-1';

    if (not Assigned(pLineSymbol)) then begin
        PanelLineEditorBase.Visible := False;
        FormLineTemplate.m_InProgress := False;
        Exit;
    end;

    name := pLineSymbol.GetName();
    pLineSymbol.GetOffset(offset);
    pLineSymbol.GetColor(color);
    pLineSymbol.GetColorLock(colorlock);
    pSimpleLineSymbol := ISimpleLineSymbol(GotoInterface(pLineSymbol, 'ISimpleLineSymbol'));
    pPointLineSymbol := IPointLineSymbol(GotoInterface(pLineSymbol, 'IPointLineSymbol'));
    if (Assigned(pSimpleLineSymbol)) then begin
        pSimpleLineSymbol.GetTemplate(pTemplate);
        pSimpleLineSymbol.GetWidth(width);
    end
    else if (Assigned(pPointLineSymbol)) then begin
        pPointLineSymbol.GetTemplate(pTemplate);
        EditSubLineWidth.Enabled := False;
        BTSelectPointSymbol.Enabled := True;
    end
    else begin
        PanelLineEditorBase.Visible := False;
        FormLineTemplate.m_InProgress := False;
        Exit;
    end;

    EditSubLineName.Text := name;
    if (EditSubLineWidth.Enabled) then begin
        EditSubLineWidth.Text := FloatToStr(width);
    end;
    EditSubLineOffset.Text := FloatToStr(offset);
    PanelSubLineColor.Color := color;
    CBSubLineColorLock.Checked := colorlock;

    if (not Assigned(pTemplate)) then begin
        CreateObj('CLineSimpleTemplate', pObj);
        pTemplate := ILineSimpleTemplate(GotoInterface(pObj, 'ILineSimpleTemplate'));
    end;

    pTemplate.GetFactor(factor);
    FormLineTemplate.EditTemplFactor.Text := FloatToStr(factor);
    pTemplate.GetCount(count);
    if (0 < count) then begin
        pTemplate.GetFirstMark(firstmark);
        if (not firstmark) then FormLineTemplate.Add(0);

        for i := 0 to count - 1 do begin
            pTemplate.GetSector(i, sector);
            FormLineTemplate.Add(sector);
        end;
    end;
    FormLineTemplate.RefreshTemplate();

    FormLineTemplate.m_InProgress := False;
    PanelLineEditorBase.Visible := True;
end;

procedure TFormMain.RefreshFillEditor(const pFillSymbol: IFillSymbol);
var
    pSimpleFillSymbol: ISimpleFillSymbol;
    pPointFillSymbol: IPointFillSymbol;
    pBorderSymbol: ILineSymbol;
    name: AnsiString;
    color: COLORREF;
    width: Double;
    space_x, space_y, offset_x, offset_y: Double;
    fillstyle, fillhatch: Longint;
    colorlock: Boolean;
begin
    m_ChangeSymbolProgress := True;

    if (not Assigned(pFillSymbol)) then begin
        PanelFillEditorBase.Visible := False;
        m_ChangeSymbolProgress := False;
        Exit;
    end;

    name := pFillSymbol.GetName();

    pFillSymbol.GetColorLock(colorlock);
    CBSubFillColorLock.Checked := colorlock;
    pSimpleFillSymbol := ISimpleFillSymbol(GotoInterface(pFillSymbol, 'ISimpleFillSymbol'));
    pPointFillSymbol := IPointFillSymbol(GotoInterface(pFillSymbol, 'IPointFillSymbol'));
    if (Assigned(pSimpleFillSymbol)) then begin
        GBSimpleFill.Visible := True;
        GBPointFill.Visible := False;

        pSimpleFillSymbol.GetColor(color);
        PanelSubFillColor.Color := color;

        pSimpleFillSymbol.GetBorderColor(color);
        PanelSubBorderColor.Color := color;

        pSimpleFillSymbol.GetBorderWidth(width);
        EditBorderWidth.Text := FloatToStr(width);

        CBFillStyle.ItemIndex := 0;
        pSimpleFillSymbol.GetFillStyle(fillstyle);
        if ((fillstyle >= 0) and (fillstyle < CBFillStyle.Items.Count)) then begin
            CBFillStyle.ItemIndex := fillstyle;
        end;

        CBFillHatch.ItemIndex := 0;
        pSimpleFillSymbol.GetFillHatch(fillhatch);
        if ((fillhatch >= 0) and (fillhatch < CBFillHatch.Items.Count)) then begin
            CBFillHatch.ItemIndex := fillhatch;
        end;       
    end
    else if (Assigned(pPointFillSymbol)) then begin
        GBSimpleFill.Visible := False;
        GBPointFill.Visible := True;

        pPointFillSymbol.GetPointsSpace(space_x, space_y);
        pPointFillSymbol.GetPointsOffset(offset_x, offset_y);
        EditFillPointSpaceX.Text := FloatToStr(space_x);
        EditFillPointSpaceY.Text := FloatToStr(space_y);
        EditFillPointOffsetX.Text := FloatToStr(offset_x);
        EditFillPointOffsetY.Text := FloatToStr(offset_y);

        CBPointFillType.ItemIndex := 0;
        pPointFillSymbol.GetFillStyle(fillstyle);
        if (fillstyle = POINTFILLSTYLE_LABELPOINT) then begin
            CBPointFillType.ItemIndex := 1;
        end;

        CBPointFillBorderAvailable.Checked := False;
        pPointFillSymbol.GetBorderSymbol(pBorderSymbol);
        if (Assigned(pBorderSymbol)) then begin
            CBPointFillBorderAvailable.Checked := True;
        end;
    end
    else begin
        PanelFillEditorBase.Visible := False;
        m_ChangeSymbolProgress := False;
        Exit;
    end;

    EditSubFillName.Text := name;
    CBSubLineColorLock.Checked := colorlock;

    m_ChangeSymbolProgress := False;
    PanelFillEditorBase.Visible := True;
end;

procedure TFormMain.RefreshFillEditor();
var
    pFillSymbol: IFillSymbol;
    index: Longint;
begin
    //面符号编辑器需要刷新
    Self.GetSelectedSubFill(pFillSymbol, index);
    Self.RefreshFillEditor(pFillSymbol);
end;

function TFormMain.GetSubFillFromNodeIndex(const nodeindex: Longint;
    out pFillSymbol: IFillSymbol; out symbolindex: Longint): Boolean;
var
    count: Longint;
begin
    Result := False;
    pFillSymbol := nil;
    symbolindex := -1;
    if (not Assigned(m_pEditorFillSymbol)) then begin
        Exit;
    end;

    count := TVSubFill.Items.Count;
    if ((0 > nodeindex) or (count <= nodeindex)) then begin
        Exit;
    end;

    symbolindex := count - 1 - nodeindex;
    Result := m_pEditorFillSymbol.GetSymbolRef(pFillSymbol, symbolindex);
end;

function TFormMain.GetSelectedSubFill(out pFillSymbol: IFillSymbol;
    out symbolindex: Longint): Boolean;
begin
    Result := False;
    if (not Assigned(TVSubFill.Selected)) then begin
        Exit;
    end;
    if (not Assigned(m_pEditorFillSymbol)) then begin
        Exit;
    end;

    Result := Self.GetSubFillFromNodeIndex(TVSubFill.Selected.Index,
        pFillSymbol, symbolindex);
end;

procedure TFormMain.EnablePointSymbolStuff(const enable: Boolean);
begin
    PanelPointSymbolEditor.Visible := enable;
    PanelRightList.Visible := enable;
    PanelRightBottom.Visible := enable;
    if (enable) then begin
        PanelLineSymbolEditor.Visible := False;
        PanelFillSymbolEditor.Visible := False;
        PanelSubLineList.Visible := False;
        PanelSubFillList.Visible := False;
    end;
end;

procedure TFormMain.EnableLineSymbolStuff(const enable: Boolean);
begin
    PanelLineSymbolEditor.Visible := enable;
    PanelSubLineList.Visible := enable;
    if (enable) then begin
        PanelPointSymbolEditor.Visible := False;
        PanelFillSymbolEditor.Visible := False;
        PanelRightList.Visible := False;
        PanelRightBottom.Visible := False;
        PanelSubFillList.Visible := False;
    end;
end;

procedure TFormMain.EnableFillSymbolStuff(const enable: Boolean);
begin
    PanelFillSymbolEditor.Visible := enable;
    PanelSubFillList.Visible := enable;
    if (enable) then begin
        PanelPointSymbolEditor.Visible := False;
        PanelLineSymbolEditor.Visible := False;
        PanelRightList.Visible := False;
        PanelRightBottom.Visible := False;
        PanelSubLineList.Visible := False;
    end;
end;

procedure TFormMain.CheckSelectedPointLayer(const index: Longint);
var
    nodetext, symname, tmp: AnsiString;
    lyr: TTreeNode;
    i, count, id, txtlen: Longint;
    pPS: IPointSymbol;
    pSPS: ISimplePointSymbol;
    pEPS: IEnvelopePointSymbol;
    pPPS: IPolyPointSymbol;
    solid: Boolean;
    symtype: TPointSymbolType;
begin
    m_pEditorPointSymbol.GetSymbolRef(pPS, index);
    if (not Assigned(pPS)) then Exit;
    count := TVLayersSelected.Items.Count;
    for i := 0 to count - 1 do begin
        lyr := TVLayersSelected.Items.Item[i];
        nodetext := lyr.Text;
        txtlen := StrLen(PChar(nodetext));
        tmp := Copy(lyr.Text, txtlen - 2, 3);
        id := StrToInt(tmp);
        if (id = index) then begin
            Self.PointSymbolEditorDraw();
            m_pEditorSpace.PostBuffer();
            SubSymbolAttrib.SetSymbol(pPS);//嘿嘿
            Exit;
        end;
    end;

    symname := pPS.GetName() + '  ' + IntToStr(index);
    lyr := TVLayersSelected.Items.AddChildFirst(nil, symname);
    lyr.Selected := True;

    symtype := CheckPointSymbolType(pPS);
    case symtype of
        POINTSYMBOLTYPE_SIMPLE: begin
            pSPS := ISimplePointSymbol(GotoInterface(pPS, 'ISimplePointSymbol'));
            pSPS.GetSolid(solid);
            if (solid) then lyr.ImageIndex := 16
            else lyr.ImageIndex := 17;
        end;
        POINTSYMBOLTYPE_ENVELOPE: begin;
            pEPS := IEnvelopePointSymbol(GotoInterface(pPS, 'IEnvelopePointSymbol'));
            pEPS.GetSolid(solid);
            if (solid) then lyr.ImageIndex := 18
            else lyr.ImageIndex := 19;
        end;
        POINTSYMBOLTYPE_POLY: begin;
            pPPS := IPolyPointSymbol(GotoInterface(pPS, 'IPolyPointSymbol'));
            pPPS.GetSolid(solid);
            if (not solid) then lyr.ImageIndex := 20
            else lyr.ImageIndex := 21;
        end;
        POINTSYMBOLTYPE_TEXT: begin;

        end;
        POINTSYMBOLTYPE_BITMAP: begin;

        end;
        else begin
            lyr.ImageIndex := 1;
        end;
    end;

    lyr.SelectedIndex := lyr.ImageIndex;
    lyr.Selected := True;
    TVLayersSelected.Refresh();
    TVLayersSelectedClick(nil);

    Self.PointSymbolEditorDraw();
    m_pEditorSpace.PostBuffer();
end;

procedure TFormMain.ClearSelectedPointLayers();
begin
    TVLayersSelected.Items.Clear();
end;

function TFormMain.GetSelectedPointLayerByIndex(const index: Longint;
    out pPointSymbol: IPointsymbol; out parentindex: Longint): Boolean;
var
    nodetext, tmp: AnsiString;
    lyr: TTreeNode;
    count, txtlen: Longint;
begin
    Result := False;
    if (0 > index) then Exit;
    count := TVLayersSelected.Items.Count;
    if (count <= index) then Exit;

    lyr := TVLayersSelected.Items.Item[index];
    nodetext := lyr.Text;
    txtlen := StrLen(PChar(nodetext));
    tmp := Copy(lyr.Text, txtlen - 2, 3);
    parentindex := StrToInt(tmp);

    m_pEditorPointSymbol.GetSymbolRef(pPointSymbol, parentindex);
    Result := True;
end;

procedure TFormMain.PreparePreviewDisplay();
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

procedure TFormMain.PrepareSymbolListDisplay(rect: TRect);
var
    pTrans: IDisplayTransformation;
    f: Double;
begin
    m_pSymbolListDisplay.GetDisplayTransformation(pTrans);
    pTrans.SetMapUnit(UNIT_MM);

    pTrans.SetDeviceRect(rect);
    f := 1;
    pTrans.SetMapScale(f);
    f := 1;
    pTrans.SetReferenceScale(f);
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

procedure TFormMain.SymbolListDraw(const rect: TRect; const pSymbol: ISymbol);
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
    x := wkspntz.x;
    y := wkspntz.y;
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
        SYMBOLTYPE_TEXT: m_pPreviewDisplay.DrawTextXY(x, y, '毛猫', recttmp);
    end;
    m_pSymbolListDisplay.FinishDraw();
    m_pSymbolListDisplay.RefreshWindow1();
end;

function TFormMain.GetPointLayerIndexFromTree(): Longint;
var
    root, node: TTreeNode;
begin
    Result := -1;
    root := TVSubPointList.Items.GetFirstNode();
    node := TVSubPointList.Selected;
    if (not Assigned(node)) then Exit;
    if (node = root) then Exit;
    Result := root.Count - node.Index - 1;
end;

function TFormMain.GetPointLayerFromTree(): IPointSymbol;
begin
    Result := nil;
    if (not Assigned(m_pEditorPointSymbol)) then Exit;
    m_pEditorPointSymbol.GetSymbolRef(Result, Self.GetPointLayerIndexFromTree());
end;

procedure TFormMain.RotatePointSelectedLayer(const delta: Longint);
var
    i, count, id: Longint;
    pPS: IPointSymbol;
    angle: Double;
begin
    count := TVLayersSelected.Items.Count;
    if (0 >= count) then Exit;
    for i := 0 to count - 1 do begin
        GetSelectedPointLayerByIndex(i, pPS, id);
        pPS.GetAngle(angle);
        angle := angle - delta;
        pPS.SetAngle(angle);
    end;

end;

procedure TFormMain.TimerCatTimer(Sender: TObject);
begin
    ImageCat1.Left := ImageCat1.Left + 2;
    TimerCat.Tag := TimerCat.Tag - 2;
    if (TimerCat.Tag <= 0) then begin
        TimerCat.Enabled := False;
    end;
end;

procedure TFormMain.ImageCat1Click(Sender: TObject);
begin
    TimerCat.Tag := 300;
    TimerCat.Enabled := True;
end;

procedure TFormMain.BlowTheShape(pSymbol: IPolyPointSymbol);
var
    node, pnt: WKSPoint;
    i, count: Longint;
begin
    count := pSymbol.GetNodeCount();
    pnt.x := 0;
    pnt.y := 0;
    for i := 0 to count - 1 do begin
        pSymbol.GetNode(i, node);
        pnt.x := pnt.x + node.x;
        pnt.y := pnt.y + node.y;
    end;

    pnt.x := pnt.x / count;
    pnt.y := pnt.y / count;

    for i := 0 to count - 1 do begin
        pSymbol.GetNode(i, node);
        node.x := node.x - pnt.x;
        node.y := node.y - pnt.y;
        pSymbol.SetNode(i, node);
    end;

    pSymbol.SetOffset(pnt.x, pnt.y);
end;

procedure TFormMain.RefreshEditSymbol();
var
    Item: TListItem;
    pSymbol: ISymbol;
    pMultiPointSymbol: IMultiPointSymbol;
    pMultiLineSymbol: IMultiLineSymbol;
    pMultiFillSymbol: IMultiFillSymbol;
    pPointSymbol: IPointSymbol;
    pLineSymbol: ILineSymbol;
    pFillSymbol: IFillSymbol;
begin
    Self.UpdatePointSymbolEditor(nil);
    Self.UpdateLineSymbolEditor(nil);
    Self.UpdateFillSymbolEditor(nil);

    Item := LVSymbolList.Selected;
    if (not Assigned(Item)) then Exit;
    m_ChangeSymbolProgress := True;

    m_pSymbolLib.GetSymbolRef(pSymbol, Item.Index);
    EditSymbolName.Text := pSymbol.GetName();
    EditSymbolCode.Text := IntToStr(pSymbol.GetCode());

    pMultiPointSymbol := IMultiPointSymbol(GotoInterface(pSymbol, 'IMultiPointSymbol'));
    if (Assigned(pMultiPointSymbol)) then begin
        Self.UpdatePointSymbolEditor(pMultiPointSymbol);
        m_ChangeSymbolProgress := False;
        Exit;
    end;

    pPointSymbol := IPointSymbol(GotoInterface(pSymbol, 'IPointSymbol'));
    if (Assigned(pPointSymbol)) then begin
        CreateMultiPointSymbol(pMultiPointSymbol);
        pMultiPointSymbol.AddSymbol(pPointSymbol);
        Self.UpdatePointSymbolEditor(pMultiPointSymbol);
        m_ChangeSymbolProgress := False;
        Exit;
    end;

    pMultiLineSymbol := IMultiLineSymbol(GotoInterface(pSymbol, 'IMultiLineSymbol'));
    if (Assigned(pMultiLineSymbol)) then begin
        Self.UpdateLineSymbolEditor(pMultiLineSymbol);
        m_ChangeSymbolProgress := False;
        Exit;
    end;

    pLineSymbol := ILineSymbol(GotoInterface(pSymbol, 'ILineSymbol'));
    if (Assigned(pLineSymbol)) then begin
        CreateMultiLineSymbol(pMultiLineSymbol);
        pMultiLineSymbol.AddSymbol(pLineSymbol);
        Self.UpdateLineSymbolEditor(pMultiLineSymbol);
        m_ChangeSymbolProgress := False;
        Exit;
    end;

    pMultiFillSymbol := IMultiFillSymbol(GotoInterface(pSymbol, 'IMultiFillSymbol'));
    if (Assigned(pMultiFillSymbol)) then begin
        Self.UpdateFillSymbolEditor(pMultiFillSymbol);
        m_ChangeSymbolProgress := False;
        Exit;
    end;

    pFillSymbol := IFillSymbol(GotoInterface(pSymbol, 'IFillSymbol'));
    if (Assigned(pFillSymbol)) then begin
        CreateMultiFillSymbol(pMultiFillSymbol);
        pMultiFillSymbol.AddSymbol(pFillSymbol);
        Self.UpdateFillSymbolEditor(pMultiFillSymbol);
        m_ChangeSymbolProgress := False;
        Exit;
    end;

    //什么都不是
    Self.UpdatePointSymbolEditor(nil);
    Self.UpdateLineSymbolEditor(nil);
    Self.UpdateFillSymbolEditor(nil);
    m_ChangeSymbolProgress := False;
end;

procedure TFormMain.PointLayerModerfied();
var
    i, absolutindex: Longint;
begin
    if (not m_InitOK) then Exit;

    Self.PushEdit();
    Self.PointSymbolEditorDraw();
    m_pEditorSpace.PostBuffer();

    if (Assigned(TVSubPointList.Selected)) then begin
        absolutindex := TVSubPointList.Selected.AbsoluteIndex;
        Self.RefreshPointLayerList();
        for i := 0 to TVSubPointList.Items[0].Count - 1 do begin
            if (TVSubPointList.Items[0].Item[i].AbsoluteIndex = absolutindex) then begin
                TVSubPointList.Items[0].Item[i].Selected := True;
                Break;
            end;
        end;
    end;
end;

function TFormMain.CheckVertexHit(pnt: WKSPoint): Boolean;
var
    i, count: Longint;
    pPntSym: IPointSymbol;
    symtype: TPointSymbolType;
    selected: Boolean;
begin
    Result := False;
    if (not Assigned(m_pEditorPointSymbol)) then Exit;
    count := m_pEditorPointSymbol.GetSymbolCount();
    for i := 0 to count - 1 do begin
        if (not Self.CheckLayerSelected(i)) then Continue;
        selected := False;
        m_pEditorPointSymbol.GetSymbolRef(pPntSym, i);
        symtype := CheckPointSymbolType(pPntSym);
        case symtype of
            POINTSYMBOLTYPE_SIMPLE: Self.CheckVertexHitSimplePoint(pPntSym, pnt, selected);
            POINTSYMBOLTYPE_ENVELOPE: Self.CheckVertexHitEnvelopePoint(pPntSym, pnt, selected);
            POINTSYMBOLTYPE_BITMAP: Self.CheckVertexHitBitmapPoint(pPntSym, pnt, selected);
            POINTSYMBOLTYPE_TEXT: Self.CheckVertexHitTextPoint(pPntSym, pnt, selected);
            POINTSYMBOLTYPE_POLY: Self.CheckVertexHitPolyPoint(pPntSym, pnt, selected);
            else begin
                Continue;
            end;
        end;

        if (selected) then Break;
    end;

    Result := selected;
end;

procedure TFormMain.CheckVertexHitSimplePoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
    out selected: Boolean);
var
    pSPS: ISimplePointSymbol;
    diameter: Double;
    wkspnt, wkspnt1: WKSPoint;
begin
    selected := False;

    pSPS := ISimplePointSymbol(GotoInterface(pSymbol, 'ISimplePointSymbol'));
    pSPS.GetDiameter(diameter);
    pSPS.GetOffset(wkspnt.x, wkspnt.y);

    wkspnt1 := wkspnt;
    wkspnt1.x := wkspnt.x + diameter/2;
    selected := CheckCursorHitVertex(wkspnt1, pnt);
    if (selected) then begin
        m_VertexEditSymbol := pSymbol;
        m_VertexEditIndex := 0;
        Exit;
    end;

    wkspnt1 := wkspnt;
    wkspnt1.x := wkspnt.x - diameter/2;
    selected := CheckCursorHitVertex(wkspnt1, pnt);
    if (selected) then begin
        m_VertexEditSymbol := pSymbol;
        m_VertexEditIndex := 1;
        Exit;
    end;

    wkspnt1 := wkspnt;
    wkspnt1.y := wkspnt.y + diameter/2;
    selected := CheckCursorHitVertex(wkspnt1, pnt);
    if (selected) then begin
        m_VertexEditSymbol := pSymbol;
        m_VertexEditIndex := 2;
        Exit;
    end;

    wkspnt1 := wkspnt;
    wkspnt1.y := wkspnt.y - diameter/2;
    selected := CheckCursorHitVertex(wkspnt1, pnt);
    if (selected) then begin
        m_VertexEditSymbol := pSymbol;
        m_VertexEditIndex := 3;
        Exit;
    end;
end;

procedure TFormMain.CheckVertexHitEnvelopePoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
    out selected: Boolean);
var
    pEPS: IEnvelopePointSymbol;
    width: Double;
    height: Double;
    offsetx: Double;
    offsety: Double;
    angle: Double;
    wkspnt, origin: WKSPoint;
begin
    selected := False;

    pEPS := IEnvelopePointSymbol(GotoInterface(pSymbol, 'IEnvelopePointSymbol'));
    pEPS.GetWidth(width);
    pEPS.GetHeight(height);
    pEPS.GetOffset(offsetx, offsety);
    pEPS.GetAngle(angle);

    origin.x := 0; origin.y := 0;
    wkspnt.x := -width/2;
    wkspnt.y := -height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    selected := CheckCursorHitVertex(wkspnt, pnt);
    if (selected) then begin
        m_VertexEditSymbol := pSymbol;
        m_VertexEditIndex := 0;
        Exit;
    end;

    wkspnt.x := -width/2;
    wkspnt.y := height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    selected := CheckCursorHitVertex(wkspnt, pnt);
    if (selected) then begin
        m_VertexEditSymbol := pSymbol;
        m_VertexEditIndex := 1;
        Exit;
    end;

    wkspnt.x := width/2;
    wkspnt.y := height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    selected := CheckCursorHitVertex(wkspnt, pnt);
    if (selected) then begin
        m_VertexEditSymbol := pSymbol;
        m_VertexEditIndex := 2;
        Exit;
    end;

    wkspnt.x := width/2;
    wkspnt.y := -height/2;
    RotateDegree(wkspnt, origin, angle);
    wkspnt.x := wkspnt.x + offsetx;
    wkspnt.y := wkspnt.y + offsety;
    selected := CheckCursorHitVertex(wkspnt, pnt);
    if (selected) then begin
        m_VertexEditSymbol := pSymbol;
        m_VertexEditIndex := 3;
        Exit;
    end;
end;

procedure TFormMain.CheckVertexHitBitmapPoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
    out selected: Boolean);
begin
    selected := False;
end;

procedure TFormMain.CheckVertexHitTextPoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
    out selected: Boolean);
begin
    selected := False;
end;

procedure TFormMain.CheckVertexHitPolyPoint(const pSymbol: IPointSymbol; const pnt: WKSPoint;
    out selected: Boolean);
var
    pPPS: IPolyPointSymbol;
    offsetx: Double;
    offsety: Double;
    angle: Double;
    wkspnt, origin: WKSPoint;
    i: Longint;
begin
    selected := False;

    pPPS := IPolyPointSymbol(GotoInterface(pSymbol, 'IPolyPointSymbol'));
    pPPS.GetOffset(offsetx, offsety);
    pPPS.GetAngle(angle);
    origin.x := 0;
    origin.y := 0;
    for i := 0 to pPPS.GetNodeCount() - 1 do begin
        pPPS.GetNode(i, wkspnt);
        RotateDegree(wkspnt, origin, angle);
        wkspnt.x := wkspnt.x + offsetx;
        wkspnt.y := wkspnt.y + offsety;
        selected := CheckCursorHitVertex(wkspnt, pnt);
        if (selected) then begin
            m_VertexEditSymbol := pSymbol;
            m_VertexEditIndex := i;
            Break;
        end;
    end;
end;

function TFormMain.CheckCursorHitVertex(const cursor: WKSPoint; const vertex: WKSPoint): Boolean;
var
    x, y: Longint;
begin
    Result := False;
    x := Round(Abs(cursor.x - vertex.x)*m_EditScale);
    y := Round(Abs(cursor.y - vertex.y)*m_EditScale);
    if ((6 > x) and (6 > y)) then begin
        Result := True;
    end;
end;

procedure TFormMain.MoveVertexSelected(const offset: WKSPoint; const newpos: WKSPoint);
var
    pointsymboltype: TPointSymbolType;
    pSimplePointSymbol: ISimplePointSymbol;
    pEnvelopePointSymbol: IEnvelopePointSymbol;
    pPolyPointSymbol: IPolyPointSymbol;
begin
    if (not Assigned(m_VertexEditSymbol)) then Exit;


    pointsymboltype := CheckPointSymbolType(m_VertexEditSymbol);
    case pointsymboltype of
        POINTSYMBOLTYPE_SIMPLE: begin
            pSimplePointSymbol := ISimplePointSymbol(GotoInterface(m_VertexEditSymbol, 'ISimplePointSymbol'));
            Self.MoveVertexSimplePoint(pSimplePointSymbol, offset);
        end;

        POINTSYMBOLTYPE_ENVELOPE: begin
            pEnvelopePointSymbol := IEnvelopePointSymbol(GotoInterface(m_VertexEditSymbol, 'IEnvelopePointSymbol'));
            Self.MoveVertexEnvelopePoint(pEnvelopePointSymbol, offset);
        end;

        POINTSYMBOLTYPE_POLY: begin
            pPolyPointSymbol := IPolyPointSymbol(GotoInterface(m_VertexEditSymbol, 'IPolyPointSymbol'));
            Self.MoveVertexPolyPoint(pPolyPointSymbol, newpos);
        end;

        else begin
            
        end;
    end;      
end;

procedure TFormMain.MoveVertexSimplePoint(pSymbol: ISimplePointSymbol;
    const offset: WKSPoint);
var
    diameter: Double;
begin
    pSymbol.GetDiameter(diameter);
    diameter := diameter + offset.y;
    pSymbol.SetDiameter(diameter);
end;

procedure TFormMain.MoveVertexEnvelopePoint(pSymbol: IEnvelopePointSymbol;
    const offset: WKSPoint);
var
    width: Double;
    height: Double;
begin
    pSymbol.GetWidth(width);
    pSymbol.Getheight(height);
    width := width + offset.x;
    height := height + offset.y;
    pSymbol.SetWidth(width);
    pSymbol.Setheight(height);
end;

procedure TFormMain.MoveVertexPolyPoint(pSymbol: IPolyPointSymbol;
    const newpos: WKSPoint);
var
    origin, node, offset: WKSPoint;
    angle: Double;
begin
    origin.x := 0;
    origin.y := 0;
    pSymbol.GetAngle(angle);
    node := newpos;
    pSymbol.GetOffset(offset.x, offset.y);
    node.x := node.x - offset.x;
    node.y := node.y - offset.y;
    RotateDegree(node, origin, 360 - angle);
    pSymbol.SetNode(m_VertexEditIndex, node);
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
    dc: HDC;
begin
    m_InitOK := False;
    m_pPreviewDisplay.GetDC(dc);
    ReleaseDC(PanelSymbolPreview.Handle, dc);
    m_pSymbolListDisplay.GetDC(dc);
    ReleaseDC(LVSymbolList.Handle, dc);
    m_pEditorSpace.Free();
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
    pObj: IObj;
begin
    m_pSingleSymbol := nil;
    m_SingleSymbolDlg := False;

    m_InitOK := False;
    m_LibDirty := False;
    m_pSymbolLibFileName := '';
    m_pEditorSpace := nil;
    m_pPreviewDisplay := nil;
    m_pEditorPointSymbol := nil;
    m_pEditorLineSymbol := nil;
    m_pEditorFillSymbol := nil;
    m_VertexEditSymbol := nil;
    m_VertexEditIndex := -1;
    m_VertexEditStart := False;
    m_EditScale := 20;
    m_LineWidth := 1;
    m_EditorTask := TEDITORTASK_DEFAULT;
    m_ChangeSymbolProgress := False;
    m_ChangeSymbolProgress := False;

    CreateObj('CSymbolLib', pObj);
    m_pEditUndoList := ISymbolLib(GoToInterface(pObj, 'ISymbolLib'));

    CreateObj('CMiniDisplay', pObj);
    m_pPreviewDisplay := IDisplay(GotoInterface(pObj, 'IDisplay'));

    CreateObj('CMiniDisplay', pObj);
    m_pSymbolListDisplay := IDisplay(GotoInterface(pObj, 'IDisplay'));
    
    SubSymbolAttrib := TSubSymbolAttrib.Create(Self);
    NewSymbol := TNewSymbol.Create(Self);
    FormLineTemplate := TFormLineTemplate.Create(Self);
    FormSymbolSelector := TFormSymbolSelector.Create(Self);
end;

procedure TFormMain.PanelSymbolEditorResize(Sender: TObject);
begin
    if (not m_InitOK) then Exit;
    m_pEditorSpace.ResetBuffer();
    Self.PointSymbolEditorDraw();
    m_pEditorSpace.PostBuffer();
end;

procedure TFormMain.SBAddCircleSolidClick(Sender: TObject);
begin
    m_EditorTask := TEDITORTASK_NEWCIRCLESOLID;
    m_pEditorSpace.SetTask(SPACETASK_TRACKCIRCLE2);
    m_pEditorSpace.SetCursor(MOUSECURSOR_CROSS2);
    EditLayerName.Text := '圆点';
end;

procedure TFormMain.SBAddCircleHollowClick(Sender: TObject);
begin
    m_EditorTask := TEDITORTASK_NEWCIRCLEHOLLOW;
    m_pEditorSpace.SetTask(SPACETASK_TRACKCIRCLE2);
    m_pEditorSpace.SetCursor(MOUSECURSOR_CROSS2);
    EditLayerName.Text := '圆环';
end;

procedure TFormMain.SBSelectClick(Sender: TObject);
begin
    m_EditorTask := TEDITORTASK_SELECT;
    m_pEditorSpace.SetCursor(MOUSECURSOR_ARROW2);
    m_pEditorSpace.SetTask(SPACETASK_IDLE);
    Self.PointSymbolEditorDraw();
    m_pEditorSpace.PostBuffer();
end;

procedure TFormMain.SBEditorZoomInClick(Sender: TObject);
begin
    if (120 < m_EditScale) then Exit;
    m_EditScale := m_EditScale*2;
    if (Assigned(m_pEditorPointSymbol)) then begin
        Self.PointSymbolEditorDraw();
        m_pEditorSpace.PostBuffer();
    end;
end;

procedure TFormMain.SBEditorZoomOutClick(Sender: TObject);
begin
    if (10 > m_EditScale) then Exit;
    m_EditScale := m_EditScale div 2;
    if (Assigned(m_pEditorPointSymbol)) then begin
        Self.PointSymbolEditorDraw();
        m_pEditorSpace.PostBuffer();
    end;
end;

procedure TFormMain.SBAddEnvSolidClick(Sender: TObject);
begin
    m_EditorTask := TEDITORTASK_NEWENVELOPESOLID;
    m_pEditorSpace.SetTask(SPACETASK_TRACKRECTANGLE2);
    m_pEditorSpace.SetCursor(MOUSECURSOR_CROSS2);
    EditLayerName.Text := '实心矩形';
end;

procedure TFormMain.SBAddEnvHollowClick(Sender: TObject);
begin
    m_EditorTask := TEDITORTASK_NEWENVELOPEHOLLOW;
    m_pEditorSpace.SetTask(SPACETASK_TRACKRECTANGLE2);
    m_pEditorSpace.SetCursor(MOUSECURSOR_CROSS2);
    EditLayerName.Text := '空心矩形';
end;

procedure TFormMain.ColorGridDrawChange(Sender: TObject);
begin
    PanelDrawColor.Color := ColorGridDraw.ForegroundColor;
    m_pEditorSpace.SetBackColor(ColorGridDraw.BackgroundColor);
    Self.PointSymbolEditorDraw();
    m_pEditorSpace.PostBuffer();
end;

procedure TFormMain.EditLineWidthChange(Sender: TObject);
var
    linewidth: Double;
begin
    try
        linewidth := StrToFloat(EditLineWidth.Text);
    except
        Exit;
    end;
    m_LineWidth := linewidth;
end;

procedure TFormMain.SBZoomFitClick(Sender: TObject);
begin
    m_EditScale := 20;
    if (Assigned(m_pEditorPointSymbol)) then begin
        Self.PointSymbolEditorDraw();
        m_pEditorSpace.PostBuffer();
    end;
end;

procedure TFormMain.SBAddPathClick(Sender: TObject);
begin
    m_EditorTask := TEDITORTASK_NEWPATH;
    m_pEditorSpace.SetTask(SPACETASK_TRACKLINE);
    m_pEditorSpace.SetCursor(MOUSECURSOR_CROSS2);
    EditLayerName.Text := '折线';
end;

procedure TFormMain.SBAddSolidPolygonClick(Sender: TObject);
begin
    m_EditorTask := TEDITORTASK_NEWPOLYGONSOLID;
    m_pEditorSpace.SetTask(SPACETASK_TRACKPOLYGON);
    m_pEditorSpace.SetCursor(MOUSECURSOR_CROSS2);
    EditLayerName.Text := '多边形';
end;

procedure TFormMain.SpeedButton4Click(Sender: TObject);
var
    pTrans: IDisplayTransformation;
    f: Double;
begin
    m_pPreviewDisplay.GetDisplayTransformation(pTrans);
    f := 1;
    pTrans.SetMapScale(f);
    Self.SymbolPreviewDraw();
    m_pPreviewDisplay.RefreshWindow1();
end;

procedure TFormMain.SpeedButton3Click(Sender: TObject);
var
    pTrans: IDisplayTransformation;
    f: Double;
begin
    m_pPreviewDisplay.GetDisplayTransformation(pTrans);
    pTrans.GetMapScale(f);
    if (0.2 > f) then Exit;
    f := f / 2;
    pTrans.SetMapScale(f);
    Self.SymbolPreviewDraw();
    m_pPreviewDisplay.RefreshWindow1();
end;

procedure TFormMain.SpeedButton5Click(Sender: TObject);
var
    pTrans: IDisplayTransformation;
    f: Double;
begin
    m_pPreviewDisplay.GetDisplayTransformation(pTrans);
    pTrans.GetMapScale(f);
    if (3 < f) then Exit;
    f := f * 2;
    pTrans.SetMapScale(f);
    Self.SymbolPreviewDraw();
    m_pPreviewDisplay.RefreshWindow1();
end;

procedure TFormMain.EditLineWidthExit(Sender: TObject);
begin
    try
        StrToFloat(EditLineWidth.Text);
    except
        Exit;
    end;
    EditLineWidth.Text := FloatToStr(m_LineWidth);
end;

procedure TFormMain.EditSymbolCodeExit(Sender: TObject);
var
    code: Longint;
begin
    code := -1;
    try
        code := StrToInt(EditSymbolCode.Text);
    except
        EditSymbolCode.Text := '?';
    end;

    if (-1 > code) then begin
        EditSymbolCode.Text := '-1';
    end;
end;

procedure TFormMain.EditSymbolCodeChange(Sender: TObject);
var
    code: Longint;
begin
    if (m_ChangeSymbolProgress) then Exit;

    try
        code := StrToInt(EditSymbolCode.Text);
    except
        Exit;
    end;

    if (Assigned(m_pEditorPointSymbol)) then begin
        m_pEditorPointSymbol.SetCode(code);
    end
    else if (Assigned(m_pEditorLineSymbol)) then begin
        m_pEditorLineSymbol.SetCode(code);
    end
    else if (Assigned(m_pEditorFillSymbol)) then begin
        m_pEditorLineSymbol.SetCode(code);
    end;
end;

procedure TFormMain.SBNewSymbolClick(Sender: TObject);
var
    pMPS: IMultiPointSymbol;
    pMLS: IMultiLineSymbol;
    pMFS: IMultiFillSymbol;
    code: Longint;
    name: Ansistring;
begin
    NewSymbol.ShowModal();
    if (0 = NewSymbol.select) then Exit;

    if (1 = NewSymbol.select) then begin
        m_ChangeSymbolProgress := True;
        CreateMultiPointSymbol(pMPS);
        code := Self.GetNextSymbolCode();
        pMPS.SetCode(code);
        name := Self.GetUndefinedSymbolName();
        pMPS.SetName(PChar(name));
        m_pSymbolLib.AddSymbol(pMPS);
        Self.RefreshSymbolList();
        LVSymbolList.Items.Item[LVSymbolList.Items.Count - 1].Selected := True;

        Self.UpdatePointSymbolEditor(pMPS);
        Self.PointSymbolEditorDraw();
        Self.m_pEditorSpace.PostBuffer();
        Self.RefreshPointLayerList();

        m_ChangeSymbolProgress := False;
        m_LibDirty := True;
    end
    else if (2 = NewSymbol.select) then begin
        m_ChangeSymbolProgress := True;
        CreateMultiLineSymbol(pMLS);
        code := Self.GetNextSymbolCode();
        pMLS.SetCode(code);
        name := Self.GetUndefinedSymbolName();
        pMLS.SetName(PChar(name));
        pMLS.AddSimpleSymbol(clBlue, 1);
        m_pSymbolLib.AddSymbol(pMLS);
        Self.RefreshSymbolList();
        LVSymbolList.Items.Item[LVSymbolList.Items.Count - 1].Selected := True;
        Self.UpdateLineSymbolEditor(pMLS);

        m_LibDirty := True;
        m_ChangeSymbolProgress := False;
    end
    else if (3 = NewSymbol.select) then begin
        m_ChangeSymbolProgress := True;
        CreateMultiFillSymbol(pMFS);
        code := Self.GetNextSymbolCode();
        pMFS.SetCode(code);
        name := Self.GetUndefinedSymbolName();
        pMFS.SetName(PChar(name));
        pMFS.AddSimpleSymbol(clOlive);
        m_pSymbolLib.AddSymbol(pMFS);
        Self.RefreshSymbolList();
        LVSymbolList.Items.Item[LVSymbolList.Items.Count - 1].Selected := True;
        Self.UpdateFillSymbolEditor(pMFS);

        m_LibDirty := True;
        m_ChangeSymbolProgress := False;
    end
    else begin
        ShowMessage('oops')
    end;
end;

procedure DoQuickSort_Int(var A: array of Longint);
  procedure QuickSort(var A: array of Longint; iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Mid, T: Longint;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo] < Mid do Inc(Lo);
      while A[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort(A, iLo, Hi);
    if Lo < iHi then QuickSort(A, Lo, iHi);
  end;

begin
  QuickSort(A, Low(A), High(A));
end;

procedure TFormMain.SBDeleteLayersClick(Sender: TObject);
var
    i, count, id: Longint;
    pPS: IPointSymbol;
    ids: array of Longint;
begin
    count := TVLayersSelected.Items.Count;
    if (0 >= count) then Exit;
    SetLength(ids, count);
    for i := 0 to count - 1 do begin
        GetSelectedPointLayerByIndex(i, pPS, id);
        ids[i] := id;
    end;

    DoQuickSort_Int(ids);

    for i := count - 1 downto 0 do begin
        m_pEditorPointSymbol.RemoveSymbol(ids[i]);
    end;
    SetLength(ids, 0);

    Self.PushEdit();

    Self.ClearSelectedPointLayers();
    Self.RefreshPointLayerList();
    Self.PointSymbolEditorDraw();
    Self.m_pEditorSpace.PostBuffer();
end;

procedure TFormMain.PanelDrawColorClick(Sender: TObject);
begin
    ColorDialog1.Color := PanelDrawColor.Color;
    if (ColorDialog1.Execute()) then begin
        PanelDrawColor.Color := ColorDialog1.Color;
    end;
end;

procedure TFormMain.SBUndoClick(Sender: TObject);
begin
    Self.UndoEdit();
end;

procedure TFormMain.SBRedoClick(Sender: TObject);
begin
    Self.RedoEdit();
end;

procedure TFormMain.TVSubPointListEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
var
    pPS: IPointSymbol;
begin
    pPS := GetPointLayerFromTree();
    if (not Assigned(pPS)) then Exit;
    pPS.SetName(PChar(S));
end;

procedure TFormMain.TVSubPointListEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
var
    root: TTreeNode;
begin
    AllowEdit := False;
    root := TVSubPointList.Items.GetFirstNode();
    Node := TVSubPointList.Selected;
    if (not Assigned(Node)) then Exit;
    if (Node = root) then Exit;
    AllowEdit := True;
end;

procedure TFormMain.SBVertexEditClick(Sender: TObject);
begin
    m_EditorTask := TEDITORTASK_VERTEXEDIT;
    m_pEditorSpace.SetCursor(MOUSECURSOR_ARROW1);
    m_pEditorSpace.SetTask(SPACETASK_IDLE);
    Self.PointSymbolEditorDraw();
    m_pEditorSpace.PostBuffer();
end;

procedure TFormMain.TVLayersSelectedDblClick(Sender: TObject);
var
    node: TTreeNode;
begin
    node := TVLayersSelected.Selected;
    if (not Assigned(node)) then begin
        ShowMessage('请从选择集中选取一个图元先。');
        Exit;
    end;
    node.Delete();
    Self.PointSymbolEditorDraw();
    m_pEditorSpace.PostBuffer();
end;

procedure TFormMain.NewSymbolLib(const firstsymboltype: TSymbolType);
var
    pSymbol: ISymbol;
    pMPS: IMultiPointSymbol;
    pMLS: IMultiLineSymbol;
    pMFS: IMultiFillSymbol;
    pTS: ISimpleTextSymbol;
    pPFS: IPointFillSymbol;
    code: Longint;
begin
    code := 0;
    if (firstsymboltype = SYMBOLTYPE_POINT) then begin
        CreateMultiPointSymbol(pMPS);
        pMPS.SetCode(code);
        pMPS.SetName('毛猫');
        pSymbol := ISymbol(GotoInterface(pMPS, 'ISymbol'));
    end
    else if (firstsymboltype = SYMBOLTYPE_LINE) then begin
        CreateMultiLineSymbol(pMLS);
        pMLS.SetCode(code);
        pMLS.SetName('毛猫');
        pMLS.AddSimpleSymbol(clBlue, 1);
        pSymbol := ISymbol(GotoInterface(pMLS, 'ISymbol'));
    end
    else if (firstsymboltype = SYMBOLTYPE_FILL) then begin
        CreateMultiFillSymbol(pMFS);
        pMFS.SetCode(code);
        pMFS.SetName('毛猫');
        CreatePointFillSymbol(pPFS, clTeal, 2, 2);
        pMFS.AddSymbol(pPFS);
        pSymbol := ISymbol(GotoInterface(pMFS, 'ISymbol'));
    end
    else begin
        CreateSimpleTextSymbol(5, 5, '天长地久', clTeal, pTS);
        pTS.SetCode(code);
        pTS.SetName('毛猫');
        pSymbol := ISymbol(GotoInterface(pTS, 'ISymbol'));
    end;

    Self.NewSymbolLib(pSymbol);
end;

procedure TFormMain.NewSymbolLib(const pDefaultSymbol: ISymbol);
var
    symboltype: TSymbolType;
    pObj: IObj;
    pPointSymbol: IPointSymbol;
    pLineSymbol: ILineSymbol;
    pFillSymbol: IFillSymbol;
    pMPS: IMultiPointSymbol;
    pMLS: IMultiLineSymbol;
    pMFS: IMultiFillSymbol;
    pTS: ISimpleTextSymbol;
    code: Longint;
    name: AnsiString;
begin
    if (not Assigned(pDefaultSymbol)) then begin
        Exit;
    end;

    m_pSymbolLibFileName := '';
    Self.Caption := '符号编辑器';
    CreateObj('CSymbolLib', pObj);
    m_pSymbolLib := ISymbolLib(GoToInterface(pObj, 'ISymbolLib'));

    symboltype := pDefaultSymbol.GetSymbolType();
    if (symboltype = SYMBOLTYPE_POINT) then begin
        m_ChangeSymbolProgress := True;
        pMPS := IMultiPointSymbol(GotoInterface(pDefaultSymbol, 'IMultiPointSymbol'));
        if (not Assigned(pMPS)) then begin
            code := pDefaultSymbol.GetCode();
            name := pDefaultSymbol.GetName();
            CreateMultiPointSymbol(pMPS);
            pMPS.SetCode(code);
            pMPS.SetName(PChar(name));
            pPointSymbol := IPointSymbol(GotoInterface(pDefaultSymbol, 'IPointSymbol'));
            pMPS.AddSymbol(pPointSymbol);
        end;

        m_pSymbolLib.AddSymbol(pMPS);
        Self.RefreshSymbolList();
        LVSymbolList.Items.Item[LVSymbolList.Items.Count - 1].Selected := True;
        Self.PointSymbolEditorDraw();
        m_ChangeSymbolProgress := False;
    end
    else if (symboltype = SYMBOLTYPE_LINE) then begin
        m_ChangeSymbolProgress := True;
        pMLS := IMultiLineSymbol(GotoInterface(pDefaultSymbol, 'IMultiLineSymbol'));
        if (not Assigned(pMLS)) then begin
            code := pDefaultSymbol.GetCode();
            name := pDefaultSymbol.GetName();
            CreateMultiLineSymbol(pMLS);
            pMLS.SetCode(code);
            pMLS.SetName(PChar(name));
            pLineSymbol := ILineSymbol(GotoInterface(pDefaultSymbol, 'ILineSymbol'));
            pMLS.AddSymbol(pLineSymbol);
        end;

        m_pSymbolLib.AddSymbol(pMLS);
        Self.RefreshSymbolList();
        LVSymbolList.Items.Item[LVSymbolList.Items.Count - 1].Selected := True;
        Self.UpdateLineSymbolEditor(pMLS);
        m_ChangeSymbolProgress := False;
    end
    else if (symboltype = SYMBOLTYPE_FILL) then begin
        m_ChangeSymbolProgress := True;
        pMFS := IMultiFillSymbol(GotoInterface(pDefaultSymbol, 'IMultiFillSymbol'));
        if (not Assigned(pMFS)) then begin
            code := pDefaultSymbol.GetCode();
            name := pDefaultSymbol.GetName();
            CreateMultiFillSymbol(pMFS);
            pMFS.SetCode(code);
            pMFS.SetName(PChar(name));
            pFillSymbol := IFillSymbol(GotoInterface(pDefaultSymbol, 'IFillSymbol'));
            pMFS.AddSymbol(pFillSymbol);
        end;

        m_pSymbolLib.AddSymbol(pMFS);
        Self.RefreshSymbolList();
        LVSymbolList.Items.Item[LVSymbolList.Items.Count - 1].Selected := True;
        Self.UpdateFillSymbolEditor(pMFS);
        m_ChangeSymbolProgress := False;
    end
    else begin
        m_ChangeSymbolProgress := True;
        pTS := ISimpleTextSymbol(GotoInterface(pDefaultSymbol, 'ISimpleTextSymbol'));
        if (Assigned(pTS)) then begin
            m_pSymbolLib.AddSymbol(pTS);
            Self.RefreshSymbolList();
            LVSymbolList.Items.Item[LVSymbolList.Items.Count - 1].Selected := True;

            Self.UpdateTextSymbolEditor(pTS);

            //refresh something

        end;
        m_ChangeSymbolProgress := False;
    end;

    m_LibDirty := False;
end;

procedure TFormMain.SBNewSymbolLibClick(Sender: TObject);
begin
    if (m_LibDirty) then begin
        if (mrYes <> MessageDlg('修改后的符号没存，确定要放弃修改吗？', mtConfirmation, [mbYes, mbNo], 0)) then begin
            Exit;
        end;
    end;

    Self.NewSymbolLib(SYMBOLTYPE_POINT);
end;

procedure TFormMain.RefreshSymbolList();
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

function TFormMain.GetNextSymbolCode(): Longint;
var
    i, count, code: Longint;
    pSymbol: ISymbol;
begin
    Result := 0;
    count := m_pSymbolLib.GetSymbolCount();
    for i := 0 to count - 1 do begin
        m_pSymbolLib.GetSymbolRef(pSymbol, i);
        code := pSymbol.GetCode();
        if (code >= Result) then begin
            Result := code + 1;
        end;
    end;
end;

function TFormMain.GetUndefinedSymbolName(): AnsiString;
var
    i, count, n: Longint;
    name: AnsiString;
    pSymbol: ISymbol;
    flag: Boolean;
begin
    Result := 'undefined';
    n := 1;
    while (True) do begin
        flag := True;
        count := m_pSymbolLib.GetSymbolCount();
        for i := 0 to count - 1 do begin
            m_pSymbolLib.GetSymbolRef(pSymbol, i);
            name := pSymbol.GetName();
            if (name = Result) then begin
                flag := False;
            end;
        end;
        if (flag) then begin
            Break;
        end;
        Result := 'undefined' + IntToStr(n);
        Inc(n);
    end;
end;

procedure TFormMain.LVSymbolListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
    Self.PanelSymbolPreview.Refresh();
    Self.UpdatePointSymbolEditor(nil);
    Self.UpdateLineSymbolEditor(nil);
    Self.UpdateFillSymbolEditor(nil);

    if (not Selected) then Exit;
    Self.RefreshEditSymbol();
end;

procedure TFormMain.TVSubPointListClick(Sender: TObject);
var
    pSymbol: ISymbol;
begin
    pSymbol := GetPointLayerFromTree();

    SubSymbolAttrib.SetSymbol(pSymbol);
end;

procedure TFormMain.TVLayersSelectedClick(Sender: TObject);
var
    pSymbol: IPointSymbol;
    parentindex, index: Longint;
begin
    if (not Assigned(TVLayersSelected.Selected)) then Exit;

    index := TVLayersSelected.Selected.Index;
    GetSelectedPointLayerByIndex(index, pSymbol, parentindex);

    SubSymbolAttrib.SetSymbol(pSymbol);
end;

procedure TFormMain.SBSaveSymbolLibClick(Sender: TObject);
//var
//    pStream: IStreamX;
//    pObj: IObj;
//    n: Longint;
begin
    if ('' = Trim(m_pSymbolLibFileName)) then begin
        SBSaveAsSymbolLibClick(Self);
        Exit;
    end;
(*
    CreateObj('CMemoryStream', pObj);
    pStream := IStreamX(GotoInterface(pObj, 'IStreamX'));
    n := m_pSymbolLib.Dump(pStream);
    if (0 >= n) then begin
        ShowMessage('err');
        Exit;
    end;
    if (not pStream.SaveToFile(PChar(m_pSymbolLibFileName))) then begin
        ShowMessage('error');
        Exit;
    end;
*)
    if (not m_pSymbolLib.SaveToFile(PChar(m_pSymbolLibFileName))) then begin
        ShowMessage('oh no');
        Exit;
    end;

    m_LibDirty := False;
end;

procedure TFormMain.SBSaveAsSymbolLibClick(Sender: TObject);
var
//    pStream: IStreamX;
//    pObj: IObj;
    filename: AnsiString;
//    n: Longint;
begin
    if (not SDSymbolLib.Execute()) then begin
        Exit;
    end;

    filename := SDSymbolLib.FileName;

(*
    CreateObj('CMemoryStream', pObj);
    pStream := IStreamX(GotoInterface(pObj, 'IStreamX'));
    n := m_pSymbolLib.Dump(pStream);
    if (0 >= n) then begin
        ShowMessage('err');
        Exit;
    end;
    if (not pStream.SaveToFile(PChar(filename))) then begin
        ShowMessage('error');
        Exit;
    end;
*)

    if (not m_pSymbolLib.SaveToFile(PChar(filename))) then begin
        ShowMessage('oh no');
        Exit;
    end;

    m_pSymbolLibFileName := filename;
    Self.Caption := '符号编辑器' + ' - ' + m_pSymbolLibFileName;
    m_LibDirty := False;
end;

procedure TFormMain.SBOpenSymbolLibClick(Sender: TObject);
var
    filename: AnsiString;
    pObj: IObj;
//    pStream: IStreamX;
//    pPersist: IPersist;
    pSymbolLib: ISymbolLib;
//    n: Longint;
begin
    if (m_LibDirty) then begin
        if (mrYes <> MessageDlg('修改后的符号没存，确定要放弃修改吗？', mtConfirmation, [mbYes, mbNo], 0)) then begin
            Exit;
        end;
    end;

    if (not ODSymbolLib.Execute()) then begin
        Exit;
    end;

    filename := ODSymbolLib.FileName;

(*
    CreateObj('CMemoryStream', pObj);
    pStream := IStreamX(GotoInterface(pObj, 'IStreamX'));
    if (not pStream.LoadFromFile(PChar(filename))) then begin
        ShowMessage('error');
        Exit;
    end;

    n := Instantiate(pStream, pPersist);
    if ((0 >= n) or not Assigned(pPersist)) then begin
        ShowMessage('err');
        Exit;
    end;
*)

    CreateObj('CSymbolLib', pObj);
    pSymbolLib := ISymbolLib(GotoInterface(pObj, 'ISymbolLib'));
    if (not pSymbolLib.LoadFromFile(PChar(filename))) then begin
        ShowMessage('on shit');
        Exit;
    end;

    m_pSymbolLib := pSymbolLib;
    Self.RefreshSymbolList();
    Self.UpdatePointSymbolEditor(nil);
    Self.UpdateLineSymbolEditor(nil);

    if (0 < LVSymbolList.Items.Count) then begin
        LVSymbolList.Items.Item[0].Selected := True;
    end;

    m_pSymbolLibFileName := filename;
    Self.Caption := '符号编辑器' + ' - ' + m_pSymbolLibFileName;
    m_LibDirty := False;
end;

procedure TFormMain.SBRotateClick(Sender: TObject);
begin
    m_EditorTask := TEDITORTASK_ROTATE;
    m_pEditorSpace.SetCursor(MOUSECURSOR_CROSSBOX);
    m_pEditorSpace.SetTask(SPACETASK_IDLE);
    Self.PointSymbolEditorDraw();
    m_pEditorSpace.PostBuffer();
end;

procedure TFormMain.SBSymbolUpClick(Sender: TObject);
var
    index: Longint;
    pSymbol: ISymbol;
begin
    if (not Assigned(LVSymbolList.Selected)) then begin
        ShowMessage('从符号列表中选一个符号先');
        Exit;
    end;

    index := LVSymbolList.Selected.Index;
    if (1 > index) then Exit;
    m_pSymbolLib.GetSymbolRef(pSymbol, index);
    Dec(index);
    m_pSymbolLib.SetSymbolOrder(pSymbol, index);
    Self.RefreshSymbolList();
    LVSymbolList.Items.Item[index].Selected := True;
    m_LibDirty := True;
end;

procedure TFormMain.SBSymbolDownClick(Sender: TObject);
var
    index, count: Longint;
    pSymbol: ISymbol;
begin
    if (not Assigned(LVSymbolList.Selected)) then begin
        ShowMessage('从符号列表中选一个符号先');
        Exit;
    end;

    index := LVSymbolList.Selected.Index;
    count := m_pSymbolLib.GetSymbolCount();
    if (count - 1 <= index) then Exit;
    m_pSymbolLib.GetSymbolRef(pSymbol, index);
    Inc(index);
    m_pSymbolLib.SetSymbolOrder(pSymbol, index);
    Self.RefreshSymbolList();
    LVSymbolList.Items.Item[index].Selected := True;
    m_LibDirty := True;
end;

procedure TFormMain.SBSelectTreeClick(Sender: TObject);
var
    layerindex: Longint;
begin
    layerindex := GetPointLayerIndexFromTree();
    Self.CheckSelectedPointLayer(layerindex);
end;

procedure TFormMain.TVSubPointListDblClick(Sender: TObject);
var
    pSymbol: ISymbol;
    colorlock: Boolean;
    absolutindex, i: Longint;
begin
    pSymbol := GetPointLayerFromTree();
    if (not Assigned(pSymbol)) then Exit;
    pSymbol.GetColorLock(colorlock);
    colorlock := not colorlock;
    pSymbol.SetColorLock(colorlock);
    Self.PushEdit();
    absolutindex := TVSubPointList.Selected.AbsoluteIndex;
    Self.RefreshPointLayerList();
    for i := 0 to TVSubPointList.Items[0].Count - 1 do begin
        if (TVSubPointList.Items[0].Item[i].AbsoluteIndex = absolutindex) then begin
            TVSubPointList.Items[0].Item[i].Selected := True;
            Break;
        end;
    end;
    SubSymbolAttrib.SetSymbol(pSymbol);
end;

procedure TFormMain.SBLayerOrderDownClick(Sender: TObject);
var
    pPointSymbol: IPointSymbol;
    layerindex: Longint;
    absolutindex: Longint;
    i: Longint;
    r: Boolean;
begin
    pPointSymbol := Self.GetPointLayerFromTree();
    if (not Assigned(pPointSymbol)) then Exit;
    Self.ClearSelectedPointLayers();

    layerindex := GetPointLayerIndexFromTree();
    if (0 < layerindex) then begin
        Dec(layerindex);
        r := m_pEditorPointSymbol.SetSymbolOrder(pPointSymbol, layerindex);
        if (r) then Self.PushEdit();
        absolutindex := TVSubPointList.Selected.AbsoluteIndex + 1;
        Self.RefreshPointLayerList();
        for i := 0 to TVSubPointList.Items[0].Count - 1 do begin
            if (TVSubPointList.Items[0].Item[i].AbsoluteIndex = absolutindex) then begin
                TVSubPointList.Items[0].Item[i].Selected := True;
                Break;
            end;
        end;
        Self.PointSymbolEditorDraw();
    end;

end;

procedure TFormMain.SBLayerOrderUpClick(Sender: TObject);
var
    pPointSymbol: IPointSymbol;
    layerindex, count: Longint;
    absolutindex: Longint;
    i: Longint;
    r: Boolean;
begin
    pPointSymbol := Self.GetPointLayerFromTree();
    if (not Assigned(pPointSymbol)) then Exit;
    Self.ClearSelectedPointLayers();

    layerindex := GetPointLayerIndexFromTree();
    count := m_pEditorPointSymbol.GetSymbolCount();
    if (count - 1 > layerindex) then begin
        Inc(layerindex);
        r := m_pEditorPointSymbol.SetSymbolOrder(pPointSymbol, layerindex);
        if (r) then Self.PushEdit();
        absolutindex := TVSubPointList.Selected.AbsoluteIndex - 1;
        Self.RefreshPointLayerList();
        for i := 0 to TVSubPointList.Items[0].Count - 1 do begin
            if (TVSubPointList.Items[0].Item[i].AbsoluteIndex = absolutindex) then begin
                TVSubPointList.Items[0].Item[i].Selected := True;
                Break;
            end;
        end;
        Self.PointSymbolEditorDraw();
    end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
    SubSymbolAttrib.Free();
    NewSymbol.Free();
    FormLineTemplate.Free();
    FormSymbolSelector.Free();
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    if (m_LibDirty and not m_SingleSymbolDlg) then begin
        if (mrYes <> MessageDlg('修改后的符号没存，确定要放弃修改吗？', mtConfirmation, [mbYes, mbNo], 0)) then begin
            CanClose := False;
            Exit;
        end;
    end;

    CanClose := True;
end;

procedure TFormMain.EditSymbolNameChange(Sender: TObject);
begin
    if (m_ChangeSymbolProgress) then Exit;
    if (not Assigned(LVSymbolList.Selected)) then Exit;

    if (Assigned(m_pEditorPointSymbol)) then begin
        m_pEditorPointSymbol.SetName(PChar(EditSymbolName.Text));
        TVSubPointList.Items[0].Text := EditSymbolName.Text;
    end
    else if (Assigned(m_pEditorLineSymbol)) then begin
        m_pEditorLineSymbol.SetName(PChar(EditSymbolName.Text));
    end
    else if (Assigned(m_pEditorFillSymbol)) then begin
        m_pEditorFillSymbol.SetName(PChar(EditSymbolName.Text));
    end;

    LVSymbolList.Selected.Caption := EditSymbolName.Text;
    m_LibDirty := True;
end;

procedure TFormMain.FormPaint(Sender: TObject);
begin
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.RefreshLineEditor();
var
    pLineSymbol: ILineSymbol;
    index: Longint;
begin
    //线符号编辑器需要刷新
    Self.GetSelectedSubLine(pLineSymbol, index);
    Self.RefreshLineEditor(pLineSymbol);
end;

procedure TFormMain.TemplateModified();
var
    pLineSymbol: ILineSymbol;
    pSimpleLineSymbol: ISimpleLineSymbol;
    pPointLineSymbol: IPointLineSymbol;
    pTemplate: ILineSimpleTemplate;
    i, index, size: Longint;
    factor: Double;
    pObj: IObj;
    flag: Boolean;
begin
    Self.GetSelectedSubLine(pLineSymbol, index);
    if (not Assigned(pLineSymbol)) then begin
        Exit;
    end;

    factor := 1;
    try
        factor := StrToFloat(FormLineTemplate.EditTemplFactor.Text);
    except
    end;

    pTemplate := nil;
    size := Length(FormLineTemplate.m_arrayPattStore);
    if (0 < size) then begin
        flag := False;
        for i := 0 to size-1 do begin
            if (0 < FormLineTemplate.m_arrayPattStore[i]) then begin
                flag := True;
                Break;
            end;
        end;

        if (flag) then begin
            CreateObj('CLineSimpleTemplate', pObj);
            pTemplate := ILineSimpleTemplate(GotoInterface(pObj, 'ILineSimpleTemplate'));

            for i := 0 to size-1 do begin
                pTemplate.AddSector(FormLineTemplate.m_arrayPattStore[i]);
            end;
        end;

        pTemplate.SetFirstMark(True);
        if (0 = FormLineTemplate.m_arrayPattStore[0]) then begin
            pTemplate.SetFirstMark(False);
        end;

        pTemplate.SetFactor(factor);
    end;

    pSimpleLineSymbol := ISimpleLineSymbol(GotoInterface(pLineSymbol, 'ISimpleLineSymbol'));
    pPointLineSymbol := IPointLineSymbol(GotoInterface(pLineSymbol, 'IPointLineSymbol'));
    if (Assigned(pSimpleLineSymbol)) then begin
        pSimpleLineSymbol.SetTemplate(pTemplate);
    end
    else if (Assigned(pPointLineSymbol)) then begin
        pPointLineSymbol.SetTemplate(pTemplate);
    end;

    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.TVSubLineClick(Sender: TObject);
begin
    Self.RefreshLineEditor();
end;

procedure TFormMain.EditSubLineNameChange(Sender: TObject);
var
    pLineSymbol: ILineSymbol;
    index: Longint;
    name: AnsiString;
begin
    if (FormLineTemplate.m_InProgress) then Exit;

    Self.GetSelectedSubLine(pLineSymbol, index);
    if (not Assigned(pLineSymbol)) then begin
        Exit;
    end;

    name := EditSubLineName.Text;
    pLineSymbol.SetName(PChar(name));
    Self.RefreshSubLineList();
    TVSubLine.Items[TVSubLine.Items.Count - 1 - index].Selected := True;
    Self.PushEdit();
end;

procedure TFormMain.EditSubLineWidthChange(Sender: TObject);
var
    pLineSymbol: ILineSymbol;
    pSimpleLineSymbol: ISimpleLineSymbol;
    index: Longint;
    width: Double;
begin
    if (FormLineTemplate.m_InProgress) then Exit;

    try
        width := StrToFloat(EditSubLineWidth.Text);
    except
        Exit;
    end;

    Self.GetSelectedSubLine(pLineSymbol, index);
    if (not Assigned(pLineSymbol)) then begin
        Exit;
    end;

    pSimpleLineSymbol := ISimpleLineSymbol(GotoInterface(pLineSymbol, 'ISimpleLineSymbol'));
    if (not Assigned(pSimpleLineSymbol)) then begin
        Exit;
    end;

    pSimpleLineSymbol.SetWidth(width);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.EditSubLineOffsetChange(Sender: TObject);
var
    pLineSymbol: ILineSymbol;
    index: Longint;
    offset: Double;
begin
    if (FormLineTemplate.m_InProgress) then Exit;

    try
        offset := StrToFloat(EditSubLineOffset.Text);
    except
        Exit;
    end;

    Self.GetSelectedSubLine(pLineSymbol, index);
    if (not Assigned(pLineSymbol)) then begin
        Exit;
    end;

    pLineSymbol.SetOffset(offset);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.PanelSubLineColorClick(Sender: TObject);
var
    pLineSymbol: ILineSymbol;
    index: Longint;
begin
    Self.GetSelectedSubLine(pLineSymbol, index);
    if (not Assigned(pLineSymbol)) then begin
        Exit;
    end;

    ColorDialog1.Color := PanelSubLineColor.Color;
    if (ColorDialog1.Execute()) then begin
        PanelSubLineColor.Color := ColorDialog1.Color;
    end;

    pLineSymbol.SetColor(PanelSubLineColor.Color);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.CBSubLineColorLockClick(Sender: TObject);
var
    pLineSymbol: ILineSymbol;
    index: Longint;
begin
    if (FormLineTemplate.m_InProgress) then Exit;

    Self.GetSelectedSubLine(pLineSymbol, index);
    if (not Assigned(pLineSymbol)) then begin
        Exit;
    end;

    pLineSymbol.SetColorLock(CBSubLineColorLock.Checked);
    Self.PushEdit();
    Self.RefreshSubLineList();
    TVSubLine.Items[TVSubLine.Items.Count - 1 - index].Selected := True;
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.ColorGridSubLineColorChange(Sender: TObject);
var
    pLineSymbol: ILineSymbol;
    index: Longint;
begin
    Self.GetSelectedSubLine(pLineSymbol, index);
    if (not Assigned(pLineSymbol)) then begin
        Exit;
    end;

    PanelSubLineColor.Color := ColorGridSubLineColor.ForegroundColor;
    pLineSymbol.SetColor(PanelSubLineColor.Color);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.SBSubLineUpClick(Sender: TObject);
var
    index: Longint;
    pLineSymbol: ILineSymbol;
begin
    if (not Assigned(m_pEditorLineSymbol)) then Exit;
    if (not Assigned(TVSubLine.Selected)) then Exit;

    Self.GetSelectedSubLine(pLineSymbol, index);
    if (TVSubLine.Items.Count-1 <= index) then Exit;

    m_pEditorLineSymbol.SetSymbolOrder(pLineSymbol, index+1);
    Self.RefreshSubLineList();
    TVSubLine.Items[TVSubLine.Items.Count - 2 - index].Selected := True;

    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.SBSubLineDownClick(Sender: TObject);
var
    index: Longint;
    pLineSymbol: ILineSymbol;
begin
    if (not Assigned(m_pEditorLineSymbol)) then Exit;
    if (not Assigned(TVSubLine.Selected)) then Exit;

    Self.GetSelectedSubLine(pLineSymbol, index);
    if (0 >= index) then Exit;

    m_pEditorLineSymbol.SetSymbolOrder(pLineSymbol, index-1);
    Self.RefreshSubLineList();
    TVSubLine.Items[TVSubLine.Items.Count - index].Selected := True;

    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.TVSubLineDblClick(Sender: TObject);
var
    pLineSymbol: ILineSymbol;
    index: Longint;
begin
    if (FormLineTemplate.m_InProgress) then Exit;

    Self.GetSelectedSubLine(pLineSymbol, index);
    if (not Assigned(pLineSymbol)) then begin
        Exit;
    end;

    CBSubLineColorLock.Checked := not CBSubLineColorLock.Checked;
    pLineSymbol.SetColorLock(CBSubLineColorLock.Checked);
    Self.PushEdit();
    Self.RefreshSubLineList();
    TVSubLine.Items[TVSubLine.Items.Count - 1 - index].Selected := True;
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.SBClearSelectClick(Sender: TObject);
begin
    Self.ClearSelectedPointLayers();
    Self.PointSymbolEditorDraw();
    m_pEditorSpace.PostBuffer();
end;

procedure TFormMain.SBModifySubPointClick(Sender: TObject);
begin
    if (not Assigned(m_pEditorPointSymbol)) then Exit;
    SubSymbolAttrib.Show();
end;

procedure TFormMain.SBDeleteSymbolClick(Sender: TObject);
var
    index, count: Longint;
begin
    if (not Assigned(LVSymbolList.Selected)) then begin
        ShowMessage('从符号列表中选一个符号先');
        Exit;
    end;

    index := LVSymbolList.Selected.Index;
    m_pSymbolLib.RemoveSymbol(index);
    count := m_pSymbolLib.GetSymbolCount();
    if (count <= index) then begin
        Dec(index);
    end;

    Self.RefreshSymbolList();
    if (0 <= index) then begin
        LVSymbolList.Items.Item[index].Selected := True;
    end
    else begin
        UpdatePointSymbolEditor(nil);
        UpdateLineSymbolEditor(nil);
    end;

    m_LibDirty := True;
end;

procedure TFormMain.LVSymbolListAdvancedCustomDrawItem(
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


procedure TFormMain.BitBtn4Click(Sender: TObject);
var
    pLineSymbol: ILineSymbol;
    index, treeindex: Longint;
begin
    if (not Assigned(m_pEditorLineSymbol)) then begin
        Exit;
    end;

    Self.GetSelectedSubLine(pLineSymbol, index);
    if (not Assigned(pLineSymbol)) then begin
        Exit;
    end;

    m_pEditorLineSymbol.RemoveSymbol(index);
    Self.RefreshSubLineList();

    treeindex := TVSubLine.Items.Count - index;
    if (TVSubLine.Items.Count > treeindex) then begin
        TVSubLine.Items[treeindex].Selected := True;
    end
    else if (TVSubLine.Items.Count > 0) then begin
        TVSubLine.Items[TVSubLine.Items.Count-1].Selected := True;
    end;

    Self.RefreshLineEditor();
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.BitBtn1Click(Sender: TObject);
begin
    if (not Assigned(m_pEditorLineSymbol)) then Exit;
    m_pEditorLineSymbol.AddSimpleSymbol(clBlue, 0.5);

    Self.RefreshSubLineList();
    if (TVSubLine.Items.Count > 0) then begin
        TVSubLine.Items[0].Selected := True;
    end;

    Self.RefreshLineEditor();
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.BitBtn2Click(Sender: TObject);
var
    pPointLineSymbol: IPointLineSymbol;
begin
    if (not Assigned(m_pEditorLineSymbol)) then Exit;
    CreatePointLineSymbol(pPointLineSymbol);
    m_pEditorLineSymbol.AddSymbol(pPointLineSymbol);

    Self.RefreshSubLineList();
    if (TVSubLine.Items.Count > 0) then begin
        TVSubLine.Items[0].Selected := True;
    end;

    Self.RefreshLineEditor();
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.BitBtn3Click(Sender: TObject);
begin
    ShowMessage('没做');
end;

procedure TFormMain.BTSelectPointSymbolClick(Sender: TObject);
var
    index: Longint;
    pLineSymbol: ILineSymbol;
    pPointSymbol: IPointSymbol;
    pSymbol: ISymbol;
    pPLS: IPointLineSymbol;
begin
    Self.GetSelectedSubLine(pLineSymbol, index);
    if (not Assigned(pLineSymbol)) then begin
        Exit;
    end;

    pPLS := IPointLineSymbol(GotoInterface(pLineSymbol, 'IPointLineSymbol'));
    if (not Assigned(pPLS)) then begin
        Exit;
    end;

    pPLS.GetPointSymbol(pPointSymbol);    
    FormSymbolSelector.SetSymbolType(SYMBOLTYPE_POINT);
    FormSymbolSelector.SetSymbol(pPointSymbol);
    FormSymbolSelector.ShowModal();
    FormSymbolSelector.GetSymbol(pSymbol);
    if (Assigned(pSymbol)) then begin
        pPointSymbol := IPointSymbol(GotoInterface(pSymbol, 'IPointSymbol'));
        pPLS.SetPointSymbol(pPointSymbol);
        Self.PushEdit();
        Self.SymbolPreviewDraw();
    end;
end;

procedure TFormMain.PanelSymbolPreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.BitBtn7Click(Sender: TObject);
begin
    ShowMessage('没做哦');
end;

procedure TFormMain.SpeedButton6Click(Sender: TObject);
begin
    Self.UndoEdit();
end;

procedure TFormMain.SpeedButton7Click(Sender: TObject);
begin
    Self.RedoEdit();
end;

procedure TFormMain.TVSubFillDblClick(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    index: Longint;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    CBSubFillColorLock.Checked := not CBSubFillColorLock.Checked;
    pFillSymbol.SetColorLock(CBSubFillColorLock.Checked);
    Self.PushEdit();
    Self.RefreshSubFillList();
    TVSubFill.Items[TVSubFill.Items.Count - 1 - index].Selected := True;
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.TVSubFillClick(Sender: TObject);
begin
    Self.RefreshFillEditor();
end;

procedure TFormMain.BitBtn5Click(Sender: TObject);
begin
    if (not Assigned(m_pEditorFillSymbol)) then Exit;
    m_pEditorFillSymbol.AddSimpleSymbol(clFuchsia);

    Self.RefreshSubFillList();
    if (TVSubFill.Items.Count > 0) then begin
        TVSubFill.Items[0].Selected := True;
    end;

    Self.RefreshFillEditor();
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.BitBtn6Click(Sender: TObject);
var
    pPointFillSymbol: IPointFillSymbol;
begin
    if (not Assigned(m_pEditorFillSymbol)) then Exit;
    CreatePointFillSymbol(pPointFillSymbol, clTeal, 2, 2);
    m_pEditorFillSymbol.AddSymbol(pPointFillSymbol);

    Self.RefreshSubFillList();
    if (TVSubFill.Items.Count > 0) then begin
        TVSubFill.Items[0].Selected := True;
    end;

    Self.RefreshFillEditor();
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.BitBtn8Click(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    index, treeindex: Longint;
begin
    if (not Assigned(m_pEditorFillSymbol)) then begin
        Exit;
    end;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    m_pEditorFillSymbol.RemoveSymbol(index);
    Self.RefreshSubFillList();

    treeindex := TVSubFill.Items.Count - index;
    if (TVSubFill.Items.Count > treeindex) then begin
        TVSubFill.Items[treeindex].Selected := True;
    end
    else if (TVSubFill.Items.Count > 0) then begin
        TVSubFill.Items[TVSubFill.Items.Count-1].Selected := True;
    end;

    Self.RefreshFillEditor();
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.BTSelectPointSymbol_PointFillClick(Sender: TObject);
var
    index: Longint;
    pFillSymbol: IFillSymbol;
    pPointSymbol: IPointSymbol;
    pSymbol: ISymbol;
    pPFS: IPointFillSymbol;
begin
    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pPFS := IPointFillSymbol(GotoInterface(pFillSymbol, 'IPointFillSymbol'));
    if (not Assigned(pPFS)) then begin
        Exit;
    end;

    pPFS.GetPointSymbol(pPointSymbol);
    FormSymbolSelector.SetSymbolType(SYMBOLTYPE_POINT);
    FormSymbolSelector.SetSymbol(pPointSymbol);
    FormSymbolSelector.ShowModal();
    FormSymbolSelector.GetSymbol(pSymbol);
    if (Assigned(pSymbol)) then begin
        pPointSymbol := IPointSymbol(GotoInterface(pSymbol, 'IPointSymbol'));
        pPFS.SetPointSymbol(pPointSymbol);
        Self.PushEdit();
        Self.SymbolPreviewDraw();
    end;
end;

procedure TFormMain.BTSelectSymbol_PointFillBorderClick(Sender: TObject);
var
    index: Longint;
    pFillSymbol: IFillSymbol;
    pLineSymbol: ILineSymbol;
    pSymbol: ISymbol;
    pPFS: IPointFillSymbol;
begin
    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pPFS := IPointFillSymbol(GotoInterface(pFillSymbol, 'IPointFillSymbol'));
    if (not Assigned(pPFS)) then begin
        Exit;
    end;

    pPFS.GetBorderSymbol(pLineSymbol);
    FormSymbolSelector.SetSymbolType(SYMBOLTYPE_LINE);
    FormSymbolSelector.SetSymbol(pLineSymbol);
    FormSymbolSelector.ShowModal();
    FormSymbolSelector.GetSymbol(pSymbol);
    if (not Assigned(pSymbol)) then begin
        Exit;
    end;

    pLineSymbol := ILineSymbol(GotoInterface(pSymbol, 'ILineSymbol'));
    pPFS.SetBorderSymbol(pLineSymbol);
    Self.PushEdit();
    Self.SymbolPreviewDraw();

    CBPointFillBorderAvailable.Checked := True;
end;

procedure TFormMain.CBPointFillBorderAvailableClick(Sender: TObject);
var
    index: Longint;
    pFillSymbol: IFillSymbol;
    pBorderSymbol: ILineSymbol;
    pPFS: IPointFillSymbol;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pPFS := IPointFillSymbol(GotoInterface(pFillSymbol, 'IPointFillSymbol'));
    if (not Assigned(pPFS)) then begin
        Exit;
    end;

    pPFS.GetBorderSymbol(pBorderSymbol);
    if (Assigned(pBorderSymbol)) then begin
        m_pBorderSymbolSaved := pBorderSymbol;
    end;

    if (CBPointFillBorderAvailable.Checked) then begin
        pPFS.SetBorderSymbol(m_pBorderSymbolSaved);
    end
    else begin
        pPFS.SetBorderSymbol(nil);
    end;

    Self.PushEdit();
    Self.SymbolPreviewDraw();

end;

procedure TFormMain.ColorGridSubFillColorChange(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    index: Longint;
begin
    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    PanelSubFillColor.Color := ColorGridSubFillColor.ForegroundColor;
    pFillSymbol.SetColor(PanelSubFillColor.Color);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.PanelSubFillColorClick(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    index: Longint;
begin
    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    ColorDialog1.Color := PanelSubFillColor.Color;
    if (ColorDialog1.Execute()) then begin
        PanelSubFillColor.Color := ColorDialog1.Color;
    end
    else begin
        Exit;
    end;

    pFillSymbol.SetColor(PanelSubFillColor.Color);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.ColorGridSubBorderColorChange(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    pSimpleFillSymbol: ISimpleFillSymbol;
    index: Longint;
begin
    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pSimpleFillSymbol := ISimpleFillSymbol(GotoInterface(pFillSymbol, 'ISimpleFillSymbol'));

    PanelSubBorderColor.Color := ColorGridSubBorderColor.ForegroundColor;
    pSimpleFillSymbol.SetBorderColor(PanelSubBorderColor.Color);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.CBFillStyleChange(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    pSimpleFillSymbol: ISimpleFillSymbol;
    index: Longint;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pSimpleFillSymbol := ISimpleFillSymbol(GotoInterface(pFillSymbol, 'ISimpleFillSymbol'));
    pSimpleFillSymbol.SetFillStyle(CBFillStyle.ItemIndex);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.CBFillHatchChange(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    pSimpleFillSymbol: ISimpleFillSymbol;
    index: Longint;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pSimpleFillSymbol := ISimpleFillSymbol(GotoInterface(pFillSymbol, 'ISimpleFillSymbol'));
    pSimpleFillSymbol.SetFillHatch(CBFillHatch.ItemIndex);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.EditBorderWidthChange(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    pSimpleFillSymbol: ISimpleFillSymbol;
    index: Longint;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pSimpleFillSymbol := ISimpleFillSymbol(GotoInterface(pFillSymbol, 'ISimpleFillSymbol'));
    pSimpleFillSymbol.SetBorderWidth(StrToFloat(EditBorderWidth.Text));
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.PanelSubBorderColorClick(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    pSimpleFillSymbol: ISimpleFillSymbol;
    index: Longint;
begin
    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    ColorDialog1.Color := PanelSubBorderColor.Color;
    if (ColorDialog1.Execute()) then begin
        PanelSubBorderColor.Color := ColorDialog1.Color;
    end
    else begin
        Exit;
    end;

    pSimpleFillSymbol := ISimpleFillSymbol(GotoInterface(pFillSymbol, 'ISimpleFillSymbol'));
    pSimpleFillSymbol.SetBorderColor(PanelSubBorderColor.Color);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.EditFillPointSpaceXChange(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    pPointFillSymbol: IPointFillSymbol;
    index: Longint;
    space_x, space_y: Double;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pPointFillSymbol := IPointFillSymbol(GotoInterface(pFillSymbol, 'IPointFillSymbol'));
    pPointFillSymbol.GetPointsSpace(space_x, space_y);
    space_x := StrToFloat(EditFillPointSpaceX.Text);
    pPointFillSymbol.SetPointsSpace(space_x, space_y);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.EditFillPointSpaceYChange(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    pPointFillSymbol: IPointFillSymbol;
    index: Longint;
    space_x, space_y: Double;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pPointFillSymbol := IPointFillSymbol(GotoInterface(pFillSymbol, 'IPointFillSymbol'));
    pPointFillSymbol.GetPointsSpace(space_x, space_y);
    space_y := StrToFloat(EditFillPointSpaceY.Text);
    pPointFillSymbol.SetPointsSpace(space_x, space_y);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.EditFillPointOffsetXChange(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    pPointFillSymbol: IPointFillSymbol;
    index: Longint;
    offset_x, offset_y: Double;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pPointFillSymbol := IPointFillSymbol(GotoInterface(pFillSymbol, 'IPointFillSymbol'));
    pPointFillSymbol.GetPointsOffset(offset_x, offset_y);
    offset_x := StrToFloat(EditFillPointOffsetX.Text);
    pPointFillSymbol.SetPointsOffset(offset_x, offset_y);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.EditFillPointOffsetYChange(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    pPointFillSymbol: IPointFillSymbol;
    index: Longint;
    offset_x, offset_y: Double;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pPointFillSymbol := IPointFillSymbol(GotoInterface(pFillSymbol, 'IPointFillSymbol'));
    pPointFillSymbol.GetPointsOffset(offset_x, offset_y);
    offset_y := StrToFloat(EditFillPointOffsetY.Text);
    pPointFillSymbol.SetPointsOffset(offset_x, offset_y);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.CBPointFillTypeChange(Sender: TObject);
var
    pFillSymbol: IFillSymbol;
    pPointFillSymbol: IPointFillSymbol;
    index: Longint;
    fillstyle: TPointFillStyle;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    fillstyle := POINTFILLSTYLE_REGULAR;
    if (CBPointFillType.ItemIndex = 1) then begin
        fillstyle := POINTFILLSTYLE_LABELPOINT;
    end;

    pPointFillSymbol := IPointFillSymbol(GotoInterface(pFillSymbol, 'IPointFillSymbol'));

    pPointFillSymbol.SetFillStyle(fillstyle);
    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.CBSubFillColorLockClick(Sender: TObject);
var
    index: Longint;
    pFillSymbol: IFillSymbol;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pFillSymbol.SetColorLock(CBSubFillColorLock.Checked);

    Self.PushEdit();
    Self.RefreshSubFillList();
    TVSubFill.Items[TVSubFill.Items.Count - 1 - index].Selected := True;
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.EditSubFillNameChange(Sender: TObject);
var
    index: Longint;
    pFillSymbol: IFillSymbol;
begin
    if (m_ChangeSymbolProgress) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (not Assigned(pFillSymbol)) then begin
        Exit;
    end;

    pFillSymbol.SetName(PChar(EditSubFillName.Text));

    Self.PushEdit();
    Self.RefreshSubFillList();
    TVSubFill.Items[TVSubFill.Items.Count - 1 - index].Selected := True;
end;

procedure TFormMain.SpeedButton1Click(Sender: TObject);
var
    index: Longint;
    pFillSymbol: IFillSymbol;
begin
    if (not Assigned(m_pEditorFillSymbol)) then Exit;
    if (not Assigned(TVSubFill.Selected)) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (TVSubFill.Items.Count-1 <= index) then Exit;

    m_pEditorFillSymbol.SetSymbolOrder(pFillSymbol, index+1);
    Self.RefreshSubFillList();
    TVSubFill.Items[TVSubFill.Items.Count - 2 - index].Selected := True;

    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.SpeedButton2Click(Sender: TObject);
var
    index: Longint;
    pFillSymbol: IFillSymbol;
begin
    if (not Assigned(m_pEditorFillSymbol)) then Exit;
    if (not Assigned(TVSubFill.Selected)) then Exit;

    Self.GetSelectedSubFill(pFillSymbol, index);
    if (0 >= index) then Exit;

    m_pEditorFillSymbol.SetSymbolOrder(pFillSymbol, index-1);
    Self.RefreshSubFillList();
    TVSubFill.Items[TVSubFill.Items.Count - index].Selected := True;

    Self.PushEdit();
    Self.SymbolPreviewDraw();
end;

procedure TFormMain.btnSingleSymbolOKClick(Sender: TObject);
begin
    m_pSymbolLib.GetSymbolRef(m_pSingleSymbol, 0);
    Self.Close();
end;

procedure TFormMain.btnSingleSymbolCancelClick(Sender: TObject);
begin
    m_pSingleSymbol := nil;
    Self.Close();
end;

end.
