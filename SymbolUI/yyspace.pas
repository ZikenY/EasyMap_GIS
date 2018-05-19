unit yyspace;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics;


//以下两行定义绘制的polyline、polygon的最大点数，绘制的时候最多只能有这么多的点
const MAXPOINTSINPOLYLINE = 30000;
const MAXPOINTSINPOLYGON = MAXPOINTSINPOLYLINE;

//用于将绘制的<折线/多边形>返回给调用者
type
    TyyShapeTracked = record
        m_Points: array[0..MAXPOINTSINPOLYLINE] of tagPOINT;
        m_PointCount: Integer;
    end;

//用于将绘制的<矩形>返回给调用者
type
    TyyRectTracked = record
        m_Point1: tagPOINT;
        m_Point2: tagPOINT;
    end;

//用于将绘制的<圆>返回给调用者
type
    TyyCircleTracked = record
        m_Center: tagPOINT;
        m_Radius: Integer;
    end;



//------------------------------------------------------------------------------
//事件响应函数形式声明（定义外界传入的回调函数的声明形式）
//------------------------------------------------------------------------------
type
    //
    TEVENTPaintHandle = procedure(dc: HDC) of Object;

    //当窗体大小发生改变时，传出窗体新的高度和宽度
    TEVENTSizeHandle = procedure(width, height: Integer) of Object;

    //当窗体的屏幕位置发生改变时
    TEVENTMoveHandle = procedure() of Object;

    //当鼠标按键按下时
    TEVENTMouseDownHandle = procedure(Button: TMouseButton;
                                      Shift: TShiftState;
                                      X: Integer;
                                      Y: Integer
                                      ) of Object;

    //当鼠标按键松开时
    TEVENTMouseUpHandle = procedure(Button: TMouseButton;
                                    Shift: TShiftState;
                                    X: Integer;
                                    Y: Integer
                                    ) of Object;

    //当鼠标光标移动时
    TEVENTMouseMoveHandle = procedure(Shift: TShiftState;
                                      X: Integer;
                                      Y: Integer
                                      ) of Object;

    //当鼠标按键双击时
    TEVENTMouseDoubleClickHandle = procedure(Button: TMouseButton;
                                             Shift: TShiftState;
                                             X: Integer;
                                             Y: Integer
                                             ) of Object;

    //当键盘按键按下时
    TEVENTKeyDownHandle = procedure(Key: Word;
                                    Shift: TShiftState
                                    ) of Object;

    //当键盘按键松开时
    TEVENTKeyUpHandle = procedure(Key: Word;
                                  Shift: TShiftState
                                  ) of Object;

    //-------------------------------------------------------------
    //正在绘制<折线>（每绘制一个点就引发一次调用）
    //
    //参数：
    //      polyline            <输入>      用来返回当前所绘制的<折线>
    //      ContinueTracking    <输出>      是否继续绘制（传入False就代表绘制结束）
    //
    //-------------------------------------------------------------
    TEVENTPolylineTrackingHandle = procedure(var Polyline: TyyShapeTracked;
                                             var ContinueTracking: Boolean
                                             ) of Object;

    //-------------------------------------------------------------
    //<折线>绘制完成
    //
    //参数：
    //      polyline            <输入>      用来返回所绘制的<折线>
    //
    //-------------------------------------------------------------
    TEVENTPolylineTrackedHandle = procedure(var Polyline: TyyShapeTracked) of Object;

    //-------------------------------------------------------------
    //正在绘制<多边形>（每绘制一个点就引发一次调用）
    //
    //参数：
    //      polygon             <输入>      用来返回当前所绘制的<多边形>
    //      ContinueTracking    <输出>      是否继续绘制（传入False就代表绘制结束）
    //
    //-------------------------------------------------------------
    TEVENTPolygonTrackingHandle = procedure(var Polygon: TyyShapeTracked;
                                            var ContinueTracking: Boolean
                                            ) of Object;

    //-------------------------------------------------------------
    //<多边形>绘制完成
    //
    //参数：
    //      Polygon             <输入>      用来返回所绘制的<多边形>
    //
    //-------------------------------------------------------------
    TEVENTPolygonTrackedHandle = procedure(var Polygon: TyyShapeTracked) of Object;

    //-------------------------------------------------------------
    //<矩形>绘制完成
    //
    //参数：
    //      Rectangle           <输入>      用来返回所绘制的<矩形>
    //
    //-------------------------------------------------------------
    TEVENTRectTrackedHandle = procedure(var Rectangle: TyyRectTracked) of Object;

    //-------------------------------------------------------------
    //<圆>绘制完成
    //
    //参数：
    //      Circle              <输入>      用来返回所绘制的<圆>
    //
    //-------------------------------------------------------------
    TEVENTCircleTrackedHandle = procedure(var Circle: TyyCircleTracked) of Object;

    //当屏幕漫游操作结束时，传出屏幕坐标的偏移
    TEVENTPanHandle = procedure(X, Y: Integer) of Object;
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
//定义鼠标指针常量，每一个常量对应yyres.RES中的一个光标资源
//------------------------------------------------------------------------------
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
const MAXSPACEDEFINEDCURSORTYPE = MOUSECURSOR_PALETTE; //本unit定义的鼠标光标个数
//------------------------------------------------------------------------------
type TSpaceCursorType = Integer;    //Space的鼠标形状，其值是以上各常量中的一个


//------------------------------------------------------------------------------
//  以下常量定义了yySpace的各种操作（绘制多边形、绘制圆形等等）
//------------------------------------------------------------------------------
const SPACETASK_IDLE                    = 0;                        //do nothing
const SPACETASK_TRACKLINE               = SPACETASK_IDLE + 1;       //downup－move－downup
const SPACETASK_TRACKLINE2              = SPACETASK_TRACKLINE + 1;  //down－drag－up
const SPACETASK_TRACKPOLYGON            = SPACETASK_TRACKLINE2 + 1;
const SPACETASK_TRACKRECTANGLE          = SPACETASK_TRACKPOLYGON + 1;   //downup－move－downup
const SPACETASK_TRACKRECTANGLE2         = SPACETASK_TRACKRECTANGLE + 1; //down－drag－up
const SPACETASK_TRACKCIRCLE             = SPACETASK_TRACKRECTANGLE2 + 1;    //downup－move－downup
const SPACETASK_TRACKCIRCLE2            = SPACETASK_TRACKCIRCLE + 1;//down－drag－up
const SPACETASK_PAN                     = SPACETASK_TRACKCIRCLE2 + 1;
//------------------------------------------------------------------------------
type TSpaceTask = Integer;      //sPACE的当前操作，其值是以上各常量中的一个


//------------------------------------------------------------------------------
//Declaration yySpace
//以下定义了yySpace的结构
//------------------------------------------------------------------------------
type
TyySpace = class(TWinControl)
  private
//------------------------------------------------------------------------------
    //message handles
    //事件响应函数
//------------------------------------------------------------------------------
    procedure WMPaint(var Message: TWMPaint);               message WM_PAINT;
    procedure WMSize(var Message: TWMSize);                 message WM_SIZE;
    procedure WMMove(var Message: TWMMove);                 message WM_MOVE;
    procedure WMMouseMove(var Message: TWMMouseMove);       message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown);   message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown);   message WM_RBUTTONDOWN;
    procedure WMMButtonDown(var Message: TWMMButtonDown);   message WM_MBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp);       message WM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TWMRButtonUp);       message WM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TWMMButtonUp);       message WM_MBUTTONUP;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);   message WM_LBUTTONDBLCLK;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk);   message WM_RBUTTONDBLCLK;
    procedure WMMButtonDblClk(var Message: TWMMButtonDblClk);   message WM_MBUTTONDBLCLK;
    procedure WMKeyDown(var Message: TWMKeyDown);           message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp);               message WM_KEYUP;
//------------------------------------------------------------------------------

    //用于处理鼠标按下事件
    //由WMLButtonDown、WMRButtonDown、WMMButtonDown三个事件响应函数调用
    procedure DoMouseDown(Button: TMouseButton;
                          Shift: TShiftState;
                          X: Integer;
                          Y: Integer
                          );

    //用于处理鼠标松开事件
    //由WMLButtonUp、WMRButtonUp、WMMButtonUp三个事件响应函数调用
    procedure DoMouseUp(Button: TMouseButton;
                        Shift: TShiftState;
                        X: Integer;
                        Y: Integer
                        );

    //用于处理鼠标双击事件
    //由WMLButtonDblClk、WMRButtonDblClk、WMMButtonDblClk三个事件响应函数调用
    procedure DoMouseDoubleClick(Button: TMouseButton;
                                 Shift: TShiftState;
                                 X: Integer;
                                 Y: Integer
                                 );

    //由WMPaint事件响应函数调用
    //在窗体重绘时把画了一半的东西（比如正在绘制折线）重画一遍
    procedure OnSelfPaint();

    //在绘制<折线>时，由DoMouseDown函数调用
    //每绘制一个点调用一次
    //TEVENTPolylineTrackingHandle事件也由本函数引发
    procedure TrackLineProcess();

    //在<折线>绘制完成后，用于结束绘制操作
    procedure TrackLineFinish();

    //在绘制<多边形>时，由DoMouseDown函数调用
    //每绘制一个点调用一次
    //TEVENTPolygonTrackingHandle事件也由本函数引发
    procedure TrackPolygonProcess();

    //在<多边形>绘制完成后，用于结束绘制操作
    procedure TrackPolygonFinish();

    //在<矩形>绘制完成后，用于结束绘制操作
    procedure TrackRectangleFinish();

    //在<圆>绘制完成后，用于结束绘制操作
    procedure TrackCircleFinish();

  protected
//------------------------------------------------------------------------------
    //用来接受外界传入的一组回调函数（用来代替事件机制）
//------------------------------------------------------------------------------
    m_EventHandle_Paint             : TEVENTPaintHandle;            //
    m_EventHandle_Size              : TEVENTSizeHandle;             //窗体大小改变
    m_EventHandle_Move              : TEVENTMoveHandle;             //窗体移动
    m_EventHandle_MouseDown         : TEVENTMouseDownHandle;        //按下鼠标键
    m_EventHandle_MouseUp           : TEVENTMouseUpHandle;          //松开鼠标键
    m_EventHandle_MouseMove         : TEVENTMouseMoveHandle;        //鼠标移动
    m_EventHandle_MouseDoubleClick  : TEVENTMouseDoubleClickHandle; //鼠标双击
    //注意当窗体的tabstop=false时不会引发键盘事件
    m_EventHandle_KeyDown           : TEVENTKeyDownHandle;          //按下键
    m_EventHandle_KeyUp             : TEVENTKeyUpHandle;            //松开键
    m_EventHandle_PolylineTracking  : TEVENTPolylineTrackingHandle; //polyline画了一段
    m_EventHandle_PolylineTracked   : TEVENTPolylineTrackedHandle;  //polyline搞定了
    m_EventHandle_PolygonTracking   : TEVENTPolygonTrackingHandle;  //polygon画了一段
    m_EventHandle_PolygonTracked    : TEVENTPolygonTrackedHandle;   //polygon搞定了
    m_EventHandle_RectTracked       : TEVENTRectTrackedHandle;      //懒得写了
    m_EventHandle_CircleTracked     : TEVENTCircleTrackedHandle;    // :(
    m_EventHandle_Pan               : TEVENTPanHandle;
//------------------------------------------------------------------------------

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

//------------------------------------------------------------------------------
    //由外界调用，用来注册事件回调函数的地址
    //对于yySpace来说，是给m_EventHandle_*成员赋值
//------------------------------------------------------------------------------
    function InitialPaintEventHandle(EventHandle: TEVENTPaintHandle): Boolean;
    function InitialSizeEventHandle(EventHandle: TEVENTSizeHandle): Boolean;
    function InitialMoveEventHandle(EventHandle: TEVENTMoveHandle): Boolean;
    function InitialMouseDownEventHandle(EventHandle: TEVENTMouseDownHandle): Boolean;
    function InitialMouseUpEventHandle(EventHandle: TEVENTMouseUpHandle): Boolean;
    function InitialMouseMoveEventHandle(EventHandle: TEVENTMouseMoveHandle): Boolean;
    function InitialMouseDoubleClickEventHandle(EventHandle: TEVENTMouseDoubleClickHandle): Boolean;
    function InitialKeyDownEventHandle(EventHandle: TEVENTKeyDownHandle): Boolean;
    function InitialKeyUpEventHandle(EventHandle: TEVENTKeyUpHandle): Boolean;
    function InitialPolylineEventHandle(DoingEventHandle: TEVENTPolylineTrackingHandle;
                                        DoneEventHandle: TEVENTPolylineTrackedHandle
                                        ): Boolean;
    function InitialPolygonEventHandle(DoingEventHandle: TEVENTPolygonTrackingHandle;
                                       DoneEventHandle: TEVENTPolygonTrackedHandle
                                       ): Boolean;
    function InitialRectEventHandle(DoneHandle: TEVENTRectTrackedHandle): Boolean;
    function InitialCircleEventHandle(DoneHandle: TEVENTCircleTrackedHandle): Boolean;
    function InitialPanEventHandle(PanHandle: TEVENTPanHandle): Boolean;
//------------------------------------------------------------------------------

    //yySpace的背景颜色
    procedure SetBackColor(color: TColor);
    function GetBackColor(): TColor;

    //yySpace的光标形状
    function SetCursor(MouseCursor: TSpaceCursorType): Boolean;
    function GetCursor(): TSpaceCursorType;

    //取得内存dc
    function GetBufferDC(): HDC;

    //重置buffer大小
    procedure ResetBuffer();

    //将屏后buff的内容贴到前台
    procedure PostBuffer();

    //用于取消yySpace的当前操作（让yySpace处于idle状态）
    procedure SetIdleTask();//do nothing
    //设置和获取yySpace的各种状态
    function SetTask(SpaceTask: TSpaceTask): Boolean;
    function GetTask(): TSpaceTask;

    procedure DrawFocusEnvelope(const envelope: TRect);
    procedure DrawSelectBox(const rect: TRect; const color: COLORREF);
    procedure DrawEnvelope(envelope: TRect; linecolor: COLORREF; linewidth: Longint;
        hollow: Boolean; fillcolor: COLORREF); overload;
    procedure DrawEnvelope(envelope: TRect; color: COLORREF; linewidth: Longint;
        hollow: Boolean); overload;
    procedure DrawCircle(circle: TyyCircleTracked; linecolor: COLORREF;
        linewidth: Longint; hollow: Boolean; fillcolor: COLORREF); overload;
    procedure DrawCircle(center: tagPOINT; radius: Longint; linecolor: COLORREF;
        linewidth: Longint; hollow: Boolean; fillcolor: COLORREF); overload;
    procedure DrawCircle(center: tagPOINT; radius: Longint; color: COLORREF;
        linewidth: Longint; hollow: Boolean); overload;
    procedure DrawShape(var shape: TyyShapeTracked; linecolor: COLORREF;
        linewidth: Longint; hollow: Boolean; fillcolor: COLORREF);
    procedure DrawPolyline(var shape: TyyShapeTracked; linecolor: COLORREF;
        linewidth: Longint);
    procedure DrawFillPolygon(var shape: TyyShapeTracked; color: COLORREF;
        linewidth: Longint);
    
  private
    m_nSpaceID      : Integer;

    m_DC            : HDC;              //自己的dc
    m_MemDC         : HDC;              //后台内存dc
    m_BlankDC       : HDC;              //呜呜
    m_PenColor      : TColor;           //绘制时的颜色
    m_PenStyle      : Integer;          //绘制时的线型
    m_PenWidth      : Integer;          //绘制时的线宽
    m_BrushColor    : TColor;
    m_BrushStyle    : Integer;

    m_MouseCursor   : TSpaceCursorType; //鼠标指针形状
    m_CurrentTask   : TSpaceTask;       //记录yySpace的当前操作状态

    m_TrackPoints   : array of tagPOINT;//在绘制（折线、多边形等）时，记录已经绘制的每一个点

    //这个私有变量用来标识窗体刚刚重绘过
    //主要在绘制过程中避免留下难看的痕迹
    m_bJustOnPaint  : Boolean;

  end;
//------------------------------------------------------------------------------

function CreateSpace(Parent: HWND; out yySpace: TyySpace): Boolean;

implementation

{$R yySpace.res}

//判断指针是否有效（nil返回False）
function IsValid(p: Pointer): Boolean;
begin
    if (p = nil) then Result := False
    else Result := True;
end;

//判断指针是否无效（nil返回True）
function IsInvalid(p: Pointer): Boolean;
begin
    if (p = nil) then Result := True
    else Result := False;
end;

//绘制矩形
function _DrawEnvelope(dc: HDC; point1: tagPOINT; point2: tagPOINT;
    rop2: Integer): Boolean; overload;
var
    rect1: TRect;
    nOldRop2: Integer;
begin
    if (point1.x < point2.x) then begin
        rect1.Left := point1.x;
        rect1.Right := point2.x;
    end
    else begin
        rect1.Left := point2.x;
        rect1.Right := point1.x;
    end;
    if (point1.y < point2.y) then begin
        rect1.Top := Point1.y;
        rect1.Bottom := Point2.y;
    end
    else begin
        rect1.Top := Point2.y;
        rect1.Bottom := Point1.y;
    end;

    nOldRop2 := GetRop2(dc);
    Rectangle(dc, rect1.Left, rect1.Top, rect1.Right, rect1.Bottom);
    SetRop2(dc, nOldRop2);

    Result := False;
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

function _DrawEnvelope(dc: HDC; envelope: TRect; color: COLORREF; linewidth: Longint;
    hollow: Boolean): Boolean; overload;
begin
    Result := _DrawEnvelope(dc, envelope, color, linewidth, hollow, color);
end;

procedure _DrawSelectBox(dc: HDC; envelope: TRect; color: COLORREF);
var
    tagBrush: tagLOGBRUSH;
    BrushNew, BrushOld: HBRUSH;
    PenNew, PenOld: HPEN;
begin

    PenNew := CreatePen(PS_DASH, 1, color);
    PenOld := SelectObject(dc, PenNew);
    tagBrush.lbStyle := BS_HOLLOW;
    BrushNew := CreateBrushIndirect(tagBrush);
    BrushOld := SelectObject(dc, BrushNew);

    Rectangle(dc, envelope.Left, envelope.Top, envelope.Right, envelope.Bottom);

    SelectObject(dc, BrushOld);
    SelectObject(dc, PenOld);
    DeleteObject(BrushNew);
    DeleteObject(PenNew);
end;

//绘制焦点矩形
function _DrawFocusEnvelope(dc: HDC; point1: tagPOINT; point2: tagPOINT; rop2: Integer): Boolean;
var
    rect1: TRect;
    nOldRop2: Integer;
begin
    if (point1.x < point2.x) then begin
        rect1.Left := point1.x;
        rect1.Right := point2.x;
    end
    else begin
        rect1.Left := point2.x;
        rect1.Right := point1.x;
    end;
    if (point1.y < point2.y) then begin
        rect1.Top := Point1.y;
        rect1.Bottom := Point2.y;
    end
    else begin
        rect1.Top := Point2.y;
        rect1.Bottom := Point1.y;
    end;

    nOldRop2 := GetRop2(dc);
    DrawFocusRect(dc, rect1);
    SetRop2(dc, nOldRop2);

    Result := False;
end;

//画圆
function _DrawCircle(dc: HDC; center: tagPOINT; Point: tagPOINT): Boolean; overload;
var
    tagBrush: tagLOGBRUSH;
    BrushNew, BrushOld: HBRUSH;
    r: Integer; //半径
begin

    tagBrush.lbStyle := BS_HOLLOW;
    BrushNew := CreateBrushIndirect(tagBrush);
    BrushOld := SelectObject(dc, BrushNew);

    r := Round(Abs(Sqrt(Sqr(center.x - Point.x) + Sqr(center.y - Point.y))));
    Ellipse(dc,
            center.x - r,
            center.y - r,
            center.x + r,
            center.y + r);

    //恢复以前的hdc
    SelectObject(dc, BrushOld);
    DeleteObject(BrushNew);

    Result := True;
end;

//画圆
function _DrawCircle(dc: HDC; center: tagPOINT; Point: tagPOINT; linecolor: COLORREF;
    linewidth: Longint; hollow: Boolean; fillcolor: COLORREF): Boolean; overload;
var
    tagBrush: tagLOGBRUSH;
    BrushNew, BrushOld: HBRUSH;
    PenNew, PenOld: HPEN;
    r: Integer; //半径
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

    r := Round(Abs(Sqrt(Sqr(center.x - Point.x) + Sqr(center.y - Point.y))));
    Ellipse(dc,
            center.x - r,
            center.y - r,
            center.x + r,
            center.y + r);

    //恢复以前的hdc
    SelectObject(dc, BrushOld);
    SelectObject(dc, PenOld);
    DeleteObject(BrushNew);
    DeleteObject(PenNew);

    Result := True;
end;

//画圆
function _DrawCircle(dc: HDC; center: tagPOINT; radius: Longint; linecolor: COLORREF;
    linewidth: Longint; hollow: Boolean; fillcolor: COLORREF): Boolean; overload;
var
    pnt: tagPOINT;
begin
    pnt.x := center.x + radius;
    pnt.y := center.y;
    Result := _DrawCircle(dc, center, pnt, linecolor, linewidth, hollow, fillcolor);
end;

function _DrawCircle(dc: HDC; center: tagPOINT; radius: Longint; color: COLORREF;
    linewidth: Longint; hollow: Boolean): Boolean; overload;
begin
    Result := _DrawCircle(dc, center, radius, color, linewidth, hollow, color);
end;

//画多边形
function _DrawShape(dc: HDC; var shape: TyyShapeTracked; linecolor: COLORREF;
    linewidth: Longint; hollow: Boolean; fillcolor: COLORREF): Boolean;
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

    if (hollow) then begin
        Polyline(dc, shape.m_Points, shape.m_PointCount);
    end
    else begin
        Polygon(dc, shape.m_Points, shape.m_PointCount);
    end;       

    //恢复以前的hdc
    SelectObject(dc, BrushOld);
    SelectObject(dc, PenOld);
    DeleteObject(BrushNew);
    DeleteObject(PenNew);

    Result := True;
end;
//------------------------------------------------------------------------------




constructor TyySpace.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    Self.IsControl := False;
    Self.ShowHint := False;
    Self.ParentShowHint := False;
    //不这样做
//    Self.TabStop := True;   //让窗体可以获得焦点，目的是响应keydown、keyup消息
    Self.m_bJustOnPaint := False;

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

    m_DC        := 0;
    m_MemDC     := 0;
    m_BlankDC   := 0;

end;

destructor TyySpace.Destroy;
begin
    if (Self.m_DC > 0) then begin
        ReleaseDC(Self.Handle, m_DC);
    end;

    if (Self.m_MemDC > 0) then begin
        DeleteDC(m_MemDC);
    end;

    if (Self.m_BlankDC > 0) then begin
        DeleteDC(m_BlankDC);
    end;

    inherited Destroy;
end;

procedure TyySpace.ResetBuffer();
var
    rect: TRect;
    bitmap: HBITMAP;
    brush: HBRUSH;
begin
    if (Self.ParentWindow <= 0) then begin
        Exit;
    end;

    Windows.GetClientRect(Self.ParentWindow, rect);
    Self.Width := rect.Right - rect.Left;
    Self.Height := rect.Bottom - rect.Top;

    bitmap := CreateCompatibleBitmap(Self.m_DC,
                                     Self.Width,
                                     Self.Height
                                     );
    SelectObject(Self.m_MemDC, bitmap);
    DeleteObject(bitmap);

    brush := CreateSolidBrush(Self.Color);
    rect := Self.GetClientRect();
    FillRect(Self.m_MemDC, rect, brush);
    DeleteObject(brush);

    Self.SetBackColor(Self.Color);

end;

function TyySpace.GetBufferDC(): HDC;
begin
    Result := m_MemDC;
end;

procedure TyySpace.PostBuffer();
var
    rect: TRect;
begin                             
    rect := Self.GetClientRect();
    BitBlt(m_DC,
           0,
           0,
           rect.Right - rect.Left,
           rect.Bottom - rect.Top,
           m_MemDC,
           0,
           0,
           SRCCOPY
           );

end;

procedure TyySpace.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
begin
    if (Message.DC = 0) then begin
        BeginPaint(Handle, PS);
        EndPaint(Handle, PS);
    end;

    Self.PostBuffer();
    Self.OnSelfPaint();
    m_EventHandle_Paint(m_DC);
end;

procedure TyySpace.OnSelfPaint();
var
    LogBrush: TLogBrush;
    brush, OldBrush: HBRUSH;

    NewPen, OldPen: HPEN;
    nOldRop2: Integer;

    i, count: Integer;
begin
    if ((m_CurrentTask = SPACETASK_IDLE)
      or (High(m_TrackPoints) <= 0)) then Exit;

    //下一次鼠标移动时不应该再擦除了
    m_bJustOnPaint := True;

    //准备dc
    LogBrush.lbStyle := BS_SOLID;
    LogBrush.lbColor := Self.Color;
    brush := CreateBrushIndirect(LogBrush);
    FillRect(m_DC, Self.ClientRect, brush);
    DeleteObject(brush);

    LogBrush.lbStyle := m_BrushStyle;
    LogBrush.lbColor := m_BrushColor;
    brush := CreateBrushIndirect(LogBrush);
    OldBrush := SelectObject(m_DC, brush);

    NewPen := CreatePen(m_PenStyle, m_PenWidth, m_PenColor);
    OldPen := SelectObject(m_DC, NewPen);
    nOldRop2 := GetRop2(m_DC);
    SetRop2(m_DC, R2_XORPEN);//异或方式

    //把该干的再干一遍
    case m_CurrentTask of
        SPACETASK_TRACKLINE,
        SPACETASK_TRACKLINE2,
        SPACETASK_TRACKPOLYGON: begin
            count := High(m_TrackPoints) + 1;

            MoveToEx(m_DC,
                     m_TrackPoints[0].x,
                     m_TrackPoints[0].y,
                     nil
                     );

            for i := 1 to count - 2 do begin    //注意最后一条线段是临时的，所以不用画
                LineTo(m_DC,
                       m_TrackPoints[i].x,
                       m_TrackPoints[i].y
                       );
            end;
        end;
    end;

    //干掉这次遗留的东西
    SelectObject(m_DC, OldPen);
    DeleteObject(NewPen);
    SelectObject(m_DC, OldBrush);
    DeleteObject(brush);
    SetRop2(m_DC, nOldRop2);
end;

procedure TyySpace.WMSize(var Message: TWMSize);
begin
    if (IsValid(@m_EventHandle_Size)) then begin
        m_EventHandle_Size(Self.Width, Self.Height);
    end;
end;

procedure TyySpace.WMMove(var Message: TWMMove);
begin
    if (IsValid(@m_EventHandle_Move)) then begin
        m_EventHandle_Move();
    end;
end;

procedure TyySpace.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
var
    n: Integer;
begin
    //先调用回调函数，等会再干活
    if (IsValid(@m_EventHandle_MouseDown)) then begin
        m_EventHandle_MouseDown(Button,
                                Shift,
                                X,
                                Y);
    end;

    if (Button = mbLeft) then begin
        case m_CurrentTask of
            SPACETASK_TRACKLINE,
            SPACETASK_TRACKPOLYGON: begin
                if (High(m_TrackPoints) < 0) then begin
                    //第一个点
                    SetLength(m_TrackPoints, 2);    //注意这里的第二个点留给mousemove
                    m_TrackPoints[0].x := X;
                    m_TrackPoints[0].y := Y;
                    m_TrackPoints[1].x := X;
                    m_TrackPoints[1].y := Y;

                    if (m_CurrentTask = SPACETASK_TRACKLINE) then begin
                        TrackLineProcess();
                    end
                    else begin
                        TrackPolygonProcess();
                    end;
                end
                else begin
                    //第n个点
                    n := High(m_TrackPoints) + 1;
                    SetLength(m_TrackPoints, n + 1);
                    m_TrackPoints[n - 1].x := X;
                    m_TrackPoints[n - 1].y := Y;
                    m_TrackPoints[n].x := X;
                    m_TrackPoints[n].y := Y;
                    if (m_CurrentTask = SPACETASK_TRACKLINE) then begin
                        TrackLineProcess();
                    end
                    else begin
                        TrackPolygonProcess();
                    end;
                end;
            end;

            SPACETASK_TRACKLINE2: begin
                SetLength(m_TrackPoints, 2);
                m_TrackPoints[0].x := X;
                m_TrackPoints[0].y := Y;
                m_TrackPoints[1].x := X;
                m_TrackPoints[1].y := Y;
            end;

            SPACETASK_TRACKRECTANGLE,
            SPACETASK_TRACKCIRCLE,
            SPACETASK_TRACKRECTANGLE2,
            SPACETASK_TRACKCIRCLE2: begin
                if (High(m_TrackPoints) < 0) then begin
                    //第一个点
                    SetLength(m_TrackPoints, 2);    //注意这里的第二个点留给mousemove
                    m_TrackPoints[0].x := X;
                    m_TrackPoints[0].y := Y;
                    m_TrackPoints[1].x := X;
                    m_TrackPoints[1].y := Y;
                end
                else begin
                    //第二个点
                    SetLength(m_TrackPoints, 3);
                    //注意这里的m_TrackPoints[2]才是终点，
                    //m_TrackPoints[1]是临时点，需要擦除mousemove时画上的rect，
                    //如果m_TrackPoints[1] = m_TrackPoints[0]代表不需要擦除
                    //都留在TrackRectangleFinish()中处理
                    m_TrackPoints[2].x := X;
                    m_TrackPoints[2].y := Y;

                    if (m_CurrentTask = SPACETASK_TRACKRECTANGLE) then begin
                        TrackRectangleFinish();
                    end
                    else if (m_CurrentTask = SPACETASK_TRACKCIRCLE) then begin
                        TrackCircleFinish();
                    end;
                end;
            end;

            SPACETASK_PAN: begin
                SetLength(m_TrackPoints, 2);
                m_TrackPoints[0].x := X;
                m_TrackPoints[0].y := Y;
                m_TrackPoints[1].x := X;
                m_TrackPoints[1].y := Y;

            end;

            else begin
                if (High(m_TrackPoints) >= 0) then begin
                    SetLength(m_TrackPoints, 0);
                end;
            end;
        end;
    end
    else if (Button = mbRight) then begin
        case m_CurrentTask of
            SPACETASK_TRACKLINE: begin
                TrackLineFinish();
            end;

            SPACETASK_TRACKPOLYGON: begin
                TrackPolygonFinish();
            end;

            SPACETASK_TRACKRECTANGLE,
            SPACETASK_TRACKCIRCLE: begin
                SetLength(m_TrackPoints, 0);
            end;

            else begin
                if (High(m_TrackPoints) >= 0) then begin
                    SetLength(m_TrackPoints, 0);
                end;
            end;
        end;
    end
    else begin
        //middle button

    end;
end;

procedure TyySpace.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
    //先invoke回调函数，等会再干活
    if (IsValid(@m_EventHandle_MouseUp)) then begin
        m_EventHandle_MouseUp(Button,
                              Shift,
                              X,
                              Y);
    end;

    if (Button = mbLeft) then begin
        case m_CurrentTask of
            SPACETASK_TRACKLINE2: begin
                if (High(m_TrackPoints) > 0) then begin
                    SetLength(m_TrackPoints, High(m_TrackPoints) + 2);
                    m_TrackPoints[High(m_TrackPoints)].x := X;
                    m_TrackPoints[High(m_TrackPoints)].y := Y;
                    TrackLineFinish();
                end;
            end;

            SPACETASK_TRACKRECTANGLE2,
            SPACETASK_TRACKCIRCLE2: begin
                if (High(m_TrackPoints) > 0) then begin
                    SetLength(m_TrackPoints, 3);
                    //注意这里的m_TrackPoints[2]才是终点，
                    m_TrackPoints[2].x := X;
                    m_TrackPoints[2].y := Y;

                    if (m_CurrentTask = SPACETASK_TRACKRECTANGLE2) then begin
                        TrackRectangleFinish();
                    end
                    else begin
                        TrackCircleFinish();
                    end;
                end;
            end;

            SPACETASK_PAN: begin
                if (High(m_TrackPoints) > 0) then begin
                    if (IsValid(@m_EventHandle_Pan)) then begin
                        m_EventHandle_Pan(X - m_TrackPoints[0].x,
                                          Y - m_TrackPoints[0].y
                                          );
                    end;
                    SetLength(m_TrackPoints, 0);
                end;
            end;
            
        end;
    end
    else if (Button = mbRight) then begin
        case m_CurrentTask of
            SPACETASK_TRACKLINE: begin

            end;

            SPACETASK_TRACKPOLYGON: begin

            end;

            SPACETASK_TRACKRECTANGLE: begin

            end;

            SPACETASK_TRACKCIRCLE: begin

            end;

            else begin

            end;
        end;
    end
    else begin
        //middle button

    end;
end;

procedure TyySpace.DoMouseDoubleClick(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
    if (IsValid(@m_EventHandle_MouseDoubleClick)) then begin
        m_EventHandle_MouseDoubleClick(Button,
                                       Shift,
                                       X,
                                       Y);
    end;
end;

procedure TyySpace.WMMouseMove(var Message: TWMMouseMove);
var
    X, Y: Integer;
    Shift: TShiftState;

    LogBrush: tagLOGBRUSH;
    NewBrush, OldBrush: HBRUSH;
    NewPen, NewPen2, OldPen: HPEN;
    nOldRop2: Integer;
    nLastLine: Integer;

    rect: TRect;
begin
    X := Message.XPos;
    Y := Message.YPos;
    Shift := KeysToShiftState(Message.Keys);

    //先搞定回调函数，稍后再干活
    if (IsValid(@m_EventHandle_MouseMove)) then begin
        m_EventHandle_MouseMove(Shift,
                                X,
                                Y);
    end;

    //配置hdc和pen，准备绘制橡皮筋
    NewPen := CreatePen(m_PenStyle, m_PenWidth, m_PenColor);
    OldPen := SelectObject(m_DC, NewPen);

    LogBrush.lbStyle := m_BrushStyle;
    LogBrush.lbColor := m_BrushColor;
    NewBrush := CreateBrushIndirect(LogBrush);
    OldBrush := SelectObject(m_DC, NewBrush);

    nOldRop2 := GetRop2(m_DC);
    SetRop2(m_DC, R2_XORPEN);//异或方式

    case m_CurrentTask of
        SPACETASK_TRACKLINE,
        SPACETASK_TRACKLINE2,
        SPACETASK_TRACKPOLYGON: begin
            nLastLine := High(m_TrackPoints) - 1;   //最后一段起点
            if (nLastLine >= 0) then begin
                //画最后一段
                //开始干活
                if (not m_bJustOnPaint) then begin
                    //先干掉上次干的痕迹
                    MoveToEx(m_DC,
                             m_TrackPoints[nLastLine].x,
                             m_TrackPoints[nLastLine].y,
                             nil
                             );
                    LineTo(m_DC,
                           m_TrackPoints[nLastLine + 1].x,
                           m_TrackPoints[nLastLine + 1].y
                           );
                    if (m_CurrentTask = SPACETASK_TRACKPOLYGON) then begin
                        //是多边形，要多画一条边
                        NewPen2 := CreatePen(PS_DOT, 1, m_PenColor);
                        NewPen := SelectObject(m_DC, NewPen2);
                        LineTo(m_DC,
                               m_TrackPoints[0].x,
                               m_TrackPoints[0].y
                               );
                        SelectObject(m_DC, NewPen);
                        DeleteObject(NewPen2);
                    end;
                end
                else begin
                    m_bJustOnPaint := False;
                end;
                //再干一次
                m_TrackPoints[nLastLine + 1].x := X;
                m_TrackPoints[nLastLine + 1].y := Y;
                MoveToEx(m_DC,
                         m_TrackPoints[nLastLine].x,
                         m_TrackPoints[nLastLine].y,
                         nil
                         );
                LineTo(m_DC,
                       m_TrackPoints[nLastLine + 1].x,
                       m_TrackPoints[nLastLine + 1].y
                       );
                if (m_CurrentTask = SPACETASK_TRACKPOLYGON) then begin
                    //是多边形，要多画一条边
                    NewPen2 := CreatePen(PS_DOT, 1, m_PenColor);
                    NewPen := SelectObject(m_DC, NewPen2);
                    LineTo(m_DC,
                           m_TrackPoints[0].x,
                           m_TrackPoints[0].y
                           );
                    SelectObject(m_DC, NewPen);
                    DeleteObject(NewPen2);
                end;
            end;
        end;

        SPACETASK_TRACKRECTANGLE,
        SPACETASK_TRACKRECTANGLE2: begin
            if (not m_bJustOnPaint) then begin
                if (High(m_TrackPoints) = 1) then begin
                    Self.PostBuffer();
                    m_TrackPoints[1].x := X;
                    m_TrackPoints[1].y := Y;
                    _DrawEnvelope(m_DC,
                                      m_TrackPoints[0],
                                      m_TrackPoints[1],
                                      R2_XORPEN
                                      );
                end;
            end
            else begin
                m_bJustOnPaint := False;
            end;

        end;

        SPACETASK_TRACKCIRCLE,
        SPACETASK_TRACKCIRCLE2: begin
            if (not m_bJustOnPaint) then begin
                if (High(m_TrackPoints) = 1) then begin
                    Self.PostBuffer();
                    m_TrackPoints[1].x := X;
                    m_TrackPoints[1].y := Y;
                    _DrawCircle(m_DC,
                               m_TrackPoints[0],
                               m_TrackPoints[1]
                               );
                end;
            end
            else begin
                m_bJustOnPaint := False;
            end;

        end;

        SPACETASK_PAN: begin
            if (High(m_TrackPoints) > 0) then begin
                m_TrackPoints[1].x := X;
                m_TrackPoints[1].y := Y;

                rect := Self.GetClientRect();
                //这里用到了两层位图
                if (Self.m_BlankDC > 0) then begin
                    NewBrush := CreateSolidBrush(clSilver);
                    FillRect(Self.m_BlankDC, rect, NewBrush);
                    DeleteObject(NewBrush);

                    BitBlt(m_BlankDC,
                           X - m_TrackPoints[0].x,
                           Y - m_TrackPoints[0].y,
                           rect.Right - rect.Left,
                           rect.Bottom - rect.Top,
                           m_MemDC,
                           0,
                           0,
                           SRCCOPY
                           );

                    BitBlt(m_DC,
                           0,
                           0,
                           rect.Right - rect.Left,
                           rect.Bottom - rect.Top,
                           m_BlankDC,
                           0,
                           0,
                           SRCCOPY
                           );
                end
                else begin
                    BitBlt(m_DC,
                           X - m_TrackPoints[0].x,
                           Y - m_TrackPoints[0].y,
                           rect.Right - rect.Left,
                           rect.Bottom - rect.Top,
                           m_MemDC,
                           0,
                           0,
                           SRCCOPY
                           );
                end;


            end;

        end;

        else begin

        end;
    end;

    //干掉这次遗留的东西
    SelectObject(m_DC, OldPen);
    DeleteObject(NewPen);
    SelectObject(m_DC, OldBrush);
    DeleteObject(NewBrush);
    SetRop2(m_DC, nOldRop2);
end;

procedure TyySpace.WMLButtonDown(var Message: TWMLButtonDown);
begin
    DoMouseDown(mbLeft,
                KeysToShiftState(Message.Keys),
                Message.XPos,
                Message.YPos);
end;

procedure TyySpace.WMRButtonDown(var Message: TWMRButtonDown);
begin
    DoMouseDown(mbRight,
                KeysToShiftState(Message.Keys),
                Message.XPos,
                Message.YPos);
end;

procedure TyySpace.WMMButtonDown(var Message: TWMMButtonDown);
begin
    DoMouseDown(mbMiddle,
                KeysToShiftState(Message.Keys),
                Message.XPos,
                Message.YPos);
end;

procedure TyySpace.WMLButtonUp(var Message: TWMLButtonUp);
begin
    DoMouseUp(mbLeft,
              KeysToShiftState(Message.Keys),
              Message.XPos,
              Message.YPos);
end;

procedure TyySpace.WMRButtonUp(var Message: TWMRButtonUp);
begin
    DoMouseUp(mbRight,
              KeysToShiftState(Message.Keys),
              Message.XPos,
              Message.YPos);
end;

procedure TyySpace.WMMButtonUp(var Message: TWMMButtonUp);
begin
    DoMouseUp(mbMiddle,
              KeysToShiftState(Message.Keys),
              Message.XPos,
              Message.YPos);
end;

procedure TyySpace.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
    DoMouseDoubleClick(mbLeft,
                       KeysToShiftState(Message.Keys),
                       Message.XPos,
                       Message.YPos);
end;

procedure TyySpace.WMRButtonDblClk(var Message: TWMRButtonDblClk);
begin
    DoMouseDoubleClick(mbRight,
                       KeysToShiftState(Message.Keys),
                       Message.XPos,
                       Message.YPos);
end;

procedure TyySpace.WMMButtonDblClk(var Message: TWMMButtonDblClk);
begin
    DoMouseDoubleClick(mbMiddle,
                       KeysToShiftState(Message.Keys),
                       Message.XPos,
                       Message.YPos);
end;

procedure TyySpace.WMKeyDown(var Message: TWMKeyDown);
var
  ShiftState: TShiftState;
begin
    if (IsValid(@m_EventHandle_KeyDown)) then begin
        ShiftState := KeyDataToShiftState(Message.KeyData);
        m_EventHandle_KeyDown(Message.CharCode,
                              ShiftState);
    end;
end;

procedure TyySpace.WMKeyUp(var Message: TWMKeyUp);
var
  ShiftState: TShiftState;
begin
    if (IsValid(@m_EventHandle_KeyUp)) then begin
        ShiftState := KeyDataToShiftState(Message.KeyData);
        m_EventHandle_KeyUp(Message.CharCode,
                            ShiftState);
    end;
end;

procedure TyySpace.TrackLineProcess();
var
    yyPolyline: TyyShapeTracked;
    i: Integer;
    bContinueTrack: Boolean;
begin
    if (IsInvalid(@m_EventHandle_PolylineTracking)) then Exit;

    yyPolyline.m_PointCount := High(m_TrackPoints); //注意m_TrackPoints中最后一个是无效点
    if (yyPolyline.m_PointCount > MAXPOINTSINPOLYLINE) then begin
        //点数量不能大于MAXPOINTSINPOLYLINE
        yyPolyline.m_PointCount := MAXPOINTSINPOLYLINE;
    end;
    for i := 0 to yyPolyline.m_PointCount - 1 do begin
        yyPolyline.m_Points[i].x := m_TrackPoints[i].x;
        yyPolyline.m_Points[i].y := m_TrackPoints[i].y;
    end;
    m_EventHandle_PolylineTracking(yyPolyline, bContinueTrack);

    if (not bContinueTrack) then begin
        TrackLineFinish();
    end;

end;

procedure TyySpace.TrackLineFinish();
var
    yyPolyline: TyyShapeTracked;
    i: Integer;
    NewPen, OldPen: HPEN;
    nOldRop2: Integer;
begin
    Self.PostBuffer();

    NewPen := CreatePen(m_PenStyle, m_PenWidth, m_PenColor);
    OldPen := SelectObject(m_DC, NewPen);
    nOldRop2 := GetRop2(m_DC);
    SetRop2(m_DC, R2_XORPEN);//异或方式

//    count := High(m_TrackPoints) + 1;   //注意这里有一个多余的点

    if (High(m_TrackPoints) < 2) then begin
        //线不能少于2个点，注意这里有一个是临时点
        if (High(m_TrackPoints) >= 0) then begin
            if (not m_bJustOnPaint) then begin
                //擦掉绘制过程
(*
                MoveToEx(m_DC,
                         m_TrackPoints[0].x,
                         m_TrackPoints[0].y,
                         nil
                         );
                LineTo(m_DC,
                       m_TrackPoints[1].x,
                       m_TrackPoints[1].y
                       );
*)
            end;
            SetLength(m_TrackPoints, 0);
        end;

        //干掉这次遗留的东西
        SelectObject(m_DC, OldPen);
        DeleteObject(NewPen);
        SetRop2(m_DC, nOldRop2);
        Exit;
    end;

    if (not m_bJustOnPaint) then begin
        //擦掉最后一根线
(*
        MoveToEx(m_DC,
                 m_TrackPoints[count - 2].x,
                 m_TrackPoints[count - 2].y,
                 nil
                 );
        LineTo(m_DC,
               m_TrackPoints[count - 1].x,
               m_TrackPoints[count - 1].y
               );
*)
    end
    else begin
        m_bJustOnPaint := False;
    end;

    //--------------------------------------------------------------------------
    //在这里polyline已经出来了，注意是n-1个点，最后一个点是多余的
    //--------------------------------------------------------------------------

    if (IsInvalid(@m_EventHandle_PolylineTracked)) then begin
        SetLength(m_TrackPoints, 0);
        //干掉这次遗留的东西
        SelectObject(m_DC, OldPen);
        DeleteObject(NewPen);
        SetRop2(m_DC, nOldRop2);

        Exit;
    end;

    SetLength(m_TrackPoints, High(m_TrackPoints));  //最后一个点是临时点

    yyPolyline.m_PointCount := High(m_TrackPoints) + 1;
    if (yyPolyline.m_PointCount > MAXPOINTSINPOLYLINE) then begin
        //点数量不能大于MAXPOINTSINPOLYLINE
        yyPolyline.m_PointCount := MAXPOINTSINPOLYLINE;
    end;
    for i := 0 to yyPolyline.m_PointCount - 1 do begin
        yyPolyline.m_Points[i].x := m_TrackPoints[i].x;
        yyPolyline.m_Points[i].y := m_TrackPoints[i].y;
    end;
    m_EventHandle_PolylineTracked(yyPolyline);

    if (m_CurrentTask <> SPACETASK_TRACKLINE) then begin
        //干掉这次遗留的东西
        SelectObject(m_DC, OldPen);
        DeleteObject(NewPen);
        SetRop2(m_DC, nOldRop2);

        Exit;
    end;

    SetRop2(m_DC, R2_COPYPEN);

(*
    MoveToEx(m_DC,
             m_TrackPoints[0].x,
             m_TrackPoints[0].y,
             nil
             );

    for i := 1 to High(m_TrackPoints) do begin
        LineTo(m_DC,
               m_TrackPoints[i].x,
               m_TrackPoints[i].y
               );
    end;
*)
    //干掉这次遗留的东西
    SetLength(m_TrackPoints, 0);
    SelectObject(m_DC, OldPen);
    DeleteObject(NewPen);
    SetRop2(m_DC, nOldRop2);

end;

procedure TyySpace.TrackPolygonProcess();
var
    yyPolygon: TyyShapeTracked;
    i: Integer;
    bContinueTrack: Boolean;
begin
    if (IsInvalid(@m_EventHandle_PolygonTracking)) then Exit;

    yyPolygon.m_PointCount := High(m_TrackPoints); //注意m_TrackPoints中最后一个是无效点
    if (yyPolygon.m_PointCount > MAXPOINTSINPOLYGON) then begin
        //点数量不能大于MAXPOINTSINPOLYGON
        yyPolygon.m_PointCount := MAXPOINTSINPOLYGON;
    end;
    for i := 0 to yyPolygon.m_PointCount - 1 do begin
        yyPolygon.m_Points[i].x := m_TrackPoints[i].x;
        yyPolygon.m_Points[i].y := m_TrackPoints[i].y;
    end;
    m_EventHandle_PolygonTracking(yyPolygon, bContinueTrack);

    if (not bContinueTrack) then begin
        TrackPolygonFinish();
    end;
end;

procedure TyySpace.TrackPolygonFinish();
var
    yyPolygon: TyyShapeTracked;
    i: Integer;
    NewPen, NewPen2, OldPen: HPEN;
    nOldRop2: Integer;
    logBrush: tagLOGBRUSH;
    NewBrush, OldBrush: HBRUSH;
begin
    Self.PostBuffer();

    NewPen := CreatePen(m_PenStyle, m_PenWidth, m_PenColor);
    OldPen := SelectObject(m_DC, NewPen);
    nOldRop2 := GetRop2(m_DC);
    SetRop2(m_DC, R2_XORPEN);//异或方式

    logBrush.lbStyle := m_BrushStyle;
    logBrush.lbColor := m_BrushColor;
    NewBrush := CreateBrushInDirect(logBrush);
    OldBrush := SelectObject(m_DC, NewBrush);

//    count := High(m_TrackPoints) + 1;   //注意这里有一个多余的点

    if (High(m_TrackPoints) < 3) then begin
        //多边形不能少于3个点，注意这里有一个是临时点
        if (High(m_TrackPoints) >= 0) then begin
            if (not m_bJustOnPaint) then begin
                //擦掉绘制过程
(*
                MoveToEx(m_DC,
                         m_TrackPoints[0].x,
                         m_TrackPoints[0].y,
                         nil
                         );
                for i := 1 to count - 1 do begin
                    LineTo(m_DC,
                           m_TrackPoints[i].x,
                           m_TrackPoints[i].y
                           );
                end;
*)
                NewPen2 := CreatePen(PS_DOT, 1, m_PenColor);
                NewPen := SelectObject(m_DC, NewPen2);
(*
                LineTo(m_DC,
                       m_TrackPoints[0].x,
                       m_TrackPoints[0].y
                       );
*)
                SelectObject(m_DC, NewPen);
                DeleteObject(NewPen2);
            end;
            SetLength(m_TrackPoints, 0);
        end;

        //干掉这次遗留的东西
        SelectObject(m_DC, OldPen);
        DeleteObject(NewPen);
        SetRop2(m_DC, nOldRop2);
        SelectObject(m_DC, OldBrush);
        DeleteObject(NewBrush);
        Exit;
    end;

    if (not m_bJustOnPaint) then begin
        //擦掉最后两根线
(*
        MoveToEx(m_DC,
                 m_TrackPoints[count - 2].x,
                 m_TrackPoints[count - 2].y,
                 nil
                 );
        LineTo(m_DC,
               m_TrackPoints[count - 1].x,
               m_TrackPoints[count - 1].y
               );
*)
        NewPen2 := CreatePen(PS_DOT, 1, m_PenColor);
        NewPen := SelectObject(m_DC, NewPen2);
        LineTo(m_DC,
               m_TrackPoints[0].x,
               m_TrackPoints[0].y
               );
        SelectObject(m_DC, NewPen);
        DeleteObject(NewPen2);
        //闭合
(*
        LineTo(m_DC,
               m_TrackPoints[count - 2].x,
               m_TrackPoints[count - 2].y
               );
*)
    end;

    //--------------------------------------------------------------------------
    //在这里polygon已经出来了，注意是n-1个点，最后一个点是多余的
    //--------------------------------------------------------------------------

    if (IsInvalid(@m_EventHandle_PolygonTracked)) then begin
        //注意起点和最后一个点被两次绘制，最终被干掉了，所以要加上
(*
        MoveToEx(m_DC,
                 m_TrackPoints[0].x,
                 m_TrackPoints[0].y,
                 nil
                 );
        LineTo(m_DC,
               m_TrackPoints[0].x + 1,
               m_TrackPoints[0].y + 1
               );
        MoveToEx(m_DC,
                 m_TrackPoints[High(m_TrackPoints) - 1].x,
                 m_TrackPoints[High(m_TrackPoints) - 1].y,
                 nil
                 );
        LineTo(m_DC,
               m_TrackPoints[High(m_TrackPoints) - 1].x + 1,
               m_TrackPoints[High(m_TrackPoints) - 1].y + 1
               );
*)
        SetLength(m_TrackPoints, 0);
        //干掉这次遗留的东西
        SelectObject(m_DC, OldPen);
        DeleteObject(NewPen);
        SetRop2(m_DC, nOldRop2);
        Exit;
    end;

    SetLength(m_TrackPoints, High(m_TrackPoints));  //最后一个点是临时点
    yyPolygon.m_PointCount := High(m_TrackPoints) + 1;
    if (yyPolygon.m_PointCount > MAXPOINTSINPOLYGON) then begin
        //点数量不能大于MAXPOINTSINPOLYLINE
        yyPolygon.m_PointCount := MAXPOINTSINPOLYGON;
    end;
    for i := 0 to yyPolygon.m_PointCount - 1 do begin
        yyPolygon.m_Points[i].x := m_TrackPoints[i].x;
        yyPolygon.m_Points[i].y := m_TrackPoints[i].y;
    end;
    m_EventHandle_PolygonTracked(yyPolygon);

    if (m_CurrentTask <> SPACETASK_TRACKPOLYGON) then begin
        SetLength(m_TrackPoints, 0);
        //干掉这次遗留的东西
        SelectObject(m_DC, OldPen);
        DeleteObject(NewPen);
        SetRop2(m_DC, nOldRop2);
        Exit;
    end;

(*
    BeginPath(m_DC);
    MoveToEx(m_DC,
             m_TrackPoints[0].x,
             m_TrackPoints[0].y,
             nil
             );
    for i := 1 to High(m_TrackPoints) do begin
        LineTo(m_DC,
               m_TrackPoints[i].x,
               m_TrackPoints[i].y
               );
    end;
    LineTo(m_DC,
           m_TrackPoints[0].x,
           m_TrackPoints[0].y
           );
    EndPath(m_DC);
    nFillRop2 := GetRop2(m_DC);
    SetRop2(m_DC, R2_COPYPEN);
    FillPath(m_DC);
*)
    (*
    //边框
    MoveToEx(m_DC,
             m_TrackPoints[0].x,
             m_TrackPoints[0].y,
             nil
             );
    for i := 1 to High(m_TrackPoints) do begin
        LineTo(m_DC,
               m_TrackPoints[i].x,
               m_TrackPoints[i].y
               );
    end;
    LineTo(m_DC,
           m_TrackPoints[0].x,
           m_TrackPoints[0].y
           );
    *)

//    SetRop2(m_DC, nFillRop2);
    //干掉这次遗留的东西
    SetLength(m_TrackPoints, 0);
    SelectObject(m_DC, OldPen);
    DeleteObject(NewPen);
    SelectObject(m_DC, OldBrush);
    DeleteObject(NewBrush);
    SetRop2(m_DC, nOldRop2);
end;

procedure TyySpace.TrackRectangleFinish();
var
    yyRect: TyyRectTracked;
    NewPen, OldPen: HPEN;
    nOldRop2: Integer;
begin
    if (High(m_TrackPoints) <> 2) then begin
        //去死吧
        SetLength(m_TrackPoints, 0);
        Exit;
    end;

    NewPen := CreatePen(m_PenStyle, m_PenWidth, m_PenColor);
    OldPen := SelectObject(m_DC, NewPen);
    nOldRop2 := GetRop2(m_DC);
    SetRop2(m_DC, R2_XORPEN);//异或方式

    //干掉最后一次mousemove
    _DrawEnvelope(
        m_DC,
        m_TrackPoints[0],
        m_TrackPoints[1],
        R2_XORPEN);
    //画上正确的
    _DrawEnvelope(
        m_DC,
        m_TrackPoints[0],
        m_TrackPoints[2],
        R2_XORPEN);

    //调用回调
    if (IsValid(@m_EventHandle_RectTracked)) then begin
        yyRect.m_Point1 := m_TrackPoints[0];
        yyRect.m_Point2 := m_TrackPoints[2];
        m_EventHandle_RectTracked(yyRect);
    end;

    if (m_CurrentTask <> SPACETASK_TRACKRECTANGLE) then begin
        //干掉这次遗留的东西
        SetLength(m_TrackPoints, 0);
        SelectObject(m_DC, OldPen);
        DeleteObject(NewPen);
        SetRop2(m_DC, nOldRop2);
        Exit;
    end;

    //干掉这次遗留的东西
    SelectObject(m_DC, OldPen);
    DeleteObject(NewPen);
    SetRop2(m_DC, nOldRop2);
    SetLength(m_TrackPoints, 0);

end;

procedure TyySpace.TrackCircleFinish();
var
    yyCircle: TyyCircleTracked;

    NewPen, OldPen: HPEN;
    nOldRop2: Integer;
//    logBrush: tagLOGBRUSH;
//    NewBrush, OldBrush: HBRUSH;
begin
    if (High(m_TrackPoints) <> 2) then begin
        //去死吧
        SetLength(m_TrackPoints, 0);
        Exit;
    end;

    NewPen := CreatePen(m_PenStyle, m_PenWidth, m_PenColor);
    OldPen := SelectObject(m_DC, NewPen);
    nOldRop2 := GetRop2(m_DC);
    SetRop2(m_DC, R2_XORPEN);//异或方式

//    logBrush.lbStyle := m_BrushStyle;
//    logBrush.lbColor := m_BrushColor;
//    NewBrush := CreateBrushInDirect(logBrush);
//    OldBrush := SelectObject(m_DC, NewBrush);

    //干掉最后一次mousemove
    _DrawCircle(m_DC,
               m_TrackPoints[0],
               m_TrackPoints[1]
               );
    //画上足球的
    SetRop2(m_DC, R2_COPYPEN);
//    BeginPath(m_DC);
    _DrawCircle(m_DC,
               m_TrackPoints[0],
               m_TrackPoints[2]
               );
//    EndPath(m_DC);

    if (Isvalid(@m_EventHandle_CircleTracked)) then begin
        yyCircle.m_Center := m_TrackPoints[0];
        yyCircle.m_Radius := Round(Abs(Sqrt(Sqr(m_TrackPoints[0].x - m_TrackPoints[2].x)
          + Sqr(m_TrackPoints[0].y - m_TrackPoints[2].y))));
        m_EventHandle_CircleTracked(yyCircle);
    end;

    if (m_CurrentTask <> SPACETASK_TRACKCIRCLE) then begin
        SetLength(m_TrackPoints, 0);
        SelectObject(m_DC, OldPen);
        DeleteObject(NewPen);
//        SelectObject(m_DC, OldBrush);
//        DeleteObject(NewBrush);
        SetRop2(m_DC, nOldRop2);
        Exit;
    end;

    //干掉这次遗留的东西
    SetLength(m_TrackPoints, 0);
    SelectObject(m_DC, OldPen);
    DeleteObject(NewPen);
//    SelectObject(m_DC, OldBrush);
//    DeleteObject(NewBrush);
    SetRop2(m_DC, nOldRop2);
end;

procedure TyySpace.DrawFocusEnvelope(const envelope: TRect);
begin
    _DrawFocusEnvelope(m_MemDC, envelope.TopLeft, envelope.BottomRight, R2_COPYPEN);
end;

procedure TyySpace.DrawSelectBox(const rect: TRect; const color: COLORREF);
begin
    _DrawSelectBox(m_MemDC, rect, color);
end;

procedure TyySpace.DrawEnvelope(envelope: TRect; linecolor: COLORREF;
    linewidth: Longint; hollow: Boolean; fillcolor: COLORREF);
begin
    _DrawEnvelope(m_MemDC, envelope, linecolor, linewidth, hollow, fillcolor);
end;

procedure TyySpace.DrawEnvelope(envelope: TRect; color: COLORREF; linewidth: Longint;
    hollow: Boolean);
begin
    _DrawEnvelope(m_MemDC, envelope, color, linewidth, hollow);
end;

procedure TyySpace.DrawCircle(circle: TyyCircleTracked; linecolor: COLORREF;
    linewidth: Longint; hollow: Boolean; fillcolor: COLORREF);
begin
    _DrawCircle(m_MemDC, circle.m_Center, circle.m_Radius, linecolor,
        linewidth, hollow, fillcolor);
end;

procedure TyySpace.DrawCircle(center: tagPOINT; radius: Longint; linecolor: COLORREF;
    linewidth: Longint; hollow: Boolean; fillcolor: COLORREF);
begin
    _DrawCircle(m_MemDC, center, radius, linecolor, linewidth, hollow, fillcolor);
end;

procedure TyySpace.DrawCircle(center: tagPOINT; radius: Longint; color: COLORREF;
    linewidth: Longint; hollow: Boolean);
begin
    _DrawCircle(m_MemDC, center, radius, color, linewidth, hollow);
end;

procedure TyySpace.DrawShape(var shape: TyyShapeTracked; linecolor: COLORREF;
    linewidth: Longint; hollow: Boolean; fillcolor: COLORREF);
begin
    _DrawShape(m_MemDC, shape, linecolor, linewidth, hollow, fillcolor);
end;

procedure TyySpace.DrawPolyline(var shape: TyyShapeTracked; linecolor: COLORREF;
    linewidth: Longint);
begin
    _DrawShape(m_MemDC, shape, linecolor, linewidth, True, linecolor);
end;

procedure TyySpace.DrawFillPolygon(var shape: TyyShapeTracked; color: COLORREF;
    linewidth: Longint);
begin
    _DrawShape(m_MemDC, shape, color, linewidth, False, color);
end;

procedure TyySpace.SetIdleTask();
begin
    m_CurrentTask := SPACETASK_IDLE;
    SetLength(m_TrackPoints, 0);
end;

function TyySpace.SetTask(SpaceTask: TSpaceTask): Boolean;
begin
    Result := False;

    case SpaceTask of
        SPACETASK_IDLE: begin
            SetIdleTask();
        end;

        SPACETASK_TRACKLINE: begin
            if (m_CurrentTask <> SPACETASK_TRACKLINE) then begin
                SetIdleTask();
                m_CurrentTask := SPACETASK_TRACKLINE;
            end;
        end;

        SPACETASK_TRACKLINE2: begin
            if (m_CurrentTask <> SPACETASK_TRACKLINE2) then begin
                SetIdleTask();
                m_CurrentTask := SPACETASK_TRACKLINE2;
            end;
        end;

        SPACETASK_TRACKPOLYGON: begin
            if (m_CurrentTask <> SPACETASK_TRACKPOLYGON) then begin
                SetIdleTask();
                m_CurrentTask := SPACETASK_TRACKPOLYGON;
            end;
        end;

        SPACETASK_TRACKRECTANGLE: begin
            if (m_CurrentTask <> SPACETASK_TRACKRECTANGLE) then begin
                SetIdleTask();
                m_CurrentTask := SPACETASK_TRACKRECTANGLE;
            end;
        end;

        SPACETASK_TRACKRECTANGLE2: begin
            if (m_CurrentTask <> SPACETASK_TRACKRECTANGLE2) then begin
                SetIdleTask();
                m_CurrentTask := SPACETASK_TRACKRECTANGLE2;
            end;
        end;

        SPACETASK_TRACKCIRCLE: begin
            if (m_CurrentTask <> SPACETASK_TRACKCIRCLE) then begin
                SetIdleTask();
                m_CurrentTask := SPACETASK_TRACKCIRCLE;
            end;
        end;

        SPACETASK_TRACKCIRCLE2: begin
            if (m_CurrentTask <> SPACETASK_TRACKCIRCLE2) then begin
                SetIdleTask();
                m_CurrentTask := SPACETASK_TRACKCIRCLE2;
            end;
        end;

        SPACETASK_PAN: begin
            if (m_CurrentTask <> SPACETASK_PAN) then begin
                SetIdleTask();
                m_CurrentTask := SPACETASK_PAN;
            end;
        end;

        else begin
            Exit;
        end;
    end;
end;

function TyySpace.GetTask(): TSpaceTask;
begin
    Result := m_CurrentTask;
end;



function TyySpace.InitialPaintEventHandle(EventHandle: TEVENTPaintHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@EventHandle)) then begin
        Exit;
    end;
    m_EventHandle_Paint := EventHandle;
    Result := True;
end;

function TyySpace.InitialSizeEventHandle(EventHandle: TEVENTSizeHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@EventHandle)) then begin
        Exit;
    end;
    m_EventHandle_Size := EventHandle;
    Result := True;
end;

function TyySpace.InitialMoveEventHandle(EventHandle: TEVENTMoveHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@EventHandle)) then begin
        Exit;
    end;
    m_EventHandle_Move := EventHandle;
    Result := True;
end;

function TyySpace.InitialMouseDownEventHandle(EventHandle: TEVENTMouseDownHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@EventHandle)) then begin
        Exit;
    end;
    m_EventHandle_MouseDown := EventHandle;
    Result := True;
end;

function TyySpace.InitialMouseUpEventHandle(EventHandle: TEVENTMouseUpHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@EventHandle)) then begin
        Exit;
    end;
    m_EventHandle_MouseUp := EventHandle;
    Result := True;
end;

function TyySpace.InitialMouseMoveEventHandle(EventHandle: TEVENTMouseMoveHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@EventHandle)) then begin
        Exit;
    end;
    m_EventHandle_MouseMove := EventHandle;
    Result := True;
end;

function TyySpace.InitialMouseDoubleClickEventHandle(EventHandle:
  TEVENTMouseDoubleClickHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@EventHandle)) then begin
        Exit;
    end;
    m_EventHandle_MouseDoubleClick := EventHandle;
    Result := True;
end;

function TyySpace.InitialKeyDownEventHandle(EventHandle: TEVENTKeyDownHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@EventHandle)) then begin
        Exit;
    end;
    m_EventHandle_KeyDown := EventHandle;
    Result := True;
end;

function TyySpace.InitialKeyUpEventHandle(EventHandle: TEVENTKeyUpHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@EventHandle)) then begin
        Exit;
    end;
    m_EventHandle_KeyUp := EventHandle;
    Result := True;
end;

function TyySpace.InitialPolylineEventHandle(DoingEventHandle: TEVENTPolylineTrackingHandle;
  DoneEventHandle: TEVENTPolylineTrackedHandle): Boolean;
begin
(*
    Result := False;
    if ((IsInvalid(@DoingEventHandle))
      or (IsInvalid(@DoneEventHandle))) then begin
        Exit;
    end;
*)
    m_EventHandle_PolylineTracking := DoingEventHandle;
    m_EventHandle_PolylineTracked := DoneEventHandle;
    Result := True;
end;

function TyySpace.InitialPolygonEventHandle(DoingEventHandle: TEVENTPolygonTrackingHandle;
  DoneEventHandle: TEVENTPolygonTrackedHandle): Boolean;
begin
(*
    Result := False;
    if ((IsInvalid(@DoingEventHandle))
      or (IsInvalid(@DoneEventHandle))) then begin
        Exit;
    end;
*)
    m_EventHandle_PolygonTracking := DoingEventHandle;
    m_EventHandle_PolygonTracked := DoneEventHandle;
    Result := True;
end;

function TyySpace.InitialRectEventHandle(DoneHandle: TEVENTRectTrackedHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@DoneHandle)) then begin
        Exit;
    end;

    m_EventHandle_RectTracked := DoneHandle;
    Result := True;
end;

function TyySpace.InitialCircleEventHandle(DoneHandle: TEVENTCircleTrackedHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@DoneHandle)) then begin
        Exit;
    end;

    m_EventHandle_CircleTracked := DoneHandle;
    Result := True;
end;

function TyySpace.InitialPanEventHandle(PanHandle: TEVENTPanHandle): Boolean;
begin
    Result := False;
    if (IsInvalid(@PanHandle)) then begin
        Exit;
    end;

    m_EventHandle_Pan := PanHandle;
    Result := True;
end;

procedure TyySpace.SetBackColor(color: TColor);
var
    bitmap: HBITMAP;
    brush: HBRUSH;
begin
    Self.Color := color;
    if (m_BlankDC > 0) then begin
        DeleteDC(m_BlankDC);
    end;

    m_BlankDC := CreateCompatibleDC(Self.m_DC);
    bitmap := CreateCompatibleBitmap(Self.m_DC,
                                     Self.GetClientRect().Right - Self.GetClientRect().Left,
                                     Self.GetClientRect().Bottom - Self.GetClientRect().Top
                                     );
    SelectObject(m_BlankDC, bitmap);
    DeleteObject(bitmap);

    brush := CreateSolidBrush(Self.Color);
    FillRect(m_BlankDC, Self.GetClientRect(), brush);
    DeleteObject(brush);
end;

function TyySpace.GetBackColor(): TColor;
begin
    Result := Self.Color;
end;

function TyySpace.SetCursor(MouseCursor: TSpaceCursorType): Boolean;
begin
    Result := False;
    if ((MouseCursor < 1) or (MouseCursor > MAXSPACEDEFINEDCURSORTYPE)) then begin
        Exit;
    end;

    m_MouseCursor := MouseCursor;
    Self.Cursor := m_MouseCursor;

    Result := True;
end;

function TyySpace.GetCursor(): TSpaceCursorType;
begin
    Result := m_MouseCursor;
end;


function CreateSpace(Parent: HWND; out yySpace: TyySpace): Boolean;
var
    ParentRect: TRect;
begin
    Result := False;
    if (Parent <= 0) then begin
        Exit;
    end;

    yySpace := TyySpace.Create(nil);
    yySpace.ParentWindow := Parent;
    yySpace.Left := 0;
    yySpace.Top := 0;
    GetWindowRect(Parent, ParentRect);
    yySpace.Width := ParentRect.Right - ParentRect.Left;
    yySpace.Height := ParentRect.Bottom - ParentRect.Top;
    yySpace.m_nSpaceID := 0;

    SetLength(yySpace.m_TrackPoints, 0);
    yySpace.m_DC := GetDC(yySpace.Handle);
    yySpace.m_PenColor := clYellow;
    yySpace.m_PenStyle := PS_SOLID;
    yySpace.m_PenWidth := 1;
    yySpace.m_BrushColor := clWhite;
    yySpace.m_BrushStyle := BS_NULL;

    yySpace.m_MemDC := CreateCompatibleDC(yySpace.m_DC);
    yySpace.SetBackColor(clWhite);

    Result := True;
end;


initialization

finalization

end.
