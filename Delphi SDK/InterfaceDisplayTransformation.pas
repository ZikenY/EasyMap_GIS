unit InterfaceDisplayTransformation;

interface

uses
    InterfaceObj, InterfaceGeometry, WKSStructs, Windows;

type TMapUnits = Longword;
const UNIT_DEGREE       = TMapUnits(0); //“度”用于没有投影的经纬度坐标系
const UNIT_M            = TMapUnits(1);
const UNIT_KM           = TMapUnits(2);
const UNIT_CM           = TMapUnits(3);
const UNIT_MM           = TMapUnits(4);
const UNIT_MILE         = TMapUnits(5);
const UNIT_NAUTICALMILE = TMapUnits(6);
const UNIT_INCH         = TMapUnits(7);
const UNIT_FOOT         = TMapUnits(8);

type TDisplayVisiExtChg = procedure(var viewextent: WKSRect) of Object;

type IDisplayTransformation = interface(IPersist)
    procedure SetPlaneRotate(const degree: Double); stdcall;
    procedure GetPlaneRotate(out degree: Double); stdcall;

    procedure SetAttitude(const degree: Double); stdcall;
    procedure GetAttitude(out degree: Double); stdcall;

    procedure SetRotateCenter(var rotatecenter: WKSPoint); stdcall;
    procedure GetRotateCenter(out rotatecenter: WKSPoint); stdcall;

    procedure SetMapUnit(const mapunit: TMapUnits); stdcall;
    function GetMapUnit(): TMapUnits; stdcall;

    procedure SetMapScale(const mapscale: Double); stdcall;
    procedure GetMapScale(out mapscale: Double); stdcall;

    procedure SetMapCenter(const center: WKSPoint); stdcall;
    procedure GetMapCenter(out center: WKSPoint); stdcall;

    procedure SetVisibleExtent(var extent: WKSRect); stdcall;
    procedure GetVisibleExtent(out extent: WKSRect); stdcall;

    procedure SetDeviceRect(var rect: TRect); stdcall;
    procedure GetDeviceRect(out rect: TRect); stdcall;

    procedure SetLogPixel(const logpixelx: Longint; const logpixely: Longint); stdcall;
    procedure GetLogPixel(out logpixelx: Longint; logpixely: Longint); stdcall;

    procedure Device2MapXY(const x_dev: Longint; const y_dev: Longint; out x_map: Double;
        out y_map: Double); stdcall;
    procedure Map2DeviceXY(const x_map: Double; const y_map: Double; out x_dev: Longint;
        out y_dev: Longint); stdcall;

    procedure Device2Map(var point_dev: TPoint; out point_map: WKSPoint); stdcall;
    procedure Map2Device(var point_map: WKSPoint; out point_dev: TPoint); stdcall;

    procedure Device2Map_Envelope(var envelope_dev: TRect; out envelope_map: WKSRect); stdcall;

    procedure SetReferenceScale(const refscale: Double); stdcall;
    procedure GetReferenceScale(out refscale: Double); stdcall;

    //点是否在可视平面范围内
    function PointInPlane(const x: Double; const y: Double): Boolean; stdcall;

    //用visible extent来裁切geometry，用于绘制之前的处理
    procedure ClipGeometry(const pGeometry: IGeometry; out ppClipped: IGeometry); stdcall;

    function SetVisibleExtentHandle(pHandle: TDisplayVisiExtChg): Boolean; stdcall;
end;

implementation

end.
