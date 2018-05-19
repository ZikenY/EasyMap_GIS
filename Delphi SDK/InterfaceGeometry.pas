unit InterfaceGeometry;

interface

//------------------------------------------------------------------------------
//  geometry diagram£º
//
//
//                               ____________________
//                              |                   |<
//                              |   IGeometry       |<______________________
//                              |___________________|<____________         |
//                                 ^ ^ ^ ^ ^ ^                   |         |
//     ____________________________| | | | | |_________          |         |
//    |       _______________________| | | |_________ |          |         |
//    |      |                 ________| |____      | |          |         |
//    |  ____|______      ____|_____    _____|____  | |    ______|_______  |
//    | |          |     |         |   |         |  | |   |             |  |
//    | | IPoint   |     | IPath   |   | IRing   |  | |   | IEnvelope   |  |
//    | |__________|     |_________|   |_________|  | |   |_____________|  |
//    |      :              :               :       | |                    |
//    |___   |              |  _____________|_______| |            ________|______
//       |   |              | |             |  _______|           |              |
//    ___|___*________    __*_|_________  __*_|_________          |   ICircle    |
//   |               |   |             | |             |          |______________|
//   | IMultiPoint   |   | IPolyline   | | IPolygon    |
//   |_______________|   |_____________| |_____________|
//
//
//
//
//                                                                  ;@)
//
//------------------------------------------------------------------------------

uses
    InterfaceObj, WKSStructs;

type
    TGeometryType = Longint;
const
    GEOMETRYTYPE_UNKNOWN    = TGeometryType(0);
    GEOMETRYTYPE_POINT      = TGeometryType(1);
    GEOMETRYTYPE_MULTIPOINT = TGeometryType(2);
    GEOMETRYTYPE_PATH       = TGeometryType(3);
    GEOMETRYTYPE_RING       = TGeometryType(4);
    GEOMETRYTYPE_POLYLINE   = TGeometryType(5);
    GEOMETRYTYPE_POLYGON    = TGeometryType(6);
    GEOMETRYTYPE_ENVELOPE   = TGeometryType(7);
    GEOMETRYTYPE_CIRCLE     = TGeometryType(8);
    GEOMETRYTYPE_ELLIPSE    = TGeometryType(9);

type TShapeType = Longint;
const SHAPETYPE_UNKNOWN     = TShapeType(0);
const SHAPETYPE_POINT       = TShapeType(1);
const SHAPETYPE_MULTIPOINT  = TShapeType(2);
const SHAPETYPE_POLYLINE    = TShapeType(3);
const SHAPETYPE_POLYGON     = TShapeType(4);

type TVertexType = Longint;
const VERTEXTYPE_COMMON     = TVertexType(0);
const VERTEXTYPE_ARCSTART   = TVertexType(1);

type IGeometry = interface(IPersist)
    function GetGeometryType(): TGeometryType; stdcall;
    function GetDimension(): Word; stdcall;
    function GetMBR(out mbr: WKSRect): Boolean; stdcall;
    function Select(var envelop: WKSRect; PartialSelect: Boolean): Boolean; stdcall;
    function Move(const delta_x: Double; const delta_y: Double): Boolean; stdcall;
    function Ratate(var origin: WKSPoint; const angle: Double): Boolean; stdcall;
end;

type IPoint = interface(IGeometry)
    procedure SetX(const x: Double) stdcall;
    procedure SetY(const y: Double) stdcall;
    procedure SetZ(const z: Double) stdcall;
    procedure GetX(out x: Double) stdcall;
    procedure GetY(out y: Double) stdcall;
    procedure GetZ(out z: Double) stdcall;
    procedure SetCoordinates(var point: WKSPointZ) stdcall;
    procedure GetCoordinates(out point: WKSPointZ) stdcall;
end;

type IMultiPoint = interface(IGeometry)
    function AddPoint(var point: WKSPointZ): Boolean; stdcall;
    function DeletePoint(const index: Longword): Boolean; stdcall;
    procedure ClearPoint(); stdcall;
    function GetPoint(var point: WKSPointZ; const index: Longword): Boolean; stdcall;
    function SetPointOrder(const oldorder: Longword; const neworder: Longword): Boolean; stdcall;
    function GetPointCount(): Longword; stdcall;
end;

type IPath = interface(IGeometry)
    function AddPoint(var point: WKSPointZ; const vt: TVertexType): Boolean; stdcall;
    function InsertPoint(const index: Longword; var point: WKSPointZ;
        const vt: TVertexType): Boolean; stdcall;
    function DeletePoint(const index: Longword): Boolean; stdcall;
    procedure ClearPoint(); stdcall;
    function SetPoint(const index: Longword; var point: WKSPointZ; const vt: TVertexType): Boolean; stdcall;
    function GetPoint(const index: Longword; var point: WKSPointZ; var vt: TVertexType): Boolean; stdcall;
    function GetPoint1(const index: Longword; var point: WKSPointZ): Boolean; stdcall;
    function GetPointCount(): Longword; stdcall;
    function GetLength(out length: Double): Boolean; stdcall;
    function GetPositionByDistance(const distance: Double; out position: WKSPoint): Boolean; stdcall;
    function IsValid(): Boolean; stdcall;
end;

type IRing = interface(IGeometry)
    function AddPoint(var point: WKSPointZ; const vt: TVertexType): Boolean; stdcall;
    function InsertPoint(const index: Longword; var point: WKSPointZ;
        const vt: TVertexType): Boolean; stdcall;
    function DeletePoint(const index: Longword): Boolean; stdcall;
    procedure ClearPoint(); stdcall;
    function SetPoint(const index: Longword; var point: WKSPointZ; const vt: TVertexType): Boolean; stdcall;
    function GetPoint(const index: Longword; var point: WKSPointZ; var vt: TVertexType): Boolean; stdcall;
    function GetPoint1(const index: Longword; var point: WKSPointZ): Boolean; stdcall;
    function GetPointCount(): Longword; stdcall;
    function GetArea(length: Double): Boolean; stdcall;
    function GetLength(out length: Double): Boolean; stdcall;
    function IsPostiveDirection(): Boolean; stdcall;
    function IsValid(): Boolean; stdcall;
end;

type IPolyline = interface(IGeometry)
    function AddPathRef(pPath: IPath): Boolean; stdcall;
    function DeletePath(const index: Longword): Boolean; stdcall;
    procedure ClearPath(); stdcall;
    function GetPathRef(out ppPath: IPath; const index: Longword): Boolean; stdcall;
    function SetPathOrder(const oldorder: Longword; const neworder: Longword): Boolean; stdcall;
    function GetPathCount(): Longword; stdcall;
    function GetLength(out length: Double): Boolean; stdcall;
    function GetPositionByDistance(const distance: Double; out position: WKSPoint): Boolean; stdcall;
    function IsValid(): Boolean; stdcall;
end;

type IPolygon = interface(IGeometry)
    function AddRingRef(pRing: IRing): Boolean; stdcall;
    function DeleteRing(const index: Longword): Boolean; stdcall;
    procedure ClearRing(); stdcall;
    function GetRingRef(out ppRing: IRing; const index: Longword): Boolean; stdcall;
    function SetRingOrder(const oldorder: Longword; const neworder: Longword): Boolean; stdcall;
    function GetRingCount(): Longword; stdcall;
    function GetArea(out length: Double): Boolean; stdcall;
    function GetPerimeter(out perimeter: Double): Boolean; stdcall;
    function IsValid(): Boolean; stdcall;
end;

type IEnvelope = interface(IGeometry)
    procedure SetMinX(const minx: Double); stdcall;
    procedure SetMinY(const miny: Double); stdcall;
    procedure SetMaxX(const maxx: Double); stdcall;
    procedure SetMaxY(const maxy: Double); stdcall;
    procedure GetMinX(out minx: Double); stdcall;
    procedure GetMinY(out miny: Double); stdcall;
    procedure GetMaxX(out maxx: Double); stdcall;
    procedure GetMaxY(out maxy: Double); stdcall;
    procedure GetArea(out area: Double); stdcall;
    procedure GetPerimeter(out perimeter: Double); stdcall;
end;

type ICircle = interface(IGeometry)
    procedure SetCenter(var center: WKSPointZ); stdcall;
    procedure GetCenter(out center: WKSPointZ); stdcall;
    procedure SetRadius(const radius: Double); stdcall;
    procedure GetRadius(out radius: Double); stdcall;
    procedure GetArea(out area: Double); stdcall;
    procedure GetPerimeter(perimeter: Double); stdcall;
end;

type IEllipse = interface(IGeometry)
    procedure SetMinX(const minx: Double); stdcall;
    procedure SetMinY(const miny: Double); stdcall;
    procedure SetMaxX(const maxx: Double); stdcall;
    procedure SetMaxY(const maxy: Double); stdcall;
    procedure GetMinX(out minx: Double); stdcall;
    procedure GetMinY(out miny: Double); stdcall;
    procedure GetMaxX(out maxx: Double); stdcall;
    procedure GetMaxY(out maxy: Double); stdcall;
    procedure GetArea(out area: Double); stdcall;
    procedure GetPerimeter(out perimeter: Double); stdcall;
end;

implementation

end.
