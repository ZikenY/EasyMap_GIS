unit InterfaceLayerAgent;

interface

uses
    InterfaceObj, InterfaceGeometry, InterfaceDisplayTransformation, InterfaceDisplay,
    InterfaceLayer, InterfaceTrackCancel, InterfaceFields, WKSStructs, Windows;

type
    TVectorFeatureType = Longint;
const
    VECTORFEATURETYPE_UNKNOWN   = TVectorFeatureType(-1);
    VECTORFEATURETYPE_GEOMETRY  = TVectorFeatureType(0);
    VECTORFEATURETYPE_TEXT      = TVectorFeatureType(1);

type
    TSlimRendererType = Longint;
const
    SLIMRENDERERTYPE_SIMPLE     = TSlimRendererType(0);
    SLIMRENDERERTYPE_UNIQUEVALUE = TSlimRendererType(1);
    SLIMRENDERERTYPE_GRADE      = TSlimRendererType(2);

type TGeometryColumnInfo = record
    ShpType: TShapeType;
    FeatureType: TVectorFeatureType;
    MapUnit: TMapUnits;
    BaseScale: Double;
    DomainXY: WKSRect;
    ToleranceXY: Double;
    MaxZ: Double;
    MinZ: Double;
    ToleranceZ: Double;
    SIParam1: Longword;
    SIParam2: Longword;
    SIParam3: Double;
end;

type IVectorFeature = interface(IObj)
    function GetFID(): Longword; stdcall;
    function GetMBR(out mbr: WKSRect): Boolean; stdcall;
    procedure GetLayer(out ppLayer: ILayer); stdcall;
    function SetGeometryRef(const pGeometry: IGeometry): Boolean; stdcall;
    procedure GetGeometryRef(out ppGeometry: IGeometry); stdcall;
    function GetFieldCount(): Longword; stdcall;
    function SetFieldValue(const index: Longword; const fieldvalue: PChar): Boolean; stdcall;
    function GetFieldValue(const index: Longword; out ppFieldValue: IFieldValue): Boolean; stdcall;
    procedure SetAnnotation(const annotation: PChar); stdcall;
    function GetAnnotation(): PChar; stdcall;
    function Delete(): Boolean; stdcall;
    function Update(): Boolean; stdcall;
end;


type IVectorLayerAgent = interface(IObj)
    function SetLayer(const pLayer: ILayer): Boolean; stdcall;
    function GetLayer(out ppLayer: ILayer): Boolean; stdcall;
    function CreateSlimLayer(
        const shapetype:    TShapeType;
        const mapunit:      TMapUnits;
        const basescale:    Double;
        const precision:    Double;
        var extent:         WKSRect;
        const indexlevel:   Longint;
        const pFields:      IFields;
        const annotation:   Boolean;
        const filename:     PChar;
        const filemap:      Boolean
        ): Boolean; stdcall;

    function LoadSlimLayer(
        const filename:     PChar;
        const readonly:     Boolean;
        const filemap:      Boolean
        ): Boolean; stdcall;
    function LoadShapeLayer(
        const filename:     PChar;
        const mapunit:      TMapUnits;
        const basescale:    Double;
        const precision:    Double;
        const indexlevel:   Longint;
        const readonly:     Boolean
        ): Boolean; stdcall;
    procedure GetPrecision(precision: Double); stdcall;
    procedure SetRefScale(const scale: Double); stdcall;
    procedure GetRefScale(out scale: Double) stdcall;
    function Select(const pFids: IIntArray; const append: Boolean): Longword; stdcall;
    function Deselect(const pFids: IIntArray): Longword; stdcall;
    function GetSelection(out ppFids: IIntArray): Longword; stdcall;
    function GetFids(out ppFids: IIntArray): Boolean; stdcall;
    function GetFeatureCount(): Longword; stdcall;
    function DeleteFeature(const fid: Longword): Boolean; stdcall;
    function CreateFeature(out ppFeature: IVectorFeature): Boolean; stdcall;
    function GetFeature(const fid: Longword; out ppFeature: IVectorFeature): Boolean; stdcall;
    function RapidModifyPoint(const fid: Longword; var point: WKSPoint): Boolean; stdcall;
    function Identify(out ppFids: IIntArray; var envelope: WKSRect;
        const partialselect: Boolean): Boolean; stdcall;
    procedure GetGeometryColumnInfo(out geocolinfo: TGeometryColumnInfo); stdcall;
    procedure GetFields(out ppFields: IFields); stdcall;
    function SetDisplayField(const fieldindex: Longint): Boolean; stdcall;
    procedure GetDisplayField(out fieldindex: Longint); stdcall;
    function SetDefaultSymbol(const pSymbol: ISymbol): Boolean; stdcall;
    function GetDefaultSymbol(out ppSymbol: ISymbol): Boolean; stdcall;
    procedure SetRendererType(const renderertype: TSlimRendererType); stdcall;
    function GetRendererType(): TSlimRendererType; stdcall;
    function SetSymbol(const key: PChar; const pSymbol: ISymbol): Boolean; stdcall;
    function GetSymbol(out ppKey: IAnsiString; out ppSymbol: ISymbol): Boolean; stdcall;
    function GetSymbolByIndex(const index: Longword; out ppKey: IAnsiString;
        out ppSymbol: ISymbol): Boolean; stdcall;
    function GetSymbolCount(): Longword; stdcall;
    procedure ClearSymbol(); stdcall;
    function SetRendererField(const fieldindex: Longint): Boolean; stdcall;
    function GetRendererField(): Longint; stdcall;
    procedure SetShowDefaultSymbol(const showdefaultsymbol: Boolean); stdcall;
    function GetShowDefaultSymbol(): Boolean; stdcall;
    function ReadOnly(): Boolean; stdcall;

    //----------------------------------------------------------------------------
    //  网络分析
    //----------------------------------------------------------------------------
    //  设置容差，用于增加路径点、障碍点等
    procedure SetNetTolerance(const tolerance: Double); stdcall;
    function GetNetTolerance(): Double; stdcall;

    //  创建拓扑网络，field < 0代表用geometry的长度创建图
    function CreateNetTopo(const field: Longint; const bidirectional: Boolean): Boolean; stdcall;
    function CreateNetTopo2(const field_from_to: Longword; const field_to_from: Longword): Boolean; stdcall;

    //  路径点
    function AddNetRoute(var route: WKSPoint): Boolean; stdcall;
    function RemoveNetRoute(var route: WKSPoint): Boolean; stdcall;
    function GetNetRoutes(out ppRoutes: IMultiPoint): Boolean; stdcall;
    procedure ClearNetRoutes() stdcall;

    //  障碍点
    function AddNetBarrierPoint(var barrier: WKSPoint): Boolean; stdcall;
    function RemoveNetBarrierPoint(var barrier: WKSPoint): Boolean; stdcall;
    function GetNetBarrierPoints(out ppBarriers: IMultiPoint): Boolean; stdcall;
    procedure ClearNetBarrierPoints() stdcall;

    //  障碍边
    function AddNetBlockedBiEdge(const fid: Longword): Boolean; stdcall;
    function AddNetBlockedSingleEdge(var from: WKSPoint; var _to: WKSPoint): Boolean; stdcall;
    function RemoveNetBlockedEdge(const fid: Longword): Boolean; stdcall;
    function RemoveNetBlockedSingleEdge(var from: WKSPoint; var _to: WKSPoint): Boolean; stdcall;
    function GetNetBlockedEdgeCount(): Longword; stdcall;
    function GetNetBlockedEdgeByIndex(const i: Longword; out fid: Longword; out from: WKSPoint; out _to: WKSPoint): Boolean; stdcall;
    function GetNetBlockedEdgeIDs(out ppFids: IIntArray): Boolean; stdcall;
    procedure ClearNetBlockedEdges(); stdcall;

    //  计算最佳路径
    function DoBestPath(out ppPath: IPath; out ppFids: IIntArray): Boolean; stdcall;

    //  清除拓扑网络
    procedure ClearNetTopo() stdcall;

    //  存储拓扑网络到Stream
    function StoreNetTopo(): Boolean; stdcall;
    function RestoreNetTopo(): Boolean; stdcall;
    //----------------------------------------------------------------------------

    function Shapefile2ESD(const esdfilename: PChar): Boolean; stdcall;
end;


type TElementType = Longint;
const ELEMENTTYPE_GEOMETRY  = TElementType(1);
const ELEMENTTYPE_TEXT      = TElementType(2);

type IElement = interface(IPersist)
    function Draw(const pDisplayCache: IDisplayCache; const cacheid: Longint;
        const pEnvelope: PWKSRect; const pTrackCancel: ITrackCancel): TDrawResult; stdcall;
    function DrawSelected(const pDisplayCache: IDisplayCache; const cacheid: Longint;
        const pEnvelope: PWKSRect; const pTrackCancel: ITrackCancel): TDrawResult; stdcall;
    function Draw1(const pDisplay: IDisplay): TDrawResult; stdcall;
    function DrawEx(const dc: HDC; const pTrans: IDisplayTransformation): Boolean; stdcall;
    function DrawSelectedEx(const dc: HDC; const pTrans: IDisplayTransformation): Boolean; stdcall;
    function Valid(): Boolean; stdcall;
    function GetElementType(): TElementType; stdcall;
    function Move(const delta_x: Double; const delta_y: Double): Boolean; stdcall;
    function GetExtent(out extent: WKSRect): Boolean; stdcall;
    function SelectTest(var envelope: WKSRect; const partialselect: Boolean): Boolean; stdcall;
    procedure Select(); stdcall;
    procedure Deselect(); stdcall;
    function IsSelected(): Boolean; stdcall;
    function SetGeometry(const pGeometry: IGeometry): Boolean; stdcall;
    procedure GetGeometry(out ppGeometry: IGeometry); stdcall;
    procedure SetText(const text: PChar); stdcall;
    function GetText(): PChar; stdcall;
    function SetSymbol(const pSymbol: ISymbol): Boolean; stdcall;
    function GetSymbol(const symboltype: TSymbolType; out ppSymbol: ISymbol): Boolean; stdcall;
    procedure SetReferenceScale(const refscale: Double); stdcall;
    procedure GetReferenceScale(out refscale: Double); stdcall;
end;

type IElementLayerAgent = interface(IObj)
    function SetLayer(const pLayer: ILayer): Boolean; stdcall;
    function GetLayer(out ppLayer: ILayer): Boolean; stdcall;
    function CreateElementLayer(const layername: PChar): Boolean; stdcall;

    procedure SetRefScale(const scale: Double); stdcall;
    procedure GetRefScale(out scale: Double); stdcall;

    function AddElement(const pElement: IElement): Longword; stdcall;
    function GetElement(const id: Longword; out ppElement: IElement): Boolean; stdcall;
    function SetElement(const id: Longword; const pElement: IElement): Boolean; stdcall;
    function RemoveElement(const id: Longword): Boolean; stdcall;
    function GetSelectElements(out ppIDs: IIntArray): Boolean; stdcall;
    procedure MoveSelectElements(const delta_x: Double; const delta_y: Double); stdcall;
    function RemoveSelectedElements(): Boolean; stdcall;
    function GetElementIDFromIndex(const index: Longword ; out id: Longword): Boolean; stdcall;
    function GetElementCount(): Longword; stdcall;
    procedure ClearElements(); stdcall;
    function Identify(out ppIDs: IIntArray; var envelope: WKSRect;
        const partialselect: Boolean): Boolean; stdcall;
end;


implementation

end.
