unit InterfaceLayer;

interface

uses
    InterfaceObj, InterfaceDisplayTransformation, InterfaceDisplay, InterfaceTrackCancel,
    WKSStructs, Windows;

type ILayer = interface(IPersist)
    function DrawData(const pDisplay: IDisplay; const cacheid: Longint;
        const pEnvelope: PWKSRect; const pTrackCancel: ITrackCancel): TDrawResult; stdcall;
    function DrawSelection(const pDisplay: IDisplay; const cacheid: Longint;
        const pEnvelope: PWKSRect; const pTrackCancel: ITrackCancel): TDrawResult; stdcall;
    function GetExtent(out fullext: WKSRect): Boolean; stdcall;
    function GetMapUnit(): TMapUnits; stdcall;
    function GetBaseScale(scale: Double): Boolean; stdcall;
    procedure SetName(const name: PChar); stdcall;
    function GetName(): PChar; stdcall;
    procedure SetVisible(const visible: Boolean); stdcall;
    function GetVisible(): Boolean; stdcall;
    procedure SetAlpha(const alpha: Byte); stdcall;
    function GetAlpha(): Byte; stdcall;
    procedure SetScaleLimit(const maxscale: Double; const minscale: Double); stdcall;
    procedure GetScaleLimit(out maxscale: Double; out minscale: Double); stdcall;
    procedure SetTag(const tag: Longint); stdcall;
    function GetTag(): Longint; stdcall;
    function GetSpatialReference(): PChar; stdcall;
    procedure SetSelectable(const selectable: Boolean); stdcall;
    function GetSelectable(): Boolean; stdcall;
    function Select(var envelope: WKSRect; const partialselect: Boolean; append: Boolean): Longword; stdcall;
    function Deselect(var envelope: WKSRect; const partialselect: Boolean): Longword; stdcall;
    function GetSelectCount(): Longword; stdcall;
    procedure ClearSelection(); stdcall;
end;


type IEditLayer = interface(IPersist)
    //设置编辑原子操作点
    function SetUndoPoint(): Boolean; stdcall;

    //撤销、重做
    function EditUndoable(): Boolean; stdcall;
    function EditRedoable(): Boolean; stdcall;
    function EditUndo(): Boolean; stdcall;
    function EditRedo(): Boolean; stdcall;

    //保存/取消所做的修改
    function EditCancel(): Boolean; stdcall;
    function SaveData(): Boolean; stdcall;
    function IsDirty(): Boolean; stdcall;

    //该图层是否参与由Map发起的统一编辑操作
    function SetMapEditable(const mapeditable: Boolean): Boolean; stdcall;
    function GetMapEditable(): Boolean; stdcall;
end;


type IGroupLayer = interface(ILayer)
    function AddLayer(const pLayer: ILayer): Boolean; stdcall;
    function DeleteLayer(const index: Longword): Boolean; stdcall;
    function DeleteLayerEx(pLayer: ILayer): Boolean; stdcall;
    function GetLayer(out ppLayer: ILayer; const index: Longword): Boolean; stdcall;
    function SetLayerOrder(const pLayer: ILayer; const neworder: Longword): Boolean; stdcall;
    procedure ClearLayers(); stdcall;
    function GetLayerCount(): Longword; stdcall;
    function GetAllCount(): Longword; stdcall;
    function FindLayer(out ppLayer: ILayer; const layername: PChar;
        const classtype: PChar): Boolean; stdcall;
end;


implementation

end.
