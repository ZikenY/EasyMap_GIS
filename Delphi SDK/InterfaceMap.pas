unit InterfaceMap;

interface

uses
    InterfaceObj, InterfaceDisplay, InterfaceLayer, WKSStructs, Windows;

type
    TBookMarkStyle = Longint;
const
    BOOKMARKSTYLE_RECT      = TBookMarkStyle(0);
    BOOKMARKSTYLE_ROUNDRECT = TBookMarkStyle(1);
    BOOKMARKSTYLE_ELLIPSE   = TBookMarkStyle(2);

type
    TBookMarkVisible = Longint;
const
    BOOKMARKVISIBLE_NONE    = TBookMarkVisible(0);
    BOOKMARKVISIBLE_ALL     = TBookMarkVisible(1);
    BOOKMARKVISIBLE_CURRENT = TBookMarkVisible(2);

type IMap = interface(IPersist)
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
    procedure ClearAllData(); stdcall;

    function GetReferenceScale(out refscale: Double): Boolean; stdcall;
    function GetSpatialReference(): PChar; stdcall;

    function GetMapScale(out scale: Double): Boolean; stdcall;
    function GetVisibleExtent(out extent: WKSRect): Boolean; stdcall;

    //取得当前所加载的数据的最大范围
    function GetFullExtent(out fullext: WKSRect): Boolean; stdcall;

    function SetSelectSymbol(const pSymbol: ISymbol): Boolean; stdcall;
    function GetSelectSymbol(const symboltype: TSymbolType;
        out ppSymbol: ISymbol): Boolean; stdcall;

    //方便些
    function SelectByPoint(var point: WKSPoint; const append: Boolean): Longword; stdcall;
    function Select(var envelope: WKSRect; const partialselect: Boolean; const append: Boolean): Longword; stdcall;
    function DeselectByPoint(var point: WKSPoint): Longword; stdcall;
    function Deselect(var envelope: WKSRect; const partialselect: Boolean): Longword; stdcall;
    procedure ClearSelection(); stdcall;

    procedure SetName(const mapname: PChar);  stdcall;
    function GetName(): PChar; stdcall;

    //图层列表的统一编辑处理
    function SetUndoPoint(const desc: PChar): Boolean; stdcall;
    function EditUndoable(): Boolean; stdcall;
    function EditRedoable(): Boolean; stdcall;
    function EditUndo(): Boolean; stdcall;
    function EditRedo(): Boolean; stdcall;
    function EditCancel(): Boolean; stdcall;
    function SaveData(): Boolean; stdcall;
    function IsDirty(): Boolean; stdcall;
end;

type IRapidDraw = interface(IPersist)
    //管理一组图层，当窗体重绘的时候就draw
    function RD_AddLayer(const pLayer: ILayer): Boolean; stdcall;
    function RD_RemoveLayer(const index: Longword): Boolean; stdcall;
    function RD_RemoveLayerEx(pLayer: ILayer): Boolean; stdcall;
    function RD_GetLayer(out ppLayer: ILayer; const index: Longword): Boolean; stdcall;
    function RD_SetLayerOrder(const pLayer: ILayer; const neworder: Longword): Boolean; stdcall;
    procedure RD_ClearLayers(); stdcall;
    function RD_GetLayerCount(): Longword; stdcall;

    //是否实时绘制
    procedure EnableRapidDraw(Enable: Boolean); stdcall;
    function RapidDrawEnabled(): Boolean; stdcall;
end;

//位置书签功能
type IPlaceBookmark = interface(IPersist)

    //增加新bookmark，当前旋转中心作为bookmark点
    //返回bookmark id，唯一标识
    //返回<0代表失败
    function AddBookmark(const text: PChar): Longint; stdcall;

    //指定范围
    //返回bookmark id，唯一标识
    //返回<0代表失败
    function AddBookmarkEx(var extent: WKSRect; const text: PChar): Longint; stdcall;

    //修改bookmark
    function ModifyBookmark(const id: Longint; var extent: WKSRect;
        const text: PChar): Boolean; stdcall;

    //通过id得到bookmark
    function GetBookmarkByID(const id: Longint; out extent: WKSRect;
        out ppText: IAnsiString): Boolean; stdcall;

    //index <--> bookmark id
    //返回<0代表失败
    function GetBookmarkIDByIndex(const index: Longint): Longint; stdcall;
    function GetBookmarkIndexByID(const id: Longint): Longint; stdcall;

    //得到当前bookmark
    function GetCurrentBookmarkID(): Longint; stdcall;
    function GetCurrentBookmark(out extent: WKSRect; out ppText: IAnsiString): Boolean; stdcall;

    //将显示范围设置成当前的bookmark
    function SetViewToCurrentBookmark(): Boolean; stdcall;

    //设置当前bookmark为下一个bookmark
    //返回bookmark id < 0 表示已经到头
    function NextBookmark(): Longint; stdcall;

    //设置当前bookmark为上一个bookmark
    //返回bookmark id < 0 表示已经到头
    function PreviousBookmark(): Longint; stdcall;

    //删除指定id的bookmark
    function DeleteBookmark(const id: Longint): Boolean; stdcall;

    //删除所有bookmark
    procedure ClearBookmarks(); stdcall;

    //个数
    function GetBookmarkCount(): Longword; stdcall;

    //是否显示bookmark
    procedure SetBookmarksVisible(const visible: TBookMarkVisible); stdcall;
    function GetBookmarksVisible(): TBookMarkVisible; stdcall;

    //字体、形状和底色
    function SetBookmarkSymbol(const pTextSymbol: ITextSymbol): Boolean; stdcall;
    function GetBookmarkSymbol(out ppTextSymbol: ITextSymbol): Boolean; stdcall;
    procedure SetBookmarkStyle(const style: TBookMarkStyle); stdcall;
    function GetBookmarkStyle(): TBookMarkStyle; stdcall;
    procedure SetBookmarkColor(const noactive: COLORREF; const active: COLORREF); stdcall;
    procedure GetBookmarkColor(out noactive: COLORREF; out active: COLORREF); stdcall;

    procedure DisableActiveBookmarkShow(); stdcall;

    //存储
    function SaveBookmarksTo(pStream: IStreamX): Longword; stdcall;
    function LoadBookmarksFrom(pStream: IStreamX): Longword; stdcall;
end;

implementation

end.
