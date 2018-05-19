#if !defined(INTERFACEMAP_INCLUDED_)
#define INTERFACEMAP_INCLUDED_

#include "InterfaceLabelLayer.h"
#include "InterfaceDisplay.h"

namespace easymap
{

typedef long BookMarkStyle;
const BookMarkStyle BOOKMARKSTYLE_RECT          = 0;
const BookMarkStyle BOOKMARKSTYLE_ROUNDRECT     = 1;
const BookMarkStyle BOOKMARKSTYLE_ELLIPSE       = 2;

typedef long BookMarkVisible;
const BookMarkVisible BOOKMARKVISIBLE_NONE      = 0;
const BookMarkVisible BOOKMARKVISIBLE_ALL       = 1;
const BookMarkVisible BOOKMARKVISIBLE_CURRENT   = 2;

class IMap;
typedef TSmartPtr<IMap> IMapPtr;
class IRapidDraw;
typedef TSmartPtr<IRapidDraw> IRapidDrawPtr;
class IPlaceBookmark;
typedef TSmartPtr<IPlaceBookmark> IPlaceBookmarkPtr;

//管理一组图层，处理比例尺、显示范围
//并且统一管理这组图层的编辑回滚操作
class IMap : public IPersist
{
public:
    //加载一个图层到GeoMap中
    virtual bool __stdcall AddLayer(const ILayer* pLayer) = 0;

    //卸载某个图层
    virtual bool __stdcall DeleteLayer(const dword index) = 0;

    //卸载某个图层，可能是grouplayer下的图层
    virtual bool __stdcall DeleteLayerEx(ILayer* pLayer) = 0;

    //得到某一个图层
    virtual bool __stdcall GetLayer(ILayer** ppLayer, const dword index) const = 0;

    //设置图层顺序
    virtual bool __stdcall SetLayerOrder(const ILayer* pLayer, const dword neworder) = 0;

    //卸载所有的图层
    virtual void __stdcall ClearLayers() = 0;

    //已经加载的图层数量，注意不包括GroupLayer下面的子图层
    virtual dword __stdcall GetLayerCount() const = 0;

    //全部的图层数量，包括GroupLayer下面的子图层
    virtual dword __stdcall GetAllCount() const = 0;

    //通过图层名称查找某一个图层，包括grouplayer之下的子图层，注意只返回满足条件的第一个图层
    //classtype可以为NULL，指定图层类型
    virtual bool __stdcall FindLayer(ILayer** ppLayer, const char* const layername,
        const char* const classtype) const = 0;

    //卸载所有的飞机
    virtual void __stdcall ClearAllData() = 0;

    virtual bool __stdcall GetReferenceScale(double& refscale) const = 0;
    virtual const char* __stdcall GetSpatialReference() const = 0;

    virtual bool __stdcall GetMapScale(double& scale) const = 0;
    virtual bool __stdcall GetVisibleExtent(WKSRect& extent) const = 0;

    //取得当前所加载的数据的最大范围
    virtual bool __stdcall GetFullExtent(WKSRect& fullext) const = 0;

    virtual bool __stdcall SetSelectSymbol(const ISymbol* pSymbol) = 0;
    virtual bool __stdcall GetSelectSymbol(const SymbolType symboltype,
        ISymbol** ppSymbol) const = 0;

    //方便些
    virtual dword __stdcall SelectByPoint(const WKSPoint& point, const bool append) = 0;
    virtual dword __stdcall Select(const WKSRect& envelope, const bool partialselect,
        const bool append) = 0;
    virtual dword __stdcall DeselectByPoint(const WKSPoint& point) = 0;
    virtual dword __stdcall Deselect(const WKSRect& envelope, const bool partialselect) = 0;
    virtual void __stdcall ClearSelection() = 0;

    virtual void __stdcall SetName(const char* const mapname) = 0;
    virtual const char* __stdcall GetName() const = 0;

    //图层列表的统一编辑处理
    virtual bool __stdcall SetUndoPoint(const char* const desc) = 0;
    virtual bool __stdcall EditUndoable() const = 0;
    virtual bool __stdcall EditRedoable() const = 0;
    virtual bool __stdcall EditUndo() = 0;
    virtual bool __stdcall EditRedo() = 0;
    virtual bool __stdcall EditCancel() = 0;
    virtual bool __stdcall SaveData() = 0;
    virtual bool __stdcall IsDirty() const = 0;
};


//管理一组图层，进行快速屏幕更新
//适用于GPS信号的实时跟踪显示
class IRapidDraw : public IPersist
{
public:
    //管理一组图层，当窗体重绘的时候就draw
    virtual bool __stdcall RD_AddLayer(const ILayer* pLayer) = 0;
    virtual bool __stdcall RD_RemoveLayer(const dword index) = 0;
    virtual bool __stdcall RD_RemoveLayerEx(ILayer* pLayer) = 0;
    virtual bool __stdcall RD_GetLayer(ILayer** ppLayer, const dword index) const = 0;
    virtual bool __stdcall RD_SetLayerOrder(const ILayer* pLayer, const dword neworder) = 0;
    virtual void __stdcall RD_ClearLayers() = 0;
    virtual dword __stdcall RD_GetLayerCount() const = 0;

    //是否实时绘制
    virtual void __stdcall EnableRapidDraw(bool Enable) = 0;
    virtual bool __stdcall RapidDrawEnabled() const = 0;
};


//位置书签功能
class IPlaceBookmark : public IPersist
{
public:
    //增加新bookmark，当前旋转中心作为bookmark点
    //返回bookmark id，唯一标识
    //返回<0代表失败
    virtual long __stdcall AddBookmark(const char* const text) = 0;

    //指定范围
    //返回bookmark id，唯一标识
    //返回<0代表失败
    virtual long __stdcall AddBookmarkEx(const WKSRect& extent, const char* const text) = 0;

    //修改bookmark
    virtual bool __stdcall ModifyBookmark(const long id, const WKSRect& extent,
        const char* const text) = 0;

    //通过id得到bookmark
    virtual bool __stdcall GetBookmarkByID(const long id, WKSRect& extent,
        IAnsiString** ppText) const = 0;

    //index <--> bookmark id
    //返回<0代表失败
    virtual long __stdcall GetBookmarkIDByIndex(const long index) const = 0;
    virtual long __stdcall GetBookmarkIndexByID(const long id) const = 0;

    //得到当前bookmark
    virtual long __stdcall GetCurrentBookmarkID() const = 0;
    virtual bool __stdcall GetCurrentBookmark(WKSRect& extent, IAnsiString** ppText) const = 0;

    //将显示范围设置成当前的bookmark，注意是旋转中心
    virtual bool __stdcall SetViewToCurrentBookmark() = 0;

    //设置当前bookmark为下一个bookmark
    //返回bookmark id < 0 表示已经到头
    virtual long __stdcall NextBookmark() = 0;

    //设置当前bookmark为上一个bookmark
    //返回bookmark id < 0 表示已经到头
    virtual long __stdcall PreviousBookmark() = 0;

    //删除指定id的bookmark
    virtual bool __stdcall DeleteBookmark(const long id) = 0;

    //删除所有bookmark
    virtual void __stdcall ClearBookmarks() = 0;

    //个数
    virtual dword __stdcall GetBookmarkCount() const = 0;

    //是否显示bookmark
    virtual void __stdcall SetBookmarksVisible(const BookMarkVisible visible) = 0;
    virtual BookMarkVisible __stdcall GetBookmarksVisible() const = 0;

    //字体、形状和底色
    virtual bool __stdcall SetBookmarkSymbol(const ITextSymbol* pTextSymbol) = 0;
    virtual bool __stdcall GetBookmarkSymbol(ITextSymbol** ppTextSymbol) const = 0;
    virtual void __stdcall SetBookmarkStyle(const BookMarkStyle style) = 0;
    virtual BookMarkStyle __stdcall GetBookmarkStyle() const = 0;
    virtual void __stdcall SetBookmarkColor(const COLORREF noactive, const COLORREF active) = 0;
    virtual void __stdcall GetBookmarkColor(COLORREF& noactive, COLORREF& active) const = 0;

    //不突出显示当前bookmark
    virtual void __stdcall DisableActiveBookmarkShow() = 0;

    //存储
    virtual dword __stdcall SaveBookmarksTo(IStreamX* pStream) = 0;
    virtual dword __stdcall LoadBookmarksFrom(IStreamX* pStream) = 0;
};

}

#endif