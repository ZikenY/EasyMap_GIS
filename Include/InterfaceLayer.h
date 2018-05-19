#if !defined(INTERFACELAYER_INCLUDED_)
#define INTERFACELAYER_INCLUDED_

#include "InterfaceDisplay.h"
#include "InterfaceTrackCancel.h"

namespace easymap
{

class ILayer;
class IEditLayer;
class IGroupLayer;

typedef TSmartPtr<ILayer> ILayerPtr;
typedef TSmartPtr<IEditLayer> IEditLayerPtr;
typedef TSmartPtr<IGroupLayer> IGroupLayerPtr;

class ILayer : public IPersist
{
public:
    virtual DrawResult __stdcall DrawData(
        const IDisplay*         pDisplay,
        const long              cacheid,
        const WKSRect* const    pEnvelope,
        const ITrackCancel*     pTrackCancel
        ) = 0;

    virtual DrawResult __stdcall DrawSelection(
        const IDisplay*         pDisplay,
        const long              cacheid,
        const WKSRect* const    pEnvelope,
        const ITrackCancel*     pTrackCancel
        ) = 0;

    virtual bool __stdcall GetExtent(WKSRect& fullext) const = 0;
    virtual MapUnits __stdcall GetMapUnit() const = 0;
    virtual bool __stdcall GetBaseScale(double& scale) const = 0;
    virtual void __stdcall SetName(const char* const name) = 0;
    virtual const char* __stdcall GetName() const = 0;
    virtual void __stdcall SetVisible(const bool visible) = 0;
    virtual bool __stdcall GetVisible() const = 0;
    virtual void __stdcall SetAlpha(const byte alpha) = 0;
    virtual byte __stdcall GetAlpha() const = 0;
    virtual void __stdcall SetScaleLimit(const double maxscale, const double minscale) = 0;
    virtual void __stdcall GetScaleLimit(double& maxscale, double& minscale) const = 0;
    virtual void __stdcall SetTag(const long tag) = 0;
    virtual long __stdcall GetTag() const = 0;

    virtual const char* __stdcall GetSpatialReference() const = 0;

    //为了方便
    virtual void __stdcall SetSelectable(const bool selectable) = 0;
    virtual bool __stdcall GetSelectable() const = 0;
    virtual dword __stdcall Select(const WKSRect& envelope, const bool partialselect,
        const bool append = true) = 0;
    virtual dword __stdcall Deselect(const WKSRect& envelope, const bool partialselect) = 0;
    virtual dword __stdcall GetSelectCount() const = 0;
    virtual void __stdcall ClearSelection() = 0;
};


//================================================================================
//  支持编辑的
//================================================================================
class IEditLayer : public IPersist
{
public:
    //设置编辑原子操作点
    virtual bool __stdcall SetUndoPoint() = 0;

    //撤销、重做
    virtual bool __stdcall EditUndoable() const = 0;
    virtual bool __stdcall EditRedoable() const = 0;
    virtual bool __stdcall EditUndo() = 0;
    virtual bool __stdcall EditRedo() = 0;

    //保存/取消所做的修改
    virtual bool __stdcall EditCancel() = 0;
    virtual bool __stdcall SaveData() = 0;
    virtual bool __stdcall IsDirty() const = 0;

    //该图层是否参与由Map发起的统一编辑操作
    virtual bool __stdcall SetMapEditable(const bool mapeditable) = 0;
    virtual bool __stdcall GetMapEditable() const = 0;
};


//================================================================================
//  逻辑层GroupLayer用来管理一组Layer列表
//================================================================================
class IGroupLayer : public ILayer
{
public:
    virtual bool __stdcall AddLayer(const ILayer* pLayer) = 0;
    virtual bool __stdcall DeleteLayer(const dword index) = 0;
    virtual bool __stdcall DeleteLayerEx(ILayer* pLayer) = 0;
    virtual bool __stdcall GetLayer(ILayer** ppLayer, const dword index) const = 0;
    virtual bool __stdcall SetLayerOrder(const ILayer* pLayer, const dword neworder) = 0;
    virtual void __stdcall ClearLayers() = 0;
    virtual dword __stdcall GetLayerCount() const = 0;
    virtual dword __stdcall GetAllCount() const = 0;
    virtual bool __stdcall FindLayer(ILayer** ppLayer, const char* const layername,
        const char* const classtype) const = 0;
};

}

#endif
