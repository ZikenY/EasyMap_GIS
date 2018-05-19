#if !defined(GROUPLAYER_INCLUDED_)
#define GROUPLAYER_INCLUDED_

#include "..\\include\\InterfaceLayer.h"
#include "Display.h"

namespace easymap
{

class CGroupLayer;
typedef TSmartPtr<CGroupLayer> CGroupLayerPtr;

class CGroupLayer : public IGroupLayer
{
CLASS_NAME(CGroupLayer)
PERSIST_DUMP(CGroupLayer)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CGroupLayer();
    CGroupLayer(
        const MapUnits  mapunit,
        const double    basescale
        );
private:
    ~CGroupLayer();

private:
    double              m_MaxVisualScale;       //最大显示比例尺
    double              m_MinVisualScale;       //最小显示比例尺
    long                m_Tag;                  //好玩
    bool                m_Visible;              //是否可视
    byte                m_Alpha;                //透明度
    bool                m_Selectable;           //是否参与选择操作
    string              m_Name;                 //图层名字

    list<ILayerPtr>     m_Layers;
    MapUnits            m_MapUnit;
    double              m_BaseScale;
    string              m_SR;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    DrawResult __stdcall DrawData(
        const IDisplay*         pDisplay,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancel*     pTrackCancel    = NULL
        );

    DrawResult __stdcall DrawSelection(
        const IDisplay*         pDisplay,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancel*     pTrackCancel    = NULL
        );

    DrawResult DrawData(
        const CDisplayPtr       pDisplay,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );

    DrawResult DrawSelection(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );

public:
    void __stdcall SetName(const char* const name);
    const char* __stdcall GetName() const;
    void __stdcall SetVisible(const bool visible);
    bool __stdcall GetVisible() const;
    void __stdcall SetAlpha(const byte alpha);
    byte __stdcall GetAlpha() const;
    void __stdcall SetScaleLimit(const double maxscale, const double minscale);
    void __stdcall GetScaleLimit(double& maxscale, double& minscale) const;
    void __stdcall SetTag(const long tag);
    long __stdcall GetTag() const;
    void __stdcall SetSelectable(const bool selectable);
    bool __stdcall GetSelectable() const;

    bool __stdcall GetExtent(WKSRect& fullext) const;
    MapUnits __stdcall GetMapUnit() const;
    bool __stdcall GetBaseScale(double& scale) const;
    const char* __stdcall GetSpatialReference() const;

    bool __stdcall AddLayer(const ILayer* pLayer);
    bool __stdcall DeleteLayerEx(ILayer* pLayer);
    bool __stdcall GetLayer(ILayer** ppLayer, const dword index) const;
    bool __stdcall SetLayerOrder(const ILayer* pLayer, const dword neworder);

    bool AddLayer(const ILayerPtr pLayer);
    bool __stdcall DeleteLayer(const dword index);
    bool DeleteLayerEx(ILayerPtr pLayer);
    bool GetLayer(ILayerPtr& pLayer, const dword index) const;
    bool SetLayerOrder(const ILayerPtr pLayer, const dword neworder);
    void __stdcall ClearLayers();
    dword __stdcall GetLayerCount() const;
    dword __stdcall GetAllCount() const;
    bool __stdcall FindLayer(ILayer** ppLayer, const char* const layername,
        const char* const classtype) const;

    dword __stdcall Select(const WKSRect& envelope, const bool partialselect = true,
        const bool append = true);
    dword Deselect(const WKSPoint& point);
    dword __stdcall Deselect(const WKSRect& envelope, const bool partialselect = true);
    dword __stdcall GetSelectCount() const;
    void __stdcall ClearSelection();
    dword Select(const WKSPoint& point, const bool append = true);
};
//================================================================================

CLASS_FACTORY(CGroupLayer)

}

#endif
