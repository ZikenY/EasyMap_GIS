#if !defined(BITMAPLAYER_INCLUDED_)
#define BITMAPLAYER_INCLUDED_

#include "CommonInclude.h"
#include "..\\include\\InterfaceBitmapLayer.h"
#include "Display.h"
#include "dibapi.h"

namespace easymap
{

class CBitmapLayer;
typedef TSmartPtr<CBitmapLayer> CBitmapLayerPtr;

class CBitmapLayer : public IBitmapLayer
{
CLASS_NAME(CBitmapLayer)
PERSIST_DUMP(CBitmapLayer)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CBitmapLayer();

private:
    ~CBitmapLayer();

private:
    double      m_MaxVisualScale;
    double      m_MinVisualScale;
    long        m_Tag;
    bool        m_Visible;
    byte        m_Alpha;
    string      m_Name;
    double      m_BaseScale;
    string      m_SR;

    DOMInfo     m_DOMInfo;
    string      m_FileName;
    HDIB        m_hDib;

public:
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

    DrawResult DrawData1(
        const IDisplayCache*    pDisplayCache,
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
    dword __stdcall Select(const WKSRect& envelope, const bool partialselect,
        const bool append);
    dword __stdcall Deselect(const WKSRect& envelope, const bool partialselect);
    dword __stdcall GetSelectCount() const;
    void __stdcall ClearSelection();

    bool __stdcall GetExtent(WKSRect& fullext) const;
    MapUnits __stdcall GetMapUnit() const;
    bool __stdcall GetBaseScale(double& scale) const;
    const char* __stdcall GetSpatialReference() const;

    bool __stdcall LoadBmpFile(const char* const filename, const bool loaddom);
    const char* __stdcall GetBmpFileName() const;
    void __stdcall SetDomInfo(const DOMInfo& dominfo);
    void __stdcall GetDomInfo(DOMInfo& dominfo) const;
};
CLASS_FACTORY(CBitmapLayer)

}

#endif
