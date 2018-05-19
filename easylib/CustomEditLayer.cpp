#include "CommonInclude.h"
#include "CustomEditLayer.h"

namespace easymap
{

CCustomEditLayer::CCustomEditLayer()
{
    m_Name              = "<Layer>";
    m_Visible           = true;
    m_Alpha             = 255;
    m_Selectable        = true;
    m_MaxVisualScale    = 0;
    m_MinVisualScale    = 0;
    m_Tag               = 0;
    m_MapEditable       = true;
}

CCustomEditLayer::~CCustomEditLayer()
{
}

dword __stdcall CCustomEditLayer::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_MaxVisualScale);
    ps->Write(m_MinVisualScale);
    ps->Write(m_Tag);
    ps->WriteBool(m_Visible);
    ps->Write(m_Alpha);
    ps->WriteBool(m_Selectable);
    bool gg = false;
    ps->WriteBool(gg);//!
    ps->Write(m_Name);
    ps->WriteBool(m_MapEditable);

    this->PresaveInstance(ps, assist);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CCustomEditLayer::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Read(m_MaxVisualScale);
    ps->Read(m_MinVisualScale);
    ps->Read(m_Tag);
    ps->ReadBool(m_Visible);
    ps->Read(m_Alpha);
    ps->ReadBool(m_Selectable);
    bool gg;
    ps->ReadBool(gg);
    ps->Read(m_Name);
    ps->ReadBool(m_MapEditable);

    this->PreloadInstance(ps, assist);

    return pStream->GetPos() - oldpos;
}

DrawResult CCustomEditLayer::DrawData(const CDisplayPtr pDisplay, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    if (!this->GetVisible()) {return LAYERDRAW_NOVISIBLE;}

    CDisplayCachePtr pDisplayCache;
    CAST_PTR(pDisplay, pDisplayCache, CDisplayCache);
    long cacheid1 = cacheid;
    if (cacheid < 0)
    {
        cacheid1 = pDisplayCache->CreateCache(pDisplay->GetBackgroundColor());
        pDisplayCache->CopyPrimaryToCache(cacheid1, 0, 0);
    }

    DrawResult r = this->DrawData1(pDisplayCache, cacheid1, pEnvelope, pTrackCancel);

    if (cacheid < 0)
    {
        pDisplayCache->PostCacheToPrimary(cacheid1);
        pDisplayCache->DeleteCache(cacheid1);
    }

    return r;
}

DrawResult CCustomEditLayer::DrawData1(const CDisplayCachePtr pDisplayCache, const long cacheid, 
    const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    if (!this->GetVisible()) {return LAYERDRAW_NOVISIBLE;}

    CDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, CDisplay);

    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);
    double mapscale;
    pTrans->GetMapScale(mapscale);

    if ((0.0001 < m_MaxVisualScale && (mapscale > m_MaxVisualScale))
        || (0.0001 < m_MinVisualScale && (mapscale < m_MinVisualScale)))
    {
        return LAYERDRAW_EXCEEDLIMIT;
    }

    return this->DrawLayerData(pDisplayCache, cacheid, pEnvelope, pTrackCancel);
}

DrawResult CCustomEditLayer::DrawSelection(const CDisplayCachePtr pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope,
    const ITrackCancelPtr pTrackCancel)
{
    CDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, CDisplay);
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);
    double mapscale;
    pTrans->GetMapScale(mapscale);

    if ((0.0001 < m_MaxVisualScale && (mapscale > m_MaxVisualScale))
        || (0.0001 < m_MinVisualScale && (mapscale < m_MinVisualScale)))
    {
        return LAYERDRAW_EXCEEDLIMIT;
    }

    long cacheid1 = cacheid;
    if (cacheid < 0)
    {
        cacheid1 = pDisplayCache->CreateCache(pDisplay->GetBackgroundColor());
        pDisplayCache->CopyPrimaryToCache(cacheid1, 0, 0);
    }

    DrawResult r = this->DrawLayerSelection(pDisplayCache, cacheid1,
        pEnvelope, pTrackCancel);

    if (cacheid < 0)
    {
        pDisplayCache->PostCacheToPrimary(cacheid1);
        pDisplayCache->DeleteCache(cacheid1);
    }

    return r;
}

DrawResult __stdcall CCustomEditLayer::DrawData(const IDisplay* pDisplay, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    if (_invalid(pDisplay)) return LAYERDRAW_UNEXPECTED;
    CDisplayPtr pDisp = (CDisplay*)pDisplay;
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->DrawData(pDisp, cacheid, pEnvelope, pTC);
}

DrawResult __stdcall CCustomEditLayer::DrawSelection(const IDisplay* pDisplay, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    if (_invalid(pDisplay)) return LAYERDRAW_UNEXPECTED;
    IDisplayPtr pDisp = (IDisplay*)pDisplay;
    CDisplayCachePtr pDisplayCache;
    CAST_PTR(pDisp, pDisplayCache, IDisplayCache)

    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->DrawSelection(pDisplayCache, cacheid, pEnvelope, pTC);
}

void __stdcall CCustomEditLayer::SetName(const char* const name)
{
    m_Name = name;
}

const char* __stdcall CCustomEditLayer::GetName() const
{
    return m_Name.c_str();
}

void __stdcall CCustomEditLayer::SetVisible(const bool visible)
{
    m_Visible = visible;
}

bool __stdcall CCustomEditLayer::GetVisible() const
{
    return m_Visible;
}

void __stdcall CCustomEditLayer::SetAlpha(const byte alpha)
{
    m_Alpha = alpha;
}

byte __stdcall CCustomEditLayer::GetAlpha() const
{
    return m_Alpha;
}

void __stdcall CCustomEditLayer::SetScaleLimit(const double maxscale, const double minscale)
{
    m_MaxVisualScale = maxscale;
    m_MinVisualScale = minscale;
}

void __stdcall CCustomEditLayer::GetScaleLimit(double& maxscale, double& minscale) const
{
    maxscale = m_MaxVisualScale;
    minscale = m_MinVisualScale;
}

void __stdcall CCustomEditLayer::SetTag(const long tag)
{
    m_Tag = tag;
}

long __stdcall CCustomEditLayer::GetTag() const
{
    return m_Tag;
}

void __stdcall CCustomEditLayer::SetSelectable(const bool selectable)
{
    m_Selectable = selectable;
}

bool __stdcall CCustomEditLayer::GetSelectable() const
{
    return m_Selectable;
}

bool __stdcall CCustomEditLayer::Clone(IObj** ppObj) const
{
    return false;
}

bool __stdcall CCustomEditLayer::SetMapEditable(const bool mapeditable)
{
    m_MapEditable = mapeditable;
    return true;
}

bool __stdcall CCustomEditLayer::GetMapEditable() const
{
    return m_MapEditable;
}

}
