#include "CommonInclude.h"
#include "GroupLayer.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CGroupLayer)

CGroupLayer::CGroupLayer()
{
    INIT_REFCOUNT

    m_Name              = "<GroupLayer>";
    m_Visible           = true;
    m_Alpha             = 255;
    m_Selectable        = true;
    m_MaxVisualScale    = 0;
    m_MinVisualScale    = 0;
    m_Tag               = 0;

    m_MapUnit = UNIT_M;
    m_BaseScale = 0;
    m_SR = "毛猫";
}


CGroupLayer::CGroupLayer(const MapUnits mapunit, const double basescale)
{
    INIT_REFCOUNT

    m_Name              = "<GroupLayer>";
    m_Visible           = true;
    m_Alpha             = 255;
    m_Selectable        = true;
    m_MaxVisualScale    = 0;
    m_MinVisualScale    = 0;
    m_Tag               = 0;

    m_MapUnit = mapunit;
    m_BaseScale = basescale;
    m_SR = "毛猫";
}

CGroupLayer::~CGroupLayer()
{
}

bool __stdcall CGroupLayer::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ILayer"))
        || (0 == strcmp(interfacename, "IGroupLayer"))
        || (0 == strcmp(interfacename, "CGroupLayer")))
    {
        *pp = this;
    }
    else
    {
        *pp = NULL;
        return false;
    }

    static_cast<IObj*>(*pp)->_AddRef();

    return true;
}

dword __stdcall CGroupLayer::_SaveInstance(IStreamX* pStream, void* const assist) const
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

    dword count = m_Layers.size();
    ps->Write(count);
    list<ILayerPtr>::const_iterator it = m_Layers.begin();
    while (it != m_Layers.end())
    {
        ILayerPtr pLayer = *it;
        pLayer->_DumpTo(pStream, assist);
        it++;
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CGroupLayer::_LoadInstance(IStreamX* pStream, void* const assist)
{
    this->ClearLayers();

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

    dword count = 0;
    ps->Read(count);
    for (dword i = 0; i < count; i++)
    {
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom(ps, pPersist, assist);
        ILayerPtr pLayer;
        CAST_PTR(pPersist, pLayer, ILayer)
        m_Layers.push_back(pLayer);
    }

    return pStream->GetPos() - oldpos;
}

void __stdcall CGroupLayer::SetName(const char* const name)
{
    m_Name = name;
}

const char* __stdcall CGroupLayer::GetName() const
{
    return m_Name.c_str();
}

void __stdcall CGroupLayer::SetVisible(const bool visible)
{
    m_Visible = visible;
}

bool __stdcall CGroupLayer::GetVisible() const
{
    return m_Visible;
}

void __stdcall CGroupLayer::SetAlpha(const byte alpha)
{
    m_Alpha = alpha;
}

byte __stdcall CGroupLayer::GetAlpha() const
{
    return m_Alpha;
}

void __stdcall CGroupLayer::SetScaleLimit(const double maxscale, const double minscale)
{
    m_MaxVisualScale = maxscale;
    m_MinVisualScale = minscale;
}

void __stdcall CGroupLayer::GetScaleLimit(double& maxscale, double& minscale) const
{
    maxscale = m_MaxVisualScale;
    minscale = m_MinVisualScale;
}

void __stdcall CGroupLayer::SetTag(const long tag)
{
    m_Tag = tag;
}

long __stdcall CGroupLayer::GetTag() const
{
    return m_Tag;
}

void __stdcall CGroupLayer::SetSelectable(const bool selectable)
{
    m_Selectable = selectable;
}

bool __stdcall CGroupLayer::GetSelectable() const
{
    return m_Selectable;
}

bool __stdcall CGroupLayer::Clone(IObj** ppObj) const
{
    return false;
}

DrawResult CGroupLayer::DrawData(const CDisplayPtr pDisplay, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    if (!this->GetVisible()) {return LAYERDRAW_NOVISIBLE;}

    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);
    double mapscale;
    pTrans->GetMapScale(mapscale);

    if ((0.0001 < m_MaxVisualScale && (mapscale > m_MaxVisualScale))
        || (0.0001 < m_MinVisualScale && (mapscale < m_MinVisualScale)))
    {
        return LAYERDRAW_EXCEEDLIMIT;
    }

    DrawResult r = LAYERDRAW_NORMAL;

    if (m_Layers.size() > 0)
    {
        list<ILayerPtr>::const_iterator it = m_Layers.end();
        while (it != m_Layers.begin())
        {
            ILayerPtr pLayer = *(--it);
            if (!pLayer->GetVisible())
            {
                continue;
            }

            r = pLayer->DrawData(pDisplay._p(), cacheid, pEnvelope,
                pTrackCancel._p());
            if (LAYERDRAW_TRACKCANCEL == r)
            {
                break;
            }
        }
    }

    return r;
}

DrawResult CGroupLayer::DrawSelection(const CDisplayCachePtr pDisplayCache,
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

    DrawResult r = LAYERDRAW_NORMAL;

    if (m_Layers.size() > 0)
    {
        list<ILayerPtr>::const_iterator it = m_Layers.end();
        while (it != m_Layers.begin())
        {
            ILayerPtr pLayer = *(--it);
            if (!pLayer->GetVisible()) continue;

            r = pLayer->DrawSelection(pDisplay._p(), cacheid, pEnvelope, pTrackCancel._p());
            if (LAYERDRAW_TRACKCANCEL == r)
            {
                break;
            }
        }
    }

    return r;

}

DrawResult __stdcall CGroupLayer::DrawData(const IDisplay* pDisplay, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    if (_invalid(pDisplay)) return LAYERDRAW_UNEXPECTED;
    CDisplayPtr pDisp = (CDisplay*)pDisplay;
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->DrawData(pDisp, cacheid, pEnvelope, pTC);
}

DrawResult __stdcall CGroupLayer::DrawSelection(const IDisplay* pDisplay, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    if (_invalid(pDisplay)) return LAYERDRAW_UNEXPECTED;
    IDisplayPtr pDisp = (IDisplay*)pDisplay;
    CDisplayCachePtr pDisplayCache;
    CAST_PTR(pDisp, pDisplayCache, IDisplayCache)
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->DrawSelection(pDisplayCache, cacheid, pEnvelope, pTC);
}

bool __stdcall CGroupLayer::GetExtent(WKSRect& fullext) const
{
    if (1 > m_Layers.size())
    {
        return false;
    }

    bool r = false;
    //先取出第一个有效的layer范围
    list<ILayerPtr>::const_iterator it = m_Layers.begin();
    while (it != m_Layers.end())
    {
        if ((*it)->GetExtent(fullext))
        {
            it++;
            r = true;
            break;
        }
        it++;
    }

    //再和剩下的图层范围叠加
    while (it != m_Layers.end())
    {
        ILayerPtr pLayer = *it;
        it++;
        WKSRect ext;
        if (!pLayer->GetExtent(ext))
        {
            continue;
        }
        r = true;

        UpdateFullExtent(fullext, ext);
    }
    return r;
}

MapUnits __stdcall CGroupLayer::GetMapUnit() const
{
    return m_MapUnit;
}

bool __stdcall CGroupLayer::GetBaseScale(double& scale) const
{
    scale = m_BaseScale;
    return true;
}

const char* __stdcall CGroupLayer::GetSpatialReference() const
{
    return m_SR.c_str();
}

bool CGroupLayer::AddLayer(const ILayerPtr pLayer)
{
    if (!pLayer.Assigned())
    {
        return false;
    }

    list<ILayerPtr>::const_iterator it = m_Layers.begin();
    while (it != m_Layers.end())
    {
        if (pLayer.Compare(*it))
        {
            return false;
        }
        it++;
    }
    m_Layers.push_front(pLayer);

    if (m_Layers.size() == 1)
    {
        m_MapUnit = pLayer->GetMapUnit();
        pLayer->GetBaseScale(m_BaseScale);
        m_SR = pLayer->GetSpatialReference();
    }

    return true;
}

bool __stdcall CGroupLayer::DeleteLayer(const dword index)
{
    if ((0 > index) || (m_Layers.size() <= index))
    {
        return false;
    }

    list<ILayerPtr>::iterator it = m_Layers.begin();
    std::advance(it, index);
    m_Layers.erase(it);
    return true;
}

bool CGroupLayer::DeleteLayerEx(ILayerPtr pLayer)
{
    dword size = m_Layers.size();
    for (dword i = 0; i < size; i++)
    {
        ILayerPtr pLyr;
        this->GetLayer(pLyr, i);
        if (pLyr.Compare(pLayer))
        {
            m_Layers.remove(pLyr);
            return true;
        }
        CGroupLayerPtr pGroupLayer;
        CAST_PTR(pLyr, pGroupLayer, CGroupLayer)
        if (pGroupLayer.Assigned())
        {
            bool r = pGroupLayer->DeleteLayerEx(pLayer);
            if (r) return true;
        }
    }

    return false;
}

bool CGroupLayer::GetLayer(ILayerPtr& pLayer, const dword index) const
{
    pLayer = NULL;
    if ((0 > index) || (m_Layers.size() <= index))
    {
        return false;
    }
    list<ILayerPtr>::const_iterator it = m_Layers.begin();
    std::advance(it, index);
    pLayer = *it;
    return true;
}

bool CGroupLayer::SetLayerOrder(const ILayerPtr pLayer, const dword neworder)
{
    dword size = m_Layers.size();
    if (neworder >= size) return false;

    for (dword i = 0; i < size; i++)
    {
        ILayerPtr pLyr;
        this->GetLayer(pLyr, i);
        if (pLyr.Compare(pLayer))
        {
            if (neworder == i) return true;
            list<ILayerPtr>::iterator it;
            m_Layers.remove(pLyr);
            it = m_Layers.begin();
            std::advance(it, neworder);
            m_Layers.insert(it, pLyr);
            return true;
        }
    }

    return false;
}

void __stdcall CGroupLayer::ClearLayers()
{
    while (0 < this->GetLayerCount())
    {
        this->DeleteLayer(0);
    }
}

dword __stdcall CGroupLayer::GetLayerCount() const
{
    return m_Layers.size();
}

dword __stdcall CGroupLayer::GetAllCount() const
{
    dword all = 0;
    dword count = m_Layers.size();
    for (dword i = 0; i < count; i++)
    {
        all++;
        ILayerPtr pLayer;
        this->GetLayer(pLayer, i);
        CGroupLayerPtr pGroupLayer;
        CAST_PTR(pLayer, pGroupLayer, CGroupLayer)
        if (pGroupLayer.Assigned())
        {
            all += pGroupLayer->GetAllCount();
        }
    }

    return all;
}

dword __stdcall CGroupLayer::Select(const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    dword count = 0;
    dword layercount = this->GetLayerCount();
    for (dword i = 0; i < layercount; i++)
    {
        ILayerPtr pSubLayer;
        this->GetLayer(pSubLayer, i);
        if (pSubLayer->GetVisible() && pSubLayer->GetSelectable())
        {
            count += pSubLayer->Select(envelope, partialselect, append);
        }
    }
    return count;
}

dword CGroupLayer::Deselect(const WKSPoint& point)
{
    WKSRect envelope;
    envelope.left = envelope.right = point.x;
    envelope.top = envelope.bottom = point.y;
    return this->Deselect(envelope, true);
}

dword __stdcall CGroupLayer::Deselect(const WKSRect& envelope, const bool partialselect)
{
    dword count = 0;
    dword layercount = this->GetLayerCount();
    for (dword i = 0; i < layercount; i++)
    {
        ILayerPtr pSubLayer;
        this->GetLayer(pSubLayer, i);
        if (pSubLayer->GetVisible() && pSubLayer->GetSelectable())
        {
            count += pSubLayer->Deselect(envelope, partialselect);
        }
    }
    return count;
}

dword __stdcall CGroupLayer::GetSelectCount() const
{
    dword count = 0;
    dword layercount = this->GetLayerCount();
    for (dword i = 0; i < layercount; i++)
    {
        ILayerPtr pSubLayer;
        this->GetLayer(pSubLayer, i);
        count += pSubLayer->GetSelectCount();
    }
    return count;
}

void __stdcall CGroupLayer::ClearSelection()
{
    dword layercount = this->GetLayerCount();
    for (dword i = 0; i < layercount; i++)
    {
        ILayerPtr pSubLayer;
        this->GetLayer(pSubLayer, i);
        pSubLayer->ClearSelection();
    }
}

dword CGroupLayer::Select(const WKSPoint& point, const bool append)
{
    WKSRect envelope;
    envelope.left = envelope.right = point.x;
    envelope.top = envelope.bottom = point.y;
    return this->Select(envelope, true, append);
}

bool __stdcall CGroupLayer::AddLayer(const ILayer* pLayer)
{
    if (_invalid(pLayer)) return false;
    ILayerPtr pL = (ILayer*)pLayer;
    return this->AddLayer(pL);
}

bool __stdcall CGroupLayer::DeleteLayerEx(ILayer* pLayer)
{
    if (_invalid(pLayer)) return false;
    ILayerPtr pL = pLayer;
    return this->DeleteLayerEx(pL);
}

bool __stdcall CGroupLayer::GetLayer(ILayer** ppLayer, const dword index) const
{
    if (_invalid(ppLayer)) return false;
    ILayerPtr pL;
    bool r = this->GetLayer(pL, index);
    if (pL.Assigned())
    {
        *ppLayer = pL._p();
        (*ppLayer)->_AddRef();
    }

    return r;
}

void _findsublayer(ILayerPtr& pFound, IGroupLayerPtr pGroupLayer,
    const char* const layername, const char* const classtype)
{
    string findname = layername;

    dword layercount = pGroupLayer->GetLayerCount();
    for (dword i = 0; i < layercount; i++)
    {
        ILayerPtr pLayerTmp;
        pGroupLayer->GetLayer(pLayerTmp._ref(), i);

        string tempname = pLayerTmp->GetName();
        if (findname == tempname)
        {
            if ((classtype == NULL) || (string(classtype) == ""))
            {
                pFound = pLayerTmp;
                break;
            }
            else
            {
                string findclasstype = classtype;
                string layerclassname = pLayerTmp->_GetClassName();
                if (findclasstype == layerclassname)
                {
                    pFound = pLayerTmp;
                    break;
                }
            }
        }

        IGroupLayerPtr pSubGroup;
        CAST_PTR(pLayerTmp, pSubGroup, IGroupLayer)
        if (pSubGroup.Assigned())
        {
            _findsublayer(pFound, pSubGroup, layername, classtype);
        }

        if (pFound.Assigned())
        {
            break;
        }
    }
}

bool __stdcall CGroupLayer::FindLayer(ILayer** ppLayer, const char* const layername,
    const char* const classtype) const
{
    if (_invalid(ppLayer) || _invalid(layername))
    {
        return false;
    }

    string findname = layername;

    ILayerPtr pL;

    list<ILayerPtr>::const_iterator it = m_Layers.begin();
    while (it != m_Layers.end())
    {
        ILayerPtr pLayerTmp = *it;
        string tempname = pLayerTmp->GetName();
        if (findname == tempname)
        {
            if ((classtype == NULL) || (string(classtype) == ""))
            {
                pL = pLayerTmp;
                break;
            }
            else
            {
                string findclasstype = classtype;
                string layerclassname = pLayerTmp->_GetClassName();
                if (findclasstype == layerclassname)
                {
                    pL = pLayerTmp;
                    break;
                }
            }
        }

        IGroupLayerPtr pGroupLayer;
        CAST_PTR(pLayerTmp, pGroupLayer, IGroupLayer)
        if (pGroupLayer.Assigned())
        {
            _findsublayer(pL, pGroupLayer, layername, classtype);
        }

        if (pL.Assigned())
        {
            break;
        }

        it++;
    }

    if (pL.Assigned())
    {
        *ppLayer = pL._p();
        (*ppLayer)->_AddRef();
        return true;
    }

    return false;
}

bool __stdcall CGroupLayer::SetLayerOrder(const ILayer* pLayer, const dword neworder)
{
    if (_invalid(pLayer)) return false;
    ILayerPtr pL = (ILayer*)pLayer;
    return this->SetLayerOrder(pL, neworder);
}

}
