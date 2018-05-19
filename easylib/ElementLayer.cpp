#include "CommonInclude.h"
#include "ElementLayer.h"
#include "MemoryStream.h"
#include "MultiSymbol.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CElementLayer)
CLASS_FACTORY_INSTANCE(CGeometryElement)
CLASS_FACTORY_INSTANCE(CTextElement)


void _Create_sucking_Symbol(GeometryType geotype, ISymbolPtr& pSymbol)
{
    switch (geotype)
    {
    case GEOMETRYTYPE_POINT:
    case GEOMETRYTYPE_MULTIPOINT:
        {
            IMultiPointSymbolPtr pMPS = new CMultiPointSymbol;
            pMPS->AddSimpleSymbol(RGB(180, 58, 113), 3.5);
            CAST_PTR(pMPS, pSymbol, ISymbol)
        }
        break;

    case GEOMETRYTYPE_POLYLINE:
        {
            IMultiLineSymbolPtr pMLS = new CMultiLineSymbol;
            pMLS->AddSimpleSymbol(RGB(180, 58, 113), 1);
            CAST_PTR(pMLS, pSymbol, ISymbol)
        }
        break;

    case GEOMETRYTYPE_POLYGON:
    case GEOMETRYTYPE_ENVELOPE:
    case GEOMETRYTYPE_CIRCLE:
    case GEOMETRYTYPE_ELLIPSE:
        {
            CSimpleFillSymbolPtr pFillSymbol = new CSimpleFillSymbol;
            pFillSymbol->SetColor(RGB(110, 158, 180));
            pFillSymbol->SetFillStyle(BS_HATCHED);
            pFillSymbol->SetFillHatch(HS_DIAGCROSS);
            pFillSymbol->SetBorderColor(RGB(210, 58, 33));
            pFillSymbol->SetBorderWidth(0.8);
            IMultiFillSymbolPtr pMFS = new CMultiFillSymbol;
            pMFS->AddSymbol((IFillSymbol*)(pFillSymbol._p()));
            CAST_PTR(pMFS, pSymbol, ISymbol)
        }
        break;

    default:
        {
        }
    }
}

CElementLayer::CElementLayer()
{
    INIT_REFCOUNT

    this->Init();
}

CElementLayer::~CElementLayer()
{
}

void CElementLayer::Init()
{
    m_Name = "Elements";
    m_MapUnit = UNIT_MM;
    m_BaseScale = 1;
    m_RefScale = 0;
    m_MaxID = 0;

    m_UndoProcs.clear();
    m_UndoIndex = 0;
    EditProcess ep;
    m_UndoProcs.push_back(ep);
    m_SaveDirty = false;
    m_XX = "hell";
}

bool __stdcall CElementLayer::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ILayer")))
    {
        *pp = static_cast<ILayer*>(this);
    }
    else if (0 == strcmp(interfacename, "IEditLayer"))
    {
        *pp = static_cast<IEditLayer*>(this);
    }
    else if ((0 == strcmp(interfacename, "CCustomEditLayer"))
        || (0 == strcmp(interfacename, "CElementLayer")))
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

dword CElementLayer::PresaveInstance(CStreamPtr pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    dword elementcount = m_Elements.size();
    pStream->Write(elementcount);

    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        dword id = it->first;
        pStream->Write(id);
        IElementPtr pElement = it->second;
        IStreamX* psx = pStream._p();
        pElement->_DumpTo(psx, assist);
        it++;
    }

    pStream->Write(&m_MapUnit, sizeof(MapUnits));
    pStream->Write(m_BaseScale);
    pStream->Write(m_RefScale);
    pStream->Write(m_MaxID);

    return pStream->GetPos() - oldpos;
}

dword CElementLayer::PreloadInstance(CStreamPtr pStream, void* const assist)
{
    this->ClearElements();

    dword oldpos = pStream->GetPos();

    dword elementcount;
    pStream->Read(elementcount);
    for (dword i = 0; i < elementcount; i++)
    {
        dword id;
        pStream->Read(id);
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom(pStream, pPersist, assist);
        IElementPtr pElement;
        CAST_PTR(pPersist, pElement, CElement)
        m_Elements[id] = pElement;
    }

    pStream->Read(&m_MapUnit, sizeof(MapUnits));
    pStream->Read(m_BaseScale);
    pStream->Read(m_RefScale);
    pStream->Read(m_MaxID);

    m_UndoProcs.clear();
    EditProcess ep;
    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        dword id = it->first;
        IElementPtr pElement = it->second;
        ep.elements[id] = pElement;
        it++;
    }
    m_UndoProcs.push_back(ep);
    m_UndoIndex = 0;
    m_SaveDirty = false;

    return pStream->GetPos() - oldpos;
}

DrawResult CElementLayer::DrawLayerData(const CDisplayCachePtr pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    DrawResult r = LAYERDRAW_NORMAL;

    CDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, CDisplay)
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);

    WKSRect viewextent;
    if (pEnvelope)
    {
        viewextent = *pEnvelope;
        CorrectEnvelope(viewextent);
    }
    else
    {
        pTrans->GetVisibleExtent(viewextent);
    }

    double oldrefscale;
    pTrans->GetReferenceScale(oldrefscale);
    pTrans->SetReferenceScale(m_RefScale);

    long tmpid = pDisplayCache->CreateCache(RGB(255, 255, 255), m_Alpha);
    pDisplayCache->CopyCacheToCache(cacheid, tmpid, 0, 0);

    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pElement = it->second;
        it++;

        WKSRect elementextent;
        pElement->GetExtent(elementextent);
        if (!EnvelopesTouched(viewextent, elementextent))
        {
            continue;
        }

        pElement->Draw(pDisplayCache._p(), tmpid, pEnvelope, pTrackCancel._p());
    }

    pDisplayCache->CopyCacheToCache(tmpid, cacheid, 0, 0);
    pDisplayCache->DeleteCache(tmpid);
    pTrans->SetReferenceScale(oldrefscale);

    return r;
}

DrawResult CElementLayer::DrawLayerSelection(const CDisplayCachePtr pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    DrawResult r = LAYERDRAW_NORMAL;

    CDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, CDisplay)
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);

    WKSRect viewextent;
    if (pEnvelope)
    {
        viewextent = *pEnvelope;
        CorrectEnvelope(viewextent);
    }
    else
    {
        pTrans->GetVisibleExtent(viewextent);
    }

    double oldrefscale;
    pTrans->GetReferenceScale(oldrefscale);
    pTrans->SetReferenceScale(m_RefScale);

    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pElement = it->second;
        it++;

        WKSRect elementextent;
        pElement->GetExtent(elementextent);
        if (!EnvelopesTouched(viewextent, elementextent))
        {
            continue;
        }

        pElement->DrawSelected(pDisplayCache._p(), cacheid, pEnvelope, pTrackCancel._p());
    }

    pTrans->SetReferenceScale(oldrefscale);

    return r;
}

bool CElementLayer::DrawEx(const HDC dc, const CDisplayTransformationPtr pTrans,
    const WKSRect* const pEnvelope) const
{
    if (!pTrans.Assigned()) return false;

    WKSRect viewextent;
    if (pEnvelope)
    {
        viewextent = *pEnvelope;
        CorrectEnvelope(viewextent);
    }
    else
    {
        pTrans->GetVisibleExtent(viewextent);
    }

    double oldrefscale;
    pTrans->GetReferenceScale(oldrefscale);
    pTrans->SetReferenceScale(m_RefScale);

    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pElement = it->second;
        it++;

        WKSRect elementextent;
        pElement->GetExtent(elementextent);
        if (!EnvelopesTouched(viewextent, elementextent))
        {
            continue;
        }

        pElement->DrawEx(dc, pTrans._p());
    }

    pTrans->SetReferenceScale(oldrefscale);

    return true;
}

bool CElementLayer::DrawSelectedEx(const HDC dc, const CDisplayTransformationPtr pTrans,
    const WKSRect* const pEnvelope) const
{
    if (!pTrans.Assigned()) return false;

    WKSRect viewextent;
    if (pEnvelope)
    {
        viewextent = *pEnvelope;
        CorrectEnvelope(viewextent);
    }
    else
    {
        pTrans->GetVisibleExtent(viewextent);
    }

    double oldrefscale;
    pTrans->GetReferenceScale(oldrefscale);
    pTrans->SetReferenceScale(m_RefScale);

    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pElement = it->second;
        it++;

        WKSRect elementextent;
        pElement->GetExtent(elementextent);
        if (!EnvelopesTouched(viewextent, elementextent))
        {
            continue;
        }

        pElement->DrawSelectedEx(dc, pTrans._p());
    }

    pTrans->SetReferenceScale(oldrefscale);

    return true;
}

bool __stdcall CElementLayer::GetExtent(WKSRect& fullext) const
{
    if (m_Elements.size() < 1) {return false;}

    IElementPtr pElement;
    bool r = false;
    //先取出第一个有效的extent
    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        pElement = it->second;
        if (pElement->GetExtent(fullext))
        {
            it++;
            r = true;
            break;
        }
        it++;
    }

    //再和剩下的范围叠加
    while (it != m_Elements.end())
    {
        pElement = it->second;
        it++;
        WKSRect ext;
        if (!pElement->GetExtent(ext))
        {
            continue;
        }
        r = true;

        UpdateFullExtent(fullext, ext);
    }
    return r;
}

MapUnits __stdcall CElementLayer::GetMapUnit() const
{
    return m_MapUnit;
}

bool __stdcall CElementLayer::GetBaseScale(double& scale) const
{
    scale = 1;
    return true;
}

const char* __stdcall CElementLayer::GetSpatialReference() const
{
    return m_XX.c_str();
}

void CElementLayer::SetRefScale(const double& scale)
{
    m_RefScale = scale;
}

void CElementLayer::GetRefScale(double& scale) const
{
    scale = m_RefScale;
}

dword CElementLayer::Select(const WKSPoint& point, const bool append)
{
    WKSRect envelope;
    envelope.left = envelope.right = point.x;
    envelope.top = envelope.bottom = point.y;
    return this->Select(envelope, true, append);
}

dword __stdcall CElementLayer::Select(const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    dword count = 0;
    if (!append)
    {
        this->ClearSelection();
    }

    WKSRect env = envelope;
    CorrectEnvelope(env);

    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pElement = it->second;
        it++;

        WKSRect elementextent;
        pElement->GetExtent(elementextent);

        //??
        CTextElementPtr pTextElement;
        CAST_PTR(pElement, pTextElement, CTextElement)
        if (!EnvelopesTouched(env, elementextent) && !pTextElement.Assigned())
        {
            continue;
        }

        if (pElement->SelectTest(env, partialselect))
        {
            pElement->Select();
            count++;
        }
    }

    return count;
}

dword CElementLayer::Deselect(const WKSPoint& point)
{
    WKSRect envelope;
    envelope.left = envelope.right = point.x;
    envelope.top = envelope.bottom = point.y;
    return this->Deselect(envelope, true);
}

dword __stdcall CElementLayer::Deselect(const WKSRect& envelope, const bool partialselect)
{
    dword count = 0;

    WKSRect env = envelope;
    CorrectEnvelope(env);

    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pElement = it->second;
        it++;

        WKSRect elementextent;
        pElement->GetExtent(elementextent);
        if (!EnvelopesTouched(envelope, elementextent))
        {
            continue;
        }

        if (pElement->SelectTest(env, partialselect))
        {
            pElement->Deselect();
            count++;
        }
    }

    return count;
}

dword __stdcall CElementLayer::GetSelectCount() const
{
    dword count = 0;

    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pElement = it->second;
        it++;

        if (pElement->IsSelected())
        {
            count++;
        }
    }

    return count;
}

void __stdcall CElementLayer::ClearSelection()
{
    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pElement = it->second;
        it++;
        pElement->Deselect();
    }
}

dword CElementLayer::AddElement(const IElementPtr pElement)
{
    if (!pElement.Assigned()) {return 0;}
    if (!pElement->Valid()) {return 0;}

    IObjPtr pObj;
    CLONE_PTR(pElement, pObj)
    IElementPtr pCloneElement;
    CAST_PTR(pObj, pCloneElement, CElement)
    m_Elements[++m_MaxID] = pCloneElement;
    return m_MaxID;
}

bool CElementLayer::GetElement(const dword id, IElementPtr& pElement) const
{
    map<dword, IElementPtr>::const_iterator it = m_Elements.find(id);
    if (it == m_Elements.end()) return false;

    IObjPtr pObj;
    CLONE_PTR(it->second, pObj)
    CAST_PTR(pObj, pElement, CElement)
    return true;
}

bool CElementLayer::SetElement(const dword id, const IElementPtr& pElement)
{
    map<dword, IElementPtr>::const_iterator it = m_Elements.find(id);
    if (it == m_Elements.end()) return false;

    if (pElement.Assigned())
    {
        IObjPtr pObj;
        CLONE_PTR(pElement, pObj)
        IElementPtr pCloneElement;
        CAST_PTR(pObj, pCloneElement, CElement)
        m_Elements[id] = pCloneElement;
    }

    return true;
}

bool CElementLayer::RemoveElement(const dword id)
{
    map<dword, IElementPtr>::const_iterator it = m_Elements.find(id);
    if (it == m_Elements.end()) return false;

    m_Elements.erase(id);
    return true;
}

bool CElementLayer::GetSelectElements(vector<dword>& ids) const
{
    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pEleTmp = it->second;
        if (pEleTmp->IsSelected())
        {
            ids.push_back(it->first);
        }
        it++;
    }

    return true;
}

void CElementLayer::MoveSelectElements(const double& delta_x, const double& delta_y)
{
    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        dword elementid = it->first;
        IElementPtr pEleTmp = it->second;
        it++;

        if (pEleTmp->IsSelected())
        {
            IObjPtr pObj;
            CLONE_PTR(pEleTmp, pObj)
            IElementPtr pElementCloned;
            CAST_PTR(pObj, pElementCloned, CElement)
            pElementCloned->Move(delta_x, delta_y);
            pElementCloned->Select();
            m_Elements[elementid] = pElementCloned;
        }
    }
}

bool CElementLayer::RemoveSelectedElements()
{
    bool r = false;
    map<dword, IElementPtr>::iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pEleTmp = it->second;

        if (pEleTmp->IsSelected())
        {
            map<dword, IElementPtr>::iterator it1 = it++;
            m_Elements.erase(it1);
            r = true;
        }
        else
        {
            it++;
        }
    }

    return r;
}

bool CElementLayer::GetElementIDFromIndex(const dword index, dword& id) const
{
    if (m_Elements.size() <= index) return false;

    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    std::advance(it, index);
    id = it->first;
    return true;
}

dword CElementLayer::GetElementCount() const
{
    return m_Elements.size();
}

void CElementLayer::ClearElements()
{
    m_Elements.clear();
}

bool CElementLayer::Identify(vector<dword>& resultids, const WKSRect& envelope,
    const bool partialselect)
{
    WKSRect env = envelope;
    CorrectEnvelope(env);

    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        IElementPtr pElement = it->second;
        it++;

        WKSRect elementextent;
        pElement->GetExtent(elementextent);

        CTextElementPtr pTextElement;
        CAST_PTR(pElement, pTextElement, CTextElement)
        if (!EnvelopesTouched(env, elementextent) && !pTextElement.Assigned())
        {
            continue;
        }

        if (pElement->SelectTest(env, partialselect))
        {
            resultids.push_back(it->first);
        }
    }

    return true;
}

bool __stdcall CElementLayer::SetUndoPoint()
{
    long listsize = m_UndoProcs.size();
    for (long i = m_UndoIndex + 1; i < listsize; i++)
    {
        m_UndoProcs.pop_back();
    }

    EditProcess ep;
    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        dword id = it->first;
        IElementPtr pElement = it->second;
        ep.elements[id] = pElement;
        it++;
    }

    m_UndoProcs.push_back(ep);
    m_UndoIndex++;
    m_SaveDirty = true;
    return true;
}

bool __stdcall CElementLayer::EditUndoable() const
{
    return (0 < m_UndoIndex) ? true : false;
}

bool __stdcall CElementLayer::EditRedoable() const
{
    long ls = m_UndoProcs.size() - 1;
    return (ls > m_UndoIndex) ? true : false;
}

bool __stdcall CElementLayer::EditUndo()
{
    if (!this->EditUndoable()) {return false;}

    m_Elements.clear();
    EditProcess ep = m_UndoProcs[--m_UndoIndex];
    map<dword, IElementPtr>::const_iterator it = ep.elements.begin();
    while (it != ep.elements.end())
    {
        dword id = it->first;
        IElementPtr pElement = it->second;
        m_Elements[id] = pElement;
        it++;
    }

    return true;
}

bool __stdcall CElementLayer::EditRedo()
{
    if (!this->EditRedoable()) {return false;}

    m_Elements.clear();
    EditProcess ep = m_UndoProcs[++m_UndoIndex];
    map<dword, IElementPtr>::const_iterator it = ep.elements.begin();
    while (it != ep.elements.end())
    {
        dword id = it->first;
        IElementPtr pElement = it->second;
        m_Elements[id] = pElement;
        it++;
    }

    return true;
}

bool __stdcall CElementLayer::EditCancel()
{
    if (m_UndoProcs.size() <= 0) {return false;}
    if (!m_SaveDirty) {return false;}

    m_Elements.clear();
    EditProcess ep = m_UndoProcs[0];
    map<dword, IElementPtr>::const_iterator it = ep.elements.begin();
    while (it != ep.elements.end())
    {
        dword id = it->first;
        IElementPtr pElement = it->second;
        m_Elements[id] = pElement;
        it++;
    }

    long listsize = m_UndoProcs.size();
    for (long i = 1; i < listsize; i++)
    {
        m_UndoProcs.pop_back();
    }

    m_UndoIndex = 0;
    m_SaveDirty = false;
    return true;
}

bool __stdcall CElementLayer::SaveData()
{
    if (!m_SaveDirty) {return false;}
    this->SetUndoPoint();
    m_UndoProcs.clear();
    EditProcess ep;
    map<dword, IElementPtr>::const_iterator it = m_Elements.begin();
    while (it != m_Elements.end())
    {
        dword id = it->first;
        IElementPtr pElement = it->second;
        ep.elements[id] = pElement;
        it++;
    }
    m_UndoProcs.push_back(ep);
    m_UndoIndex = 0;
    m_SaveDirty = false;
    return true;
}

bool __stdcall CElementLayer::IsDirty() const
{
//    if (!this->EditUndoable()) {return false;}
//    return m_SaveDirty;
    return false;
}


CElement::CElement()
{
    m_Text = "Element";
    m_ReferenceScale = 0;
    m_Selected = false;
}

CElement::~CElement()
{
}

DrawResult CElement::Draw(const CDisplayPtr pDisplay) const
{
    if (!pDisplay.Assigned())
    {
        return LAYERDRAW_NOREADY;
    }

    CDisplayCachePtr pCache;
    CAST_PTR(pDisplay, pCache, CDisplayCache)
    if (!pCache.Assigned())
    {
        return LAYERDRAW_NOREADY;
    }

    COLORREF bgcolor = pDisplay->GetBackgroundColor();
    long cacheid = pCache->CreateCache(bgcolor);
    pCache->CopyPrimaryToCache(cacheid, 0, 0);
    DrawResult r = this->Draw(pCache, cacheid, NULL, NULL);
    pCache->PostCacheToPrimary(cacheid);
    pCache->DeleteCache(cacheid);
    return r;
}

bool __stdcall CElement::Valid() const
{
    return m_pGeometry.Assigned() ? true : false;
}

bool __stdcall CElement::Move(const double delta_x, const double delta_y)
{
    if (!this->Valid()) {return false;}
    return m_pGeometry->Move(delta_x, delta_y);
}

bool __stdcall CElement::GetExtent(WKSRect& extent) const
{
    if (!m_pGeometry.Assigned()) {return false;}
    m_pGeometry->GetMBR(extent);
    return true;
}

bool __stdcall CElement::SelectTest(const WKSRect& envelope, const bool partialselect) const
{
    if (!m_pGeometry.Assigned()) {return false;}
    return m_pGeometry->Select(envelope, partialselect);
}

void __stdcall CElement::Select()
{
    m_Selected = true;
}

void __stdcall CElement::Deselect()
{
    m_Selected = false;
}

bool __stdcall CElement::IsSelected() const
{
    return m_Selected;
}

bool CElement::SetGeometry(const IGeometryPtr pGeometry)
{
    if (!pGeometry.Assigned()) {return false;}

    GeometryType geotype = pGeometry->GetGeometryType();
    switch (geotype)
    {
    case GEOMETRYTYPE_MULTIPOINT:
    case GEOMETRYTYPE_POINT:
    case GEOMETRYTYPE_POLYLINE:
    case GEOMETRYTYPE_POLYGON:
    case GEOMETRYTYPE_ENVELOPE:
    case GEOMETRYTYPE_CIRCLE:
    case GEOMETRYTYPE_ELLIPSE:
        break;

    default:
        return false;
    }

    IObjPtr pObj;
    CLONE_PTR(pGeometry, pObj)
    CAST_PTR(pObj, m_pGeometry, IGeometry);
    return true;
}

void CElement::GetGeometry(IGeometryPtr &pGeometry)
{
    pGeometry.Clear();
    if (!m_pGeometry.Assigned()) {return;}

    IObjPtr pObj;
    CLONE_PTR(m_pGeometry, pObj)
    CAST_PTR(pObj, pGeometry, IGeometry);
}

void __stdcall CElement::SetText(const char* const text)
{
    m_Text = text;
}

const char* __stdcall CElement::GetText() const
{
    return m_Text.c_str();
}

void __stdcall CElement::SetReferenceScale(const double refscale)
{
    m_ReferenceScale = refscale;
}

void __stdcall CElement::GetReferenceScale(double& refscale) const
{
    refscale = m_ReferenceScale;
}

DrawResult __stdcall CElement::Draw(const IDisplayCache* pDisplayCache, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel) const
{
    if (_invalid(pDisplayCache)) return LAYERDRAW_NOCACHE;
    CDisplayCachePtr pDC = (CDisplayCache*)pDisplayCache;
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->Draw(pDC, cacheid, pEnvelope, pTC);
}

DrawResult __stdcall CElement::DrawSelected(const IDisplayCache* pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel) const
{
    if (_invalid(pDisplayCache)) return LAYERDRAW_NOCACHE;
    CDisplayCachePtr pDC = (CDisplayCache*)pDisplayCache;
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->DrawSelected(pDC, cacheid, pEnvelope, pTC);
}

DrawResult __stdcall CElement::Draw1(const IDisplay* pDisplay) const
{
    if (_invalid(pDisplay)) return LAYERDRAW_UNEXPECTED;
    CDisplayPtr pD = (CDisplay*)pDisplay;
    return this->Draw(pD);
}

bool __stdcall CElement::DrawEx(const HDC dc, const IDisplayTransformation* pTrans) const
{
    if (_invalid(pTrans)) return false;
    CDisplayTransformationPtr pT = (CDisplayTransformation*)pTrans;
    return this->DrawEx(dc, pT);
}

bool __stdcall CElement::DrawSelectedEx(const HDC dc, const IDisplayTransformation* pTrans) const
{
    if (_invalid(pTrans)) return false;
    CDisplayTransformationPtr pT = (CDisplayTransformation*)pTrans;
    return this->DrawSelectedEx(dc, pT);
}

bool __stdcall CElement::SetGeometry(const IGeometry* pGeometry)
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pG = (IGeometry*)pGeometry;
    return this->SetGeometry(pG);
}

void __stdcall CElement::GetGeometry(IGeometry** ppGeometry)
{
    if (_invalid(ppGeometry)) return;
    IGeometryPtr pG;
    this->GetGeometry(pG);
    if (pG.Assigned())
    {
        *ppGeometry = (IGeometry*)pG._p();
        (*ppGeometry)->_AddRef();
    }
}


CGeometryElement::CGeometryElement()
{
    INIT_REFCOUNT

    this->Init();
}

CGeometryElement::~CGeometryElement()
{
}

bool __stdcall CGeometryElement::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IElement"))
        || (0 == strcmp(interfacename, "CElement"))
        || (0 == strcmp(interfacename, "CGeometryElement")))
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

dword __stdcall CGeometryElement::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_pGeometry->_DumpTo(pStream, assist);
    ps->Write(m_Text);
    ps->Write(m_ReferenceScale);

    m_pPointSymbol->_DumpTo(pStream, assist);
    m_pLineSymbol->_DumpTo(pStream, assist);
    m_pFillSymbol->_DumpTo(pStream, assist);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CGeometryElement::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    CPersistPtr pPersist;
    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pGeometry, IGeometry)

    ps->Read(m_Text);
    ps->Read(m_ReferenceScale);

    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pPointSymbol, IPointSymbol)
    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pLineSymbol, ILineSymbol)
    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pFillSymbol, IFillSymbol)

    return pStream->GetPos() - oldpos;
}

void CGeometryElement::Init()
{
    m_pGeometry.Clear();
    m_Text = "Shape";
    m_ReferenceScale = 0;
    m_Selected = false;

    IMultiPointSymbolPtr pMPS = new CMultiPointSymbol;
    pMPS->AddSimpleSymbol(RGB(180, 58, 113), 3.5);
    CAST_PTR(pMPS, m_pPointSymbol, IPointSymbol)

    IMultiLineSymbolPtr pMLS = new CMultiLineSymbol;
    pMLS->AddSimpleSymbol(RGB(180, 58, 113), 1);
    CAST_PTR(pMLS, m_pLineSymbol, ILineSymbol)

    CSimpleFillSymbolPtr pFillSymbol = new CSimpleFillSymbol;
    pFillSymbol->SetColor(RGB(110, 158, 180));
    pFillSymbol->SetFillStyle(BS_HATCHED);
    pFillSymbol->SetFillHatch(HS_DIAGCROSS);
    pFillSymbol->SetBorderColor(RGB(210, 58, 33));
    pFillSymbol->SetBorderWidth(0.8);
    IMultiFillSymbolPtr pMFS = new CMultiFillSymbol;
    pMFS->AddSymbol((IFillSymbol*)(pFillSymbol._p()));
    CAST_PTR(pMFS, m_pFillSymbol, IFillSymbol)
}

inline void TrackEnvelope(const HDC dc, const int penstyle, const COLORREF color,
    const long rop2, const long pensize, const long left, const long top,
    const long right, const long bottom)
{
    HPEN pen = ::CreatePen(penstyle, pensize, color);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    long rop2saved = ::SetROP2(dc, rop2);
    ::MoveToEx(dc, right, top, NULL);
    ::LineTo(dc, left, top);
    ::LineTo(dc, left, bottom);
    ::LineTo(dc, right, bottom);
    ::LineTo(dc, right, top);

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
};

inline void TrackSelectedEnvelope(const HDC dc, const tagRECT& rect)
{
    long ps = PS_DASH;
    if ((abs(rect.left - rect.right) < 20) || (abs(rect.bottom - rect.top) < 20))
    {
        ps = PS_SOLID;
    }

    TrackEnvelope(dc, ps, RGB(255, 120, 85), R2_COPYPEN, 1, rect.left, rect.top,
        rect.right, rect.bottom);
}

DrawResult CGeometryElement::Draw(const CDisplayCachePtr pDisplayCache, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel) const
{
    DrawResult r = LAYERDRAW_NORMAL;

    CDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, CDisplay)

    ISymbolPtr pSymbol;
    GeometryType geotype = m_pGeometry->GetGeometryType();
    switch (geotype)
    {
    case GEOMETRYTYPE_POINT:
    case GEOMETRYTYPE_MULTIPOINT:
        CAST_PTR(m_pPointSymbol, pSymbol, ISymbol)
        break;

    case GEOMETRYTYPE_POLYLINE:
        CAST_PTR(m_pLineSymbol, pSymbol, ISymbol)
        break;

    case GEOMETRYTYPE_POLYGON:
    case GEOMETRYTYPE_ENVELOPE:
    case GEOMETRYTYPE_CIRCLE:
    case GEOMETRYTYPE_ELLIPSE:
        CAST_PTR(m_pFillSymbol, pSymbol, ISymbol)
        break;

    default:
        {
            return LAYERDRAW_NOREADY;
        }
    }
    pDisplayCache->SetCacheSymbol(cacheid, pSymbol);
    if (pDisplay->IsDrawing())
    {
        return LAYERDRAW_DISPLAYNOREADY;
    }

    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);
    double oldrefscale;
    pTrans->GetReferenceScale(oldrefscale);
    pTrans->SetReferenceScale(m_ReferenceScale);
    pDisplay->StartDraw();
    pDisplayCache->DrawCacheGeometry(cacheid, m_pGeometry);
    pDisplay->FinishDraw();
    pTrans->SetReferenceScale(oldrefscale);
    return r;
}

inline void TrackNode(const HDC dc, const long x, const long y)
{
    LOGBRUSH logbrush;
    logbrush.lbColor = RGB(0, 0, 255);
    logbrush.lbStyle = BS_SOLID;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    int rop2saved = ::SetROP2(dc, R2_COPYPEN);

    tagRECT node = {x - 5, y - 5, x + 5, y + 5};
    ::FillRect(dc, &node, brush);

    ::DeleteObject(brush);
    ::SetROP2(dc, rop2saved);
}

inline void TrackSelectedPointNode(const HDC dc, const CDisplayTransformationPtr pTrans,
    const IGeometryPtr pGeometry)
{
    CPointPtr pPoint;
    CAST_PTR(pGeometry, pPoint, CPoint)
    if (!pPoint.Assigned()) {return;}

    double x, y, z;
    pPoint->GetCoordinates(x, y, z);
    long X, Y;
    pTrans->Map2DeviceXY(x, y, X, Y);
    TrackNode(dc, X, Y);
}

inline void TrackSelectedMultiPointNode(const HDC dc, const CDisplayTransformationPtr pTrans,
    const IGeometryPtr pGeometry)
{
    CMultiPointPtr pMultiPoint;
    CAST_PTR(pGeometry, pMultiPoint, CMultiPoint)
    if (!pMultiPoint.Assigned()) {return;}

    long pointcount = pMultiPoint->GetPointCount();
    for (long i = 0; i < pointcount; i++)
    {
        WKSPointZ point;
        pMultiPoint->GetPoint(point, i);
        long X, Y;
        pTrans->Map2DeviceXY(point.x, point.y, X, Y);
        TrackNode(dc, X, Y);
    }
}

inline void TrackSelectedEnvelopeNode(const HDC dc, const CDisplayTransformationPtr pTrans,
    const IGeometryPtr pGeometry)
{
    WKSRect mbr;
    pGeometry->GetMBR(mbr);

    tagRECT rect;
    pTrans->Map2DeviceXY(mbr.left, mbr.bottom, rect.left, rect.bottom);
    pTrans->Map2DeviceXY(mbr.right, mbr.top, rect.right, rect.top);

    long x = rect.left;
    long y = rect.top;
    TrackNode(dc, x, y);
    x = rect.right;
    y = rect.top;
    TrackNode(dc, x, y);
    x = rect.right;
    y = rect.bottom;
    TrackNode(dc, x, y);
    x = rect.left;
    y = rect.bottom;
    TrackNode(dc, x, y);
}

inline void TrackSelectedCircleNode(const HDC dc, const CDisplayTransformationPtr pTrans,
    const IGeometryPtr pGeometry)
{
    CCirclePtr pCircle;
    CAST_PTR(pGeometry, pCircle, CCircle)
    if (!pCircle.Assigned()) {return;}

    WKSPointZ center;
    pCircle->GetCenter(center);
    double radius;
    pCircle->GetRadius(radius);

    POINT point1, point2, point3, point4;
    pTrans->Map2DeviceXY(center.x - radius, center.y, point1.x, point1.y);
    pTrans->Map2DeviceXY(center.x, center.y - radius, point2.x, point2.y);
    pTrans->Map2DeviceXY(center.x + radius, center.y, point3.x, point3.y);
    pTrans->Map2DeviceXY(center.x, center.y + radius, point4.x, point4.y);

    TrackNode(dc, point1.x, point1.y);
    TrackNode(dc, point2.x, point2.y);
    TrackNode(dc, point3.x, point3.y);
    TrackNode(dc, point4.x, point4.y);
}

inline void TrackSelectedPathNode(const HDC dc,
    const CDisplayTransformationPtr pTrans, const CPathPtr pPath)
{
    long pointcount = pPath->GetPointCount();
    for (long i = 0; i < pointcount; i++)
    {
        WKSPointZ point;
        pPath->GetPoint1(i, point);
        long X, Y;
        pTrans->Map2DeviceXY(point.x, point.y, X, Y);
        TrackNode(dc, X, Y);
    }
}

inline void TrackSelectedRingNode(const HDC dc,
    const CDisplayTransformationPtr pTrans, const CRingPtr pRing)
{
    long pointcount = pRing->GetPointCount();
    for (long i = 0; i < pointcount; i++)
    {
        WKSPointZ point;
        pRing->GetPoint1(i, point);
        long X, Y;
        pTrans->Map2DeviceXY(point.x, point.y, X, Y);
        TrackNode(dc, X, Y);
    }
}

inline void TrackSelectedPolylineNode(const HDC dc,
    const CDisplayTransformationPtr pTrans, const IGeometryPtr pGeometry)
{
    CPolylinePtr pPolyline;
    CAST_PTR(pGeometry, pPolyline, CPolyline)
    long geocount = pPolyline->GetPathCount();
    for (long i = 0; i < geocount; i++)
    {
        CPathPtr pPath;
        pPolyline->GetPathRef(pPath, i);
        TrackSelectedPathNode(dc, pTrans, pPath);
    }
}

inline void TrackSelectedPolygonNode(const HDC dc,
    const CDisplayTransformationPtr pTrans, const IGeometryPtr pGeometry)
{
    CPolygonPtr pPolygon;
    CAST_PTR(pGeometry, pPolygon, CPolygon)
    long geocount = pPolygon->GetRingCount();
    for (long i = 0; i < geocount; i++)
    {
        CRingPtr pRing;
        pPolygon->GetRingRef(pRing, i);
        TrackSelectedRingNode(dc, pTrans, pRing);
    }
}

inline void TrackSelectedGeometryNode(const HDC dc, const CDisplayTransformationPtr pTrans,
    const IGeometryPtr pGeometry)
{
    GeometryType geotype = pGeometry->GetGeometryType();
    switch (geotype)
    {
    case GEOMETRYTYPE_POINT:
        TrackSelectedPointNode(dc, pTrans, pGeometry);
        break;

    case GEOMETRYTYPE_MULTIPOINT:
        TrackSelectedMultiPointNode(dc, pTrans, pGeometry);
        break;

    case GEOMETRYTYPE_POLYLINE:
        TrackSelectedPolylineNode(dc, pTrans, pGeometry);
        break;

    case GEOMETRYTYPE_POLYGON:
        TrackSelectedPolygonNode(dc, pTrans, pGeometry);
        break;

    case GEOMETRYTYPE_ENVELOPE:
    case GEOMETRYTYPE_ELLIPSE:
        TrackSelectedEnvelopeNode(dc, pTrans, pGeometry);
        break;

    case GEOMETRYTYPE_CIRCLE:
        TrackSelectedCircleNode(dc, pTrans, pGeometry);
        break;

    default:
        {}
    }
}

DrawResult CGeometryElement::DrawSelected(const CDisplayCachePtr pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel) const
{
    DrawResult r = LAYERDRAW_NORMAL;

    CDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, CDisplay)
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);

    if (m_Selected)
    {
        if (pDisplay->IsDrawing())
        {
            return LAYERDRAW_DISPLAYNOREADY;
        }

        pDisplay->StartDraw();

        WKSRect mbr;
        m_pGeometry->GetMBR(mbr);
        tagRECT rect;
        pTrans->Map2DeviceXY(mbr.left, mbr.bottom, rect.left, rect.bottom);
        pTrans->Map2DeviceXY(mbr.right, mbr.top, rect.right, rect.top);

        HDC dc;
        pDisplayCache->GetCacheDC(cacheid, dc);
        TrackSelectedEnvelope(dc, rect);
        TrackSelectedGeometryNode(dc, pTrans, m_pGeometry);

        pDisplay->FinishDraw();
    }

    return r;
}

bool CGeometryElement::DrawEx(const HDC dc, const CDisplayTransformationPtr pTrans) const
{
    if (!pTrans.Assigned()) return false;

    ISymbolPtr pSymbol;
    GeometryType geotype = m_pGeometry->GetGeometryType();
    switch (geotype)
    {
    case GEOMETRYTYPE_POINT:
    case GEOMETRYTYPE_MULTIPOINT:
        CAST_PTR(m_pPointSymbol, pSymbol, ISymbol)
        break;

    case GEOMETRYTYPE_POLYLINE:
        CAST_PTR(m_pLineSymbol, pSymbol, ISymbol)
        break;

    case GEOMETRYTYPE_POLYGON:
    case GEOMETRYTYPE_ENVELOPE:
    case GEOMETRYTYPE_CIRCLE:
    case GEOMETRYTYPE_ELLIPSE:
        CAST_PTR(m_pFillSymbol, pSymbol, ISymbol)
        break;

    default:
        {
            return false;
        }
    }

    double oldrefscale;
    pTrans->GetReferenceScale(oldrefscale);
    pTrans->SetReferenceScale(m_ReferenceScale);
    pSymbol->Prepare(dc, pTrans._p());
    bool r = pSymbol->Draw(m_pGeometry._p());
    pTrans->SetReferenceScale(oldrefscale);
    return r;
}

bool CGeometryElement::DrawSelectedEx(const HDC dc, const CDisplayTransformationPtr pTrans) const
{
    if (!pTrans.Assigned()) return false;
    if (!m_Selected) return false;

    WKSRect mbr;
    m_pGeometry->GetMBR(mbr);
    tagRECT rect;
    pTrans->Map2DeviceXY(mbr.left, mbr.bottom, rect.left, rect.bottom);
    pTrans->Map2DeviceXY(mbr.right, mbr.top, rect.right, rect.top);

    TrackSelectedEnvelope(dc, rect);
    TrackSelectedGeometryNode(dc, pTrans, m_pGeometry);

    return true;
}

bool __stdcall CGeometryElement::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CGeometryElement::Clone(IObjPtr& pObj) const
{
    if (!this->Valid()) {return false;}

    CGeometryElementPtr pGeoElement = new CGeometryElement;

    IObjPtr pObjTmp;
    CLONE_PTR(m_pGeometry, pObjTmp)
    CAST_PTR(pObjTmp, pGeoElement->m_pGeometry, IGeometry)

    pGeoElement->m_Text = m_Text;
    pGeoElement->m_ReferenceScale = m_ReferenceScale;
    pGeoElement->m_Selected = m_Selected;

    CLONE_PTR(m_pPointSymbol, pObjTmp)
    CAST_PTR(pObjTmp, pGeoElement->m_pPointSymbol, IPointSymbol)

    CLONE_PTR(m_pLineSymbol, pObjTmp)
    CAST_PTR(pObjTmp, pGeoElement->m_pLineSymbol, ILineSymbol)

    CLONE_PTR(m_pFillSymbol, pObjTmp)
    CAST_PTR(pObjTmp, pGeoElement->m_pFillSymbol, IFillSymbol)

    CAST_PTR(pGeoElement, pObj, IObj)

    return true;
}

ElementType __stdcall CGeometryElement::GetElementType() const
{
    return ELEMENTTYPE_GEOMETRY;
}

bool __stdcall CGeometryElement::SetSymbol(const ISymbol* const pSymbol)
{
    if (_invalid(pSymbol)) return false;
    ISymbolPtr pS = (ISymbol*)pSymbol;
    return this->SetSymbol(pS);
}

bool __stdcall CGeometryElement::GetSymbol(const SymbolType symboltype, ISymbol** ppSymbol) const
{
    if (_invalid(ppSymbol)) return false;
    *ppSymbol = NULL;
    ISymbolPtr pS;
    bool r = this->GetSymbol(symboltype, pS);
    if (pS.Assigned())
    {
        *ppSymbol = (ISymbol*)pS._p();
        (*ppSymbol)->_AddRef();
    }
    return r;
}

bool CGeometryElement::SetSymbol(const ISymbolPtr pSymbol)
{
    if (!pSymbol.Assigned()) return false;

    IObjPtr pObj;
    CLONE_PTR(pSymbol, pObj)
    SymbolType symboltype = pSymbol->GetSymbolType();
    switch (symboltype)
    {
    case SYMBOLTYPE_POINT:
        CAST_PTR(pObj, m_pPointSymbol, IPointSymbol)
        break;

    case SYMBOLTYPE_LINE:
        CAST_PTR(pObj, m_pLineSymbol, ILineSymbol)
        break;

    case SYMBOLTYPE_FILL:
        CAST_PTR(pObj, m_pFillSymbol, IFillSymbol)
        break;

    default:
        return false;
    }
    
    return true;
}

bool CGeometryElement::GetSymbol(const SymbolType symboltype, ISymbolPtr& pSymbol) const
{
    pSymbol.Clear();
    IObjPtr pObj;

    switch (symboltype)
    {
    case SYMBOLTYPE_POINT:
        CLONE_PTR(m_pPointSymbol, pObj)
        break;

    case SYMBOLTYPE_LINE:
        CLONE_PTR(m_pLineSymbol, pObj)
        break;

    case SYMBOLTYPE_FILL:
        CLONE_PTR(m_pFillSymbol, pObj)
        break;

    default:
        return false;
    }

    CAST_PTR(pObj, pSymbol, ISymbol)
    return true;
}

CTextElement::CTextElement()
{
    INIT_REFCOUNT

    this->Init();
}

CTextElement::~CTextElement()
{
}

bool __stdcall CTextElement::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IElement"))
        || (0 == strcmp(interfacename, "CElement"))
        || (0 == strcmp(interfacename, "CTextElement")))
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

dword __stdcall CTextElement::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_pGeometry->_DumpTo(pStream, assist);
    ps->Write(m_Text);
    ps->Write(m_ReferenceScale);

    m_pTextSymbol->_DumpTo(pStream, assist);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CTextElement::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    CPersistPtr pPersist;
    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pGeometry, IGeometry)

    ps->Read(m_Text);
    ps->Read(m_ReferenceScale);

    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pTextSymbol, ITextSymbol)

    m_TextExtOK = false;
    return pStream->GetPos() - oldpos;
}

void CTextElement::Init()
{
    m_pGeometry.Clear();
    m_Text = "Text";
    m_ReferenceScale = 0;
    m_Selected = false;
    m_TextExtOK = false;

    CSimpleTextSymbolPtr pTextSymbol = new CSimpleTextSymbol();
    pTextSymbol->SetColor(RGB(30, 65, 230));
    CAST_PTR(pTextSymbol, m_pTextSymbol, ITextSymbol)
}

DrawResult CTextElement::Draw(const CDisplayCachePtr pDisplayCache, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel) const
{
    DrawResult r = LAYERDRAW_NORMAL;

    if (!m_pTextSymbol.Assigned())
    {
        CSimpleTextSymbolPtr pTextSymbol = new CSimpleTextSymbol();
        pTextSymbol->SetColor(RGB(30, 65, 230));
    }

    CDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, CDisplay)
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);

    ISymbolPtr pSymbol;
    CAST_PTR(m_pTextSymbol, pSymbol, ISymbol)
    pDisplayCache->SetCacheSymbol(cacheid, pSymbol);
    if (pDisplay->IsDrawing())
    {
        return LAYERDRAW_DISPLAYNOREADY;
    }

    double oldrefscale;
    pTrans->GetReferenceScale(oldrefscale);
    pTrans->SetReferenceScale(m_ReferenceScale);
    pDisplay->StartDraw();
    RECT rect;
    pDisplayCache->DrawCacheText(cacheid, m_pGeometry, m_Text.c_str(), rect);
    pTrans->Device2MapXY(rect.left, rect.bottom,
        const_cast<CTextElement*>(this)->m_TextExt.left,
        const_cast<CTextElement*>(this)->m_TextExt.bottom);
    pTrans->Device2MapXY(rect.right, rect.top,
        const_cast<CTextElement*>(this)->m_TextExt.right,
        const_cast<CTextElement*>(this)->m_TextExt.top);
    const_cast<CTextElement*>(this)->m_TextExtOK = true;
    pDisplay->FinishDraw();
    pTrans->SetReferenceScale(oldrefscale);
    return r;
}

DrawResult CTextElement::DrawSelected(const CDisplayCachePtr pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel) const
{
    DrawResult r = LAYERDRAW_NORMAL;

    CDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, CDisplay)
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);

    if (m_Selected)
    {
        ISymbolPtr pSymbol;
        CAST_PTR(m_pTextSymbol, pSymbol, ISymbol)
        pDisplayCache->SetCacheSymbol(cacheid, pSymbol);
        if (pDisplay->IsDrawing())
        {
            return LAYERDRAW_DISPLAYNOREADY;
        }

        WKSRect mbr;
        m_pGeometry->GetMBR(mbr);
        CPointPtr pPoint = new CPoint();
        pPoint->SetX(mbr.left);
        pPoint->SetY(mbr.top);

        double oldrefscale;
        pTrans->GetReferenceScale(oldrefscale);
        pTrans->SetReferenceScale(m_ReferenceScale);
        pDisplay->StartDraw();

        RECT rect;
        pDisplayCache->DrawCacheText(cacheid, m_pGeometry, m_Text.c_str(), rect);
        rect.left -= 5;
        rect.right += 5;
        rect.top -= 2;
        rect.bottom += 2;
        HDC dc;
        pDisplayCache->GetCacheDC(cacheid, dc);
        TrackSelectedEnvelope(dc, rect);

        pDisplay->FinishDraw();
        pTrans->SetReferenceScale(oldrefscale);
    }

    return r;
}

bool CTextElement::DrawEx(const HDC dc, const CDisplayTransformationPtr pTrans) const
{
    if (!pTrans.Assigned()) return false;

    if (!m_pTextSymbol.Assigned())
    {
        CSimpleTextSymbolPtr pTextSymbol = new CSimpleTextSymbol();
        pTextSymbol->SetColor(RGB(30, 65, 230));
    }

    ISymbolPtr pSymbol;
    CAST_PTR(m_pTextSymbol, pSymbol, ISymbol)

    double oldrefscale;
    pTrans->GetReferenceScale(oldrefscale);
    pTrans->SetReferenceScale(m_ReferenceScale);
    pSymbol->Prepare(dc, pTrans._p());
    bool r = pSymbol->Draw(m_pGeometry._p());
    pTrans->SetReferenceScale(oldrefscale);
    return r;
}

bool CTextElement::DrawSelectedEx(const HDC dc, const CDisplayTransformationPtr pTrans) const
{
    if (!pTrans.Assigned()) return false;
    if (!m_Selected) return false;

    WKSRect mbr;
    m_pGeometry->GetMBR(mbr);
    tagRECT rect;
    pTrans->Map2DeviceXY(mbr.left, mbr.bottom, rect.left, rect.bottom);
    pTrans->Map2DeviceXY(mbr.right, mbr.top, rect.right, rect.top);

    TrackSelectedEnvelope(dc, rect);
    TrackSelectedGeometryNode(dc, pTrans, m_pGeometry);

    return true;
}

ElementType __stdcall CTextElement::GetElementType() const
{
    return ELEMENTTYPE_TEXT;
}

bool __stdcall CTextElement::Move(const double& delta_x, const double& delta_y)
{
    if (!CElement::Move(delta_x, delta_y)) {return false;}

    if (m_TextExtOK)
    {
        m_TextExt.left += delta_x;
        m_TextExt.right += delta_x;
        m_TextExt.top += delta_y;
        m_TextExt.bottom += delta_y;
    }

    return true;
}

bool __stdcall CTextElement::GetExtent(WKSRect& extent) const
{
    if (m_TextExtOK)
    {
        extent = m_TextExt;
        return true;
    }
    else
    {
        return CElement::GetExtent(extent);
    }
}

bool __stdcall CTextElement::SelectTest(const WKSRect& envelope, const bool partialselect) const
{
    if (!this->Valid()) {return false;}

    if (m_TextExtOK)
    {
        if (partialselect)
        {
            return EnvelopesTouched(envelope, m_TextExt);
        }
        else
        {
            return EnvelopesContented(envelope, m_TextExt);
        }
    }
    else
    {
        return CElement::SelectTest(envelope, partialselect);
    }
}

bool __stdcall CTextElement::SetSymbol(const ISymbol* const pSymbol)
{
    if (_invalid(pSymbol)) return false;
    ISymbolPtr pS = (ISymbol*)pSymbol;
    ITextSymbolPtr pTextSymbol;
    CAST_PTR(pS, pTextSymbol, ITextSymbol)
    if (!pTextSymbol.Assigned()) return false; 
    this->SetTextSymbol(pTextSymbol);
    return true;
}

bool __stdcall CTextElement::GetSymbol(const SymbolType symboltype, ISymbol** ppSymbol) const
{
    if (_invalid(ppSymbol)) return false;
    *ppSymbol = NULL;
    ITextSymbolPtr pT;
    bool r = this->GetTextSymbol(pT);
    if (pT.Assigned())
    {
        ISymbolPtr pS;
        CAST_PTR(pT, pS, ISymbol)
        *ppSymbol = pS._p();
        (*ppSymbol)->_AddRef();
    }
    return r;
}

void CTextElement::SetTextSymbol(const ITextSymbolPtr pTextSymbol)
{
    if (!pTextSymbol.Assigned()) return;
    IObjPtr pObj;
    CLONE_PTR(pTextSymbol, pObj)
    CAST_PTR(pObj, m_pTextSymbol, ITextSymbol)
}

bool CTextElement::GetTextSymbol(ITextSymbolPtr& pTextSymbol) const
{
    if (!this->Valid()) {return false;}

    IObjPtr pObj;
    CLONE_PTR(m_pTextSymbol, pObj)
    CAST_PTR(pObj, pTextSymbol, ITextSymbol)
    return true;
}

bool CTextElement::GetTextExtent(WKSRect& extent) const
{
    if (!this->Valid()) {return false;}

    if (m_TextExtOK)
    {
        extent = m_TextExt;
        return true;
    }
    else
    {
        m_pGeometry->GetMBR(extent);
        return false;
    }
}

bool __stdcall CTextElement::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = (IObj*)po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CTextElement::Clone(IObjPtr& pObj) const
{
    if (!this->Valid()) {return false;}
    
    CTextElementPtr pTextElement = new CTextElement;

    IObjPtr pObjTmp;
    CLONE_PTR(m_pGeometry, pObjTmp)
    CAST_PTR(pObjTmp, pTextElement->m_pGeometry, IGeometry)

    pTextElement->m_Text = m_Text;
    pTextElement->m_ReferenceScale = m_ReferenceScale;
    pTextElement->m_Selected = m_Selected;

    pObjTmp.Clear();
    CLONE_PTR(m_pTextSymbol, pObjTmp)
    CAST_PTR(pObjTmp, pTextElement->m_pTextSymbol, ITextSymbol)

    pTextElement->m_TextExt = m_TextExt;
    pTextElement->m_TextExtOK = m_TextExtOK;

    CAST_PTR(pTextElement, pObj, IObj)

    return true;
}

}