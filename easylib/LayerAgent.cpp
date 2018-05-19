#include "CommonInclude.h"
#include "LayerAgent.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CVectorLayerAgent)
CLASS_FACTORY_INSTANCE(CElementLayerAgent)

CVectorLayerAgent::CVectorLayerAgent()
{
    INIT_REFCOUNT
}

CVectorLayerAgent::~CVectorLayerAgent()
{
}

bool __stdcall CVectorLayerAgent::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IVectorLayerAgent"))
        || (0 == strcmp(interfacename, "CVectorLayerAgent")))
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

bool __stdcall CVectorLayerAgent::Clone(IObj** ppObj) const
{
    return false;
}

bool __stdcall CVectorLayerAgent::SetLayer(const ILayer* pLayer)
{
    m_pVectorLayer.Clear();
    if (_invalid(pLayer))
    {
        return true;
    }

    ILayerPtr pL = (ILayer*)pLayer;
    CAST_PTR(pL, m_pVectorLayer, CVectorLayer)
    if (!m_pVectorLayer.Assigned())
    {
        return false;
    }

    return true;
}

bool __stdcall CVectorLayerAgent::GetLayer(ILayer** ppLayer) const
{
    if (_invalid(ppLayer)) return false;
    assert(!*ppLayer);
    *ppLayer = NULL;

    if (m_pVectorLayer.Assigned())
    {
        m_pVectorLayer->GotoInterface("ILayer", (void**)(ppLayer));
    }

    return true;
}

bool __stdcall CVectorLayerAgent::CreateSlimLayer(const ShapeType shapetype,
    const MapUnits mapunit, const double basescale, const double precision,
    const WKSRect& extent, const long indexlevel, const IFields* pFields,
    const bool annotation, const char* const filename, const bool filemap)
{
    m_pVectorLayer.Clear();

    CFieldsPtr pF = (CFields*)pFields;
    CSlimLayerPtr pSlimLayer = new CSlimLayer(shapetype, mapunit, basescale, precision,
        extent, indexlevel, pF, annotation);
    if (!pSlimLayer->Valid())
    {
        return false;
    }

    string sfilename;
    if (_valid(filename))
        sfilename = Trim(string(filename));

    if (sfilename != "")
    {
        if (!pSlimLayer->AttachToFile(sfilename, filemap))
        {
            return false;
        }
    }

    CAST_PTR(pSlimLayer, m_pVectorLayer, CVectorLayer);
    return true;
}

bool __stdcall CVectorLayerAgent::LoadSlimLayer(const char* const filename,
    const bool readonly, const bool filemap)
{
    m_pVectorLayer.Clear();
    if (_invalid(filename))
        return false;

    CSlimLayerPtr pSlimLayer = new CSlimLayer(filename, readonly, filemap);
    if (!pSlimLayer->Valid())
    {
        return false;
    }

    CAST_PTR(pSlimLayer, m_pVectorLayer, CVectorLayer)
    return true;
}

bool __stdcall CVectorLayerAgent::LoadShapeLayer(const char* const filename,
    const MapUnits mapunit, const double basescale, const double precision,
    const long indexlevel, const bool readonly)
{
    m_pVectorLayer.Clear();
    if (_invalid(filename))
        return false;

    CShapeLayerPtr pShapeLayer = new CShapeLayer(filename, mapunit, basescale,
        precision, indexlevel, readonly);
    if (!pShapeLayer->Valid())
    {
        return false;
    }

    CAST_PTR(pShapeLayer, m_pVectorLayer, CVectorLayer)
    return true;
}

void __stdcall CVectorLayerAgent::GetPrecision(double& precision) const
{
    m_pVectorLayer->GetPrecision(precision);
}

void __stdcall CVectorLayerAgent::SetRefScale(const double scale)
{
    m_pVectorLayer->SetRefScale(scale);
}

void __stdcall CVectorLayerAgent::GetRefScale(double& scale) const
{
    m_pVectorLayer->GetRefScale(scale);
}

dword __stdcall CVectorLayerAgent::Select(const IIntArray* pFids, const bool append)
{
    if (_invalid(pFids)) return 0;

    vector<dword> fidsv;
    dword count = pFids->GetSize();
    for (dword i = 0; i < count; i++)
    {
        long fid;
        pFids->GetAt(i, fid);
        fidsv.push_back(fid);
    }

    return m_pVectorLayer->Select(fidsv, append);
}

dword __stdcall CVectorLayerAgent::Deselect(const IIntArray* pFids)
{
    if (_invalid(pFids)) return 0;

    vector<dword> fidsv;
    dword count = pFids->GetSize();
    for (dword i = 0; i < count; i++)
    {
        long fid;
        pFids->GetAt(i, fid);
        fidsv.push_back(fid);
    }

    return m_pVectorLayer->Deselect(fidsv);
}

dword __stdcall CVectorLayerAgent::GetSelection(IIntArray** ppFids) const
{
    if (_invalid(ppFids)) return 0;
    assert(!*ppFids);
    *ppFids = NULL;

    vector<dword> fids;
    dword r = m_pVectorLayer->GetSelection(fids);
    *ppFids = new CIntArray;
    (*ppFids)->_AddRef();
    vector<dword>::const_iterator it = fids.begin();
    while (it != fids.end())
    {
        (*ppFids)->Add(*it);
        it++;
    }

    return r;
}

bool __stdcall CVectorLayerAgent::GetFids(IIntArray** ppFids) const
{
    if (_invalid(ppFids)) return false;
    assert(!*ppFids);
    *ppFids = NULL;

    vector<dword> fids;
    bool r = m_pVectorLayer->GetFids(fids);
    *ppFids = new CIntArray;
    (*ppFids)->_AddRef();
    vector<dword>::const_iterator it = fids.begin();
    while (it != fids.end())
    {
        (*ppFids)->Add(*it);
        it++;
    }

    return r;
}

dword __stdcall CVectorLayerAgent::GetFeatureCount() const
{
    return m_pVectorLayer->GetFeatureCount();
}

bool __stdcall CVectorLayerAgent::DeleteFeature(const dword fid)
{
    return m_pVectorLayer->DeleteFeature(fid);
}

bool __stdcall CVectorLayerAgent::CreateFeature(IVectorFeature** ppFeature)
{
    if (_invalid(ppFeature)) return false;
    assert(!*ppFeature);
    *ppFeature = NULL;

    IVectorFeaturePtr pVF;
    bool r = m_pVectorLayer->CreateFeature(pVF);
    if (pVF.Assigned())
    {
        *ppFeature = pVF._p();
        (*ppFeature)->_AddRef();
    }

    return r;
}

bool __stdcall CVectorLayerAgent::GetFeature(const dword fid, IVectorFeature** ppFeature)
{
    if (_invalid(ppFeature)) return false;
    assert(!*ppFeature);
    *ppFeature = NULL;

    IVectorFeaturePtr pVF;
    bool r = m_pVectorLayer->GetFeature(fid, pVF);
    if (pVF.Assigned())
    {
        *ppFeature = pVF._p();
        (*ppFeature)->_AddRef();
    }

    return r;
}

bool __stdcall CVectorLayerAgent::RapidModifyPoint(const dword fid, const WKSPoint& point)
{
    return m_pVectorLayer->RapidModifyPoint(fid, point);
}

bool __stdcall CVectorLayerAgent::Identify(IIntArray** ppFids, const WKSRect& envelope,
    const bool partialselect)
{
    if (_invalid(ppFids)) return false;
    assert(!*ppFids);
    *ppFids = NULL;

    vector<dword> fids;
    bool r = m_pVectorLayer->Identify(fids, envelope, partialselect);
    if (r)
    {
        *ppFids = new CIntArray;
        (*ppFids)->_AddRef();
        vector<dword>::const_iterator it = fids.begin();
        while (it != fids.end())
        {
            (*ppFids)->Add(*it);
            it++;
        }
    }

    return r;
}

void __stdcall CVectorLayerAgent::GetGeometryColumnInfo(GeometryColumnInfo& geocolinfo) const
{
    m_pVectorLayer->GetGeometryColumnInfo(geocolinfo);
}

void __stdcall CVectorLayerAgent::GetFields(IFields** ppFields) const
{
    if (_invalid(ppFields)) return;
    assert(!*ppFields);
    *ppFields = NULL;

    CFieldsPtr pFields;
    m_pVectorLayer->GetFields(pFields);
    if (pFields.Assigned())
    {
        *ppFields = pFields._p();
        (*ppFields)->_AddRef();
    }
}

bool __stdcall CVectorLayerAgent::SetDisplayField(const long fieldindex)
{
    return m_pVectorLayer->SetDisplayField(fieldindex);
}

long __stdcall CVectorLayerAgent::GetDisplayField() const
{
    return m_pVectorLayer->GetDisplayField();
}

bool __stdcall CVectorLayerAgent::SetDefaultSymbol(const ISymbol* pSymbol)
{
    if (_invalid(pSymbol)) return false;

    ISymbolPtr pS = (ISymbol*)pSymbol;
    return m_pVectorLayer->SetDefaultSymbol(pS, true);
}

bool __stdcall CVectorLayerAgent::GetDefaultSymbol(ISymbol** ppSymbol) const
{
    if (_invalid(ppSymbol)) return false;
    assert(!*ppSymbol);
    *ppSymbol = NULL;

    ISymbolPtr pS;
    bool r = m_pVectorLayer->GetDefaultSymbol(pS);
    if (pS.Assigned())
    {
        *ppSymbol = pS._p();
        (*ppSymbol)->_AddRef();
    }

    return r;
}

void __stdcall CVectorLayerAgent::SetRendererType(const SlimRendererType renderertype)
{
    m_pVectorLayer->SetRendererType(renderertype, true);
}

SlimRendererType __stdcall CVectorLayerAgent::GetRendererType() const
{
    return m_pVectorLayer->GetRendererType();
}

bool __stdcall CVectorLayerAgent::SetSymbol(const char* const key, const ISymbol* pSymbol)
{
    if (_invalid(pSymbol)) return false;
    ISymbolPtr pS = (ISymbol*)pSymbol;
    string skey = key;
    return m_pVectorLayer->SetSymbol(skey, pS);
}

bool __stdcall CVectorLayerAgent::GetSymbol(const char* const key, ISymbol** ppSymbol) const
{
    if (_invalid(ppSymbol)) return false;
    assert(!*ppSymbol);
    *ppSymbol = NULL;

    ISymbolPtr pS;
    string skey = key;
    bool r = m_pVectorLayer->GetSymbol(skey, pS);
    if (pS.Assigned())
    {
        *ppSymbol = pS._p();
        (*ppSymbol)->_AddRef();
    }

    return r;
}

bool __stdcall CVectorLayerAgent::GetSymbolByIndex(const dword index, IAnsiString** ppKey, ISymbol** ppSymbol) const
{
    if (_invalid(ppKey) || _invalid(ppSymbol)) return false;
    assert(!*ppKey);
    assert(!*ppSymbol);
    *ppKey = NULL;
    *ppSymbol = NULL;

    ISymbolPtr pS;
    string skey;
    bool r = m_pVectorLayer->GetSymbolByIndex(index, skey, pS);
    if (r && pS.Assigned())
    {
        *ppSymbol = pS._p();
        (*ppSymbol)->_AddRef();

        *ppKey = new CAnsiString;
        (*ppKey)->_AddRef();
        (*ppKey)->SetText(skey.c_str());
    }

    return r;
}

dword __stdcall CVectorLayerAgent::GetSymbolCount() const
{
    return m_pVectorLayer->GetSymbolCount();
}

void __stdcall CVectorLayerAgent::ClearSymbols()
{
    m_pVectorLayer->ClearSymbols();
}

bool __stdcall CVectorLayerAgent::SetRendererField(const long fieldindex)
{
    return m_pVectorLayer->SetRendererField(fieldindex, true);
}

long __stdcall CVectorLayerAgent::GetRendererField() const
{
    return m_pVectorLayer->GetRendererField();
}

void __stdcall CVectorLayerAgent::SetShowDefaultSymbol(const bool showdefaultsymbol)
{
    m_pVectorLayer->SetShowDefaultSymbol(showdefaultsymbol, true);
}

bool __stdcall CVectorLayerAgent::GetShowDefaultSymbol() const
{
    return m_pVectorLayer->GetShowDefaultSymbol();
}

bool __stdcall CVectorLayerAgent::ReadOnly() const
{
    return m_pVectorLayer->ReadOnly();
}

void __stdcall CVectorLayerAgent::SetNetTolerance(const double tolerance)
{
    m_pVectorLayer->SetNetTolerance(tolerance);
}

double __stdcall CVectorLayerAgent::GetNetTolerance() const
{
    return m_pVectorLayer->GetNetTolerance();
}

bool __stdcall CVectorLayerAgent::CreateNetTopo(const long field, const bool bidirectional)
{
    return m_pVectorLayer->CreateNetTopo(field, bidirectional);
}

bool __stdcall CVectorLayerAgent::CreateNetTopo2(const dword field_from_to,
    const dword field_to_from)
{
    return m_pVectorLayer->CreateNetTopo2(field_from_to, field_to_from);
}

bool __stdcall CVectorLayerAgent::AddNetRoute(const WKSPoint& route)
{
    return m_pVectorLayer->AddNetRoute(route);
}

bool __stdcall CVectorLayerAgent::RemoveNetRoute(const WKSPoint& route)
{
    return m_pVectorLayer->RemoveNetRoute(route);
}

bool __stdcall CVectorLayerAgent::GetNetRoutes(IMultiPoint** ppRoutes) const
{
    return m_pVectorLayer->GetNetRoutes(ppRoutes);
}

void __stdcall CVectorLayerAgent::ClearNetRoutes()
{
    m_pVectorLayer->ClearNetRoutes();
}

bool __stdcall CVectorLayerAgent::AddNetBarrierPoint(const WKSPoint& barrier)
{
    return m_pVectorLayer->AddNetBarrierPoint(barrier);
}

bool __stdcall CVectorLayerAgent::RemoveNetBarrierPoint(const WKSPoint& barrier)
{
    return m_pVectorLayer->RemoveNetBarrierPoint(barrier);
}

bool __stdcall CVectorLayerAgent::GetNetBarrierPoints(IMultiPoint** ppBarriers) const
{
    return m_pVectorLayer->GetNetBarrierPoints(ppBarriers);
}

void __stdcall CVectorLayerAgent::ClearNetBarrierPoints()
{
    m_pVectorLayer->ClearNetBarrierPoints();
}

bool __stdcall CVectorLayerAgent::AddNetBlockedBiEdge(const dword fid)
{
    return m_pVectorLayer->AddNetBlockedBiEdge(fid);
}

bool __stdcall CVectorLayerAgent::AddNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to)
{
    return m_pVectorLayer->AddNetBlockedSingleEdge(from, to);
}

bool __stdcall CVectorLayerAgent::RemoveNetBlockedBiEdge(const dword fid)
{
    return m_pVectorLayer->RemoveNetBlockedBiEdge(fid);
}

bool __stdcall CVectorLayerAgent::RemoveNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to)
{
    return m_pVectorLayer->RemoveNetBlockedSingleEdge(from, to);
}

dword __stdcall CVectorLayerAgent::GetNetBlockedEdgeCount() const
{
    return m_pVectorLayer->GetNetBlockedEdgeCount();
}

bool __stdcall CVectorLayerAgent::GetNetBlockedEdgeByIndex(const dword i, dword& fid,
    WKSPoint& from, WKSPoint& to) const
{
    return m_pVectorLayer->GetNetBlockedEdgeByIndex(i, fid, from, to);
}

bool __stdcall CVectorLayerAgent::GetNetBlockedEdgeIDs(IIntArray** ppFids) const
{
    if (_invalid(ppFids))
        return false;
    assert(!*ppFids);
    *ppFids = new CIntArray;
    (*ppFids)->_AddRef();

    vector<dword> fids;
    bool r = m_pVectorLayer->GetNetBlockedEdgeIDs(fids);
    for (long i = 0; i < fids.size(); i++)
    {
        (*ppFids)->Add(fids[i]);
    }

    return r;
}

void __stdcall CVectorLayerAgent::ClearNetBlockedEdges()
{
    m_pVectorLayer->ClearNetBlockedEdges();
}

bool __stdcall CVectorLayerAgent::DoBestPath(IPath** ppPath, IIntArray** ppFids)
{
    return m_pVectorLayer->DoBestPath(ppPath, ppFids);
}

void __stdcall CVectorLayerAgent::ClearNetTopo()
{
    m_pVectorLayer->ClearNetTopo();
}

bool __stdcall CVectorLayerAgent::StoreNetTopo()
{
    return m_pVectorLayer->StoreNetTopo();
}

bool __stdcall CVectorLayerAgent::RestoreNetTopo()
{
    return m_pVectorLayer->RestoreNetTopo();
}

bool __stdcall CVectorLayerAgent::Shapefile2ESD(const char* const esdfilename) const
{
    if (_invalid(esdfilename)) {return false;}
    string sfilename = esdfilename;

    CShapeLayerPtr pShapeLayer;
    CAST_PTR(m_pVectorLayer, pShapeLayer, CShapeLayer)
    if (!pShapeLayer.Assigned()) {return false;}

    return pShapeLayer->SaveData2ESD(sfilename);
}



CElementLayerAgent::CElementLayerAgent()
{
    INIT_REFCOUNT
}

CElementLayerAgent::~CElementLayerAgent()
{
}

bool __stdcall CElementLayerAgent::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IElementLayerAgent"))
        || (0 == strcmp(interfacename, "CElementLayerAgent")))
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

bool __stdcall CElementLayerAgent::Clone(IObj** ppObj) const
{
    return false;
}

bool __stdcall CElementLayerAgent::SetLayer(const ILayer* pLayer)
{
    m_pElementLayer.Clear();
    if (_invalid(pLayer))
    {
        return true;
    }

    ILayerPtr pL = (ILayer*)pLayer;
    CAST_PTR(pL, m_pElementLayer, CElementLayer)
    if (!m_pElementLayer.Assigned())
    {
        return false;
    }

    return true;
}

bool __stdcall CElementLayerAgent::GetLayer(ILayer** ppLayer) const
{
    if (_invalid(ppLayer)) return false;
    assert(!*ppLayer);
    *ppLayer = NULL;

    if (m_pElementLayer.Assigned())
    {
        m_pElementLayer->GotoInterface("ILayer", (void**)(ppLayer));
    }

    return true;
}

bool __stdcall CElementLayerAgent::CreateElementLayer(const char* const layername)
{
    m_pElementLayer.Clear();
    m_pElementLayer = new CElementLayer;
    if (layername)
        m_pElementLayer->SetName(layername);

    return true;
}

void __stdcall CElementLayerAgent::SetRefScale(const double scale)
{
    m_pElementLayer->SetRefScale(scale);
}

void __stdcall CElementLayerAgent::GetRefScale(double& scale) const
{
    m_pElementLayer->GetRefScale(scale);
}

dword __stdcall CElementLayerAgent::AddElement(const IElement* pElement)
{
    if (_invalid(pElement)) return 0;
    IElementPtr pE = (IElement*)pElement;
    return m_pElementLayer->AddElement(pE);
}

bool __stdcall CElementLayerAgent::GetElement(const dword id, IElement** ppElement) const
{
    if (_invalid(ppElement)) return false;
    assert(!*ppElement);
    *ppElement = NULL;

    IElementPtr pE;
    bool r = m_pElementLayer->GetElement(id, pE);
    if (pE.Assigned())
    {
        *ppElement = pE._p();
        (*ppElement)->_AddRef();
    }

    return r;
}

bool __stdcall CElementLayerAgent::SetElement(const dword id, const IElement* pElement)
{
    IElementPtr pE = (IElement*)pElement;
    return m_pElementLayer->SetElement(id, pE);
}

bool __stdcall CElementLayerAgent::RemoveElement(const dword id)
{
    return m_pElementLayer->RemoveElement(id);
}

bool __stdcall CElementLayerAgent::GetSelectElements(IIntArray** ppIDs) const
{
    if (_invalid(ppIDs)) return false;
    assert(!*ppIDs);
    *ppIDs = NULL;

    vector<dword> ids;
    bool r = m_pElementLayer->GetSelectElements(ids);
    if (r)
    {
        *ppIDs = new CIntArray;
        vector<dword>::const_iterator it = ids.begin();
        while (it != ids.end())
        {
            (*ppIDs)->Add(*it);
            it++;
        }
    }

    return r;
}

void __stdcall CElementLayerAgent::MoveSelectElements(const double delta_x, const double delta_y)
{
    m_pElementLayer->MoveSelectElements(delta_x, delta_y);
}

bool __stdcall CElementLayerAgent::RemoveSelectedElements()
{
    return m_pElementLayer->RemoveSelectedElements();
}

bool __stdcall CElementLayerAgent::GetElementIDFromIndex(const dword index, dword& id) const
{
    return m_pElementLayer->GetElementIDFromIndex(index, id);
}

dword __stdcall CElementLayerAgent::GetElementCount() const
{
    return m_pElementLayer->GetElementCount();
}

void __stdcall CElementLayerAgent::ClearElements()
{
    m_pElementLayer->ClearElements();
}

bool __stdcall CElementLayerAgent::Identify(IIntArray** ppIDs, const WKSRect& envelope,
    const bool partialselect)
{
    if (_invalid(ppIDs)) return false;
    assert(!*ppIDs);
    *ppIDs = NULL;

    vector<dword> ids;
    return m_pElementLayer->Identify(ids, envelope, partialselect);
    *ppIDs = new CIntArray;
    vector<dword>::const_iterator it = ids.begin();
    while (it != ids.end())
    {
        (*ppIDs)->Add(*it);
        it++;
    }

    return true;
}

}