#include "CommonInclude.h"
#include "GeometryTracker.h"
#include "SimpleSymbol.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CMoveTracker)

CMoveTracker::CMoveTracker()
{
    INIT_REFCOUNT

    m_CacheID = -1;
    m_Started = false;

    CSimplePointSymbolPtr pSimPntSym = new CSimplePointSymbol;
    pSimPntSym->SetColor(RGB(25, 60, 230));
    pSimPntSym->SetDiameter(3);
    CAST_PTR(pSimPntSym, m_pPntSym, IPointSymbol)

    CSimpleLineSymbolPtr pSimLineSym = new CSimpleLineSymbol;
    pSimLineSym->SetColor(RGB(0, 180, 80));
    pSimLineSym->SetWidth(1);
    CAST_PTR(pSimLineSym, m_pLineSym, ILineSymbol)

    CSimpleFillSymbolPtr pSimFillSym = new CSimpleFillSymbol;
    pSimFillSym->SetFillStyle(BS_SOLID);
    pSimFillSym->SetColor(RGB(199, 215, 158));
    pSimFillSym->SetBorderColor(RGB(0, 80, 180));
    pSimFillSym->SetBorderWidth(0.3);
    CAST_PTR(pSimFillSym, m_pFillSym, IFillSymbol)

    m_pTextSym = new CSimpleTextSymbol;
}

CMoveTracker::~CMoveTracker()
{
    m_Started = false;
    this->ClearDisplay();
}

bool __stdcall CMoveTracker::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IGeometryTracker"))
        || (0 == strcmp(interfacename, "IMoveTracker"))
        || (0 == strcmp(interfacename, "CMoveTracker")))
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

bool __stdcall CMoveTracker::Clone(IObj** ppObj) const
{
    return false;
}

bool CMoveTracker::_Start(const long X, const long Y)
{
    m_Start.x = X;
    m_Start.y = Y;

    return this->Resize();
}

bool CMoveTracker::_Finish()
{
    WKSPoint pnt_from, pnt_to;
    m_pTrans->Device2Map(m_Start, pnt_from);
    m_pTrans->Device2Map(m_MoveTo, pnt_to);

    double delta_x, delta_y;
    delta_x = pnt_to.x - pnt_from.x;
    delta_y = pnt_to.y - pnt_from.y;

    m_It = m_GeoList.begin();
    while (m_It != m_GeoList.end())
    {
        IGeometryPtr pGeo = *m_It;
        pGeo->Move(delta_x, delta_y);
        m_It++;
    }

    m_pCache->EraseCacheContent(m_CacheID);
    m_It = m_GeoList.begin();

    return true;
}

bool CMoveTracker::SetDisplay(const CDisplayPtr pDisplay)
{
    if (!pDisplay.Assigned())
    {
        return false;
    }

    if (!this->ClearDisplay())
    {
        return false;
    }

    m_pDisplay = pDisplay._p();
    CAST_PTR(m_pDisplay, m_pCache, CDisplayCache)
    if (!m_pCache._p())
    {
        m_pDisplay.Clear();
        return false;
    }

    m_pTrans.Clear();
    m_pDisplay->GetDisplayTransformation(m_pTrans);

    m_CacheID = m_pCache->CreateCache(RGB(255, 255, 255), 255, false, SRCAND);
    return true;
}

bool __stdcall CMoveTracker::SetDisplay(const IDisplay* pDisplay)
{
    CDisplayPtr pD = (CDisplay*)pDisplay;
    return this->SetDisplay(pD);
}

void CMoveTracker::GetDisplay(CDisplayPtr pDisplay) const
{
    pDisplay = m_pDisplay._p();
}

bool __stdcall CMoveTracker::GetDisplay(IDisplay** ppDisplay) const
{
    if (_invalid(ppDisplay))
        return false;

    assert(!*ppDisplay);
    *ppDisplay = NULL;

    CDisplayPtr pD;
    this->GetDisplay(pD);
    *ppDisplay = (IDisplay*)(pD._p());
    if (_valid(*ppDisplay))
    {
        (*ppDisplay)->_AddRef();
        return true;
    }

    return false;
}

bool __stdcall CMoveTracker::ClearDisplay()
{
    if (m_Started)
    {
        return false;
    }

    if (m_pDisplay.Assigned())
    {
        if (0 <= m_CacheID)
        {
            m_pCache->DeleteCache(m_CacheID);
        }

        m_CacheID = -1;
        m_pDisplay.Clear();
        m_pCache.Clear();
        m_pTrans.Clear();
    }

    return true;
}

bool CMoveTracker::SetSymbol(const ISymbolPtr pSymbol)
{
    if (!pSymbol.Assigned())
    {
        return false;
    }

    SymbolType symtype = pSymbol->GetSymbolType();
    switch (symtype)
    {
    case SYMBOLTYPE_POINT:
        CAST_PTR(pSymbol, m_pPntSym, IPointSymbol)
        break;
    case SYMBOLTYPE_LINE:
        CAST_PTR(pSymbol, m_pLineSym, ILineSymbol)
        break;
    case SYMBOLTYPE_FILL:
        CAST_PTR(pSymbol, m_pFillSym, IFillSymbol)
        break;
    case SYMBOLTYPE_TEXT:
        CAST_PTR(pSymbol, m_pTextSym, ITextSymbol)
        break;
    default:
        return false;
    }

    if (m_Started || !m_pCache.Assigned() || (0 > m_CacheID))
    {
        return false;
    }

    return true;
}

bool __stdcall CMoveTracker::SetSymbol(const ISymbol* pSymbol)
{
    ISymbolPtr pS = (ISymbol*)pSymbol;
    return this->SetSymbol(pS);
}

bool CMoveTracker::GetSymbol(const SymbolType symtype, ISymbolPtr& pSymbol) const
{
    pSymbol.Clear();

    switch (symtype)
    {
    case SYMBOLTYPE_POINT:
        CAST_PTR(m_pPntSym, pSymbol, ISymbol)
        break;
    case SYMBOLTYPE_LINE:
        CAST_PTR(m_pLineSym, pSymbol, ISymbol);
        break;
    case SYMBOLTYPE_FILL:
        CAST_PTR(m_pFillSym, pSymbol, ISymbol);
        break;
    case SYMBOLTYPE_TEXT:
        CAST_PTR(m_pTextSym, pSymbol, ISymbol);
        break;
    default:
        return false;
    }

    return true;
}

bool __stdcall CMoveTracker::GetSymbol(const SymbolType symtype, ISymbol** ppSymbol) const
{
    if (_invalid(ppSymbol))
        return false;

    assert(!*ppSymbol);
    *ppSymbol = NULL;

    ISymbolPtr pS;
    bool r = this->GetSymbol(symtype, pS);
    *ppSymbol = (ISymbol*)(pS._p());
    if (_valid(*ppSymbol))
    {
        (*ppSymbol)->_AddRef();
    }

    return r;
}

bool __stdcall CMoveTracker::Start(const long X, const long Y)
{
    if (m_Started || !m_pCache.Assigned() || (0 > m_CacheID))
    {
        return false;
    }

    ISymbolPtr pSymbol;
    if (m_pPntSym.Assigned())
    {
        CAST_PTR(m_pPntSym, pSymbol, ISymbol);
        m_pCache->SetCacheSymbol(m_CacheID, pSymbol);
    }
    if (m_pLineSym.Assigned())
    {
        CAST_PTR(m_pLineSym, pSymbol, ISymbol);
        m_pCache->SetCacheSymbol(m_CacheID, pSymbol);
    }
    if (m_pFillSym.Assigned())
    {
        CAST_PTR(m_pFillSym, pSymbol, ISymbol);
        m_pCache->SetCacheSymbol(m_CacheID, pSymbol);
    }
    if (m_pTextSym.Assigned())
    {
        CAST_PTR(m_pTextSym, pSymbol, ISymbol);
        m_pCache->SetCacheSymbol(m_CacheID, pSymbol);
    }

    bool r = this->_Start(X, Y);
    if (r)
    {
        m_Started = true;
    }

    return r;
}

bool __stdcall CMoveTracker::Finish()
{
    if (!m_Started)
    {
        return false;
    }

    bool r = this->_Finish();
    if (r)
    {
        m_Started = false;
    }

    return r;
}

bool __stdcall CMoveTracker::Started() const
{
    return m_Started;
}

bool __stdcall CMoveTracker::MouseDown(const long X, const long Y)
{
    return false;
}

bool __stdcall CMoveTracker::MouseMove(const long X, const long Y)
{
    if (!m_Started)
    {
        return false;
    }

    m_MoveTo.x = X;
    m_MoveTo.y = Y;

    m_pDisplay->RefreshWindow1();
    return this->Refresh();
}

bool __stdcall CMoveTracker::Resize() const
{
    if (!m_pDisplay.Assigned())
    {
        return false;
    }

    if (m_pDisplay->IsDrawing())
    {
        m_pDisplay->FinishDraw();
    }

    if (!m_pDisplay->StartDraw())
    {
        return false;
    }

    list<IGeometryPtr>::const_iterator it = m_GeoList.begin();
    while (it != m_GeoList.end())
    {
        IGeometryPtr pGeo = *it;
        m_pCache->DrawCacheGeometry(m_CacheID, pGeo);
        it++;
    }

    m_pDisplay->FinishDraw();

    return true;
}

bool __stdcall CMoveTracker::Refresh() const
{
    if (!m_Started)
    {
        return false;
    }

    long delta_x, delta_y;
    delta_x = m_MoveTo.x - m_Start.x;
    delta_y = m_MoveTo.y - m_Start.y;

    HDC dc;
    m_pDisplay->GetDC(dc);
    m_pCache->PostCache1(dc, m_CacheID, delta_x, delta_y);

    return true;
}

bool CMoveTracker::AddGeometryRef(const IGeometryPtr pGeo)
{
    if (!pGeo.Assigned())
    {
        return false;
    }

    m_GeoList.push_back(pGeo);
    return true;
}

bool __stdcall CMoveTracker::AddGeometryRef(const IGeometry* pGeo)
{
    IGeometryPtr pG = (IGeometry*)pGeo;
    return this->AddGeometryRef(pG);
}

bool CMoveTracker::NextGeometryRef(IGeometryPtr& pGeo)
{
    pGeo.Clear();    
    if (m_It == m_GeoList.end())
    {
        return false;
    }

    pGeo = *(m_It++);
    return true;
}

bool __stdcall CMoveTracker::NextGeometryRef(IGeometry** ppGeo)
{
    if (_invalid(ppGeo))
        return false;
    assert(!*ppGeo);
    *ppGeo = NULL;

    IGeometryPtr pG;
    bool r = this->NextGeometryRef(pG);
    *ppGeo = (IGeometry*)(pG._p());
    if (_valid(*ppGeo))
    {
        (*ppGeo)->_AddRef();
    }

    return r;
}

void __stdcall CMoveTracker::ResetIterator()
{
    m_It = m_GeoList.begin();
}

dword __stdcall CMoveTracker::GetGeometryCount() const
{
    return m_GeoList.size();
}

void __stdcall CMoveTracker::ClearGeometry()
{
    m_GeoList.clear();
    m_It = m_GeoList.begin();    
}

}
