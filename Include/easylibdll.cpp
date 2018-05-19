#include "easylibdll.h"

namespace easymap
{

easylibdll easylibdll::m_DLL;

easylibdll::easylibdll()
{
    m_Handle = NULL;
    m_pCreateObj = NULL;
    this->_loadeasylib("easylib.dll");
}

easylibdll::~easylibdll()
{
    if (m_Handle) ::FreeLibrary(m_Handle);
}

void easylibdll::_loadeasylib(const char* const filename)
{
    if (m_Handle)
        return;

    m_Handle = ::LoadLibrary(filename);
    if (!m_Handle)
    {
        m_pCreateObj = NULL;
        return;
    }

    m_pCreateObj = (_CreateObj*)::GetProcAddress(m_Handle, "CreateObj");
    m_pInstantiate = (_Instantiate*)::GetProcAddress(m_Handle, "Instantiate");

    m_pCreateGeometry = (_CreateGeometry*)::GetProcAddress(m_Handle, "CreateGeometry");
    m_pGeometry2Stream = (_Geometry2Stream*)::GetProcAddress(m_Handle, "Geometry2Stream");
    m_pStream2Geometry = (_Stream2Geometry*)::GetProcAddress(m_Handle, "Stream2Geometry");
    m_pCreateSimplePointSymbol = (_CreateSimplePointSymbol*)::GetProcAddress(m_Handle, "CreateSimplePointSymbol");
    m_pCreateEnvelopePointSymbol = (_CreateEnvelopePointSymbol*)::GetProcAddress(m_Handle, "CreateEnvelopePointSymbol");
    m_pCreatePolyPointSymbol = (_CreatePolyPointSymbol*)::GetProcAddress(m_Handle, "CreatePolyPointSymbol");
    m_pCreateSimpleLineSymbol = (_CreateSimpleLineSymbol*)::GetProcAddress(m_Handle, "CreateSimpleLineSymbol");
    m_pCreatePointLineSymbol = (_CreatePointLineSymbol*)::GetProcAddress(m_Handle, "CreatePointLineSymbol");
    m_pCreateSimpleFillSymbol = (_CreateSimpleFillSymbol*)::GetProcAddress(m_Handle, "CreateSimpleFillSymbol");
    m_pCreateSimpleTextSymbol = (_CreateSimpleTextSymbol*)::GetProcAddress(m_Handle, "CreateSimpleTextSymbol");
    m_pCreateMultiPointSymbol = (_CreateMultiPointSymbol*)::GetProcAddress(m_Handle, "CreateMultiPointSymbol");
    m_pCreateMultiLineSymbol = (_CreateMultiLineSymbol*)::GetProcAddress(m_Handle, "CreateMultiLineSymbol");
    m_pCreateMultiFillSymbol = (_CreateMultiFillSymbol*)::GetProcAddress(m_Handle, "CreateMultiFillSymbol");

    m_pCreateSlimData = (_CreateSlimData*)::GetProcAddress(m_Handle, "CreateSlimData");
    m_pLoadSlimData = (_LoadSlimData*)::GetProcAddress(m_Handle, "LoadSlimData");
    m_pLoadShapeFile = (_LoadShapeFile*)::GetProcAddress(m_Handle, "LoadShapeFile");
}

HINSTANCE easylibdll::getDLLHandle()
{
    return easylibdll::m_DLL.m_Handle;
}

void easylibdll::LoadEasyLib(const char* const filename)
{
    easylibdll::m_DLL._loadeasylib(filename);
}

bool easylibdll::CreateObj(const char* classname, IObjPtr& pObj)
{
    IObj* po = NULL;
    (*m_DLL.m_pCreateObj)(classname, &po);
    if (!po) return false;
    pObj = po;
    po->_Release();
    return true;
}

dword easylibdll::Instantiate(IStreamXPtr pStream, IPersistPtr& pPersist)
{
    IPersist* pP = NULL;
    dword r = (*m_DLL.m_pInstantiate)(pStream._p(), &pP);
    pPersist = pP;
    if (pP) pP->_Release();
    return r;
}

bool easylibdll::CreateGeometry(GeometryType geometrytype, IGeometryPtr& pGeometry)
{
    IGeometry* pG = NULL;
    bool r = (*m_DLL.m_pCreateGeometry)(geometrytype, &pG);
    pGeometry = pG;
    if (pG) pG->_Release();
    return r;
}

dword easylibdll::Geometry2Stream(const IGeometryPtr pGeometry, IStreamXPtr pStream)
{
    return (*m_DLL.m_pGeometry2Stream)(pGeometry._p(), pStream._p());
}

dword easylibdll::Stream2Geometry(IStreamXPtr pStream, IGeometryPtr& pGeometry)
{
    IGeometry* pG = NULL;
    dword r = (*m_DLL.m_pStream2Geometry)(pStream._p(), &pG);
    pGeometry = pG;
    if (pG) pG->_Release();
    return r;
}

bool easylibdll::CreateSimplePointSymbol(const double diameter, const COLORREF color,
    ISimplePointSymbolPtr& pSimplePointSymbol)
{
    ISimplePointSymbol* pS = NULL;
    bool r = (*m_DLL.m_pCreateSimplePointSymbol)(diameter, color, &pS);
    pSimplePointSymbol = pS;
    if (pS) pS->_Release();
    return r;
}

bool easylibdll::CreateEnvelopePointSymbol(const double width, const double height,
    const COLORREF color, IEnvelopePointSymbolPtr& pEnvelopePointSymbol)
{
    IEnvelopePointSymbol* pS = NULL;
    bool r = (*m_DLL.m_pCreateEnvelopePointSymbol)(width, height, color, &pS);
    pEnvelopePointSymbol = pS;
    if (pS) pS->_Release();
    return r;
}

bool easylibdll::CreatePolyPointSymbol(const bool solid, const double linewidth,
    const COLORREF color, IPolyPointSymbolPtr& pPolyPointSymbol)
{
    IPolyPointSymbol* pS = NULL;
    bool r = (*m_DLL.m_pCreatePolyPointSymbol)(solid, linewidth, color, &pS);
    pPolyPointSymbol = pS;
    if (pS) pS->_Release();
    return r;
}

bool easylibdll::CreateSimpleLineSymbol(const double width, const COLORREF color,
    ISimpleLineSymbolPtr& pSimpleLineSymbol)
{
    ISimpleLineSymbol* pS = NULL;
    bool r = (*m_DLL.m_pCreateSimpleLineSymbol)(width, color, &pS);
    pSimpleLineSymbol = pS;
    if (pS) pS->_Release();
    return r;
}

bool easylibdll::CreatePointLineSymbol(IPointLineSymbolPtr& pPointLineSymbol)
{
    IPointLineSymbol* pS = NULL;
    bool r = (*m_DLL.m_pCreatePointLineSymbol)(&pS);
    pPointLineSymbol = pS;
    if (pS) pS->_Release();
    return r;
}

bool easylibdll::CreateSimpleFillSymbol(const COLORREF color, const double borderwidth,
    const COLORREF bordercolor, ISimpleFillSymbolPtr& pSimpleFillSymbol)
{
    ISimpleFillSymbol* pS = NULL;
    bool r = (*m_DLL.m_pCreateSimpleFillSymbol)(color, borderwidth, bordercolor, &pS);
    pSimpleFillSymbol = pS;
    if (pS) pS->_Release();
    return r;
}

bool easylibdll::CreateSimpleTextSymbol(const double width, const double height,
    const char* const text, const COLORREF textcolor, ISimpleTextSymbolPtr& pSimpleTextSymbol)
{
    ISimpleTextSymbol* pS = NULL;
    bool r = (*m_DLL.m_pCreateSimpleTextSymbol)(width, height, text, textcolor, &pS);
    pSimpleTextSymbol = pS;
    if (pS) pS->_Release();
    return r;
}

bool easylibdll::CreateMultiPointSymbol(IMultiPointSymbolPtr& pMultiPointSymbol)
{
    IMultiPointSymbol* pS = NULL;
    bool r = (*m_DLL.m_pCreateMultiPointSymbol)(&pS);
    pMultiPointSymbol = pS;
    if (pS) pS->_Release();
    return r;
}

bool easylibdll::CreateMultiLineSymbol(IMultiLineSymbolPtr& pMultiLineSymbol)
{
    IMultiLineSymbol* pS = NULL;
    bool r = (*m_DLL.m_pCreateMultiLineSymbol)(&pS);
    pMultiLineSymbol = pS;
    if (pS) pS->_Release();
    return r;
}

bool easylibdll::CreateMultiFillSymbol(IMultiFillSymbolPtr& pMultiFillSymbol)
{
    IMultiFillSymbol* pS = NULL;
    bool r = (*m_DLL.m_pCreateMultiFillSymbol)(&pS);
    pMultiFillSymbol = pS;
    if (pS) pS->_Release();
    return r;
}

bool easylibdll::CreateSlimData(const char* const filename, ILayerPtr& pLayer,
    const MapUnits mapunit, const double basescale, const double precision, const long indexlevel,
    const ShapeType shapetype, const WKSRect extent, const IFieldsPtr pFields, const bool anno,
    const bool filemap)
{
    IFields* pF = pFields._p();
    ILayer* pL = NULL;
    bool r = (*m_DLL.m_pCreateSlimData)(filename, &pL, mapunit, basescale, precision,
        indexlevel, shapetype, extent, pF, anno, filemap);
    pLayer = pL;
    if (pL) pL->_Release();
    return r;
}

bool easylibdll::LoadSlimData(const char* const filename, ILayerPtr& pLayer, bool readonly,
    const bool filemap)
{
    ILayer* pL = NULL;
    bool r = (*m_DLL.m_pLoadSlimData)(filename, &pL, readonly, filemap);
    pLayer = pL;
    if (pL) pL->_Release();
    return r;
}

bool easylibdll::LoadShapeFile(const char* const filename, ILayerPtr& pLayer, MapUnits mapunit,
    double basescale, double precision, long indexlevel, bool readonly)
{
    ILayer* pL = NULL;
    bool r = (*m_DLL.m_pLoadShapeFile)(filename, &pL, mapunit, basescale, precision,
        indexlevel, readonly);
    pLayer = pL;
    if (pL) pL->_Release();
    return r;
}

}