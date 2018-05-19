#include "easylib.h"
#include <windows.h>
#include <stdio.h>
#include "FileMapStream.h"
#include "MultiSymbol.h"
#include "ShapeLayer.h"

namespace easymap
{

BOOL APIENTRY DllMain(HANDLE hModule, DWORD ul_reason_for_call, LPVOID lpReserved)
{
    return TRUE;
}


bool DEF_EXPORT WINAPI CreateObj(const char* classname, IObj** ppObj)
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr p;
    _FactoryManager::CreateInstance(classname, p);
    if (!p.Assigned()) return false;
    *ppObj = (IObj*)p._p();
    (*ppObj)->_AddRef();
    return true;
}

dword DEF_EXPORT WINAPI Instantiate(IStreamX* pStream, IPersist** ppPersist)
{
    if (_invalid(pStream) || _invalid(ppPersist)) return 0;
    assert(!*ppPersist);

    CStreamPtr ps = (CStream*)pStream;
    CPersistPtr pp;
    dword r = CPersist::Instantiate(ps, pp);
    if (pp.Assigned())
    {
        *ppPersist = (IPersist*)pp._p();
        (*ppPersist)->_AddRef();
    }
    return r;
}

bool DEF_EXPORT WINAPI CreateFileMappingStream(const char* filename,
    const bool readonly, IStreamX** ppStream)
{
    if (_invalid(filename) || _invalid(ppStream)) return false;
    assert(!*ppStream);

    CFileMapStreamPtr pFileMapStream = new CFileMapStream(filename, readonly);
    if (INVALID_HANDLE_VALUE == pFileMapStream->GetMapFileHandle()) return false;
    *ppStream = (IStreamX*)pFileMapStream._p();
    (*ppStream)->_AddRef();
    return true;
}

bool DEF_EXPORT WINAPI CreateGeometry(easymap::GeometryType geometrytype,
    IGeometry** ppGeometry)
{
    if (_invalid(ppGeometry)) return false;
    assert(!*ppGeometry);

    IGeometryPtr pGeometry;
    switch (geometrytype)
    {
        case GEOMETRYTYPE_POINT: pGeometry = (IGeometry*)(new CPoint);
            break;
        case GEOMETRYTYPE_MULTIPOINT: pGeometry = (IGeometry*)(new CMultiPoint);
            break;
        case GEOMETRYTYPE_PATH: pGeometry = (IGeometry*)(new CPath);
            break;
        case GEOMETRYTYPE_RING: pGeometry = (IGeometry*)(new CRing);
            break;
        case GEOMETRYTYPE_POLYLINE: pGeometry = (IGeometry*)(new CPolyline);
            break;
        case GEOMETRYTYPE_POLYGON: pGeometry = (IGeometry*)(new CPolygon);
            break;
        case GEOMETRYTYPE_ENVELOPE: pGeometry = (IGeometry*)(new CEnvelope);
            break;
        case GEOMETRYTYPE_CIRCLE: pGeometry = (IGeometry*)(new CCircle);
            break;
        case GEOMETRYTYPE_ELLIPSE: pGeometry = (IGeometry*)(new CEllipse);
            break;
        default: return false;
    }

    *ppGeometry = pGeometry._p();
    (*ppGeometry)->_AddRef();
    return true;
}

dword DEF_EXPORT WINAPI Geometry2Stream(const IGeometry* pGeometry, IStreamX* pStream)
{
    if (_invalid(pGeometry) || _invalid(pStream)) return 0;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    CStreamPtr ps = (CStream*)pStream;
    return Geometry2Stream(pg, ps);
}

dword DEF_EXPORT WINAPI Stream2Geometry(IStreamX* pStream, IGeometry** ppGeometry)
{
    if (_invalid(pStream) || _invalid(ppGeometry)) return 0;
    assert(!*ppGeometry);

    CStreamPtr ps = (CStream*)pStream;
    IGeometryPtr pg;
    dword r = Stream2Geometry(ps, pg);
    if (pg.Assigned())
    {
        *ppGeometry = pg._p();
        (*ppGeometry)->_AddRef();
    }
    return r;
}

bool DEF_EXPORT WINAPI CreateSimplePointSymbol(const double diameter, const COLORREF color,
    ISimplePointSymbol** ppSimplePointSymbol)
{
    if (_invalid(ppSimplePointSymbol)) return false;
    *ppSimplePointSymbol = new CSimplePointSymbol();
    (*ppSimplePointSymbol)->_AddRef();
    (*ppSimplePointSymbol)->SetDiameter(diameter);
    (*ppSimplePointSymbol)->SetColor(color);
    return true;
}

bool DEF_EXPORT WINAPI CreateEnvelopePointSymbol(const double width, const double height,
    const COLORREF color, IEnvelopePointSymbol** ppEnvelopePointSymbol)
{
    if (_invalid(ppEnvelopePointSymbol)) return false;
    assert(!*ppEnvelopePointSymbol);

    *ppEnvelopePointSymbol = new CEnvelopePointSymbol();
    (*ppEnvelopePointSymbol)->_AddRef();
    (*ppEnvelopePointSymbol)->SetWidth(width);
    (*ppEnvelopePointSymbol)->SetHeight(height);
    (*ppEnvelopePointSymbol)->SetColor(color);
    return true;
}

bool DEF_EXPORT WINAPI CreatePolyPointSymbol(const bool solid, const double linewidth,
    const COLORREF color, IPolyPointSymbol** ppPolyPointSymbol)
{
    if (_invalid(ppPolyPointSymbol)) return false;
    assert(!*ppPolyPointSymbol);

    *ppPolyPointSymbol = new CPolyPointSymbol;
    (*ppPolyPointSymbol)->_AddRef();
    (*ppPolyPointSymbol)->SetSolid(solid);
    (*ppPolyPointSymbol)->SetLineWidth(linewidth);
    (*ppPolyPointSymbol)->SetColor(color);
    return true;
}

bool DEF_EXPORT WINAPI CreateSimpleLineSymbol(const double width, const COLORREF color,
    ISimpleLineSymbol** ppSimpleLineSymbol)
{
    if (_invalid(ppSimpleLineSymbol)) return false;
    assert(!*ppSimpleLineSymbol);

    *ppSimpleLineSymbol = new CSimpleLineSymbol;
    (*ppSimpleLineSymbol)->_AddRef();
    (*ppSimpleLineSymbol)->SetWidth(width);
    (*ppSimpleLineSymbol)->SetColor(color);
    return true;
}

bool DEF_EXPORT WINAPI CreatePointLineSymbol(IPointLineSymbol** ppPointLineSymbol)
{
    if (_invalid(ppPointLineSymbol)) return false;
    assert(!*ppPointLineSymbol);

    *ppPointLineSymbol = new CPointLineSymbol;
    (*ppPointLineSymbol)->_AddRef();
    return true;
}

bool DEF_EXPORT WINAPI CreateSimpleFillSymbol(const COLORREF color, const double borderwidth,
    const COLORREF bordercolor, ISimpleFillSymbol** ppSimpleFillSymbol)
{
    if (_invalid(ppSimpleFillSymbol)) return false;
    assert(!*ppSimpleFillSymbol);

    *ppSimpleFillSymbol = new CSimpleFillSymbol;
    (*ppSimpleFillSymbol)->_AddRef();
    (*ppSimpleFillSymbol)->SetColor(color);
    (*ppSimpleFillSymbol)->SetBorderWidth(borderwidth);
    (*ppSimpleFillSymbol)->SetBorderColor(bordercolor);
    return true;
}

bool DEF_EXPORT WINAPI CreatePointFillSymbol(IPointFillSymbol** ppPointFillSymbol,
    const COLORREF color, const double spacex, const double spacey)
{
    if (_invalid(ppPointFillSymbol)) return false;
    *ppPointFillSymbol = new CPointFillSymbol();
    (*ppPointFillSymbol)->_AddRef();
    (*ppPointFillSymbol)->SetColor(color);
    (*ppPointFillSymbol)->SetPointsSpace(spacex, spacey);
    return true;
}

bool DEF_EXPORT WINAPI CreateSimpleTextSymbol(const double width, const double height,
    const char* const text, const COLORREF textcolor, ISimpleTextSymbol** ppSimpleTextSymbol)
{
    if (_invalid(ppSimpleTextSymbol)) return false;
    assert(!*ppSimpleTextSymbol);

    *ppSimpleTextSymbol = new CSimpleTextSymbol;
    (*ppSimpleTextSymbol)->_AddRef();
    (*ppSimpleTextSymbol)->SetWidth(width);
    (*ppSimpleTextSymbol)->SetHeight(height);
    (*ppSimpleTextSymbol)->SetText(text);
    (*ppSimpleTextSymbol)->SetColor(textcolor);
    return true;
}

bool DEF_EXPORT WINAPI CreateMultiPointSymbol(IMultiPointSymbol** ppMultiPointSymbol)
{
    if (_invalid(ppMultiPointSymbol)) return false;
    assert(!*ppMultiPointSymbol);

    *ppMultiPointSymbol = new CMultiPointSymbol;
    (*ppMultiPointSymbol)->_AddRef();
    return true;
}

bool DEF_EXPORT WINAPI CreateMultiLineSymbol(IMultiLineSymbol** ppMultiLineSymbol)
{
    if (_invalid(ppMultiLineSymbol)) return false;
    assert(!*ppMultiLineSymbol);

    *ppMultiLineSymbol = new CMultiLineSymbol;
    (*ppMultiLineSymbol)->_AddRef();
    return true;
}

bool DEF_EXPORT WINAPI CreateMultiFillSymbol(IMultiFillSymbol** ppMultiFillSymbol)
{
    if (_invalid(ppMultiFillSymbol)) return false;
    assert(!*ppMultiFillSymbol);

    *ppMultiFillSymbol = new CMultiFillSymbol;
    (*ppMultiFillSymbol)->_AddRef();
    return true;
}

bool DEF_EXPORT WINAPI CreateSlimData(const char* const filename, ILayer** ppLayer,
    const MapUnits mapunit, const double basescale, const double precision, const long indexlevel,
    const ShapeType shapetype, const WKSRect extent, const IFields* pFields, const bool anno,
    const bool filemap)
{
    if (_invalid(ppLayer)) return false;
    assert(!*ppLayer);

    IFieldsPtr pIFields = (IFields*)pFields;
    CFieldsPtr pCFields;
    CAST_PTR(pIFields, pCFields, CFields);
    CSlimLayerPtr pSL = new CSlimLayer(shapetype, mapunit, basescale, precision, extent,
        indexlevel, pCFields, anno);
    if (!pSL->Valid())
    {
        return false;
    }

    string sfilename = Trim(filename);
    if (sfilename != "")
    {
        string pathpart = GetDirectoryPart(sfilename);
        CreateDirectory(pathpart.c_str(), NULL);
        pSL->AttachToFile(sfilename, filemap);
    }

    ILayerPtr pLayer;
    CAST_PTR(pSL, pLayer, ILayer)
    *ppLayer = pLayer._p();
    (*ppLayer)->_AddRef();
    return true;
}

bool DEF_EXPORT WINAPI LoadSlimData(const char* const filename, ILayer** ppLayer, bool readonly,
    bool filemap)
{
    if (_invalid(ppLayer)) return false;
    assert(!*ppLayer);

    CSlimLayerPtr pSL = new CSlimLayer(filename, readonly, filemap);
    if (!pSL->Valid())
    {
        return false;
    }

    ILayerPtr pLayer;
    CAST_PTR(pSL, pLayer, ILayer)
    *ppLayer = pLayer._p();
    (*ppLayer)->_AddRef();
    return true;
}

bool DEF_EXPORT WINAPI LoadShapeFile(const char* const filename, ILayer** ppLayer,
    MapUnits mapunit, double basescale, double precision, long indexlevel, bool readonly)
{
    if (_invalid(ppLayer)) return false;
    assert(!*ppLayer);

    CShapeLayerPtr pSL = new CShapeLayer(filename, mapunit, basescale, precision,
        indexlevel, readonly);
    if (!pSL->Valid())
    {
        return false;
    }

    ILayerPtr pLayer;
    CAST_PTR(pSL, pLayer, ILayer)
    *ppLayer = pLayer._p();
    (*ppLayer)->_AddRef();
    return true;
}

}
