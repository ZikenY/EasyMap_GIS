#if !defined(EASYLIB_INCLUDED_)
#define EASYLIB_INCLUDED_

#include "..\\include\\InterfaceStream.h"
#include "..\\include\\InterfaceGeometry.h"
#include "..\\include\\InterfaceSymbol.h"
#include "..\\include\\InterfaceLayer.h"
#include "..\\include\\InterfaceFields.h"

#if !defined(WINAPI)
    #define WINAPI __stdcall
#endif

#if !defined(DEF_EXPORT)
    #define DEF_EXPORT __declspec(dllexport)
#endif

namespace easymap
{

extern "C"
{
    bool DEF_EXPORT WINAPI CreateObj(const char* classname, IObj** ppObj);
    dword DEF_EXPORT WINAPI Instantiate(IStreamX* pStream, IPersist** ppPersist);
    bool DEF_EXPORT WINAPI CreateFileMappingStream(const char* filename, const bool readonly,
        IStreamX** pStream);

    bool DEF_EXPORT WINAPI CreateGeometry(GeometryType geometrytype, IGeometry** ppGeometry);
    dword DEF_EXPORT WINAPI Geometry2Stream(const IGeometry* pGeometry, IStreamX* pStream);
    dword DEF_EXPORT WINAPI Stream2Geometry(IStreamX* pStream, IGeometry** ppGeometry);

    bool DEF_EXPORT WINAPI CreateSimplePointSymbol(const double diameter, const COLORREF color,
        ISimplePointSymbol** ppSimplePointSymbol);
    bool DEF_EXPORT WINAPI CreateEnvelopePointSymbol(const double width, const double height,
        const COLORREF color, IEnvelopePointSymbol** ppEnvelopePointSymbol);
    bool DEF_EXPORT WINAPI CreatePolyPointSymbol(const bool solid, const double linewidth,
        const COLORREF color, IPolyPointSymbol** ppPolyPointSymbol);
    bool DEF_EXPORT WINAPI CreateSimpleLineSymbol(const double width, const COLORREF color,
        ISimpleLineSymbol** ppSimpleLineSymbol);
    bool DEF_EXPORT WINAPI CreatePointLineSymbol(IPointLineSymbol** ppPointLineSymbol);
    bool DEF_EXPORT WINAPI CreateSimpleFillSymbol(const COLORREF color, const double borderwidth,
        const COLORREF bordercolor, ISimpleFillSymbol** ppSimpleFillSymbol);
    bool DEF_EXPORT WINAPI CreatePointFillSymbol(IPointFillSymbol** ppPointFillSymbol,
        const COLORREF color, const double spacex, const double spacey);
    bool DEF_EXPORT WINAPI CreateSimpleTextSymbol(const double width, const double height,
        const char* const text, const COLORREF textcolor, ISimpleTextSymbol** ppSimpleTextSymbol);
    bool DEF_EXPORT WINAPI CreateMultiPointSymbol(IMultiPointSymbol** ppMultiPointSymbol);
    bool DEF_EXPORT WINAPI CreateMultiLineSymbol(IMultiLineSymbol** ppMultiLineSymbol);
    bool DEF_EXPORT WINAPI CreateMultiFillSymbol(IMultiFillSymbol** ppMultiFillSymbol);

    bool DEF_EXPORT WINAPI CreateSlimData(const char* const filename, ILayer** ppLayer,
        const MapUnits mapunit, const double basescale, const double precision, const long indexlevel,
        const ShapeType shapetype, const WKSRect extent, const IFields* pFields, const bool anno,
        const bool filemap);

    bool DEF_EXPORT WINAPI LoadSlimData(const char* const filename, ILayer** ppLayer, bool readonly,
        bool filemap);
    bool DEF_EXPORT WINAPI LoadShapeFile(const char* const filename, ILayer** ppLayer,
        MapUnits mapunit, double basescale, double precision, long indexlevel, bool readonly);
}

}

#endif