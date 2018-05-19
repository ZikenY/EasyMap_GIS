#if !defined(EASYLIBDLL_INCLUDED_)
#define EASYLIBDLL_INCLUDED_

#include "windows.h"
#include "BasicType.h"
#include "InterfaceSymbol.h"
#include "InterfaceLayer.h"
#include "InterfaceFields.h"

#if !defined(WINAPI)
    #define WINAPI __stdcall
#endif

namespace easymap
{

class easylibdll
{
private:
    typedef void (WINAPI _CreateObj)(const char* classname, IObj** ppObj);
    typedef dword (WINAPI _Instantiate)(IStreamX* pStream, IPersist** ppPersist);
    typedef bool (WINAPI _CreateGeometry)(GeometryType geometrytype, IGeometry** ppGeometry);
    typedef dword (WINAPI _Geometry2Stream)(const IGeometry* pGeometry, IStreamX* pStream);
    typedef dword (WINAPI _Stream2Geometry)(IStreamX* pStream, IGeometry** ppGeometry);
    typedef bool (WINAPI _CreateSimplePointSymbol)(const double diameter, const COLORREF color,
        ISimplePointSymbol** ppSimplePointSymbol);
    typedef bool (WINAPI _CreateEnvelopePointSymbol)(const double width, const double height,
        const COLORREF color, IEnvelopePointSymbol** ppEnvelopePointSymbol);
    typedef bool (WINAPI _CreatePolyPointSymbol)(const bool solid, const double linewidth,
        const COLORREF color, IPolyPointSymbol** ppPolyPointSymbol);
    typedef bool (WINAPI _CreateSimpleLineSymbol)(const double width, const COLORREF color,
        ISimpleLineSymbol** ppSimpleLineSymbol);
    typedef bool (WINAPI _CreatePointLineSymbol)(IPointLineSymbol** ppPointLineSymbol);
    typedef bool (WINAPI _CreateSimpleFillSymbol)(const COLORREF color, const double borderwidth,
        const COLORREF bordercolor, ISimpleFillSymbol** ppSimpleFillSymbol);
    typedef bool (WINAPI _CreateSimpleTextSymbol)(const double width, const double height,
        const char* const text, const COLORREF textcolor, ISimpleTextSymbol** ppSimpleTextSymbol);
    typedef bool (WINAPI _CreateMultiPointSymbol)(IMultiPointSymbol** ppMultiPointSymbol);
    typedef bool (WINAPI _CreateMultiLineSymbol)(IMultiLineSymbol** ppMultiLineSymbol);
    typedef bool (WINAPI _CreateMultiFillSymbol)(IMultiFillSymbol** ppMultiFillSymbol);

    typedef bool (WINAPI _CreateSlimData)(const char* const filename, ILayer** ppLayer,
        const MapUnits mapunit, const double basescale, const double precision, const long indexlevel,
        const ShapeType shapetype, const WKSRect extent, const IFields* pFields, const bool anno,
        const bool filemap);
    typedef bool (WINAPI _LoadSlimData)(const char* const filename, ILayer** ppLayer, bool readonly,
        const bool filemap);
    typedef bool (WINAPI _LoadShapeFile)(const char* const filename, ILayer** ppLayer,
        MapUnits mapunit, double basescale, double precision, long indexlevel, bool readonly);

private:
    HINSTANCE m_Handle;
    static easylibdll m_DLL;

private:
    _CreateObj* m_pCreateObj;
    _Instantiate* m_pInstantiate;
    _CreateGeometry* m_pCreateGeometry;
    _Geometry2Stream* m_pGeometry2Stream;
    _Stream2Geometry* m_pStream2Geometry;
    _CreateSimplePointSymbol* m_pCreateSimplePointSymbol;
    _CreateEnvelopePointSymbol* m_pCreateEnvelopePointSymbol;
    _CreatePolyPointSymbol* m_pCreatePolyPointSymbol;
    _CreateSimpleLineSymbol* m_pCreateSimpleLineSymbol;
    _CreatePointLineSymbol* m_pCreatePointLineSymbol;
    _CreateSimpleFillSymbol* m_pCreateSimpleFillSymbol;
    _CreateSimpleTextSymbol* m_pCreateSimpleTextSymbol;
    _CreateMultiPointSymbol* m_pCreateMultiPointSymbol;
    _CreateMultiLineSymbol* m_pCreateMultiLineSymbol;
    _CreateMultiFillSymbol* m_pCreateMultiFillSymbol;
    _CreateSlimData* m_pCreateSlimData;
    _LoadSlimData* m_pLoadSlimData;
    _LoadShapeFile* m_pLoadShapeFile;

private:
    easylibdll();

public:
    ~easylibdll();
    void _loadeasylib(const char* const filename);

public:
    static HINSTANCE getDLLHandle();
    static void LoadEasyLib(const char* const filename);

    static bool CreateObj(const char* classname, IObjPtr& pObj);
    static dword Instantiate(IStreamXPtr pStream, IPersistPtr& pPersist);
    static bool CreateGeometry(GeometryType geometrytype, IGeometryPtr& pGeometry);
    static dword Geometry2Stream(const IGeometryPtr pGeometry, IStreamXPtr pStream);
    static dword Stream2Geometry(IStreamXPtr pStream, IGeometryPtr& pGeometry);

    static bool CreateSimplePointSymbol(const double diameter, const COLORREF color,
        ISimplePointSymbolPtr& pSimplePointSymbol);
    static bool CreateEnvelopePointSymbol(const double width, const double height,
        const COLORREF color, IEnvelopePointSymbolPtr& pEnvelopePointSymbol);
    static bool CreatePolyPointSymbol(const bool solid, const double linewidth,
        const COLORREF color, IPolyPointSymbolPtr& pPolyPointSymbol);
    static bool CreateSimpleLineSymbol(const double width, const COLORREF color,
        ISimpleLineSymbolPtr& pSimpleLineSymbol);
    static bool CreatePointLineSymbol(IPointLineSymbolPtr& pPointLineSymbol);
    static bool CreateSimpleFillSymbol(const COLORREF color, const double borderwidth,
        const COLORREF bordercolor, ISimpleFillSymbolPtr& pSimpleFillSymbol);
    static bool CreateSimpleTextSymbol(const double width, const double height,
        const char* const text, const COLORREF textcolor, ISimpleTextSymbolPtr& pSimpleTextSymbol);
    static bool CreateMultiPointSymbol(IMultiPointSymbolPtr& pMultiPointSymbol);
    static bool CreateMultiLineSymbol(IMultiLineSymbolPtr& pMultiLineSymbol);
    static bool CreateMultiFillSymbol(IMultiFillSymbolPtr& pMultiFillSymbol);

    static bool CreateSlimData(const char* const filename, ILayerPtr& pLayer, const MapUnits mapunit,
        const double basescale, const double precision, const long indexlevel, const ShapeType shapetype,
        const WKSRect extent, const IFieldsPtr pFields, const bool anno, const bool filemap);
    static bool LoadSlimData(const char* const filename, ILayerPtr& pLayer, bool readonly,
        const bool filemap);
    static bool LoadShapeFile(const char* const filename, ILayerPtr& pLayer,
        MapUnits mapunit, double basescale, double precision, long indexlevel, bool readonly);

};

//通过类名称字符串创建对象，并传出特定的接口类型
#define EASYLIB_CREATEOBJ(class_name, ptr, interface_name)\
    ptr.Clear();\
    {\
        easymap::IObjPtr pObj;\
        easymap::easylibdll::CreateObj(#class_name, pObj);\
        CAST_PTR(pObj, ptr, interface_name);\
    }

}

#endif