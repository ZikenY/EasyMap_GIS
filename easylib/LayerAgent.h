#if !defined(LAYERAGENT_INCLUDED_)
#define LAYERAGENT_INCLUDED_

#include "CommonInclude.h"
#include "..\\include\\InterfaceLayerAgent.h"
#include "ShapeLayer.h"
#include "ElementLayer.h"

namespace easymap
{

class CVectorLayerAgent;
typedef TSmartPtr<CVectorLayerAgent> CVectorLayerAgentPtr;
class CElementLayerAgent;
typedef TSmartPtr<CElementLayerAgent> CElementLayerAgentPtr;

class CVectorLayerAgent : public IVectorLayerAgent
{
CLASS_NAME(CVectorLayerAgent)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CVectorLayerAgent();

private:
    ~CVectorLayerAgent();

private:
    CVectorLayerPtr m_pVectorLayer;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    bool __stdcall SetLayer(const ILayer* pLayer);
    bool __stdcall GetLayer(ILayer** ppLayer) const;

    bool __stdcall CreateSlimLayer(
        const ShapeType     shapetype,
        const MapUnits      mapunit,
        const double        basescale,
        const double        precision,
        const WKSRect&      extent,
        const long          indexlevel,
        const IFields*      pFields,
        const bool          annotation,
        const char* const   filename,
        const bool          filemap
        );

    bool __stdcall LoadSlimLayer(
        const char* const   filename,
        const bool          readonly,
        const bool          filemap
        );

    bool __stdcall LoadShapeLayer(
        const char* const   filename,
        const MapUnits      mapunit,
        const double        basescale,
        const double        precision,
        const long          indexlevel,
        const bool          readonly
        );

    void __stdcall GetPrecision(double& precision) const;
    void __stdcall SetRefScale(const double scale);
    void __stdcall GetRefScale(double& scale) const;

    dword __stdcall Select(const IIntArray* pFids, const bool append);
    dword __stdcall Deselect(const IIntArray* pFids);
    dword __stdcall GetSelection(IIntArray** ppFids) const;

    bool __stdcall GetFids(IIntArray** ppFids) const;
    dword __stdcall GetFeatureCount() const;
    bool __stdcall GetFeature(const dword fid, IGeometry** ppGeometry,
        char* const text) const;
    bool __stdcall GetFeatureMBR(const dword fid, WKSRect& mbr) const;
    bool __stdcall DeleteFeature(const dword fid);

    bool __stdcall CreateFeature(IVectorFeature** ppFeature);
    bool __stdcall GetFeature(const dword fid, IVectorFeature** ppFeature);

    //这个方法不管undo/redo，并且只要可能，就不会导致文件增大
    //可以用于GPS信号的实时刷新
    bool __stdcall RapidModifyPoint(const dword fid, const WKSPoint& point);

    bool __stdcall Identify(IIntArray** ppFids, const WKSRect& envelope,
        const bool partialselect);

    void __stdcall GetGeometryColumnInfo(GeometryColumnInfo& geocolinfo) const;

    void __stdcall GetFields(IFields** ppFields) const;
    bool __stdcall SetDisplayField(const long fieldindex);
    long __stdcall GetDisplayField() const;

    bool __stdcall SetDefaultSymbol(const ISymbol* pSymbol);
    bool __stdcall GetDefaultSymbol(ISymbol** ppSymbol) const;

    void __stdcall SetRendererType(const SlimRendererType renderertype);
    SlimRendererType __stdcall GetRendererType() const;
    bool __stdcall SetSymbol(const char* const key, const ISymbol* pSymbol);
    bool __stdcall GetSymbol(const char* const key, ISymbol** ppSymbol) const;
    bool __stdcall GetSymbolByIndex(const dword index, IAnsiString** ppKey, ISymbol** ppSymbol) const;
    dword __stdcall GetSymbolCount() const;
    void __stdcall ClearSymbols();
    bool __stdcall SetRendererField(const long fieldindex);
    long __stdcall GetRendererField() const;
    void __stdcall SetShowDefaultSymbol(const bool showdefaultsymbol);
    bool __stdcall GetShowDefaultSymbol() const;

    bool __stdcall ReadOnly() const;

    void __stdcall SetNetTolerance(const double tolerance);
    double __stdcall GetNetTolerance() const;
    bool __stdcall CreateNetTopo(const long field, const bool bidirectional);
    bool __stdcall CreateNetTopo2(const dword field_from_to, const dword field_to_from);
    bool __stdcall AddNetRoute(const WKSPoint& route);
    bool __stdcall RemoveNetRoute(const WKSPoint& route);
    bool __stdcall GetNetRoutes(IMultiPoint** ppRoutes) const;
    void __stdcall ClearNetRoutes();
    bool __stdcall AddNetBarrierPoint(const WKSPoint& barrier);
    bool __stdcall RemoveNetBarrierPoint(const WKSPoint& barrier);
    bool __stdcall GetNetBarrierPoints(IMultiPoint** ppBarriers) const;
    void __stdcall ClearNetBarrierPoints();
    bool __stdcall  AddNetBlockedBiEdge(const dword fid);
    bool __stdcall  AddNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to);
    bool __stdcall  RemoveNetBlockedBiEdge(const dword fid);
    bool __stdcall  RemoveNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to);
    dword __stdcall GetNetBlockedEdgeCount() const;
    bool __stdcall GetNetBlockedEdgeByIndex(const dword i, dword& fid, WKSPoint& from, WKSPoint& to) const;
    bool __stdcall  GetNetBlockedEdgeIDs(IIntArray** ppFids) const;
    void __stdcall  ClearNetBlockedEdges();
    bool __stdcall DoBestPath(IPath** ppPath, IIntArray** ppFids);
    void __stdcall ClearNetTopo();
    bool __stdcall StoreNetTopo();
    bool __stdcall RestoreNetTopo();

    bool __stdcall Shapefile2ESD(const char* const esdfilename) const;
};


class CElementLayerAgent : public IElementLayerAgent
{
CLASS_NAME(CElementLayerAgent)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CElementLayerAgent();

private:
    ~CElementLayerAgent();

private:
    CElementLayerPtr m_pElementLayer;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    bool __stdcall SetLayer(const ILayer* pLayer);
    bool __stdcall GetLayer(ILayer** ppLayer) const;
    bool __stdcall CreateElementLayer(const char* const layername);

    void __stdcall SetRefScale(const double scale);
    void __stdcall GetRefScale(double& scale) const;

    dword __stdcall AddElement(const IElement* pElement);
    bool __stdcall GetElement(const dword id, IElement** ppElement) const;
    bool __stdcall SetElement(const dword id, const IElement* pElement);
    bool __stdcall RemoveElement(const dword id);
    bool __stdcall GetSelectElements(IIntArray** ppIDs) const;
    void __stdcall MoveSelectElements(const double delta_x, const double delta_y);
    bool __stdcall RemoveSelectedElements();
    bool __stdcall GetElementIDFromIndex(const dword index, dword& id) const;
    dword __stdcall GetElementCount() const;
    void __stdcall ClearElements();

    bool __stdcall Identify(IIntArray** ppIDs, const WKSRect& envelope,
        const bool partialselect);
};

CLASS_FACTORY(CVectorLayerAgent)
CLASS_FACTORY(CElementLayerAgent)
}

#endif