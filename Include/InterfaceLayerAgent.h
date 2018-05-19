#if !defined(INTERFACELAYERAGENT_INCLUDED_)
#define INTERFACELAYERAGENT_INCLUDED_

#include "InterfaceLayer.h"
#include "InterfaceFields.h"
#include "InterfaceSupport.h"

namespace easymap
{

class IVectorFeature;
class IElement;
class IVectorLayerAgent;
class IElementLayerAgent;
typedef TSmartPtr<IVectorFeature> IVectorFeaturePtr;
typedef TSmartPtr<IElement> IElementPtr;
typedef TSmartPtr<IVectorLayerAgent> IVectorLayerAgentPtr;
typedef TSmartPtr<IElementLayerAgent> IElementLayerAgentPtr;


//0 - 普通要素； //1 - 注记
typedef long VectorFeatureType;
const VectorFeatureType VECTORFEATURETYPE_UNKNOWN     = -1;
const VectorFeatureType VECTORFEATURETYPE_GEOMETRY    = 0;
const VectorFeatureType VECTORFEATURETYPE_TEXT        = 1;

typedef long SlimRendererType;
const SlimRendererType SLIMRENDERERTYPE_SIMPLE      = 0;
const SlimRendererType SLIMRENDERERTYPE_UNIQUEVALUE = 1;
const SlimRendererType SLIMRENDERERTYPE_GRADE       = 2;

typedef struct
{
    //为了符号化等其它需要，要素层必须有统一的几何类型
    ShapeType ShpType;

    VectorFeatureType FeatureType;

    //坐标单位
    MapUnits MapUnit;

    //图层的原始比例尺，即1:1显示的时候所使用的显示比例尺
    double BaseScale;

    //图幅范围和坐标精度
    WKSRect DomainXY;
    double ToleranceXY;
    double MaxZ;
    double MinZ;
    double ToleranceZ;

    //索引参数
    dword SIParam1;
    dword SIParam2;
    double SIParam3;
}GeometryColumnInfo;


//================================================================================
//  矢量要素，支持几何对象的添加、删除、修改、选择、查询
//================================================================================
class IVectorFeature : public IObj
{
public:
    virtual dword __stdcall GetFID() const = 0;
    virtual bool __stdcall GetMBR(WKSRect& mbr) const = 0;
    virtual void __stdcall GetLayer(ILayer** ppLayer) = 0;
    virtual bool __stdcall SetGeometryRef(const IGeometry* const pGeometry) = 0;
    virtual void __stdcall GetGeometryRef(IGeometry** ppGeometry) = 0;
    virtual dword __stdcall GetFieldCount() const = 0;
    virtual bool __stdcall SetFieldValue(const dword index, const char* const fieldvalue) = 0;
    virtual bool __stdcall GetFieldValue(const dword index, IFieldValue** ppFieldValue) const = 0;
    virtual void __stdcall SetAnnotation(const char* const annotation) = 0;
    virtual const char* __stdcall GetAnnotation() const = 0;
    virtual bool __stdcall Delete() = 0;
    virtual bool __stdcall Update() = 0;
};


//================================================================================
//  用来搞定slimlayer和shapelayer
//================================================================================
class IVectorLayerAgent : public IObj
{
public:
    virtual bool __stdcall SetLayer(const ILayer* pLayer) = 0;
    virtual bool __stdcall GetLayer(ILayer** ppLayer) const = 0;

    virtual bool __stdcall CreateSlimLayer(
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
        ) = 0;

    virtual bool __stdcall LoadSlimLayer(
        const char* const   filename,
        const bool          readonly,
        const bool          filemap
        ) = 0;

    virtual bool __stdcall LoadShapeLayer(
        const char* const   filename,
        const MapUnits      mapunit,
        const double        basescale,
        const double        precision,
        const long          indexlevel,
        const bool          readonly
        ) = 0;

    virtual void __stdcall GetPrecision(double& precision) const = 0;
    virtual void __stdcall SetRefScale(const double scale) = 0;
    virtual void __stdcall GetRefScale(double& scale) const = 0;

    virtual dword __stdcall Select(const IIntArray* pFids, const bool append) = 0;
    virtual dword __stdcall Deselect(const IIntArray* pFids) = 0;
    virtual dword __stdcall GetSelection(IIntArray** ppFids) const = 0;

    virtual bool __stdcall GetFids(IIntArray** ppFids) const = 0;
    virtual dword __stdcall GetFeatureCount() const = 0;
    virtual bool __stdcall DeleteFeature(const dword fid) = 0;
    virtual bool __stdcall CreateFeature(IVectorFeature** ppFeature) = 0;
    virtual bool __stdcall GetFeature(const dword fid, IVectorFeature** ppFeature) = 0;

    //这个方法不管undo/redo，并且只要可能，就不会导致文件增大
    //可以用于GPS信号的实时刷新
    virtual bool __stdcall RapidModifyPoint(const dword fid, const WKSPoint& point) = 0;

    virtual bool __stdcall Identify(IIntArray** ppFids, const WKSRect& envelope,
        const bool partialselect) = 0;

    virtual void __stdcall GetGeometryColumnInfo(GeometryColumnInfo& geocolinfo) const = 0;

    virtual void __stdcall GetFields(IFields** ppFields) const = 0;
    virtual bool __stdcall SetDisplayField(const long fieldindex) = 0;
    virtual long __stdcall GetDisplayField() const = 0;

    virtual bool __stdcall SetDefaultSymbol(const ISymbol* pSymbol) = 0;
    virtual bool __stdcall GetDefaultSymbol(ISymbol** ppSymbol) const = 0;

    virtual void __stdcall SetRendererType(const SlimRendererType renderertype) = 0;
    virtual SlimRendererType __stdcall GetRendererType() const = 0;
    virtual bool __stdcall SetSymbol(const char* const key, const ISymbol* pSymbol) = 0;
    virtual bool __stdcall GetSymbol(const char* const key, ISymbol** ppSymbol) const = 0;
    virtual bool __stdcall GetSymbolByIndex(const dword index, IAnsiString** ppKey, ISymbol** ppSymbol) const = 0;
    virtual dword __stdcall GetSymbolCount() const = 0;
    virtual void __stdcall ClearSymbols() = 0;
    virtual bool __stdcall SetRendererField(const long fieldindex) = 0;
    virtual long __stdcall GetRendererField() const = 0;
    virtual void __stdcall SetShowDefaultSymbol(const bool showdefaultsymbol) = 0;
    virtual bool __stdcall GetShowDefaultSymbol() const = 0;

    virtual bool __stdcall ReadOnly() const = 0;

    //----------------------------------------------------------------------------
    //  网络分析
    //----------------------------------------------------------------------------
    //  设置容差，用于增加路径点、障碍点等
    virtual void __stdcall SetNetTolerance(const double tolerance) = 0;
    virtual double __stdcall GetNetTolerance() const = 0;

    //  创建拓扑网络，field < 0代表用geometry的长度创建图
    virtual bool __stdcall CreateNetTopo(const long field, const bool bidirectional) = 0;
    virtual bool __stdcall CreateNetTopo2(const dword field_from_to, const dword field_to_from) = 0;

    //  路径点
    virtual bool __stdcall AddNetRoute(const WKSPoint& route) = 0;
    virtual bool __stdcall RemoveNetRoute(const WKSPoint& route) = 0;
    virtual bool __stdcall GetNetRoutes(IMultiPoint** ppRoutes) const = 0;
    virtual void __stdcall ClearNetRoutes() = 0;

    //  障碍点
    virtual bool __stdcall AddNetBarrierPoint(const WKSPoint& barrier) = 0;
    virtual bool __stdcall RemoveNetBarrierPoint(const WKSPoint& barrier) = 0;
    virtual bool __stdcall GetNetBarrierPoints(IMultiPoint** ppBarriers) const = 0;
    virtual void __stdcall ClearNetBarrierPoints() = 0;

    //  障碍边
    virtual bool __stdcall AddNetBlockedBiEdge(const dword fid) = 0;
    virtual bool __stdcall AddNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to) = 0;
    virtual bool __stdcall RemoveNetBlockedBiEdge(const dword fid) = 0;
    virtual bool __stdcall RemoveNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to) = 0;
    virtual dword __stdcall GetNetBlockedEdgeCount() const = 0;
    virtual bool __stdcall GetNetBlockedEdgeByIndex(const dword i, dword& fid, WKSPoint& from, WKSPoint& to) const = 0;
    virtual bool __stdcall GetNetBlockedEdgeIDs(IIntArray** ppFids) const = 0;
    virtual void __stdcall ClearNetBlockedEdges() = 0;

    //  计算最佳路径
    virtual bool __stdcall DoBestPath(IPath** ppPath, IIntArray** ppFids) = 0;

    //  清除拓扑网络
    virtual void __stdcall ClearNetTopo() = 0;

    //  存储拓扑网络到数据文件中
    virtual bool __stdcall StoreNetTopo() = 0;
    virtual bool __stdcall RestoreNetTopo() = 0;
    //----------------------------------------------------------------------------

    virtual bool __stdcall Shapefile2ESD(const char* const esdfilename) const = 0;
};


//Element类型
typedef long ElementType;
const ElementType ELEMENTTYPE_GEOMETRY  = 1;
const ElementType ELEMENTTYPE_TEXT      = 2;
//================================================================================
//  图外要素
//================================================================================
class IElement : public IPersist
{
public:
    virtual DrawResult __stdcall Draw(
        const IDisplayCache*    pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope,
        const ITrackCancel*     pTrackCancel
        ) const = 0;

    virtual DrawResult __stdcall DrawSelected(
        const IDisplayCache*    pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope,
        const ITrackCancel*     pTrackCancel
        ) const = 0;

    virtual DrawResult __stdcall Draw1(const IDisplay* pDisplay) const = 0;

    virtual bool __stdcall DrawEx(const HDC dc, const IDisplayTransformation* pTrans) const = 0;
    virtual bool __stdcall DrawSelectedEx(const HDC dc, const IDisplayTransformation* pTrans) const = 0;

    virtual bool __stdcall Valid() const = 0;
    virtual ElementType __stdcall GetElementType() const = 0;
    virtual bool __stdcall Move(const double delta_x, const double delta_y) = 0;
    virtual bool __stdcall GetExtent(WKSRect& extent) const = 0;
    virtual bool __stdcall SelectTest(const WKSRect& envelope, const bool partialselect) const = 0;
    virtual void __stdcall Select() = 0;
    virtual void __stdcall Deselect() = 0;
    virtual bool __stdcall IsSelected() const = 0;
    virtual bool __stdcall SetGeometry(const IGeometry* pGeometry) = 0;
    virtual void __stdcall GetGeometry(IGeometry** ppGeometry) = 0;
    virtual void __stdcall SetText(const char* const text) = 0;
    virtual const char* __stdcall GetText() const = 0;
    virtual bool __stdcall SetSymbol(const ISymbol* const pSymbol) = 0;
    virtual bool __stdcall GetSymbol(const SymbolType symboltype, ISymbol** ppSymbol) const = 0;
    virtual void __stdcall SetReferenceScale(const double refscale) = 0;
    virtual void __stdcall GetReferenceScale(double& refscale) const = 0;
};


//================================================================================
//  用来搞定elementlayer
//================================================================================
class IElementLayerAgent : public IObj
{
public:
    virtual bool __stdcall SetLayer(const ILayer* pLayer) = 0;
    virtual bool __stdcall GetLayer(ILayer** ppLayer) const = 0;
    virtual bool __stdcall CreateElementLayer(const char* const layername) = 0;

    virtual void __stdcall SetRefScale(const double scale) = 0;
    virtual void __stdcall GetRefScale(double& scale) const = 0;

    virtual dword __stdcall AddElement(const IElement* pElement) = 0;
    virtual bool __stdcall GetElement(const dword id, IElement** ppElement) const = 0;
    virtual bool __stdcall SetElement(const dword id, const IElement* pElement) = 0;
    virtual bool __stdcall RemoveElement(const dword id) = 0;
    virtual bool __stdcall GetSelectElements(IIntArray** ppIDs) const = 0;
    virtual void __stdcall MoveSelectElements(const double delta_x, const double delta_y) = 0;
    virtual bool __stdcall RemoveSelectedElements() = 0;
    virtual bool __stdcall GetElementIDFromIndex(const dword index, dword& id) const = 0;
    virtual dword __stdcall GetElementCount() const = 0;
    virtual void __stdcall ClearElements() = 0;

    virtual bool __stdcall Identify(IIntArray** ppIds, const WKSRect& envelope,
        const bool partialselect) = 0;
};

}

#endif