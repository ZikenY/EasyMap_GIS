#if !defined(SHAPELAYER_INCLUDED_)
#define SHAPELAYER_INCLUDED_

#include "SlimLayer.h"

namespace easymap
{

class CShapeLayer;
typedef TSmartPtr<CShapeLayer> CShapeLayerPtr;

class CShapeFeature;
typedef TSmartPtr<CShapeFeature> CShapeFeaturePtr;

class CShapeLayer : public CVectorLayer
{
//CLASS_NAME(CShapeLayer)
friend class _ClassFactory_CShapeLayer;
PERSIST_DUMP(CShapeLayer)
EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CShapeLayer(
        const string&       filename,
        const MapUnits      mapunit,
        const double&       basescale,
        const double&       precision,
        const long          indexlevel,
        const bool          readonly
        );

    ~CShapeLayer();

private:
    CShapeLayer();

    //准备好等待监听的事件类型
    void InitEventTypes();

    bool Init(
        const string&       filename,
        const MapUnits      mapunit,
        const double&       basescale,
        const double&       precision,
        const long          indexlevel,
        const bool          readonly
        );

    bool LockFile();
    bool UnlockFile();
    bool ModifiedByOther();
    void RereadShapeFile();

private:
    string m_SHPName;
    CSlimLayerPtr m_pSlimLayer;
    CFieldsPtr m_pFields;
    bool m_ReadOnly;

    //用于锁住shapefile
    HANDLE m_hSHP;
    HANDLE m_hSHX;
    HANDLE m_hDBF;

    //用于判断shapefile是否被别人改了
    FILETIME m_LastTime;

private:
    dword PresaveInstance(CStreamPtr pStream, void* const assist) const;
    dword PreloadInstance(CStreamPtr pStream, void* const assist);

    DrawResult DrawLayerData(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope,
        const ITrackCancelPtr   pTrackCancel
        );

    DrawResult DrawLayerSelection(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope,
        const ITrackCancelPtr   pTrackCancel
        );

    //由ShapeFeature调用，用于发送消息
    void NoticeFeatureAdded(const dword fid) const;
    void NoticeFeatureModified(const dword fid) const;

public:
    const char* const __stdcall _GetClassName() const//!
    {
        return "CShapeLayer";
    };

    bool _GotoInterface(const char* const interfacename, void** pp);

    DrawResult DrawData1(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );

    DrawResult DrawSelection(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );


    bool __stdcall GetExtent(WKSRect& fullext) const;
    MapUnits __stdcall GetMapUnit() const;
    bool __stdcall GetBaseScale(double& scale) const;
    const char* __stdcall GetSpatialReference() const;
    void GetPrecision(double& precision) const;
    void SetRefScale(const double& scale);
    void GetRefScale(double& scale) const;

    void __stdcall SetAlpha(const byte alpha);
    byte __stdcall GetAlpha() const;

    dword Select(const WKSPoint& point, const bool append = true);
    dword __stdcall Select(const WKSRect& envelope, const bool partialselect = true,
        const bool append = true);
    dword Select(const vector<dword>& fids, const bool append = true);
    dword Deselect(const WKSPoint& point);
    dword __stdcall Deselect(const WKSRect& envelope, const bool partialselect = true);
    dword Deselect(const vector<dword>& fids);
    dword GetSelection(vector<dword>& fids) const;
    dword __stdcall GetSelectCount() const;
    void __stdcall ClearSelection();

    bool __stdcall SetUndoPoint();
    bool __stdcall EditUndoable() const;
    bool __stdcall EditRedoable() const;
    bool __stdcall EditUndo();
    bool __stdcall EditRedo();
    bool __stdcall EditCancel();
    bool __stdcall SaveData();
    bool __stdcall IsDirty() const;

    bool SetDefaultSymbol(const ISymbolPtr pSymbol, const bool save2esd);
    bool GetDefaultSymbol(ISymbolPtr& pSymbol) const;

    void SetRendererType(const SlimRendererType renderertype, const bool save2esd);
    SlimRendererType GetRendererType() const;
    bool SetSymbol(const string& key, const ISymbolPtr pSymbol);
    bool GetSymbol(const string& key, ISymbolPtr& pSymbol) const;
    bool GetSymbolByIndex(const dword index, string& key, ISymbolPtr& pSymbol) const;
    dword GetSymbolCount() const;
    void ClearSymbols();
    bool SetRendererField(const long fieldindex, const bool save2esd);
    long GetRendererField() const;
    void SetShowDefaultSymbol(const bool showdefaultsymbol, const bool save2esd);
    bool GetShowDefaultSymbol() const;

    bool GetFids(vector<dword>& fids) const;
    dword GetFeatureCount() const;
    dword AddFeature(const IGeometryPtr pGeometry, const string& fieldvalues,
        const string& annotation);
    bool SetFeature(const dword fid, const IGeometryPtr pGeometry,
        const string& fieldvalues, const string& annotation);
    bool GetFeature(const dword fid, IGeometryPtr& pGeometry,
        string& fieldvalues, string& annotation);
    bool GetFeatureGeometry(const dword fid, IGeometryPtr& pGeometry) const;
    bool GetFeatureMBR(const dword fid, WKSRect& mbr) const;
    bool DeleteFeature(const dword fid);

    bool CreateFeature(IVectorFeaturePtr& pFeature);
    bool GetFeature(const dword fid, IVectorFeaturePtr& pFeature);

    bool RapidModifyPoint(const dword fid, const WKSPoint& point);

    bool Identify(vector<dword>& fids, const WKSRect& envelope,
        const bool partialselect = true);

    bool ImpreciseSearch(const WKSRect& extent, vector<dword>& fids);

    bool SetFields(const string& fields);
    void GetFields(string& fields) const;

    bool SetAlias(const string& alias);
    string GetAlias() const;

    void GetGeometryColumnInfo(GeometryColumnInfo& geocolinfo) const;

    void GetFields(CFieldsPtr& pFields) const;
    bool SetDisplayField(const long fieldindex);
    long GetDisplayField() const;

    bool SetSR(const string& sr);

    string GetShapeFileName() const;
    bool ReadOnly() const;

    void SetNetTolerance(const double tolerance);
    double GetNetTolerance() const;
    bool CreateNetTopo(const long field, const bool bidirectional);
    bool CreateNetTopo2(const dword field_from_to, const dword field_to_from);
    bool AddNetRoute(const WKSPoint& route);
    bool RemoveNetRoute(const WKSPoint& route);
    bool GetNetRoutes(IMultiPoint** ppRoutes) const;
    void ClearNetRoutes();
    bool AddNetBarrierPoint(const WKSPoint& barrier);
    bool RemoveNetBarrierPoint(const WKSPoint& barrier);
    bool GetNetBarrierPoints(IMultiPoint** ppBarriers) const;
    void ClearNetBarrierPoints();
    bool AddNetBlockedBiEdge(const dword fid);
    bool AddNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to);
    bool RemoveNetBlockedBiEdge(const dword fid);
    bool RemoveNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to);
    dword GetNetBlockedEdgeCount() const;
    bool GetNetBlockedEdgeByIndex(const dword i, dword& fid, WKSPoint& from, WKSPoint& to) const;
    bool GetNetBlockedEdgeIDs(vector<dword>& fids) const;
    void ClearNetBlockedEdges();
    bool DoBestPath(IPath** ppPath, IIntArray** ppFids);
    void ClearNetTopo();
    bool StoreNetTopo();
    bool RestoreNetTopo();

    bool Valid() const;

    bool SaveData2ESD(const string& esdfilename);

friend class CShapeFeature;
};
CLASS_FACTORY(CShapeLayer)


class CShapeFeature : public IVectorFeature
{
CLASS_NAME(CShapeFeature)
EVENTS_DISPATCHER
NO_EVENTS_LISTENER

private:
    CShapeFeature(const CShapeLayerPtr pShapeLayer,
        const CSlimFeaturePtr pSlimFeature);
    ~CShapeFeature();

private:
    CShapeFeature();

    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    CShapeLayerPtr m_pShapeLayer;
    CSlimFeaturePtr m_pSlimFeature;

public:
    dword __stdcall GetFID() const;
    bool __stdcall GetMBR(WKSRect& mbr) const;
    void GetLayer(CVectorLayerPtr& pLayer);
    bool SetGeometryRef(const IGeometryPtr pGeometry);
    void GetGeometryRef(IGeometryPtr &pGeometry);
    bool SetGeometry(const IGeometryPtr pGeometry);
    void GetGeometry(IGeometryPtr &pGeometry);
    void SetFieldValues(const string& fieldvalues);
    string FieldValuesAsString() const;
    bool __stdcall Delete();
    bool __stdcall Update();

    void GetFields(CFieldsPtr& pFields) const;
    bool SetFieldValue(const dword index, const CFieldValuePtr pValue);
    bool GetFieldValue(const dword index, CFieldValuePtr& pValue) const;
    bool FieldValueAsString(const dword index, string& value) const;

    void __stdcall GetLayer(ILayer** ppLayer);
    bool __stdcall SetGeometryRef(const IGeometry* const pGeometry);
    void __stdcall GetGeometryRef(IGeometry** ppGeometry);
    dword __stdcall GetFieldCount() const;
    bool __stdcall SetFieldValue(const dword index, const char* const fieldvalue);
    bool __stdcall GetFieldValue(const dword index, IFieldValue** ppFieldValue) const;
    void __stdcall SetAnnotation(const char* const annotation);
    const char* __stdcall GetAnnotation() const;

friend class CShapeLayer;
};

}

#endif
