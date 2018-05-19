#if !defined(SLIMLAYER_INCLUDED_)
#define SLIMLAYER_INCLUDED_

#include "CustomEditLayer.h"
#include "CellQuadTree.h"
#include "Fields.h"
#include "NetTopoBase.h"

using namespace easymap::net;

namespace easymap
{

class CVectorLayer;
typedef TSmartPtr<CVectorLayer> CVectorLayerPtr;

class CSlimLayer;
typedef TSmartPtr<CSlimLayer> CSlimLayerPtr;

class CSlimFeature;
typedef TSmartPtr<CSlimFeature> CSlimFeaturePtr;

typedef long SlimEditType;
const SlimEditType SLIMEDIT_NEW     = 1;
const SlimEditType SLIMEDIT_MODIFY  = 2;
const SlimEditType SLIMEDIT_DELETE  = 3;

const long SLIM_RESERVED_LENGTH = 250;

//================================================================================
//  矢量Layer，支持几何对象的添加、删除、修改、选择、查询
//  搞这个是为了一起搞定SlimLayer和ShapeLayer
//================================================================================
class CVectorLayer : public CCustomEditLayer
{
CLASS_NAME(CVectorLayer)    //注意派生类必须改写_GetClassName()函数！

protected:
    CVectorLayer();

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp)
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
            || (0 == strcmp(interfacename, "CVectorLayer")))
        {
            *pp = this;
        }
        else
        {
            return this->_GotoInterface(interfacename, pp);//由派生类支持具体的类名
        }

        static_cast<IObj*>(*pp)->_AddRef();
        return true;
    };

private:
    //由派生类支持具体的类名
    virtual bool _GotoInterface(const char* const interfacename, void** pp) = 0;

public:
    virtual void GetPrecision(double& precision) const = 0;
    virtual void SetRefScale(const double& scale) = 0;
    virtual void GetRefScale(double& scale) const = 0;

    virtual dword Select(const vector<dword>& fids, const bool append = true) = 0;
    virtual dword Deselect(const vector<dword>& fids) = 0;
    virtual dword GetSelection(vector<dword>& fids) const = 0;

    virtual bool GetFids(vector<dword>& fids) const = 0;
    virtual dword GetFeatureCount() const = 0;
    virtual dword AddFeature(const IGeometryPtr pGeometry, const string& fieldvalues,
        const string& annotation) = 0;
    virtual bool GetFeature(const dword fid, IGeometryPtr& pGeometry,
        string& fieldvalues, string& annotation) = 0;
    virtual bool GetFeatureGeometry(const dword fid, IGeometryPtr& pGeometry) const = 0;
    virtual bool GetFeatureMBR(const dword fid, WKSRect& mbr) const = 0;
    virtual bool DeleteFeature(const dword fid) = 0;
    virtual bool CreateFeature(IVectorFeaturePtr& pFeature) = 0;
    virtual bool GetFeature(const dword fid, IVectorFeaturePtr& pFeature) = 0;

    virtual bool RapidModifyPoint(const dword fid, const WKSPoint& point) = 0;

    virtual bool Identify(vector<dword>& fids, const WKSRect& envelope,
        const bool partialselect = true) = 0;

    virtual bool ImpreciseSearch(const WKSRect& extent, vector<dword>& fids) = 0;

    virtual bool SetFields(const string& fields) = 0;
    virtual void GetFields(string& fields) const = 0;

    virtual void GetGeometryColumnInfo(GeometryColumnInfo& geocolinfo) const = 0;

    virtual void GetFields(CFieldsPtr& pFields) const = 0;
    virtual bool SetDisplayField(const long fieldindex) = 0;
    virtual long GetDisplayField() const = 0;

    virtual bool SetDefaultSymbol(const ISymbolPtr pSymbol, const bool save2esd) = 0;
    virtual bool GetDefaultSymbol(ISymbolPtr& pSymbol) const = 0;

    virtual void SetRendererType(const SlimRendererType renderertype, const bool save2esd) = 0;
    virtual SlimRendererType GetRendererType() const = 0;
    virtual bool SetSymbol(const string& key, const ISymbolPtr pSymbol) = 0;
    virtual bool GetSymbol(const string& key, ISymbolPtr& pSymbol) const = 0;
    virtual bool GetSymbolByIndex(const dword index, string& key, ISymbolPtr& pSymbol) const = 0;
    virtual dword GetSymbolCount() const = 0;
    virtual void ClearSymbols() = 0;
    virtual bool SetRendererField(const long fieldindex, const bool save2esd) = 0;
    virtual long GetRendererField() const = 0;
    virtual void SetShowDefaultSymbol(const bool showdefaultsymbol, const bool save2esd) = 0;
    virtual bool GetShowDefaultSymbol() const = 0;

    virtual bool ReadOnly() const = 0;

    //----------------------------------------------------------------------------
    //  网络分析
    //----------------------------------------------------------------------------
    //  设置容差，用于增加路径点、障碍点等
    virtual void SetNetTolerance(const double tolerance) = 0;
    virtual double GetNetTolerance() const = 0;

    //  创建拓扑网络，field < 0代表用geometry的长度创建图
    virtual bool CreateNetTopo(const long field, const bool bidirectional) = 0;
    virtual bool CreateNetTopo2(const dword field_from_to, const dword field_to_from) = 0;

    //  路径点
    virtual bool AddNetRoute(const WKSPoint& route) = 0;
    virtual bool RemoveNetRoute(const WKSPoint& route) = 0;
    virtual bool GetNetRoutes(IMultiPoint** ppRoutes) const = 0;
    virtual void ClearNetRoutes() = 0;

    //  障碍点
    virtual bool AddNetBarrierPoint(const WKSPoint& barrier) = 0;
    virtual bool RemoveNetBarrierPoint(const WKSPoint& barrier) = 0;
    virtual bool GetNetBarrierPoints(IMultiPoint** ppBarriers) const = 0;
    virtual void ClearNetBarrierPoints() = 0;

    //  障碍边
    virtual bool AddNetBlockedBiEdge(const dword fid) = 0;
    virtual bool AddNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to) = 0;
    virtual bool RemoveNetBlockedBiEdge(const dword fid) = 0;
    virtual bool RemoveNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to) = 0;
    virtual dword GetNetBlockedEdgeCount() const = 0;
    virtual bool GetNetBlockedEdgeByIndex(const dword i, dword& fid, WKSPoint& from, WKSPoint& to) const = 0;
    virtual bool GetNetBlockedEdgeIDs(vector<dword>& fids) const = 0;
    virtual void ClearNetBlockedEdges() = 0;

    //  计算最佳路径
    virtual bool DoBestPath(IPath** ppPath, IIntArray** ppFids) = 0;

    //  清除拓扑网络
    virtual void ClearNetTopo() = 0;

    //  存储拓扑网络到数据文件中
    virtual bool StoreNetTopo() = 0;
    virtual bool RestoreNetTopo() = 0;
    //----------------------------------------------------------------------------
};


//================================================================================
//  SlimData 矢量Layer，既可全部放在内存中，也可放在文件中
//  注意创建时为在内存中，执行了AttachToFile()后就放在文件中了
//================================================================================
class CSlimLayer : public CVectorLayer
{
//这个类必须手工CLASS_NAME(CSlimLayer)
friend class _ClassFactory_CSlimLayer;
PERSIST_DUMP(CSlimLayer)
EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    //--------- SlimLayer基本属性 ---------
    struct SlimHeader
    {
        char            Reserved[SLIM_RESERVED_LENGTH];
        char            Alias[120];
        char            MainVersion;
        char            SubVersion;
        dword           IndexBlockSize;
        char            FieldsInfo[2000];
        long            DisplayField;
    };

private:
    struct Feature
    {
        dword           fid;
        WKSRect         mbr;
        IGeometryPtr    geometry;
        string          fieldvalues;
        string          annotation;
    };

    //编辑的原子操作
    struct EditAtom
    {
        SlimEditType edittype;
        dword fid;
        dword olddelta;
        dword newdelta;
        WKSRect oldmbr;
        WKSRect newmbr;
    };

    //每次编辑undo的内容，由一个或多个原子操作组成
    struct EditProcess
    {
        vector<EditAtom> atoms;
    };


private:
    CSlimLayer();
    CSlimLayer(const CSlimLayer& slimlayer);
public:
    //创建一个SlimLayer，放在内存中
    CSlimLayer(
        const ShapeType     shapetype,
        const MapUnits      mapunit,
        const double&       basescale,
        const double&       precision,
        const WKSRect&      extent,
        const long          indexlevel,
        const CFieldsPtr    pFields,
        const bool          annotation
        );

    //打开一个已经存在的文件
    //  filemap = true  -> 使用内存映射文件，高速，消耗内存
    //  filemap = false -> 使用随机文件存取方式，最小内存占用，支持大文件
    CSlimLayer(const string& filename, const bool readonly = false,
        const bool filemap = true);
private:
    ~CSlimLayer();

    //准备好等待监听的事件类型
    void InitEventTypes();

    //初始化一些飞机
    void InitialSlimLayer(CStreamPtr pStream);

    //将文件绑定到this
    void FromFile(const string& filename, const bool readonly,
        const bool filemap);

    //...
    bool CheckHeader() const;

    //搞不定了
    void GotoHell();

private:
    SlimHeader              m_Header;
    GeometryColumnInfo      m_GeoColumnInfo;
    WKSRect                 m_DataExtent;
    dword                   m_MaxFID;
    dword                   m_FeatureCount;

    double                  m_ReferenceScale;
    CStreamPtr              m_pFeatureData;
    CStreamPtr              m_pIndexStream;
    map<dword, dword>       m_FidIndex;         //fid索引：fid -> delta
    CMultiQuadTreePtr       m_pSpatialIndex;
    list<dword>             m_SelectedFIDs;
    ISymbolPtr              m_pDefaultSymbol;
    map<string, ISymbolPtr> m_SymbolMap;
    long                    m_RendererField; //-1: fid  从0开始为第一个属性字段
    SlimRendererType        m_RendererType;
    bool                    m_ShowDefaultSymbol;
    IPointPtr               m_pAuxPoint;    //用于加速RapidModifyPoint()

    vector<EditProcess>     m_UndoProcs;        //记录每一次编辑操作（每一次都由多个原子操作组成）
    EditProcess             m_EditProc;         //从上一次SetUndoPoint之后的所有改动
                                                //如果为empty则没有任何修改

    long                    m_UndoIndex;        //记录当前将要undo的是哪一个process
    bool                    m_SaveDirty;

    bool                    m_ModifyDirty;      //是否执行过添加、删除、修改要素的操作，在savedata中
                                                //依据这个来决定是否Index2DataStream

    FILETIME                m_LastTime;

    NetTopo                 m_Net;
    bool                    m_FastShortest;

    string        m_SR;

private:
    void SelectObjectsByEnvelope(vector<dword>& resultfids, const WKSRect& envelope,
        const bool partialselect);
    void Feature2Stream(const Feature& feature);   //将feature写入m_pFeatureData当前位置
    void Stream2Feature(Feature& feature);         //从m_pFeatureData当前位置中读取feature

    void Index2Stream();
    void Stream2Index();

    void Index2DataStream();                    //将索引写入主数据文件
    void DataStream2Index();                    //从主数据文件中读入索引

    void Symbols2Stream();
    void Stream2Symbols();

    void UpdateFixedHeader();
    void ReloadFixedHeader();

    bool EditUndoRestoreIndex(const EditProcess& ep);   //用ep的内容恢复索引
    bool EditRedoRestoreIndex(const EditProcess& ep);   //用ep的内容恢复索引

    void CleanEditUndo();                       //扔掉没有setundopoint的内容

    void WriteModifyTime();
    bool ModifiedByOther();
    void RereadIndexStream();

private:
    dword PresaveInstance(CStreamPtr pStream, void* const assist) const;
    dword PreloadInstance(CStreamPtr pStream, void* const assist);

    DrawResult DrawLayerData(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );

    DrawResult DrawLayerSelection(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );

    bool GetSlimData(CStreamPtr pStream);

public:
    const char* const __stdcall _GetClassName() const//!
    {
        return "CSlimLayer";
    };

    bool _GotoInterface(const char* const interfacename, void** pp);

    bool __stdcall GetExtent(WKSRect& fullext) const;
    MapUnits __stdcall GetMapUnit() const;
    bool __stdcall GetBaseScale(double& scale) const;
    const char* __stdcall GetSpatialReference() const;
    void GetPrecision(double& precision) const;
    void SetRefScale(const double& scale);
    void GetRefScale(double& scale) const;

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

    //用来支持质地填充等
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

    bool Identify(vector<dword>& fids, const WKSRect& envelope,
        const bool partialselect = true);

    bool ImpreciseSearch(const WKSRect& extent, vector<dword>& fids);

    bool SetFields(const string& fields);
    void GetFields(string& fields) const;

    void GetFields(CFieldsPtr& pFields) const;
    bool SetDisplayField(const long fieldindex);
    long GetDisplayField() const;

    bool SetAlias(const string& alias);
    string GetAlias() const;

    void GetGeometryColumnInfo(GeometryColumnInfo& geocolinfo) const;
    bool SetSR(const string& sr);

    //绑定到指定的文件上
    //  filemap = true  -> 使用内存映射文件，高速，消耗内存
    //  filemap = false -> 使用随机文件存取方式，最小内存占用，支持大文件
    bool AttachToFile(const string& filename, const bool filemap = true);

    //返回绑定的内存映射文件名，如果在内存中就返回""
    string GetFileName() const;

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

friend class CShapeLayer;
};
//================================================================================
CLASS_FACTORY(CSlimLayer)

//================================================================================
//  用于辅助操作slimlayer中的要素
//================================================================================
class CSlimFeature : public IVectorFeature
{
CLASS_NAME(CSlimFeature)
EVENTS_DISPATCHER
NO_EVENTS_LISTENER

private:
    CSlimFeature();
    ~CSlimFeature();

private:
    dword           m_fid;
    IGeometryPtr    m_geometry;
    CFieldValuesPtr m_pFieldValues;
    string          m_Annotation;
    CSlimLayerPtr   m_slimlayer;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

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

    void __stdcall GetLayer(ILayer** ppLayer);
    bool __stdcall SetGeometryRef(const IGeometry* const pGeometry);
    void __stdcall GetGeometryRef(IGeometry** ppGeometry);
    dword __stdcall GetFieldCount() const;
    bool __stdcall SetFieldValue(const dword index, const char* const fieldvalue);
    bool __stdcall GetFieldValue(const dword index, IFieldValue** ppFieldValue) const;
    void __stdcall SetAnnotation(const char* const annotation);
    const char* __stdcall GetAnnotation() const;

friend class CSlimLayer;
};
//================================================================================

bool CheckTrackCancel(const ITrackCancelPtr pTrackCancel, const long cacheid);

}

#endif
