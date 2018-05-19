#if !defined(GEOMETRYLABEL_INCLUDED_)
#define GEOMETRYLABEL_INCLUDED_

#include "CommonInclude.h"
#include "CellNonupleTree.h"
#include "Geometry.h"

namespace easymap
{

class CGeometryLabel;
typedef TSmartPtr<CGeometryLabel> CGeometryLabelPtr;

class CLabelCNFilter;
typedef TSmartPtr<CLabelCNFilter> CLabelCNFilterPtr;

//------------------------------------------------------------------------
//  用九叉树检索算法计算label点
//------------------------------------------------------------------------
class CGeometryLabel : public IPersist
{
CLASS_NAME(CGeometryLabel)
PERSIST_DUMP(CGeometryLabel)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

    //返回计算结果，label点
    class CLabelCNResult : public CCellNonupleTree::CCellNonupleResult
    {
    CLASS_NAME(CLabelCNResult)
    NO_EVENTS_DISPATCHER
    NO_EVENTS_LISTENER
    private:
        CLabelCNResult();
        CLabelCNResult(const double x, const double y);
        ~CLabelCNResult();

        WKSPoint m_Result;

    public:
        bool __stdcall GotoInterface(const char* const interfacename, void** pp);
        bool __stdcall Clone(IObj** ppObj) const;

        void GetResult(double& x, double& y) const;

    friend class CLabelCNFilter;
    };
    typedef TSmartPtr<CLabelCNResult> CLabelCNResultPtr;

public:
    CGeometryLabel();
private:
    ~CGeometryLabel(){};

private:
    IGeometryPtr m_pGeometry;

    bool GetRingLabelPoint(const IRingPtr pRing, double& x, double& y) const;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    void SetGeometry(const IGeometryPtr pGeometry);
    void GetGeometry(IGeometryPtr& pGeometry) const;
    bool GetLabelPoints(vector<WKSPoint>& labelpoints) const;
};

//------------------------------------------------------------------------
//  过滤器，保存一个Geometry，供CGeometryLabel实用
//------------------------------------------------------------------------
class CLabelCNFilter : public CCellNonupleTree::CCellNonupleFilter
{
CLASS_NAME(CLabelCNFilter)
PERSIST_DUMP(CLabelCNFilter)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

private:
    CLabelCNFilter();
    ~CLabelCNFilter(){};

    IGeometryPtr m_Geometry;

    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    bool CheckExtent(const WKSRect& extent, const long level) const;
    void Search(const double& x, const double& y, const long level,
        CCellNonupleTree::CCellNonupleResultPtr& pResult) const;

    void GetGeometry(IGeometryPtr& pGeometry) const;

friend class CGeometryLabel;
};

CLASS_FACTORY(CGeometryLabel)
CLASS_FACTORY(CLabelCNFilter)

}

#endif