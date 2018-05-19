#include "CommonInclude.h"
#include "GeometryLabel.h"
#include "MathLib.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CGeometryLabel)
CLASS_FACTORY_INSTANCE(CLabelCNFilter)

//---------------------------------------------------------------------------
//  CLabelCNResult
//---------------------------------------------------------------------------
CGeometryLabel::CLabelCNResult::CLabelCNResult(const double x, const double y)
{
    INIT_REFCOUNT

    m_Result.x = x;
    m_Result.y = y;
}

CGeometryLabel::CLabelCNResult::~CLabelCNResult()
{
}

bool __stdcall CGeometryLabel::CLabelCNResult::Clone(IObj** ppObj) const
{
    return false;
}

bool __stdcall CGeometryLabel::CLabelCNResult::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CLabelCNResult")))
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

void CGeometryLabel::CLabelCNResult::GetResult(double& x, double& y) const
{
    x = m_Result.x;
    y = m_Result.y;
}
//---------------------------------------------------------------------------
//  CLabelCNResult
//---------------------------------------------------------------------------



//---------------------------------------------------------------------------
//  CLabelCNFilter
//---------------------------------------------------------------------------
CLabelCNFilter::CLabelCNFilter()
{
    INIT_REFCOUNT
}

bool __stdcall CLabelCNFilter::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CLabelCNFilter")))
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

bool __stdcall CLabelCNFilter::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CLabelCNFilter::Clone(IObjPtr& pObj) const
{
    CLabelCNFilterPtr pFilter = new CLabelCNFilter();

    if (m_Geometry.Assigned())
    {
        IObjPtr pObjTmp;
        CLONE_PTR(m_Geometry, pObjTmp)
        CAST_PTR(pObjTmp, pFilter->m_Geometry, IGeometry)
    }

    CAST_PTR(pFilter, pObj, IObj)
    return true;
}

dword __stdcall CLabelCNFilter::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    long flag = 1;
    if (m_Geometry.Assigned())
    {
        pStream->WriteData(&flag, sizeof(long));
        m_Geometry->_DumpTo(pStream, assist);
    }
    else
    {
        flag = 0;
        pStream->WriteData(&flag, sizeof(long));
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CLabelCNFilter::_LoadInstance(IStreamX* pStream, void* const assist)
{
    m_Geometry.Clear();

    dword oldpos = pStream->GetPos();

    long flag;
    pStream->ReadData(&flag, sizeof(long));
    if (flag)
    {
        //恢复过滤器
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom((CStream*)pStream, pPersist, assist);
        CAST_PTR(pPersist, m_Geometry, IGeometry)
    }

    return pStream->GetPos() - oldpos;
}

bool CLabelCNFilter::CheckExtent(const WKSRect& extent, const long level) const
{
    if (!m_Geometry.Assigned())
    {
        return false;
    }

    return m_Geometry->Select(extent);
}

void CLabelCNFilter::Search(const double& x, const double& y, const long level,
    CCellNonupleTree::CCellNonupleResultPtr& pResult) const
{
    pResult.Clear();
    if (!m_Geometry.Assigned())
    {
        return;
    }

    WKSRect centerrect;
    centerrect.left = centerrect.right = x;
    centerrect.top = centerrect.bottom = y;

    //如果中心点成功，返回result
    if (m_Geometry->Select(centerrect))
    {
        pResult = new CGeometryLabel::CLabelCNResult(centerrect.left, centerrect.bottom);
    }
}

void CLabelCNFilter::GetGeometry(IGeometryPtr& pGeometry) const
{
    pGeometry = m_Geometry;
}
//---------------------------------------------------------------------------
//  CLabelCNFilter
//---------------------------------------------------------------------------



//---------------------------------------------------------------------------
//  CGeometryLabel
//---------------------------------------------------------------------------
CGeometryLabel::CGeometryLabel()
{
    INIT_REFCOUNT
}

bool __stdcall CGeometryLabel::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CGeometryLabel")))
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

bool __stdcall CGeometryLabel::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CGeometryLabel::Clone(IObjPtr& pObj) const
{
    CGeometryLabelPtr pGL = new CGeometryLabel;

    IObjPtr pObjTmp;
    if (m_pGeometry.Assigned())
    {
        CLONE_PTR(m_pGeometry, pObjTmp)
        CAST_PTR(pObjTmp, pGL->m_pGeometry, IGeometry)
    }

    pObj = pGL._p();
    return true;
}

dword __stdcall CGeometryLabel::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    long flag = 1;
    if (m_pGeometry.Assigned())
    {
        //保存geometry
        pStream->WriteData(&flag, sizeof(long));
        m_pGeometry->_DumpTo(pStream, assist);
    }
    else
    {
        flag = 0;
        pStream->WriteData(&flag, sizeof(long));
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CGeometryLabel::_LoadInstance(IStreamX* pStream, void* const assist)
{
    m_pGeometry.Clear();

    dword oldpos = pStream->GetPos();

    long flag;
    pStream->ReadData(&flag, sizeof(long));
    if (flag)
    {
        //恢复geometry
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom((CStream*)pStream, pPersist, assist);
        CAST_PTR(pPersist, m_pGeometry, IGeometry)
    }

    return pStream->GetPos() - oldpos;
}

void CGeometryLabel::SetGeometry(const IGeometryPtr pGeometry)
{
    m_pGeometry = pGeometry;
}

void CGeometryLabel::GetGeometry(IGeometryPtr& pGeometry) const
{
    pGeometry = m_pGeometry;
}

inline bool GetPathLabelPoint(const IPathPtr pPath, double& x, double& y)
{
    if (pPath->GetPointCount() < 2)
    {
        return false;
    }

    double halflength, length_tmp = 0;
    pPath->GetLength(halflength);
    halflength = halflength / 2;
    WKSPointZ point1, point2;
    pPath->GetPoint1(0, point1);
    dword pointcount = pPath->GetPointCount();
    for (dword i = 1; i < pointcount; i++)
    {
        pPath->GetPoint1(i, point2);

        //找中心点附近
        length_tmp += mathlib::Distance(point1, point2);
        if (length_tmp >= halflength)
        {
            x = (point1.x + point2.x) / 2;
            y = (point1.y + point2.y) / 2;
            return true;
        }

        point1 = point2;
    }

    return true;
}

bool CGeometryLabel::GetRingLabelPoint(const IRingPtr pRing, double& x, double& y) const
{
    if (pRing->GetPointCount() == 0)
    {
        return false;
    }

    CCellNonupleTree::CNTParams paramerters;
    pRing->GetMBR(paramerters.extent);
    paramerters.level = 3;
    CCellNonupleTreePtr pCNT = new CCellNonupleTree(paramerters);

    CLabelCNFilterPtr pFilter = new CLabelCNFilter;
    CAST_PTR(pRing, pFilter->m_Geometry, IGeometry)
    pCNT->SetFilter(pFilter._p());

    CCellNonupleTree::CCellNonupleResultPtr pResult;
    pCNT->Search(pResult);
    if (!pResult.Assigned())
    {
        return false;
    }

    CLabelCNResultPtr pLabelCNResult;
    CAST_PTR(pResult, pLabelCNResult, CLabelCNResult)
    pLabelCNResult->GetResult(x, y);
    return true;
}

bool CGeometryLabel::GetLabelPoints(vector<WKSPoint>& labelpoints) const
{
    if (!m_pGeometry.Assigned())
    {
        return false;
    }

    bool r = false;
    dword i, segcount = 0;
    WKSPointZ labelpoint;
    WKSRect wksrect;

    GeometryType geotype = m_pGeometry->GetGeometryType();
    IRingPtr pRing;
    if (geotype == GEOMETRYTYPE_RING)
    {
        CAST_PTR(m_pGeometry, pRing, IRing)
        r = this->GetRingLabelPoint(pRing, labelpoint.x, labelpoint.y);
        if (!r)
        {
            pRing->GetMBR(wksrect);
            GetRectCenter(wksrect, labelpoint);
        }

        labelpoints.push_back(labelpoint);
        return r;
    }

    if (geotype == GEOMETRYTYPE_POLYGON)
    {
        IPolygonPtr pPolygon;
        CAST_PTR(m_pGeometry, pPolygon, IPolygon)
        segcount = pPolygon->GetRingCount();
        for (i = 0; i < segcount; i++)
        {
            pRing.Clear();
            pPolygon->GetRingRef(pRing._ref(), i);
            if (this->GetRingLabelPoint(pRing, labelpoint.x, labelpoint.y))
            {
                r = true;
            }
            else
            {
                pRing->GetMBR(wksrect);
                GetRectCenter(wksrect, labelpoint);
            }

            labelpoints.push_back(labelpoint);
        }

        return r;
    }

    IPointPtr pGeoPoint;
    if (geotype == GEOMETRYTYPE_POINT)
    {
        CAST_PTR(m_pGeometry, pGeoPoint, IPoint)
        pGeoPoint->GetCoordinates(labelpoint);
        labelpoints.push_back(labelpoint);
        return true;
    }

    if (geotype == GEOMETRYTYPE_MULTIPOINT)
    {
        IMultiPointPtr pMultiPoint;
        CAST_PTR(m_pGeometry, pMultiPoint, IMultiPoint)
        segcount = pMultiPoint->GetPointCount();
        if (segcount == 0)
        {
            return false;
        }

        for (i = 0; i < segcount; i++)
        {
            pMultiPoint->GetPoint(labelpoint, i);
            labelpoints.push_back(labelpoint);
        }

        return true;
    }

    IPathPtr pPath;
    if (geotype == GEOMETRYTYPE_PATH)
    {
        CAST_PTR(m_pGeometry, pPath, IPath);
        r = GetPathLabelPoint(pPath, labelpoint.x, labelpoint.y);
        if (r)
        {
            labelpoints.push_back(labelpoint);
        }

        return r;
    }

    if (geotype == GEOMETRYTYPE_POLYLINE)
    {
        IPolylinePtr pPolyline;
        CAST_PTR(m_pGeometry, pPolyline, IPolyline);
        segcount = pPolyline->GetPathCount();
        for (i = 0; i < segcount; i++)
        {
            pPath.Clear();
            pPolyline->GetPathRef(pPath._ref(), i);
            if (GetPathLabelPoint(pPath, labelpoint.x, labelpoint.y))
            {
                labelpoints.push_back(labelpoint);
                r = true;
            }
        }

        return r;
    }

    WKSRect mbr;
    r = m_pGeometry->GetMBR(mbr);
    labelpoint.x = (mbr.left + mbr.right) / 2;
    labelpoint.y = (mbr.top + mbr.bottom) / 2;
    if (r)
    {
        labelpoints.push_back(labelpoint);
    }

    return r;
}
//---------------------------------------------------------------------------
//  CGeometryLabel
//---------------------------------------------------------------------------

}