#include "CommonInclude.h"
#include "Geometry.h"
#include "MathLib.h"
#include <math.h>

#if !defined(__sqr)
template <typename T>
inline const T __sqr(const T& a)
{
    return a*a;
}
#endif

namespace easymap
{

CLASS_FACTORY_INSTANCE(CPoint)
CLASS_FACTORY_INSTANCE(CMultiPoint)
CLASS_FACTORY_INSTANCE(CPath)
CLASS_FACTORY_INSTANCE(CRing)
CLASS_FACTORY_INSTANCE(CPolyline)
CLASS_FACTORY_INSTANCE(CPolygon)
CLASS_FACTORY_INSTANCE(CEnvelope)
CLASS_FACTORY_INSTANCE(CCircle)
CLASS_FACTORY_INSTANCE(CEllipse)

using namespace mathlib;

//------------------------------------------------------------------------------
//  以下是CPoint的定义
//------------------------------------------------------------------------------
CPoint::CPoint()
{
    INIT_REFCOUNT
}

CPoint::CPoint(const WKSPointZ& point) : m_Point(point)
{
    INIT_REFCOUNT
}

CPoint::CPoint(const CPoint& point)
{
    INIT_REFCOUNT

    m_Point = point.m_Point;
}

CPoint::~CPoint()
{
}

bool __stdcall CPoint::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IGeometry"))
        || (0 == strcmp(interfacename, "CGeometry"))
        || (0 == strcmp(interfacename, "IPoint"))
        || (0 == strcmp(interfacename, "CPoint")))
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

dword __stdcall CPoint::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    pStream->WriteData(&m_Point, sizeof(WKSPointZ));
    return pStream->GetPos() - oldpos;
}

dword __stdcall CPoint::_LoadInstance(IStreamX* pStream, void* const assist)
{
    pStream->ReadData(&m_Point, sizeof(WKSPointZ));
    return sizeof(WKSPointZ);
}

GeometryType __stdcall CPoint::GetGeometryType() const
{
    return GEOMETRYTYPE_POINT;
}

word __stdcall CPoint::GetDimension() const
{
    return 0;
}

bool __stdcall CPoint::GetMBR(WKSRect& mbr) const
{
    mbr.left = m_Point.x;
    mbr.right = m_Point.x;
    mbr.top = m_Point.y;
    mbr.bottom = m_Point.y;
    return true;
}

bool __stdcall CPoint::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CPoint(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool __stdcall CPoint::Clone(IObjPtr& pObj) const
{
    pObj = new CPoint(*this);
    return true;
}

bool __stdcall CPoint::Select(const WKSPoint& pnt)
{
    WKSPoint pnt1;
    pnt1.x = m_Point.x;
    pnt1.y = m_Point.y;

    return PointEqual(pnt1, pnt);
}

bool __stdcall CPoint::Select(const WKSRect& envelope, bool PartialSelect)
{
    if ((envelope.left <= m_Point.x)
        && (envelope.right >= m_Point.x)
        && (envelope.bottom <= m_Point.y)
        && (envelope.top >= m_Point.y))
    {
        return true;
    }
    else
    {
        return false;
    }
}

bool __stdcall CPoint::Move(const double delta_x, const double delta_y)
{
    m_Point.x += delta_x;
    m_Point.y += delta_y;
    return true;
}

bool __stdcall CPoint::Ratate(const WKSPoint& origin, const double angle)
{
    return false;
}

bool CPoint::Disjoint(IGeometryPtr pGeometry)
{
    throw;
}

bool CPoint::Crosses(IGeometryPtr pGeometry)
{
    throw;
}

bool CPoint::Overlaps(IGeometryPtr pGeometry)
{
    throw;
}

bool CPoint::Within(IGeometryPtr pGeometry)
{
    throw;
}

bool CPoint::Contains(IGeometryPtr pGeometry)
{
    throw;
}

bool CPoint::Equals(IGeometryPtr pGeometry)
{
    throw;
}

bool CPoint::Touches(IGeometryPtr pGeometry)
{
    throw;
}

void __stdcall CPoint::SetX(const double x)
{
    m_Point.x = x;
}

void __stdcall CPoint::SetY(const double y)
{
    m_Point.y = y;
}

void __stdcall CPoint::SetZ(const double z)
{
    m_Point.z = z;
}

void __stdcall CPoint::GetX(double& x) const
{
    x = m_Point.x;
}

void __stdcall CPoint::GetY(double& y) const
{
    y = m_Point.y;
}

void __stdcall CPoint::GetZ(double& z) const
{
    z = m_Point.z;
}

void __stdcall CPoint::SetCoordinates(const double x, const double y, const double z)
{
    m_Point.x = x;
    m_Point.y = y;
    m_Point.z = z;
}

void __stdcall CPoint::GetCoordinates(double& x, double& y, double& z) const
{
    x = m_Point.x;
    y = m_Point.y;
    z = m_Point.z;
}

void __stdcall CPoint::SetCoordinates(const WKSPointZ& point)
{
    m_Point = point;
}

void __stdcall CPoint::GetCoordinates(WKSPointZ& point) const
{
    point = m_Point;
}

//------------------------------------------------------------------------------
//  以下是CMultiPoint的定义
//------------------------------------------------------------------------------
CMultiPoint::CMultiPoint()
{
    INIT_REFCOUNT
}

CMultiPoint::CMultiPoint(const CMultiPoint& multipoint)
{
    INIT_REFCOUNT

    vector<WKSPointZ>::const_iterator it = multipoint.m_Points.begin();
    while (it != multipoint.m_Points.end())
    {
        m_Points.push_back(*it);
        it++;
    }
}

CMultiPoint::~CMultiPoint()
{
    m_Points.clear();
}

bool __stdcall CMultiPoint::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IGeometry"))
        || (0 == strcmp(interfacename, "CGeometry"))
        || (0 == strcmp(interfacename, "IMultiPoint"))
        || (0 == strcmp(interfacename, "CMultiPoint")))
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

dword __stdcall CMultiPoint::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    CMultiPointPtr pMP = (CMultiPoint*)this;
    CMultiPoint::_MultiPoint2Stream(pMP, ps);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CMultiPoint::_LoadInstance(IStreamX* pStream, void* const assist)
{
    CMultiPointPtr pMP = this;
    CStreamPtr ps = (CStream*)pStream;
    return CMultiPoint::_Stream2MultiPoint(ps, pMP);
}

GeometryType __stdcall CMultiPoint::GetGeometryType() const
{
    return GEOMETRYTYPE_MULTIPOINT;
}

word __stdcall CMultiPoint::GetDimension() const
{
    return 0;
}

bool __stdcall CMultiPoint::GetMBR(WKSRect& mbr) const
{
    dword count = m_Points.size();
    if (0 >= count)
    {
        return false;
    }

    vector<WKSPointZ>::const_iterator it = m_Points.begin();
    WKSPointZ point = *it;
    it++;
    mbr.left = mbr.right = point.x;
    mbr.top = mbr.bottom = point.y;

    while (it != m_Points.end())
    {
        WKSPointZ point = *it;
        it++;
        if (point.x < mbr.left)
        {
            mbr.left = point.x;
        }
        else if (point.x > mbr.right)
        {
            mbr.right = point.x;
        }

        if (point.y > mbr.top)
        {
            mbr.top = point.y;
        }
        else if (point.y < mbr.bottom)
        {
            mbr.bottom = point.y;
        }
    }
    return true;
}

bool __stdcall CMultiPoint::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CMultiPoint(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool __stdcall CMultiPoint::Clone(IObjPtr& pObj) const
{
    pObj = new CMultiPoint(*this);
    return true;
}

bool __stdcall CMultiPoint::Select(const WKSPoint& pnt)
{
    for (dword i = 0; i < m_Points.size() - 1; i++)
    {
        WKSPoint pnt1;
        pnt1.x = m_Points[i].x;
        pnt1.y = m_Points[i].y;

        if (PointEqual(pnt1, pnt))
        {
            return true;
        }
    }
    return false;
}

bool __stdcall CMultiPoint::Select(const WKSRect& envelope, bool PartialSelect)
{
    for (dword i = 0; i < m_Points.size(); i++)
    {
        WKSPoint pnt1;
        pnt1.x = m_Points[i].x;
        pnt1.y = m_Points[i].y;

        if (PartialSelect)
        {
            if (PointInEnvelope(pnt1.x, pnt1.y, envelope))
            {
                return true;
            }
        }
        else
        {
            if (!PointInEnvelope(pnt1.x, pnt1.y, envelope))
            {
                return false;
            }
        }
    }

    return false;
}

bool __stdcall CMultiPoint::Move(const double delta_x, const double delta_y)
{
    for (dword i = 0; i < m_Points.size(); i++)
    {
        m_Points[i].x += delta_x;
        m_Points[i].y += delta_y;
    }

    return true;
}

bool __stdcall CMultiPoint::Ratate(const WKSPoint& origin, const double angle)
{
    return false;
}

bool CMultiPoint::Disjoint(IGeometryPtr pGeometry)
{
    return true;
}

bool CMultiPoint::Crosses(IGeometryPtr pGeometry)
{
    return true;
}

bool CMultiPoint::Overlaps(IGeometryPtr pGeometry)
{
    return true;
}

bool CMultiPoint::Within(IGeometryPtr pGeometry)
{
    return true;
}

bool CMultiPoint::Contains(IGeometryPtr pGeometry)
{
    return true;
}

bool CMultiPoint::Equals(IGeometryPtr pGeometry)
{
    return true;
}

bool CMultiPoint::Touches(IGeometryPtr pGeometry)
{
    throw;
}

bool __stdcall CMultiPoint::AddPoint(const WKSPointZ& point)
{
    m_Points.push_back(point);
    return true;
}

bool __stdcall CMultiPoint::DeletePoint(const dword index)
{
    if ((0 > index) || (m_Points.size() <= index))
    {
        return false;
    }

    vector<WKSPointZ>::iterator it = m_Points.begin();
    for (dword i = 0; i < index; i++)
    {
        it++;
    }
    m_Points.erase(it);
    return true;
}

void __stdcall CMultiPoint::ClearPoint()
{
    m_Points.clear();
}

bool __stdcall CMultiPoint::GetPoint(WKSPointZ& point, const dword index) const
{
    if ((0 > index) || (m_Points.size() <= index))
    {
        return false;
    }
    point = m_Points[index];
    return true;
}

bool __stdcall CMultiPoint::SetPointOrder(const dword oldorder, const dword neworder)
{
    return false;
}

dword __stdcall CMultiPoint::GetPointCount() const
{
    return m_Points.size();
}

//------------------------------------------------------------------------------
//  以下是CVertexArray的定义
//------------------------------------------------------------------------------
CVertexArray::CVertexArray()
{
}

CVertexArray::CVertexArray(const CVertexArray& vertexarray)
{
    dword vectorcount = vertexarray.m_VertexPoints.size();
    m_VertexPoints.resize(vectorcount);
    for (dword i = 0; i < vectorcount; i++)
    {
        m_VertexPoints[i] = vertexarray.m_VertexPoints[i];
    }
}

CVertexArray& CVertexArray::operator=(const CVertexArray& rhs)
{
    if (this == &rhs)
    {
        return *this;
    }

    this->Clear_Points();

    dword vectorcount = rhs.m_VertexPoints.size();
    m_VertexPoints.resize(vectorcount);
    for (dword i = 0; i < vectorcount; i++)
    {
        m_VertexPoints[i] = rhs.m_VertexPoints[i];
    }

    return *this;
}

CVertexArray::~CVertexArray()
{
    m_VertexPoints.clear();
}

bool CVertexArray::Add_Point(const VertexPoint& point)
{
    m_VertexPoints.push_back(point);

    return true;
}

bool CVertexArray::Insert_Point(const dword index, const VertexPoint& point)
{
    if ((0 > index) || (m_VertexPoints.size() <= index))
    {
        return false;
    }

    vector<VertexPoint>::iterator it = m_VertexPoints.begin();
    m_VertexPoints.insert(it + index, point);

    return true;
}

bool CVertexArray::Delete_Point(const dword index)
{
    if ((0 > index) || (m_VertexPoints.size() <= index))
    {
        return false;
    }

    vector<VertexPoint>::iterator it = m_VertexPoints.begin();
    m_VertexPoints.erase(it + index);

    return true;
}

void CVertexArray::Clear_Points()
{
    m_VertexPoints.clear();
}

bool CVertexArray::Set_Point(const dword index, const VertexPoint& point)
{
    if ((0 > index) || (m_VertexPoints.size() <= index))
    {
        return false;
    }

    m_VertexPoints[index] = point;

    return true;
}

bool CVertexArray::Get_Point(const dword index, VertexPoint& point) const
{
    if ((0 > index) || (m_VertexPoints.size() <= index))
    {
        return false;
    }

    point = m_VertexPoints[index];
    return true;
}

dword CVertexArray::Get_PointCount() const
{
    return m_VertexPoints.size();
}

bool CVertexArray::Get_MBR(WKSRect& mbr) const
{
    dword count = m_VertexPoints.size();
    if (0 >= count)
    {
        return false;
    }

    CVertexArray::VertexPoint vertexpnt = m_VertexPoints[0];
    mbr.left = vertexpnt.PointData.x;
    mbr.right = vertexpnt.PointData.x;
    mbr.top = vertexpnt.PointData.y;
    mbr.bottom = vertexpnt.PointData.y;

    for (dword i = 1; i < count; i++)
    {
        vertexpnt = m_VertexPoints[i];
        if (vertexpnt.PointData.x < mbr.left)
            mbr.left = vertexpnt.PointData.x;
        else if (vertexpnt.PointData.x > mbr.right)
            mbr.right = vertexpnt.PointData.x;
        if (vertexpnt.PointData.y < mbr.bottom)
            mbr.bottom = vertexpnt.PointData.y;
        else if (vertexpnt.PointData.y > mbr.top)
            mbr.top = vertexpnt.PointData.y;
    }

    return true;
}

bool CVertexArray::Get_Length(double& length) const
{
    dword i, count;
    count = m_VertexPoints.size();
    if (2 > count)
    {
        return false;
    }

    CVertexArray::VertexPoint pnt0 = m_VertexPoints[0];
    length = 0.0;
    for (i = 1; i < count; i++)
    {
        CVertexArray::VertexPoint pnt1 = m_VertexPoints[i];
        length += ::sqrt(__sqr(pnt1.PointData.x - pnt0.PointData.x)
            + __sqr(pnt1.PointData.y - pnt0.PointData.y));
        ::memcpy(&pnt0.PointData, &pnt1.PointData, sizeof(WKSPointZ));
    }
    return true;
}

bool CVertexArray::Get_Perimeter(double& perimeter) const
{
    dword count = m_VertexPoints.size();
    if (3 > count)
    {
        return false;
    }

    this->Get_Length(perimeter);
    CVertexArray::VertexPoint pnt0 = m_VertexPoints[0];
    CVertexArray::VertexPoint pnt1 = m_VertexPoints[count - 1];
    perimeter += ::sqrt(__sqr(pnt1.PointData.x - pnt0.PointData.x)
        + __sqr(pnt1.PointData.y - pnt0.PointData.y));
    return true;
}

//用来辅助计算积分面积
inline void _addsegarea(double& area, const double& x1, const double& y1,
    const double& x2, const double& y2)
{
    area = area + x1*y2 - x2*y1;
}

bool CVertexArray::Get_Area(double& area) const
{
    VertexPoint pnt1, pnt2;
    dword pntsize = m_VertexPoints.size();
    if (3 > pntsize)
    {
        return false;
    }

    area = 0;
    pnt1 = m_VertexPoints[0];
    for (dword i = 1; i < pntsize; i++)
    {
        pnt2 = m_VertexPoints[i];
        _addsegarea(area, pnt1.PointData.x, pnt1.PointData.y,
            pnt2.PointData.x, pnt2.PointData.y);
        pnt1 = pnt2;
    }
    pnt2 = m_VertexPoints[0];
    _addsegarea(area, pnt1.PointData.x, pnt1.PointData.y,
        pnt2.PointData.x, pnt2.PointData.y);
    area /= 2;

    return true;
}

bool CVertexArray::Is_PostiveDirection() const
{
    dword pntsize = m_VertexPoints.size();
    if (3 > pntsize)
    {
        return false;
    }

    double area;
    this->Get_Area(area);
    return area > 0 ? true : false;
}

bool CVertexArray::Get_PathValid() const
{
    if (2 > m_VertexPoints.size())
    {
        return false;
    }

    return true;
}

bool CVertexArray::Get_RingValid() const
{
    if (3 > m_VertexPoints.size())
    {
        return false;
    }

    return true;
}

void CVertexArray::VPA2VP(vector<WKSPoint>& pnts) const
{
    pnts.clear();
    dword count = this->Get_PointCount();
    for (long i = 0; i < count; i++)
    {
        CVertexArray::VertexPoint vp;
        this->Get_Point(i, vp);
        WKSPoint pnt;
        pnt.x = vp.PointData.x;
        pnt.y = vp.PointData.y;
        pnts.push_back(pnt);
    }
}

void CVertexArray::VPA2VP_Reverse(vector<WKSPoint>& pnts) const
{
    pnts.clear();
    dword count = this->Get_PointCount();
    for (long i = count-1; i >= 0; i--)
    {
        CVertexArray::VertexPoint vp;
        this->Get_Point(i, vp);
        WKSPoint pnt;
        pnt.x = vp.PointData.x;
        pnt.y = vp.PointData.y;
        pnts.push_back(pnt);
    }
}

//------------------------------------------------------------------------------
//  以下是CPath的定义
//------------------------------------------------------------------------------
CPath::CPath()
{
    INIT_REFCOUNT
}

CPath::CPath(const CPath& path)
{
    INIT_REFCOUNT

    m_VertexPointArray = path.m_VertexPointArray;
}

CPath::CPath(const WKSRect& envelope)
{
    INIT_REFCOUNT

    CVertexArray::VertexPoint pnt;
    pnt.PointType = VERTEXTYPE_COMMON;

    pnt.PointData.x = envelope.left;
    pnt.PointData.y = envelope.top;
    m_VertexPointArray.Add_Point(pnt);
    pnt.PointData.x = envelope.left;
    pnt.PointData.y = envelope.bottom;
    m_VertexPointArray.Add_Point(pnt);
    pnt.PointData.x = envelope.right;
    pnt.PointData.y = envelope.bottom;
    m_VertexPointArray.Add_Point(pnt);
    pnt.PointData.x = envelope.right;
    pnt.PointData.y = envelope.top;
    m_VertexPointArray.Add_Point(pnt);
    pnt.PointData.x = envelope.left;
    pnt.PointData.y = envelope.top;
    m_VertexPointArray.Add_Point(pnt);
}

CPath::~CPath()
{
}

bool __stdcall CPath::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IGeometry"))
        || (0 == strcmp(interfacename, "CGeometry"))
        || (0 == strcmp(interfacename, "IPath"))
        || (0 == strcmp(interfacename, "CPath")))
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

dword __stdcall CPath::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    CPathPtr pPath = (CPath*)this;
    CPath::_Path2Stream(pPath, ps);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CPath::_LoadInstance(IStreamX* pStream, void* const assist)
{
    CPathPtr pPath = this;
    CStreamPtr ps = (CStream*)pStream;
    return CPath::_Stream2Path(ps, pPath);
}

GeometryType __stdcall CPath::GetGeometryType() const
{
    return GEOMETRYTYPE_PATH;
}

word __stdcall CPath::GetDimension() const
{
    return 1;
}

bool __stdcall CPath::GetMBR(WKSRect& mbr) const
{
    dword count = m_VertexPointArray.Get_PointCount();
    if (2 > count)
    {
        return false;
    }

    m_VertexPointArray.Get_MBR(mbr);
    return true;
}

bool __stdcall CPath::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CPath(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool __stdcall CPath::Clone(IObjPtr& pObj) const
{
    pObj = new CPath(*this);
    return true;
}

bool __stdcall CPath::Select(const WKSPoint& pnt)
{
    vector<WKSPoint> pnts;
    m_VertexPointArray.VPA2VP(pnts);
    return mathlib::IsPointOnLineArray(pnts, pnt);
}

bool __stdcall CPath::Select(const WKSRect& envelope, bool PartialSelect)
{
    dword count = m_VertexPointArray.Get_PointCount();
    if (2 > count)
    {
        return false;
    }

    dword i;
    CVertexArray::VertexPoint point;
    for (i = 0; i < count; i++)
    {
        m_VertexPointArray.Get_Point(i, point);
        if (0 != PointInEnvelope(point.PointData.x, point.PointData.y, envelope))
        {
            return true;
        }
    }

    WKSPoint pnt[4];
    pnt[0].x = envelope.left;
    pnt[0].y = envelope.bottom;
    pnt[1].x = envelope.right;
    pnt[1].y = envelope.bottom;
    pnt[2].x = envelope.right;
    pnt[2].y = envelope.top;
    pnt[3].x = envelope.left;
    pnt[3].y = envelope.top;

    for (i = 0; i < count - 1; i++)
    {
        WKSPoint segpnt1, segpnt2;
        m_VertexPointArray.Get_Point(i, point);
        segpnt1.x = point.PointData.x;
        segpnt1.y = point.PointData.y;
        m_VertexPointArray.Get_Point(i + 1, point);
        segpnt2.x = point.PointData.x;
        segpnt2.y = point.PointData.y;

        WKSPoint outpnt;
        bool parallel;
        bool r = mathlib::GetRelationTwoLine(pnt[0], pnt[1], segpnt1, segpnt2, outpnt, parallel);
        if (r)
        {
            return true;
        }
        r = mathlib::GetRelationTwoLine(pnt[1], pnt[2], segpnt1, segpnt2, outpnt, parallel);
        if (r)
        {
            return true;
        }
        r = mathlib::GetRelationTwoLine(pnt[2], pnt[3], segpnt1, segpnt2, outpnt, parallel);
        if (r)
        {
            return true;
        }
        r = mathlib::GetRelationTwoLine(pnt[3], pnt[0], segpnt1, segpnt2, outpnt, parallel);
        if (r)
        {
            return true;
        }
    }

    return false;
}

bool __stdcall CPath::Move(const double delta_x, const double delta_y)
{
    dword count = m_VertexPointArray.Get_PointCount();
    for (dword i = 0; i < count; i++)
    {
        CVertexArray::VertexPoint pnt;
        m_VertexPointArray.Get_Point(i, pnt);
        pnt.PointData.x += delta_x;
        pnt.PointData.y += delta_y;
        m_VertexPointArray.Set_Point(i, pnt);
    }

    return true;
}

bool __stdcall CPath::Ratate(const WKSPoint& origin, const double angle)
{
    return false;
}

bool CPath::Disjoint(IGeometryPtr pGeometry)
{
    return true;
}

bool CPath::Crosses(IGeometryPtr pGeometry)
{
    return true;
}

bool CPath::Overlaps(IGeometryPtr pGeometry)
{
    return true;
}

bool CPath::Within(IGeometryPtr pGeometry)
{
    return true;
}

bool CPath::Contains(IGeometryPtr pGeometry)
{
    return true;
}

bool CPath::Equals(IGeometryPtr pGeometry)
{
    return true;
}

bool CPath::Touches(IGeometryPtr pGeometry)
{
    throw;
}

bool __stdcall CPath::AddPoint(const WKSPointZ& point, const VertexType vertextype)
{
    CVertexArray::VertexPoint pnt;
    ::memcpy(&pnt.PointData, &point, sizeof(WKSPointZ));
    pnt.PointType = vertextype;
    m_VertexPointArray.Add_Point(pnt);
    return true;
}

bool __stdcall CPath::InsertPoint(const dword index, const WKSPointZ& point,
    const VertexType vertextype)
{
    if ((0 > index) || (m_VertexPointArray.Get_PointCount() <= index))
    {
        return false;
    }

    CVertexArray::VertexPoint pnt;
    ::memcpy(&pnt.PointData, &point, sizeof(WKSPointZ));
    pnt.PointType = vertextype;
    return m_VertexPointArray.Insert_Point(index, pnt);
}

bool __stdcall CPath::DeletePoint(const dword index)
{
    if ((0 > index) || (m_VertexPointArray.Get_PointCount() <= index))
    {
        return false;
    }

    return m_VertexPointArray.Delete_Point(index);
}

void __stdcall CPath::ClearPoint()
{
    m_VertexPointArray.Clear_Points();
}

bool __stdcall CPath::SetPoint(const dword index, const WKSPointZ& point,
    const VertexType vertextype)
{
    if ((0 > index) || (m_VertexPointArray.Get_PointCount() <= index))
    {
        return false;
    }

    CVertexArray::VertexPoint pnt;
    ::memcpy(&pnt.PointData, &point, sizeof(WKSPointZ));
    pnt.PointType = vertextype;
    return m_VertexPointArray.Set_Point(index, pnt);
}

bool __stdcall CPath::GetPoint(const dword index, WKSPointZ& point,
    VertexType& vertextype) const
{
    if ((0 > index) || (m_VertexPointArray.Get_PointCount() <= index))
    {
        return false;
    }

    CVertexArray::VertexPoint pnt;
    bool r = m_VertexPointArray.Get_Point(index, pnt);
    if (!r)
    {
        return false;
    }

    ::memcpy(&point, &pnt.PointData, sizeof(WKSPointZ));
    vertextype = pnt.PointType;
    return true;
}

bool __stdcall CPath::GetPoint1(const dword index, WKSPointZ& point) const
{
    VertexType vertextype;
    return this->GetPoint(index, point, vertextype);
}

dword __stdcall CPath::GetPointCount() const
{
    return m_VertexPointArray.Get_PointCount();
}

bool __stdcall CPath::GetLength(double& length) const
{
    return m_VertexPointArray.Get_Length(length);
}

bool __stdcall CPath::GetPositionByDistance(const double distance, WKSPoint& position) const
{
    dword pointcount = m_VertexPointArray.Get_PointCount();
    if (pointcount < 2)
        return false;

    double dist_in = 0;
    CVertexArray::VertexPoint point_from;
    m_VertexPointArray.Get_Point(0, point_from);
    for (long i = 1; i < pointcount; i++)
    {
        CVertexArray::VertexPoint point_to;
        m_VertexPointArray.Get_Point(i, point_to);
        double dist_seg = ::sqrt(point_to.PointData.dis2(point_from.PointData));
        if ((dist_in + dist_seg - distance) >= 0)
        {
            double delta_factor = (distance - dist_in) / dist_seg;
            position.x = point_from.PointData.x
                + (point_to.PointData.x - point_from.PointData.x) * delta_factor;
            position.y = point_from.PointData.y
                + (point_to.PointData.y - point_from.PointData.y) * delta_factor;

            return true;
        }

        dist_in += dist_seg;
        point_from = point_to;
    }

    m_VertexPointArray.Get_Point(pointcount - 1, point_from);
    position = point_from.PointData;
    return false;
}

bool __stdcall CPath::IsValid() const
{
    return m_VertexPointArray.Get_PathValid();
}

//------------------------------------------------------------------------------
//  以下是CRing的定义
//------------------------------------------------------------------------------
CRing::CRing()
{
    INIT_REFCOUNT
}

CRing::CRing(const CRing& ring)
{
    INIT_REFCOUNT

    m_VertexPointArray = ring.m_VertexPointArray;
}

CRing::CRing(const WKSRect& envelope)
{
    INIT_REFCOUNT

    CVertexArray::VertexPoint pnt;
    pnt.PointType = VERTEXTYPE_COMMON;

    pnt.PointData.x = envelope.left;
    pnt.PointData.y = envelope.top;
    m_VertexPointArray.Add_Point(pnt);
    pnt.PointData.x = envelope.left;
    pnt.PointData.y = envelope.bottom;
    m_VertexPointArray.Add_Point(pnt);
    pnt.PointData.x = envelope.right;
    pnt.PointData.y = envelope.bottom;
    m_VertexPointArray.Add_Point(pnt);
    pnt.PointData.x = envelope.right;
    pnt.PointData.y = envelope.top;
    m_VertexPointArray.Add_Point(pnt);
    pnt.PointData.x = envelope.left;
    pnt.PointData.y = envelope.top;
    m_VertexPointArray.Add_Point(pnt);
}

CRing::~CRing()
{
}

bool __stdcall CRing::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IGeometry"))
        || (0 == strcmp(interfacename, "CGeometry"))
        || (0 == strcmp(interfacename, "IRing"))
        || (0 == strcmp(interfacename, "CRing")))
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

dword __stdcall CRing::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    CRingPtr pRing = (CRing*)this;
    CRing::_Ring2Stream(pRing, ps);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CRing::_LoadInstance(IStreamX* pStream, void* const assist)
{
    CRingPtr pRing = this;
    CStreamPtr ps = (CStream*)pStream;
    return CRing::_Stream2Ring(ps, pRing);
}

GeometryType __stdcall CRing::GetGeometryType() const
{
    return GEOMETRYTYPE_RING;
}

word __stdcall CRing::GetDimension() const
{
    return 2;
}

bool __stdcall CRing::GetMBR(WKSRect& mbr) const
{
    dword count = m_VertexPointArray.Get_PointCount();
    if (3 > count)
    {
        return false;
    }

    m_VertexPointArray.Get_MBR(mbr);
    return true;
}

bool __stdcall CRing::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CRing(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool __stdcall CRing::Clone(IObjPtr& pObj) const
{
    pObj = new CRing(*this);
    return true;
}

bool __stdcall CRing::Select(const WKSPoint& pnt)
{
    vector<WKSPoint> pnts;
    m_VertexPointArray.VPA2VP(pnts);
    long r = mathlib::PointInPolygon(pnt, pnts);

    if (0 >= r)
    {
        m_VertexPointArray.VPA2VP_Reverse(pnts);
        r = mathlib::PointInPolygon(pnt, pnts);
        return (r > 0);
    }

    return true;
}

bool __stdcall CRing::Select(const WKSRect& envelope, bool PartialSelect)
{
    vector<WKSPoint> pnts;
    m_VertexPointArray.VPA2VP(pnts);
    WKSPoint pnt[4];
    pnt[0].x = envelope.left;
    pnt[0].y = envelope.bottom;
    pnt[1].x = envelope.right;
    pnt[1].y = envelope.bottom;
    pnt[2].x = envelope.right;
    pnt[2].y = envelope.top;
    pnt[3].x = envelope.left;
    pnt[3].y = envelope.top;

    dword i;
    for (i = 0; i < 4; i++)
    {
        long r = mathlib::PointInPolygon(pnt[i], pnts);
        if (0 < r)
        {
            return true;
        }
    }

    m_VertexPointArray.VPA2VP_Reverse(pnts);
    for (i = 0; i < 4; i++)
    {
        long r = mathlib::PointInPolygon(pnt[i], pnts);
        if (0 < r)
        {
            return true;
        }
    }

    dword count = m_VertexPointArray.Get_PointCount();
    for (i = 0; i < count; i++)
    {
        CVertexArray::VertexPoint point;
        m_VertexPointArray.Get_Point(i, point);
        if (0 != PointInEnvelope(point.PointData.x, point.PointData.y, envelope))
        {
            return true;
        }
    }

    CVertexArray::VertexPoint point;
    for (i = 0; i < count - 1; i++)
    {
        WKSPoint segpnt1, segpnt2;
        m_VertexPointArray.Get_Point(i, point);
        segpnt1.x = point.PointData.x;
        segpnt1.y = point.PointData.y;
        m_VertexPointArray.Get_Point(i + 1, point);
        segpnt2.x = point.PointData.x;
        segpnt2.y = point.PointData.y;

        WKSPoint outpnt;
        bool parallel;
        bool r = mathlib::GetRelationTwoLine(pnt[0], pnt[1], segpnt1, segpnt2, outpnt, parallel);
        if (r)
        {
            return true;
        }
        r = mathlib::GetRelationTwoLine(pnt[1], pnt[2], segpnt1, segpnt2, outpnt, parallel);
        if (r)
        {
            return true;
        }
        r = mathlib::GetRelationTwoLine(pnt[2], pnt[3], segpnt1, segpnt2, outpnt, parallel);
        if (r)
        {
            return true;
        }
        r = mathlib::GetRelationTwoLine(pnt[3], pnt[0], segpnt1, segpnt2, outpnt, parallel);
        if (r)
        {
            return true;
        }
    }

    return false;
}

bool __stdcall CRing::Move(const double delta_x, const double delta_y)
{
    dword count = m_VertexPointArray.Get_PointCount();
    for (dword i = 0; i < count; i++)
    {
        CVertexArray::VertexPoint pnt;
        m_VertexPointArray.Get_Point(i, pnt);
        pnt.PointData.x += delta_x;
        pnt.PointData.y += delta_y;
        m_VertexPointArray.Set_Point(i, pnt);
    }

    return true;
}

bool __stdcall CRing::Ratate(const WKSPoint& origin, const double angle)
{
    return false;
}

bool CRing::Disjoint(IGeometryPtr pGeometry)
{
    return true;
}

bool CRing::Crosses(IGeometryPtr pGeometry)
{
    return true;
}

bool CRing::Overlaps(IGeometryPtr pGeometry)
{
    return true;
}

bool CRing::Within(IGeometryPtr pGeometry)
{
    return true;
}

bool CRing::Contains(IGeometryPtr pGeometry)
{
    return true;
}

bool CRing::Equals(IGeometryPtr pGeometry)
{
    return true;
}

bool CRing::Touches(IGeometryPtr pGeometry)
{
    throw;
}

bool __stdcall CRing::AddPoint(const WKSPointZ& point, const VertexType vertextype)
{
    CVertexArray::VertexPoint pnt;
    ::memcpy(&pnt.PointData, &point, sizeof(WKSPointZ));
    pnt.PointType = vertextype;
    m_VertexPointArray.Add_Point(pnt);
    return true;
}

bool __stdcall CRing::InsertPoint(const dword index, const WKSPointZ& point,
    const VertexType vertextype)
{
    if ((0 > index) || (m_VertexPointArray.Get_PointCount() <= index))
    {
        return false;
    }

    CVertexArray::VertexPoint pnt;
    ::memcpy(&pnt.PointData, &point, sizeof(WKSPointZ));
    pnt.PointType = vertextype;
    return m_VertexPointArray.Insert_Point(index, pnt);
}

bool __stdcall CRing::DeletePoint(const dword index)
{
    if ((0 > index) || (m_VertexPointArray.Get_PointCount() <= index))
    {
        return false;
    }

    return m_VertexPointArray.Delete_Point(index);
}

void __stdcall CRing::ClearPoint()
{
    m_VertexPointArray.Clear_Points();
}

bool __stdcall CRing::SetPoint(const dword index, const WKSPointZ& point,
    const VertexType vertextype)
{
    if ((0 > index) || (m_VertexPointArray.Get_PointCount() <= index))
    {
        return false;
    }

    CVertexArray::VertexPoint pnt;
    ::memcpy(&pnt.PointData, &point, sizeof(WKSPointZ));
    pnt.PointType = vertextype;
    return m_VertexPointArray.Set_Point(index, pnt);
}

bool __stdcall CRing::GetPoint(const dword index, WKSPointZ& point,
    VertexType& vertextype) const
{
    if ((0 > index) || (m_VertexPointArray.Get_PointCount() <= index))
    {
        return false;
    }

    CVertexArray::VertexPoint pnt;
    bool r = m_VertexPointArray.Get_Point(index, pnt);
    if (!r)
    {
        return false;
    }

    ::memcpy(&point, &pnt.PointData, sizeof(WKSPointZ));
    vertextype = pnt.PointType;
    return true;
}

bool __stdcall CRing::GetPoint1(const dword index, WKSPointZ& point) const
{
    VertexType vertextype;
    return this->GetPoint(index, point, vertextype);
}

dword __stdcall CRing::GetPointCount() const
{
    return m_VertexPointArray.Get_PointCount();
}

bool __stdcall CRing::GetPerimeter(double& perimeter) const
{
    return m_VertexPointArray.Get_Perimeter(perimeter);
}

bool __stdcall CRing::GetArea(double& area) const
{
    return m_VertexPointArray.Get_Area(area);
}

bool __stdcall CRing::IsPostiveDirection() const
{
    double area;
    if (!this->GetArea(area))
    {
        return false;
    }

    return 0 <= area;
}

bool __stdcall CRing::IsValid() const
{
    return m_VertexPointArray.Get_RingValid();
}

//------------------------------------------------------------------------------
//  以下是CPolyline的定义
//------------------------------------------------------------------------------
CPolyline::CPolyline()
{
    INIT_REFCOUNT
}

CPolyline::CPolyline(const CPolyline& polyline)
{
    INIT_REFCOUNT

    vector<CPathPtr>::const_iterator it = polyline.m_Items.begin();
    while (it != polyline.m_Items.end())
    {
        IObjPtr pObj;
        (*it)->Clone(pObj);
        CPathPtr pPath;
        CAST_PTR(pObj, pPath, CPath)
        this->m_Items.push_back(pPath);
        it++;
    }
}

CPolyline::~CPolyline()
{
}

bool __stdcall CPolyline::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IGeometry"))
        || (0 == strcmp(interfacename, "CGeometry"))
        || (0 == strcmp(interfacename, "IPolyline"))
        || (0 == strcmp(interfacename, "CPolyline")))
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

dword __stdcall CPolyline::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    CPolylinePtr pPolyline = (CPolyline*)this;
    CPolyline::_Polyline2Stream(pPolyline, ps);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CPolyline::_LoadInstance(IStreamX* pStream, void* const assist)
{
    CPolylinePtr pPolyline = this;
    CStreamPtr ps = (CStream*)pStream;
    return CPolyline::_Stream2Polyline(ps, pPolyline);
}

GeometryType __stdcall CPolyline::GetGeometryType() const
{
    return GEOMETRYTYPE_POLYLINE;
}

word __stdcall CPolyline::GetDimension() const
{
    return 1;
}

bool __stdcall CPolyline::GetMBR(WKSRect& mbr) const
{
    dword count = m_Items.size();
    if (0 >= count)
    {
        return false;
    }

    bool flag = false;
    vector<CPathPtr>::const_iterator it = m_Items.begin();
    while (it != m_Items.end())
    {
        CPathPtr pItem = *it;
        it++;
        if (!pItem->GetMBR(mbr)) continue;
        flag = true;
        break;
    }
    if (!flag)
    {
        return false;
    }

    while (it != m_Items.end())
    {
        CPathPtr pItem = *it;
        it++;
        WKSRect mbr1;
        if (!pItem->GetMBR(mbr1))
            continue;
        if (mbr1.left < mbr.left)
            mbr.left = mbr1.left;
        if (mbr1.right > mbr.right)
            mbr.right = mbr1.right;
        if (mbr1.top > mbr.top)
            mbr.top = mbr1.top;
        if (mbr1.bottom < mbr.bottom)
            mbr.bottom = mbr1.bottom;
    }
    return true;
}

bool __stdcall CPolyline::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CPolyline(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool __stdcall CPolyline::Clone(IObjPtr& pObj) const
{
    pObj = new CPolyline(*this);
    return true;
}

bool __stdcall CPolyline::Select(const WKSPoint& pnt)
{
    dword count = m_Items.size();
    for (dword i = 0; i < count; i++)
    {
        CPathPtr pPath = m_Items[i];
        bool r = pPath->Select(pnt);
        if (r)
        {
            return true;
        }
    }

    return false;
}

bool __stdcall CPolyline::Select(const WKSRect& envelope, bool PartialSelect)
{
    dword count = m_Items.size();
    for (dword i = 0; i < count; i++)
    {
        CPathPtr pPath = m_Items[i];
        bool r = pPath->Select(envelope, PartialSelect);
        if (r)
        {
            return true;
        }
    }

    return false;
}

bool __stdcall CPolyline::Move(const double delta_x, const double delta_y)
{
    dword count = m_Items.size();
    for (dword i = 0; i < count; i++)
    {
        CPathPtr pPath = m_Items[i];
        pPath->Move(delta_x, delta_y);
    }

    return true;
}

bool __stdcall CPolyline::Ratate(const WKSPoint& origin, const double angle)
{
    return false;
}

bool CPolyline::Disjoint(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolyline::Crosses(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolyline::Overlaps(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolyline::Within(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolyline::Contains(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolyline::Equals(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolyline::Touches(IGeometryPtr pGeometry)
{
    throw;
}

bool __stdcall CPolyline::AddPathRef(CPathPtr pPath)
{
    if (!pPath.Assigned())
    {
        return false;
    }

    vector<CPathPtr>::const_iterator it = m_Items.begin();
    while (it != m_Items.end())
    {
        if (pPath == (*it))
        {
            return false;
        }
        it++;
    }
    m_Items.push_back(pPath);
    return true;
}

bool __stdcall CPolyline::AddPathRef(IPath* pPath)
{
    CPathPtr pP = (CPath*)pPath;
    return this->AddPathRef(pP);
}

bool __stdcall CPolyline::DeletePath(const dword index)
{
    if ((0 > index) || (m_Items.size() <= index))
    {
        return false;
    }

    vector<CPathPtr>::iterator it = m_Items.begin();
    for (dword i = 0; i < index; i++) it++;
    m_Items.erase(it);
    return true;
}

void __stdcall CPolyline::ClearPath()
{
    m_Items.clear();
}

bool __stdcall CPolyline::GetPathRef(CPathPtr& pPath, const dword index) const
{
    if ((0 > index) || (m_Items.size() <= index))
    {
        return false;
    }

    pPath = m_Items[index];
    return true;
}

bool __stdcall CPolyline::GetPathRef(IPath** ppPath, const dword index) const
{
    if (_invalid(ppPath)) return false;
    assert(!*ppPath);

    CPathPtr pPath;
    if (!this->GetPathRef(pPath, index))
    {
        return false;
    }

    *ppPath = pPath._p();
    (*ppPath)->_AddRef();
    return true;
}

bool __stdcall CPolyline::SetPathOrder(const dword oldorder, const dword neworder)
{
    return false;
}

dword __stdcall CPolyline::GetPathCount() const
{
    return m_Items.size();
}

bool __stdcall CPolyline::GetLength(double& length) const
{
    length = 0.0;
    vector<CPathPtr>::const_iterator it = m_Items.begin();
    while (it != m_Items.end())
    {
        CPathPtr pPath = *it;
        double tmplen;
        pPath->GetLength(tmplen);
        length += tmplen;
        it++;
    }

    return true;
}

bool __stdcall CPolyline::GetPositionByDistance(const double distance, WKSPoint& position) const
{
    double dist_in = 0;
    vector<CPathPtr>::const_iterator it = m_Items.begin();
    while (it != m_Items.end())
    {
        CPathPtr pPath = *it;
        double path_length;
        if (pPath->GetLength(path_length))
        {
            if (pPath->GetPositionByDistance(distance - dist_in, position))
            {
                return true;
            }

            dist_in += path_length;
        }

        it++;
    }

    return false;
}

bool __stdcall CPolyline::IsValid() const
{
    return true;//?????????
}

//------------------------------------------------------------------------------
//  以下是CPolygon的定义
//------------------------------------------------------------------------------
CPolygon::CPolygon()
{
    INIT_REFCOUNT
}

CPolygon::CPolygon(const CPolygon& polygon)
{
    INIT_REFCOUNT

    vector<CRingPtr>::const_iterator it = polygon.m_Items.begin();
    while (it != polygon.m_Items.end())
    {
        IObjPtr pObj;
        (*it)->Clone(pObj);
        CRingPtr pRing;
        CAST_PTR(pObj, pRing, CRing)
        this->m_Items.push_back(pRing);
        it++;
    }
}

CPolygon::~CPolygon()
{
}

bool __stdcall CPolygon::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IGeometry"))
        || (0 == strcmp(interfacename, "CGeometry"))
        || (0 == strcmp(interfacename, "IPolygon"))
        || (0 == strcmp(interfacename, "CPolygon")))
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

dword __stdcall CPolygon::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    CPolygonPtr pPolygon = (CPolygon*)this;
    CPolygon::_Polygon2Stream(pPolygon, ps);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CPolygon::_LoadInstance(IStreamX* pStream, void* const assist)
{
    CPolygonPtr pPolygon = this;
    CStreamPtr ps = (CStream*)pStream;
    return CPolygon::_Stream2Polygon(ps, pPolygon);
}

GeometryType __stdcall CPolygon::GetGeometryType() const
{
    return GEOMETRYTYPE_POLYGON;
}

word __stdcall CPolygon::GetDimension() const
{
    return 2;
}

bool __stdcall CPolygon::GetMBR(WKSRect& mbr) const
{
    dword count = m_Items.size();
    if (0 >= count)
    {
        return false;
    }

    bool flag = false;
    vector<CRingPtr>::const_iterator it = m_Items.begin();
    while (it != m_Items.end())
    {
        CRingPtr pItem = *it;
        it++;
        if (!pItem->GetMBR(mbr))
        {
            continue;
        }
        flag = true;
        break;
    }
    if (!flag)
    {
        return false;
    }

    while (it != m_Items.end())
    {
        CRingPtr pItem = *it;
        it++;
        WKSRect mbr1;
        if (!pItem->GetMBR(mbr1))
            continue;
        if (mbr1.left < mbr.left)
            mbr.left = mbr1.left;
        if (mbr1.right > mbr.right)
            mbr.right = mbr1.right;
        if (mbr1.top > mbr.top)
            mbr.top = mbr1.top;
        if (mbr1.bottom < mbr.bottom)
            mbr.bottom = mbr1.bottom;
    }
    return true;
}

bool __stdcall CPolygon::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CPolygon(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool __stdcall CPolygon::Clone(IObjPtr& pObj) const
{
    pObj = new CPolygon(*this);
    return true;
}

bool __stdcall CPolygon::Select(const WKSPoint& pnt)
{
    dword count = m_Items.size();
    for (dword i = 0; i < count; i++)
    {
        CRingPtr pRing = m_Items[i];
        bool r = pRing->Select(pnt);
        if (r)
        {
            return true;
        }
    }

    return false;
}

bool __stdcall CPolygon::Select(const WKSRect& envelope, bool PartialSelect)
{
    dword count = m_Items.size();
    for (dword i = 0; i < count; i++)
    {
        CRingPtr pRing = m_Items[i];

        bool r = pRing->Select(envelope, PartialSelect);
        if (r)
        {
            return true;
        }
    }

    return false;
}

bool __stdcall CPolygon::Move(const double delta_x, const double delta_y)
{
    dword count = m_Items.size();
    for (dword i = 0; i < count; i++)
    {
        CRingPtr pRing = m_Items[i];
        pRing->Move(delta_x, delta_y);
    }

    return true;
}

bool __stdcall CPolygon::Ratate(const WKSPoint& origin, const double angle)
{
    return false;
}

bool CPolygon::Disjoint(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolygon::Crosses(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolygon::Overlaps(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolygon::Within(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolygon::Contains(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolygon::Equals(IGeometryPtr pGeometry)
{
    return true;
}

bool CPolygon::Touches(IGeometryPtr pGeometry)
{
    throw;
}

bool __stdcall CPolygon::AddRingRef(CRingPtr pRing)
{
    if (!pRing.Assigned())
    {
        return false;
    }

    vector<CRingPtr>::const_iterator it = m_Items.begin();
    while (it != m_Items.end())
    {
        if (pRing == (*it))
        {
            return false;
        }
        it++;
    }
    m_Items.push_back(pRing);
    return true;
}

bool __stdcall CPolygon::AddRingRef(IRing* pRing)
{
    CRingPtr pR = (CRing*)pRing;
    return this->AddRingRef(pR);
}

bool __stdcall CPolygon::DeleteRing(const dword index)
{
    if ((0 > index) || (m_Items.size() <= index))
    {
        return false;
    }

    vector<CRingPtr>::iterator it = m_Items.begin();
    for (dword i = 0; i < index; i++) it++;
    m_Items.erase(it);
    return true;
}

void __stdcall CPolygon::ClearRing()
{
    m_Items.clear();
}

bool __stdcall CPolygon::GetRingRef(CRingPtr& pRing, const dword index) const
{
    if ((0 > index) || (m_Items.size() <= index))
    {
        return false;
    }

    pRing = m_Items[index];
    return true;
}

bool __stdcall CPolygon::GetRingRef(IRing** ppRing, const dword index) const
{
    if (_invalid(ppRing)) return false;
    assert(!*ppRing);

    CRingPtr pRing;
    if (!this->GetRingRef(pRing, index))
    {
        return false;
    }

    *ppRing = pRing._p();
    (*ppRing)->_AddRef();
    return true;
}

bool __stdcall CPolygon::SetRingOrder(const dword oldorder, const dword neworder)
{
    return false;
}

dword __stdcall CPolygon::GetRingCount() const
{
    return m_Items.size();
}

bool __stdcall CPolygon::GetArea(double& area) const
{
    area = 0.0;
    vector<CRingPtr>::const_iterator it = m_Items.begin();
    while (it != m_Items.end())
    {
        CRingPtr pRing = *it;
        double tmparea;
        pRing->GetArea(tmparea);
        area += tmparea;
    }
    return true;
}

bool __stdcall CPolygon::GetPerimeter(double& perimeter) const
{
    perimeter = 0.0;
    vector<CRingPtr>::const_iterator it = m_Items.begin();
    while (it != m_Items.end())
    {
        CRingPtr pRing = *it;
        double tmp;
        pRing->GetPerimeter(tmp);
        perimeter += tmp;
    }

    return true;
}

bool __stdcall CPolygon::IsValid() const
{
    return true;//?????????
}

//------------------------------------------------------------------------------
//  以下是CEnvelope的定义
//------------------------------------------------------------------------------
CEnvelope::CEnvelope()
{
    INIT_REFCOUNT
}

CEnvelope::CEnvelope(const WKSRect& rect)
{
    INIT_REFCOUNT

    m_Env = rect;
    CorrectEnvelope(m_Env);
}

CEnvelope::CEnvelope(const CEnvelope& envelope)
{
    INIT_REFCOUNT

    m_Env = envelope.m_Env;
}

CEnvelope::~CEnvelope()
{
}

bool __stdcall CEnvelope::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IGeometry"))
        || (0 == strcmp(interfacename, "CGeometry"))
        || (0 == strcmp(interfacename, "IEnvelope"))
        || (0 == strcmp(interfacename, "CEnvelope")))
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

dword __stdcall CEnvelope::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    pStream->WriteData(&m_Env, sizeof(WKSRect));
    return pStream->GetPos() - oldpos;
}

dword __stdcall CEnvelope::_LoadInstance(IStreamX* pStream, void* const assist)
{
    pStream->ReadData(&m_Env, sizeof(WKSRect));
    return sizeof(WKSRect);
}

GeometryType __stdcall CEnvelope::GetGeometryType() const
{
    return GEOMETRYTYPE_ENVELOPE;
}

word __stdcall CEnvelope::GetDimension() const
{
    return 2;
}

bool __stdcall CEnvelope::GetMBR(WKSRect& mbr) const
{
    mbr = m_Env;
    CorrectEnvelope(mbr);
    return true;
}

bool __stdcall CEnvelope::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CEnvelope(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool __stdcall CEnvelope::Clone(IObjPtr& pObj) const
{
    pObj = new CEnvelope(*this);
    return true;
}

bool __stdcall CEnvelope::Select(const WKSPoint& pnt)
{
    WKSRect thisenvelope = m_Env;
    CorrectEnvelope(thisenvelope);
    if ((thisenvelope.bottom < pnt.y)
        && (thisenvelope.top > pnt.y)
        && (thisenvelope.left < pnt.x)
        && (thisenvelope.right > pnt.x))
    {
        return true;
    }
    else
    {
        return false;
    }
}

bool __stdcall CEnvelope::Select(const WKSRect& envelope, bool PartialSelect)
{
    WKSRect selectenvelope = envelope;
    WKSRect thisenvelope = m_Env;
    CorrectEnvelope(selectenvelope);
    CorrectEnvelope(thisenvelope);
    if (PartialSelect)
    {
        return EnvelopesTouched(selectenvelope, thisenvelope);
    }
    else
    {
        return EnvelopesContented(selectenvelope, thisenvelope);
    }
}

bool __stdcall CEnvelope::Move(const double delta_x, const double delta_y)
{
    m_Env.bottom += delta_y;
    m_Env.left += delta_x;
    m_Env.right += delta_x;
    m_Env.top += delta_y;

    return true;
}

bool __stdcall CEnvelope::Ratate(const WKSPoint& origin, const double angle)
{
    return false;
}

bool CEnvelope::Disjoint(IGeometryPtr pGeometry)
{
    return true;
}

bool CEnvelope::Crosses(IGeometryPtr pGeometry)
{
    return true;
}

bool CEnvelope::Overlaps(IGeometryPtr pGeometry)
{
    return true;
}

bool CEnvelope::Within(IGeometryPtr pGeometry)
{
    return true;
}

bool CEnvelope::Contains(IGeometryPtr pGeometry)
{
    return true;
}

bool CEnvelope::Equals(IGeometryPtr pGeometry)
{
    return true;
}

bool CEnvelope::Touches(IGeometryPtr pGeometry)
{
    throw;
}

void __stdcall CEnvelope::SetMinX(const double minx)
{
    m_Env.left = minx;
}

void __stdcall CEnvelope::SetMinY(const double miny)
{
    m_Env.bottom = miny;
}

void __stdcall CEnvelope::SetMaxX(const double maxx)
{
    m_Env.right = maxx;
}

void __stdcall CEnvelope::SetMaxY(const double maxy)
{
    m_Env.top = maxy;
}

void __stdcall CEnvelope::GetMinX(double& minx) const
{
    minx = m_Env.left;
}

void __stdcall CEnvelope::GetMinY(double& miny) const
{
    miny = m_Env.bottom;
}

void __stdcall CEnvelope::GetMaxX(double& maxx) const
{
    maxx = m_Env.right;
}

void __stdcall CEnvelope::GetMaxY(double& maxy) const
{
    maxy = m_Env.top;
}

void __stdcall CEnvelope::GetArea(double& area) const
{
    area = fabs((m_Env.right - m_Env.left) * (m_Env.top - m_Env.bottom));
}

void __stdcall CEnvelope::GetPerimeter(double& perimeter) const
{
    perimeter = (fabs(m_Env.right - m_Env.left) + fabs(m_Env.top - m_Env.bottom)) * 2;
}

void __stdcall CEnvelope::TrimEnvelope()
{
    CorrectEnvelope(m_Env);
}

//------------------------------------------------------------------------------
//  以下是CCircle的定义
//------------------------------------------------------------------------------
CCircle::CCircle()
{
    INIT_REFCOUNT
}

CCircle::CCircle(const WKSPointZ& center, const double& radius)
{
    INIT_REFCOUNT

    m_Center = center;
    m_Radius = radius;
}

CCircle::CCircle(const CCircle& circle)
{
    INIT_REFCOUNT

    m_Center = circle.m_Center;
    m_Radius = circle.m_Radius;
}

CCircle::~CCircle()
{
}

bool __stdcall CCircle::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IGeometry"))
        || (0 == strcmp(interfacename, "CGeometry"))
        || (0 == strcmp(interfacename, "ICircle"))
        || (0 == strcmp(interfacename, "CCircle")))
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

dword __stdcall CCircle::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    pStream->WriteData(&m_Center, sizeof(WKSPointZ));
    pStream->WriteData(&m_Radius, sizeof(double));
    return pStream->GetPos() - oldpos;
}

dword __stdcall CCircle::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    pStream->ReadData(&m_Center, sizeof(WKSPointZ));
    pStream->ReadData(&m_Radius, sizeof(double));
    return pStream->GetPos() - oldpos;
}

GeometryType __stdcall CCircle::GetGeometryType() const
{
    return GEOMETRYTYPE_CIRCLE;
}

word __stdcall CCircle::GetDimension() const
{
    return 2;
}

bool __stdcall CCircle::GetMBR(WKSRect& mbr) const
{
    mbr.left = m_Center.x - m_Radius;
    mbr.right = m_Center.x + m_Radius;
    mbr.top = m_Center.y + m_Radius;
    mbr.bottom = m_Center.y - m_Radius;
    return true;
}

bool __stdcall CCircle::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CCircle(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool __stdcall CCircle::Clone(IObjPtr& pObj) const
{
    pObj = new CCircle(*this);
    return true;
}

bool __stdcall CCircle::Select(const WKSPoint& pnt)
{
    double a = m_Center.x - pnt.x;
    double b = m_Center.y - pnt.y;
    double c = sqrt(a*a + b*b);
    if (c < m_Radius)
    {
        return true;
    }
    return false;
}

bool __stdcall CCircle::Select(const WKSRect& envelope, bool PartialSelect)
{
    //拉倒
    if (PointInEnvelope(m_Center.x, m_Center.y, envelope))
    {
        return true;
    }

    WKSPoint pnt;
    pnt.x = envelope.left;
    pnt.y = envelope.top;
    if (this->Select(pnt))
    {
        return true;
    }

    pnt.x = envelope.left;
    pnt.y = envelope.bottom;
    if (this->Select(pnt))
    {
        return true;
    }

    pnt.x = envelope.right;
    pnt.y = envelope.bottom;
    if (this->Select(pnt))
    {
        return true;
    }

    pnt.x = envelope.right;
    pnt.y = envelope.top;
    if (this->Select(pnt))
    {
        return true;
    }

    return false;
}

bool __stdcall CCircle::Move(const double delta_x, const double delta_y)
{
    m_Center.x += delta_x;
    m_Center.y += delta_y;
    return true;
}

bool __stdcall CCircle::Ratate(const WKSPoint& origin, const double angle)
{
    return false;
}

bool CCircle::Disjoint(IGeometryPtr pGeometry)
{
    return true;
}

bool CCircle::Crosses(IGeometryPtr pGeometry)
{
    return true;
}

bool CCircle::Overlaps(IGeometryPtr pGeometry)
{
    return true;
}

bool CCircle::Within(IGeometryPtr pGeometry)
{
    return true;
}

bool CCircle::Contains(IGeometryPtr pGeometry)
{
    return true;
}

bool CCircle::Equals(IGeometryPtr pGeometry)
{
    return true;
}

bool CCircle::Touches(IGeometryPtr pGeometry)
{
    throw;
}

void __stdcall CCircle::SetCenter(const WKSPointZ& center)
{
    m_Center = center;
}

void __stdcall CCircle::GetCenter(WKSPointZ& center) const
{
    center = m_Center;
}

void __stdcall CCircle::SetRadius(const double radius)
{
    m_Radius = radius;
}

void __stdcall CCircle::GetRadius(double& radius) const
{
    radius = m_Radius;
}

void __stdcall CCircle::GetArea(double& area) const
{
    area = m_Radius*m_Radius*3.1415927;
}

void __stdcall CCircle::GetPerimeter(double& perimeter) const
{
    perimeter = 2*m_Radius*3.1415927;
}

//------------------------------------------------------------------------------
//  以下是CEllipse的定义
//------------------------------------------------------------------------------
CEllipse::CEllipse()
{
    INIT_REFCOUNT
}

CEllipse::CEllipse(const WKSRect& rect)
{
    INIT_REFCOUNT

    m_Env = rect;
    CorrectEnvelope(m_Env);
}

CEllipse::CEllipse(const CEllipse& envelope)
{
    INIT_REFCOUNT

    m_Env = envelope.m_Env;
}

CEllipse::~CEllipse()
{
}

bool __stdcall CEllipse::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IGeometry"))
        || (0 == strcmp(interfacename, "CGeometry"))
        || (0 == strcmp(interfacename, "IEllipse"))
        || (0 == strcmp(interfacename, "CEllipse")))
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

dword __stdcall CEllipse::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    pStream->WriteData(&m_Env, sizeof(WKSRect));
    return pStream->GetPos() - oldpos;
}

dword __stdcall CEllipse::_LoadInstance(IStreamX* pStream, void* const assist)
{
    pStream->ReadData(&m_Env, sizeof(WKSRect));
    return sizeof(WKSRect);
}

GeometryType __stdcall CEllipse::GetGeometryType() const
{
    return GEOMETRYTYPE_ELLIPSE;
}

word __stdcall CEllipse::GetDimension() const
{
    return 2;
}

bool __stdcall CEllipse::GetMBR(WKSRect& mbr) const
{
    mbr = m_Env;
    CorrectEnvelope(mbr);
    return true;
}

bool __stdcall CEllipse::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CEllipse(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool __stdcall CEllipse::Clone(IObjPtr& pObj) const
{
    pObj = new CEllipse(*this);
    return true;
}

bool __stdcall CEllipse::Select(const WKSPoint& pnt)
{
    WKSRect envelope = m_Env;
    CorrectEnvelope(envelope);
    if ((envelope.bottom < pnt.y)
        && (envelope.top > pnt.y)
        && (envelope.left < pnt.x)
        && (envelope.right > pnt.x))
    {
        return true;
    }
    else
    {
        return false;
    }
}

bool __stdcall CEllipse::Select(const WKSRect& envelope, bool PartialSelect)
{
    WKSRect selectenvelope = envelope;
    WKSRect thisenvelope = m_Env;
    CorrectEnvelope(selectenvelope);
    CorrectEnvelope(thisenvelope);
    if (PartialSelect)
    {
        return EnvelopesTouched(selectenvelope, thisenvelope);
    }
    else
    {
        return EnvelopesContented(selectenvelope, thisenvelope);
    }
}

bool __stdcall CEllipse::Move(const double delta_x, const double delta_y)
{
    m_Env.bottom += delta_y;
    m_Env.left += delta_x;
    m_Env.right += delta_x;
    m_Env.top += delta_y;

    return true;
}

bool __stdcall CEllipse::Ratate(const WKSPoint& origin, const double angle)
{
    return false;
}

bool CEllipse::Disjoint(IGeometryPtr pGeometry)
{
    return true;
}

bool CEllipse::Crosses(IGeometryPtr pGeometry)
{
    return true;
}

bool CEllipse::Overlaps(IGeometryPtr pGeometry)
{
    return true;
}

bool CEllipse::Within(IGeometryPtr pGeometry)
{
    return true;
}

bool CEllipse::Contains(IGeometryPtr pGeometry)
{
    return true;
}

bool CEllipse::Equals(IGeometryPtr pGeometry)
{
    return true;
}

bool CEllipse::Touches(IGeometryPtr pGeometry)
{
    throw;
}

void __stdcall CEllipse::SetMinX(const double minx)
{
    m_Env.left = minx;
}

void __stdcall CEllipse::SetMinY(const double miny)
{
    m_Env.bottom = miny;
}

void __stdcall CEllipse::SetMaxX(const double maxx)
{
    m_Env.right = maxx;
}

void __stdcall CEllipse::SetMaxY(const double maxy)
{
    m_Env.top = maxy;
}

void __stdcall CEllipse::GetMinX(double& minx) const
{
    minx = m_Env.left;
}

void __stdcall CEllipse::GetMinY(double& miny) const
{
    miny = m_Env.bottom;
}

void __stdcall CEllipse::GetMaxX(double& maxx) const
{
    maxx = m_Env.right;
}

void __stdcall CEllipse::GetMaxY(double& maxy) const
{
    maxy = m_Env.top;
}

void __stdcall CEllipse::GetArea(double& area) const
{
    area = 0;
}

void __stdcall CEllipse::GetPerimeter(double& perimeter) const
{
    perimeter = 0;
}

void __stdcall CEllipse::TrimEnvelope()
{
    CorrectEnvelope(m_Env);
}


//------------------------------------------------------------------------------
//  Geometry对象 <-> Stream
//------------------------------------------------------------------------------
dword Geometry2Stream(const IGeometryPtr pGeometry, CStreamPtr pStream)
{
    if (!pGeometry.Assigned() || !pStream.Assigned()) {return 0;}

    GeometryType geostreamtype = pGeometry->GetGeometryType();
    //写入1byte的geotype前缀
    pStream->Write(&geostreamtype, sizeof(GeometryType));

    switch(geostreamtype)
    {
    case GEOMETRYTYPE_POINT:
        return sizeof(GeometryType) + CPoint::_Point2Stream((CPoint*)pGeometry._p(), pStream);
    case GEOMETRYTYPE_MULTIPOINT:
        return sizeof(GeometryType) + CMultiPoint::_MultiPoint2Stream((CMultiPoint*)pGeometry._p(), pStream);
    case GEOMETRYTYPE_PATH:
        return sizeof(GeometryType) + CPath::_Path2Stream((CPath*)pGeometry._p(), pStream);
    case GEOMETRYTYPE_RING:
        return sizeof(GeometryType) + CRing::_Ring2Stream((CRing*)pGeometry._p(), pStream);
    case GEOMETRYTYPE_POLYLINE:
        return sizeof(GeometryType) + CPolyline::_Polyline2Stream((CPolyline*)pGeometry._p(), pStream);
    case GEOMETRYTYPE_POLYGON:
        return sizeof(GeometryType) + CPolygon::_Polygon2Stream((CPolygon*)pGeometry._p(), pStream);
    case GEOMETRYTYPE_ENVELOPE:
        return sizeof(GeometryType) + CEnvelope::_Envelope2Stream((CEnvelope*)pGeometry._p(), pStream);
    case GEOMETRYTYPE_CIRCLE:
        return sizeof(GeometryType) + CCircle::_Circle2Stream((CCircle*)pGeometry._p(), pStream);
    case GEOMETRYTYPE_ELLIPSE:
        return sizeof(GeometryType) + CEllipse::_Ellipse2Stream((CEllipse*)pGeometry._p(), pStream);
    default:
        return sizeof(GeometryType);
    }
}

dword Stream2Geometry(CStreamPtr pStream, IGeometryPtr& pGeometry)
{
    if (!pStream.Assigned()) {return 0;}

    //读出1byte的geotype前缀
    GeometryType geotype;
    pStream->Read(&geotype, sizeof(GeometryType));

    switch(geotype)
    {
    case GEOMETRYTYPE_POINT:
        {
            CPointPtr pGeoPoint = new CPoint;
            pGeometry = (IGeometry*)pGeoPoint._p();
            return sizeof(GeometryType) + CPoint::_Stream2Point(pStream, pGeoPoint);
        }
    case GEOMETRYTYPE_MULTIPOINT:
        {
            CMultiPointPtr pMultiPoint = new CMultiPoint;
            pGeometry = (IGeometry*)pMultiPoint._p();
            return sizeof(GeometryType) + CMultiPoint::_Stream2MultiPoint(pStream, pMultiPoint);
        }
    case GEOMETRYTYPE_PATH:
        {
            CPathPtr pPath = new CPath;
            pGeometry = (IGeometry*)pPath._p();
            return sizeof(GeometryType) + CPath::_Stream2Path(pStream, pPath);
        }
    case GEOMETRYTYPE_RING:
        {
            CRingPtr pRing = new CRing;
            pGeometry = (IGeometry*)pRing._p();
            return sizeof(GeometryType) + CRing::_Stream2Ring(pStream, pRing);
        }
    case GEOMETRYTYPE_POLYLINE:
        {
            CPolylinePtr pPolyline = new CPolyline;
            pGeometry = (IGeometry*)pPolyline._p();
            return sizeof(GeometryType) + CPolyline::_Stream2Polyline(pStream, pPolyline);
        }
    case GEOMETRYTYPE_POLYGON:
        {
            CPolygonPtr pPolygon = new CPolygon;
            pGeometry = (IGeometry*)pPolygon._p();
            return sizeof(GeometryType) + CPolygon::_Stream2Polygon(pStream, pPolygon);
        }
    case GEOMETRYTYPE_ENVELOPE:
        {
            CEnvelopePtr pEnvelope = new CEnvelope;
            pGeometry = (IGeometry*)pEnvelope._p();
            return sizeof(GeometryType) + CEnvelope::_Stream2Envelope(pStream, pEnvelope);
        }
    case GEOMETRYTYPE_CIRCLE:
        {
            CCirclePtr pCircle = new CCircle;
            pGeometry = (IGeometry*)pCircle._p();
            return sizeof(GeometryType) + CCircle::_Stream2Circle(pStream, pCircle);
        }
    case GEOMETRYTYPE_ELLIPSE:
        {
            CEllipsePtr pEllipse = new CEllipse;
            pGeometry = (IGeometry*)pEllipse._p();
            return sizeof(GeometryType) + CEllipse::_Stream2Ellipse(pStream, pEllipse);
        }
    default:
        {
            pGeometry = NULL;
            return sizeof(GeometryType);
        }
    }
}

dword CPoint::_Point2Stream(const CPointPtr pPoint, CStreamPtr pStream)
{
    if (!pPoint.Assigned() || !pStream.Assigned()) {return 0;}

    pStream->Write(&pPoint->m_Point, sizeof(WKSPointZ));
    return sizeof(WKSPointZ);
}

dword CPoint::_Stream2Point(CStreamPtr pStream, CPointPtr pPoint)
{
    if (!pStream.Assigned() || !pPoint.Assigned()) {return 0;}

    pStream->Read(&pPoint->m_Point, sizeof(WKSPointZ));
    return sizeof(WKSPointZ);
}

dword CMultiPoint::_MultiPoint2Stream(const CMultiPointPtr pMultiPoint, CStreamPtr pStream)
{
    if (!pMultiPoint.Assigned() || !pStream.Assigned()) {return 0;}

    dword oldpos = pStream->GetPos();

    dword pointcount = pMultiPoint->GetPointCount();
    pStream->Write(&pointcount, sizeof(dword));
    for (dword i = 0; i < pointcount; i++)
    {
        WKSPointZ point;
        pMultiPoint->GetPoint(point, i);
        pStream->Write(&point, sizeof(WKSPointZ));
    }
    return pStream->GetPos() - oldpos;
}

dword CMultiPoint::_Stream2MultiPoint(CStreamPtr pStream, CMultiPointPtr pMultiPoint)
{
    if (!pStream.Assigned() || !pMultiPoint.Assigned()) {return 0;}

    pMultiPoint->ClearPoint();

    dword oldpos = pStream->GetPos();
    dword itemcount;
    pStream->Read(&itemcount, sizeof(dword));
    for (dword i = 0; i < itemcount; i++)
    {
        WKSPointZ point;
        pStream->Read(&point, sizeof(WKSPointZ));
        pMultiPoint->AddPoint(point);
    }
    return pStream->GetPos() - oldpos;
}

dword CPath::_Path2Stream(const CPathPtr pPath, CStreamPtr pStream)
{
    if (!pPath.Assigned() || !pStream.Assigned()) {return 0;}

    dword oldpos = pStream->GetPos();

    dword pointcount = pPath->GetPointCount();
    pStream->Write(&pointcount, sizeof(dword));
    for (dword i = 0; i < pointcount; i++)
    {
        WKSPointZ point;
        VertexType vertextype;
        pPath->GetPoint(i, point, vertextype);
        pStream->Write(&point, sizeof(WKSPointZ));
        pStream->Write(&vertextype, sizeof(VertexType));
    }
    return pStream->GetPos() - oldpos;
}

dword CPath::_Stream2Path(CStreamPtr pStream, CPathPtr pPath)
{
    if (!pStream.Assigned() || !pPath.Assigned()) {return 0;}

    pPath->ClearPoint();

    dword oldpos = pStream->GetPos();

    dword pointcount;
    pStream->Read(&pointcount, sizeof(dword));
    for (dword i = 0; i < pointcount; i++)
    {
        WKSPointZ point;
        pStream->Read(&point, sizeof(WKSPointZ));
        VertexType vertextype;
        pStream->Read(&vertextype, sizeof(VertexType));
        pPath->AddPoint(point, vertextype);
    }
    return pStream->GetPos() - oldpos;
}

dword CRing::_Ring2Stream(const CRingPtr pRing, CStreamPtr pStream)
{
    if (!pRing.Assigned() || !pStream.Assigned()) {return 0;}

    dword oldpos = pStream->GetPos();

    dword pointcount = pRing->GetPointCount();
    pStream->Write(&pointcount, sizeof(dword));
    for (dword i = 0; i < pointcount; i++)
    {
        WKSPointZ point;
        VertexType vertextype;
        pRing->GetPoint(i, point, vertextype);
        pStream->Write(&point, sizeof(WKSPointZ));
        pStream->Write(&vertextype, sizeof(VertexType));
    }
    return pStream->GetPos() - oldpos;
}

dword CRing::_Stream2Ring(CStreamPtr pStream, CRingPtr pRing)
{
    if (!pStream.Assigned() || !pRing.Assigned()) {return 0;}

    pRing->ClearPoint();

    dword oldpos = pStream->GetPos();

    dword pointcount;
    pStream->Read(&pointcount, sizeof(dword));
    for (dword i = 0; i < pointcount; i++)
    {
        WKSPointZ point;
        pStream->Read(&point, sizeof(WKSPointZ));
        VertexType vertextype;
        pStream->Read(&vertextype, sizeof(VertexType));
        pRing->AddPoint(point, vertextype);
    }
    return pStream->GetPos() - oldpos;
}

dword CPolyline::_Polyline2Stream(const CPolylinePtr pPolyline, CStreamPtr pStream)
{
    if (!pPolyline.Assigned() || !pStream.Assigned()) {return 0;}

    dword oldpos = pStream->GetPos();

    dword itemcount = pPolyline->GetPathCount();
    pStream->Write(&itemcount, sizeof(dword));
    for (dword i = 0; i < itemcount; i++)
    {
        CPathPtr pPath;
        pPolyline->GetPathRef(pPath, i);
        CPath::_Path2Stream(pPath, pStream);
    }
    return pStream->GetPos() - oldpos;
}

dword CPolyline::_Stream2Polyline(CStreamPtr pStream, CPolylinePtr pPolyline)
{
    if (!pStream.Assigned() || !pPolyline.Assigned()) {return 0;}

    pPolyline->ClearPath();

    dword oldpos = pStream->GetPos();

    dword itemcount;
    pStream->Read(&itemcount, sizeof(dword));
    for (dword i = 0; i < itemcount; i++)
    {
        CPathPtr pPath = new CPath;
        CPath::_Stream2Path(pStream, pPath);
        pPolyline->AddPathRef(pPath);
    }
    return pStream->GetPos() - oldpos;
}

dword CPolygon::_Polygon2Stream(const CPolygonPtr pPolygon, CStreamPtr pStream)
{
    if (!pPolygon.Assigned() || !pStream.Assigned()) {return 0;}

    dword oldpos = pStream->GetPos();

    dword itemcount = pPolygon->GetRingCount();
    pStream->Write(&itemcount, sizeof(dword));
    for (dword i = 0; i < itemcount; i++)
    {
        CRingPtr pRing;
        pPolygon->GetRingRef(pRing, i);
        CRing::_Ring2Stream(pRing, pStream);
    }
    return pStream->GetPos() - oldpos;
}

dword CPolygon::_Stream2Polygon(CStreamPtr pStream, CPolygonPtr pPolygon)
{
    if (!pStream.Assigned() || !pPolygon.Assigned()) {return 0;}

    pPolygon->ClearRing();

    dword oldpos = pStream->GetPos();

    dword itemcount;
    pStream->Read(&itemcount, sizeof(dword));
    for (dword i = 0; i < itemcount; i++)
    {
        CRingPtr pRing = new CRing;
        CRing::_Stream2Ring(pStream, pRing);
        pPolygon->AddRingRef(pRing);
    }
    return pStream->GetPos() - oldpos;
}

dword CEnvelope::_Envelope2Stream(const CEnvelopePtr pEnvelope, CStreamPtr pStream)
{
    if (!pStream.Assigned() || !pEnvelope.Assigned()) {return 0;}

    pStream->Write(&pEnvelope->m_Env, sizeof(WKSRect));
    return sizeof(WKSRect);
}

dword CEnvelope::_Stream2Envelope(CStreamPtr pStream, CEnvelopePtr pEnvelope)
{
    if (!pStream.Assigned() || !pEnvelope.Assigned()) {return 0;}

    pStream->Read(&pEnvelope->m_Env, sizeof(WKSRect));
    return sizeof(WKSRect);
}

dword CCircle::_Circle2Stream(const CCirclePtr pCircle, CStreamPtr pStream)
{
    if (!pStream.Assigned() || !pCircle.Assigned()) {return 0;}

    pStream->Write(&pCircle->m_Center, sizeof(WKSPointZ));
    pStream->Write(&pCircle->m_Radius, sizeof(double));
    return sizeof(WKSPoint) + sizeof(double);
}

dword CCircle::_Stream2Circle(CStreamPtr pStream, CCirclePtr pCircle)
{
    if (!pStream.Assigned() || !pCircle.Assigned()) {return 0;}

    pStream->Read(&pCircle->m_Center, sizeof(WKSPointZ));
    pStream->Read(&pCircle->m_Radius, sizeof(double));
    return sizeof(WKSPoint) + sizeof(double);
}

dword CEllipse::_Ellipse2Stream(const CEllipsePtr pEllipse, CStreamPtr pStream)
{
    if (!pStream.Assigned() || !pEllipse.Assigned()) {return 0;}

    pStream->Write(&pEllipse->m_Env, sizeof(WKSRect));
    return sizeof(WKSRect);
}

dword CEllipse::_Stream2Ellipse(CStreamPtr pStream, CEllipsePtr pEllipse)
{
    if (!pStream.Assigned() || !pEllipse.Assigned()) {return 0;}

    pStream->Read(&pEllipse->m_Env, sizeof(WKSRect));
    return sizeof(WKSRect);
}

//------------------------------------------------------------------------------


IPolylinePtr Polygon2Polyline(const IPolygonPtr pPolygon)
{
    CPolygonPtr pPolygon1 = (CPolygon*)pPolygon._p();

    CPolylinePtr pPolyline = new CPolyline;
    long ringcount = pPolygon1->GetRingCount();
    for (int i = 0; i < ringcount; i++)
    {
        CRingPtr pRing;
        pPolygon1->GetRingRef(pRing, i);

        CPathPtr pPath = new CPath;
        long pointcount = pRing->GetPointCount();
        for (int j = 0; j < pointcount; j++)
        {
            WKSPointZ point;
            pRing->GetPoint1(j, point);
            pPath->AddPoint(point);
        }

        pPolyline->AddPathRef(pPath);
    }

    return (IPolyline*)pPolyline._p();
}

IPathPtr Ring2Path(const IRingPtr pRing)
{
    IPathPtr pPath = new CPath;

    long pointcount = pRing->GetPointCount();
    if (pointcount < 1)
    {
        return pPath;
    }

    WKSPointZ point, start;
    pRing->GetPoint1(0, start);
    pPath->AddPoint(start);
    for (int i = 1; i < pointcount; i++)
    {
        pRing->GetPoint1(i, point);
        pPath->AddPoint(point);
    }

    if ((pointcount > 2)
        && (!equal(point.x, start.x) || !equal(point.y, start.y)))
    {
        pPath->AddPoint(start);
    }

    return pPath;
}

}
