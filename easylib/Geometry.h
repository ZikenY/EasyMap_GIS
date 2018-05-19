#if !defined(GEOMETRY_INCLUDED_)
#define GEOMETRY_INCLUDED_

#include "CommonInclude.h"
#include "..\\include\\InterfaceGeometry.h"

namespace easymap
{

class CPoint;
class CMultiPoint;
class CPath;
class CRing;
class CPolyline;
class CPolygon;
class CEnvelope;
class CCircle;
class CEllipse;

typedef TSmartPtr<CPoint>       CPointPtr;
typedef TSmartPtr<CMultiPoint>  CMultiPointPtr;
typedef TSmartPtr<CPath>        CPathPtr;
typedef TSmartPtr<CRing>        CRingPtr;
typedef TSmartPtr<CPolyline>    CPolylinePtr;
typedef TSmartPtr<CPolygon>     CPolygonPtr;
typedef TSmartPtr<CEnvelope>    CEnvelopePtr;
typedef TSmartPtr<CCircle>      CCirclePtr;
typedef TSmartPtr<CEllipse>     CEllipsePtr;

//------------------------------------------------------------------------------
//  Geometry对象 <-> Stream
//  这两个函数是独立于Persist功能的，不存储类型信息，因此冗余小，用于存储矢量数据
//  注意这两个函数所操作的stream头部是1byte的前缀，用于标识GeometryType
dword Geometry2Stream(const IGeometryPtr pGeometry, CStreamPtr pStream);
//  这个函数中创建了一个Geometry！！！
dword Stream2Geometry(CStreamPtr pStream, IGeometryPtr& pGeometry);
//------------------------------------------------------------------------------

//================================================================================
//  点类型，由x, y, z组成
//================================================================================
class CPoint : public IPoint
{
CLASS_NAME(CPoint)
PERSIST_DUMP(CPoint)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CPoint();
    CPoint(const WKSPointZ& point);
    CPoint(const CPoint& point);
private:
    ~CPoint();

private:
    WKSPointZ m_Point;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool __stdcall Clone(IObjPtr& pObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    GeometryType __stdcall GetGeometryType() const;
    word __stdcall GetDimension() const;
    bool __stdcall GetMBR(WKSRect& mbr) const;
    bool __stdcall Select(const WKSPoint& pnt);
    bool __stdcall Select(const WKSRect& envelope, bool PartialSelect = true);
    bool __stdcall Move(const double delta_x, const double delta_y);
    bool __stdcall Ratate(const WKSPoint& origin, const double angle);

public:
    bool Disjoint(IGeometryPtr pGeometry);
    bool Crosses(IGeometryPtr pGeometry);
    bool Overlaps(IGeometryPtr pGeometry);
    bool Within(IGeometryPtr pGeometry);
    bool Contains(IGeometryPtr pGeometry);
    bool Equals(IGeometryPtr pGeometry);
    bool Touches(IGeometryPtr pGeometry);

public:
    void __stdcall SetX(const double x);
    void __stdcall SetY(const double y);
    void __stdcall SetZ(const double z);
    void __stdcall GetX(double& x) const;
    void __stdcall GetY(double& y) const;
    void __stdcall GetZ(double& z) const;
    void __stdcall SetCoordinates(const double x, const double y, const double z);
    void __stdcall GetCoordinates(double& x, double& y, double& z) const;
    void __stdcall SetCoordinates(const WKSPointZ& point);
    void __stdcall GetCoordinates(WKSPointZ& point) const;

    static dword _Point2Stream(const CPointPtr pPoint, CStreamPtr pStream);
    static dword _Stream2Point(CStreamPtr pStream, CPointPtr pPoint);
};
//================================================================================


//================================================================================
//  点群，由多个Point组成
//================================================================================
class CMultiPoint : public IMultiPoint
{
CLASS_NAME(CMultiPoint)
PERSIST_DUMP(CMultiPoint)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CMultiPoint();
    CMultiPoint(const CMultiPoint& multipoint);
private:
    ~CMultiPoint();

private:
    vector<WKSPointZ> m_Points;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool __stdcall Clone(IObjPtr& pObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    GeometryType __stdcall GetGeometryType() const;
    word __stdcall GetDimension() const;
    bool __stdcall GetMBR(WKSRect& mbr) const;
    bool __stdcall Select(const WKSPoint& pnt);
    bool __stdcall Select(const WKSRect& envelope, bool PartialSelect = true);
    bool __stdcall Move(const double delta_x, const double delta_y);
    bool __stdcall Ratate(const WKSPoint& origin, const double angle);

public:
    bool Disjoint(IGeometryPtr pGeometry);
    bool Crosses(IGeometryPtr pGeometry);
    bool Overlaps(IGeometryPtr pGeometry);
    bool Within(IGeometryPtr pGeometry);
    bool Contains(IGeometryPtr pGeometry);
    bool Equals(IGeometryPtr pGeometry);
    bool Touches(IGeometryPtr pGeometry);

public:
    //管理其中的点
    bool __stdcall AddPoint(const WKSPointZ& point);
    bool __stdcall DeletePoint(const dword index);
    void __stdcall ClearPoint();
    bool __stdcall GetPoint(WKSPointZ& point, const dword index) const;
    bool __stdcall SetPointOrder(const dword oldorder, const dword neworder);
    dword __stdcall GetPointCount() const;

    static dword _MultiPoint2Stream(const CMultiPointPtr pMultiPoint, CStreamPtr pStream);
    static dword _Stream2MultiPoint(CStreamPtr pStream, CMultiPointPtr pMultiPoint);
};
//================================================================================


//================================================================================
//  带点类型的点数组，用来支持path和ring
//================================================================================
class CVertexArray
{
private:
    typedef struct
    {
        WKSPointZ PointData;
        VertexType PointType;
    }VertexPoint;

private:
    CVertexArray();
    CVertexArray(const CVertexArray& vertexarray);
    CVertexArray& operator=(const CVertexArray& rhs);
    ~CVertexArray();

private:
    vector<VertexPoint> m_VertexPoints;

private:
    bool Add_Point(const VertexPoint& point);
    bool Insert_Point(const dword index, const VertexPoint& point);
    bool Delete_Point(const dword index);
    void Clear_Points();
    bool Set_Point(const dword index, const VertexPoint& point);
    bool Get_Point(const dword index, VertexPoint& point) const;
    dword Get_PointCount() const;

    bool Get_MBR(WKSRect& mbr) const;

    bool Get_Length(double& length) const;
    bool Get_Perimeter(double& perimeter) const;

    //  积分面积
    bool Get_Area(double& area) const;

    //  方向性，根据积分面积的正负号来判断
    bool Is_PostiveDirection() const;

    bool Get_PathValid() const;
    bool Get_RingValid() const;

    void VPA2VP(vector<WKSPoint>& pnts) const;
    void VPA2VP_Reverse(vector<WKSPoint>& pnts) const;

friend class CPath;
friend class CRing;
};
//================================================================================


//================================================================================
//  连续线类型
//================================================================================
class CPath : public IPath
{
CLASS_NAME(CPath)
PERSIST_DUMP(CPath)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CPath();
    CPath(const CPath& path);
    CPath(const WKSRect& envelope);
private:
    ~CPath();

private:
    CVertexArray m_VertexPointArray;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    GeometryType __stdcall GetGeometryType() const;
    word __stdcall GetDimension() const;
    bool __stdcall GetMBR(WKSRect& mbr) const;
    bool __stdcall Clone(IObjPtr& pObj) const;
    bool __stdcall Select(const WKSPoint& pnt);
    bool __stdcall Select(const WKSRect& envelope, bool PartialSelect = true);
    bool __stdcall Move(const double delta_x, const double delta_y);
    bool __stdcall Ratate(const WKSPoint& origin, const double angle);

public:
    bool Disjoint(IGeometryPtr pGeometry);
    bool Crosses(IGeometryPtr pGeometry);
    bool Overlaps(IGeometryPtr pGeometry);
    bool Within(IGeometryPtr pGeometry);
    bool Contains(IGeometryPtr pGeometry);
    bool Equals(IGeometryPtr pGeometry);
    bool Touches(IGeometryPtr pGeometry);

public:
    bool __stdcall AddPoint(const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON);
    bool __stdcall InsertPoint(const dword index, const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON);
    bool __stdcall DeletePoint(const dword index);
    void __stdcall ClearPoint();
    bool __stdcall SetPoint(const dword index, const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON);
    bool __stdcall GetPoint(const dword index, WKSPointZ& point,
        VertexType& vertextype) const;
    bool __stdcall GetPoint1(const dword index, WKSPointZ& point) const;
    dword __stdcall GetPointCount() const;

    bool __stdcall GetLength(double& length) const;
    bool __stdcall GetPositionByDistance(const double distance, WKSPoint& position) const;

    bool __stdcall IsValid() const;

    static dword _Path2Stream(const CPathPtr pPath, CStreamPtr pStream);
    static dword _Stream2Path(CStreamPtr pStream, CPathPtr pPath);
};
//================================================================================


//================================================================================
//  连续圈，注意圈是有方向性的
//================================================================================
class CRing : public IRing
{
CLASS_NAME(CRing)
PERSIST_DUMP(CRing)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CRing();
    CRing(const CRing& ring);
    CRing(const WKSRect& envelope);
private:
    ~CRing();

private:
    CVertexArray m_VertexPointArray;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    GeometryType __stdcall GetGeometryType() const;
    word __stdcall GetDimension() const;
    bool __stdcall GetMBR(WKSRect& mbr) const;
    bool __stdcall Clone(IObjPtr& pObj) const;
    bool __stdcall Select(const WKSPoint& pnt);
    bool __stdcall Select(const WKSRect& envelope, bool PartialSelect = true);
    bool __stdcall Move(const double delta_x, const double delta_y);
    bool __stdcall Ratate(const WKSPoint& origin, const double angle);

public:
    bool Disjoint(IGeometryPtr pGeometry);
    bool Crosses(IGeometryPtr pGeometry);
    bool Overlaps(IGeometryPtr pGeometry);
    bool Within(IGeometryPtr pGeometry);
    bool Contains(IGeometryPtr pGeometry);
    bool Equals(IGeometryPtr pGeometry);
    bool Touches(IGeometryPtr pGeometry);

public:
    bool __stdcall AddPoint(const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON);
    bool __stdcall InsertPoint(const dword index, const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON);
    bool __stdcall DeletePoint(const dword index);
    void __stdcall ClearPoint();
    bool __stdcall SetPoint(const dword index, const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON);
    bool __stdcall GetPoint(const dword index, WKSPointZ& point,
        VertexType& vertextype) const;
    bool __stdcall GetPoint1(const dword index, WKSPointZ& point) const;
    dword __stdcall GetPointCount() const;

    //  注意积分面积是带符号的
    bool __stdcall GetArea(double& area) const;

    //  周长
    bool __stdcall GetPerimeter(double& perimeter) const;

    //  方向性，根据积分面积的正负号来判断
    bool __stdcall IsPostiveDirection() const;

    bool __stdcall IsValid() const;

    static dword _Ring2Stream(const CRingPtr pRing, CStreamPtr pStream);
    static dword _Stream2Ring(CStreamPtr pStream, CRingPtr pRing);
};
//================================================================================


//================================================================================
//  复合折线，由多个Path组成
//================================================================================
class CPolyline : public IPolyline
{
CLASS_NAME(CPolyline)
PERSIST_DUMP(CPolyline)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CPolyline();
    CPolyline(const CPolyline& polyline);
private:
    ~CPolyline();

private:
    vector<CPathPtr> m_Items;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    GeometryType __stdcall GetGeometryType() const;
    word __stdcall GetDimension() const;
    bool __stdcall GetMBR(WKSRect& mbr) const;
    bool __stdcall Clone(IObjPtr& pObj) const;
    bool __stdcall Select(const WKSPoint& pnt);
    bool __stdcall Select(const WKSRect& envelope, bool PartialSelect = true);
    bool __stdcall Move(const double delta_x, const double delta_y);
    bool __stdcall Ratate(const WKSPoint& origin, const double angle);

public:
    bool Disjoint(IGeometryPtr pGeometry);
    bool Crosses(IGeometryPtr pGeometry);
    bool Overlaps(IGeometryPtr pGeometry);
    bool Within(IGeometryPtr pGeometry);
    bool Contains(IGeometryPtr pGeometry);
    bool Equals(IGeometryPtr pGeometry);
    bool Touches(IGeometryPtr pGeometry);

public:
    //  注意使用的是所传入飞机的引用
    bool __stdcall AddPathRef(CPathPtr pPath);
    bool __stdcall AddPathRef(IPath* pPath);

    bool __stdcall DeletePath(const dword index);

    void __stdcall ClearPath();

    //  注意所传出的飞机是内部Item的引用
    bool __stdcall GetPathRef(CPathPtr& pPath, const dword index) const;
    bool __stdcall GetPathRef(IPath** ppPath, const dword index) const;

    bool __stdcall SetPathOrder(const dword oldorder, const dword neworder);

    dword __stdcall GetPathCount() const;

    bool __stdcall GetLength(double& length) const;
    bool __stdcall GetPositionByDistance(const double distance, WKSPoint& position) const;

    bool __stdcall IsValid() const;

    static dword _Polyline2Stream(const CPolylinePtr pPolyline, CStreamPtr pStream);
    static dword _Stream2Polyline(CStreamPtr pStream, CPolylinePtr pPolyline);
};
//================================================================================


//================================================================================
//  复合多边形，由多个Ring组成
//================================================================================
class CPolygon : public IPolygon
{
CLASS_NAME(CPolygon)
PERSIST_DUMP(CPolygon)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CPolygon();
    CPolygon(const CPolygon& polygon);
private:
    ~CPolygon();

private:
    vector<CRingPtr> m_Items;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    GeometryType __stdcall GetGeometryType() const;
    word __stdcall GetDimension() const;
    bool __stdcall GetMBR(WKSRect& mbr) const;
    bool __stdcall Clone(IObjPtr& pObj) const;
    bool __stdcall Select(const WKSPoint& pnt);
    bool __stdcall Select(const WKSRect& envelope, bool PartialSelect = true);
    bool __stdcall Move(const double delta_x, const double delta_y);
    bool __stdcall Ratate(const WKSPoint& origin, const double angle);

public:
    bool Disjoint(IGeometryPtr pGeometry);
    bool Crosses(IGeometryPtr pGeometry);
    bool Overlaps(IGeometryPtr pGeometry);
    bool Within(IGeometryPtr pGeometry);
    bool Contains(IGeometryPtr pGeometry);
    bool Equals(IGeometryPtr pGeometry);
    bool Touches(IGeometryPtr pGeometry);

public:
    //  注意使用的是所传入飞机的引用
    bool __stdcall AddRingRef(CRingPtr pRing);
    bool __stdcall AddRingRef(IRing* pRing);

    bool __stdcall DeleteRing(const dword index);

    void __stdcall ClearRing();

    //  注意所传出的飞机是内部Item的引用
    bool __stdcall GetRingRef(CRingPtr& pRing, const dword index) const;
    bool __stdcall GetRingRef(IRing** ppRing, const dword index) const;

    bool __stdcall SetRingOrder(const dword oldorder, const dword neworder);

    dword __stdcall GetRingCount() const;

    bool __stdcall GetArea(double& area) const;
    bool __stdcall GetPerimeter(double& perimeter) const;
    bool __stdcall IsValid() const;

    static dword _Polygon2Stream(const CPolygonPtr pPolygon, CStreamPtr pStream);
    static dword _Stream2Polygon(CStreamPtr pStream, CPolygonPtr pPolygon);
};
//================================================================================


//================================================================================
//  矩形
//================================================================================
class CEnvelope : public IEnvelope
{
CLASS_NAME(CEnvelope)
PERSIST_DUMP(CEnvelope)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CEnvelope();
    CEnvelope(const WKSRect& rect);
    CEnvelope(const CEnvelope& envelope);
private:
    ~CEnvelope();

private:
    WKSRect m_Env;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    GeometryType __stdcall GetGeometryType() const;
    word __stdcall GetDimension() const;
    bool __stdcall GetMBR(WKSRect& mbr) const;
    bool __stdcall Clone(IObjPtr& pObj) const;
    bool __stdcall Select(const WKSPoint& pnt);
    bool __stdcall Select(const WKSRect& envelope, bool PartialSelect = true);
    bool __stdcall Move(const double delta_x, const double delta_y);
    bool __stdcall Ratate(const WKSPoint& origin, const double angle);

public:
    bool Disjoint(IGeometryPtr pGeometry);
    bool Crosses(IGeometryPtr pGeometry);
    bool Overlaps(IGeometryPtr pGeometry);
    bool Within(IGeometryPtr pGeometry);
    bool Contains(IGeometryPtr pGeometry);
    bool Equals(IGeometryPtr pGeometry);
    bool Touches(IGeometryPtr pGeometry);

public:
    void __stdcall SetMinX(const double minx);
    void __stdcall SetMinY(const double miny);
    void __stdcall SetMaxX(const double maxx);
    void __stdcall SetMaxY(const double maxy);
    void __stdcall GetMinX(double& minx) const;
    void __stdcall GetMinY(double& miny) const;
    void __stdcall GetMaxX(double& maxx) const;
    void __stdcall GetMaxY(double& maxy) const;
    void __stdcall GetArea(double& area) const;
    void __stdcall GetPerimeter(double& perimeter) const;
    void __stdcall TrimEnvelope();

    static dword _Envelope2Stream(const CEnvelopePtr pEnvelope, CStreamPtr pStream);
    static dword _Stream2Envelope(CStreamPtr pStream, CEnvelopePtr pEnvelope);
};
//================================================================================


//================================================================================
//  圆
//================================================================================
class CCircle : public ICircle
{
CLASS_NAME(CCircle)
PERSIST_DUMP(CCircle)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CCircle();
    CCircle(const WKSPointZ& center, const double& radius);
    CCircle(const CCircle& circle);
private:
    ~CCircle();

private:
    WKSPointZ m_Center;
    double m_Radius;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    GeometryType __stdcall GetGeometryType() const;
    word __stdcall GetDimension() const;
    bool __stdcall GetMBR(WKSRect& mbr) const;
    bool __stdcall Clone(IObjPtr& pObj) const;
    bool __stdcall Select(const WKSPoint& pnt);
    bool __stdcall Select(const WKSRect& envelope, bool PartialSelect = true);
    bool __stdcall Move(const double delta_x, const double delta_y);
    bool __stdcall Ratate(const WKSPoint& origin, const double angle);

public:
    bool Disjoint(IGeometryPtr pGeometry);
    bool Crosses(IGeometryPtr pGeometry);
    bool Overlaps(IGeometryPtr pGeometry);
    bool Within(IGeometryPtr pGeometry);
    bool Contains(IGeometryPtr pGeometry);
    bool Equals(IGeometryPtr pGeometry);
    bool Touches(IGeometryPtr pGeometry);

public:
    void __stdcall SetCenter(const WKSPointZ& center);
    void __stdcall GetCenter(WKSPointZ& center) const;
    void __stdcall SetRadius(const double radius);
    void __stdcall GetRadius(double& radius) const;
    void __stdcall GetArea(double& area) const;
    void __stdcall GetPerimeter(double& perimeter) const;

    static dword _Circle2Stream(const CCirclePtr pCircle, CStreamPtr pStream);
    static dword _Stream2Circle(CStreamPtr pStream, CCirclePtr pCircle);
};
//================================================================================


//================================================================================
//  椭圆
//================================================================================
class CEllipse : public IEllipse
{
CLASS_NAME(CEllipse)
PERSIST_DUMP(CEllipse)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CEllipse();
    CEllipse(const WKSRect& rect);
    CEllipse(const CEllipse& envelope);
private:
    ~CEllipse();

private:
    WKSRect m_Env;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    GeometryType __stdcall GetGeometryType() const;
    word __stdcall GetDimension() const;
    bool __stdcall GetMBR(WKSRect& mbr) const;
    bool __stdcall Clone(IObjPtr& pObj) const;
    bool __stdcall Select(const WKSPoint& pnt);
    bool __stdcall Select(const WKSRect& envelope, bool PartialSelect = true);
    bool __stdcall Move(const double delta_x, const double delta_y);
    bool __stdcall Ratate(const WKSPoint& origin, const double angle);

public:
    bool Disjoint(IGeometryPtr pGeometry);
    bool Crosses(IGeometryPtr pGeometry);
    bool Overlaps(IGeometryPtr pGeometry);
    bool Within(IGeometryPtr pGeometry);
    bool Contains(IGeometryPtr pGeometry);
    bool Equals(IGeometryPtr pGeometry);
    bool Touches(IGeometryPtr pGeometry);

public:
    void __stdcall SetMinX(const double minx);
    void __stdcall SetMinY(const double miny);
    void __stdcall SetMaxX(const double maxx);
    void __stdcall SetMaxY(const double maxy);
    void __stdcall GetMinX(double& minx) const;
    void __stdcall GetMinY(double& miny) const;
    void __stdcall GetMaxX(double& maxx) const;
    void __stdcall GetMaxY(double& maxy) const;
    void __stdcall GetArea(double& area) const;
    void __stdcall GetPerimeter(double& perimeter) const;
    void __stdcall TrimEnvelope();

    static dword _Ellipse2Stream(const CEllipsePtr pEllipse, CStreamPtr pStream);
    static dword _Stream2Ellipse(CStreamPtr pStream, CEllipsePtr pEllipse);
};
//================================================================================

CLASS_FACTORY(CPoint)
CLASS_FACTORY(CMultiPoint)
CLASS_FACTORY(CPath)
CLASS_FACTORY(CRing)
CLASS_FACTORY(CPolyline)
CLASS_FACTORY(CPolygon)
CLASS_FACTORY(CEnvelope)
CLASS_FACTORY(CCircle)
CLASS_FACTORY(CEllipse)

IPolylinePtr Polygon2Polyline(const IPolygonPtr pPolygon);
IPathPtr Ring2Path(const IRingPtr pRing);

}

#endif