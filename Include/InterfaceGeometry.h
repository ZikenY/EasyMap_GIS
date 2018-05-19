#if !defined(INTERFACEGEOMETRY_INCLUDED_)
#define INTERFACEGEOMETRY_INCLUDED_

#include "WKSInclude.h"
#include "InterfacePersist.h"

namespace easymap
{

//几何对象类型
typedef long GeometryType;
const GeometryType GEOMETRYTYPE_UNKNOWN     = 0;  //天知道是什么
const GeometryType GEOMETRYTYPE_POINT       = 1;  //点
const GeometryType GEOMETRYTYPE_MULTIPOINT  = 2;  //点群
const GeometryType GEOMETRYTYPE_PATH        = 3;  //连续线
const GeometryType GEOMETRYTYPE_RING        = 4;  //圈
const GeometryType GEOMETRYTYPE_POLYLINE    = 5;  //复合折线
const GeometryType GEOMETRYTYPE_POLYGON     = 6;  //复合多边形
const GeometryType GEOMETRYTYPE_ENVELOPE    = 7;  //矩形
const GeometryType GEOMETRYTYPE_CIRCLE      = 8;  //圆
const GeometryType GEOMETRYTYPE_ELLIPSE     = 9;

//FT!
typedef long ShapeType;
const ShapeType SHAPETYPE_UNKNOWN           = 0;
const ShapeType SHAPETYPE_POINT             = 1;
const ShapeType SHAPETYPE_MULTIPOINT        = 2;
const ShapeType SHAPETYPE_POLYLINE          = 3;
const ShapeType SHAPETYPE_POLYGON           = 4;

//点在path、ring中的类型（普通点还是圆弧的起点）
typedef long VertexType;
const VertexType VERTEXTYPE_COMMON          = 0;
const VertexType VERTEXTYPE_ARCSTART        = 1;

class IGeometry;
class IPoint;
class IMultiPoint;
class IPath;
class IRing;
class IPolyline;
class IPolygon;
class IEnvelope;
class ICircle;
class IEllipse;
typedef TSmartPtr<IGeometry>    IGeometryPtr;
typedef TSmartPtr<IPoint>       IPointPtr;
typedef TSmartPtr<IMultiPoint>  IMultiPointPtr;
typedef TSmartPtr<IPath>        IPathPtr;
typedef TSmartPtr<IRing>        IRingPtr;
typedef TSmartPtr<IPolyline>    IPolylinePtr;
typedef TSmartPtr<IPolygon>     IPolygonPtr;
typedef TSmartPtr<IEnvelope>    IEnvelopePtr;
typedef TSmartPtr<ICircle>      ICirclePtr;
typedef TSmartPtr<IEllipse>     IEllipsePtr;

//================================================================================
//  几何对象的原型
//================================================================================
class IGeometry : public IPersist
{
public:
    virtual GeometryType __stdcall GetGeometryType() const = 0;
    virtual word __stdcall GetDimension() const = 0;
    virtual bool __stdcall GetMBR(WKSRect& mbr) const = 0;
    virtual bool __stdcall Select(const WKSRect& envelope, bool PartialSelect = true) = 0;
    virtual bool __stdcall Move(const double delta_x, const double delta_y) = 0;
    virtual bool __stdcall Ratate(const WKSPoint& origin, const double angle) = 0;
};
//================================================================================


class IPoint : public IGeometry
{
public:
    virtual void __stdcall SetX(const double x) = 0;
    virtual void __stdcall SetY(const double y) = 0;
    virtual void __stdcall SetZ(const double z) = 0;
    virtual void __stdcall GetX(double& x) const = 0;
    virtual void __stdcall GetY(double& y) const = 0;
    virtual void __stdcall GetZ(double& z) const = 0;
    virtual void __stdcall SetCoordinates(const WKSPointZ& point) = 0;
    virtual void __stdcall GetCoordinates(WKSPointZ& point) const = 0;
};

class IMultiPoint : public IGeometry
{
public:
    virtual bool __stdcall AddPoint(const WKSPointZ& point) = 0;
    virtual bool __stdcall DeletePoint(const dword index) = 0;
    virtual void __stdcall ClearPoint() = 0;
    virtual bool __stdcall GetPoint(WKSPointZ& point, const dword index) const = 0;
    virtual bool __stdcall SetPointOrder(const dword oldorder, const dword neworder) = 0;
    virtual dword __stdcall GetPointCount() const = 0;
};

class IPath : public IGeometry
{
public:
    virtual bool __stdcall AddPoint(const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON) = 0;
    virtual bool __stdcall InsertPoint(const dword index, const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON) = 0;
    virtual bool __stdcall DeletePoint(const dword index) = 0;
    virtual void __stdcall ClearPoint() = 0;
    virtual bool __stdcall SetPoint(const dword index, const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON) = 0;
    virtual bool __stdcall GetPoint(const dword index, WKSPointZ& point,
        VertexType& vertextype) const = 0;
    virtual bool __stdcall GetPoint1(const dword index, WKSPointZ& point) const = 0;
    virtual dword __stdcall GetPointCount() const = 0;

    virtual bool __stdcall GetLength(double& length) const = 0;
    virtual bool __stdcall GetPositionByDistance(const double distance, WKSPoint& position) const = 0;
    virtual bool __stdcall IsValid() const = 0;
};

class IRing : public IGeometry
{
public:
    virtual bool __stdcall AddPoint(const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON) = 0;
    virtual bool __stdcall InsertPoint(const dword index, const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON) = 0;
    virtual bool __stdcall DeletePoint(const dword index) = 0;
    virtual void __stdcall ClearPoint() = 0;
    virtual bool __stdcall SetPoint(const dword index, const WKSPointZ& point,
        const VertexType vertextype = VERTEXTYPE_COMMON) = 0;
    virtual bool __stdcall GetPoint(const dword index, WKSPointZ& point,
        VertexType& vertextype) const = 0;
    virtual bool __stdcall GetPoint1(const dword index, WKSPointZ& point) const = 0;
    virtual dword __stdcall GetPointCount() const = 0;
    virtual bool __stdcall GetArea(double& area) const = 0;
    virtual bool __stdcall GetPerimeter(double& perimeter) const = 0;
    virtual bool __stdcall IsPostiveDirection() const = 0;
    virtual bool __stdcall IsValid() const = 0;
};

class IPolyline : public IGeometry
{
public:
    virtual bool __stdcall AddPathRef(IPath* pPath) = 0;
    virtual bool __stdcall DeletePath(const dword index) = 0;
    virtual void __stdcall ClearPath() = 0;
    virtual bool __stdcall GetPathRef(IPath** ppPath, const dword index) const = 0;
    virtual bool __stdcall SetPathOrder(const dword oldorder, const dword neworder) = 0;
    virtual dword __stdcall GetPathCount() const = 0;
    virtual bool __stdcall GetLength(double& length) const = 0;
    virtual bool __stdcall GetPositionByDistance(const double distance, WKSPoint& position) const = 0;
    virtual bool __stdcall IsValid() const = 0;
};

class IPolygon : public IGeometry
{
public:
    virtual bool __stdcall AddRingRef(IRing* pRing) = 0;
    virtual bool __stdcall DeleteRing(const dword index) = 0;
    virtual void __stdcall ClearRing() = 0;
    virtual bool __stdcall GetRingRef(IRing** ppRing, const dword index) const = 0;
    virtual bool __stdcall SetRingOrder(const dword oldorder, const dword neworder) = 0;
    virtual dword __stdcall GetRingCount() const = 0;
    virtual bool __stdcall GetArea(double& area) const = 0;
    virtual bool __stdcall GetPerimeter(double& perimeter) const = 0;
    virtual bool __stdcall IsValid() const = 0;
};

class IEnvelope : public IGeometry
{
public:
    virtual void __stdcall SetMinX(const double minx) = 0;
    virtual void __stdcall SetMinY(const double miny) = 0;
    virtual void __stdcall SetMaxX(const double maxx) = 0;
    virtual void __stdcall SetMaxY(const double maxy) = 0;
    virtual void __stdcall GetMinX(double& minx) const = 0;
    virtual void __stdcall GetMinY(double& miny) const = 0;
    virtual void __stdcall GetMaxX(double& maxx) const = 0;
    virtual void __stdcall GetMaxY(double& maxy) const = 0;
    virtual void __stdcall GetArea(double& area) const = 0;
    virtual void __stdcall GetPerimeter(double& perimeter) const = 0;
};

class ICircle : public IGeometry
{
public:
    virtual void __stdcall SetCenter(const WKSPointZ& center) = 0;
    virtual void __stdcall GetCenter(WKSPointZ& center) const = 0;
    virtual void __stdcall SetRadius(const double radius) = 0;
    virtual void __stdcall GetRadius(double& radius) const = 0;
    virtual void __stdcall GetArea(double& area) const = 0;
    virtual void __stdcall GetPerimeter(double& perimeter) const = 0;
};

class IEllipse : public IGeometry
{
public:
    virtual void __stdcall SetMinX(const double minx) = 0;
    virtual void __stdcall SetMinY(const double miny) = 0;
    virtual void __stdcall SetMaxX(const double maxx) = 0;
    virtual void __stdcall SetMaxY(const double maxy) = 0;
    virtual void __stdcall GetMinX(double& minx) const = 0;
    virtual void __stdcall GetMinY(double& miny) const = 0;
    virtual void __stdcall GetMaxX(double& maxx) const = 0;
    virtual void __stdcall GetMaxY(double& maxy) const = 0;
    virtual void __stdcall GetArea(double& area) const = 0;
    virtual void __stdcall GetPerimeter(double& perimeter) const = 0;
};

}

#endif