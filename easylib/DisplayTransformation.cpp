#include "CommonInclude.h"
#include "DisplayTransformation.h"
#include "Geometry.h"
#include "MathLib.h"

namespace easymap
{

using namespace mathlib;

CLASS_FACTORY_INSTANCE(CDisplayTransformation)

const double PLANEROTATE_FACTOR1 = 28;
const double PLANEROTATE_FACTOR2 = 32;

CDisplayTransformation::CDisplayTransformation()
{
    INIT_REFCOUNT

    m_pVisiExtChg = NULL;

    m_InnerScale        = 1;
    m_MapCenter.x       = 0;
    m_MapCenter.y       = 0;
    m_DeviceRect.left   = 0;
    m_DeviceRect.right  = 800;
    m_DeviceRect.top    = 0;
    m_DeviceRect.bottom = 600;
    m_MapUnit           = UNIT_M;
    m_LogPixelX         = 96;
    m_LogPixelY         = 96;

    //缺省不设置参考比例尺，即在绘制的时候不随图放缩
    m_ReferenceScale    = 0;

    m_pPlot3D = new CPlot3D;
    m_pPlot3D->SetViewAzimuth(0);
    m_pPlot3D->SetCameraAzimuth(-180);
    m_pPlot3D->SetViewElevation(-90);
    m_pPlot3D->SetCameraElevation(90);
    m_pPlot3D->SetViewRoll(0);

    m_RotateCenter = m_MapCenter;

    m_SR = "Unknown Coordinate System?";
}

CDisplayTransformation::~CDisplayTransformation()
{
}

void CDisplayTransformation::finishplanerotate(const double& degree)
{
    //--------------------------------------------------------------------
    //记下世界中心旋转前，最终旋转中心的位置参考
    double attitude;
    this->GetAttitude(attitude);
    tagRECT rect;
    this->GetDeviceRect(rect);
    double degreefactor = PLANEROTATE_FACTOR1
        + PLANEROTATE_FACTOR2*(double(rect.bottom - rect.top) / double(rect.right - rect.left));
    double tmp = (degreefactor - attitude) / degreefactor;
    long rotatewnd_y = long((rect.bottom + rect.top) / 2 * tmp);
    WKSPoint centerpnt, oldcenterpnt = m_MapCenter;
//    this->Device2MapXY((rect.right + rect.left) / 2, rotatewnd_y, centerpnt.x, centerpnt.y);
    this->_device2map_rotated2d((rect.right + rect.left) / 2, rotatewnd_y, centerpnt);
    oldcenterpnt.x = -centerpnt.x + oldcenterpnt.x;
    oldcenterpnt.y = -centerpnt.y + oldcenterpnt.y;
    //--------------------------------------------------------------------

    m_RotateCenter = oldcenterpnt;

    //--------------------------------------------------------------------
    //旋转原世界中心
    m_pPlot3D->SetViewAzimuth(degree);
    m_pPlot3D->SetCameraAzimuth(degree - 180);
    //--------------------------------------------------------------------

    //--------------------------------------------------------------------
    //原世界中心已经旋转，修改世界中心的位置
    //目的是将最终旋转中心移动到旋转前的位置
//    this->Device2MapXY((rect.right + rect.left) / 2, rotatewnd_y, centerpnt.x, centerpnt.y);
      this->_device2map_rotated2d((rect.right + rect.left) / 2, rotatewnd_y, centerpnt);
    centerpnt.x = centerpnt.x + oldcenterpnt.x;
    centerpnt.y = centerpnt.y + oldcenterpnt.y;
    m_MapCenter = centerpnt;
    //--------------------------------------------------------------------

    //这是实际的旋转中心
    m_RotateCenter.x += centerpnt.x;
    m_RotateCenter.y += centerpnt.y;
}

void CDisplayTransformation::finishtranslation(bool callback)
{
    double planerotate;
    this->GetPlaneRotate(planerotate);
    this->finishplanerotate(planerotate);

    //起辅助作用的
    m_VisibleExtent.left = this->_device2map_x(m_DeviceRect.left);
    m_VisibleExtent.right = this->_device2map_x(m_DeviceRect.right);
    m_VisibleExtent.bottom = this->_device2map_y(m_DeviceRect.bottom);
    m_VisibleExtent.top = this->_device2map_y(m_DeviceRect.top);

    double width = m_VisibleExtent.right - m_VisibleExtent.left;
    double height = m_VisibleExtent.top - m_VisibleExtent.bottom;
    width = (width > height ? width : height) / 2;
    m_ExtentExt.left = m_MapCenter.x - width;
    m_ExtentExt.bottom = m_MapCenter.y - width;
    m_ExtentExt.right = m_MapCenter.x + width;
    m_ExtentExt.top = m_MapCenter.y + width;
    ExtentViewExtentSlight(m_ExtentExt);

    if (callback && m_pVisiExtChg)
    {
        WKSRect viewext;
        this->GetVisibleExtent(viewext);
        (*m_pVisiExtChg)(viewext);
    }
}

bool __stdcall CDisplayTransformation::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IDisplayTransformation"))
        || (0 == strcmp(interfacename, "CDisplayTransformation")))
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

dword __stdcall CDisplayTransformation::_SaveInstance(IStreamX* pStream,
    void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_InnerScale);
    ps->Write(&m_MapCenter, sizeof(WKSPoint));
    ps->Write(&m_DeviceRect, sizeof(RECT));
    ps->Write(&m_MapUnit, sizeof(MapUnits));
    ps->Write(m_LogPixelX);
    ps->Write(m_LogPixelY);
    ps->Write(m_ReferenceScale);
    ps->Write(m_SR);

    m_pPlot3D->_DumpTo(pStream, assist);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CDisplayTransformation::_LoadInstance(IStreamX* pStream,
    void* const assist)
{
    m_pVisiExtChg = NULL;

    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Read(m_InnerScale);
    ps->Read(&m_MapCenter, sizeof(WKSPoint));
    ps->Read(&m_DeviceRect, sizeof(RECT));
    ps->Read(&m_MapUnit, sizeof(MapUnits));
    ps->Read(m_LogPixelX);
    ps->Read(m_LogPixelY);
    ps->Read(m_ReferenceScale);
    ps->Read(m_SR);

    CPersistPtr pPersist;
    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pPlot3D, CPlot3D)

    return pStream->GetPos() - oldpos;

    this->finishtranslation();
}

void CDisplayTransformation::GetPlot3D(CPlot3DPtr& pPlot3D) const
{
    IObjPtr pObj;
    CLONE_PTR(m_pPlot3D, pObj)
    CAST_PTR(pObj, pPlot3D, CPlot3D)
}

void __stdcall CDisplayTransformation::SetPlaneRotate(const double degree)
{
    this->finishplanerotate(degree);
    this->finishtranslation();
}

void __stdcall CDisplayTransformation::GetPlaneRotate(double& degree) const
{
    m_pPlot3D->GetViewAzimuth(degree);
}

void __stdcall CDisplayTransformation::SetAttitude(const double degree)
{
    if ((degree < 0) || (degree > 89))
    {
        return;
    }

    WKSPoint rotatecenter;
    this->GetRotateCenter(rotatecenter);

    double d = 90 - degree;
    double t = ((long)degree % 90) / 8.0;
    m_pPlot3D->SetViewElevation(-d + t/1.5);
    m_pPlot3D->SetCameraElevation(d - t);

    this->finishtranslation(false);

    this->SetRotateCenter(rotatecenter);
}

void __stdcall CDisplayTransformation::GetAttitude(double& degree) const
{
    m_pPlot3D->GetCameraElevation(degree);
    degree = 90 - degree;
}

void __stdcall CDisplayTransformation::SetRotateCenter(const WKSPoint& rotatecenter)
{
    m_MapCenter = rotatecenter;
    this->finishtranslation(false);

    WKSPoint delta;
    delta.x = m_MapCenter.x - m_RotateCenter.x;
    delta.y = m_MapCenter.y - m_RotateCenter.y;

    m_MapCenter.x += delta.x;
    m_MapCenter.y += delta.y;
    this->finishtranslation();
}

void __stdcall CDisplayTransformation::GetRotateCenter(WKSPoint& rotatecenter) const
{
    rotatecenter = m_RotateCenter;
}

void __stdcall CDisplayTransformation::SetMapUnit(const MapUnits mapunit)
{
    m_MapUnit = mapunit;
}

MapUnits __stdcall CDisplayTransformation::GetMapUnit() const
{
    return m_MapUnit;
}

void __stdcall CDisplayTransformation::SetMapScale(const double mapscale)
{
    WKSPoint rotatecenter;
    this->GetRotateCenter(rotatecenter);
    m_InnerScale = m_LogPixelX * GetInchQuotiety(m_MapUnit) / mapscale;
    this->finishtranslation(false);
    this->SetRotateCenter(rotatecenter);
}

void __stdcall CDisplayTransformation::GetMapScale(double& mapscale) const
{
    mapscale = 1 / (m_InnerScale / m_LogPixelX / GetInchQuotiety(m_MapUnit));
}

void __stdcall CDisplayTransformation::SetMapCenter(const WKSPoint& center)
{
    m_MapCenter = center;
    this->finishtranslation();
}

void __stdcall CDisplayTransformation::GetMapCenter(WKSPoint& center) const
{
    center = m_MapCenter;
}

void __stdcall CDisplayTransformation::SetVisibleExtent(const WKSRect& extent)
{
    //重新计算坐标原点以及当前比例尺
    m_MapCenter.x = (extent.right + extent.left)/2;
    m_MapCenter.y = (extent.top + extent.bottom)/2;

    long viewheight, viewwidth;
    viewheight = abs(m_DeviceRect.bottom - m_DeviceRect.top);
    viewwidth = abs(m_DeviceRect.right - m_DeviceRect.left);

    if (0.000000001 > abs(viewwidth))
    {
        viewwidth = 1;
    }

    double extheight = fabs(extent.top - extent.bottom);
    double extwidth = fabs(extent.right - extent.left);
    if (0.000000001 > extwidth)
    {
        extwidth = 0.000000001;
    }

    if ((extheight / extwidth) < double(viewheight) / double(viewwidth))
    {
        if (0.000000001 > extwidth)
        {
            extwidth = 0.000000001;
        }
        m_InnerScale = viewwidth / extwidth;
    }
    else
    {
        if (0.000000001 > extheight)
        {
            extheight = 0.000000001;
        }
        m_InnerScale = viewheight / extheight;
    }

    this->finishtranslation();
}

void __stdcall CDisplayTransformation::GetVisibleExtent(WKSRect& extent) const
{
    extent = m_VisibleExtent;
}

void __stdcall CDisplayTransformation::SetDeviceRect(const RECT& rect)
{
    RECT RectBak = m_DeviceRect;
    m_DeviceRect = rect;

    WKSRect ext;
    ext.left = this->_device2map_x(m_DeviceRect.left);
    ext.right = this->_device2map_x(m_DeviceRect.right);
    ext.bottom = this->_device2map_y(m_DeviceRect.bottom);
    ext.top = this->_device2map_y(m_DeviceRect.top);
    if (0.000000001 > fabs(ext.right - ext.left))
    {
        m_DeviceRect = RectBak;
        return;
    }

    m_InnerScale = fabs((m_DeviceRect.right - m_DeviceRect.left)
        / (ext.right - ext.left));

    m_pPlot3D->SetScreenBound(m_DeviceRect.right - m_DeviceRect.left,
        m_DeviceRect.bottom - m_DeviceRect.top);

    double width = m_DeviceRect.right - m_DeviceRect.left;
    double height = m_DeviceRect.bottom - m_DeviceRect.top;
    width = width > height ? width : height;
    m_pPlot3D->SetCameraDistance(width);
    m_pPlot3D->SetProjPlaneDistance(width);

    this->finishtranslation();
}

void __stdcall CDisplayTransformation::GetDeviceRect(RECT& rect) const
{
    rect = m_DeviceRect;
}

void __stdcall CDisplayTransformation::SetLogPixel(const long logpixelx,
    const long logpixely)
{
    if ((0 >= logpixelx) || (0 >= logpixely))
    {
        return;
    }

    m_LogPixelX = logpixelx;
    m_LogPixelY = logpixely;

    this->finishtranslation();
}

void __stdcall CDisplayTransformation::GetLogPixel(long& logpixelx,
    long& logpixely) const
{
    logpixelx = m_LogPixelX;
    logpixely = m_LogPixelY;
}

bool __stdcall CDisplayTransformation::PointInPlane(const double x, const double y) const
{
    return PointInEnvelope(x, y, m_ExtentExt);
}



inline void _MovePointInEnvelope(WKSPoint& pnt, const WKSRect& envelope)
{
    pnt.x = (pnt.x > envelope.left) ? pnt.x : envelope.left;
    pnt.x = (pnt.x < envelope.right) ? pnt.x : envelope.right;
    pnt.y = (pnt.y < envelope.top) ? pnt.y : envelope.top;
    pnt.y = (pnt.y > envelope.bottom) ? pnt.y : envelope.bottom;
}

struct _seg_clipline
{
    WKSPointZ from;
    WKSPointZ to;
};

void CDisplayTransformation::ClipPath(const IPathPtr pPath, IGeometryPtr& pClipped) const
{
    pClipped = NULL;

    dword pointcount = pPath->GetPointCount();
    if (pointcount < 2)
        return;

    static vector<_seg_clipline> segments;
    static _seg_clipline line;
    long segment_i = 0;

    dword i;
    for (i = 1; i < pointcount; i++)
    {
        pPath->GetPoint1(i-1, line.from);
        pPath->GetPoint1(i, line.to);
        if (0 < EnvelopeClipLine(m_ExtentExt, line.from.x, line.from.y, line.to.x, line.to.y))
        {
            if (segments.size() <= segment_i)
            {
                segments.resize(segment_i + 1);
            }
            segments[segment_i++] = line;
        }

    }

    pPath->ClearPoint();

    if (segment_i < 1)
    {
        return;
    }

    pPath->AddPoint(segments[0].from);
    for (i = 0; i < segment_i; i++)
    {
        pPath->AddPoint(segments[i].to);
    }

    IPolylinePtr Polyline = new CPolyline;
    Polyline->AddPathRef(pPath._p());
    CAST_PTR(Polyline, pClipped, IGeometry);
}

/*
void CDisplayTransformation::ClipPath(const IPathPtr pPath, IGeometryPtr& pClipped) const
{
    pClipped = NULL;

    dword pointcount = pPath->GetPointCount();
    if (pointcount < 2)
        return;

    IPolylinePtr pClippedPolyline = new CPolyline;
    CAST_PTR(pClippedPolyline, pClipped, IGeometry);

    WKSPointZ pointz;
    pPath->GetPoint1(0, pointz);
    bool inner = PointInEnvelope(pointz.x, pointz.y, m_ExtentExt);

    IPathPtr pSubPath = new CPath;
    if (inner)
    {
        pSubPath->AddPoint(pointz);
    }

    bool addlast = true;
    for (dword i = 1; i < pointcount; i++)
    {
        addlast = true;
        _seg_clipline line;
        pPath->GetPoint1(i-1, line.from);
        pPath->GetPoint1(i, line.to);
        long flag = EnvelopeClipLine(m_ExtentExt, line.from.x, line.from.y, line.to.x, line.to.y);
        if ((flag == 1) || (flag == 2))
        {
            if (inner)
            {
                //从里面出去
                pSubPath->AddPoint(line.to);
                IObjPtr pObj1;
                pSubPath->Clone(pObj1._ref());
                IPathPtr pPath1;
                CAST_PTR(pObj1, pPath1, IPath)
                pClippedPolyline->AddPathRef(pPath1._p());
                pSubPath->ClearPoint();
                addlast = false;
            }
            else
            {
                //从外面进来
                pSubPath->AddPoint(line.from);
                pSubPath->AddPoint(line.to);
            }

            inner = !inner;
        }
        else if (flag == 3)
        {
            //两个交点
            IPathPtr pPath1 = new CPath;
            pPath1->AddPoint(line.from);
            pPath1->AddPoint(line.to);
            pClippedPolyline->AddPathRef(pPath1._p());
            addlast = false;
        }
        else if (inner)
        {
            //在里面转
            pSubPath->AddPoint(line.to);
        }
    }

    if (addlast)
    {
        pClippedPolyline->AddPathRef(pSubPath._p());
    }
}
*/

void CDisplayTransformation::ClipRing(const IRingPtr pRing, IGeometryPtr& pClipped) const
{
    pClipped = NULL;

    dword pointcount = pRing->GetPointCount();
    if (pointcount < 3)
        return;

    WKSPointZ point;
    for (dword i = 0; i < pointcount; i++)
    {
        pRing->GetPoint1(i, point);
        _MovePointInEnvelope(point, m_ExtentExt);
        pRing->SetPoint(i, point);
    }

    WKSPointZ point1;
    pRing->GetPoint1(0, point1);
    if (point.x != point1.x || point.y != point1.y)
    {
        pRing->AddPoint(point1);
    }

    CAST_PTR(pRing, pClipped, IGeometry)
}

void CDisplayTransformation::ClipPolyline(const IPolylinePtr pPolyline, IGeometryPtr& pClipped) const
{
    IPolylinePtr pClippedPolyline = new CPolyline;
    dword i, pathcount = pPolyline->GetPathCount();
    for (i = 0; i < pathcount; i++)
    {
        IPathPtr pPath;
        pPolyline->GetPathRef(pPath._ref(), i);
        IGeometryPtr pClippedGeometry;
        this->ClipPath(pPath, pClippedGeometry);
        if (pClippedGeometry.Assigned())
        {
            IPolylinePtr pSubPolyline;
            CAST_PTR(pClippedGeometry, pSubPolyline, IPolyline)
            dword subpathcount = pSubPolyline->GetPathCount();
            for (dword j = 0; j < subpathcount; j++)
            {
                IPathPtr pPath;
                pSubPolyline->GetPathRef(pPath._ref(), j);
                pClippedPolyline->AddPathRef(pPath._p());
            }
        }
    }

    CAST_PTR(pClippedPolyline, pClipped, IGeometry)
}

void CDisplayTransformation::ClipPolygon(const IPolygonPtr pPolygon, IGeometryPtr& pClipped) const
{
    dword ringcount = pPolygon->GetRingCount();
    for (dword i = 0; i < ringcount; i++)
    {
        IRingPtr pRing;
        pPolygon->GetRingRef(pRing._ref(), i);
        IGeometryPtr pClippedGeometry;
        this->ClipRing(pRing, pClippedGeometry);
    }

    CAST_PTR(pPolygon, pClipped, IGeometry)
}

void CDisplayTransformation::ClipMultiPoint(const IMultiPointPtr pMultiPoint, IGeometryPtr& pClipped) const
{
    IMultiPointPtr pClippedMultiPoint = new CMultiPoint;
    dword pointcount = pMultiPoint->GetPointCount();
    for (dword i = 0; i < pointcount; i++)
    {
        WKSPointZ pointz;
        pMultiPoint->GetPoint(pointz, i);
        if (PointInEnvelope(pointz.x, pointz.y, m_ExtentExt))
        {
            pClippedMultiPoint->AddPoint(pointz);
        }
    }

    if (pClippedMultiPoint->GetPointCount() > 0)
    {
        CAST_PTR(pClippedMultiPoint, pClipped, IGeometry)
    }
    else
    {
        pClipped = NULL;
    }
}

bool CDisplayTransformation::_cliprect(WKSRect& rect) const
{
    if (EnvelopesSeparated(rect, m_VisibleExtent))
    {
        //都在外面
        return false;
    }

    if (PointInEnvelope(rect.left, rect.bottom, m_VisibleExtent)
        && PointInEnvelope(rect.right, rect.top, m_VisibleExtent))
    {
        //都在里面
        return true;
    }

    if (rect.left < m_VisibleExtent.left)
    {
        rect.left = m_VisibleExtent.left;
    }

    if (rect.bottom < m_VisibleExtent.bottom)
    {
        rect.bottom = m_VisibleExtent.bottom;
    }

    if (rect.right > m_VisibleExtent.right)
    {
        rect.right = m_VisibleExtent.right;
    }

    if (rect.top > m_VisibleExtent.top)
    {
        rect.top = m_VisibleExtent.top;
    }

    return true;
}

void __stdcall CDisplayTransformation::ClipGeometry(const IGeometry* pGeometry,
    IGeometry** ppClipped) const
{
    if (_invalid(pGeometry) || _invalid(ppClipped))
        return;
    assert(!*ppClipped);
    *ppClipped = NULL;

    IGeometryPtr pOriginal = (IGeometry*)pGeometry;
    IGeometryPtr pClipped;
    double x, y;

    switch (pOriginal->GetGeometryType())
    {
    case GEOMETRYTYPE_POLYLINE:
        {
            IPolylinePtr pPolyline;
            CAST_PTR(pOriginal, pPolyline, IPolyline)
            this->ClipPolyline(pPolyline, pClipped);
        }
        break;

    case GEOMETRYTYPE_POLYGON:
        {
            IPolygonPtr pPolygon;
            CAST_PTR(pOriginal, pPolygon, IPolygon)
            this->ClipPolygon(pPolygon, pClipped);
        }
        break;

    case GEOMETRYTYPE_PATH:
        {
            IPathPtr pPath;
            CAST_PTR(pOriginal, pPath, IPath)
            this->ClipPath(pPath, pClipped);
        }
        break;

    case GEOMETRYTYPE_RING:
        {
            IRingPtr pRing;
            CAST_PTR(pOriginal, pRing, IRing)
            this->ClipRing(pRing, pClipped);
        }
        break;

    case GEOMETRYTYPE_MULTIPOINT:
        {
            IMultiPointPtr pMultiPoint;
            CAST_PTR(pOriginal, pMultiPoint, IMultiPoint)
            this->ClipMultiPoint(pMultiPoint, pClipped);
        }
        break;

    case GEOMETRYTYPE_POINT:
        {
            IPointPtr pPoint;
            CAST_PTR(pOriginal, pPoint, IPoint)
            pPoint->GetX(x);
            pPoint->GetY(y);
            if (PointInEnvelope(x, y, m_ExtentExt))
            {
                pClipped = pOriginal;
            }
        }
        break;

    case GEOMETRYTYPE_ENVELOPE:
        {
            IEnvelopePtr pEnvelope;
            CAST_PTR(pOriginal, pEnvelope, IEnvelope)
            WKSRect envelope;
            pEnvelope->GetMBR(envelope);
            if (this->_cliprect(envelope))
            {
                pEnvelope->SetMinX(envelope.left);
                pEnvelope->SetMinY(envelope.bottom);
                pEnvelope->SetMaxX(envelope.right);
                pEnvelope->SetMaxY(envelope.top);
                CAST_PTR(pEnvelope, pClipped, IGeometry)
            }
        }
        break;

    default:
        {
            IObjPtr pObjClone;
            CLONE_PTR(pOriginal, pObjClone);
            CAST_PTR(pObjClone, pClipped, IGeometry)
        }
    }

    if (pClipped.Assigned())
    {
        *ppClipped = pClipped._p();
        (*ppClipped)->_AddRef();
    }
}


//  用于计算旋转以前的坐标
//---------------------------------------------------------------------------
inline double CDisplayTransformation::_device2map_x(const double x_dev) const
{
    return (x_dev + m_InnerScale*m_MapCenter.x
        - ((m_DeviceRect.right + m_DeviceRect.left)/2)) / m_InnerScale;
}

inline double CDisplayTransformation::_device2map_y(const double y_dev) const
{
    return (-y_dev + m_InnerScale*m_MapCenter.y
        + ((m_DeviceRect.bottom + m_DeviceRect.top)/2)) / m_InnerScale;
}

inline double CDisplayTransformation::_map2device_x(const double x_map) const
{
    return (m_InnerScale*x_map - m_InnerScale*m_MapCenter.x
        + (m_DeviceRect.right + m_DeviceRect.left)/2);
}

inline double CDisplayTransformation::_map2device_y(const double y_map) const
{
    return  (m_InnerScale*(-y_map) + m_InnerScale*m_MapCenter.y
        + (m_DeviceRect.bottom + m_DeviceRect.top)/2);
}
//---------------------------------------------------------------------------

void CDisplayTransformation::_device2map_rotated2d(const double dev_x,
    const double dev_y, WKSPoint& point_map) const
{
    point_map.x = this->_device2map_x(dev_x);
    point_map.y = this->_device2map_y(dev_y);

    double planerotate;
    this->GetPlaneRotate(planerotate);
    if (fabs(planerotate) > 0.0001)
    {
        planerotate = -planerotate;
        mathlib::RotateDegree(point_map, m_MapCenter, planerotate);
    }
}

//以这两个函数为准
//---------------------------------------------------------------------------
void __stdcall CDisplayTransformation::Device2MapXY(const long x_dev,
    const long y_dev, double& x_map, double& y_map) const
{
    double rotate = 0.0;
    this->GetAttitude(rotate);
    if (rotate < 0.0001)
    {
        WKSPoint point_map;
        this->_device2map_rotated2d(x_dev, y_dev, point_map);
        x_map = point_map.x;
        y_map = point_map.y;
    }
    else
    {
        //没搞定俯仰
        WKSPoint point_map;
        this->_device2map_rotated2d(x_dev, y_dev, point_map);
        x_map = point_map.x;
        y_map = point_map.y;
    }
}

void __stdcall CDisplayTransformation::Map2DeviceXY(const double x_map,
    const double y_map, long& x_dev, long& y_dev) const
{
    WKSPoint point_map(x_map, y_map);

    double rotate = 0.0;
    this->GetAttitude(rotate);
    if (rotate < 0.0001)
    {
        this->GetPlaneRotate(rotate);
        if (fabs(rotate) > 0.0001)
        {
            mathlib::RotateDegree(point_map, m_MapCenter, rotate);
        }

        x_dev = (long)this->_map2device_x(point_map.x);
        y_dev = (long)this->_map2device_y(point_map.y);
    }
    else
    {
        double pos_x = this->_map2device_x(point_map.x);
        double pos_y = this->_map2device_y(point_map.y);
        pos_x = pos_x - (m_DeviceRect.right - m_DeviceRect.left) / 2;
        pos_y = -pos_y + (m_DeviceRect.bottom - m_DeviceRect.top) / 2;
        m_pPlot3D->TransformPoint(pos_x, pos_y, 0, pos_x, pos_y);
        x_dev = (long)pos_x;
        y_dev = (long)pos_y;
    }
}
//---------------------------------------------------------------------------

void __stdcall CDisplayTransformation::Device2Map(const POINT& point_dev,
    WKSPoint& point_map) const
{
    this->Device2MapXY(point_dev.x, point_dev.y, point_map.x, point_map.y);
}

void __stdcall CDisplayTransformation::Map2Device(const WKSPoint& point_map,
    POINT& point_dev) const
{
    this->Map2DeviceXY(point_map.x, point_map.y, point_dev.x, point_dev.y);
}

void __stdcall CDisplayTransformation::Device2Map_Envelope(const tagRECT& envelope_dev,
    WKSRect& envelope_map) const
{
    WKSPoint pnt1, pnt2, pnt3, pnt4;
    this->Device2MapXY(envelope_dev.left, envelope_dev.bottom, pnt1.x, pnt1.y);
    this->Device2MapXY(envelope_dev.right, envelope_dev.bottom, pnt2.x, pnt2.y);
    this->Device2MapXY(envelope_dev.right, envelope_dev.top, pnt3.x, pnt3.y);
    this->Device2MapXY(envelope_dev.left, envelope_dev.top, pnt4.x, pnt4.y);
    envelope_map.left = pnt1.x;
    envelope_map.bottom = pnt1.y;
    envelope_map.right = pnt1.x;
    envelope_map.top = pnt1.y;

    envelope_map.left = min(envelope_map.left, pnt2.x);
    envelope_map.bottom = min(envelope_map.bottom, pnt2.y);
    envelope_map.right = max(envelope_map.right, pnt2.x);
    envelope_map.top = max(envelope_map.top, pnt2.y);

    envelope_map.left = min(envelope_map.left, pnt3.x);
    envelope_map.bottom = min(envelope_map.bottom, pnt3.y);
    envelope_map.right = max(envelope_map.right, pnt3.x);
    envelope_map.top = max(envelope_map.top, pnt3.y);

    envelope_map.left = min(envelope_map.left, pnt4.x);
    envelope_map.bottom = min(envelope_map.bottom, pnt4.y);
    envelope_map.right = max(envelope_map.right, pnt4.x);
    envelope_map.top = max(envelope_map.top, pnt4.y);
}

//  内部使用，用于误差小于1像素的XX
//---------------------------------------------------------------------------------
void CDisplayTransformation::Device2Map(const WKSPoint& point_dev,
    WKSPoint& point_map) const
{
    double rotate = 0.0;
    this->GetAttitude(rotate);
    if (rotate < 0.0001)
    {
        this->_device2map_rotated2d(point_dev.x, point_dev.y, point_map);
    }
    else
    {
        //没搞定，只考虑2维
        this->_device2map_rotated2d(point_dev.x, point_dev.y, point_map);
    }
}

void CDisplayTransformation::Map2Device(const WKSPoint& point_map,
    WKSPoint& point_dev) const
{
    point_dev = point_map;

    double rotate = 0.0001;
    this->GetAttitude(rotate);
    if (rotate < 0.0001)
    {
        this->GetPlaneRotate(rotate);
        if (fabs(rotate) > 0.0001)
        {
            point_dev = point_map;
            mathlib::RotateDegree(point_dev, m_MapCenter, rotate);
        }

        point_dev.x = this->_map2device_x(point_dev.x);
        point_dev.y = this->_map2device_y(point_dev.y);
    }
    else
    {
        point_dev.x = this->_map2device_x(point_dev.x);
        point_dev.y = this->_map2device_y(point_dev.y);

        point_dev.x = point_dev.x - (m_DeviceRect.right - m_DeviceRect.left) / 2;
        point_dev.y = -point_dev.y + (m_DeviceRect.bottom - m_DeviceRect.top) / 2;
        m_pPlot3D->TransformPoint(point_dev.x, point_dev.y, 0, point_dev.x, point_dev.y);
    }
}
//---------------------------------------------------------------------------------

void __stdcall CDisplayTransformation::SetReferenceScale(const double refscale)
{
    m_ReferenceScale = refscale;
}

void __stdcall CDisplayTransformation::GetReferenceScale(double& refscale) const
{
    refscale = m_ReferenceScale;
}

bool __stdcall CDisplayTransformation::SetSpatialReference(const string& sr)
{
    m_SR = sr;
    return true;
}

bool __stdcall CDisplayTransformation::GetSpatialReference(string& sr) const
{
    sr = m_SR;
    return true;
}

bool __stdcall CDisplayTransformation::SetVisibleExtentHandle(DisplayVisiExtChg* const pHandle)
{
    m_pVisiExtChg = pHandle;
    return true;
}

bool __stdcall CDisplayTransformation::Clone(IObj** ppObj) const
{
    return false;
}

double CDisplayTransformation::GetMeterQuotiety(const MapUnits unit)
{
    switch (unit)
    {
    case UNIT_M:
        return 1;
    case UNIT_KM:
        return 1000;
    case UNIT_NAUTICALMILE:
        return 1852;
    case UNIT_MILE:
        return 1609.344;
    case UNIT_INCH:
        return 0.0254;
    case UNIT_FOOT:
        return 0.3048;
    case UNIT_MM:
        return 0.001;
    case UNIT_CM:
        return 0.01;
    case UNIT_DEGREE:
        return 111320.69507081912;//嘿嘿嘿嘿嘿嘿嘿嘿
    default:
        return 0;
    }
}

double CDisplayTransformation::GetInchQuotiety(const MapUnits unit)
{
    switch (unit)
    {
    case UNIT_M:
        return 39.37007874015748031496062992126;
    case UNIT_KM:
        return 39370.07874015748031496062992126;
    case UNIT_NAUTICALMILE:
        return 72913.385826771653543307086614173;
    case UNIT_MILE:
        return 63360;
    case UNIT_INCH:
        return 1;
    case UNIT_FOOT:
        return 12;
    case UNIT_MM:
        return 0.03937007874015748031496062992126;
    case UNIT_CM:
        return 0.3937007874015748031496062992126;
    case UNIT_DEGREE:
        return 4382704.5303472094488188976377953;
    default:
        return 0;
    }
}

bool POINTinRECT(const long& x, const long& y, const tagRECT& rect)
{
    if ((x > rect.left)
        && (x < rect.right)
        && (y > rect.top)
        && (y < rect.bottom))
    {
        return true;
    }

    return false;
}

void ExtentViewExtentSlight(WKSRect& viewextent)
{
    //扩大一些
    double width = viewextent.right- viewextent.left;
    double height = viewextent.top - viewextent.bottom;
    double cnt;
    if (width > height)
    {
        cnt = (viewextent.top + viewextent.bottom) / 2;
        viewextent.top = cnt + width / 2;
        viewextent.bottom = cnt - width / 2;
    }
    else
    {
        cnt = (viewextent.left + viewextent.right) / 2;
        viewextent.left = cnt - height / 2;
        viewextent.right = cnt + height / 2;
    }

    width = width / 5;
    viewextent.left -= width;
    viewextent.bottom -= width;
    viewextent.right += width;
    viewextent.top += width;
}

}
