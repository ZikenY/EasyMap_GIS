#include "CommonInclude.h"
#include "DrawGeometry.h"
#include "MathLib.h"
#include "GeometryLabel.h"

namespace easymap
{

namespace drawgeometry
{

using namespace mathlib;

static const dword static_path_point_count = 10000;
static tagPOINT static_window_points_array[static_path_point_count];

//-----------   辅助函数    -------------
inline void DrawPoint(const WKSPointZ& point,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    long X, Y;
    pTrans->Map2DeviceXY(point.x, point.y, X, Y);
    ::Ellipse(dc, X - 1, Y - 1, X + 1, Y + 1);
}

void DoDrawLineStream(CStreamPtr pStream, const long pointcount,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSPointZ point;
    pStream->Read(&point, sizeof(WKSPointZ));
    VertexType vertextype;
    pStream->Read(&vertextype, sizeof(VertexType));

    long X, Y;
    pTrans->Map2DeviceXY(point.x, point.y, X, Y);
    ::MoveToEx(dc, X, Y, NULL);

    for (long i = 1; i < pointcount; i++)
    {
        pStream->Read(&point, sizeof(WKSPointZ));
        pStream->Read(&vertextype, sizeof(VertexType));
        pTrans->Map2DeviceXY(point.x, point.y, X, Y);
        ::LineTo(dc, X, Y);
    }
}

void DoDrawRingStream(CStreamPtr pStream,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    long pointcount;
    pStream->Read(pointcount);

    if (0 < pointcount)
    {
        //待会儿绘制边界线的时候要再次用这个pos
        dword savepos = pStream->GetPos();

        //填充
        static tagPOINT points[1000];
        tagPOINT *pPoints = points;
        if (1000 < pointcount)
        {
            pPoints = new tagPOINT[pointcount];
        }

        for (long i = 0; i < pointcount; i++)
        {
            WKSPointZ point;
            pStream->Read(&point, sizeof(WKSPointZ));
            VertexType vertextype;
            pStream->Read(&vertextype, sizeof(VertexType));
            pTrans->Map2DeviceXY(point.x, point.y, pPoints[i].x, pPoints[i].y);
        }

        ::Polygon(dc, pPoints, pointcount);

        if (1000 < pointcount)
        {
            delete[] pPoints;
        }

        //再搞一遍，绘制边界线
        pStream->MovePos(savepos, SOFROMBEGINNING);
        DoDrawLineStream(pStream, pointcount, pTrans, dc);
    }
}
//--------------------------------------

//********************************************************************************
//  第一组函数，传入Geometry
//********************************************************************************

bool DrawGeneralGeometry(const IGeometryPtr pGeometry,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    switch (pGeometry->GetGeometryType())
    {
    case GEOMETRYTYPE_POINT:
        return DrawPoint((CPoint*)pGeometry._p(), pTrans, dc);
    case GEOMETRYTYPE_MULTIPOINT:
        return DrawMultiPoint((CMultiPoint*)pGeometry._p(), pTrans, dc);
    case GEOMETRYTYPE_PATH:
        return DrawPath((CPath*)pGeometry._p(), pTrans, dc);
    case GEOMETRYTYPE_RING:
        return DrawRing((CRing*)pGeometry._p(), pTrans, dc);
    case GEOMETRYTYPE_POLYLINE:
        return DrawPolyline((CPolyline*)pGeometry._p(), pTrans, dc);
    case GEOMETRYTYPE_POLYGON:
        return DrawPolygon((CPolygon*)pGeometry._p(), pTrans, dc);
    case GEOMETRYTYPE_ENVELOPE:
        return DrawEnvelope((CEnvelope*)pGeometry._p(), pTrans, dc);
    case GEOMETRYTYPE_CIRCLE:
        return DrawCircle((CCircle*)pGeometry._p(), pTrans, dc);
    case GEOMETRYTYPE_ELLIPSE:
        return DrawEllipse((CEllipse*)pGeometry._p(), pTrans, dc);
    default:
        return false;
    }
}

bool DrawPoint(const CPointPtr pPoint,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSPointZ pnt;
    pPoint->GetCoordinates(pnt);
    DrawPoint(pnt, pTrans, dc);
    return true;
}

bool DrawMultiPoint(const CMultiPointPtr pMultiPoint,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    long geocount = pMultiPoint->GetPointCount();
    for (long i = 0; i < geocount; i++)
    {
        WKSPointZ point;
        pMultiPoint->GetPoint(point, i);
        DrawPoint(point, pTrans, dc);
    }
    return true;
}

//绘制一节（mark），不管是solid还是hollow，自动改写sectorprogress，需要进位就改写sectorindex
//只绘制一节，to没有绘制完就自动改写to到本次绘制的结束点
//如果to绘制完了就返回true，否则（修改了to）就返回false
bool draw_sector(const HDC dc, double quo, const ILineSimpleTemplatePtr pTemplate,
    long& sectorindex, long& sectorprogress, const WKSPoint& from, WKSPoint& to,
    callback_draw_template_sector* const drawmark)
{
    bool solid;
    pTemplate->GetFirstMark(solid);
    if ((0 != sectorindex) && ((sectorindex % 2 != 0)))
    {
        solid = !solid;
    }

    if ((from.x == to.x) && (from.y == to.y)) return true;

    double factor;
    pTemplate->GetFactor(factor);
    dword mark_l;
    double mark, dist, markpg;
    double progress_lefttodraw;
    long _drawpoints;

    //传入的等待绘制线段的总长度（毫米）
    dist = sqrt(from.dis2(to)) / quo;

    //mark_l为这个mark的长度（点个数）
    bool r = pTemplate->GetSector(sectorindex, mark_l);
    if (!r) return true; //出错，结束绘制

    //这个mark的长度（毫米）
    mark = factor * mark_l;

    //上次已经绘制过的mark长度（必须跳过，毫米）
    markpg = factor * sectorprogress;

    //这个mark内剩下的没有绘制的长度（毫米）
    progress_lefttodraw = mark - markpg;

    if (progress_lefttodraw > dist)
    {
        //可以在这个mark内搞定to，不用进位sectorindex，只用修改sectorprogress

        //这次绘制的点数
        _drawpoints = (long)(dist / factor);

        //绘制完这一次mark后，修改progress（mark单位）
        sectorprogress = (long)((markpg + dist) / factor);

        r = true;
    }
    else
    {
        //在这个mark内搞不定to，要进位sectorindex，并且还要把sectorprogress清为0
        //同时还要修改to

        //这次绘制的点数
        _drawpoints = mark_l - sectorprogress;

        dword markcount;
        pTemplate->GetCount(markcount);
        if (markcount - 1 > sectorindex)
        {
            //sectorindex进位
            sectorindex++;
        }
        else
        {
            //sectorindex归零
            sectorindex = 0;
        }

        //sectorprogress是肯定要归零的
        sectorprogress = 0;

        //修改to，
        double ratio = progress_lefttodraw / dist;
        WKSPoint tmp;
        tmp.x = ratio * (to.x - from.x) + from.x;
        tmp.y = ratio * (to.y - from.y) + from.y;
        if (fabs(tmp.x - from.x) > 0.001 || fabs(tmp.y - from.y) > 0.001)
        {
            to = tmp;
        }

        r = false;
    }

    //绘制。。。
    if (solid)
    {
        if (_valid(drawmark))
        {
            (*drawmark)(from, to, _drawpoints);
        }
        else
        {
            //普通制图线
            HPEN pen = (HPEN)::GetCurrentObject(dc, OBJ_PEN);
            LOGPEN logpen;
            ::GetObject((HGDIOBJ)pen, sizeof(LOGPEN), &logpen);
            double penwidth = logpen.lopnWidth.x;
            if (3 < penwidth)
            {
                logpen.lopnWidth.x = logpen.lopnWidth.y = 1;
                pen = ::CreatePenIndirect(&logpen);
                HPEN pensaved = (HPEN)::SelectObject(dc, pen);
                HBRUSH brush = ::CreateSolidBrush(logpen.lopnColor);
                HBRUSH brushsaved = (HBRUSH)::SelectObject(dc, brush);

                WKSPoint pnt1, pnt2, pnt3, pnt4;
                VerticalPointsToLine(from, to, penwidth/2, pnt1, pnt2);
                VerticalPointsToLine(to, from, penwidth/2, pnt3, pnt4);
                tagPOINT env[5];
                env[0].x = (long)pnt1.x;
                env[0].y = (long)pnt1.y;
                env[1].x = (long)pnt2.x;
                env[1].y = (long)pnt2.y;
                env[2].x = (long)pnt3.x;
                env[2].y = (long)pnt3.y;
                env[3].x = (long)pnt4.x;
                env[3].y = (long)pnt4.y;
                env[4].x = (long)pnt1.x;
                env[4].y = (long)pnt1.y;

                ::Polygon(dc, env, 5);

                ::DeleteObject(pen);
                ::DeleteObject(brush);
                ::SelectObject(dc, pensaved);
                ::SelectObject(dc, brushsaved);
            }
            else
            {
                ::MoveToEx(dc, (long)from.x, (long)from.y, NULL);
                ::LineTo(dc, (long)to.x, (long)to.y);
            }
        }
    }

    return r;
}

//用线模板绘制线段（path中的一段）
void draw_template_line(const HDC dc, double quo, const ILineSimpleTemplatePtr pTemplate,
    long& sectorindex, long& sectorprogress, const WKSPoint& from, const WKSPoint& to,
    callback_draw_template_sector* const drawmark)
{
    bool solid;
    pTemplate->GetFirstMark(solid);
    if ((0 != sectorindex) && ((sectorindex % 2 != 0)))
    {
        solid = !solid;
    }

    WKSPoint _from, _to;
    _from = from;
    _to = to;
    bool r = false;
    while (!r)
    {
        r = draw_sector(dc, quo, pTemplate, sectorindex, sectorprogress,
            _from, _to, drawmark);
        _from = _to;
        _to = to;
    }

    //线段起点绘制成圆点
    if (solid && _invalid(drawmark) && ((from.x != to.x) || (from.y != to.y)))
    {
        double penwidth = -1;
        LOGPEN logpen;
        HPEN pen, pensaved;
        HBRUSH brush, brushsaved;
        pen = (HPEN)::GetCurrentObject(dc, OBJ_PEN);
        ::GetObject((HGDIOBJ)pen, sizeof(LOGPEN), &logpen);
        penwidth = logpen.lopnWidth.x;
        if (3 < penwidth)
        {
            logpen.lopnWidth.x = logpen.lopnWidth.y = 1;
            pen = ::CreatePenIndirect(&logpen);
            pensaved = (HPEN)::SelectObject(dc, pen);
            brush = ::CreateSolidBrush(logpen.lopnColor);
            brushsaved = (HBRUSH)::SelectObject(dc, brush);

            double t = penwidth/2;
            ::Ellipse(dc, from.x-t, from.y-t, from.x+t, from.y+t);

            ::DeleteObject(pen);
            ::DeleteObject(brush);
            ::SelectObject(dc, pensaved);
            ::SelectObject(dc, brushsaved);
        }
    }
}

bool DrawPath(const CPathPtr pPath, const CDisplayTransformationPtr pTrans, HDC dc,
    const ILineSimpleTemplatePtr pTemplate, const double& distance)
{
    long pointcount = pPath->GetPointCount();
    if (2 > pointcount) return false;

    long i;
    WKSPointZ point;
    tagPOINT wndpnt, *pPointsWnd;
    if (static_path_point_count < pointcount)
    {
        pPointsWnd = new tagPOINT[pointcount];
    }
    else
    {
        pPointsWnd = static_window_points_array;
    }

    //是否需要使用线模板
    bool simple = true;
    if (pTemplate.Assigned())
    {
        dword markcount;
        pTemplate->GetCount(markcount);
        if (0 < markcount)
        {
            simple = false;
        }
    }

    long sectorindex = 0;
    long sectorprogress = 0;

    //计算象素->毫米对应关系
    long logpixelx, logpixely;
    pTrans->GetLogPixel(logpixelx, logpixely);
    double quotiety_x = logpixelx
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double quotiety_y = logpixely
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double quotiety = (quotiety_x + quotiety_y) / 2;

    double quo, refscale;
    pTrans->GetReferenceScale(refscale);
    if (refscale > 0)
    {
        //使用参考比例尺，屏幕图元尺寸随着显示比例尺变化而变化
        double mapscale;
        pTrans->GetMapScale(mapscale);
        double scalequot = refscale / mapscale;
        quo = quotiety * scalequot;
    }
    else
    {
        quo = quotiety;
    }

    if (0.0000001 < fabs(distance))
    {
        //绘制平行线
        double dist = distance*quo;

        geo_path path;
        for (i = 0; i < pointcount; i++)
        {
            pPath->GetPoint1(i, point);
            tagPOINT newpnt;
            pTrans->Map2Device(point, newpnt);
            WKSPoint geopoint(newpnt.x, newpnt.y);
            path.push_back(geopoint);
        }

        geo_path parallel;
        path.parallel(parallel, dist);
        parallel.neat();

        for (i = 0; i < parallel.size(); i++)
        {
            pPointsWnd[i].x = Round(parallel[i].x);
            pPointsWnd[i].y = Round(parallel[i].y);
        }

        if (simple)
        {
            ::Polyline(dc, pPointsWnd, parallel.size());
        }
        else
        {
            for (i = 0; i < parallel.size() - 1; i++)
            {
                draw_template_line(dc, quo, pTemplate, sectorindex, sectorprogress,
                    parallel[i], parallel[i+1]);
            }
        }
    }
    else
    {
        //绘制原始的path（非平行线）
        for (i = 0; i < pointcount; i++)
        {
            pPath->GetPoint1(i, point);
            pTrans->Map2DeviceXY(point.x, point.y, wndpnt.x, wndpnt.y);
            pPointsWnd[i] = wndpnt;
        }

        if (simple)
        {
            ::Polyline(dc, pPointsWnd, pointcount);
        }
        else
        {
            for (i = 0; i < pointcount - 1; i++)
            {
                WKSPoint from, to;
                from.x = (double)pPointsWnd[i].x;
                from.y = (double)pPointsWnd[i].y;
                to.x = (double)pPointsWnd[i+1].x;
                to.y = (double)pPointsWnd[i+1].y;
                draw_template_line(dc, quo, pTemplate, sectorindex, sectorprogress,
                    from, to);
            }
        }
    }

    if (static_path_point_count < pointcount)
    {
        delete[] pPointsWnd;
    }

    return true;
}

bool DrawRing(const CRingPtr pRing, const CDisplayTransformationPtr pTrans, HDC dc)
{
    long pointcount = pRing->GetPointCount();
    if (3 > pointcount) return false;

    long i;
    WKSPointZ point;
    pRing->GetPoint1(0, point);
    long X0, Y0;
    pTrans->Map2DeviceXY(point.x, point.y, X0, Y0);
    ::BeginPath(dc);
    ::MoveToEx(dc, X0, Y0, NULL);
    for (i = 1; i < pointcount; i++)
    {
        pRing->GetPoint1(i, point);
        long X, Y;
        pTrans->Map2DeviceXY(point.x, point.y, X, Y);
        ::LineTo(dc, X, Y);
    }
    ::LineTo(dc, X0, Y0);
    ::EndPath(dc);
    ::FillPath(dc);

    //绘制边界线
    ::MoveToEx(dc, X0, Y0, NULL);
    for (i = 1; i < pointcount; i++)
    {
        pRing->GetPoint1(i, point);
        long X, Y;
        pTrans->Map2DeviceXY(point.x, point.y, X, Y);
        ::LineTo(dc, X, Y);
    }
    ::LineTo(dc, X0, Y0);

    return true;
}

bool DrawPolyline(const CPolylinePtr pPolyline,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    long geocount = pPolyline->GetPathCount();
    for (long i = 0; i < geocount; i++)
    {
        CPathPtr pPath;
        pPolyline->GetPathRef(pPath, i);
        DrawPath(pPath._p(), pTrans, dc);
    }
    return true;
}

bool DrawPolygon(const CPolygonPtr pPolygon,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    dword ringcount = pPolygon->GetRingCount();
    if (0 >= ringcount)
    {
        return false;
    }
    else if (1 == ringcount)
    {
        //如果只有一个Ring，那就直接画这个Ring
        CRingPtr pRing;
        pPolygon->GetRingRef(pRing, 0);
        DrawRing(pRing._p(), pTrans, dc);
        return true;
    }

    //有多个Ring，必须用PolyPolygon()来搞定

    //array of count of vertices
    int* pPolyCounts = new int[ringcount];
    if (!pPolyCounts)
    {
        return false;
    }

    //确定所有点的数量
    dword pntcount = 0;
    dword i = 0;
    for (; i < ringcount; i++)
    {
        CRingPtr pRing;
        pPolygon->GetRingRef(pRing, i);
        dword tmpcount = pRing->GetPointCount();
        pntcount += tmpcount;
        pPolyCounts[i] = tmpcount;//记录每一个Ring的点数量
    }
    //申请足够大的POINT数组
    POINT* pPnts = new POINT[pntcount];
    if (!pPnts)
    {
        delete[] pPolyCounts;
        return false;
    }

    //给点数组赋值
    dword pntindex = 0;
    for (i = 0; i < ringcount; i++)
    {
        CRingPtr pRing;
        pPolygon->GetRingRef(pRing, i);
        dword tmpcount = pRing->GetPointCount();
        for (dword j = 0; j < tmpcount; j++)
        {
            WKSPointZ wkspnt;
            pRing->GetPoint1(j, wkspnt);
            pTrans->Map2DeviceXY(wkspnt.x, wkspnt.y, pPnts[pntindex].x,
                pPnts[pntindex].y);
            pntindex++;
        }
    }

    //绘制Polygon
    ::PolyPolygon(dc, pPnts, pPolyCounts, ringcount);
    //绘制边界线
    ::PolyPolyline(dc, pPnts, (DWORD*)pPolyCounts, ringcount);

    delete[] pPnts;
    delete[] pPolyCounts;

    return true;
}

inline void DrawEnvelope(const WKSRect& envelope,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSPoint left_bottom(envelope.left, envelope.bottom);
    WKSPoint right_bottom(envelope.right, envelope.bottom);
    WKSPoint right_top(envelope.right, envelope.top);
    WKSPoint left_top(envelope.left, envelope.top);

    tagPOINT points[5];
    pTrans->Map2DeviceXY(left_bottom.x, left_bottom.y, points[0].x, points[0].y);
    pTrans->Map2DeviceXY(right_bottom.x, right_bottom.y, points[1].x, points[1].y);
    pTrans->Map2DeviceXY(right_top.x, right_top.y, points[2].x, points[2].y);
    pTrans->Map2DeviceXY(left_top.x, left_top.y, points[3].x, points[3].y);
    points[4].x = points[0].x;
    points[4].y = points[0].y;

    ::Polygon(dc, points, 5);
}

bool DrawEnvelope(const CEnvelopePtr pEnvelope,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSRect envelope;
    pEnvelope->GetMinX(envelope.left);
    pEnvelope->GetMinY(envelope.bottom);
    pEnvelope->GetMaxX(envelope.right);
    pEnvelope->GetMaxY(envelope.top);

    DrawEnvelope(envelope, pTrans, dc);
    return true;
}

bool DrawCircle(const CCirclePtr pCircle,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSRect envelope;
    pCircle->GetMBR(envelope);
    WKSPoint left_bottom(envelope.left, envelope.bottom);
    WKSPoint right_bottom(envelope.right, envelope.bottom);
    WKSPoint right_top(envelope.right, envelope.top);
    WKSPoint left_top(envelope.left, envelope.top);

    tagPOINT points[4];
    pTrans->Map2DeviceXY(left_bottom.x, left_bottom.y, points[0].x, points[0].y);
    pTrans->Map2DeviceXY(right_bottom.x, right_bottom.y, points[1].x, points[1].y);
    pTrans->Map2DeviceXY(right_top.x, right_top.y, points[2].x, points[2].y);
    pTrans->Map2DeviceXY(left_top.x, left_top.y, points[3].x, points[3].y);

    tagRECT rect;
    rect.left = points[0].x;
    rect.bottom = points[0].y;
    rect.right = points[0].x;
    rect.top = points[0].y;

    rect.left = rect.left < points[1].x ? rect.left : points[1].x;
    rect.bottom = rect.bottom > points[1].y ? rect.bottom : points[1].y;
    rect.right = rect.right > points[1].x ? rect.right : points[1].x;
    rect.top = rect.top < points[1].y ? rect.top : points[1].y;

    rect.left = rect.left < points[2].x ? rect.left : points[2].x;
    rect.bottom = rect.bottom > points[2].y ? rect.bottom : points[2].y;
    rect.right = rect.right > points[2].x ? rect.right : points[2].x;
    rect.top = rect.top < points[2].y ? rect.top : points[2].y;

    rect.left = rect.left < points[3].x ? rect.left : points[3].x;
    rect.bottom = rect.bottom > points[3].y ? rect.bottom : points[3].y;
    rect.right = rect.right > points[3].x ? rect.right : points[3].x;
    rect.top = rect.top < points[3].y ? rect.top : points[3].y;

    ::BeginPath(dc);
    ::Ellipse(dc, rect.left, rect.top, rect.right, rect.bottom);
    ::EndPath(dc);
    ::FillPath(dc);
    ::Ellipse(dc, rect.left, rect.top, rect.right, rect.bottom);
    return true;
}

inline void DrawEllipse(const WKSRect& envelope,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSPoint left_bottom(envelope.left, envelope.bottom);
    WKSPoint right_bottom(envelope.right, envelope.bottom);
    WKSPoint right_top(envelope.right, envelope.top);
    WKSPoint left_top(envelope.left, envelope.top);

    tagPOINT points[4];
    pTrans->Map2DeviceXY(left_bottom.x, left_bottom.y, points[0].x, points[0].y);
    pTrans->Map2DeviceXY(right_bottom.x, right_bottom.y, points[1].x, points[1].y);
    pTrans->Map2DeviceXY(right_top.x, right_top.y, points[2].x, points[2].y);
    pTrans->Map2DeviceXY(left_top.x, left_top.y, points[3].x, points[3].y);

    tagRECT rect;
    rect.left = points[0].x;
    rect.bottom = points[0].y;
    rect.right = points[0].x;
    rect.top = points[0].y;

    rect.left = rect.left < points[1].x ? rect.left : points[1].x;
    rect.bottom = rect.bottom > points[1].y ? rect.bottom : points[1].y;
    rect.right = rect.right > points[1].x ? rect.right : points[1].x;
    rect.top = rect.top < points[1].y ? rect.top : points[1].y;

    rect.left = rect.left < points[2].x ? rect.left : points[2].x;
    rect.bottom = rect.bottom > points[2].y ? rect.bottom : points[2].y;
    rect.right = rect.right > points[2].x ? rect.right : points[2].x;
    rect.top = rect.top < points[2].y ? rect.top : points[2].y;

    rect.left = rect.left < points[3].x ? rect.left : points[3].x;
    rect.bottom = rect.bottom > points[3].y ? rect.bottom : points[3].y;
    rect.right = rect.right > points[3].x ? rect.right : points[3].x;
    rect.top = rect.top < points[3].y ? rect.top : points[3].y;

    ::BeginPath(dc);
    ::Ellipse(dc, rect.left, rect.top, rect.right, rect.bottom);
    ::EndPath(dc);
    ::FillPath(dc);
    ::Ellipse(dc, rect.left, rect.top, rect.right, rect.bottom);
}

bool DrawEllipse(const CEllipsePtr pEllipse,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSRect mbr;
    pEllipse->GetMBR(mbr);
    DrawEllipse(mbr, pTrans, dc);
    return true;
}

//********************************************************************************
//  第一组结束
//********************************************************************************


//********************************************************************************
//  第二组函数，传入Stream
//********************************************************************************

long DrawGeneralStream(CStreamPtr pStream, const CDisplayTransformationPtr pTrans, HDC dc)
{
    GeometryType geotype;//读出1byte的geotype前缀
    pStream->Read(&geotype, sizeof(GeometryType));

    //根据geotype调用具体的绘制函数
    switch(geotype)
    {
    case GEOMETRYTYPE_POINT:
        return sizeof(GeometryType) + DrawPointStream(pStream, pTrans, dc);
    case GEOMETRYTYPE_MULTIPOINT:
        return sizeof(GeometryType) + DrawMultiPointStream(pStream, pTrans, dc);
    case GEOMETRYTYPE_PATH:
        return sizeof(GeometryType) + DrawPathStream(pStream, pTrans, dc);
    case GEOMETRYTYPE_RING:
        return sizeof(GeometryType) + DrawRingStream(pStream, pTrans, dc);
    case GEOMETRYTYPE_POLYLINE:
        return sizeof(GeometryType) + DrawPolylineStream(pStream, pTrans, dc);
    case GEOMETRYTYPE_POLYGON:
        return sizeof(GeometryType) + DrawPolygonStream(pStream, pTrans, dc);
    case GEOMETRYTYPE_ENVELOPE:
        return sizeof(GeometryType) + DrawEnvelopeStream(pStream, pTrans, dc);
    case GEOMETRYTYPE_CIRCLE:
        return sizeof(GeometryType) + DrawCircleStream(pStream, pTrans, dc);
    case GEOMETRYTYPE_ELLIPSE:
        return sizeof(GeometryType) + DrawEllipseStream(pStream, pTrans, dc);
    default:
        return sizeof(GeometryType);
    }
}

long DrawPointStream(CStreamPtr pStream,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSPointZ point;
    pStream->Read(&point, sizeof(WKSPointZ));
    DrawPoint(point, pTrans, dc);
    return sizeof(WKSPointZ);//
}

long DrawMultiPointStream(CStreamPtr pStream,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    long oldpos = pStream->GetPos();
    long itemcount;
    pStream->Read(itemcount);
    for (long i = 0; i < itemcount; i++)
    {
        WKSPointZ point;
        pStream->Read(&point, sizeof(WKSPointZ));
        DrawPoint(point, pTrans, dc);
    }
    return pStream->GetPos() - oldpos;
}

long DrawPathStream(CStreamPtr pStream,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    long oldpos = pStream->GetPos();
    long pointcount;
    pStream->Read(pointcount);
    if (0 < pointcount)
    {
        DoDrawLineStream(pStream, pointcount, pTrans, dc);
    }

    return pStream->GetPos() - oldpos;
}

long DrawRingStream(CStreamPtr pStream,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    long oldpos = pStream->GetPos();
    DoDrawRingStream(pStream, pTrans, dc);
    return pStream->GetPos() - oldpos;
}

long DrawPolylineStream(CStreamPtr pStream,
    const CDisplayTransformationPtr pTrans, HDC dc)
{
    long oldpos = pStream->GetPos();

    long itemcount;
    pStream->Read(&itemcount, sizeof(long));
    for (long i = 0; i < itemcount; i++)
    {
        DrawPathStream(pStream, pTrans, dc);
    }

    return pStream->GetPos() - oldpos;
}

long DrawPolygonStream(CStreamPtr pStream,
   const CDisplayTransformationPtr pTrans, HDC dc)
{
    long oldpos = pStream->GetPos();

    dword ringcount;
    pStream->Read(&ringcount, sizeof(dword));

    //如果只有一个Ring，那就直接画这个Ring
    if (1 == ringcount)
    {
        DrawRingStream(pStream, pTrans, dc);
        return pStream->GetPos() - oldpos;
    }

    //有多个Ring，必须用PolyPolygon()来搞定

    //array of count of vertices
    int* pPolyCounts = new int[ringcount];

    //确定所有点的数量
    dword pntcount = 0;
    dword i = 0;
    for (; i < ringcount; i++)
    {
        long tmpcount;
        pStream->Read(&tmpcount, sizeof(long));
        pntcount += tmpcount;
        pPolyCounts[i] = tmpcount;//记录每一个Ring的点数量
        pStream->MovePos(tmpcount*(sizeof(WKSPointZ) + sizeof(VertexType)), SOFROMCURRENT);
    }
    //申请足够大的POINT数组
    POINT* pPnts = new POINT[pntcount];

    //取出每个点数值

    pStream->MovePos(oldpos + sizeof(long));

    dword pntindex = 0;
    for (i = 0; i < ringcount; i++)
    {
        dword tmpcount;
        pStream->Read(&tmpcount, sizeof(dword));
        for (dword j = 0; j < tmpcount; j++)
        {
            WKSPointZ wkspnt;
            pStream->Read(&wkspnt, sizeof(WKSPointZ));
            pTrans->Map2DeviceXY(wkspnt.x, wkspnt.y, pPnts[pntindex].x,
                pPnts[pntindex].y);
            pStream->MovePos(sizeof(VertexType), SOFROMCURRENT);
            pntindex++;
        }
    }

    //绘制Polygon
    ::PolyPolygon(dc, pPnts, pPolyCounts, ringcount);
    //绘制边界线
    ::PolyPolyline(dc, pPnts, (DWORD*)pPolyCounts, ringcount);

    delete[] pPnts;
    delete[] pPolyCounts;

    return pStream->GetPos() - oldpos;
}

long DrawEnvelopeStream(CStreamPtr pStream,
   const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSRect envelope;
    pStream->Read(&envelope, sizeof(WKSRect));
    DrawEnvelope(envelope, pTrans, dc);
    return sizeof(WKSRect);
}

long DrawCircleStream(CStreamPtr pStream,
   const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSPointZ cnt;
    double radius;
    pStream->Read(&cnt, sizeof(WKSPointZ));
    pStream->Read(&radius, sizeof(double));
    CCirclePtr pCircle = new CCircle(cnt, radius);
    DrawCircle(pCircle, pTrans, dc);
    return sizeof(WKSPoint) + sizeof(double);
}

long DrawEllipseStream(CStreamPtr pStream,
   const CDisplayTransformationPtr pTrans, HDC dc)
{
    WKSRect envelope;
    pStream->Read(&envelope, sizeof(WKSRect));
    DrawEllipse(envelope, pTrans, dc);
    return sizeof(WKSRect);
}

//********************************************************************************
//  第二组结束
//********************************************************************************


bool GetLabelPoint(const IGeometryPtr pGeometry, const CDisplayTransformationPtr pTrans,
    long& X, long& Y)
{
    GeometryType geotype = pGeometry->GetGeometryType();
    switch(geotype)
    {
    case GEOMETRYTYPE_POINT:
        {
            CPointPtr pPoint = (CPoint*)pGeometry._p();
            double x, y;
            pPoint->GetX(x);
            pPoint->GetY(y);
            pTrans->Map2DeviceXY(x, y, X, Y);
        }
        break;

    case GEOMETRYTYPE_MULTIPOINT:
        {
            CMultiPointPtr pMultiPoint = (CMultiPoint*)pGeometry._p();
            CPointPtr pGeoPoint = new CPoint;
            long count = pMultiPoint->GetPointCount();
            if (0 >= count)
            {
                return false;
            }
            WKSPointZ point;
            pMultiPoint->GetPoint(point, 0);
            pTrans->Map2DeviceXY(point.x, point.y, X, Y);
        }
        return true;

    case GEOMETRYTYPE_PATH:
        {
            //想办法标注在线上，瞎搞
            CPathPtr pPath = (CPath*)pGeometry._p();
            long pointcount = pPath->GetPointCount();
            long centerindex = 0;
            if (2 < pointcount)
            {
                centerindex = pointcount / 2;
            }

            WKSPointZ p1, p2;
            if (!pPath->GetPoint1(centerindex, p1))
            {
                return false;
            }

            if (!pPath->GetPoint1(centerindex + 1, p2))
            {
                return false;
            }

            pTrans->Map2DeviceXY((p1.x + p2.x) / 2, (p1.y + p2.y) / 2,
                X, Y);
        }
        break;

    case GEOMETRYTYPE_RING:
        {
            //求出label点
            CGeometryLabelPtr pGeometryLabel = new CGeometryLabel;
            pGeometryLabel->SetGeometry(pGeometry);
            double x, y;
            vector<WKSPoint> labelpoints;
            pGeometryLabel->GetLabelPoints(labelpoints);
            if (labelpoints.size() == 0)
            {
                WKSRect mbr;
                pGeometry->GetMBR(mbr);
                x = (mbr.left + mbr.right) / 2;
                y = (mbr.top + mbr.bottom) / 2;
            }

            pTrans->Map2DeviceXY(labelpoints[0].x, labelpoints[0].y, X, Y);
        }
        break;

    case GEOMETRYTYPE_POLYLINE:
        {
            //polyline画最长的那条
            CPolylinePtr pPolyline = (CPolyline*)pGeometry._p();
            long itemcount = pPolyline->GetPathCount();
            double length = 0;
            long itemtodraw = 0;
            for (long i = 0; i < itemcount; i++)
            {
                CPathPtr pPath;
                pPolyline->GetPathRef(pPath, i);
                double tmp = 0;
                pPath->GetLength(tmp);
                if (tmp > length)
                {
                    itemtodraw = i;
                    length = tmp;
                }
            }
            CPathPtr pPath;
            pPolyline->GetPathRef(pPath, itemtodraw);
            GetLabelPoint((IGeometry*)pPath._p(), pTrans, X, Y);
        }
        break;

    case GEOMETRYTYPE_POLYGON:
        {
            //多边形画mbr面积最大的那一块
            CPolygonPtr pPolygon = (CPolygon*)pGeometry._p();
            long itemcount = pPolygon->GetRingCount();
            double area = 0;
            long itemtodraw = 0;
            for (long i = 0; i < itemcount; i++)
            {
                CRingPtr pRing;
                pPolygon->GetRingRef(pRing, i);
                WKSRect mbr;
                pRing->GetMBR(mbr);
                double tmp = ::fabs((mbr.top - mbr.bottom)
                    * (mbr.right - mbr.left));
                if (tmp > area)
                {
                    itemtodraw = i;
                    area = tmp;
                }
            }
            CRingPtr pRing;
            pPolygon->GetRingRef(pRing, itemtodraw);
            GetLabelPoint((IGeometry*)pRing._p(), pTrans, X, Y);
        }
        break;

    case GEOMETRYTYPE_ENVELOPE:
        {
            CEnvelopePtr pEnvelope = (CEnvelope*)pGeometry._p();
            double x, y;
            pEnvelope->GetMinX(x);
            pEnvelope->GetMinY(y);
            pTrans->Map2DeviceXY(x, y, X, Y);
        }
        break;

    default:
        return false;
    }

    return true;
}

bool DrawLabel(const IGeometryPtr pGeometry, const char* const pcLabel,
    const CDisplayTransformationPtr pTrans, HDC dc, RECT& textenvelope)
{
    long X, Y;
    bool r = GetLabelPoint(pGeometry, pTrans, X, Y);
    if (!r) {return false;}

    long textcount = ::strlen(pcLabel);
    ::TextOut(dc, X, Y, pcLabel, textcount);
    SIZE txtsize;
    ::GetTextExtentPoint32(dc, pcLabel, textcount, &txtsize);
    textenvelope.left = X;
    textenvelope.top = Y;
    textenvelope.right = X + txtsize.cx;
    textenvelope.bottom = Y + txtsize.cy;

//    ::Rectangle(dc, X - 2, Y - 2, X + 2, Y + 2);
    return true;
}

bool DrawLabelXY(const double& x, const double& y, const char* const pcLabel,
    const CDisplayTransformationPtr pTrans, HDC dc, RECT& textenvelope)
{
    long X, Y;
    pTrans->Map2DeviceXY(x, y, X, Y);

    long textcount = ::strlen(pcLabel);
    ::TextOut(dc, X, Y, pcLabel, textcount);
    SIZE txtsize;
    ::GetTextExtentPoint32(dc, pcLabel, textcount, &txtsize);
    textenvelope.left = X;
    textenvelope.top = Y;
    textenvelope.right = X + txtsize.cx;
    textenvelope.bottom = Y + txtsize.cy;
    return true;
}

}

}