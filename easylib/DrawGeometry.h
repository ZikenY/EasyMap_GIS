#if !defined(DRAWGEOMETRY_INCLUDED_)
#define DRAWGEOMETRY_INCLUDED_

//--------------------------------------------------------------------------------
//  这里提供简单的Geometry的绘制方法
//--------------------------------------------------------------------------------

#include "CommonInclude.h"
#include "DisplayTransformation.h"
#include "Geometry.h"
#include "..\\include\\InterfaceSymbol.h"

namespace easymap
{

namespace drawgeometry
{

//--------------------------------------------------------------------------------
//  线模板的绘制
//--------------------------------------------------------------------------------
//draw_template_line的回调，用来调用实际的绘制操作，绘制每一个mark
//注意from、to为屏幕坐标，使用double是为了保证运算精度
typedef void (callback_draw_template_sector)(const WKSPoint& from, const WKSPoint& to,
    const dword pointcount);
//用线模板绘制线段（path中的一段）缺省是绘制普通线，也可以利用drawmark来绘制点线、哈希线
//注意from、to为屏幕坐标，使用double是为了保证运算精度
void draw_template_line(const HDC dc, double quo, const ILineSimpleTemplatePtr pTemplate,
    long& sectorindex, long& sectorprogress, const WKSPoint& from, const WKSPoint& to,
    callback_draw_template_sector* const drawmark = NULL);
//--------------------------------------------------------------------------------


//********************************************************************************
//  第一组函数，传入Geometry
//********************************************************************************

//--------------------------------------------------------------------------------
//  绘制一个Geometry，不管它是什么（点、线、面都可以）
//  这个函数会根据Geometry的类型自动选择具体的绘制函数
//--------------------------------------------------------------------------------
bool DrawGeneralGeometry(
            const IGeometryPtr              pGeometry,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  绘制点
//--------------------------------------------------------------------------------
bool DrawPoint(
            const CPointPtr                 pPoint,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  绘制点群
//--------------------------------------------------------------------------------
bool DrawMultiPoint(
            const CMultiPointPtr            pMultiPoint,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  绘制连续线，注意这里支持线符号模板和平行线，distance单位为毫米
//--------------------------------------------------------------------------------
bool DrawPath(
            const CPathPtr                  pPath,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc,
            const ILineSimpleTemplatePtr    pTemplate = NULL,
            const double&                   distance = 0
            );

//--------------------------------------------------------------------------------
//  绘制圈
//--------------------------------------------------------------------------------
bool DrawRing(
            const CRingPtr                  pRing,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  绘制复合折线对象
//--------------------------------------------------------------------------------
bool DrawPolyline(
            const CPolylinePtr              pPolyline,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  绘制复合多边形对象
//--------------------------------------------------------------------------------
bool DrawPolygon(
            const CPolygonPtr               pPolygon,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  绘制矩形对象
//--------------------------------------------------------------------------------
bool DrawEnvelope(
            const CEnvelopePtr              pEnvelope,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  绘制circle
//--------------------------------------------------------------------------------
bool DrawCircle(
            const CCirclePtr                pCircle,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  绘制ellipse
//--------------------------------------------------------------------------------
bool DrawEllipse(
            const CEllipsePtr               pEllipse,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//********************************************************************************
//  第一组结束
//********************************************************************************


//********************************************************************************
//  第二组函数，传入Stream
//********************************************************************************

//--------------------------------------------------------------------------------
//  绘制Stream中的Geoemtry，用于避免Geometry对象化，加快绘制速度
//  这个函数会根据Geometry的类型自动选择具体的绘制函数
//  注意这个函数需要传入的Stream有一个byte的geometrytype前缀
//--------------------------------------------------------------------------------
long DrawGeneralStream(
            CStreamPtr                      pStream,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------以下函数的block不需要geometrytype前缀-------------------------------------

//--------------------------------------------------------------------------------
//  点Stream
//--------------------------------------------------------------------------------
long DrawPointStream(
            CStreamPtr                      pStream,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  点群Stream
//--------------------------------------------------------------------------------
long DrawMultiPointStream(
            CStreamPtr                      pStream,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  连续折线Stream
//--------------------------------------------------------------------------------
long DrawPathStream(
            CStreamPtr                      pStream,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  连续圈Stream
//--------------------------------------------------------------------------------
long DrawRingStream(
            CStreamPtr                      pStream,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  复合折线Stream
//--------------------------------------------------------------------------------
long DrawPolylineStream(
            CStreamPtr                      pStream,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  复合多边形Stream
//--------------------------------------------------------------------------------
long DrawPolygonStream(
            CStreamPtr                      pStream,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  矩形Stream
//--------------------------------------------------------------------------------
long DrawEnvelopeStream(
            CStreamPtr                      pStream,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  圆Stream
//--------------------------------------------------------------------------------
long DrawCircleStream(
            CStreamPtr                      pStream,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//--------------------------------------------------------------------------------
//  ellipse Stream
//--------------------------------------------------------------------------------
long DrawEllipseStream(
            CStreamPtr                      pStream,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc
            );

//********************************************************************************
//  第二组结束
//********************************************************************************


//--------------------------------------------------------------------------------
//  找label点
//--------------------------------------------------------------------------------
bool GetLabelPoint(
            const IGeometryPtr              pGeometry,
            const CDisplayTransformationPtr pTrans,
            POINT&                          labelpoint
            );

//--------------------------------------------------------------------------------
//  绘制标签，位置由Geometry决定
//--------------------------------------------------------------------------------
bool DrawLabel(
            const IGeometryPtr              pGeometry,
            const char* const               pcLabel,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc,
            RECT&                           textenvelope
            );

//--------------------------------------------------------------------------------
//  绘制标签，位置由x、y坐标决定
//--------------------------------------------------------------------------------
bool DrawLabelXY(
            const double&                   x,
            const double&                   y,
            const char* const               pcLabel,
            const CDisplayTransformationPtr pTrans,
            HDC                             dc,
            RECT&                           textenvelope
            );

}

}

#endif
