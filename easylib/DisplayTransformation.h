#if !defined(DISPLAYTRANSFORMATION_INCLUDED_)
#define DISPLAYTRANSFORMATION_INCLUDED_

#include "CommonInclude.h"
#include "..\\include\\InterfaceDisplayTransformation.h"
#include "Plot3D.h"

namespace easymap
{

class CDisplayTransformation;
typedef TSmartPtr<CDisplayTransformation> CDisplayTransformationPtr;

//================================================================================
//  这个类用来处理屏幕坐标和实地坐标之间的转换运算
//================================================================================
class CDisplayTransformation : public IDisplayTransformation
{
CLASS_NAME(CDisplayTransformation)
PERSIST_DUMP(CDisplayTransformation)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CDisplayTransformation();
private:
    ~CDisplayTransformation();

private:
    double      m_InnerScale;       //用于内部计算的比例尺，
                                    //注意并不直接等于显示比例尺，
                                    //参见Set/GetMapScale()方法

    WKSPoint    m_MapCenter;        //当前显示区域中心的实地坐标
    RECT        m_DeviceRect;       //显示设备（窗体）的大小
    long        m_LogPixelX;        //屏幕象素和英寸的映射关系（X轴）
    long        m_LogPixelY;        //屏幕象素和英寸的映射关系（Y轴）

    WKSPoint    m_RotateCenter;     //俯仰时的实际旋转中心

    WKSRect     m_VisibleExtent;    //当前显示范围，本来不需要这个，用来加速的
    WKSRect     m_ExtentExt;        //用来辅助旋转的，坐标变换时不起作用

    MapUnits    m_MapUnit;          //地图单位（M、KM）
    double      m_ReferenceScale;   //参考比例尺，主要用于随图放缩功能
    string      m_SR;

    CPlot3DPtr  m_pPlot3D;          //用于控制绘制时的旋转角度和俯仰角

    DisplayVisiExtChg* m_pVisiExtChg;

private:
    //为了计算实际旋转中心而搞的
    void finishplanerotate(const double& degree);

    //需要改变显示区域的时候最后调用这个
    void finishtranslation(bool callback = true);

    //  用于计算旋转以前的坐标
    inline double _device2map_x(const double x_dev) const;
    inline double _device2map_y(const double y_dev) const;
    inline double _map2device_x(const double x_map) const;
    inline double _map2device_y(const double y_map) const;

    //考虑了平面旋转，但没考虑俯仰
    void _device2map_rotated2d(const double dev_x, const double dev_y,
        WKSPoint& point_map) const;

    void ClipPath(const IPathPtr pPath, IGeometryPtr& pClipped) const;
    void ClipRing(const IRingPtr pRing, IGeometryPtr& pClipped) const;
    void ClipPolyline(const IPolylinePtr pPolyline, IGeometryPtr& pClipped) const;
    void ClipPolygon(const IPolygonPtr pPolygon, IGeometryPtr& pClipped) const;
    void ClipMultiPoint(const IMultiPointPtr pMultiPoint, IGeometryPtr& pClipped) const;

    bool _cliprect(WKSRect& rect) const;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    //---------------------------------------------------------------------------------
    //  内部使用，用于误差小于1像素的XX
    void Device2Map(const WKSPoint& point_dev, WKSPoint& point_map) const;
    void Map2Device(const WKSPoint& point_map, WKSPoint& point_dev) const;
    //---------------------------------------------------------------------------------

    void GetPlot3D(CPlot3DPtr& pPlot3D) const;

public:
    //  视图平面旋转，单位是度
    void __stdcall SetPlaneRotate(const double degree);
    void __stdcall GetPlaneRotate(double& degree) const;

    //俯仰角，单位是度
    void __stdcall SetAttitude(const double degree);
    void __stdcall GetAttitude(double& degree) const;

    //旋转中心，注意带俯仰的时候，旋转中心并不是地图当前显示区域的中心
    void __stdcall SetRotateCenter(const WKSPoint& rotatecenter);
    void __stdcall GetRotateCenter(WKSPoint& rotatecenter) const;

    //  设置/取出地图单位
    void __stdcall SetMapUnit(const MapUnits mapunit);
    MapUnits __stdcall GetMapUnit() const;

    //  设置/取出显示比例尺
    void __stdcall SetMapScale(const double mapscale);
    void __stdcall GetMapScale(double& mapscale) const;

    //  设置/取出当前显示区域的中心坐标
    void __stdcall SetMapCenter(const WKSPoint& center);
    void __stdcall GetMapCenter(WKSPoint& center) const;

    //  设置/取出当前可视范围
    void __stdcall SetVisibleExtent(const WKSRect& extent);
    void __stdcall GetVisibleExtent(WKSRect& extent) const;

    //  设置/取出显示设备（窗体）大小
    void __stdcall SetDeviceRect(const RECT& rect);
    void __stdcall GetDeviceRect(RECT& rect) const;

    //  设置/取出（屏幕象素和英寸的对应关系）
    void __stdcall SetLogPixel(const long logpixelx, const long logpixely);
    void __stdcall GetLogPixel(long& logpixelx, long& logpixely) const;

    //  屏幕坐标 <==> 实地坐标
    void __stdcall Device2MapXY(const long x_dev, const long y_dev, double& x_map,
        double& y_map) const;
    void __stdcall Map2DeviceXY(const double x_map, const double y_map,
        long& x_dev, long& y_dev) const;
    void __stdcall Device2Map(const POINT& point_dev, WKSPoint& point_map) const;
    void __stdcall Map2Device(const WKSPoint& point_map, POINT& point_dev) const;

    void __stdcall Device2Map_Envelope(const tagRECT& envelope_dev, WKSRect& envelope_map) const;

    //  设置/取出参考比例尺
    void __stdcall SetReferenceScale(const double refscale);
    void __stdcall GetReferenceScale(double& refscale) const;

    //点是否在可视平面范围内
    bool __stdcall PointInPlane(const double x, const double y) const;

    //用visible extent来裁切geometry，用于绘制之前的处理
    void __stdcall ClipGeometry(const IGeometry* pGeometry, IGeometry** ppClipped) const;

    //  空间参考垃圾
    bool __stdcall SetSpatialReference(const string& sr);
    bool __stdcall GetSpatialReference(string& sr) const;

    //当前显示范围改变时引发回调
    bool __stdcall SetVisibleExtentHandle(DisplayVisiExtChg* const pHandle);

    //  计算传入的参数和“米”的对应关系
    static double GetMeterQuotiety(const MapUnits unit);
    //  计算传入的参数和“英寸”的对应关系
    static double GetInchQuotiety(const MapUnits unit);
};
//================================================================================

CLASS_FACTORY(CDisplayTransformation)


bool POINTinRECT(const long& x, const long& y, const tagRECT& rect);
void ExtentViewExtentSlight(WKSRect& viewextent);

}

#endif
