#if !defined(INTERFACEDISPLAYTRANSFORMATION_INCLUDED_)
#define INTERFACEDISPLAYTRANSFORMATION_INCLUDED_

#include "WKSInclude.h"
#include "InterfacePersist.h"
#include "InterfaceGeometry.h"
#include "MapUnits.h"
#include "windows.h"

namespace easymap
{

class IDisplayTransformation;
typedef TSmartPtr<IDisplayTransformation> IDisplayTransformationPtr;

//当前显示范围改变时引发回调
typedef void (_stdcall DisplayVisiExtChg)(const WKSRect&);

class IDisplayTransformation : public IPersist
{
public:
    //  视图旋转角度，单位是度
    virtual void __stdcall SetPlaneRotate(const double degree) = 0;
    virtual void __stdcall GetPlaneRotate(double& degree) const = 0;

    //俯仰角，单位是度
    virtual void __stdcall SetAttitude(const double degree) = 0;
    virtual void __stdcall GetAttitude(double& degree) const = 0;

    //旋转中心，注意带俯仰的时候，旋转中心并不是地图当前显示区域的中心
    virtual void __stdcall SetRotateCenter(const WKSPoint& rotatecenter) = 0;
    virtual void __stdcall GetRotateCenter(WKSPoint& rotatecenter) const = 0;

    //  设置/取出地图单位
    virtual void __stdcall SetMapUnit(const MapUnits mapunit) = 0;
    virtual MapUnits __stdcall GetMapUnit() const = 0;

    //  设置/取出显示比例尺
    virtual void __stdcall SetMapScale(const double mapscale) = 0;
    virtual void __stdcall GetMapScale(double& mapscale) const = 0;

    //  设置/取出当前显示区域的中心坐标
    virtual void __stdcall SetMapCenter(const WKSPoint& center) = 0;
    virtual void __stdcall GetMapCenter(WKSPoint& center) const = 0;

    //  设置/取出当前可视范围
    virtual void __stdcall SetVisibleExtent(const WKSRect& extent) = 0;
    virtual void __stdcall GetVisibleExtent(WKSRect& extent) const = 0;

    //  设置/取出显示设备（窗体）大小
    virtual void __stdcall SetDeviceRect(const RECT& rect) = 0;
    virtual void __stdcall GetDeviceRect(RECT& rect) const = 0;

    //  设置/取出（屏幕象素和英寸的对应关系）
    virtual void __stdcall SetLogPixel(const long logpixelx, const long logpixely) = 0;
    virtual void __stdcall GetLogPixel(long& logpixelx, long& logpixely) const = 0;

    //  屏幕坐标 <==> 实地坐标
    virtual void __stdcall Device2MapXY(const long x_dev, const long y_dev, double& x_map,
        double& y_map) const = 0;
    virtual void __stdcall Map2DeviceXY(const double x_map, const double y_map,
        long& x_dev, long& y_dev) const = 0;
    virtual void __stdcall Device2Map(const POINT& point_dev, WKSPoint& point_map) const = 0;
    virtual void __stdcall Map2Device(const WKSPoint& point_map, POINT& point_dev) const = 0;

    virtual void __stdcall Device2Map_Envelope(const tagRECT& envelope_dev, WKSRect& envelope_map) const = 0;

    //  设置/取出参考比例尺
    virtual void __stdcall SetReferenceScale(const double refscale) = 0;
    virtual void __stdcall GetReferenceScale(double& refscale) const = 0;

    //点是否在可视平面范围内
    virtual bool __stdcall PointInPlane(const double x, const double y) const = 0;

    //用visible extent来裁切geometry，用于绘制之前的处理
    //注意只是用来保证Geometry绘制的正确性
    virtual void __stdcall ClipGeometry(const IGeometry* pGeometry, IGeometry** ppClipped) const = 0;
    //--------------------------------------------------------------------------

    //当前显示范围改变时引发回调
    virtual bool __stdcall SetVisibleExtentHandle(DisplayVisiExtChg* const pHandle) = 0;
};

}

#endif
