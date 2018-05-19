#if !defined(INTERFACELABELLAYER_INCLUDED_)
#define INTERFACELABELLAYER_INCLUDED_

#include "InterfaceLayer.h"
#include "InterfaceSupport.h"

namespace easymap
{

class ILabelLayer;
class ILabelLayerManager;

typedef TSmartPtr<ILabelLayer> ILabelLayerPtr;
typedef TSmartPtr<ILabelLayerManager> ILabelLayerManagerPtr;

//================================================================================
//  LabelLayer用来显示SlimLayer、ShapeLayer的属性标注
//================================================================================
class ILabelLayer : public ILayer
{
public:
    //  必须是SlimLayer或者ShapeLayer
    virtual bool __stdcall SetVectorLayer(const ILayer* pLayer) = 0;
    virtual bool __stdcall GetVectorLayer(ILayer** ppLayer) const = 0;

    //  标注字段设置
    virtual bool __stdcall SetFieldIndex(const long fieldindex) = 0;
    virtual long __stdcall GetFieldIndex() = 0;

    //  注记符号
    virtual bool __stdcall SetTextSymbol(const ITextSymbol* pTextSymbol) = 0;
    virtual bool __stdcall GetTextSymbol(ITextSymbol** ppTextSymbol) const = 0;

    //  参考比例尺
    virtual bool __stdcall SetRefScale(const double scale) = 0;
    virtual double __stdcall GetRefScale() const = 0;

    //  取出可视范围内的标注内容
    //      ppTextPositions以2个double为一组：
    //          0 - label点x坐标
    //          1 - label点y坐标
    //
    //      ppLabelTexts的每个string和ppTextPositions中一组2个double搭配
    //
    //      test_dc用于计算textenvelope
    virtual DrawResult __stdcall GetLabelText(IDoubleArray** ppTextPositions,
        IStringArray** ppLabelTexts, const IDisplayTransformation* pDT,
        const ITrackCancel* pTrackCancel, const WKSRect& visibleextent) = 0;

    //  清空缓存内容，用于和数据图层同步
    virtual void __stdcall ClearCache() = 0;
};

//================================================================================
//  管理一组LabelLayer，由Map控制
//================================================================================
class ILabelLayerManager : public IPersist
{
public:
    //  管理一组LabelLayer
    virtual bool __stdcall AddLabelLayer(const ILabelLayer* pLabelLayer) = 0;
    virtual bool __stdcall RemoveLabelLayer(const dword index) = 0;
    virtual bool __stdcall RemoveLabelLayerEx(ILabelLayer* pLabelLayer) = 0;
    virtual bool __stdcall GetLabelLayer(ILabelLayer** ppLabelLayer, const dword index) const = 0;
    virtual bool __stdcall SetLabelLayerOrder(const ILabelLayer* pLabelLayer, const dword neworder) = 0;
    virtual void __stdcall ClearLabelLayers() = 0;
    virtual dword __stdcall GetLabelLayerCount() const = 0;

    //  是否避让
    virtual void __stdcall SetTextAvoidable(const bool avoidable) = 0;
    virtual bool __stdcall GetTextAvoidable() const = 0;

    //  绘制标注
    virtual DrawResult __stdcall DrawLabels(const IDisplay* pDisplay, const WKSRect* const pEnvelope,
        const ITrackCancel* pTrackCancel) const = 0;

    //  是否显示自动标注
    virtual void __stdcall EnableLabelDraw(bool Enable) = 0;
    virtual bool __stdcall LabelDrawEnabled() const = 0;
};

}

#endif
