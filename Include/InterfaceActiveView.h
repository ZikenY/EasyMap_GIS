#if !defined(INTERFACEACTIVEVIEW_INCLUDED_)
#define INTERFACEACTIVEVIEW_INCLUDED_

#include "InterfaceDisplay.h"
#include "InterfaceTrackCancel.h"

namespace easymap
{

class IActiveView;
typedef TSmartPtr<IActiveView> IActiveViewPtr;

//IActiveView
class IActiveView : public IPersist
{
public:
    //Display是可以被替换的
    virtual bool __stdcall SetDisplay(IDisplay* pDisplay) = 0;
    virtual bool __stdcall GetDisplay(IDisplay** ppDisplay) const = 0;

    //传入窗体句柄，建立屏幕坐标、实地坐标（比例尺）之间的对应关系，
    //通常在窗体大小改变（OnSize）的时候调用，进行绘制操作之前必须先调用这个方法
    virtual bool __stdcall GainFocus(const HDC dc, const RECT& rect) = 0;

    //切断与（GainFocus()传入窗体句柄）窗体的绑定
    //调用了这个方法后就不能再绘制了，除非重新调用GainFocus()
    virtual bool __stdcall LostFocus() = 0;

    //GeoMap是否可以进行绘制操作（是否已经绑定了某个窗体）
    virtual bool __stdcall IsFocused() const = 0;

    //绘制数据
    //注意这个函数不会先清除dc中的原始内容
    virtual DrawResult __stdcall DrawData(
        IDisplay*               pDisplay        = NULL,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancel*     pTrackCancel    = NULL
        ) = 0;

    //绘制选择集
    virtual DrawResult __stdcall DrawSelection(
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancel*     pTrackCancel    = NULL
        ) = 0;

    //注意这个函数不会先清除dc中的原始内容
    virtual DrawResult __stdcall DrawSelectionEx(
        IDisplay*               pDisplay,
        const long              cacheid,      
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancel*     pTrackCancel    = NULL
        ) = 0;

    virtual void __stdcall RefreshWindow() = 0;

    //搽除窗体（用背景色填充）
    virtual bool __stdcall EraseView() = 0;

    //搽除选择集
    virtual bool __stdcall EraseSelectionView() = 0;

    //读取数据，绘制可视区域
    virtual DrawResult __stdcall UpdateData(const ITrackCancel* pTrackCancel = NULL) = 0;

    //读取数据，绘制可视区域
    virtual DrawResult __stdcall UpdateSelection(const ITrackCancel* pTrackCancel = NULL) = 0;

    virtual void __stdcall DrawingHint(const bool visible) = 0;
};

}

#endif