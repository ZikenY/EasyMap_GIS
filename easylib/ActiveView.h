#if !defined(ACTIVEVIEW_INCLUDED_)
#define ACTIVEVIEW_INCLUDED_

#include "CommonInclude.h"
#include "..\\include\\InterfaceActiveView.h"
#include "Display.h"

namespace easymap
{

class CActiveView;
typedef TSmartPtr<CActiveView> CActiveViewPtr;

//================================================================================
//  用来绑定窗体
//================================================================================
class CActiveView : public IActiveView
{
public:
    bool __stdcall SetDisplay(IDisplay* pDisplay);
    bool __stdcall GetDisplay(IDisplay** ppDisplay) const;

    virtual bool SetDisplay(CDisplayPtr pDisplay) = 0;
    virtual bool GetDisplay(CDisplayPtr& pDisplay) const = 0;

    DrawResult __stdcall DrawData(
        IDisplay*               pDisplay        = NULL,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancel*     pTrackCancel    = NULL
        );

    DrawResult __stdcall DrawSelectionEx(
        IDisplay*               pDisplay,
        const long              cacheid,      
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancel*     pTrackCancel    = NULL
        );

    DrawResult __stdcall DrawSelection(
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancel*     pTrackCancel    = NULL
        );

    virtual DrawResult DrawData(
        CDisplayPtr             pDisplay,
        const WKSRect* const    pEnvelope,
        const ITrackCancelPtr   pTrackCancel
        ) = 0;

    virtual DrawResult DrawSelection(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,      
        const WKSRect* const    pEnvelope,
        const ITrackCancelPtr   pTrackCancel
        ) = 0;

    virtual DrawResult DrawSelection(
        const WKSRect* const    pEnvelope,
        const ITrackCancelPtr   pTrackCancel
        ) = 0;

    bool __stdcall EraseView();
    bool __stdcall EraseSelectionView();

    DrawResult __stdcall UpdateData(const ITrackCancel* pTrackCancel = NULL);
    DrawResult __stdcall UpdateSelection(const ITrackCancel* pTrackCancel = NULL);

    DrawResult UpdateData(const ITrackCancelPtr pTrackCancel);
    DrawResult UpdateSelection(const ITrackCancelPtr pTrackCancel);
};
//================================================================================

}

#endif