#include "CommonInclude.h"
#include "ActiveView.h"

namespace easymap
{

bool __stdcall CActiveView::SetDisplay(IDisplay* pDisplay)
{
    if (_invalid(pDisplay)) return false;
    CDisplayPtr pD = (CDisplay*)pDisplay;
    return this->SetDisplay(pD);
}

bool __stdcall CActiveView::GetDisplay(IDisplay** ppDisplay) const
{
    if (_invalid(ppDisplay)) return false;
    CDisplayPtr pD;
    bool r = this->GetDisplay(pD);
    *ppDisplay = pD._p();
    if (_valid(*ppDisplay)) (*ppDisplay)->_AddRef();
    return r;
}

DrawResult __stdcall CActiveView::DrawData(IDisplay* pDisplay,
    const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    if (_invalid(pDisplay)) return LAYERDRAW_UNEXPECTED;
    CDisplayPtr pD = (CDisplay*)pDisplay;
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->DrawData(pD, pEnvelope, pTC);
}

DrawResult __stdcall CActiveView::DrawSelectionEx(IDisplay* pDisplay,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    if (_invalid(pDisplay)) return LAYERDRAW_UNEXPECTED;
    IDisplayPtr pDisp = pDisplay;
    CDisplayCachePtr pDC;
    CAST_PTR(pDisp, pDC, CDisplayCache)
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->DrawSelection(pDC, cacheid, pEnvelope, pTC);
}

DrawResult __stdcall CActiveView::DrawSelection(const WKSRect* const pEnvelope,
    const ITrackCancel* pTrackCancel)
{
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->DrawSelection(pEnvelope, pTC);
}

bool __stdcall CActiveView::EraseView()
{
    CDisplayPtr pDisplay;
    this->GetDisplay(pDisplay);
    pDisplay->EraseContent();
    return true;
}

bool __stdcall CActiveView::EraseSelectionView()
{
    CDisplayPtr pDisplay;
    this->GetDisplay(pDisplay);
    CDisplayCachePtr pCache;
    CAST_PTR(pDisplay, pCache, CDisplayCache)
    if (!pCache.Assigned())
        return false;
    pCache->EraseCachesContent();
    return true;
}

DrawResult __stdcall CActiveView::UpdateData(const ITrackCancel* pTrackCancel)
{
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->UpdateData(pTC);
}

DrawResult __stdcall CActiveView::UpdateSelection(const ITrackCancel* pTrackCancel)
{
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;
    return this->UpdateSelection(pTC);
}

DrawResult CActiveView::UpdateData(const ITrackCancelPtr pTrackCancel)
{
    if (!this->IsFocused())
    {
        return LAYERDRAW_NOREADY;
    }

    CDisplayPtr pDisplay;
    this->GetDisplay(pDisplay);
    CDisplayTransformationPtr pDT;
    pDisplay->GetDisplayTransformation(pDT);
    WKSRect viewextent;
    pDT->GetVisibleExtent(viewextent);
    double attitude, planerotate;
    pDT->GetAttitude(attitude);
    pDT->GetPlaneRotate(planerotate);
    long attr = attitude * 10000;
    long pr = planerotate * 10000;
    double a = attr % 3600000;
    double b = pr % 3600000;
    if ((fabs(a) > 0.000001) || (fabs(b) > 0.000001))
    {
        ExtentViewExtentSlight(viewextent);
    }

    return this->DrawData(NULL, &viewextent, pTrackCancel);
}

DrawResult CActiveView::UpdateSelection(const ITrackCancelPtr pTrackCancel)
{
    this->EraseSelectionView();

    CDisplayPtr pDisplay;
    this->GetDisplay(pDisplay);
    CDisplayTransformationPtr pDT;
    pDisplay->GetDisplayTransformation(pDT);
    WKSRect viewextent;
    pDT->GetVisibleExtent(viewextent);
    double attitude, planerotate;
    pDT->GetAttitude(attitude);
    pDT->GetPlaneRotate(planerotate);
    long attr = attitude * 10000;
    long pr = planerotate * 10000;
    double a = attr % 3600000;
    double b = pr % 3600000;
    if ((fabs(a) > 0.000001) || (fabs(b) > 0.000001))
    {
        ExtentViewExtentSlight(viewextent);
    }

    return this->DrawSelection(NULL, pTrackCancel);
}

}