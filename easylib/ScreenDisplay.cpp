#include "CommonInclude.h"
#include "ScreenDisplay.h"
#include "SimpleSymbol.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CScreenDisplay)

CScreenDisplay::CScreenDisplay()
{
    INIT_REFCOUNT

    m_pBufferDisplay = new CMiniDisplay;
    m_DCOK = false;
    this->SetDC(0);
    bitmapsaved_bufferdc = bitmapsaved_cachedc = bitmapsaved_blankdc = 0;
    RECT rect;
    this->GetDeviceRect(rect);
    this->SetRect(rect);
    m_NextCache = 0;
    m_PanStarted = false;
}

CScreenDisplay::~CScreenDisplay()
{
    if (m_pBufferDisplay->IsDrawing())
    {
        this->FinishDraw();
    }

    this->ReleaseBuffer();
}

void CScreenDisplay::ReleaseBuffer()
{
    if (m_DCOK)
    {
        ::DeleteDC(m_CacheDC);
        ::DeleteDC(m_BlankDC);
        HDC bufdc;
        m_pBufferDisplay->GetDC(bufdc);
        ::DeleteDC((HDC)bufdc);
        m_DCOK = false;
    }

    if (bitmapsaved_bufferdc) ::DeleteObject(bitmapsaved_bufferdc);
    if (bitmapsaved_cachedc) ::DeleteObject(bitmapsaved_cachedc);
    if (bitmapsaved_blankdc) ::DeleteObject(bitmapsaved_blankdc);
    bitmapsaved_bufferdc = bitmapsaved_cachedc = bitmapsaved_blankdc = 0;
}

void CScreenDisplay::GetDeviceRect(RECT& rect) const
{
    CDisplayTransformationPtr pDisplayTrans;
    m_pBufferDisplay->GetDisplayTransformation(pDisplayTrans);
    pDisplayTrans->GetDeviceRect(rect);
}

bool __stdcall CScreenDisplay::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IDisplay"))
        || (0 == strcmp(interfacename, "CDisplay")))
    {
        *pp = static_cast<CDisplay*>(this);
    }
    else if ((0 == strcmp(interfacename, "IDisplayCache"))
        || (0 == strcmp(interfacename, "CDisplayCache")))
    {
        *pp = static_cast<CDisplayCache*>(this);
    }
    else if ((0 == strcmp(interfacename, "IScreenBrowser"))
        || (0 == strcmp(interfacename, "CScreenBrowser")))
    {
        *pp = static_cast<CScreenBrowser*>(this);
    }
    else
    {
        *pp = NULL;
        return false;
    }

    static_cast<IObj*>(*pp)->_AddRef();
    return true;
}

bool __stdcall CScreenDisplay::Clone(IObj** ppObj) const
{
    return false;
}

dword __stdcall CScreenDisplay::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    m_pBufferDisplay->_DumpTo(pStream, assist);

    dword cachecount = m_Caches.size();
    pStream->WriteData(&cachecount, sizeof(dword));
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.begin();
    while (it != m_Caches.end())
    {
        long cacheid = it->first;
        pStream->WriteData(&cacheid, sizeof(long));
        CCacheItemPtr pCacheItem = it->second;
        CStreamPtr ps = (CStream*)pStream;
        pCacheItem->SaveToStream(ps, assist);
        it++;
    }

    pStream->WriteData(&m_NextCache, sizeof(long));

    return pStream->GetPos() - oldpos;
}

dword __stdcall CScreenDisplay::_LoadInstance(IStreamX* pStream, void* const assist)
{
    if (m_pBufferDisplay->m_IsDrawing) throw;

    if (m_DCOK) this->ReleaseBuffer();

    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_PanStarted = false;

    CPersistPtr pPersist;
    ::easymap::CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pBufferDisplay, CMiniDisplay)

    m_Caches.clear();
    dword cachecount;
    pStream->ReadData(&cachecount, sizeof(dword));
    for (dword i = 0; i < cachecount; i++)
    {
        long cacheid;
        pStream->ReadData(&cacheid, sizeof(long));
        CCacheItemPtr pCacheItem = new CCacheItem;
        pCacheItem->LoadFromStream(ps, assist);
        m_Caches[cacheid] = pCacheItem;
    }

    pStream->ReadData(&m_NextCache, sizeof(long));

    return pStream->GetPos() - oldpos;
}

bool __stdcall CScreenDisplay::SetDisplayTransformation(const IDisplayTransformation* pTrans)
{
    if (_invalid(pTrans)) return false;
    return m_pBufferDisplay->SetDisplayTransformation(pTrans);
}

bool CScreenDisplay::SetDisplayTransformation(CDisplayTransformationPtr pTrans)
{
    return m_pBufferDisplay->SetDisplayTransformation(pTrans);
}

bool __stdcall CScreenDisplay::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    return m_pBufferDisplay->GetDisplayTransformation(ppTrans);
}

bool CScreenDisplay::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    return m_pBufferDisplay->GetDisplayTransformation(pTrans);
}

bool __stdcall CScreenDisplay::SetDC(const HDC dc)
{
    if (m_pBufferDisplay->IsDrawing()) return false;
    this->ReleaseBuffer();

    m_WindowDC = dc;
    HDC bufferdc = ::CreateCompatibleDC(dc);
    m_pBufferDisplay->SetDC(bufferdc);
    m_CacheDC = ::CreateCompatibleDC(dc);
    m_BlankDC = ::CreateCompatibleDC(dc);

    map<long, CCacheItemPtr>::iterator it = m_Caches.begin();
    while (it != m_Caches.end())
    {
        CCacheItemPtr pCacheItem = it->second;
        pCacheItem->SetupDC(dc);
        it++;
    }

    m_DCOK = true;
    return true;
}

bool __stdcall CScreenDisplay::GetDC(HDC& dc) const
{
    if (!m_DCOK) return false;
    dc = (HDC)m_WindowDC;
    return true;
}

bool __stdcall CScreenDisplay::SetRect(const RECT& rect)
{
/*
    if (!m_DCOK) return false;
    HDC bufferdc;
    m_pBufferDisplay->SetRect(rect);

    m_pBufferDisplay->GetDC(bufferdc);
    long width = rect.right - rect.left;
    long height = rect.bottom - rect.top;
    bitmapsaved_bufferdc = ::CreateCompatibleBitmap(m_WindowDC, width, height);
    HBITMAP oldbitmap = (HBITMAP)::SelectObject(bufferdc, bitmapsaved_bufferdc);
    ::DeleteObject(oldbitmap);

    bitmapsaved_cachedc = ::CreateCompatibleBitmap(m_WindowDC, width, height);
    oldbitmap = (HBITMAP)::SelectObject(m_CacheDC, bitmapsaved_cachedc);
    ::DeleteObject(oldbitmap);

    bitmapsaved_blankdc = ::CreateCompatibleBitmap(m_WindowDC, width, height);
    oldbitmap = (HBITMAP)::SelectObject(m_BlankDC, bitmapsaved_blankdc);
    ::DeleteObject(oldbitmap);

    map<long, CCacheItemPtr>::iterator it = m_Caches.begin();
    while (it != m_Caches.end())
    {
        CCacheItemPtr pCacheItem = it->second;
        if (!pCacheItem->lock)
        {
            pCacheItem->Resize(rect);
        }
        it++;
    }

    this->SetBackgroundColor(this->GetBackgroundColor());
    return true;
*/

    if (!m_DCOK) return false;
    HDC bufferdc;
    m_pBufferDisplay->SetRect(rect);

    m_pBufferDisplay->GetDC(bufferdc);
    long width = rect.right - rect.left;
    long height = rect.bottom - rect.top;
    HBITMAP bitmap = ::CreateCompatibleBitmap(m_WindowDC, width, height);
    ::SelectObject(bufferdc, bitmap);
    ::DeleteObject(bitmap);

    bitmap = ::CreateCompatibleBitmap(m_WindowDC, width, height);
    ::SelectObject(m_CacheDC, bitmap);
    ::DeleteObject(bitmap);

    bitmap = ::CreateCompatibleBitmap(m_WindowDC, width, height);
    ::SelectObject(m_BlankDC, bitmap);
    ::DeleteObject(bitmap);

    map<long, CCacheItemPtr>::iterator it = m_Caches.begin();
    while (it != m_Caches.end())
    {
        CCacheItemPtr pCacheItem = it->second;
        if (!pCacheItem->lock)
        {
            pCacheItem->Resize(rect);
        }
        it++;
    }

    this->SetBackgroundColor(this->GetBackgroundColor());
    return true;

}

bool __stdcall CScreenDisplay::GetRect(RECT& rect) const
{
    if (!m_DCOK) return false;
    this->GetDeviceRect(rect);
    return true;
}

bool __stdcall CScreenDisplay::SetBackgroundColor(const COLORREF color)
{
    if (!m_pBufferDisplay->SetBackgroundColor(color))
        return false;

    COLORREF bgcolor = this->GetBackgroundColor();
    HBRUSH brush = ::CreateSolidBrush(bgcolor);
    RECT rect;
    this->GetDeviceRect(rect);
    ::FillRect(m_BlankDC, &rect, brush);
    ::DeleteObject(brush);
    return true;
}

COLORREF __stdcall CScreenDisplay::GetBackgroundColor() const
{
    return m_pBufferDisplay->GetBackgroundColor();
}

bool __stdcall CScreenDisplay::StartDraw()
{
    bool r = m_pBufferDisplay->StartDraw();
    if (!r) return false;

    CDisplayTransformationPtr pTrans;
    this->GetDisplayTransformation(pTrans);
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.begin();
    while (it != m_Caches.end())
    {
        CCacheItemPtr pCacheItem = it->second;
        pCacheItem->pPointSymbol->Prepare(pCacheItem->dc, pTrans._p());
        pCacheItem->pLineSymbol->Prepare(pCacheItem->dc, pTrans._p());
        pCacheItem->pFillSymbol->Prepare(pCacheItem->dc, pTrans._p());
        pCacheItem->pTextSymbol->Prepare(pCacheItem->dc, pTrans._p());
        it++;
    }

    return true;
}

bool __stdcall CScreenDisplay::FinishDraw()
{
    bool r = m_pBufferDisplay->FinishDraw();
//    if (r) this->RefreshWindow1();    //就是这里导致的该死的闪烁，TNND找了个把月
    return r;
}

bool __stdcall CScreenDisplay::IsDrawing() const
{
    return m_pBufferDisplay->IsDrawing();
}

bool __stdcall CScreenDisplay::SetSymbol(const ISymbol* pSymbol)
{
    return m_pBufferDisplay->SetSymbol(pSymbol);
}

bool CScreenDisplay::SetSymbol(const ISymbolPtr pSymbol)
{
    return m_pBufferDisplay->SetSymbol(pSymbol);
}

bool __stdcall CScreenDisplay::GetSymbol(const SymbolType symboltype, ISymbol** ppSymbol) const
{
    return m_pBufferDisplay->GetSymbol(symboltype, ppSymbol);
}

bool CScreenDisplay::GetSymbol(const SymbolType symboltype, ISymbolPtr& pSymbol) const
{
    return m_pBufferDisplay->GetSymbol(symboltype, pSymbol);
}

bool __stdcall CScreenDisplay::DrawGeometry(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) return false;
    return m_pBufferDisplay->DrawGeometry(pGeometry);
}

bool CScreenDisplay::DrawGeometry(const IGeometryPtr pGeometry) const
{
    return m_pBufferDisplay->DrawGeometry(pGeometry);
}

dword __stdcall CScreenDisplay::DrawStream(IStreamX* pStream) const
{
    if (_invalid(pStream)) return 0;
    return m_pBufferDisplay->DrawStream(pStream);
}

dword CScreenDisplay::DrawStream(CStreamPtr pStream) const
{
    return m_pBufferDisplay->DrawStream(pStream);
}

bool __stdcall CScreenDisplay::DrawText(const IGeometry* pGeometry,
    const char* const pcText, RECT& textenvelope) const
{
    if (_invalid(pGeometry) || _invalid(pcText)) return 0;
    return m_pBufferDisplay->DrawText(pGeometry, pcText, textenvelope);
}

bool CScreenDisplay::DrawText(const IGeometryPtr pGeometry,
    const char* const pcText, RECT& textenvelope) const
{
    return m_pBufferDisplay->DrawText(pGeometry, pcText, textenvelope);
}

bool __stdcall CScreenDisplay::DrawTextXY(const double x, const double y,
    const char* const pcText, RECT& textenvelope) const
{
    return m_pBufferDisplay->DrawTextXY(x, y, pcText, textenvelope);
}

bool __stdcall CScreenDisplay::RefreshWindow(const HDC destdc, const RECT& destrect,
    dword rop) const
{
    HDC bufferdc;
    RECT rect;
    m_pBufferDisplay->GetDC(bufferdc);
    m_pBufferDisplay->GetRect(rect);
    ::StretchBlt(
        destdc,
        destrect.left,
        destrect.top,
        destrect.right - destrect.left,
        destrect.bottom - destrect.top,
        bufferdc,
        rect.left,
        rect.top,
        rect.right - rect.left,
        rect.bottom - rect.top,
        rop
        );

    return true;
}

bool __stdcall CScreenDisplay::RefreshWindow1() const
{
    RECT rect;
    this->GetDeviceRect(rect);
    this->RefreshWindow(m_CacheDC, rect, SRCCOPY);
    long width = rect.right - rect.left;
    long height = rect.bottom - rect.top;
    ::BitBlt(m_WindowDC, rect.left, rect.top, width, height, m_CacheDC, 0, 0, SRCCOPY);
    return true;
}

void __stdcall CScreenDisplay::EraseContent(const WKSRect* const pEnvelope) const
{
    RECT rect;
    if (pEnvelope)
    {
        CDisplayTransformationPtr pTrans;
        this->GetDisplayTransformation(pTrans);
        pTrans->Map2DeviceXY(pEnvelope->left, pEnvelope->bottom, rect.left, rect.bottom);
        pTrans->Map2DeviceXY(pEnvelope->right, pEnvelope->top, rect.right, rect.top);
    }
    else
    {
        this->GetRect(rect);
    }

    HBRUSH brush = ::CreateSolidBrush(this->GetBackgroundColor());
    ::FillRect(m_BlankDC, &rect, brush);
    ::DeleteObject(brush);

    m_pBufferDisplay->EraseContent(pEnvelope);
}

long __stdcall CScreenDisplay::CreateCache(const COLORREF bgcolor, const BYTE alpha,
    const bool transparent, const DWORD rop)
{
    if (m_pBufferDisplay->IsDrawing()) {return false;}

    long cacheid = m_NextCache++;

    RECT rect;
    CDisplayTransformationPtr pTrans;
    CCacheItemPtr pCacheItem = new CCacheItem;
    pCacheItem->SetupDC(m_WindowDC);
    this->GetDisplayTransformation(pTrans);
    pCacheItem->bgcolor = bgcolor;
    pCacheItem->rop = rop;
    pCacheItem->transparent = transparent;
    pCacheItem->alpha = alpha;
    pTrans->GetDeviceRect(rect);
    pCacheItem->Resize(rect);
    m_Caches[cacheid] = pCacheItem;

    ISymbolPtr pSym;
    m_pBufferDisplay->GetSymbol(SYMBOLTYPE_POINT, pSym);
    this->SetCacheSymbol(cacheid, pSym);
    m_pBufferDisplay->GetSymbol(SYMBOLTYPE_LINE, pSym);
    this->SetCacheSymbol(cacheid, pSym);
    m_pBufferDisplay->GetSymbol(SYMBOLTYPE_FILL, pSym);
    this->SetCacheSymbol(cacheid, pSym);
    m_pBufferDisplay->GetSymbol(SYMBOLTYPE_TEXT, pSym);
    this->SetCacheSymbol(cacheid, pSym);

    return cacheid;
}

long __stdcall CScreenDisplay::GetCacheCount() const
{
    return m_Caches.size();
}

bool __stdcall CScreenDisplay::GetCacheID(const long index, long& cacheid) const
{
    if ((0 > index) || ((long)m_Caches.size() <= index))
        return false;

    map<long, CCacheItemPtr>::const_iterator it = m_Caches.begin();
    std::advance(it, index);
    cacheid = it->first;
    return true;
}

bool __stdcall CScreenDisplay::DeleteCache(const long cacheid)
{
    if (m_pBufferDisplay->IsDrawing()) return false;

    map<long, CCacheItemPtr>::iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) return false;
    m_Caches.erase(it);
    return true;
}

void __stdcall CScreenDisplay::ClearAllCaches()
{
    if (m_pBufferDisplay->IsDrawing()) return;

    m_Caches.clear();
}

bool __stdcall CScreenDisplay::SetLockCacheSize(const long cacheid, const bool lock)
{
    map<long, CCacheItemPtr>::iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) return false;
    it->second->lock = lock;
    return true;
}

bool __stdcall CScreenDisplay::GetLockCacheSize(const long cacheid, bool& lock)
{
    map<long, CCacheItemPtr>::iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) return false;
    lock = it->second->lock;
    return true;
}

bool __stdcall CScreenDisplay::SetCacheSize(const long cacheid, const dword width, const dword height)
{
    map<long, CCacheItemPtr>::iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) return false;
    RECT rect;
    rect.left = 0;
    rect.bottom = 0;
    rect.right = width;
    rect.top = height;
    it->second->Resize(rect);
    return true;
}

bool __stdcall CScreenDisplay::GetCacheDC(const long cacheid, HDC& dc) const
{
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) return false;
    CCacheItemPtr pCacheItem = it->second;
    dc = pCacheItem->dc;
    return true;
}

bool __stdcall CScreenDisplay::PostCache(const HDC dc, const long cacheid) const
{
    return this->PostCache1(dc, cacheid, 0, 0);
}

bool __stdcall CScreenDisplay::PostCache1(const HDC dc, const long cacheid, const long delta_x,
    const long delta_y) const
{
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) {return false;}

    RECT rect;
    this->GetRect(rect);
    long width = rect.right - rect.left;
    long height = rect.bottom - rect.top;
    rect.top += delta_y;
    rect.bottom += delta_y;
    rect.left += delta_x;
    rect.right += delta_x;

    CCacheItemPtr pCacheItem = it->second;

    bool msimg32ok = (NULL != extentapi::msimg32::getDLLHandle());

    if ((255 > pCacheItem->alpha) && msimg32ok)
    {
        BLENDFUNCTION blend = {AC_SRC_OVER, 0, pCacheItem->alpha, 0x00};
        extentapi::msimg32::AlphaBlend(dc, rect.left, rect.top, width,
            height, pCacheItem->dc, 0, 0, width, height, blend);
    }
    else if (pCacheItem->transparent && msimg32ok)
    {
        extentapi::msimg32::TransparentBlt(dc, rect.left, rect.top,
            width, height, pCacheItem->dc, 0, 0, width, height, pCacheItem->bgcolor);
    }
    else
    {
        ::BitBlt(dc, rect.left, rect.top, width, height,
            pCacheItem->dc, 0, 0, pCacheItem->rop);
    }

    return true;
}

bool __stdcall CScreenDisplay::PostCache2(const HDC dc, const long cacheid, const RECT& partial) const
{
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) {return false;}

    long width = partial.right - partial.left;
    long height = partial.bottom - partial.top;

    CCacheItemPtr pCacheItem = it->second;

    bool msimg32ok = (NULL != extentapi::msimg32::getDLLHandle());

    if ((255 > pCacheItem->alpha) && msimg32ok)
    {
        BLENDFUNCTION blend = {AC_SRC_OVER, 0, pCacheItem->alpha, 0x00};
        extentapi::msimg32::AlphaBlend(dc, partial.left, partial.top, width,
            height, pCacheItem->dc, partial.left, partial.top, width, height, blend);
    }
    else if (pCacheItem->transparent && msimg32ok)
    {
        extentapi::msimg32::TransparentBlt(dc, partial.left, partial.top,
            width, height, pCacheItem->dc, partial.left, partial.top, width, height,
            pCacheItem->bgcolor);
    }
    else
    {
        ::BitBlt(dc, partial.left, partial.top, width, height,
            pCacheItem->dc, partial.left, partial.top, pCacheItem->rop);
    }

    return true;
}

bool __stdcall CScreenDisplay::PostCacheToPrimary(const long cacheid) const
{
    HDC buffdc;
    this->m_pBufferDisplay->GetDC(buffdc);
    return this->PostCache(buffdc, cacheid);
}

bool __stdcall CScreenDisplay::PostCacheToPrimary1(const long cacheid, const long delta_x,
    const long delta_y) const
{
    HDC buffdc;
    this->m_pBufferDisplay->GetDC(buffdc);
    return this->PostCache1(buffdc, cacheid, delta_x, delta_y);
}

bool __stdcall CScreenDisplay::PostCacheToPrimary2(const long cacheid, const RECT& partial) const
{
    HDC buffdc;
    this->m_pBufferDisplay->GetDC(buffdc);
    return this->PostCache2(buffdc, cacheid, partial);
}

bool __stdcall CScreenDisplay::SetCacheBGColor(const long cacheid, const COLORREF color)
{
    if (this->IsDrawing()) {return false;}
    map<long, CCacheItemPtr>::iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) {return false;}
    it->second->bgcolor = color;
    RECT rect;
    this->GetDeviceRect(rect);
    it->second->Resize(rect);
    return true;
}

bool __stdcall CScreenDisplay::GetCacheBGColor(const long cacheid, COLORREF& color) const
{
    if (this->IsDrawing()) {return false;}
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) {return false;}
    color = it->second->bgcolor;
    return true;
}

bool __stdcall CScreenDisplay::SetCacheSymbol(const long cacheid, const ISymbol* pSymbol)
{
    if (_invalid(pSymbol)) return false;
    ISymbolPtr ps = (ISymbol*)pSymbol;
    return this->SetCacheSymbol(cacheid, ps);
}

bool CScreenDisplay::SetCacheSymbol(const long cacheid, const ISymbolPtr pSymbol)
{
    if (!pSymbol.Assigned()) return false;
    map<long, CCacheItemPtr>::iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) return false;

    IObjPtr pObj;
    CLONE_PTR(pSymbol, pObj)

    SymbolType symboltype = pSymbol->GetSymbolType();
    switch(symboltype)
    {
    case SYMBOLTYPE_POINT:
        CAST_PTR(pObj, it->second->pPointSymbol, IPointSymbol)
        break;

    case SYMBOLTYPE_LINE:
        CAST_PTR(pObj, it->second->pLineSymbol, ILineSymbol)
        break;

    case SYMBOLTYPE_FILL:
        CAST_PTR(pObj, it->second->pFillSymbol, IFillSymbol)
        break;

    case SYMBOLTYPE_TEXT:
        CAST_PTR(pObj, it->second->pTextSymbol, ITextSymbol)
        break;

    default:
        return false;
    }

    CDisplayTransformationPtr pDT;
    this->GetDisplayTransformation(pDT);
    if (this->IsDrawing())
    {
        ISymbolPtr pS;
        CAST_PTR(pObj, pS, ISymbol)
        pS->Prepare(it->second->dc, pDT._p());
    }

    return true;
}

bool __stdcall CScreenDisplay::GetCacheSymbol(const long cacheid, const SymbolType symboltype,
    ISymbol** ppSymbol) const
{
    if (_invalid(ppSymbol)) return false;
    ISymbolPtr ps;
    this->GetCacheSymbol(cacheid, symboltype, ps);
    *ppSymbol = ps._p();
    if (_valid(*ppSymbol))
    {
        (*ppSymbol)->_AddRef();
        return true;
    }

    return false;
}

bool CScreenDisplay::GetCacheSymbol(const long cacheid, const SymbolType symboltype,
    ISymbolPtr& pSymbol) const
{
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) {return false;}

    switch(symboltype)
    {
    case SYMBOLTYPE_POINT:
        {
            CAST_PTR(it->second->pPointSymbol, pSymbol, ISymbol)
        }
        break;
    case SYMBOLTYPE_LINE:
        {
            CAST_PTR(it->second->pLineSymbol, pSymbol, ISymbol)
        }
        break;
    case SYMBOLTYPE_FILL:
        {
            CAST_PTR(it->second->pFillSymbol, pSymbol, ISymbol)
        }
        break;
    case SYMBOLTYPE_TEXT:
        {
            CAST_PTR(it->second->pTextSymbol, pSymbol, ISymbol)
        }
        break;
    default:
        return false;
    }

    return true;
}

bool __stdcall CScreenDisplay::DrawCacheGeometry(const long cacheid, const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) {return false;}
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->DrawCacheGeometry(cacheid, pg);
}

bool CScreenDisplay::DrawCacheGeometry(const long cacheid, const IGeometryPtr pGeometry) const
{
    if (!this->IsDrawing()) {return false;}

    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end())
    {
        return false;
    }
    CCacheItemPtr pCacheItem = it->second;

    GeometryType geotype = pGeometry->GetGeometryType();
    bool r = false;
    switch (geotype)
    {
    case GEOMETRYTYPE_POINT:
    case GEOMETRYTYPE_MULTIPOINT:
        {
            r = pCacheItem->pPointSymbol->Draw(pGeometry._p());
            break;
        }

    case GEOMETRYTYPE_PATH:
    case GEOMETRYTYPE_POLYLINE:
        {
            r = pCacheItem->pLineSymbol->Draw(pGeometry._p());
            break;
        }

    case GEOMETRYTYPE_RING:
    case GEOMETRYTYPE_POLYGON:
    case GEOMETRYTYPE_ENVELOPE:
    case GEOMETRYTYPE_CIRCLE:
    case GEOMETRYTYPE_ELLIPSE:
        {
            r = pCacheItem->pFillSymbol->Draw(pGeometry._p());
            break;
        }

    default:
        {
        }
    }

    return r;
}

dword __stdcall CScreenDisplay::DrawCacheStream(const long cacheid, IStreamX* pStream) const
{
    if (_invalid(pStream)) {return false;}
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawCacheStream(cacheid, ps);
}

dword CScreenDisplay::DrawCacheStream(const long cacheid, CStreamPtr pStream) const
{
    if (!this->IsDrawing()) {return false;}

    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end())
    {
        return false;
    }
    CCacheItemPtr pCacheItem = it->second;

    GeometryType geotype;//读出1byte的geotype前缀
    dword r = 0;
    pStream->Read((char*)&geotype, sizeof(GeometryType));
    pStream->MovePos(-long(sizeof(GeometryType)), SOFROMCURRENT);

    switch(geotype)
    {
    case GEOMETRYTYPE_POINT:
    case GEOMETRYTYPE_MULTIPOINT:
        {
            r = pCacheItem->pPointSymbol->DrawStream(pStream._p());
            break;
        }
    case GEOMETRYTYPE_PATH:
    case GEOMETRYTYPE_POLYLINE:
        {
            r = pCacheItem->pLineSymbol->DrawStream(pStream._p());
            break;
        }
    case GEOMETRYTYPE_RING:
    case GEOMETRYTYPE_POLYGON:
    case GEOMETRYTYPE_ENVELOPE:
    case GEOMETRYTYPE_CIRCLE:
    case GEOMETRYTYPE_ELLIPSE:
        {
            r = pCacheItem->pFillSymbol->DrawStream(pStream._p());
            break;
        }
    default:
        {
        }
    }
    return r;
}

bool __stdcall CScreenDisplay::DrawCacheText(const long cacheid,
    const IGeometry* pGeometry, const char* const pcText, RECT& textenvelope) const
{
    if (_invalid(pGeometry)) {return false;}
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->DrawCacheText(cacheid, pg, pcText, textenvelope);
}

bool CScreenDisplay::DrawCacheText(const long cacheid, const IGeometryPtr pGeometry,
    const char* const pcText, RECT& textenvelope) const
{
    if (!this->IsDrawing()) {return false;}

    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) {return false;}
    CCacheItemPtr pCacheItem = it->second;

    pCacheItem->pTextSymbol->SetText(pcText);
    return pCacheItem->pTextSymbol->Draw(pGeometry._p(), textenvelope);
}

bool __stdcall CScreenDisplay::DrawCacheTextXY(const long cacheid, const double x,
    const double y, const char* const pcText, RECT& textenvelope) const
{
    if (!this->IsDrawing()) {return false;}

    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) {return false;}
    CCacheItemPtr pCacheItem = it->second;

    CPointPtr pPnt = new CPoint;
    pPnt->SetCoordinates(x, y, 0);

    pCacheItem->pTextSymbol->SetText(pcText);
    return pCacheItem->pTextSymbol->Draw(pPnt._p(), textenvelope);
}

bool __stdcall CScreenDisplay::CopyPrimaryToCache(const long cacheid, const long delta_x,
    const long delta_y)
{
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) {return false;}

    RECT rect;
    this->GetRect(rect);
    rect.top += delta_x;
    rect.left += delta_y;
    long width = rect.right - rect.left;
    long height = rect.bottom - rect.top;

    CCacheItemPtr pCacheItem = it->second;

    HDC buffdc;
    this->m_pBufferDisplay->GetDC(buffdc);

    ::BitBlt(pCacheItem->dc, rect.left, rect.top, width, height,
        buffdc, 0, 0, pCacheItem->rop);

    return true;
}

bool __stdcall CScreenDisplay::CopyPrimaryToCache1(const long cacheid, const RECT& partial)
{
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) {return false;}

    long width = partial.right - partial.left;
    long height = partial.bottom - partial.top;

    CCacheItemPtr pCacheItem = it->second;

    HDC buffdc;
    this->m_pBufferDisplay->GetDC(buffdc);

    ::BitBlt(pCacheItem->dc, partial.left, partial.top, width, height,
        buffdc, partial.left, partial.top, pCacheItem->rop);

    return true;
}

bool __stdcall CScreenDisplay::CopyCacheToCache(const long fromid, const long toid,
    const long delta_x, const long delta_y)
{
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(toid);
    if (it == m_Caches.end()) {return false;}
    CCacheItemPtr pToCache = it->second;

    return this->PostCache1(pToCache->dc, fromid, delta_x, delta_y);
}

bool __stdcall CScreenDisplay::CopyCacheToCache1(const long fromid, const long toid, const RECT& partial)
{
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(toid);
    if (it == m_Caches.end()) {return false;}
    CCacheItemPtr pToCache = it->second;

    return this->PostCache2(pToCache->dc, fromid, partial);
}

bool __stdcall CScreenDisplay::EraseCacheContent(const long cacheid,
    const WKSRect* const pEnvelope) const
{
    RECT rect;
    this->GetRect(rect);
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.find(cacheid);
    if (it == m_Caches.end()) {return false;}

    if (pEnvelope)
    {
        CDisplayTransformationPtr pTrans;
        this->GetDisplayTransformation(pTrans);
        pTrans->Map2DeviceXY(pEnvelope->left, pEnvelope->bottom, rect.left, rect.bottom);
        pTrans->Map2DeviceXY(pEnvelope->right, pEnvelope->top, rect.right, rect.top);
    }
    else
    {
        this->GetRect(rect);
    }
    CCacheItemPtr pCacheItem = it->second;
    HBRUSH brush = ::CreateSolidBrush(pCacheItem->bgcolor);
    ::FillRect(pCacheItem->dc, &rect, brush);
    ::DeleteObject(brush);
    return true;
}

void __stdcall CScreenDisplay::EraseCachesContent(const WKSRect* const pEnvelope) const
{
    RECT rect;
    this->GetRect(rect);
    map<long, CCacheItemPtr>::const_iterator it = m_Caches.begin();
    while (it != m_Caches.end())
    {
        this->EraseCacheContent(it->first, pEnvelope);
        it++;
    }
}

bool __stdcall CScreenDisplay::PostCacheToWindow(const long cacheid) const
{
    HDC dc;
    this->GetDC(dc);
    return this->PostCache(dc, cacheid);
}

bool __stdcall CScreenDisplay::PanStart(const POINT& screenpoint)
{
    if (m_pBufferDisplay->IsDrawing() || m_PanStarted)
        return false;

    m_PanStartPnt.x = screenpoint.x;
    m_PanStartPnt.y = screenpoint.y;
    m_PanStarted = true;
    return true;
}

bool __stdcall CScreenDisplay::PanMoveTo(const POINT& screenpoint)
{
    if (!m_PanStarted)
        return false;

    HDC bufdc;
    RECT rect;
    m_pBufferDisplay->GetDC(bufdc);
    m_pBufferDisplay->GetRect(rect);

    //清除m_BlankDC
    COLORREF bgcolor = this->GetBackgroundColor();
    HBRUSH newbrush = ::CreateSolidBrush(bgcolor);
    ::FillRect(m_BlankDC, &rect, newbrush);
    ::DeleteObject(newbrush);

    //先把偏移过的飞机贴到m_BlankDC上
    ::BitBlt(m_BlankDC, screenpoint.x - m_PanStartPnt.x,
        screenpoint.y - m_PanStartPnt.y,
        rect.right - rect.left,
        rect.bottom - rect.top,
        (HDC)bufdc, 0, 0, SRCCOPY);

    //再把m_BlankDC上的内容贴到窗体DC上
    BitBlt(m_WindowDC, 0, 0, rect.right - rect.left,
        rect.bottom - rect.top, m_BlankDC, 0, 0, SRCCOPY);

    m_PanToPnt.x = screenpoint.x;
    m_PanToPnt.y = screenpoint.y;
    return true;
}

bool __stdcall CScreenDisplay::PanStop()
{
    if (!m_PanStarted)
    {
        return false;
    }

    WKSPoint oldcenter;
    CDisplayTransformationPtr pTrans;
    m_pBufferDisplay->GetDisplayTransformation(pTrans);
    pTrans->GetMapCenter(oldcenter);

    WKSPointZ startpoint, stoppoint;
    pTrans->Device2MapXY(m_PanStartPnt.x, m_PanStartPnt.y, startpoint.x, startpoint.y);
    pTrans->Device2MapXY(m_PanToPnt.x, m_PanToPnt.y, stoppoint.x, stoppoint.y);
    double deltax = startpoint.x - stoppoint.x;
    double deltay = startpoint.y - stoppoint.y;
    WKSPoint pointcenter;
    pointcenter.x = oldcenter.x + deltax;
    pointcenter.y = oldcenter.y + deltay;

    pTrans->SetMapCenter(pointcenter);
    RECT rect;
    pTrans->GetDeviceRect(rect);

    map<long, CCacheItemPtr>::const_iterator it = m_Caches.begin();
    while (it != m_Caches.end())
    {
        CCacheItemPtr pCacheItem = it->second;
        pCacheItem->Resize(rect);
        it++;
    }

//////////////////////////////////
//  好像没必要
//    HDC bufdc;
//    m_pBufferDisplay->GetDC(bufdc);
//    ::BitBlt(bufdc, rect.left, rect.top, rect.right - rect.left,
//        rect.bottom - rect.top, m_WindowDC, 0, 0, SRCCOPY);
//////////////////////////////////

    m_PanStarted = false;
    return true;
}

bool __stdcall CScreenDisplay::Paning()
{
    return m_PanStarted;
}

bool __stdcall CScreenDisplay::MapScaleAt(const double mapscale)
{
    if (m_pBufferDisplay->IsDrawing() || m_PanStarted)
        return false;

    CDisplayTransformationPtr pTrans;
    m_pBufferDisplay->GetDisplayTransformation(pTrans);
    pTrans->SetMapScale(mapscale);
    return true;
}

bool __stdcall CScreenDisplay::MapCenterAt(const POINT& wndpnt)
{
    if (m_pBufferDisplay->IsDrawing() || m_PanStarted)
        return false;

    CDisplayTransformationPtr pTrans;
    m_pBufferDisplay->GetDisplayTransformation(pTrans);
    WKSPoint cntpnt;
    pTrans->Device2Map(wndpnt, cntpnt);
    return this->MapCenterAt1(cntpnt);
}

bool __stdcall CScreenDisplay::MapCenterAt1(const WKSPoint& center)
{
    if (m_pBufferDisplay->IsDrawing() || m_PanStarted)
        return false;

    CDisplayTransformationPtr pTrans;
    m_pBufferDisplay->GetDisplayTransformation(pTrans);
    pTrans->SetMapCenter(center);
    return true;
}

bool __stdcall CScreenDisplay::VisibleExtentIn(const RECT& RECT)
{
    if (m_pBufferDisplay->IsDrawing() || m_PanStarted)
        return false;

    CDisplayTransformationPtr pTrans;
    m_pBufferDisplay->GetDisplayTransformation(pTrans);
    WKSRect extent;
//    pTrans->Device2MapXY(RECT.left, RECT.bottom, extent.left, extent.bottom);
//    pTrans->Device2MapXY(RECT.right, RECT.top, extent.right, extent.top);
//    CorrectEnvelope(extent);
    pTrans->Device2Map_Envelope(RECT, extent);

    if ((5 < abs(RECT.right - RECT.left))
        && (5 < abs(RECT.top - RECT.bottom)))
    {
        pTrans->SetVisibleExtent(extent);
    }
    else
    {
        WKSPoint centerpnt;
        centerpnt.x = (extent.left + extent.right) / 2;
        centerpnt.y = (extent.top + extent.bottom) / 2;
        pTrans->SetMapCenter(centerpnt);
        double mapscale;
        pTrans->GetMapScale(mapscale);
        pTrans->SetMapScale(mapscale / 2);
    }
    return true;
}

bool __stdcall CScreenDisplay::VisibleExtentOut(const RECT& RECT)
{
    if (m_pBufferDisplay->IsDrawing() || m_PanStarted)
        return false;

    CDisplayTransformationPtr pTrans;
    m_pBufferDisplay->GetDisplayTransformation(pTrans);
    WKSRect extent;
//    pTrans->Device2MapXY(RECT.left, RECT.bottom, extent.left, extent.bottom);
//    pTrans->Device2MapXY(RECT.right, RECT.top, extent.right, extent.top);
//    CorrectEnvelope(extent);
    pTrans->Device2Map_Envelope(RECT, extent);

    if ((5 < abs(RECT.right - RECT.left))
        && (5 < abs(RECT.top - RECT.bottom)))
    {
        this->VisibleMapExtentOut(extent);
    }
    else
    {
        WKSPoint centerpnt;
        centerpnt.x = (extent.left + extent.right) / 2;
        centerpnt.y = (extent.top + extent.bottom) / 2;
        pTrans->SetMapCenter(centerpnt);
        double mapscale;
        pTrans->GetMapScale(mapscale);
        pTrans->SetMapScale(mapscale * 2);
    }

    return true;
}

bool __stdcall CScreenDisplay::VisibleMapExtentIn(const WKSRect& extent)
{
    if (m_pBufferDisplay->IsDrawing() || m_PanStarted)
        return false;

    CDisplayTransformationPtr pTrans;
    m_pBufferDisplay->GetDisplayTransformation(pTrans);
    pTrans->SetVisibleExtent(extent);
    return true;
}

bool __stdcall CScreenDisplay::VisibleMapExtentOut(const WKSRect& extent)
{
    if (m_pBufferDisplay->IsDrawing() || m_PanStarted)
        return false;

    CDisplayTransformationPtr pTrans;
    m_pBufferDisplay->GetDisplayTransformation(pTrans);

    WKSRect oldextent;
    pTrans->GetVisibleExtent(oldextent);
    double quotiety1 = (oldextent.right - oldextent.left)
        / (extent.right - extent.left);
    double quotiety2 = (oldextent.top - oldextent.bottom)
        / (extent.top - extent.bottom);
    if (quotiety1 > quotiety2)
    {
        quotiety1 = quotiety2;
    }
    WKSPoint centerpoint;
    centerpoint.x = (extent.left + extent.right) / 2;
    centerpoint.y = (extent.top + extent.bottom) / 2;
    pTrans->SetMapCenter(centerpoint);
    double mapscale;
    pTrans->GetMapScale(mapscale);
    pTrans->SetMapScale(mapscale * quotiety1);

    return true;
}

bool CScreenDisplay::GetBufferDC(HDC& dc) const
{
    return m_pBufferDisplay->GetDC(dc);
}





CScreenDisplay::CCacheItem::CCacheItem()
{
    INIT_REFCOUNT

    wnddc = dc = 0;
    bitmapsaved = 0;
    bgcolor = RGB(255, 255, 255);
    rop = SRCAND;
    transparent = false;
    alpha = 255;
    lock = false;
}

CScreenDisplay::CCacheItem::~CCacheItem()
{
    if (dc) ::DeleteDC(dc);
    if (bitmapsaved) ::DeleteObject(bitmapsaved);
}

bool __stdcall CScreenDisplay::CCacheItem::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "CCacheItem")))
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

bool __stdcall CScreenDisplay::CCacheItem::Clone(IObj** ppObj) const
{
    return false;
}

void CScreenDisplay::CCacheItem::SetupDC(const HDC compatibledc)
{
    if (dc) ::DeleteDC(dc);
    wnddc = compatibledc;
    dc = ::CreateCompatibleDC(wnddc);
}

void CScreenDisplay::CCacheItem::Resize(const RECT& rect)
{
/*
    if (!dc) return;
    long width = rect.right - rect.left;
    long height = rect.bottom - rect.top;
    bitmapsaved = ::CreateCompatibleBitmap(wnddc, width, height);
    HBITMAP oldbitmap = (HBITMAP)::SelectObject(dc, bitmapsaved);
    ::DeleteObject(oldbitmap);

    HBRUSH brush = ::CreateSolidBrush(bgcolor);
    ::FillRect(dc, &rect, brush);
    ::DeleteObject(brush);
*/

    if (!dc) return;
    long width = rect.right - rect.left;
    long height = rect.bottom - rect.top;
    HBITMAP bitmap = ::CreateCompatibleBitmap(wnddc, width, height);
    ::SelectObject(dc, bitmap);
    ::DeleteObject(bitmap);

    HBRUSH brush = ::CreateSolidBrush(bgcolor);
    ::FillRect(dc, &rect, brush);
    ::DeleteObject(brush);
}

void CScreenDisplay::CCacheItem::SaveToStream(CStreamPtr pStream, void* const assist) const
{
    pStream->Write(bgcolor);
    pStream->Write(rop);
    pStream->WriteBool(transparent);
    pStream->Write(alpha);
    IStreamX* psx = pStream._p();
    pPointSymbol->_DumpTo(psx, assist);
    pLineSymbol->_DumpTo(psx, assist);
    pFillSymbol->_DumpTo(psx, assist);
    pTextSymbol->_DumpTo(psx, assist);
    pStream->WriteBool(lock);
}

void CScreenDisplay::CCacheItem::LoadFromStream(CStreamPtr pStream, void* const assist)
{
    wnddc = 0;
    if (dc) ::DeleteDC(dc);
    dc = 0;
    if (bitmapsaved) ::DeleteObject(bitmapsaved);
    bitmapsaved = 0;

    pStream->Read(bgcolor);
    pStream->Read(rop);
    pStream->ReadBool(transparent);
    pStream->Read(alpha);

    CPersistPtr pPersist;
    ::easymap::CPersist::_InstantiateFrom(pStream, pPersist, assist);
    CAST_PTR(pPersist, pPointSymbol, IPointSymbol)

    ::easymap::CPersist::_InstantiateFrom(pStream, pPersist, assist);
    CAST_PTR(pPersist, pLineSymbol, ILineSymbol)

    ::easymap::CPersist::_InstantiateFrom(pStream, pPersist, assist);
    CAST_PTR(pPersist, pFillSymbol, IFillSymbol)

    ::easymap::CPersist::_InstantiateFrom(pStream, pPersist, assist);
    CAST_PTR(pPersist, pTextSymbol, ITextSymbol)

    pStream->ReadBool(lock);
}

}
