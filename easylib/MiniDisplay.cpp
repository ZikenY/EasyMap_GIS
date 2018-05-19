#include "CommonInclude.h"
#include "MiniDisplay.h"
#include "MultiSymbol.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CMiniDisplay)

CMiniDisplay::CMiniDisplay()
{
    INIT_REFCOUNT

    m_pTrans = new CDisplayTransformation;
    m_DC = 0;
    m_IsDrawing = false;

    IMultiPointSymbolPtr pMPS = new CMultiPointSymbol;
    pMPS->AddSimpleSymbol(RGB(30, 30, 200), 2);
    CAST_PTR(pMPS, m_pPointSymbol, IPointSymbol)

    IMultiLineSymbolPtr pMLS = new CMultiLineSymbol;
    pMLS->AddSimpleSymbol(RGB(30, 30, 200), 0.3);
    CAST_PTR(pMLS, m_pLineSymbol, ILineSymbol)

    IMultiFillSymbolPtr pMFS = new CMultiFillSymbol;
    pMFS->AddSimpleSymbol(RGB(160, 180, 210));
    CAST_PTR(pMFS, m_pFillSymbol, IFillSymbol)

    m_pTextSymbol = new CSimpleTextSymbol;

    m_BackgroundColor = RGB(250, 252, 252);
}

CMiniDisplay::~CMiniDisplay()
{
    if (m_IsDrawing)
    {
        this->FinishDraw();
    }
}

RECT CMiniDisplay::GetRect() const
{
    RECT rect;
    m_pTrans->GetDeviceRect(rect);
    return rect;
}

bool __stdcall CMiniDisplay::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IDisplay"))
        || (0 == strcmp(interfacename, "CDisplay"))
        || (0 == strcmp(interfacename, "CMiniDisplay")))
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

bool __stdcall CMiniDisplay::Clone(IObj** ppObj) const
{
    return false;
}

dword __stdcall CMiniDisplay::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_pTrans->_DumpTo(pStream, assist);
    m_pPointSymbol->_DumpTo(pStream, assist);
    m_pLineSymbol->_DumpTo(pStream, assist);
    m_pFillSymbol->_DumpTo(pStream, assist);
    m_pTextSymbol->_DumpTo(pStream, assist);

    ps->Write(m_BackgroundColor);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CMiniDisplay::_LoadInstance(IStreamX* pStream, void* const assist)
{
    if (m_IsDrawing) throw;

    m_DC = 0;

    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    CPersistPtr pPersist;
    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pTrans, CDisplayTransformation)

    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pPointSymbol, IPointSymbol)

    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pLineSymbol, ILineSymbol)

    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pFillSymbol, IFillSymbol)

    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pTextSymbol, ITextSymbol)

    ps->Read(m_BackgroundColor);

    return pStream->GetPos() - oldpos;
}

bool __stdcall CMiniDisplay::SetDisplayTransformation(const IDisplayTransformation* pTrans)
{
    if (_invalid(pTrans)) return false;
    m_pTrans = (CDisplayTransformation*)pTrans;
    return true;
}

bool CMiniDisplay::SetDisplayTransformation(CDisplayTransformationPtr pTrans)
{
    m_pTrans = pTrans;
    return true;
}

bool __stdcall CMiniDisplay::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    if (_invalid(ppTrans)) return false;
    assert(!*ppTrans);

    *ppTrans = m_pTrans._p();
    if (_valid(*ppTrans))
    {
        (*ppTrans)->_AddRef();
        return true;
    }

    return false;
}

bool CMiniDisplay::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    pTrans = m_pTrans;
    return true;
}

bool __stdcall CMiniDisplay::SetDC(const HDC dc)
{
    if (m_IsDrawing) return false;
    m_DC = (HDC)dc;
    return true;
}

bool __stdcall CMiniDisplay::GetDC(HDC& dc) const
{
    dc = m_DC;
    return true;
}

bool __stdcall CMiniDisplay::SetRect(const RECT& rect)
{
    if (m_IsDrawing) return false;
    m_pTrans->SetDeviceRect(rect);
    this->SetBackgroundColor(m_BackgroundColor);
    return true;
}

bool __stdcall CMiniDisplay::GetRect(RECT& rect) const
{
    rect = this->GetRect();
    return true;
}

bool __stdcall CMiniDisplay::SetBackgroundColor(const COLORREF color)
{
    if (m_IsDrawing) return false;

    m_BackgroundColor = color;
    HBRUSH brush = ::CreateSolidBrush(m_BackgroundColor);
    ::FillRect(m_DC, &this->GetRect(), brush);
    ::DeleteObject(brush);
    return true;
}

COLORREF __stdcall CMiniDisplay::GetBackgroundColor() const
{
    return m_BackgroundColor;
}

bool __stdcall CMiniDisplay::StartDraw()
{
    if (m_IsDrawing)
    {
        return false;
    }

    m_pPointSymbol->Prepare(m_DC, m_pTrans._p());
    m_pLineSymbol->Prepare(m_DC, m_pTrans._p());
    m_pFillSymbol->Prepare(m_DC, m_pTrans._p());
    m_pTextSymbol->Prepare(m_DC, m_pTrans._p());

    m_IsDrawing = true;

    return true;
}

bool __stdcall CMiniDisplay::FinishDraw()
{
    if (!m_IsDrawing)
    {
        return false;
    }

    m_IsDrawing = false;

    return true;
}

bool __stdcall CMiniDisplay::IsDrawing() const
{
    return m_IsDrawing;
}

bool __stdcall CMiniDisplay::SetSymbol(const ISymbol* pSymbol)
{
    if (_invalid(pSymbol)) return false;
    ISymbolPtr ps = (ISymbol*)pSymbol;
    return this->SetSymbol(ps);
}

bool CMiniDisplay::SetSymbol(const ISymbolPtr pSymbol)
{
    if (!pSymbol.Assigned()) return false;

    IObjPtr pObj;
    CLONE_PTR(pSymbol, pObj)

    IPointSymbolPtr pPointSymbol;
    CAST_PTR(pObj, pPointSymbol, IPointSymbol)
    if (pPointSymbol.Assigned())
    {
        m_pPointSymbol = pPointSymbol;
        if (m_IsDrawing)
        {
            m_pPointSymbol->Prepare(m_DC, m_pTrans._p());
        }
        return true;
    }

    ILineSymbolPtr pLineSymbol;
    CAST_PTR(pObj, pLineSymbol, ILineSymbol)
    if (pLineSymbol.Assigned())
    {
        m_pLineSymbol = pLineSymbol;
        if (m_IsDrawing)
        {
            m_pLineSymbol->Prepare(m_DC, m_pTrans._p());
        }
        return true;
    }

    IFillSymbolPtr pFillSymbol;
    CAST_PTR(pObj, pFillSymbol, IFillSymbol)
    if (pFillSymbol.Assigned())
    {
        m_pFillSymbol = pFillSymbol;
        if (m_IsDrawing)
        {
            m_pFillSymbol->Prepare(m_DC, m_pTrans._p());
        }
        return true;
    }

    ITextSymbolPtr pTextSymbol;
    CAST_PTR(pObj, pTextSymbol, ITextSymbol)
    if (pTextSymbol.Assigned())
    {
        m_pTextSymbol = pTextSymbol;
        if (m_IsDrawing)
        {
            m_pTextSymbol->Prepare(m_DC, m_pTrans._p());
        }
        return true;
    }

    return false;
}

bool __stdcall CMiniDisplay::GetSymbol(const SymbolType symboltype, ISymbol** ppSymbol) const
{
    if (_invalid(ppSymbol)) return false;
    ISymbolPtr ps;
    this->GetSymbol(symboltype, ps);
    *ppSymbol = ps._p();
    if (_valid(*ppSymbol))
    {
        (*ppSymbol)->_AddRef();
        return true;
    }

    return false;
}

bool CMiniDisplay::GetSymbol(const SymbolType symboltype, ISymbolPtr& pSymbol) const
{
    IObjPtr pObj;

    switch(symboltype)
    {
    case SYMBOLTYPE_POINT:
        CLONE_PTR(m_pPointSymbol, pObj);
        break;

    case SYMBOLTYPE_LINE:
        CLONE_PTR(m_pLineSymbol, pObj);
        break;

    case SYMBOLTYPE_FILL:
        CLONE_PTR(m_pFillSymbol, pObj);
        break;

    case SYMBOLTYPE_TEXT:
        CLONE_PTR(m_pTextSymbol, pObj);
        break;

    default:
        return false;
    }

    CAST_PTR(pObj, pSymbol, ISymbol)
    return true;
}

bool __stdcall CMiniDisplay::DrawGeometry(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->DrawGeometry(pg);
}

bool CMiniDisplay::DrawGeometry(const IGeometryPtr pGeometry) const
{
    if (!m_IsDrawing) return false;

    GeometryType geotype = pGeometry->GetGeometryType();
    bool r = false;
    switch (geotype)
    {
    case GEOMETRYTYPE_POINT:
    case GEOMETRYTYPE_MULTIPOINT:
        {
            r = m_pPointSymbol->Draw(pGeometry._p());
            break;
        }

    case GEOMETRYTYPE_PATH:
    case GEOMETRYTYPE_POLYLINE:
        {
            r = m_pLineSymbol->Draw(pGeometry._p());
            break;
        }

    case GEOMETRYTYPE_RING:
    case GEOMETRYTYPE_POLYGON:
    case GEOMETRYTYPE_ENVELOPE:
    case GEOMETRYTYPE_CIRCLE:
    case GEOMETRYTYPE_ELLIPSE:
        {
            r = m_pFillSymbol->Draw(pGeometry._p());
            break;
        }

    default:
        {
        }
    }

    return r;
}

dword __stdcall CMiniDisplay::DrawStream(IStreamX* pStream) const
{
    if (_invalid(pStream)) return 0;
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawStream(ps);
}

dword CMiniDisplay::DrawStream(CStreamPtr pStream) const
{
    if (!m_IsDrawing) return false;

    GeometryType geotype;//读出1byte的geotype前缀
    dword r = 0;
    pStream->Read((char*)&geotype, sizeof(GeometryType));
    pStream->MovePos(-long(sizeof(GeometryType)), SOFROMCURRENT);
    switch(geotype)
    {
    case GEOMETRYTYPE_POINT:
    case GEOMETRYTYPE_MULTIPOINT:
        {
            r = m_pPointSymbol->DrawStream(pStream._p());
            break;
        }
    case GEOMETRYTYPE_PATH:
    case GEOMETRYTYPE_POLYLINE:
        {
            r = m_pLineSymbol->DrawStream(pStream._p());
            break;
        }
    case GEOMETRYTYPE_RING:
    case GEOMETRYTYPE_POLYGON:
    case GEOMETRYTYPE_ENVELOPE:
    case GEOMETRYTYPE_CIRCLE:
    case GEOMETRYTYPE_ELLIPSE:
        {
            r = m_pFillSymbol->DrawStream(pStream._p());
            break;
        }
    default:
        {
        }
    }
    return r;
}

bool __stdcall CMiniDisplay::DrawText(const IGeometry* pGeometry,
    const char* const pcText, RECT& textenvelope) const
{
    if (_invalid(pGeometry) || _invalid(pcText)) return 0;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->DrawText(pg, pcText, textenvelope);
}

bool CMiniDisplay::DrawText(const IGeometryPtr pGeometry,
    const char* const pcText, RECT& textenvelope) const
{
    if (!m_IsDrawing) return false;
    if (!pGeometry.Assigned()) return false;
    if (_invalid(pcText)) return false;

    m_pTextSymbol->SetText(pcText);
    return m_pTextSymbol->Draw(pGeometry._p(), textenvelope);
}

bool __stdcall CMiniDisplay::DrawTextXY(const double x, const double y,
    const char* const pcText, RECT& textenvelope) const
{
    if (!m_IsDrawing || _invalid(pcText)) return false;

    CPointPtr pPnt = new CPoint;
    pPnt->SetCoordinates(x, y, 0);
    return this->DrawText(pPnt._p(), pcText, textenvelope);
}

void __stdcall CMiniDisplay::EraseContent(const WKSRect* const pEnvelope) const
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

    COLORREF bgcolor = this->GetBackgroundColor();
    HBRUSH brush = ::CreateSolidBrush(bgcolor);
    ::FillRect(m_DC, &rect, brush);
    ::DeleteObject(brush);
}

bool __stdcall CMiniDisplay::RefreshWindow(const HDC destdc,
    const RECT& destrect, dword rop) const
{
    RECT rect = this->GetRect();
    ::StretchBlt(
            (HDC)destdc,
            destrect.left,
            destrect.top,
            destrect.right - destrect.left,
            destrect.bottom - destrect.top,
            (HDC)m_DC,
            rect.left,
            rect.top,
            rect.right - rect.left,
            rect.bottom - rect.top,
            rop
            );
    return true;
}

bool __stdcall CMiniDisplay::RefreshWindow1() const
{
    //MiniDisplay直接绘制到窗体DC上，所以不需要此方法
    return true;
}

}
