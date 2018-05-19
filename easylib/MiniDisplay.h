#if !defined(MINIDISPLAY_INCLUDED_)
#define MINIDISPLAY_INCLUDED_

#include "Display.h"

namespace easymap
{

class CMiniDisplay;
typedef TSmartPtr<CMiniDisplay> CMiniDisplayPtr;

//================================================================================
//  最简单的Display，没有使用缓冲DC，直接绘制到真正的飞机上面
//  这个类设计得比较bt，一般来说还是使用ScreenDisplay吧
//================================================================================
class CMiniDisplay : public CDisplay
{
CLASS_NAME(CMiniDisplay)
PERSIST_DUMP(CMiniDisplay)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CMiniDisplay();
private:
    ~CMiniDisplay();

private:
    CDisplayTransformationPtr   m_pTrans;
    HDC                         m_DC;
    COLORREF                    m_BackgroundColor;
    bool                        m_IsDrawing;

    IPointSymbolPtr             m_pPointSymbol;
    ILineSymbolPtr              m_pLineSymbol;
    IFillSymbolPtr              m_pFillSymbol;
    ITextSymbolPtr              m_pTextSymbol;

private:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    RECT GetRect() const;

    bool __stdcall SetDisplayTransformation(const IDisplayTransformation* pTrans);
    bool __stdcall GetDisplayTransformation(IDisplayTransformation** ppTrans) const;
    bool SetDisplayTransformation(const CDisplayTransformationPtr pTrans);
    bool GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const;
    bool __stdcall SetDC(const HDC dc);
    bool __stdcall GetDC(HDC& dc) const;
    bool __stdcall SetRect(const RECT& rect);
    bool __stdcall GetRect(RECT& rect) const;
    bool __stdcall SetBackgroundColor(const COLORREF color);
    COLORREF __stdcall GetBackgroundColor() const;
    bool __stdcall StartDraw();
    bool __stdcall FinishDraw();
    bool __stdcall IsDrawing() const;
    bool __stdcall SetSymbol(const ISymbol* pSymbol);
    bool __stdcall GetSymbol(const SymbolType symboltype, ISymbol** ppSymbol) const;
    bool SetSymbol(const ISymbolPtr pSymbol);
    bool GetSymbol(const SymbolType symboltype, ISymbolPtr& pSymbol) const;
    bool __stdcall DrawGeometry(const IGeometry* pGeometry) const;
    bool DrawGeometry(const IGeometryPtr pGeometry) const;
    dword __stdcall DrawStream(IStreamX* pStream) const;
    dword DrawStream(CStreamPtr pStream) const;
    bool __stdcall DrawText(const IGeometry* pGeometry,
        const char* const pcText, RECT& textenvelope) const;
    bool DrawText(const IGeometryPtr pGeometry,
        const char* const pcText, RECT& textenvelope) const;
    bool __stdcall DrawTextXY(const double x, const double y, const char* const pcText,
        RECT& textenvelope) const;
    void __stdcall EraseContent(const WKSRect* const pEnvelope = NULL) const;
    bool __stdcall RefreshWindow(const HDC destdc, const RECT& destrect,
        dword rop = SRCCOPY) const;
    bool __stdcall RefreshWindow1() const;
    //---------------------------

friend class CScreenDisplay;
};
//================================================================================

CLASS_FACTORY(CMiniDisplay)

}

#endif