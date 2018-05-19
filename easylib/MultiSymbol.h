#if !defined(MULTISYMBOL_INCLUDED_)
#define MULTISYMBOL_INCLUDED_

#include "CommonInclude.h"
#include "SimpleSymbol.h"

namespace easymap
{

class CMultiPointSymbol;
class CMultiLineSymbol;
class CMultiFillSymbol;

typedef TSmartPtr<CMultiPointSymbol> CMultiPointSymbolPtr;
typedef TSmartPtr<CMultiLineSymbol> CMultiLineSymbolPtr;
typedef TSmartPtr<CMultiFillSymbol> CMultiFillSymbolPtr;


//================================================================================
//  复合点符号
//================================================================================
class CMultiPointSymbol : public IMultiPointSymbol
{
CLASS_NAME(CMultiPointSymbol)
PERSIST_DUMP(CMultiPointSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CMultiPointSymbol(const long code = -1);

private:
    ~CMultiPointSymbol();

private:
    string                      m_Name;
    long                        m_Code;
    HDC                         m_DC;
    CDisplayTransformationPtr   m_pTrans;
    long                        m_ROP2;

    list<IPointSymbolPtr>       m_PSs;
    COLORREF                    m_Color;
    bool                        m_ColorLock;
    double                      m_Angle;
    double                      m_OffsetX;
    double                      m_OffsetY;
    double                      m_Size;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    IPointSymbolPtr GetSubSymbol(const dword index) const;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

public:
    bool __stdcall SetCode(const long& code);
    long __stdcall GetCode() const;
    bool __stdcall SetName(const char* const name);
    const char* __stdcall GetName() const;
    bool __stdcall GetDC(HDC& dc) const;
    bool __stdcall GetDisplayTransformation(IDisplayTransformation** ppTrans) const;
    bool GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const;
    bool __stdcall GetROP2(long& rop2) const;
    bool __stdcall Prepare(const HDC dc, const IDisplayTransformation* pTrans,
        const long rop2 = R2_COPYPEN);
    bool Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
        const long rop2 = R2_COPYPEN);

    bool __stdcall Draw(const IGeometry* pGeometry) const;
    bool Draw(const IGeometryPtr pGeometry) const;
    dword __stdcall DrawStream(const IStreamX* pStream) const;
    dword DrawStream(const CStreamPtr pStream) const;
    bool __stdcall SetColor(const COLORREF color);
    bool __stdcall GetColor(COLORREF& color) const;
    void __stdcall SetColorLock(const bool color);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;


public:
    bool __stdcall SetAngle(const double angle);
    bool __stdcall GetAngle(double& angle) const;
    bool __stdcall SetOffset(const double x, const double y);
    bool __stdcall GetOffset(double& x, double& y) const;
    bool __stdcall _ParentOffset(const WKSPoint& offset,
        const double angle, const double size);

public:
    bool __stdcall AddSymbol(const IPointSymbol* pSymbol);
    bool AddSymbol(const IPointSymbolPtr pSymbol);
    bool __stdcall AddSimpleSymbol(const COLORREF color,
        const double diameter);
    bool __stdcall SetSymbolRef(const IPointSymbol* pSymbol, const dword index);
    bool SetSymbolRef(const IPointSymbolPtr pSymbol, const dword index);
    bool __stdcall RemoveSymbol(const dword index);
    bool __stdcall GetSymbolRef(IPointSymbol** ppSymbol, const dword index) const;
    bool GetSymbolRef(IPointSymbolPtr& pSymbol, const dword index) const;
    bool __stdcall SetSymbolOrder(const IPointSymbol* pSymbol, const dword neworder);
    bool SetSymbolOrder(const IPointSymbolPtr pSymbol, const dword neworder);
    dword __stdcall GetSymbolCount() const;
    void __stdcall ClearSymbols();
    bool __stdcall SetSize(const double size);
    bool __stdcall GetSize(double& size) const;
};
//================================================================================


//================================================================================
//  复合线（平行线）符号
//================================================================================
class CMultiLineSymbol : public IMultiLineSymbol
{
CLASS_NAME(CMultiLineSymbol)
PERSIST_DUMP(CMultiLineSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CMultiLineSymbol(const long code = -1);

private:
    ~CMultiLineSymbol();

private:
    string                      m_Name;
    long                        m_Code;
    HDC                         m_DC;
    CDisplayTransformationPtr   m_pTrans;
    long                        m_ROP2;

    list<ILineSymbolPtr>        m_PSs;
    COLORREF                    m_Color;
    bool                        m_ColorLock;
    double                      m_Offset;
    double                      m_Size;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    ILineSymbolPtr GetSubSymbol(const dword index) const;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

public:
    bool __stdcall SetCode(const long& code);
    long __stdcall GetCode() const;
    bool __stdcall SetName(const char* const name);
    const char* __stdcall GetName() const;
    bool __stdcall GetDC(HDC& dc) const;
    bool __stdcall GetDisplayTransformation(IDisplayTransformation** ppTrans) const;
    bool GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const;
    bool __stdcall GetROP2(long& rop2) const;
    bool __stdcall Prepare(const HDC dc, const IDisplayTransformation* pTrans,
        const long rop2 = R2_COPYPEN);
    bool Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
        const long rop2 = R2_COPYPEN);

    bool __stdcall Draw(const IGeometry* pGeometry) const;
    bool Draw(const IGeometryPtr pGeometry) const;
    dword __stdcall DrawStream(const IStreamX* pStream) const;
    dword DrawStream(const CStreamPtr pStream) const;
    bool __stdcall SetColor(const COLORREF color);
    bool __stdcall GetColor(COLORREF& color) const;
    void __stdcall SetColorLock(const bool color);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;


public:
    bool __stdcall SetOffset(const double offset);
    bool __stdcall GetOffset(double& offset) const;
    bool __stdcall _ParentOffset(const double offset, const double size);

public:
    bool __stdcall AddSymbol(const ILineSymbol* pSymbol);
    bool AddSymbol(const ILineSymbolPtr pSymbol);
    bool __stdcall AddSimpleSymbol(const COLORREF color, const double linewidth);
    bool __stdcall SetSymbolRef(const ILineSymbol* pSymbol, const dword index);
    bool SetSymbolRef(const ILineSymbolPtr pSymbol, const dword index);
    bool __stdcall RemoveSymbol(const dword index);
    bool __stdcall GetSymbolRef(ILineSymbol** ppSymbol, const dword index) const;
    bool GetSymbolRef(ILineSymbolPtr& pSymbol, const dword index) const;
    bool __stdcall SetSymbolOrder(const ILineSymbol* pSymbol, const dword neworder);
    bool SetSymbolOrder(const ILineSymbolPtr pSymbol, const dword neworder);
    dword __stdcall GetSymbolCount() const;
    void __stdcall ClearSymbols();
    bool __stdcall SetSize(const double size);
    bool __stdcall GetSize(double& size) const;
};
//================================================================================

//================================================================================
//  复合面符号
//================================================================================
class CMultiFillSymbol : public IMultiFillSymbol
{
CLASS_NAME(CMultiFillSymbol)
PERSIST_DUMP(CMultiFillSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CMultiFillSymbol(const long code = -1);

private:
    ~CMultiFillSymbol();

private:
    string                      m_Name;
    long                        m_Code;
    HDC                         m_DC;
    CDisplayTransformationPtr   m_pTrans;
    long                        m_ROP2;

    list<IFillSymbolPtr>        m_PSs;
    COLORREF                    m_Color;
    bool                        m_ColorLock;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    IFillSymbolPtr GetSubSymbol(const dword index) const;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

public:
    bool __stdcall SetCode(const long& code);
    long __stdcall GetCode() const;
    bool __stdcall SetName(const char* const name);
    const char* __stdcall GetName() const;
    bool __stdcall GetDC(HDC& dc) const;
    bool __stdcall GetDisplayTransformation(IDisplayTransformation** ppTrans) const;
    bool GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const;
    bool __stdcall GetROP2(long& rop2) const;
    bool __stdcall Prepare(const HDC dc, const IDisplayTransformation* pTrans,
        const long rop2 = R2_COPYPEN);
    bool Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
        const long rop2 = R2_COPYPEN);

    bool __stdcall Draw(const IGeometry* pGeometry) const;
    bool Draw(const IGeometryPtr pGeometry) const;
    dword __stdcall DrawStream(const IStreamX* pStream) const;
    dword DrawStream(const CStreamPtr pStream) const;
    bool __stdcall SetColor(const COLORREF color);
    bool __stdcall GetColor(COLORREF& color) const;
    void __stdcall SetColorLock(const bool color);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;

public:
    bool __stdcall AddSymbol(const IFillSymbol* pSymbol);
    bool AddSymbol(const IFillSymbolPtr pSymbol);
    bool __stdcall AddSimpleSymbol(const COLORREF color);
    bool __stdcall SetSymbolRef(const IFillSymbol* pSymbol, const dword index);
    bool SetSymbolRef(const IFillSymbolPtr pSymbol, const dword index);
    bool __stdcall RemoveSymbol(const dword index);
    bool __stdcall GetSymbolRef(IFillSymbol** ppSymbol, const dword index) const;
    bool GetSymbolRef(IFillSymbolPtr& pSymbol, const dword index) const;
    bool __stdcall SetSymbolOrder(const IFillSymbol* pSymbol, const dword neworder);
    bool SetSymbolOrder(const IFillSymbolPtr pSymbol, const dword neworder);
    dword __stdcall GetSymbolCount() const;
    void __stdcall ClearSymbols();
};
//================================================================================

CLASS_FACTORY(CMultiPointSymbol)
CLASS_FACTORY(CMultiLineSymbol)
CLASS_FACTORY(CMultiFillSymbol)
}

#endif