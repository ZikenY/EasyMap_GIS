#if !defined(SIMPLESYMBOL_INCLUDED_)
#define SIMPLESYMBOL_INCLUDED_

#include "CommonInclude.h"
#include "Display.h"
#include "..\\include\\InterfaceSymbol.h"

namespace easymap
{

class CSimplePointSymbol;
class _EnvelopePointSymbol;
class CEnvelopePointSymbol;
class _PolyPointSymbol;
class CPolyPointSymbol;
class CLineSimpleTemplate;
class CSimpleLineSymbol;
class CPointLineSymbol;
class CSimpleFillSymbol;
class CPointFillSymbol;
class CSimpleTextSymbol;

typedef TSmartPtr<CSimplePointSymbol> CSimplePointSymbolPtr;
typedef TSmartPtr<_EnvelopePointSymbol> _EnvelopePointSymbolPtr;
typedef TSmartPtr<CEnvelopePointSymbol> CEnvelopePointSymbolPtr;
typedef TSmartPtr<_PolyPointSymbol> _PolyPointSymbolPtr;
typedef TSmartPtr<CPolyPointSymbol> CPolyPointSymbolPtr;
typedef TSmartPtr<CLineSimpleTemplate> CLineSimpleTemplatePtr;
typedef TSmartPtr<CSimpleLineSymbol> CSimpleLineSymbolPtr;
typedef TSmartPtr<CPointLineSymbol> CPointLineSymbolPtr;
typedef TSmartPtr<CSimpleFillSymbol> CSimpleFillSymbolPtr;
typedef TSmartPtr<CPointFillSymbol> CPointFillSymbolPtr;
typedef TSmartPtr<CSimpleTextSymbol> CSimpleTextSymbolPtr;


//================================================================================
//  内部使用
//================================================================================
class _SimplePointSymbol : public ISimplePointSymbol
{
CLASS_NAME(_SimplePointSymbol)

public:
    _SimplePointSymbol();

protected:
    ~_SimplePointSymbol(){};

    string                      m_Name;
    long                        m_Code;
    HDC                         m_DC;
    CDisplayTransformationPtr   m_pTrans;
    long                        m_ROP2;

    COLORREF                    m_Color;
    bool                        m_ColorLock;
    double                      m_Diameter;
    double                      m_Angle;
    double                      m_OffsetX;
    double                      m_OffsetY;
    double                      m_LineWidth;
    bool                        m_Solid;

    double                      m_ParentAngle;
    double                      m_ParentOffsetX;
    double                      m_ParentOffsetY;
    double                      m_ParentSize;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

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
    void __stdcall SetColorLock(const bool lock);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;

    bool __stdcall SetAngle(const double angle);
    bool __stdcall GetAngle(double& angle) const;
    bool __stdcall SetOffset(const double x, const double y);
    bool __stdcall GetOffset(double& x, double& y) const;
    bool __stdcall _ParentOffset(const WKSPoint& offset,
        const double angle, const double size);

    bool __stdcall SetDiameter(const double diameter);
    bool __stdcall GetDiameter(double& diameter) const;
    bool __stdcall SetLineWidth(const double width);
    bool __stdcall GetLineWidth(double& width) const;
    void __stdcall SetSolid(const bool solid);
    void __stdcall GetSolid(bool& solid) const;

protected:
    void BeforeDraw(HPEN& pensaved, HBRUSH& brushsaved,
        long& rop2saved, double& factor) const;
    void AfterDraw(const HPEN& pensaved, const HBRUSH& brushsaved,
        const long& rop2saved) const;
    virtual void DrawSimplePoint(const long x, const long y) const = 0;
};
//================================================================================


//================================================================================
//  圆点（或圆圈）符号
//================================================================================
class CSimplePointSymbol : public _SimplePointSymbol
{
CLASS_NAME(CSimplePointSymbol)
PERSIST_DUMP(CSimplePointSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CSimplePointSymbol(const long code = -1);

private:
    ~CSimplePointSymbol(){};

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

private:
    void DrawSimplePoint(const long X, const long Y) const;
};
//================================================================================


//================================================================================
//  腐竹
//================================================================================
class _EnvelopePointSymbol : public _SimplePointSymbol
{
CLASS_NAME(_EnvelopePointSymbol)
PERSIST_DUMP(_EnvelopePointSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    _EnvelopePointSymbol(const bool solid = false, const long code = -1);

private:
    ~_EnvelopePointSymbol(){};

private:
    double m_Width;
    double m_Height;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall SetWidth(const double width);
    bool __stdcall GetWidth(double& width) const;
    bool __stdcall SetHeight(const double height);
    bool __stdcall GetHeight(double& height) const;

private:
    void DrawSimplePoint(const long X, const long Y) const;

friend class CEnvelopePointSymbol;
};
//================================================================================


//================================================================================
//  矩形符号
//================================================================================
class CEnvelopePointSymbol : public IEnvelopePointSymbol
{
CLASS_NAME(CEnvelopePointSymbol)
PERSIST_DUMP(CEnvelopePointSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CEnvelopePointSymbol(const bool solid = false, const long code = -1);

private:
    ~CEnvelopePointSymbol(){};

private:
    _EnvelopePointSymbolPtr m_pEP;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    bool __stdcall SetCode(const long& code);
    long __stdcall GetCode() const;
    bool __stdcall SetName(const char* const name);
    const char* __stdcall GetName() const;
    bool __stdcall GetDC(HDC& dc) const;
    bool __stdcall GetDisplayTransformation(IDisplayTransformation** ppTrans) const;
    bool __stdcall GetROP2(long& rop2) const;

    bool __stdcall Prepare(const HDC dc, const IDisplayTransformation* pTrans,
        const long rop2 = R2_COPYPEN);
    bool __stdcall Draw(const IGeometry* pGeometry) const;
    dword __stdcall DrawStream(const IStreamX* pStream) const;
    bool __stdcall SetColor(const COLORREF color);
    bool __stdcall GetColor(COLORREF& color) const;
    void __stdcall SetColorLock(const bool lock);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;

    bool GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const;
    bool Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
        const long rop2 = R2_COPYPEN);
    bool Draw(const IGeometryPtr pGeometry) const;
    dword DrawStream(const CStreamPtr pStream) const;
    bool Clone(IObjPtr& pObj) const;

    bool __stdcall SetAngle(const double angle);
    bool __stdcall GetAngle(double& angle) const;
    bool __stdcall SetOffset(const double x, const double y);
    bool __stdcall GetOffset(double& x, double& y) const;
    bool __stdcall _ParentOffset(const WKSPoint& offset,
        const double angle, const double size);

public:
    bool __stdcall SetWidth(const double width);
    bool __stdcall GetWidth(double& width) const;
    bool __stdcall SetHeight(const double height);
    bool __stdcall GetHeight(double& height) const;
    bool __stdcall SetLineWidth(const double width);
    bool __stdcall GetLineWidth(double& width) const;
    void __stdcall SetSolid(const bool solid);
    void __stdcall GetSolid(bool& solid) const;
};


//================================================================================
//  腐竹
//================================================================================
class _PolyPointSymbol : public _SimplePointSymbol
{
CLASS_NAME(_PolyPointSymbol)
PERSIST_DUMP(_PolyPointSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    _PolyPointSymbol(const bool solid = false, const long code = -1);

private:
    ~_PolyPointSymbol(){};

private:
    vector<WKSPoint> m_Points;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool AddNode(const WKSPoint& node);
    bool MoveNode(const WKSPoint& node, const dword index);
    bool DeleteNode(const dword index);
    bool GetNode(const dword index, WKSPoint& node) const;
    bool SetNode(const dword index, const WKSPoint& node);
    bool SetNodeOrder(const dword index, const dword neworder);
    dword GetNodeCount() const;
    void ClearNodes();

private:
    void DrawSimplePoint(const long X, const long Y) const;

friend class CPolyPointSymbol;
};
//================================================================================


//================================================================================
//  多边形或折线点符号
//================================================================================
class CPolyPointSymbol : public IPolyPointSymbol
{
CLASS_NAME(CPolyPointSymbol)
PERSIST_DUMP(CPolyPointSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CPolyPointSymbol(const bool solid = false, const long code = -1);

private:
    ~CPolyPointSymbol(){};

private:
    _PolyPointSymbolPtr m_pPP;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    bool __stdcall SetCode(const long& code);
    long __stdcall GetCode() const;
    bool __stdcall SetName(const char* const name);
    const char* __stdcall GetName() const;
    bool __stdcall GetDC(HDC& dc) const;
    bool __stdcall GetDisplayTransformation(IDisplayTransformation** ppTrans) const;
    bool __stdcall GetROP2(long& rop2) const;

    bool __stdcall Prepare(const HDC dc, const IDisplayTransformation* pTrans,
        const long rop2 = R2_COPYPEN);
    bool __stdcall Draw(const IGeometry* pGeometry) const;
    dword __stdcall DrawStream(const IStreamX* pStream) const;
    bool __stdcall SetColor(const COLORREF color);
    bool __stdcall GetColor(COLORREF& color) const;
    void __stdcall SetColorLock(const bool lock);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;

    bool GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const;
    bool Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
        const long rop2 = R2_COPYPEN);
    bool Draw(const IGeometryPtr pGeometry) const;
    dword DrawStream(const CStreamPtr pStream) const;
    bool Clone(IObjPtr& pObj) const;

    bool __stdcall SetAngle(const double angle);
    bool __stdcall GetAngle(double& angle) const;
    bool __stdcall SetOffset(const double x, const double y);
    bool __stdcall GetOffset(double& x, double& y) const;
    bool __stdcall _ParentOffset(const WKSPoint& offset,
        const double angle, const double size);

public:
    bool __stdcall AddNode(const WKSPoint& node);
    bool __stdcall MoveNode(const WKSPoint& node, const dword index);
    bool __stdcall DeleteNode(const dword index);
    bool __stdcall GetNode(const dword index, WKSPoint& node) const;
    bool __stdcall SetNode(const dword index, const WKSPoint& node);
    bool __stdcall SetNodeOrder(const dword index, const dword neworder);
    dword __stdcall GetNodeCount() const;
    void __stdcall ClearNodes();
    bool __stdcall SetLineWidth(const double width);
    bool __stdcall GetLineWidth(double& width) const;
    void __stdcall SetSolid(const bool solid);
    void __stdcall GetSolid(bool& solid) const;
};
//================================================================================


//================================================================================
//  线模板
//================================================================================
class CLineSimpleTemplate : public ILineSimpleTemplate
{
CLASS_NAME(CLineSimpleTemplate)
PERSIST_DUMP(CLineSimpleTemplate)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

private:
    vector<dword> m_Sectors;
    bool m_FirstMark;
    double m_Factor;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

public:
    CLineSimpleTemplate();
    ~CLineSimpleTemplate(){};

public:
    bool __stdcall SetFirstMark(const bool mark);
    bool __stdcall GetFirstMark(bool& mark) const;
    bool __stdcall AddSector(const dword sector);
    bool __stdcall GetSector(const dword index, dword& sector) const;
    bool __stdcall GetCount(dword& count) const;
    bool __stdcall Clear();
    bool __stdcall SetFactor(const double factor);
    bool __stdcall GetFactor(double& factor) const;
};
//================================================================================


//================================================================================
//  普通线符号
//================================================================================
class CSimpleLineSymbol : public ISimpleLineSymbol
{
CLASS_NAME(CSimpleLineSymbol)
PERSIST_DUMP(CSimpleLineSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CSimpleLineSymbol(const long code = -1);

private:
    ~CSimpleLineSymbol(){};

private:
    string                      m_Name;
    long                        m_Code;
    HDC                         m_DC;
    CDisplayTransformationPtr   m_pTrans;
    long                        m_ROP2;

    COLORREF                    m_Color;
    bool                        m_ColorLock;
    double                      m_Width;
    double                      m_Offset;
    ILineSimpleTemplatePtr      m_pTemplate;
    double                      m_ParentOffset;
    double                      m_ParentSize;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

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
    void __stdcall SetColorLock(const bool lock);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;

    bool __stdcall SetWidth(const double width);
    bool __stdcall GetWidth(double& width) const;
    bool __stdcall SetTemplate(const ILineSimpleTemplate* const pTemplate);
    bool __stdcall GetTemplate(ILineSimpleTemplate** ppTemplate) const;
    bool SetTemplate(const ILineSimpleTemplatePtr pTemplate);
    bool GetTemplate(ILineSimpleTemplatePtr& pTemplate) const;
    bool __stdcall SetOffset(const double offset);
    bool __stdcall GetOffset(double& offset) const;
    bool __stdcall _ParentOffset(const double offset, const double size);

private:
    void BeforeDraw(HPEN& pensaved, long& rop2saved) const;
    void AfterDraw(const HPEN& pensaved, const long& rop2saved) const;
};
//================================================================================


//================================================================================
//  点线符号
//================================================================================
class CPointLineSymbol : public IPointLineSymbol
{
CLASS_NAME(CPointLineSymbol)
PERSIST_DUMP(CPointLineSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CPointLineSymbol(const long code = -1);

private:
    ~CPointLineSymbol(){};

private:
    string                      m_Name;
    long                        m_Code;
    HDC                         m_DC;
    CDisplayTransformationPtr   m_pTrans;
    long                        m_ROP2;

    bool                        m_ColorLock;
    double                      m_Offset;
    IPointSymbolPtr             m_pPointSymbol;
    ILineSimpleTemplatePtr      m_pTemplate;
    double                      m_ParentOffset;
    double                      m_ParentSize;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

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
    void __stdcall SetColorLock(const bool lock);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;

    bool __stdcall SetPointSymbol(const IPointSymbol* const pPointSymbol);
    bool __stdcall GetPointSymbol(IPointSymbol** ppPointSymbol) const;
    bool SetPointSymbol(const IPointSymbolPtr pPointSymbol);
    bool GetPointSymbol(IPointSymbolPtr& pPointSymbol) const;
    bool __stdcall SetTemplate(const ILineSimpleTemplate* const pTemplate);
    bool __stdcall GetTemplate(ILineSimpleTemplate** ppTemplate) const;
    bool SetTemplate(const ILineSimpleTemplatePtr pTemplate);
    bool GetTemplate(ILineSimpleTemplatePtr& pTemplate) const;
    bool __stdcall SetOffset(const double offset);
    bool __stdcall GetOffset(double& offset) const;
    bool __stdcall _ParentOffset(const double offset, const double size);
};
//================================================================================


//================================================================================
//  简单面符号
//================================================================================
class CSimpleFillSymbol : public ISimpleFillSymbol
{
CLASS_NAME(CSimpleFillSymbol)
PERSIST_DUMP(CSimpleFillSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CSimpleFillSymbol(const long code = -1);

private:
    ~CSimpleFillSymbol();

private:
    string                      m_Name;
    long                        m_Code;
    HDC                         m_DC;
    CDisplayTransformationPtr   m_pTrans;
    long                        m_ROP2;

    COLORREF                    m_FillColor;
    long                        m_FillStyle;
    long                        m_FillHatch;
    COLORREF                    m_BorderColor;
    double                      m_BorderWidth;
    bool                        m_ColorLock;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

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
    void __stdcall SetColorLock(const bool lock);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;

    bool __stdcall SetFillStyle(const long style);
    bool __stdcall GetFillStyle(long& style) const;
    bool __stdcall SetFillHatch(const long hatch);
    bool __stdcall GetFillHatch(long& hatch) const;
    bool __stdcall SetBorderColor(const COLORREF color);
    bool __stdcall GetBorderColor(COLORREF& color) const;
    bool __stdcall SetBorderWidth(const double width);
    bool __stdcall GetBorderWidth(double& width) const;

private:
    void BeforeDraw(HPEN& pensaved, HBRUSH& brushsaved,
        long& rop2saved) const;
    void AfterDraw(const HPEN& pensaved, const HBRUSH& brushsaved,
        const long& rop2saved) const;
};
//================================================================================


//================================================================================
//  点填充面符号
//================================================================================
class CPointFillSymbol : public IPointFillSymbol
{
CLASS_NAME(CPointFillSymbol)
PERSIST_DUMP(CPointFillSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CPointFillSymbol(const long code = -1);

private:
    ~CPointFillSymbol();

private:
    string                      m_Name;
    long                        m_Code;
    HDC                         m_DC;
    CDisplayTransformationPtr   m_pTrans;
    long                        m_ROP2;

    IPointSymbolPtr             m_pPointSymbol;
    ILineSymbolPtr              m_pBorderSymbol;
    double                      m_PointsSpaceX;
    double                      m_PointsSpaceY;
    double                      m_OffsetX;
    double                      m_OffsetY;
    PointFillStyle              m_FillStyle;
    bool                        m_ColorLock;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

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
    void __stdcall SetColorLock(const bool lock);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;

    bool __stdcall SetPointSymbol(const IPointSymbol* const pPointSymbol);
    bool __stdcall GetPointSymbol(IPointSymbol** ppPointSymbol) const;
    bool SetPointSymbol(const IPointSymbolPtr pPointSymbol);
    bool GetPointSymbol(IPointSymbolPtr& pPointSymbol) const;
    bool __stdcall SetPointsSpace(const double space_x, const double space_y);
    bool __stdcall GetPointsSpace(double& space_x, double& space_y) const;
    bool __stdcall SetPointsOffset(const double offset_x, const double offset_y);
    bool __stdcall GetPointsOffset(double& offset_x, double& offset_y) const;
    bool __stdcall SetFillStyle(const PointFillStyle fillstyle);
    bool __stdcall GetFillStyle(PointFillStyle& fillstyle) const;
    bool __stdcall SetBorderSymbol(const ILineSymbol* const pLineSymbol);
    bool __stdcall GetBorderSymbol(ILineSymbol** ppLineSymbol) const;
    bool SetBorderSymbol(const ILineSymbolPtr pLineSymbol);
    bool GetBorderSymbol(ILineSymbolPtr& pLineSymbol) const;

private:
    void BeforeDraw(HPEN& pensaved, HBRUSH& brushsaved,
        long& rop2saved) const;
    void AfterDraw(const HPEN& pensaved, const HBRUSH& brushsaved,
        const long& rop2saved) const;
    void GetPolygon(const IGeometryPtr pGeometry, CPolygonPtr& pPolygon,
        CRingPtr& pRing) const;
    void DrawRegularPoints(const CPolygonPtr pPolygon, const CRingPtr pRing) const;
    void DrawLabelPoint(const CPolygonPtr pPolygon, const CRingPtr pRing) const;
    void DrawSingleLabelPoint(const CRingPtr pRing) const;
};
//================================================================================


//================================================================================
//  单点注记符号
//================================================================================
class CSimpleTextSymbol : public ISimpleTextSymbol
{
CLASS_NAME(CSimpleTextSymbol)
PERSIST_DUMP(CSimpleTextSymbol)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CSimpleTextSymbol(const long code = -1);

private:
    ~CSimpleTextSymbol(){}

private:
    string                      m_Name;
    long                        m_Code;
    HDC                         m_DC;
    CDisplayTransformationPtr   m_pTrans;
    long                        m_ROP2;

    string                      m_Text;
    bool                        m_ColorLock;
    COLORREF                    m_Color;
    LOGFONT                     m_Font;
    double                      m_Width;
    double                      m_Height;
    double                      m_Angle;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

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
    void __stdcall SetColorLock(const bool lock);
    void __stdcall GetColorLock(bool& color) const;
    SymbolType __stdcall GetSymbolType() const;

    bool __stdcall Draw(const IGeometry* pGeometry, RECT& textenvelope) const;
    bool Draw(const IGeometryPtr pGeometry, RECT& textenvelope) const;
    bool __stdcall SetFont(const LOGFONT& font);
    bool __stdcall GetFont(LOGFONT& font) const;
    bool __stdcall SetWidth(const double width);
    bool __stdcall GetWidth(double& width) const;
    bool __stdcall SetHeight(const double height);
    bool __stdcall GetHeight(double& height) const;
    bool __stdcall SetAngle(const double angle);
    bool __stdcall GetAngle(double& angle) const;
    bool __stdcall SetText(const char* const text);
    const char* __stdcall GetText() const;

private:
    void BeforeDraw(HFONT& fontsaved, long& bkmodesaved,
        COLORREF& textcolorsaved, long& rop2saved) const;
    void AfterDraw(const HFONT& fontsaved, const long& bkmodesaved,
        const COLORREF& textcolorsaved, const long& rop2saved) const;
};
//================================================================================

CLASS_FACTORY(CSimplePointSymbol)
CLASS_FACTORY(_EnvelopePointSymbol)
CLASS_FACTORY(CEnvelopePointSymbol)
CLASS_FACTORY(_PolyPointSymbol)
CLASS_FACTORY(CPolyPointSymbol)
CLASS_FACTORY(CLineSimpleTemplate)
CLASS_FACTORY(CSimpleLineSymbol)
CLASS_FACTORY(CPointLineSymbol)
CLASS_FACTORY(CSimpleFillSymbol)
CLASS_FACTORY(CPointFillSymbol)
CLASS_FACTORY(CSimpleTextSymbol)

}

#endif