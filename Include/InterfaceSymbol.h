#if !defined(INTERFACESYMBOL_INCLUDED_)
#define INTERFACESYMBOL_INCLUDED_

#include "InterfaceDisplay.h"

namespace easymap
{

class ISimplePointSymbol;
class IEnvelopePointSymbol;
class IPolyPointSymbol;
class ILineSimpleTemplate;
class ISimpleLineSymbol;
class IPointLineSymbol;
class ISimpleFillSymbol;
class IPointFillSymbol;
class ISimpleTextSymbol;
class IMultiPointSymbol;
class IMultiLineSymbol;
class IMultiFillSymbol;
class ISymbolLib;

typedef TSmartPtr<ISimplePointSymbol> ISimplePointSymbolPtr;
typedef TSmartPtr<IEnvelopePointSymbol> IEnvelopePointSymbolPtr;
typedef TSmartPtr<IPolyPointSymbol> IPolyPointSymbolPtr;
typedef TSmartPtr<ILineSimpleTemplate> ILineSimpleTemplatePtr;
typedef TSmartPtr<ISimpleLineSymbol> ISimpleLineSymbolPtr;
typedef TSmartPtr<IPointLineSymbol> IPointLineSymbolPtr;
typedef TSmartPtr<ISimpleFillSymbol> ISimpleFillSymbolPtr;
typedef TSmartPtr<IPointFillSymbol> IPointFillSymbolPtr;
typedef TSmartPtr<ISimpleTextSymbol> ISimpleTextSymbolPtr;
typedef TSmartPtr<IMultiPointSymbol> IMultiPointSymbolPtr;
typedef TSmartPtr<IMultiLineSymbol> IMultiLineSymbolPtr;
typedef TSmartPtr<IMultiFillSymbol> IMultiFillSymbolPtr;
typedef TSmartPtr<ISymbolLib> ISymbolLibPtr;

typedef long PointFillStyle;
const PointFillStyle POINTFILLSTYLE_REGULAR     = 1;
const PointFillStyle POINTFILLSTYLE_LABELPOINT  = 2;


//单圆点
class ISimplePointSymbol : public IPointSymbol
{
public:
    virtual bool __stdcall SetDiameter(const double diameter) = 0;
    virtual bool __stdcall GetDiameter(double& diameter) const = 0;
    virtual bool __stdcall SetLineWidth(const double width) = 0;
    virtual bool __stdcall GetLineWidth(double& width) const = 0;
    virtual void __stdcall SetSolid(const bool solid) = 0;
    virtual void __stdcall GetSolid(bool& solid) const = 0;
};

//单方点
class IEnvelopePointSymbol : public IPointSymbol
{
public:
    virtual bool __stdcall SetWidth(const double width) = 0;
    virtual bool __stdcall GetWidth(double& width) const = 0;
    virtual bool __stdcall SetHeight(const double height) = 0;
    virtual bool __stdcall GetHeight(double& height) const = 0;
    virtual bool __stdcall SetLineWidth(const double width) = 0;
    virtual bool __stdcall GetLineWidth(double& width) const = 0;
    virtual void __stdcall SetSolid(const bool solid) = 0;
    virtual void __stdcall GetSolid(bool& solid) const = 0;
};

//折线或多边形点
class IPolyPointSymbol : public IPointSymbol
{
public:
    virtual bool __stdcall AddNode(const WKSPoint& node) = 0;
    virtual bool __stdcall MoveNode(const WKSPoint& node, const dword index) = 0;
    virtual bool __stdcall DeleteNode(const dword index) = 0;
    virtual bool __stdcall GetNode(const dword index, WKSPoint& node) const = 0;
    virtual bool __stdcall SetNode(const dword index, const WKSPoint& node) = 0;
    virtual bool __stdcall SetNodeOrder(const dword index, const dword neworder) = 0;
    virtual dword __stdcall GetNodeCount() const = 0;
    virtual void __stdcall ClearNodes() = 0;
    virtual bool __stdcall SetLineWidth(const double width) = 0;
    virtual bool __stdcall GetLineWidth(double& width) const = 0;
    virtual void __stdcall SetSolid(const bool solid) = 0;
    virtual void __stdcall GetSolid(bool& solid) const = 0;
};

//制图线符号模板
class ILineSimpleTemplate : public IPersist
{
public:
    virtual bool __stdcall SetFirstMark(const bool mark) = 0;
    virtual bool __stdcall GetFirstMark(bool& mark) const = 0;
    virtual bool __stdcall AddSector(const dword sector) = 0;
    virtual bool __stdcall GetSector(const dword index, dword& sector) const = 0;
    virtual bool __stdcall GetCount(dword& count) const = 0;
    virtual bool __stdcall Clear() = 0;
    virtual bool __stdcall SetFactor(const double factor) = 0;
    virtual bool __stdcall GetFactor(double& factor) const = 0;
};

//简单线（制图线）
class ISimpleLineSymbol : public ILineSymbol
{
public:
    virtual bool __stdcall SetWidth(const double width) = 0;
    virtual bool __stdcall GetWidth(double& width) const = 0;
    virtual bool __stdcall SetTemplate(const ILineSimpleTemplate* const pTemplate) = 0;
    virtual bool __stdcall GetTemplate(ILineSimpleTemplate** ppTemplate) const = 0;
};

//点线
class IPointLineSymbol : public ILineSymbol
{
public:
    virtual bool __stdcall SetPointSymbol(const IPointSymbol* const pPointSymbol) = 0;
    virtual bool __stdcall GetPointSymbol(IPointSymbol** ppPointSymbol) const = 0;
    virtual bool __stdcall SetTemplate(const ILineSimpleTemplate* const pTemplate) = 0;
    virtual bool __stdcall GetTemplate(ILineSimpleTemplate** ppTemplate) const = 0;
};

//简单填充面符号
class ISimpleFillSymbol : public IFillSymbol
{
public:
    virtual bool __stdcall SetFillStyle(const long style) = 0;
    virtual bool __stdcall GetFillStyle(long& style) const = 0;
    virtual bool __stdcall SetFillHatch(const long hatch) = 0;
    virtual bool __stdcall GetFillHatch(long& hatch) const = 0;
    virtual bool __stdcall SetBorderColor(const COLORREF color) = 0;
    virtual bool __stdcall GetBorderColor(COLORREF& color) const = 0;
    virtual bool __stdcall SetBorderWidth(const double width) = 0;
    virtual bool __stdcall GetBorderWidth(double& width) const = 0;
};

//点填充面符号
class IPointFillSymbol : public IFillSymbol
{
public:
    virtual bool __stdcall SetPointSymbol(const IPointSymbol* const pPointSymbol) = 0;
    virtual bool __stdcall GetPointSymbol(IPointSymbol** ppPointSymbol) const = 0;
    virtual bool __stdcall SetPointsSpace(const double space_x, const double space_y) = 0;
    virtual bool __stdcall GetPointsSpace(double& space_x, double& space_y) const = 0;
    virtual bool __stdcall SetPointsOffset(const double offset_x, const double offset_y) = 0;
    virtual bool __stdcall GetPointsOffset(double& offset_x, double& offset_y) const = 0;
    virtual bool __stdcall SetFillStyle(const PointFillStyle fillstyle) = 0;
    virtual bool __stdcall GetFillStyle(PointFillStyle& fillstyle) const = 0;
    virtual bool __stdcall SetBorderSymbol(const ILineSymbol* const pLineSymbol) = 0;
    virtual bool __stdcall GetBorderSymbol(ILineSymbol** ppLineSymbol) const = 0;
};

//简单注记符号
class ISimpleTextSymbol : public ITextSymbol
{
};

//多点符号
class IMultiPointSymbol : public IPointSymbol
{
public:
    virtual bool __stdcall AddSymbol(const IPointSymbol* pSymbol) = 0;
    virtual bool __stdcall AddSimpleSymbol(
        const COLORREF color    = RGB(0, 0, 200),
        const double diameter   = 1
        ) = 0;
    virtual bool __stdcall SetSymbolRef(const IPointSymbol* pSymbol, const dword index) = 0;
    virtual bool __stdcall RemoveSymbol(const dword index) = 0;
    virtual bool __stdcall GetSymbolRef(IPointSymbol** ppSymbol, const dword index) const = 0;
    virtual bool __stdcall SetSymbolOrder(const IPointSymbol* pSymbol, const dword neworder) = 0;
    virtual dword __stdcall GetSymbolCount() const = 0;
    virtual void __stdcall ClearSymbols() = 0;

    //缩放比例
    virtual bool __stdcall SetSize(const double size) = 0;
    virtual bool __stdcall GetSize(double& size) const = 0;
};

//多线符号（平行线）
class IMultiLineSymbol : public ILineSymbol
{
public:
    virtual bool __stdcall AddSymbol(const ILineSymbol* pSymbol) = 0;
    virtual bool __stdcall AddSimpleSymbol(
        const COLORREF color    = RGB(0, 0, 200),
        const double linewidth  = 0.5
        ) = 0;
    virtual bool __stdcall SetSymbolRef(const ILineSymbol* pSymbol, const dword index) = 0;
    virtual bool __stdcall RemoveSymbol(const dword index) = 0;
    virtual bool __stdcall GetSymbolRef(ILineSymbol** ppSymbol, const dword index) const = 0;
    virtual bool __stdcall SetSymbolOrder(const ILineSymbol* pSymbol, const dword neworder) = 0;
    virtual dword __stdcall GetSymbolCount() const = 0;
    virtual void __stdcall ClearSymbols() = 0;

    //缩放比例
    virtual bool __stdcall SetSize(const double size) = 0;
    virtual bool __stdcall GetSize(double& size) const = 0;
};

//多面符号
class IMultiFillSymbol : public IFillSymbol
{
public:
    virtual bool __stdcall AddSymbol(const IFillSymbol* pSymbol) = 0;
    virtual bool __stdcall AddSimpleSymbol(const COLORREF color = RGB(0, 0, 200)) = 0;
    virtual bool __stdcall SetSymbolRef(const IFillSymbol* pSymbol, const dword index) = 0;
    virtual bool __stdcall RemoveSymbol(const dword index) = 0;
    virtual bool __stdcall GetSymbolRef(IFillSymbol** ppSymbol, const dword index) const = 0;
    virtual bool __stdcall SetSymbolOrder(const IFillSymbol* pSymbol, const dword neworder) = 0;
    virtual dword __stdcall GetSymbolCount() const = 0;
    virtual void __stdcall ClearSymbols() = 0;
};

class ISymbolLib : public IPersist
{
    virtual bool __stdcall AddSymbol(const ISymbol* pSymbol) = 0;
    virtual bool __stdcall SetSymbolRef(const ISymbol* pSymbol, const dword index) = 0;
    virtual bool __stdcall GetSymbolRef(ISymbol** const ppSymbol, const dword index) = 0;
    virtual bool __stdcall SetSymbolOrder(const ISymbol* pSymbol, const dword neworder) = 0;
    virtual bool __stdcall RemoveSymbol(const dword index) = 0;
    virtual bool __stdcall ClearSymbols() = 0;
    virtual dword __stdcall GetSymbolCount() = 0;
    virtual bool __stdcall SetDesc(const char* const desc) = 0;
    virtual const char* const __stdcall GetDesc() const = 0;
    virtual bool __stdcall SaveToFile(const char* const filename) const = 0;
    virtual bool __stdcall LoadFromFile(const char* const filename) = 0;
};

}

#endif