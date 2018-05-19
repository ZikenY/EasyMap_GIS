#if !defined(SYMBOLLIB_INCLUDED_)
#define SYMBOLLIB_INCLUDED_

#include "CommonInclude.h"
#include "..\\include\\InterfaceSymbol.h"
#include "Display.h"

namespace easymap
{

class CSymbolLib;
typedef TSmartPtr<CSymbolLib> CSymbolLibPtr;

class CSymbolLib : public ISymbolLib
{
CLASS_NAME(CSymbolLib)
PERSIST_DUMP(CSymbolLib)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CSymbolLib();
private:
    ~CSymbolLib();

private:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

private:
    list<ISymbolPtr>    m_Symbols;
    string              m_Desc;

public:
    bool __stdcall AddSymbol(const ISymbol* pSymbol);
    bool __stdcall SetSymbolRef(const ISymbol* pSymbol, const dword index);
    bool __stdcall GetSymbolRef(ISymbol** const ppSymbol, const dword index);
    bool __stdcall SetSymbolOrder(const ISymbol* pSymbol, const dword neworder);
    bool __stdcall RemoveSymbol(const dword index);
    bool __stdcall ClearSymbols();
    dword __stdcall GetSymbolCount();
    bool __stdcall SetDesc(const char* const desc);
    const char* const __stdcall GetDesc() const;
    bool __stdcall SaveToFile(const char* const filename) const;
    bool __stdcall LoadFromFile(const char* const filename);

    bool AddSymbol(const ISymbolPtr pSymbol);
    bool SetSymbolRef(const ISymbolPtr pSymbol, const dword index);
    bool GetSymbolRef(ISymbolPtr& pSymbol, const dword index);
    bool SetSymbolOrder(const ISymbolPtr pSymbol, const dword neworder);
};

CLASS_FACTORY(CSymbolLib)
}

#endif