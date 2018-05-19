#include "CommonInclude.h"
#include "SymbolLib.h"
#include "MemoryStream.h"
#include "StringFuncs.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CSymbolLib)

static const char* SYMBOLLIBIDENTIFY = "YYCat SymbolLib";

CSymbolLib::CSymbolLib()
{
    INIT_REFCOUNT

    m_Desc = "今日闲烦，无心思量";
}

CSymbolLib::~CSymbolLib()
{
}

bool __stdcall CSymbolLib::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbolLib"))
        || (0 == strcmp(interfacename, "CSymbolLib")))
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

bool __stdcall CSymbolLib::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CSymbolLib* pSymbolLib = new CSymbolLib;

    pSymbolLib->m_Desc = this->m_Desc;

    list<ISymbolPtr>::const_iterator it = m_Symbols.begin();
    while (it != m_Symbols.end())
    {
        ISymbolPtr pSymSource = *it;
        IObjPtr pObj;
        CLONE_PTR(pSymSource, pObj)
        ISymbolPtr pSymbol;
        CAST_PTR(pObj, pSymbol, ISymbol)
        pSymbolLib->AddSymbol(pSymbol);
        it++;
    }

    return pSymbolLib->GotoInterface("IObj", (void**)ppObj);
}

dword __stdcall CSymbolLib::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    string header = SYMBOLLIBIDENTIFY;
    ps->Write(header);
    ps->Write(m_Desc);

    dword count = m_Symbols.size();
    ps->Write(count);
    list<ISymbolPtr>::const_iterator it = m_Symbols.begin();
    for (dword i = 0; i < count; i++)
    {
        (*it)->_DumpTo(pStream, assist);
        it++;
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CSymbolLib::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    string header;
    ps->Read(header);
    ps->Read(m_Desc);

    m_Symbols.clear();
    dword count;
    ps->Read(count);
    for (dword i = 0; i < count; i++)
    {
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom(ps, pPersist, assist);
        ISymbolPtr pSymbol;
        CAST_PTR(pPersist, pSymbol, ISymbol)
        m_Symbols.push_back(pSymbol);
    }


    return pStream->GetPos() - oldpos;
}

bool __stdcall CSymbolLib::AddSymbol(const ISymbol* pSymbol)
{
    if (_invalid(pSymbol)) return false;
    ISymbolPtr pp = (ISymbol*)pSymbol;
    return this->AddSymbol(pp);
}

bool __stdcall CSymbolLib::SetSymbolRef(const ISymbol* pSymbol, const dword index)
{
    if (_invalid(pSymbol)) return false;
    ISymbolPtr pp = (ISymbol*)pSymbol;
    return this->SetSymbolRef(pp, index);
}

bool __stdcall CSymbolLib::GetSymbolRef(ISymbol** const ppSymbol, const dword index)
{
    if (_invalid(ppSymbol)) return false;
    assert(!*ppSymbol);

    ISymbolPtr pp;
    this->GetSymbolRef(pp, index);
    if (!pp.Assigned()) return false;
    *ppSymbol = pp._p();
    (*ppSymbol)->_AddRef();
    return true;
}

bool __stdcall CSymbolLib::SetSymbolOrder(const ISymbol* pSymbol, const dword neworder)
{
    if (_invalid(pSymbol)) return false;
    ISymbolPtr pp = (ISymbol*)pSymbol;
    return this->SetSymbolOrder(pp, neworder);
}

bool __stdcall CSymbolLib::RemoveSymbol(const dword index)
{
    if ((m_Symbols.size() <= index) || (0 > index)) {return false;}

    list<ISymbolPtr>::iterator it = m_Symbols.begin();
    for (dword i = 0; i < index; i++)
    {
        it++;
    }
    m_Symbols.erase(it);
    return true;
}

bool __stdcall CSymbolLib::ClearSymbols()
{
    m_Symbols.clear();
    return true;
}

dword __stdcall CSymbolLib::GetSymbolCount()
{
    return m_Symbols.size();
}

bool __stdcall CSymbolLib::SetDesc(const char* const desc)
{
    if (!desc)
    {
        return false;
    }

    m_Desc = desc;
    return true;
}

const char* const __stdcall CSymbolLib::GetDesc() const
{
    return m_Desc.c_str();
}

bool __stdcall CSymbolLib::SaveToFile(const char* const filename) const
{
    IStreamXPtr pStreamX = new CMemoryStream;
    this->Dump(pStreamX._p());
    return pStreamX->SaveToFile(filename);
}

bool __stdcall CSymbolLib::LoadFromFile(const char* const filename)
{
    CStreamPtr pStream = new CMemoryStream;
    if (!pStream->LoadFromFile(filename))
    {
        return false;
    }

    CPersistPtr pPersist;
    CPersist::Instantiate(pStream, pPersist);
    CSymbolLibPtr pSymbolLib;
    CAST_PTR(pPersist, pSymbolLib, CSymbolLib)

    this->ClearSymbols();
    this->m_Desc = pSymbolLib->m_Desc;
    list<ISymbolPtr>::const_iterator it = pSymbolLib->m_Symbols.begin();
    while (it != pSymbolLib->m_Symbols.end())
    {
        ISymbolPtr pSymSource = *it;
        IObjPtr pObj;
        CLONE_PTR(pSymSource, pObj)
        ISymbolPtr pSymbol;
        CAST_PTR(pObj, pSymbol, ISymbol)
        this->AddSymbol(pSymbol);
        it++;
    }

    return true;
}

bool CSymbolLib::AddSymbol(const ISymbolPtr pSymbol)
{
    if (!pSymbol.Assigned()) return false;

    m_Symbols.push_back(pSymbol);
    return true;
}

bool CSymbolLib::SetSymbolRef(const ISymbolPtr pSymbol, const dword index)
{
    if (!pSymbol.Assigned()) return false;
    if ((m_Symbols.size() <= index) || (0 > index)) {return false;}

    list<ISymbolPtr>::iterator it = m_Symbols.begin();
    std::advance(it, index);

    *it = pSymbol;
    return true;
}

bool CSymbolLib::GetSymbolRef(ISymbolPtr& pSymbol, const dword index)
{
    if ((m_Symbols.size() <= index) || (0 > index)) {return false;}

    list<ISymbolPtr>::iterator it = m_Symbols.begin();
    std::advance(it, index);

    pSymbol = *it;
    return true;
}

bool CSymbolLib::SetSymbolOrder(const ISymbolPtr pSymbol, const dword neworder)
{
    dword size = m_Symbols.size();
    if (neworder >= size) return false;

    for (dword i = 0; i < size; i++)
    {
        ISymbolPtr pSymTmp;
        this->GetSymbolRef(pSymTmp, i);
        if (pSymTmp.Compare(pSymbol))
        {
            if (neworder == i) return true;
            list<ISymbolPtr>::iterator it;
            m_Symbols.remove(pSymTmp);
            it = m_Symbols.begin();
            std::advance(it, neworder);
            m_Symbols.insert(it, pSymTmp);
            return true;
        }
    }

    return false;
}

}