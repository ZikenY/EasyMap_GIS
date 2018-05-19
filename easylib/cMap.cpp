#include "CommonInclude.h"
#include "cMap.h"

namespace easymap
{

bool __stdcall CMap::AddLayer(const ILayer* pLayer)
{
    ILayerPtr pL = (ILayer*)pLayer;
    return this->AddLayer(pL);
}

bool __stdcall CMap::DeleteLayerEx(ILayer* pLayer)
{
    ILayerPtr pL = (ILayer*)pLayer;
    return this->DeleteLayerEx(pL);
}

bool __stdcall CMap::GetLayer(ILayer** ppLayer, const dword index) const
{
    if (_invalid(ppLayer)) return false;
    ILayerPtr pL;
    bool r = this->GetLayer(pL, index);
    *ppLayer = pL._p();
    if (_valid(*ppLayer)) (*ppLayer)->_AddRef();
    return r;
}

bool __stdcall CMap::SetLayerOrder(const ILayer* pLayer, const dword neworder)
{
    ILayerPtr pL = (ILayer*)pLayer;
    return this->SetLayerOrder(pL, neworder);
}

bool __stdcall CMap::SetSelectSymbol(const ISymbol* pSymbol)
{
    ISymbolPtr pS = (ISymbol*)pSymbol;
    return this->SetSelectSymbol(pS);
}

bool __stdcall CMap::GetSelectSymbol(const SymbolType symboltype, ISymbol** ppSymbol) const
{
    if (_invalid(ppSymbol)) return false;
    ISymbolPtr pS;
    bool r = this->GetSelectSymbol(symboltype, pS);
    *ppSymbol = pS._p();
    if (_valid(*ppSymbol)) (*ppSymbol)->_AddRef();
    return r;
}

}