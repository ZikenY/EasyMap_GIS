#if !defined(MAP_INCLUDED_)
#define MAP_INCLUDED_

#include "CommonInclude.h"
#include "..\\include\\InterfaceMap.h"
#include "Display.h"

namespace easymap
{

class CMap;

typedef TSmartPtr<CMap> CMapPtr;

//================================================================================
//  对应一幅地图，主要功能是图层管理和显示比例尺
//================================================================================
class CMap : public IMap
{
public:
    bool __stdcall AddLayer(const ILayer* pLayer);
    bool __stdcall DeleteLayerEx(ILayer* pLayer);
    bool __stdcall GetLayer(ILayer** ppLayer, const dword index) const;
    bool __stdcall SetLayerOrder(const ILayer* pLayer, const dword neworder);
    bool __stdcall SetSelectSymbol(const ISymbol* pSymbol);
    bool __stdcall GetSelectSymbol(const SymbolType symboltype, ISymbol** ppSymbol) const;

    virtual bool AddLayer(const ILayerPtr pLayer) = 0;
    virtual bool DeleteLayerEx(ILayerPtr pLayer) = 0;
    virtual bool GetLayer(ILayerPtr& pLayer, const dword index) const = 0;
    virtual bool SetLayerOrder(const ILayerPtr pLayer, const dword neworder) = 0;
    virtual bool SetSelectSymbol(const ISymbolPtr pSymbol) = 0;
    virtual bool GetSelectSymbol(const SymbolType symboltype,
        ISymbolPtr& pSymbol) const = 0;
};
//================================================================================

}

#endif