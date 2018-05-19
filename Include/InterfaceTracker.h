#if !defined(INTERFACETRACKER_INCLUDED_)
#define INTERFACETRACKER_INCLUDED_

#include "WKSInclude.h"
#include "InterfaceDisplay.h"

namespace easymap
{

class IGeometryTracker;
class IMoveTracker;
typedef TSmartPtr<IGeometryTracker> IGeometryTrackerPtr;
typedef TSmartPtr<IMoveTracker> IMoveTrackerPtr;

class IGeometryTracker : public IObj
{
public:
    virtual bool __stdcall SetDisplay(const IDisplay* pDisplay) = 0;
    virtual bool __stdcall GetDisplay(IDisplay** ppDisplay) const = 0;
    virtual bool __stdcall ClearDisplay() = 0;
    virtual bool __stdcall SetSymbol(const ISymbol* pSymbol) = 0;
    virtual bool __stdcall GetSymbol(const SymbolType symtype, ISymbol** ppSymbol) const = 0;
    virtual bool __stdcall Start(const long X, const long Y) = 0;
    virtual bool __stdcall Finish() = 0;
    virtual bool __stdcall Started() const = 0;
    virtual bool __stdcall MouseDown(const long X, const long Y) = 0;
    virtual bool __stdcall MouseMove(const long X, const long Y) = 0;
    virtual bool __stdcall Resize() const = 0;
    virtual bool __stdcall Refresh() const = 0;
};

class IMoveTracker : public IGeometryTracker
{
public:
    virtual bool __stdcall AddGeometryRef(const IGeometry* pGeo) = 0;
    virtual bool __stdcall NextGeometryRef(IGeometry** ppGeo) = 0;
    virtual void __stdcall ResetIterator() = 0;
    virtual dword __stdcall GetGeometryCount() const = 0;
    virtual void __stdcall ClearGeometry() = 0;
};

}
#endif
