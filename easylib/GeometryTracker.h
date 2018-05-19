#if !defined(GEOMETRYTRACKER_INCLUDED_)
#define GEOMETRYTRACKER_INCLUDED_

#include "CommonInclude.h"
#include "Display.h"
#include "..\\include\\InterfaceTracker.h"

namespace easymap
{

class CMoveTracker;
typedef TSmartPtr<CMoveTracker> CMoveTrackerPtr;

class CMoveTracker : public IMoveTracker
{
CLASS_NAME(CMoveTracker)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CMoveTracker();
protected:
    ~CMoveTracker();

private:
    list<IGeometryPtr>      m_GeoList;
    list<IGeometryPtr>::const_iterator m_It;
    POINT                   m_Start;
    POINT                   m_MoveTo;
    CDisplayPtr             m_pDisplay;
    CDisplayCachePtr        m_pCache;
    CDisplayTransformationPtr   m_pTrans;
    long                    m_CacheID;
    bool                    m_Started;
    IPointSymbolPtr         m_pPntSym;
    ILineSymbolPtr          m_pLineSym;
    IFillSymbolPtr          m_pFillSym;
    ITextSymbolPtr          m_pTextSym;

private:
    bool _Start(const long X, const long Y);
    bool _Finish();

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    bool SetDisplay(const CDisplayPtr pDisplay);
    bool __stdcall SetDisplay(const IDisplay* pDisplay);
    void GetDisplay(CDisplayPtr pDisplay) const;
    bool __stdcall GetDisplay(IDisplay** ppDisplay) const;
    bool __stdcall ClearDisplay();
    bool SetSymbol(const ISymbolPtr pSymbol);
    bool __stdcall SetSymbol(const ISymbol* pSymbol);
    bool GetSymbol(const SymbolType symtype, ISymbolPtr& pSymbol) const;
    bool __stdcall GetSymbol(const SymbolType symtype, ISymbol** ppSymbol) const;
    bool __stdcall Start(const long X, const long Y);
    bool __stdcall Finish();
    bool __stdcall Started() const;

    bool __stdcall MouseDown(const long X, const long Y);
    bool __stdcall MouseMove(const long X, const long Y);
    bool __stdcall Resize() const;
    bool __stdcall Refresh() const;

    bool AddGeometryRef(const IGeometryPtr pGeo);
    bool __stdcall AddGeometryRef(const IGeometry* pGeo);
    bool NextGeometryRef(IGeometryPtr& pGeo);
    bool __stdcall NextGeometryRef(IGeometry** ppGeo);
    void __stdcall ResetIterator();
    dword __stdcall GetGeometryCount() const;
    void __stdcall ClearGeometry();
};

CLASS_FACTORY(CMoveTracker)
}

#endif