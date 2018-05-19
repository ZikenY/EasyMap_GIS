#if !defined(ELEMENTLAYER_INCLUDED_)
#define ELEMENTLAYER_INCLUDED_

#include "CustomEditLayer.h"

namespace easymap
{

class CElementLayer;
typedef TSmartPtr<CElementLayer> CElementLayerPtr;
class CElement;
typedef TSmartPtr<CElement> CElementPtr;
class CGeometryElement;
typedef TSmartPtr<CGeometryElement> CGeometryElementPtr;
class CTextElement;
typedef TSmartPtr<CTextElement> CTextElementPtr;

class CElementLayer : public CCustomEditLayer
{
CLASS_NAME(CElementLayer)
PERSIST_DUMP(CElementLayer)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

private:
    //每次编辑undo的内容，由一个或多个原子操作组成
    struct EditProcess
    {
        map<dword, IElementPtr> elements;
    };

public:
    CElementLayer();
private:
    ~CElementLayer();
    void Init();

private:
    map<dword, IElementPtr> m_Elements;
    MapUnits m_MapUnit;
    double m_BaseScale;
    double m_RefScale;
    dword m_MaxID;

    vector<EditProcess> m_UndoProcs;
    long m_UndoIndex;
    bool m_SaveDirty;
    string m_XX;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);

private:
    dword PresaveInstance(CStreamPtr pStream, void* const assist) const;
    dword PreloadInstance(CStreamPtr pStream, void* const assist);

    DrawResult DrawLayerData(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );

    DrawResult DrawLayerSelection(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );

public:
    bool DrawEx(const HDC dc, const CDisplayTransformationPtr pTrans,
        const WKSRect* const pEnvelope = NULL) const;
    bool DrawSelectedEx(const HDC dc, const CDisplayTransformationPtr pTrans,
        const WKSRect* const pEnvelope = NULL) const;

public:
    bool __stdcall GetExtent(WKSRect& fullext) const;
    MapUnits __stdcall GetMapUnit() const;
    bool __stdcall GetBaseScale(double& scale) const;
    const char* __stdcall GetSpatialReference() const;

    void SetRefScale(const double& scale);
    void GetRefScale(double& scale) const;

    dword Select(const WKSPoint& point, const bool append = true);
    dword __stdcall Select(const WKSRect& envelope, const bool partialselect = true,
        const bool append = true);
    dword Deselect(const WKSPoint& point);
    dword __stdcall Deselect(const WKSRect& envelope, const bool partialselect = true);
    dword __stdcall GetSelectCount() const;
    void __stdcall ClearSelection();

    dword AddElement(const IElementPtr pElement);
    bool GetElement(const dword id, IElementPtr& pElement) const;
    bool SetElement(const dword id, const IElementPtr& pElement);
    bool RemoveElement(const dword id);
    bool GetSelectElements(vector<dword>& ids) const;
    void MoveSelectElements(const double& delta_x, const double& delta_y);
    bool RemoveSelectedElements();
    bool GetElementIDFromIndex(const dword index, dword& id) const;
    dword GetElementCount() const;
    void ClearElements();

    bool Identify(vector<dword>& resultids, const WKSRect& envelope,
        const bool partialselect);

public:
    bool __stdcall SetUndoPoint();
    bool __stdcall EditUndoable() const;
    bool __stdcall EditRedoable() const;
    bool __stdcall EditUndo();
    bool __stdcall EditRedo();
    bool __stdcall EditCancel();
    bool __stdcall SaveData();
    bool __stdcall IsDirty() const;
};


class CElement : public IElement
{
CLASS_NAME(CElement)
PERSIST_DUMP(CElement)

protected:
    CElement();
    ~CElement();

protected:
    IGeometryPtr        m_pGeometry;
    string              m_Text;
    double              m_ReferenceScale;
    bool                m_Selected;

public:
    DrawResult __stdcall Draw(const IDisplayCache* pDisplayCache, const long cacheid,
        const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel) const;
    DrawResult __stdcall DrawSelected(const IDisplayCache* pDisplayCache,
        const long cacheid, const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel) const;
    DrawResult __stdcall Draw1(const IDisplay* pDisplay) const;
    bool __stdcall DrawEx(const HDC dc, const IDisplayTransformation* pTrans) const;
    bool __stdcall DrawSelectedEx(const HDC dc, const IDisplayTransformation* pTrans) const;
    bool __stdcall SetGeometry(const IGeometry* pGeometry);
    void __stdcall GetGeometry(IGeometry** ppGeometry);

    virtual DrawResult Draw(const CDisplayCachePtr pDisplayCache, const long cacheid,
        const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel) const = 0;
    virtual DrawResult DrawSelected(const CDisplayCachePtr pDisplayCache, const long cacheid,
        const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel) const = 0;
    DrawResult Draw(const CDisplayPtr pDisplay) const;
    virtual bool DrawEx(const HDC dc, const CDisplayTransformationPtr pTrans) const = 0;
    virtual bool DrawSelectedEx(const HDC dc, const CDisplayTransformationPtr pTrans) const = 0;

    virtual bool __stdcall Valid() const;
    virtual bool __stdcall Move(const double delta_x, const double delta_y);
    virtual bool __stdcall GetExtent(WKSRect& extent) const;
    virtual bool __stdcall SelectTest(const WKSRect& envelope, const bool partialselect = true) const;
    void __stdcall Select();
    void __stdcall Deselect();
    bool __stdcall IsSelected() const;
    bool SetGeometry(const IGeometryPtr pGeometry);
    void GetGeometry(IGeometryPtr &pGeometry);
    void __stdcall SetText(const char* const text);
    const char* __stdcall GetText() const;
    void __stdcall SetReferenceScale(const double refscale);
    void __stdcall GetReferenceScale(double& refscale) const;
    virtual bool Clone(IObjPtr& pObj) const = 0;
};


class CGeometryElement : public CElement
{
CLASS_NAME(CGeometryElement)
PERSIST_DUMP(CGeometryElement)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CGeometryElement();
    ~CGeometryElement();

private:
    IPointSymbolPtr m_pPointSymbol;
    ILineSymbolPtr m_pLineSymbol;
    IFillSymbolPtr m_pFillSymbol;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    void Init();

public:
    DrawResult Draw(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope,
        const ITrackCancelPtr   pTrackCancel
        ) const;

    DrawResult DrawSelected(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope,
        const ITrackCancelPtr   pTrackCancel
        ) const;

    bool DrawEx(const HDC dc, const CDisplayTransformationPtr pTrans) const;
    bool DrawSelectedEx(const HDC dc, const CDisplayTransformationPtr pTrans) const;

    ElementType __stdcall GetElementType() const;
    bool __stdcall SetSymbol(const ISymbol* const pSymbol);
    bool __stdcall GetSymbol(const SymbolType symboltype, ISymbol** ppSymbol) const;
    bool SetSymbol(const ISymbolPtr pSymbol);
    bool GetSymbol(const SymbolType symboltype, ISymbolPtr& pSymbol) const;
};


class CTextElement : public CElement
{
CLASS_NAME(CTextElement)
PERSIST_DUMP(CTextElement)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CTextElement();
    ~CTextElement();

private:
    ITextSymbolPtr m_pTextSymbol;
    WKSRect m_TextExt;
    bool m_TextExtOK;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    void Init();

public:
    DrawResult Draw(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope,
        const ITrackCancelPtr   pTrackCancel
        ) const;

    DrawResult DrawSelected(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope,
        const ITrackCancelPtr   pTrackCancel
        ) const;

    bool DrawEx(const HDC dc, const CDisplayTransformationPtr pTrans) const;
    bool DrawSelectedEx(const HDC dc, const CDisplayTransformationPtr pTrans) const;

    ElementType __stdcall GetElementType() const;
    bool __stdcall Move(const double& delta_x, const double& delta_y);
    bool __stdcall GetExtent(WKSRect& extent) const;
    bool __stdcall SelectTest(const WKSRect& envelope, const bool partialselect = true) const;
    bool __stdcall SetSymbol(const ISymbol* const pSymbol);
    bool __stdcall GetSymbol(const SymbolType symboltype, ISymbol** ppSymbol) const;
    void SetTextSymbol(const ITextSymbolPtr pTextSymbol);
    bool GetTextSymbol(ITextSymbolPtr& pTextSymbol) const;
    bool GetTextExtent(WKSRect& extent) const;

    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;
};


CLASS_FACTORY(CElementLayer)
CLASS_FACTORY(CGeometryElement)
CLASS_FACTORY(CTextElement)

}

#endif
