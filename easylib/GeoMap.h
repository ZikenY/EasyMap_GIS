#if !defined(GEOMAP_INCLUDED_)
#define GEOMAP_INCLUDED_

#include "cMap.h"
#include "ActiveView.h"
#include "..\\include\\InterfaceLabelLayer.h"


namespace easymap
{

class CGeoMap;
typedef TSmartPtr<CGeoMap> CGeoMapPtr;

//================================================================================
//  Map的实现
//================================================================================
class CGeoMap : public CMap,
                public CActiveView,
                public ILabelLayerManager,
                public IRapidDraw,
                public IPlaceBookmark
{
private:
    class CMapCancel : public ITrackCancel
    {
        CLASS_NAME(CMapCancel)
        PERSIST_DUMP(CMapCancel)
        NO_EVENTS_DISPATCHER
        NO_EVENTS_LISTENER

        //这里要避免循环引用
        CGeoMap* m_pRaw;
        bool m_HintVisible;
        CMapCancel(CGeoMap* const pMap);
        ~CMapCancel();
        bool __stdcall GotoInterface(const char* const interfacename, void** pp);
        dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
        dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);
        bool __stdcall Clone(IObj** ppObj) const {return false;};
        bool CheckCancel() const;
        void PostProgress(const long cacheid) const;
        void ShowHint(const long cacheid) const;
    friend class CGeoMap;
    };

    //用于辅助实现标注避让
    struct LabelRect
    {
        string text;
        WKSPoint labelpoint;
        tagRECT labelenvelope;
    };

    //书签
    struct Bookmark
    {
        WKSRect extent;
        string text;
        long id;
    };

CLASS_NAME(CGeoMap)
PERSIST_DUMP(CGeoMap)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CGeoMap();
private:
    ~CGeoMap();

private:
    //－－－－－－－－－－－－－－－－－－－
    //  序列化时候要保存的
    //－－－－－－－－－－－－－－－－－－－

    //用于实现Map、Activeview
    //--------------------------------------
    CDisplayPtr         m_pDisplay;
    long                m_SelectCacheID;    //用于绘制选择集的cacheid，m_pDisplay中
    string              m_MapName;
    string              m_SR;
    list<ILayerPtr>     m_Layers;           //图层列表
    //--------------------------------------

    //图层标注
    //--------------------------------------
    IGroupLayerPtr      m_LabelLayers;      //用于保存一组LabelLayer
    long                m_LabelTextAvoidable;
    //--------------------------------------

    //快速刷新图层
    //--------------------------------------
    IGroupLayerPtr      m_pRapidDraw;       //和m_LabelLayers类似
    //--------------------------------------

    //书签
    //--------------------------------------
    vector<Bookmark>    m_Bookmarks;
    long                m_MaxBookmarkID;
    long                m_CurrentBookmarkIndex;
    ITextSymbolPtr      m_BookmarkSymbol;
    COLORREF            m_BookmarkNoActiveColor;
    COLORREF            m_BookmarkActiveColor;
    BookMarkStyle       m_BookMarkStyle;
    BookMarkVisible     m_BookMarkVisible;
    //--------------------------------------

    //－－－－－－－－－－－－－－－－－－－


    //－－－－－－－－－－－－－－－－－－－
    //  序列化时不保存的东东
    //－－－－－－－－－－－－－－－－－－－

    bool                m_Focused;
    CMapCancel*         m_pRawCancel;
    vector<string>      m_UndoProcs;
    long                m_UndoIndex;

    //－－－－－－－－－－－－－－－－－－－

private:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    void DrawBookmarks(long cacheid) const;

    //--实现CMap的部分---------
    bool AddLayer(const ILayerPtr pLayer);
    bool __stdcall DeleteLayer(const dword index);
    bool DeleteLayerEx(ILayerPtr pLayer);
    bool GetLayer(ILayerPtr& pLayer, const dword index) const;
    bool SetLayerOrder(const ILayerPtr pLayer, const dword neworder);
    void __stdcall ClearLayers();
    dword __stdcall GetLayerCount() const;
    dword __stdcall GetAllCount() const;
    bool __stdcall FindLayer(ILayer** ppLayer, const char* const layername,
        const char* const classtype) const;
    void __stdcall ClearAllData();
    bool __stdcall GetReferenceScale(double& refscale) const;
    const char* __stdcall GetSpatialReference() const;
    bool __stdcall GetMapScale(double& scale) const;
    bool __stdcall GetVisibleExtent(WKSRect& extent) const;
    bool __stdcall GetFullExtent(WKSRect& fullext) const;
    bool SetSelectSymbol(const ISymbolPtr pSymbol);
    bool GetSelectSymbol(const SymbolType symboltype, ISymbolPtr& pSymbol) const;
    dword __stdcall SelectByPoint(const WKSPoint& point, const bool append);
    dword __stdcall Select(const WKSRect& envelope, const bool partialselect,
        const bool append);
    dword __stdcall DeselectByPoint(const WKSPoint& point);
    dword __stdcall Deselect(const WKSRect& envelope, const bool partialselect);
    void __stdcall ClearSelection();
    void __stdcall SetName(const char* const mapname);
    const char* __stdcall GetName() const;
    bool __stdcall SetUndoPoint(const char* const desc);
    bool __stdcall EditUndoable() const;
    bool __stdcall EditRedoable() const;
    bool __stdcall EditUndo();
    bool __stdcall EditRedo();
    bool __stdcall EditCancel();
    bool __stdcall SaveData();
    bool __stdcall IsDirty() const;
    //----------------------------

    //--实现CActiveView的部分------
    bool SetDisplay(CDisplayPtr pDisplay);
    bool GetDisplay(CDisplayPtr& pDisplay) const;
    bool __stdcall GainFocus(const HDC dc, const RECT& rect);
    bool __stdcall LostFocus();
    bool __stdcall IsFocused() const;
    DrawResult DrawData(
        CDisplayPtr             pDisplay        = NULL,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );
    DrawResult DrawSelection(
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );
    DrawResult DrawSelection(
        const CDisplayCachePtr  pDisplayCache,
        const long              cacheid,
        const WKSRect* const    pEnvelope       = NULL,
        const ITrackCancelPtr   pTrackCancel    = NULL
        );
    void __stdcall RefreshWindow();
    void __stdcall DrawingHint(const bool visible);
    //----------------------------

    //--实现ILabelLayerManager的部分------
    bool __stdcall AddLabelLayer(const ILabelLayer* pLabelLayer);
    bool __stdcall RemoveLabelLayer(const dword index);
    bool __stdcall RemoveLabelLayerEx(ILabelLayer* pLabelLayer);
    bool __stdcall GetLabelLayer(ILabelLayer** ppLabelLayer, const dword index) const;
    bool __stdcall SetLabelLayerOrder(const ILabelLayer* pLabelLayer, const dword neworder);
    void __stdcall ClearLabelLayers();
    dword __stdcall GetLabelLayerCount() const;
    void __stdcall SetTextAvoidable(const bool avoidable);
    bool __stdcall GetTextAvoidable() const;
    DrawResult __stdcall DrawLabels(const IDisplay* pDisplay, const WKSRect* const pEnvelope,
        const ITrackCancel* pTrackCancel) const;
    void __stdcall EnableLabelDraw(bool Enable);
    bool __stdcall LabelDrawEnabled() const;
    //----------------------------

    //--实现IRapidDraw的部分------
    bool __stdcall RD_AddLayer(const ILayer* pLayer);
    bool __stdcall RD_RemoveLayer(const dword index);
    bool __stdcall RD_RemoveLayerEx(ILayer* pLayer);
    bool __stdcall RD_GetLayer(ILayer** ppLayer, const dword index) const;
    bool __stdcall RD_SetLayerOrder(const ILayer* pLayer, const dword neworder);
    void __stdcall RD_ClearLayers();
    dword __stdcall RD_GetLayerCount() const;
    void __stdcall EnableRapidDraw(bool Enable);
    bool __stdcall RapidDrawEnabled() const;
    //----------------------------

    //--实现IPlaceBookmark的部分------
    long __stdcall AddBookmark(const char* const text);
    long __stdcall AddBookmarkEx(const WKSRect& extent, const char* const text);
    bool __stdcall ModifyBookmark(const long id, const WKSRect& extent,
        const char* const text);
    bool __stdcall GetBookmarkByID(const long id, WKSRect& extent,
        IAnsiString** ppText) const;
    long __stdcall GetBookmarkIDByIndex(const long index) const;
    long __stdcall GetBookmarkIndexByID(const long id) const;
    long __stdcall GetCurrentBookmarkID() const;
    bool __stdcall GetCurrentBookmark(WKSRect& extent, IAnsiString** ppText) const;
    bool __stdcall SetViewToCurrentBookmark();
    long __stdcall NextBookmark();
    long __stdcall PreviousBookmark();
    bool __stdcall DeleteBookmark(const long id);
    void __stdcall ClearBookmarks();
    dword __stdcall GetBookmarkCount() const;
    void __stdcall SetBookmarksVisible(const BookMarkVisible visible);
    BookMarkVisible __stdcall GetBookmarksVisible() const;
    bool __stdcall SetBookmarkSymbol(const ITextSymbol* pTextSymbol);
    bool __stdcall GetBookmarkSymbol(ITextSymbol** ppTextSymbol) const;
    void __stdcall SetBookmarkStyle(const BookMarkStyle style);
    BookMarkStyle __stdcall GetBookmarkStyle() const;
    void __stdcall SetBookmarkColor(const COLORREF noactive, const COLORREF active);
    void __stdcall GetBookmarkColor(COLORREF& noactive, COLORREF& active) const;
    void __stdcall DisableActiveBookmarkShow();
    dword __stdcall SaveBookmarksTo(IStreamX* pStream);
    dword __stdcall LoadBookmarksFrom(IStreamX* pStream);

friend class CMapCancel;
};
//================================================================================

CLASS_FACTORY(CGeoMap)

}

#endif