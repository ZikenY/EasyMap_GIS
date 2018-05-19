#include "CommonInclude.h"
#include "GeoMap.h"
#include "GroupLayer.h"
#include "ScreenDisplay.h"
#include "SimpleSymbol.h"
#include "LabelLayer.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CGeoMap)

CGeoMap::CGeoMap()
{
    INIT_REFCOUNT

    m_pDisplay = new CScreenDisplay;
    CDisplayCachePtr pDisplayCache;
    CAST_PTR(m_pDisplay, pDisplayCache, CDisplayCache)
    long cacheid = pDisplayCache->CreateCache(RGB(255, 255, 255), 255, true);
    m_SelectCacheID = cacheid;

    ISymbolPtr pSym;
    CSimplePointSymbolPtr pPointSymbol = new CSimplePointSymbol();
    pPointSymbol->SetColor(RGB(255, 0, 0));
    pPointSymbol->SetDiameter(2.8);
    CAST_PTR(pPointSymbol, pSym, ISymbol)
    pDisplayCache->SetCacheSymbol(cacheid, pSym);

    CSimpleLineSymbolPtr pLineSymbol = new CSimpleLineSymbol;
    pLineSymbol->SetColor(RGB(255, 0, 0));
    pLineSymbol->SetWidth(1);
    CAST_PTR(pLineSymbol, pSym, ISymbol)
    pDisplayCache->SetCacheSymbol(cacheid, pSym);

    CSimpleFillSymbolPtr pFillSymbol = new CSimpleFillSymbol;
    pFillSymbol->SetColor(RGB(255, 0, 0));
    pFillSymbol->SetFillStyle(BS_HATCHED);
    pFillSymbol->SetFillHatch(HS_FDIAGONAL);
    pFillSymbol->SetBorderColor(RGB(0, 255, 0));
    pFillSymbol->SetBorderWidth(0.3);
    CAST_PTR(pFillSymbol, pSym, ISymbol)
    pDisplayCache->SetCacheSymbol(cacheid, pSym);

    CSimpleTextSymbolPtr pTextSymbol = new CSimpleTextSymbol;
    pTextSymbol->SetColor(RGB(255, 0, 0));
    CAST_PTR(pTextSymbol, pSym, ISymbol)
    pDisplayCache->SetCacheSymbol(cacheid, pSym);

    m_LabelLayers = new CGroupLayer;
    m_LabelTextAvoidable = 1;

    m_pRapidDraw = new CGroupLayer;

    m_CurrentBookmarkIndex = -1;
    m_MaxBookmarkID = -1;
    ISimpleTextSymbolPtr pSimpleTextSymbol = new CSimpleTextSymbol;
    pSimpleTextSymbol->SetColor(RGB(230, 255, 235));
    pSimpleTextSymbol->SetWidth(4.5);
    pSimpleTextSymbol->SetHeight(4.5);
    CAST_PTR(pSimpleTextSymbol, m_BookmarkSymbol, ITextSymbol)
    m_BookmarkNoActiveColor = RGB(20, 30, 185);
    m_BookmarkActiveColor = RGB(250, 113, 55);
    m_BookMarkStyle = BOOKMARKSTYLE_ELLIPSE;
    m_BookMarkVisible = BOOKMARKVISIBLE_ALL;

    m_MapName = "undefined map";
    m_Focused = false;
    m_UndoProcs.push_back("开搞");
    m_UndoIndex = 0;

    //解决循环引用的问题
    m_pRawCancel = new CMapCancel(this);
    m_pRawCancel->_AddRef();
}

CGeoMap::~CGeoMap()
{
    this->LostFocus();
    this->ClearLayers();
    m_LabelLayers->ClearLayers();
    m_pRapidDraw->ClearLayers();
    m_Bookmarks.clear();

    //解决循环引用的问题
    m_pRawCancel->_Release();
}

bool __stdcall CGeoMap::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IMap"))
        || (0 == strcmp(interfacename, "CMap")))
    {
        *pp = static_cast<CMap*>(this);
    }
    else if ((0 == strcmp(interfacename, "IActiveView"))
        || (0 == strcmp(interfacename, "CActiveView")))
    {
        *pp = static_cast<CActiveView*>(this);
    }
    else if (0 == strcmp(interfacename, "ILabelLayerManager"))
    {
        *pp = static_cast<ILabelLayerManager*>(this);
    }
    else if (0 == strcmp(interfacename, "IRapidDraw"))
    {
        *pp = static_cast<IRapidDraw*>(this);
    }
    else if (0 == strcmp(interfacename, "IPlaceBookmark"))
    {
        *pp = static_cast<IPlaceBookmark*>(this);
    }
    else
    {
        *pp = NULL;
        return false;
    }

    static_cast<IObj*>(*pp)->_AddRef();
    return true;
}

bool __stdcall CGeoMap::Clone(IObj** ppObj) const
{
    return false;
}

dword __stdcall CGeoMap::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_pDisplay->_DumpTo(pStream, assist);
    ps->Write(m_MapName);

    dword count = m_Layers.size();
    pStream->WriteData(&count, sizeof(dword));
    list<ILayerPtr>::const_iterator it = m_Layers.begin();
    while (it != m_Layers.end())
    {
        ILayerPtr pLayer = *it;
        pLayer->_DumpTo(pStream, assist);
        it++;
    }

    pStream->WriteData(&m_SelectCacheID, sizeof(long));

    //标注图层
    m_LabelLayers->_DumpTo(pStream, assist);
    ps->Write(m_LabelTextAvoidable);

    //快速刷新图层
    m_pRapidDraw->_DumpTo(pStream, assist);

    //书签
    long bookmarkcount = m_Bookmarks.size();
    ps->Write(bookmarkcount);
    vector<Bookmark>::const_iterator it_bookmarks = m_Bookmarks.begin();
    while (it_bookmarks != m_Bookmarks.end())
    {
        Bookmark bookmark = *it_bookmarks;
        ps->Write(bookmark.id);
        ps->Write(&bookmark.extent, sizeof(WKSRect));
        ps->Write(bookmark.text);
        it_bookmarks++;
    }

    ps->Write(m_MaxBookmarkID);
    ps->Write(m_CurrentBookmarkIndex);
    ps->Write(&m_BookmarkNoActiveColor, sizeof(COLORREF));
    ps->Write(&m_BookmarkActiveColor, sizeof(COLORREF));
    ps->Write(&m_BookMarkStyle, sizeof(BookMarkStyle));
    ps->Write(&m_BookMarkVisible, sizeof(BookMarkVisible));
    m_BookmarkSymbol->_DumpTo(pStream, assist);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CGeoMap::_LoadInstance(IStreamX* pStream, void* const assist)
{
    this->ClearLayers();
    m_LabelLayers->ClearLayers();
    m_pRapidDraw->ClearLayers();
    this->ClearBookmarks();

    if (m_Focused)
    {
        this->LostFocus();
    }

    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    CPersistPtr pPersist;
    ::easymap::CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pDisplay, CDisplay)

    ps->Read(m_MapName);

    dword count = 0;
    pStream->ReadData(&count, sizeof(dword));
    for (dword i = 0; i < count; i++)
    {
        ::easymap::CPersist::_InstantiateFrom(ps, pPersist, assist);
        ILayerPtr pLayer;
        CAST_PTR(pPersist, pLayer, ILayer)
        m_Layers.push_back(pLayer);
    }

    pStream->ReadData(&m_SelectCacheID, sizeof(long));

    //标注图层
    ::easymap::CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_LabelLayers, IGroupLayer)
    ps->Read(m_LabelTextAvoidable);

    //快速刷新图层
    ::easymap::CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pRapidDraw, IGroupLayer)

    //书签
    long bookmarkcount;
    ps->Read(bookmarkcount);
    while (bookmarkcount > 0)
    {
        Bookmark bookmark;
        ps->Read(bookmark.id);
        ps->Read(&bookmark.extent, sizeof(WKSRect));
        ps->Read(bookmark.text);
        m_Bookmarks.push_back(bookmark);
        bookmarkcount--;
    }

    ps->Read(m_MaxBookmarkID);
    ps->Read(m_CurrentBookmarkIndex);
    ps->Read(&m_BookmarkNoActiveColor, sizeof(COLORREF));
    ps->Read(&m_BookmarkActiveColor, sizeof(COLORREF));
    ps->Read(&m_BookMarkStyle, sizeof(BookMarkStyle));
    ps->Read(&m_BookMarkVisible, sizeof(BookMarkVisible));
    ::easymap::CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_BookmarkSymbol, ITextSymbol)

    m_UndoProcs.clear();
    m_UndoProcs.push_back("开搞");
    m_UndoIndex = 0;

    return pStream->GetPos() - oldpos;
}

bool CGeoMap::AddLayer(const ILayerPtr pLayer)
{
    if (!pLayer.Assigned())
    {
        return false;
    }

    list<ILayerPtr>::const_iterator it = m_Layers.begin();
    while (it != m_Layers.end())
    {
        if (pLayer == *it)
        {
            return false;
        }
        it++;
    }

    //同步undoindex
    IEditLayerPtr pEditLayer;
    CAST_PTR(pLayer, pEditLayer, IEditLayer)
    if (pEditLayer.Assigned())
    {
        dword i;
        dword undoprocscount = m_UndoProcs.size();
        for (i = 0; i < undoprocscount - 1; i++)
        {
            pEditLayer->SetUndoPoint();
        }

        for (i = m_UndoIndex; i < undoprocscount - m_UndoIndex - 1; i++)
        {
            pEditLayer->EditUndo();
        }
    }

    m_Layers.push_front(pLayer);

    if (1 == m_Layers.size())
    {
        //将map的坐标单位等刷新成layer的
        CDisplayTransformationPtr pDisplayTransformation;
        m_pDisplay->GetDisplayTransformation(pDisplayTransformation);
        pDisplayTransformation->SetMapUnit(pLayer->GetMapUnit());
        pDisplayTransformation->SetSpatialReference(pLayer->GetSpatialReference());
    }

    return true;
}

bool __stdcall CGeoMap::DeleteLayer(const dword index)
{
    if ((0 > index) || (m_Layers.size() <= index))
    {
        return false;
    }

    list<ILayerPtr>::iterator it = m_Layers.begin();
    for (dword i = 0; i < index; i++)
    {
        it++;
    }
    m_Layers.erase(it);

    if (m_Layers.size() == 0)
    {
        m_UndoProcs.clear();
        m_UndoProcs.push_back("开搞");
        m_UndoIndex = 0;
    }

    return true;
}

bool CGeoMap::DeleteLayerEx(ILayerPtr pLayer)
{
    dword size = m_Layers.size();
    for (dword i = 0; i < size; i++)
    {
        ILayerPtr pLyr;
        this->GetLayer(pLyr, i);
        if (pLyr.Compare(pLayer))
        {
            m_Layers.remove(pLyr);
            return true;
        }
        CGroupLayerPtr pGroupLayer;
        CAST_PTR(pLyr, pGroupLayer, CGroupLayer)
        if (pGroupLayer.Assigned())
        {
            bool r = pGroupLayer->DeleteLayerEx(pLayer);
            if (r) return true;
        }
    }

    return false;
}

bool CGeoMap::GetLayer(ILayerPtr& pLayer, const dword index) const
{
    pLayer = NULL;
    if ((0 > index) || (m_Layers.size() <= index))
    {
        return false;
    }

    list<ILayerPtr>::const_iterator it = m_Layers.begin();
    std::advance(it, index);
    pLayer = *it;
    return true;
}

bool CGeoMap::SetLayerOrder(const ILayerPtr pLayer, const dword neworder)
{
    dword size = m_Layers.size();
    if (neworder >= size) return false;

    for (dword i = 0; i < size; i++)
    {
        ILayerPtr pLyr;
        this->GetLayer(pLyr, i);
        if (pLyr.Compare(pLayer))
        {
            if (neworder == i) return true;
            list<ILayerPtr>::iterator it;
            m_Layers.remove(pLyr);
            it = m_Layers.begin();
            std::advance(it, neworder);
            m_Layers.insert(it, pLyr);
            return true;
        }
    }

    return false;
}

void __stdcall CGeoMap::ClearLayers()
{
    while (0 < this->GetLayerCount())
    {
        this->DeleteLayer(0);
    }
}

dword __stdcall CGeoMap::GetLayerCount() const
{
    return m_Layers.size();
}

dword __stdcall CGeoMap::GetAllCount() const
{
    dword all = 0;
    dword count = m_Layers.size();
    for (dword i = 0; i < count; i++)
    {
        all++;
        ILayerPtr pLayer;
        this->GetLayer(pLayer, i);
        CGroupLayerPtr pGroupLayer;
        CAST_PTR(pLayer, pGroupLayer, CGroupLayer)
        if (pGroupLayer.Assigned())
        {
            all += pGroupLayer->GetAllCount();
        }
    }

    return all;
}

bool __stdcall CGeoMap::FindLayer(ILayer** ppLayer, const char* const layername,
    const char* const classtype) const
{
    if (_invalid(ppLayer) || _invalid(layername))
    {
        return false;
    }

    string findname = layername;

    ILayerPtr pL;

    dword count = m_Layers.size();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayerTmp;
        this->GetLayer(pLayerTmp, i);
        string tempname = pLayerTmp->GetName();
        if (findname == tempname)
        {
            if ((classtype == NULL) || (string(classtype) == ""))
            {
                pL = pLayerTmp;
                break;
            }
            else
            {
                string findclasstype = classtype;
                string layerclassname = pLayerTmp->_GetClassName();
                if (findclasstype == layerclassname)
                {
                    pL = pLayerTmp;
                    break;
                }
            }
        }

        CGroupLayerPtr pGroupLayer;
        CAST_PTR(pLayerTmp, pGroupLayer, CGroupLayer)
        if (pGroupLayer.Assigned())
        {
            pGroupLayer->FindLayer(pL._ref(), layername, classtype);
        }

        if (pL.Assigned())
        {
            break;
        }
    }

    if (pL.Assigned())
    {
        *ppLayer = pL._p();
        (*ppLayer)->_AddRef();
        return true;
    }

    return false;
}

void __stdcall CGeoMap::ClearAllData()
{
    this->ClearLayers();
    this->ClearLabelLayers();
    this->RD_ClearLayers();
    this->ClearBookmarks();
}

bool __stdcall CGeoMap::GetReferenceScale(double& refscale) const
{
    CDisplayTransformationPtr pDisplayTransformation;
    m_pDisplay->GetDisplayTransformation(pDisplayTransformation);
    pDisplayTransformation->GetReferenceScale(refscale);
    return true;
}

const char* __stdcall CGeoMap::GetSpatialReference() const
{
    CDisplayTransformationPtr pDisplayTransformation;
    m_pDisplay->GetDisplayTransformation(pDisplayTransformation);
    pDisplayTransformation->GetSpatialReference((string)m_SR);
    return m_SR.c_str();
}

bool __stdcall CGeoMap::GetMapScale(double& scale) const
{
    CDisplayTransformationPtr pDisplayTransformation;
    m_pDisplay->GetDisplayTransformation(pDisplayTransformation);
    pDisplayTransformation->GetMapScale(scale);
    return true;
}

bool __stdcall CGeoMap::GetVisibleExtent(WKSRect& extent) const
{
    CDisplayTransformationPtr pDisplayTransformation;
    m_pDisplay->GetDisplayTransformation(pDisplayTransformation);
    pDisplayTransformation->GetVisibleExtent(extent);
    return true;
}

bool __stdcall CGeoMap::GetFullExtent(WKSRect& fullext) const
{
    if (1 > m_Layers.size())
    {
        return false;
    }

    bool r = false;
    //先取出第一个有效的layer范围
    list<ILayerPtr>::const_iterator it = m_Layers.begin();
    while (it != m_Layers.end())
    {
        if ((*it)->GetExtent(fullext))
        {
            it++;
            r = true;
            break;
        }
        it++;
    }

    //再和剩下的图层范围叠加
    while (it != m_Layers.end())
    {
        ILayerPtr pLayer = *it;
        it++;
        WKSRect ext;
        if (pLayer->GetExtent(ext))
        {
            r = true;
        }
        else
        {
            continue;
        }

        UpdateFullExtent(fullext, ext);
    }
    return r;
}

bool CGeoMap::SetSelectSymbol(const ISymbolPtr pSymbol)
{
    CDisplayCachePtr pDisplayCache;
    CAST_PTR(m_pDisplay, pDisplayCache, CDisplayCache)
    return pDisplayCache->SetCacheSymbol(m_SelectCacheID, pSymbol);
}

bool CGeoMap::GetSelectSymbol(const SymbolType symboltype, ISymbolPtr& pSymbol) const
{
    CDisplayCachePtr pDisplayCache;
    CAST_PTR(m_pDisplay, pDisplayCache, CDisplayCache)
    return pDisplayCache->GetCacheSymbol(m_SelectCacheID, symboltype, pSymbol);
}

dword __stdcall CGeoMap::SelectByPoint(const WKSPoint& point, const bool append)
{
    WKSRect envelope;
    envelope.left = envelope.right = point.x;
    envelope.top = envelope.bottom = point.y;
    return this->Select(envelope, true, append);
}

dword __stdcall CGeoMap::Select(const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    WKSRect env;
    memcpy(&env, &envelope, sizeof(WKSRect));
    CorrectEnvelope(env);

    dword count = 0;
    dword layercount = this->GetLayerCount();
    for (dword i = 0; i < layercount; i++)
    {
        ILayerPtr pLayer;
        this->GetLayer(pLayer, i);
        if (pLayer->GetVisible() && pLayer->GetSelectable())
        {
            count += pLayer->Select(env, partialselect, append);
        }
    }
    return count;
}

dword __stdcall CGeoMap::DeselectByPoint(const WKSPoint& point)
{
    WKSRect envelope;
    envelope.left = envelope.right = point.x;
    envelope.top = envelope.bottom = point.y;
    return this->Deselect(envelope, true);
}

dword __stdcall CGeoMap::Deselect(const WKSRect& envelope, const bool partialselect)
{
    WKSRect env;
    memcpy(&env, &envelope, sizeof(WKSRect));
    CorrectEnvelope(env);

    dword count = 0;
    dword layercount = this->GetLayerCount();
    for (dword i = 0; i < layercount; i++)
    {
        ILayerPtr pLayer;
        this->GetLayer(pLayer, i);
        if (pLayer->GetVisible() && pLayer->GetSelectable())
        {
            count += pLayer->Deselect(env, partialselect);
        }
    }
    return count;
}

void __stdcall CGeoMap::ClearSelection()
{
    dword layercount = this->GetLayerCount();
    for (dword i = 0; i < layercount; i++)
    {
        ILayerPtr pLayer;
        this->GetLayer(pLayer, i);
        pLayer->ClearSelection();
    }
}

void __stdcall CGeoMap::SetName(const char* const mapname)
{
    m_MapName = mapname;
}

const char* __stdcall CGeoMap::GetName() const
{
    return m_MapName.c_str();
}

void SetLayerUndoPoint(ILayerPtr pLayer)
{
    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            SetLayerUndoPoint(pL);
        }
        return;
    }

    IEditLayerPtr pEL;
    CAST_PTR(pLayer, pEL, IEditLayer)
    if (pEL.Assigned() && pEL->GetMapEditable())
    {
        pEL->SetUndoPoint();
    }
}

bool __stdcall CGeoMap::SetUndoPoint(const char* const desc)
{
    long listsize = m_UndoProcs.size();
    for (long i = m_UndoIndex + 1; i < listsize; i++)
    {
        m_UndoProcs.pop_back();
    }
                         
    //搞定所有图层
    dword count = this->GetLayerCount();
    for (dword j = 0; j < count; j++)
    {
        ILayerPtr pLayer;
        this->GetLayer(pLayer, j);
        SetLayerUndoPoint(pLayer);
    }

    m_UndoProcs.push_back(desc);
    m_UndoIndex++;
    return true;
}

bool __stdcall CGeoMap::EditUndoable() const
{
    return (0 < m_UndoIndex) ? true : false;
}

bool __stdcall CGeoMap::EditRedoable() const
{
    long ls = m_UndoProcs.size() - 1;
    return (ls > m_UndoIndex) ? true : false;
}

void UnReDo(ILayerPtr pLayer, const bool undo)
{
    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            UnReDo(pL, undo);
        }
        return;
    }

    IEditLayerPtr pEL;
    CAST_PTR(pLayer, pEL, IEditLayer)
    if (pEL.Assigned() && pEL->GetMapEditable())
    {
        if (undo)
        {
            pEL->EditUndo();
        }
        else
        {
            pEL->EditRedo();
        }
    }
}

bool __stdcall CGeoMap::EditUndo()
{
    if (!this->EditUndoable()) {return false;}

    string desc = m_UndoProcs[--m_UndoIndex];

    //undo所有图层
    dword count = this->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        this->GetLayer(pLayer, i);
        UnReDo(pLayer, true);
    }

    return true;
}

bool __stdcall CGeoMap::EditRedo()
{
    if (!this->EditRedoable()) {return false;}

    string desc = m_UndoProcs[++m_UndoIndex];

    //redo所有图层
    dword count = this->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        this->GetLayer(pLayer, i);
        UnReDo(pLayer, false);
    }

    return true;
}

void LayerDataUpdate(ILayerPtr pLayer, const bool save)
{
    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            LayerDataUpdate(pL, save);
        }
        return;
    }

    IEditLayerPtr pEL;
    CAST_PTR(pLayer, pEL, IEditLayer)
//    if (pEL.Assigned() && pEL->IsDirty())
    if (pEL.Assigned() && pEL->GetMapEditable())
    {
        save ? pEL->SaveData() : pEL->EditCancel();
    }

    return;
}

bool __stdcall CGeoMap::EditCancel()
{
    if (m_UndoProcs.size() <= 0) {return false;}
    //cancel所有图层
    dword count = this->GetLayerCount();
    for (dword j = 0; j < count; j++)
    {
        ILayerPtr pLayer;
        this->GetLayer(pLayer, j);
        LayerDataUpdate(pLayer, false);
    }

    m_UndoProcs.clear();
    m_UndoProcs.push_back("开搞");
    m_UndoIndex = 0;
    return true;
}

bool __stdcall CGeoMap::SaveData()
{
    this->SetUndoPoint("");
    //save所有图层
    dword count = this->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        this->GetLayer(pLayer, i);
        LayerDataUpdate(pLayer, true);
    }

    m_UndoProcs.clear();
    m_UndoProcs.push_back("savedata");
    m_UndoIndex = 0;
    return true;
}

bool GetLayerDirty(ILayerPtr pLayer)
{
    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            if (GetLayerDirty(pL))
            {
                return true;
            }
        }
        return false;
    }

    IEditLayerPtr pEL;
    CAST_PTR(pLayer, pEL, IEditLayer)
    if (pEL.Assigned() && pEL->GetMapEditable())
    {
        return pEL->IsDirty();
    }
    else
    {
        return false;
    }
}

bool __stdcall CGeoMap::IsDirty() const
{
    //检查所有图层的dirty
    dword count = this->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        this->GetLayer(pLayer, i);
        if (GetLayerDirty(pLayer))
        {
            return true;
        }
    }

    return false;
}


bool CGeoMap::SetDisplay(CDisplayPtr pDisplay)
{
    if (!pDisplay.Assigned() || (pDisplay == m_pDisplay))
    {
        return false;
    }

    m_pDisplay = pDisplay;
    return true;
}

bool CGeoMap::GetDisplay(CDisplayPtr& pDisplay) const
{
    pDisplay = m_pDisplay;
    return true;
}

bool __stdcall CGeoMap::GainFocus(const HDC dc, const RECT& rect)
{
    this->LostFocus();
    if (!m_pDisplay->SetDC(dc)) return false;
    m_pDisplay->SetRect(rect);

    CDisplayTransformationPtr pDisplayTransformation;
    m_pDisplay->GetDisplayTransformation(pDisplayTransformation);

    long logpixelx = ::GetDeviceCaps((HDC)dc, LOGPIXELSX);
    long logpixely = ::GetDeviceCaps((HDC)dc, LOGPIXELSY);
    pDisplayTransformation->SetLogPixel(logpixelx, logpixely);

    m_Focused = true;
    return true;
}

bool __stdcall CGeoMap::LostFocus()
{
    if (m_Focused)
    {
        m_Focused = false;
    }

    return true;
}

bool __stdcall CGeoMap::IsFocused() const
{
    return m_Focused;
}

DrawResult CGeoMap::DrawData(CDisplayPtr pDisplay, const WKSRect* const pEnvelope,
    const ITrackCancelPtr pTrackCancel)
{
    if (!this->IsFocused()) {return LAYERDRAW_NOREADY;}

    CDisplayPtr pDisp;
    if (pDisplay.Assigned())
    {
        pDisp = pDisplay;
    }
    else
    {
        pDisp = m_pDisplay;
    }

    ITrackCancelPtr pTC;
    if (pTrackCancel.Assigned())
    {
        pTC = pTrackCancel._p();
    }
    else
    {
        pTC = m_pRawCancel;
    }

    DrawResult r = LAYERDRAW_NORMAL;

    //为了解决闪烁问题，用一个cache来画，而不对primarydc eraseview
    COLORREF bgcolor = pDisp->GetBackgroundColor();
    IDisplayCachePtr pDisplayCache;
    CAST_PTR(pDisp, pDisplayCache, IDisplayCache)
    long originalcacheid = pDisplayCache->CreateCache(bgcolor, 255);

    COLORREF skycolor = RGB(158, 205, 231);
    CDisplayTransformationPtr pDT;
    pDisp->GetDisplayTransformation(pDT);
    double attitude;
    pDT->GetAttitude(attitude);
    HDC cache_dc;
    pDisplayCache->GetCacheDC(originalcacheid, cache_dc);

    if (attitude > 0.0001)
    {
        //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        HBRUSH brushsky = ::CreateSolidBrush(skycolor);
        HBRUSH brushsaved = (HBRUSH)::SelectObject(cache_dc, brushsky);
        tagRECT rect_wnd;
        pDT->GetDeviceRect(rect_wnd);
        ::FillRect(cache_dc, &rect_wnd, brushsky);
        ::SelectObject(cache_dc, brushsaved);
        ::DeleteObject(brushsky);
        //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        WKSRect viewextent;
        pDT->GetVisibleExtent(viewextent);
        ExtentViewExtentSlight(viewextent);
        WKSPoint pnt1(viewextent.left, viewextent.bottom);
        WKSPoint pnt2(viewextent.right, viewextent.bottom);
        WKSPoint pnt3(viewextent.right, viewextent.top);
        WKSPoint pnt4(viewextent.left, viewextent.top);
        HBRUSH brushbase = ::CreateSolidBrush(bgcolor);
        HBRUSH oldbrush = (HBRUSH)::SelectObject(cache_dc, brushbase);
        HPEN penbase = ::CreatePen(PS_NULL, 0, bgcolor);
        HPEN oldpen = (HPEN)::SelectObject(cache_dc, penbase);
        tagPOINT points_dev[4];
        pDT->Map2Device(pnt1, points_dev[0]);
        pDT->Map2Device(pnt2, points_dev[1]);
        pDT->Map2Device(pnt3, points_dev[2]);
        pDT->Map2Device(pnt4, points_dev[3]);
        ::Polygon(cache_dc, points_dev, 4);

        ::SelectObject(cache_dc, oldbrush);
        ::DeleteObject(brushbase);
        ::SelectObject(cache_dc, oldpen);
        ::DeleteObject(penbase);
        //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    }

    list<ILayerPtr>::iterator it = m_Layers.end();
    while (m_Layers.begin() != it)
    {
        ILayerPtr pLayer = *(--it);
        r = pLayer->DrawData(pDisp._p(), originalcacheid, pEnvelope, pTC._p());
        if (LAYERDRAW_TRACKCANCEL == r)
        {
            break;
        }
    }
/*
    if (attitude > 0.0001)
    {
        //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        WKSRect viewextent;
        pDT->GetVisibleExtent(viewextent);
        ExtentViewExtentSlight(viewextent);
        HPEN penbase = ::CreatePen(PS_SOLID, 8, skycolor);
        HPEN oldpen = (HPEN)::SelectObject(cache_dc, penbase);
        WKSPoint pnt1, pnt2;
        tagPOINT points_dev[2];

        pnt1.x = viewextent.left;
        pnt1.y = viewextent.bottom;
        pnt2.x = viewextent.right;
        pnt2.y = viewextent.bottom;
        pDT->Map2Device(pnt1, points_dev[0]);
        pDT->Map2Device(pnt2, points_dev[1]);
        ::MoveToEx(cache_dc, points_dev[0].x, points_dev[0].y, NULL);
        ::LineTo(cache_dc, points_dev[1].x, points_dev[1].y);

        pnt1.x = viewextent.right;
        pnt1.y = viewextent.bottom;
        pnt2.x = viewextent.right;
        pnt2.y = viewextent.top;
        pDT->Map2Device(pnt1, points_dev[0]);
        pDT->Map2Device(pnt2, points_dev[1]);
        ::MoveToEx(cache_dc, points_dev[0].x, points_dev[0].y, NULL);
        ::LineTo(cache_dc, points_dev[1].x, points_dev[1].y);

        pnt1.x = viewextent.right;
        pnt1.y = viewextent.top;
        pnt2.x = viewextent.left;
        pnt2.y = viewextent.top;
        pDT->Map2Device(pnt1, points_dev[0]);
        pDT->Map2Device(pnt2, points_dev[1]);
        ::MoveToEx(cache_dc, points_dev[0].x, points_dev[0].y, NULL);
        ::LineTo(cache_dc, points_dev[1].x, points_dev[1].y);

        pnt1.x = viewextent.left;
        pnt1.y = viewextent.top;
        pnt2.x = viewextent.left;
        pnt2.y = viewextent.bottom;
        pDT->Map2Device(pnt1, points_dev[0]);
        pDT->Map2Device(pnt2, points_dev[1]);
        ::MoveToEx(cache_dc, points_dev[0].x, points_dev[0].y, NULL);
        ::LineTo(cache_dc, points_dev[1].x, points_dev[1].y);
        ::SelectObject(cache_dc, oldpen);
        ::DeleteObject(penbase);
        //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    }
*/
    //在此更新primary dc，就不会导致闪烁
    pDisplayCache->PostCacheToPrimary(originalcacheid);
    pDisplayCache->DeleteCache(originalcacheid);

    if (r != LAYERDRAW_TRACKCANCEL)
    {
        r = this->DrawLabels(pDisp._p(), pEnvelope, pTC._p());
    }

    return r;
}

DrawResult CGeoMap::DrawSelection(CDisplayCachePtr pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope,
    const ITrackCancelPtr pTrackCancel)
{
    if (!this->IsFocused()) {return LAYERDRAW_NOREADY;}

    ITrackCancelPtr pTC;
    if (pTrackCancel.Assigned())
    {
        pTC = pTrackCancel._p();
    }
    else
    {
        pTC = m_pRawCancel;
    }

    IDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, IDisplay)

    list<ILayerPtr>::iterator it = m_Layers.end();
    while (m_Layers.begin() != it)
    {
        ILayerPtr pLayer = *(--it);
        DrawResult r = pLayer->DrawSelection(pDisplay._p(), cacheid,
            pEnvelope, pTC._p());
        if (LAYERDRAW_TRACKCANCEL == r)
        {
            return LAYERDRAW_TRACKCANCEL;
        }
    }

    return LAYERDRAW_NORMAL;
}

DrawResult CGeoMap::DrawSelection(const WKSRect* const pEnvelope,
    const ITrackCancelPtr pTrackCancel)
{
    CDisplayCachePtr pDisplayCache;
    CAST_PTR(m_pDisplay, pDisplayCache, CDisplayCache)
    return this->DrawSelection(pDisplayCache, m_SelectCacheID, pEnvelope,
        pTrackCancel);
}

void __stdcall CGeoMap::RefreshWindow()
{
    CDisplayPtr pDisplay;
    this->GetDisplay(pDisplay);
    IScreenBrowserPtr pScreenBrowser;
    CAST_PTR(pDisplay, pScreenBrowser, IScreenBrowser)
    if (pScreenBrowser->Paning())
        return;

    CDisplayCachePtr pCache;
    CAST_PTR(pDisplay, pCache, CDisplayCache)

    //创建内存DC准备绘制RapidDraw和标签
    COLORREF bgcolor = pDisplay->GetBackgroundColor();
    long originalcacheid = pCache->CreateCache(bgcolor);

    //在primary和windowdc之间隔一层
    pCache->CopyPrimaryToCache(originalcacheid, 0, 0);

    //快速刷新图层
    m_pRapidDraw->DrawData(pDisplay._p(), originalcacheid, NULL, NULL);

    //书签
    this->DrawBookmarks(originalcacheid);

    pCache->PostCacheToWindow(originalcacheid);
    pCache->DeleteCache(originalcacheid);

    //搞上selection
    pCache->PostCacheToWindow(m_SelectCacheID);
}

void __stdcall CGeoMap::DrawingHint(const bool visible)
{
    m_pRawCancel->m_HintVisible = visible;
}

void CGeoMap::DrawBookmarks(long cacheid) const
{
    if (m_BookMarkVisible == BOOKMARKVISIBLE_NONE)
        return;

    CDisplayPtr pDisplay;
    this->GetDisplay(pDisplay);
    CDisplayTransformationPtr pDT;
    pDisplay->GetDisplayTransformation(pDT);

    CDisplayCachePtr pCache;
    CAST_PTR(pDisplay, pCache, CDisplayCache)
    //创建一个临时cache，bookmark先画在这上面，测试textrect
    long cacheid_test = pCache->CreateCache();
    HDC dc, dc_test;
    pCache->GetCacheDC(cacheid_test, dc_test);
    pCache->GetCacheDC(cacheid, dc);

    HPEN pen = ::CreatePen(PS_NULL, 1, m_BookmarkNoActiveColor);
    HBRUSH brush = ::CreateSolidBrush(m_BookmarkNoActiveColor);
    HPEN oldpen = (HPEN)::SelectObject(dc, pen);
    HBRUSH oldbrush = (HBRUSH)::SelectObject(dc, brush);

    //为了使标签在3D旋转中始终坚挺，所以不直接用符号的绘制方法
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    long logpixelx, logpixely;
    pDT->GetLogPixel(logpixelx, logpixely);
    double quotiety_x = logpixelx
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double quotiety_y = logpixely
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double width, height;
    m_BookmarkSymbol->GetWidth(width);
    m_BookmarkSymbol->GetHeight(height);
    LOGFONT font1;
    m_BookmarkSymbol->GetFont(font1);
    font1.lfWidth = (long)(width * quotiety_x / 2);
    font1.lfHeight = (long)(height * quotiety_y);
    COLORREF textcolor;
    m_BookmarkSymbol->GetColor(textcolor);
    long rop2;
    m_BookmarkSymbol->GetROP2(rop2);

    HFONT font = ::CreateFontIndirect(&font1);
    HFONT fontsaved = (HFONT)::SelectObject(dc, font);
    long bkmodesaved = ::SetBkMode(dc, TRANSPARENT);
    COLORREF textcolorsaved = ::SetTextColor(dc, textcolor);
    long rop2saved = ::SetROP2(dc, rop2);
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    IPointPtr pPoint = new CPoint;
    IGeometryPtr pPosition;
    CAST_PTR(pPoint, pPosition, IGeometry)
    RECT textrect;
    long inc = -1;
    vector<Bookmark>::const_iterator it_bookmarks = m_Bookmarks.begin();
    while (it_bookmarks != m_Bookmarks.end())
    {
        inc++;
        Bookmark bookmark = *it_bookmarks;
        if ((m_BookMarkVisible == BOOKMARKVISIBLE_CURRENT) && (inc != m_CurrentBookmarkIndex))
        {
            it_bookmarks++;
            continue;
        }

        WKSPoint bookmark_point;
        GetRectCenter(bookmark.extent, bookmark_point);
        if (!pDT->PointInPlane(bookmark_point.x, bookmark_point.y))
        {
            it_bookmarks++;
            continue;
        }

        POINT bookmark_point_dev;
        pDT->Map2Device(bookmark_point, bookmark_point_dev);

        //在test_dc上绘制一下text，调整textrect大小
        pPoint->SetX(bookmark_point.x);
        pPoint->SetY(bookmark_point.y);
        m_BookmarkSymbol->SetText(bookmark.text.c_str());
        m_BookmarkSymbol->Prepare(dc_test, (IDisplayTransformation*)(pDT._p()), R2_COPYPEN);
        m_BookmarkSymbol->Draw(pPosition._p(), textrect);
        textrect.left -= 12;
        textrect.right += 12;
        textrect.top -= 15;
        textrect.bottom += 15;
        if ((textrect.right - textrect.left) < 50)
        {
            long tmp = 50 - (textrect.right - textrect.left);
            textrect.left -= tmp / 2;
            textrect.right += tmp / 2;
        }

        //外框向上偏移textrect高度，向左偏移1/3个宽度
        long test_move_x = (textrect.right - textrect.left) / 3;
        long test_move_y = textrect.bottom - textrect.top;
        textrect.top -= test_move_y;
        textrect.bottom -= test_move_y;
        textrect.left -= test_move_x;
        textrect.right -= test_move_x;

        //active bookmark
        HBRUSH activebrush;
        HBRUSH noactivebrush;
        if (bookmark.id == m_CurrentBookmarkIndex)
        {
            activebrush = ::CreateSolidBrush(m_BookmarkActiveColor);
            noactivebrush = (HBRUSH)::SelectObject(dc, activebrush);
        }

        //在cacheid上画出外框
        switch (m_BookMarkStyle)
        {
        case BOOKMARKSTYLE_ROUNDRECT:
            {
                ::RoundRect(dc, textrect.left, textrect.top, textrect.right,
                    textrect.bottom, (textrect.right - textrect.left) + (textrect.right - textrect.left) / 3,
                    (textrect.bottom - textrect.top) + (textrect.bottom - textrect.top) / 3);
            }
            break;
        case BOOKMARKSTYLE_ELLIPSE:
            {
                ::Ellipse(dc, textrect.left, textrect.top, textrect.right,
                    textrect.bottom);
            }
            break;
        default:
            {
                ::Rectangle(dc, textrect.left, textrect.top, textrect.right,
                    textrect.bottom);
            }
        }

        //绘制文本，在框子中间
        ::TextOut(dc, textrect.left + 12, textrect.top + 15, bookmark.text.c_str(), bookmark.text.size());

        //画上箭头
        POINT arrowpoints[3];
        arrowpoints[0].x = (textrect.right + textrect.left) / 2 - 10;
        arrowpoints[0].y = textrect.bottom - 10;
        arrowpoints[1].x = (textrect.right + textrect.left) / 2 + 10;
        arrowpoints[1].y = textrect.bottom - 10;
        arrowpoints[2] = bookmark_point_dev;
        ::Polygon(dc, arrowpoints, 3);

        //active bookmark
        if (bookmark.id == m_CurrentBookmarkIndex)
        {
            ::SelectObject(dc, noactivebrush);
            ::DeleteObject(activebrush);
        }

        it_bookmarks++;
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    font = (HFONT)::SelectObject(dc, fontsaved);
    ::DeleteObject(font);
    ::SetBkMode(dc, bkmodesaved);
    ::SetTextColor(dc, textcolorsaved);
    ::SetROP2(dc, rop2saved);
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ::SelectObject(dc, oldpen);
    ::SelectObject(dc, oldbrush);
    ::DeleteObject(pen);
    ::DeleteObject(brush);
    pCache->DeleteCache(cacheid_test);
}

bool __stdcall CGeoMap::AddLabelLayer(const ILabelLayer* pLabelLayer)
{
    if (!pLabelLayer)
    {
        return false;
    }

    ILabelLayerPtr pLabelLayerTmp = (ILabelLayer*)pLabelLayer;
    ILayerPtr pLayerTmp;
    CAST_PTR(pLabelLayerTmp, pLayerTmp, ILayer)
    return m_LabelLayers->AddLayer(pLayerTmp._p());
}

bool __stdcall CGeoMap::RemoveLabelLayer(const dword index)
{
    if (index >= m_LabelLayers->GetLayerCount())
    {
        return false;
    }

    return m_LabelLayers->DeleteLayer(index);
}

bool __stdcall CGeoMap::RemoveLabelLayerEx(ILabelLayer* pLabelLayer)
{
    if (!pLabelLayer)
    {
        return false;
    }

    ILabelLayerPtr pLabelLayerTmp = pLabelLayer;
    ILayerPtr pLayerTmp;
    CAST_PTR(pLabelLayerTmp, pLayerTmp, ILayer)
    return m_LabelLayers->DeleteLayerEx(pLayerTmp._p());
}

bool __stdcall CGeoMap::GetLabelLayer(ILabelLayer** ppLabelLayer, const dword index) const
{
    if (_invalid(ppLabelLayer)) return false;
    assert(!*ppLabelLayer);

    if (index >= m_LabelLayers->GetLayerCount())
    {
        return false;
    }

    ILayerPtr pLayer;
    if (!m_LabelLayers->GetLayer(pLayer._ref(), index))
    {
        return false;
    }

    ILabelLayerPtr pLabelLayer;
    CAST_PTR(pLayer, pLabelLayer, ILabelLayer)

    *ppLabelLayer = pLabelLayer._p();
    (*ppLabelLayer)->_AddRef();
    return true;
}

bool __stdcall CGeoMap::SetLabelLayerOrder(const ILabelLayer* pLabelLayer, const dword neworder)
{
    if (!pLabelLayer)
    {
        return false;
    }

    ILabelLayerPtr pLabelLayerTmp = (ILabelLayer*)pLabelLayer;
    ILayerPtr pLayer;
    CAST_PTR(pLabelLayerTmp, pLayer, ILayer)
    return m_LabelLayers->SetLayerOrder(pLayer._p(), neworder);
}

void __stdcall CGeoMap::ClearLabelLayers()
{
    m_LabelLayers->ClearLayers();
}

dword __stdcall CGeoMap::GetLabelLayerCount() const
{
    return m_LabelLayers->GetLayerCount();
}

void __stdcall CGeoMap::SetTextAvoidable(const bool avoidable)
{
    m_LabelTextAvoidable = avoidable ? 1 : 0;
}

bool __stdcall CGeoMap::GetTextAvoidable() const
{
    return (m_LabelTextAvoidable != 0);
}

inline bool CheckMapTrackCancel(const ITrackCancelPtr pTrackCancel, const long cacheid)
{
    static DWORD map_checklast = 0;
    static DWORD map_postlast = 0;
    if (!pTrackCancel.Assigned())
    {
        map_checklast = ::GetTickCount();
        map_postlast = ::GetTickCount();
        return false;
    }

    DWORD tick = ::GetTickCount();
    bool check = ((tick - map_checklast) > 200);
    bool post = ((tick - map_postlast) > 900);

    if (post)
    {
        pTrackCancel->PostProgress(cacheid);
        map_postlast = tick;
    }

    if (check)
    {
        map_checklast = tick;
        return pTrackCancel->CheckCancel();
    }
    else
    {
        return false;
    }
}

inline bool RECTsTouched(const tagRECT& rect1, const tagRECT& rect2)
{
    return (rect1.left > rect2.right
        || rect1.right < rect2.left
        || rect1.top > rect2.bottom
        || rect1.bottom < rect2.top) ? false : true;
}

DrawResult __stdcall CGeoMap::DrawLabels(const IDisplay* pDisplay, const WKSRect* const pEnvelope,
    const ITrackCancel* pTrackCancel) const
{
    if (!this->IsFocused())
    {
        return LAYERDRAW_NOREADY;
    }

    IDisplayPtr pDisp;
    if (pDisplay)
    {
        pDisp = (IDisplay*)pDisplay;
    }
    else
    {
        pDisp = m_pDisplay._p();
    }

    IDisplayTransformationPtr pDT;
    pDisp->GetDisplayTransformation(pDT._ref());
    double referencescale_saved;
    pDT->GetReferenceScale(referencescale_saved);

    WKSRect visibleextent;
    if (pEnvelope)
    {
        visibleextent = *pEnvelope;
    }
    else
    {
        pDT->GetVisibleExtent(visibleextent);
    }

    ITrackCancelPtr pTC;
    if (pTrackCancel)
    {
        pTC = (ITrackCancel*)pTrackCancel;
    }
    else
    {
        pTC = m_pRawCancel;
    }

    DrawResult r = LAYERDRAW_NORMAL;

    vector<LabelRect> labellist;

    //搞个临时dc用来计算textenvelope
    IDisplayCachePtr pDisplayCache;
    CAST_PTR(pDisp, pDisplayCache, IDisplayCache)
    long tempid = pDisplayCache->CreateCache();
    HDC dc_temp;
    pDisplayCache->GetCacheDC(tempid, dc_temp);

    //用来绘制label的cache
    long finalcacheid = pDisplayCache->CreateCache(0, 255);
    pDisplayCache->CopyPrimaryToCache(finalcacheid, 0, 0);
    HDC final_cachedc;
    pDisplayCache->GetCacheDC(finalcacheid, final_cachedc);
    IPointPtr pLabelPoint = new CPoint();

    //遍历标注层
    dword layercount = m_LabelLayers->GetLayerCount();
    for (dword layer_i = 0; layer_i < layercount; layer_i++)
    {
        ILayerPtr pLayer;
        m_LabelLayers->GetLayer(pLayer._ref(), layer_i);
        ILabelLayerPtr pLabelLayer;
        CAST_PTR(pLayer, pLabelLayer, ILabelLayer)

        //从图层中取出绘制参数
        double layerrefscale = pLabelLayer->GetRefScale();
        pDT->SetReferenceScale(layerrefscale);
        ITextSymbolPtr pTextSymbol;
        pLabelLayer->GetTextSymbol(pTextSymbol._ref());
        pTextSymbol->Prepare(final_cachedc, pDT._p(), R2_COPYPEN);

        //从图层中取出要绘制的Label以及位置信息
        IDoubleArrayPtr pTextPositions;
        IStringArrayPtr pLabelTexts;
        r = pLabelLayer->GetLabelText(pTextPositions._ref(), pLabelTexts._ref(),
            pDT._p(), pTC._p(), visibleextent);

        bool trackcancelflag = false;
        CheckMapTrackCancel(NULL, -1);

        dword position_i = 0;

        if (pTextPositions.Assigned() && pLabelTexts.Assigned())
        {
            pTC->ShowHint(finalcacheid);

            //遍历从标注层中取出的标注
            dword textcount = pLabelTexts->GetSize();
            for (dword text_i = 0; text_i < textcount; text_i++)
            {
                if ((text_i % 100) == 0)
                {
                    trackcancelflag = CheckMapTrackCancel(pTC, finalcacheid);
                    if (trackcancelflag)
                    {
                        r = LAYERDRAW_TRACKCANCEL;
                        break;
                    }
                }

                IAnsiStringPtr pLabelString;
                pLabelTexts->GetAt(text_i, pLabelString._ref());

                LabelRect labelrect;
                labelrect.text = pLabelString->GetText();

                pTextPositions->GetAt(position_i++, labelrect.labelpoint.x);
                pTextPositions->GetAt(position_i++, labelrect.labelpoint.y);

                pTextSymbol->SetText(labelrect.text.c_str());
                pLabelPoint->SetX(labelrect.labelpoint.x);
                pLabelPoint->SetY(labelrect.labelpoint.y);

                bool conflic = false;
                if (m_LabelTextAvoidable)
                {
                    //避让，用dc_temp来测试
                    pTextSymbol->Prepare(dc_temp, pDT._p(), R2_COPYPEN);
                    pTextSymbol->Draw(pLabelPoint._p(), labelrect.labelenvelope);

                    vector<LabelRect>::const_iterator it_labellist = labellist.begin();
                    while (it_labellist != labellist.end())
                    {
                        if (RECTsTouched((*it_labellist).labelenvelope, labelrect.labelenvelope))
                        {
                            conflic = true;
                            break;
                        }

                        it_labellist++;
                    }

                    //注意如果conflic == true，pTextSymbol里面就依旧是dc_temp
                    //因为也不需要绘制到final_cachedc上了
                    pTextSymbol->Prepare(final_cachedc, pDT._p(), R2_COPYPEN);
                }

                if (!conflic)
                {
                    labellist.push_back(labelrect);

                    //绘制出来
                    if (pDT->PointInPlane(labelrect.labelpoint.x, labelrect.labelpoint.y))
                    {
                        tagRECT textenv_dev;
                        pTextSymbol->Draw(pLabelPoint._p(), textenv_dev);
                    }

                }
            }
        }

        if (LAYERDRAW_TRACKCANCEL == r)
        {
            break;
        }
    }

    //恢复场景
    pDT->SetReferenceScale(referencescale_saved);
    pDisplayCache->PostCacheToPrimary(finalcacheid);
    pDisplayCache->DeleteCache(finalcacheid);
    pDisplayCache->DeleteCache(tempid);

    return r;
}

void __stdcall CGeoMap::EnableLabelDraw(bool Enable)
{
    m_LabelLayers->SetVisible(Enable);
}

bool __stdcall CGeoMap::LabelDrawEnabled() const
{
    return m_LabelLayers->GetVisible();
}

//-----------------------------------------------------------------
//  实现IRapidDraw的部分
//-----------------------------------------------------------------
bool __stdcall CGeoMap::RD_AddLayer(const ILayer* pLayer)
{
    return m_pRapidDraw->AddLayer(pLayer);
}

bool __stdcall CGeoMap::RD_RemoveLayer(const dword index)
{
    return m_pRapidDraw->DeleteLayer(index);
}

bool __stdcall CGeoMap::RD_RemoveLayerEx(ILayer* pLayer)
{
    return m_pRapidDraw->DeleteLayerEx(pLayer);
}

bool __stdcall CGeoMap::RD_GetLayer(ILayer** ppLayer, const dword index) const
{
    return m_pRapidDraw->GetLayer(ppLayer, index);
}

bool __stdcall CGeoMap::RD_SetLayerOrder(const ILayer* pLayer, const dword neworder)
{
    return m_pRapidDraw->SetLayerOrder(pLayer, neworder);
}

void __stdcall CGeoMap::RD_ClearLayers()
{
    m_pRapidDraw->ClearLayers();
}

dword __stdcall CGeoMap::RD_GetLayerCount() const
{
    return m_pRapidDraw->GetLayerCount();
}

void __stdcall CGeoMap::EnableRapidDraw(bool Enable)
{
    m_pRapidDraw->SetVisible(Enable);
}

bool __stdcall CGeoMap::RapidDrawEnabled() const
{
    return m_pRapidDraw->GetVisible();
}

//-----------------------------------------------------------------
//  实现IPlaceBookmark的部分
//-----------------------------------------------------------------
long __stdcall CGeoMap::AddBookmark(const char* const text)
{
    Bookmark bookmark;
    bookmark.text = "我是流氓我怕谁";
    bookmark.id = ++m_MaxBookmarkID;

    CDisplayTransformationPtr pDT;
    m_pDisplay->GetDisplayTransformation(pDT);
    WKSRect extent;
    pDT->GetVisibleExtent(extent);
    WKSPoint rc;
    pDT->GetRotateCenter(rc);
    MoveRectTo(extent, rc);

    bookmark.extent = extent;

    if (_valid(text) && (string(text) != ""))
    {
        bookmark.text = text;
    }

    m_Bookmarks.push_back(bookmark);
    m_CurrentBookmarkIndex = m_Bookmarks.size() - 1;
    return m_MaxBookmarkID;
}

long __stdcall CGeoMap::AddBookmarkEx(const WKSRect& extent, const char* const text)
{
    Bookmark bookmark;
    bookmark.text = "我是流氓我怕谁";
    bookmark.id = ++m_MaxBookmarkID;
    bookmark.extent = extent;

    if (_valid(text) && (string(text) != ""))
    {
        bookmark.text = text;
    }

    m_Bookmarks.push_back(bookmark);
    m_CurrentBookmarkIndex = m_Bookmarks.size() - 1;
    return m_MaxBookmarkID;
}

bool __stdcall CGeoMap::ModifyBookmark(const long id, const WKSRect& extent,
    const char* const text)
{
    long index = GetBookmarkIndexByID(id);
    if (index < 0)
        return false;

    Bookmark bookmark = m_Bookmarks[index];
    bookmark.extent = extent;
    if (_valid(text))
    {
        bookmark.text = text;
    }

    return true;
}

bool __stdcall CGeoMap::GetBookmarkByID(const long id, WKSRect& extent,
    IAnsiString** ppText) const
{
    if (_invalid(ppText))
        return false;
    assert(!*ppText);

    long index = GetBookmarkIndexByID(id);
    if (index < 0)
        return false;

    Bookmark bookmark = m_Bookmarks[index];
    extent = bookmark.extent;
    IAnsiStringPtr pAS = new CAnsiString;
    pAS->SetText(bookmark.text.c_str());
    *ppText = pAS._p();
    (*ppText)->_AddRef();
    return true;
}

long __stdcall CGeoMap::GetBookmarkIDByIndex(const long index) const
{
    if ((index < 0) || (index >= (long)m_Bookmarks.size()))
        return -1;

    Bookmark bookmark = m_Bookmarks[index];
    return bookmark.id;
}

long __stdcall CGeoMap::GetBookmarkIndexByID(const long id) const
{
    long boookmarkindex = 0;
    vector<Bookmark>::const_iterator it = m_Bookmarks.begin();
    while (it != m_Bookmarks.end())
    {
        Bookmark bookmark = *it;
        if (bookmark.id == id)
        {
            return boookmarkindex;
        }

        boookmarkindex++;
        it++;
    }

    return -1;
}

long __stdcall CGeoMap::GetCurrentBookmarkID() const
{
    if (m_CurrentBookmarkIndex < 0)
        return -1;

    Bookmark bookmark = m_Bookmarks[m_CurrentBookmarkIndex];
    return bookmark.id;
}

bool __stdcall CGeoMap::GetCurrentBookmark(WKSRect& extent, IAnsiString** ppText) const
{
    if (_invalid(ppText))
        return false;
    assert(!*ppText);

    if (m_CurrentBookmarkIndex < 0)
        return false;

    Bookmark bookmark = m_Bookmarks[m_CurrentBookmarkIndex];
    extent = bookmark.extent;
    IAnsiStringPtr pAS = new CAnsiString;
    pAS->SetText(bookmark.text.c_str());
    *ppText = pAS._p();
    (*ppText)->_AddRef();

    return true;
}

bool __stdcall CGeoMap::SetViewToCurrentBookmark()
{
    if (m_CurrentBookmarkIndex < 0)
        return false;

    Bookmark bookmark = m_Bookmarks[m_CurrentBookmarkIndex];

    CDisplayTransformationPtr pDT;
    m_pDisplay->GetDisplayTransformation(pDT);
    pDT->SetVisibleExtent(bookmark.extent);
    WKSPoint rc = GetRectCenter(bookmark.extent);
    pDT->SetRotateCenter(rc);

    return true;
}

long __stdcall CGeoMap::NextBookmark()
{
    long bookmark_max = m_Bookmarks.size() - 1;
    if (m_CurrentBookmarkIndex < bookmark_max)
    {
        return this->GetBookmarkIDByIndex(++m_CurrentBookmarkIndex);
    }

    return -1;
}

long __stdcall CGeoMap::PreviousBookmark()
{
    if ((m_CurrentBookmarkIndex > 0) && (m_Bookmarks.size() > 0))
    {
        return this->GetBookmarkIDByIndex(--m_CurrentBookmarkIndex);
    }

    return -1;
}

bool __stdcall CGeoMap::DeleteBookmark(const long id)
{
    long boookmarkindex = -1;
    vector<Bookmark>::iterator it = m_Bookmarks.begin();
    while (it != m_Bookmarks.end())
    {
        boookmarkindex++;
        Bookmark bookmark = *it;
        if (bookmark.id == id)
        {
            if (boookmarkindex == m_CurrentBookmarkIndex)
            {
                if (boookmarkindex == m_Bookmarks.size() - 1)
                {
                    if (m_CurrentBookmarkIndex == 0)
                    {
                        //只有这一个，干掉后就空了
                        m_CurrentBookmarkIndex = -1;
                    }
                    else
                    {
                        //当前指向末尾，干掉后指向前一个
                        m_CurrentBookmarkIndex--;
                    }
                }
                else
                {
                    //没到最后一个，指向下一个
                    m_CurrentBookmarkIndex++;
                }
            }

            m_Bookmarks.erase(it);
            return true;
        }

        it++;
    }

    return false;
}

void __stdcall CGeoMap::ClearBookmarks()
{
    m_Bookmarks.clear();
    m_CurrentBookmarkIndex = -1;
    m_MaxBookmarkID = -1;
}

dword __stdcall CGeoMap::GetBookmarkCount() const
{
    return m_Bookmarks.size();
}

void __stdcall CGeoMap::SetBookmarksVisible(const BookMarkVisible visible)
{
    m_BookMarkVisible = visible;
}

BookMarkVisible __stdcall CGeoMap::GetBookmarksVisible() const
{
    return m_BookMarkVisible;
}

bool __stdcall CGeoMap::SetBookmarkSymbol(const ITextSymbol* pTextSymbol)
{
    if (_invalid(pTextSymbol))
        return false;

    ITextSymbolPtr p = (ITextSymbol*)pTextSymbol;
    IObjPtr pObjTmp;
    CLONE_PTR(p, pObjTmp)
    CAST_PTR(pObjTmp, m_BookmarkSymbol, ITextSymbol)
    return true;
}

bool __stdcall CGeoMap::GetBookmarkSymbol(ITextSymbol** ppTextSymbol) const
{
    if (_invalid(ppTextSymbol))
        return false;
    assert(!*ppTextSymbol);

    if (!m_BookmarkSymbol.Assigned())
        return false;

    IObjPtr pObjTmp;
    CLONE_PTR(m_BookmarkSymbol, pObjTmp)
    ITextSymbolPtr p;
    CAST_PTR(pObjTmp, p, ITextSymbol)

    *ppTextSymbol = p._p();
    (*ppTextSymbol)->_AddRef();
    return true;
}

void __stdcall CGeoMap::SetBookmarkStyle(const BookMarkStyle style)
{
    m_BookMarkStyle = style;
}

BookMarkStyle __stdcall CGeoMap::GetBookmarkStyle() const
{
    return m_BookMarkStyle;
}

void __stdcall CGeoMap::SetBookmarkColor(const COLORREF noactive, const COLORREF active)
{
    m_BookmarkNoActiveColor = noactive;
    m_BookmarkActiveColor = active;
}

void __stdcall CGeoMap::GetBookmarkColor(COLORREF& noactive, COLORREF& active) const
{
    noactive = m_BookmarkNoActiveColor;
    active = m_BookmarkActiveColor;
}

void __stdcall CGeoMap::DisableActiveBookmarkShow()
{
    m_CurrentBookmarkIndex = -1;
}

dword __stdcall CGeoMap::SaveBookmarksTo(IStreamX* pStream)
{
    if (_invalid(pStream))
        return 0;

    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_MaxBookmarkID);
    ps->Write(m_CurrentBookmarkIndex);
    ps->Write(&m_BookmarkNoActiveColor, sizeof(COLORREF));
    ps->Write(&m_BookmarkActiveColor, sizeof(COLORREF));
    ps->Write(&m_BookMarkStyle, sizeof(BookMarkStyle));
    ps->Write(&m_BookMarkVisible, sizeof(BookMarkVisible));
    m_BookmarkSymbol->_DumpTo(pStream, NULL);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CGeoMap::LoadBookmarksFrom(IStreamX* pStream)
{
    if (_invalid(pStream))
        return 0;

    this->ClearBookmarks();

    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Read(m_MaxBookmarkID);
    ps->Read(m_CurrentBookmarkIndex);
    ps->Read(&m_BookmarkNoActiveColor, sizeof(COLORREF));
    ps->Read(&m_BookmarkActiveColor, sizeof(COLORREF));
    ps->Read(&m_BookMarkStyle, sizeof(BookMarkStyle));
    ps->Read(&m_BookMarkVisible, sizeof(BookMarkVisible));
    CPersistPtr pPersist;
    ::easymap::CPersist::_InstantiateFrom(ps, pPersist, NULL);
    CAST_PTR(pPersist, m_BookmarkSymbol, ITextSymbol)

    return pStream->GetPos() - oldpos;
}



//-----------------------------------------------------------------
//  TrackCancel的实现
//-----------------------------------------------------------------
CGeoMap::CMapCancel::CMapCancel(CGeoMap* const pMap)
{
    INIT_REFCOUNT

    //不可增加引用计数，以免造成循环引用
    m_pRaw = pMap;

    m_HintVisible = true;
}

CGeoMap::CMapCancel::~CMapCancel()
{
}

bool __stdcall CGeoMap::CMapCancel::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ITrackCancel"))
        || (0 == strcmp(interfacename, "CMapCancel")))
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

dword __stdcall CGeoMap::CMapCancel::_LoadInstance(IStreamX* pStream, void* const assist)
{
    return 0;
}

dword __stdcall CGeoMap::CMapCancel::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    return 0;
}

bool CGeoMap::CMapCancel::CheckCancel() const
{
    MSG msg;
    if (::PeekMessage(&msg, NULL, WM_KEYDOWN, WM_KEYDOWN, PM_REMOVE))
    {
        return (27 == msg.wParam);
    }

    return false;
}

void CGeoMap::CMapCancel::PostProgress(const long cacheid) const
{
    CDisplayPtr pDisplay;
    m_pRaw->GetDisplay(pDisplay);
    CDisplayCachePtr pCache;
    CAST_PTR(pDisplay, pCache, CDisplayCache)
    if (pCache.Assigned())
    {
        pCache->PostCacheToPrimary(cacheid);
    }

    pDisplay->RefreshWindow1();
    this->ShowHint(cacheid);
}

void CGeoMap::CMapCancel::ShowHint(const long cacheid) const
{
    if (!m_HintVisible)
    {
        return;
    }

    //在屏幕上画框提示
    HDC dc;
    RECT rect;
    CDisplayPtr pDisplay;
    m_pRaw->GetDisplay(pDisplay);
    pDisplay->GetDC(dc);
    pDisplay->GetRect(rect);
    long x = rect.right - rect.left - 70;
    long y = rect.bottom - rect.top - 15;

    HBRUSH oldbrush, brush = ::CreateSolidBrush(RGB(0, 0, 255));
    oldbrush = (HBRUSH)::SelectObject(dc, brush);
    rect.left = x - 58;
    rect.right = x + 58;
    rect.top = y - 10;
    rect.bottom = y + 10;
//    ::FillRect(dc, &rect, brush);
    ::SelectObject(dc, oldbrush);
    ::DeleteObject(brush);

    brush = ::CreateSolidBrush(RGB(255, 255, 200));
    oldbrush = (HBRUSH)::SelectObject(dc, brush);
    rect.left = x - 56;
    rect.right = x + 56;
    rect.top = y - 8;
    rect.bottom = y + 8;
    ::FillRect(dc, &rect, brush);
    ::SelectObject(dc, oldbrush);
    ::DeleteObject(brush);


    LOGFONT logfont;
    ::memset(&logfont, 0, sizeof(LOGFONT));
    ::strcpy(logfont.lfFaceName, "Lucida Console");
    logfont.lfHeight = 13;
    logfont.lfWidth = 9;
    HFONT font = ::CreateFontIndirect(&logfont);
    HFONT fontsaved = (HFONT)::SelectObject(dc, font);
    long bkmodesaved = ::SetBkMode(dc, TRANSPARENT);
    COLORREF textcolorsaved = ::SetTextColor(dc, RGB(20, 35, 42));
    long rop2saved = ::SetROP2(dc, R2_COPYPEN);
    UINT oldalign = ::GetTextAlign(dc);
    ::SetTextAlign(dc, TA_CENTER);

    const char* txt = "drawing...";
    long len = ::strlen(txt);
    ::TextOut(dc, x, y-6, txt, len);

    ::SetTextAlign(dc, oldalign);
    ::SelectObject(dc, fontsaved);
    ::DeleteObject(font);
    ::SetBkMode(dc, bkmodesaved);
    ::SetTextColor(dc, textcolorsaved);
    ::SetROP2(dc, rop2saved);
}

}
