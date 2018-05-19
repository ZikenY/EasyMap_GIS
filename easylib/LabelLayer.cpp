#include "CommonInclude.h"
#include "LabelLayer.h"
#include "GeometryLabel.h"
#include "..\\include\\Messages.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CLabelLayer)

const dword MAX_LABELCACHE_COUNT = 10000;

CLabelLayer::CLabelLayer()
{
    INIT_REFCOUNT

    m_Name              = "<LabelLayer>";
    m_Visible           = true;
    m_Alpha             = 255;
    m_MaxVisualScale    = 0;
    m_MinVisualScale    = 0;
    m_RefScale          = 0;
    m_Tag               = 0;
    m_FieldIndex        = -1;

    IObjPtr pObj;
    _FactoryManager::CreateInstance("CSimpleTextSymbol", pObj);
    CAST_PTR(pObj, m_pTextSymbol, ITextSymbol)
}

CLabelLayer::~CLabelLayer()
{
    //我是监听者
    KICKASS_LISTENER_FROM_DISPATCHERS
}

bool __stdcall CLabelLayer::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ILayer"))
        || (0 == strcmp(interfacename, "ILabelLayer"))
        || (0 == strcmp(interfacename, "CLabelLayer")))
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

dword __stdcall CLabelLayer::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_MaxVisualScale);
    ps->Write(m_MinVisualScale);
    ps->Write(m_RefScale);
    ps->Write(m_Tag);
    ps->WriteBool(m_Visible);
    ps->Write(m_Alpha);
    ps->Write(m_Name);
    ps->Write(m_FieldIndex);
    if (m_pVL.Assigned())
    {
        bool flag = true;
        ps->WriteBool(flag);
        ILayerPtr pVectorLayer;
        CAST_PTR(m_pVL, pVectorLayer, ILayer)
        pVectorLayer->_DumpTo(pStream, assist);
    }
    else
    {
        bool flag = false;
        ps->WriteBool(flag);
    }

    m_pTextSymbol->_DumpTo(pStream, assist);

    long size = m_LabelCache.size();
    ps->Write(size);
    map<dword, LabelText>::const_iterator it_Cache = m_LabelCache.begin();
    while (it_Cache != m_LabelCache.end())
    {
        ps->Write(it_Cache->first);
        ps->Write(it_Cache->second.text);
        size = it_Cache->second.labelpoints.size();
        ps->Write(size);
        for (long i = 0; i < size; i++)
        {
            ps->Write(&it_Cache->second.labelpoints[i], sizeof(WKSPoint));
        }

        it_Cache++;
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CLabelLayer::_LoadInstance(IStreamX* pStream, void* const assist)
{
    m_LabelCache.clear();
    m_FidsToSaved.clear();

    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;
    CPersistPtr pPersist;

    ps->Read(m_MaxVisualScale);
    ps->Read(m_MinVisualScale);
    ps->Read(m_RefScale);
    ps->Read(m_Tag);
    ps->ReadBool(m_Visible);
    ps->Read(m_Alpha);
    ps->Read(m_Name);
    ps->Read(m_FieldIndex);
    bool flag;
    ps->ReadBool(flag);
    if (flag)
    {
        ::easymap::CPersist::_InstantiateFrom(ps, pPersist, assist);
        CAST_PTR(pPersist, m_pVL, CVectorLayer)
    }

    ::easymap::CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pTextSymbol, ITextSymbol)

    long cachesize;
    ps->Read(cachesize);
    for (long i = 0; i < cachesize; i++)
    {
        dword fid;
        ps->Read(fid);
        LabelText labeltext;
        ps->Read(labeltext.text);
        long rectcount;
        ps->Read(rectcount);
        for (long j = 0; j < rectcount; j++)
        {
            WKSPoint labelpoint;
            ps->Read(&labelpoint, sizeof(WKSPoint));
            labeltext.labelpoints.push_back(labelpoint);
        }

        m_LabelCache[fid] = labeltext;
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CLabelLayer::DispatchMessage(const event_identity ei, const IObj* pMessage,
    const easy_variant& tag, const char* const message_description)
{
    switch (ei)
    {
    case MESSAGE_VECTORLAYER_ADDED:
    case MESSAGE_VECTORLAYER_DELETED:
    case MESSAGE_VECTORLAYER_MODIFIED:
        {
            this->RemoveCacheItemForEdit(tag.value_long);
        }
        break;

    case MESSAGE_VECTORLAYER_EDITSAVED:
        {
            this->PostCacheItemsForEdit();
        }
        break;

    case MESSAGE_VECTORLAYER_EDITCANCELED:
        {
            this->RollbackCacheItemsForEdit();
        }
        break;

    case MESSAGE_VECTORLAYER_UNDO:
    case MESSAGE_VECTORLAYER_REDO:
        {
            this->UndoRedoCacheItemsForEdit();
        }

    case MESSAGE_VECTORLAYER_INDEXRELOADED:
        {
            this->ClearCache();
        }

    default:
        break;
    }

    return true;
}

void CLabelLayer::PostCacheItemsForEdit()
{
    m_FidsToSaved.clear();
}

void CLabelLayer::RollbackCacheItemsForEdit()
{
    this->UndoRedoCacheItemsForEdit();
    m_FidsToSaved.clear();
}

void CLabelLayer::UndoRedoCacheItemsForEdit()
{
    vector<dword>::const_iterator it_FidsToSaved = m_FidsToSaved.begin();
    while (it_FidsToSaved != m_FidsToSaved.end())
    {
        map<dword, LabelText>::iterator it_LabelCache = m_LabelCache.find(*it_FidsToSaved);
        if (it_LabelCache != m_LabelCache.end())
        {
            m_LabelCache.erase(it_LabelCache);
        }

        it_FidsToSaved++;
    }
}

void CLabelLayer::RemoveCacheItemForEdit(dword fid)
{
    map<dword, LabelText>::iterator it = m_LabelCache.find(fid);
    if (it == m_LabelCache.end())
        return;

    m_LabelCache.erase(it);
    m_FidsToSaved.push_back(fid);
}

void __stdcall CLabelLayer::SetName(const char* const name)
{
    m_Name = name;
}

const char* __stdcall CLabelLayer::GetName() const
{
    return m_Name.c_str();
}

void __stdcall CLabelLayer::SetVisible(const bool visible)
{
    m_Visible = visible;
}

bool __stdcall CLabelLayer::GetVisible() const
{
    return m_Visible;
}

void __stdcall CLabelLayer::SetAlpha(const byte alpha)
{
    m_Alpha = alpha;
}

byte __stdcall CLabelLayer::GetAlpha() const
{
    return m_Alpha;
}

void __stdcall CLabelLayer::SetScaleLimit(const double maxscale, const double minscale)
{
    m_MaxVisualScale = maxscale;
    m_MinVisualScale = minscale;
}

void __stdcall CLabelLayer::GetScaleLimit(double& maxscale, double& minscale) const
{
    maxscale = m_MaxVisualScale;
    minscale = m_MinVisualScale;
}

void __stdcall CLabelLayer::SetTag(const long tag)
{
    m_Tag = tag;
}

long __stdcall CLabelLayer::GetTag() const
{
    return m_Tag;
}

void __stdcall CLabelLayer::SetSelectable(const bool selectable)
{
}

bool __stdcall CLabelLayer::GetSelectable() const
{
    return false;
}

bool __stdcall CLabelLayer::Clone(IObj** ppObj) const
{
    return false;
}

DrawResult __stdcall CLabelLayer::DrawData(const IDisplay* pDisplay, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    if (!m_Visible) return LAYERDRAW_NOVISIBLE;
    if (!m_pVL.Assigned()) return LAYERDRAW_UNEXPECTED;
    if (_invalid(pDisplay)) return LAYERDRAW_UNEXPECTED;

    CDisplayPtr pDisp = (CDisplay*)pDisplay;

    CDisplayCachePtr pDisplayCache;
    CAST_PTR(pDisp, pDisplayCache, CDisplayCache);
    long cacheid1 = cacheid;
    if (cacheid < 0)
    {
        cacheid1 = pDisplayCache->CreateCache(pDisplay->GetBackgroundColor());
        pDisplayCache->CopyPrimaryToCache(cacheid1, 0, 0);
    }

    DrawResult r = this->DoDrawData(pDisplayCache._p(), cacheid1, pEnvelope, pTrackCancel);

    if (cacheid < 0)
    {
        pDisplayCache->PostCacheToPrimary(cacheid1);
        pDisplayCache->DeleteCache(cacheid1);
    }

    return r;
}

DrawResult CLabelLayer::DoDrawData(const IDisplayCache* pDisplayCache, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    if (!m_Visible) return LAYERDRAW_NOVISIBLE;
    if (!m_pVL.Assigned()) return LAYERDRAW_UNEXPECTED;
    if (_invalid(pDisplayCache)) return LAYERDRAW_UNEXPECTED;
    IDisplayCachePtr pDispCache = (IDisplayCache*)pDisplayCache;
    IDisplayPtr pDisplay;
    CAST_PTR(pDispCache, pDisplay, IDisplay)
    IDisplayTransformationPtr pDT;
    pDisplay->GetDisplayTransformation(pDT._ref());
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;

    double mapscale, oldrefscale;
    pDT->GetMapScale(mapscale);

    if ((0.0001 < m_MaxVisualScale && (mapscale > m_MaxVisualScale))
        || (0.0001 < m_MinVisualScale && (mapscale < m_MinVisualScale)))
    {
        return LAYERDRAW_EXCEEDLIMIT;
    }

    pDT->GetReferenceScale(oldrefscale);
    pDT->SetReferenceScale(m_RefScale);

    long tmpid = pDispCache->CreateCache(RGB(255, 255, 255), m_Alpha);
    pDispCache->CopyCacheToCache(cacheid, tmpid, 0, 0);
    pDispCache->SetCacheSymbol(tmpid, m_pTextSymbol._p());
    if (!pDisplay->StartDraw())
    {
        pDT->SetReferenceScale(oldrefscale);
        return LAYERDRAW_DISPLAYNOREADY;
    }

    WKSRect viewextent;
    if (pEnvelope)
    {
        viewextent = *pEnvelope;
        CorrectEnvelope(viewextent);
    }
    else
    {
        pDT->GetVisibleExtent(viewextent);
    }

    vector<dword> fids;
    m_pVL->ImpreciseSearch(viewextent, fids);

    bool trackcancelflag = false;
    CheckTrackCancel(NULL, -1);

    IVectorFeaturePtr pFeature;
    for (dword i = 0; i < fids.size(); i++)
    {
        dword fid = fids[i];
        map<dword, LabelText>::const_iterator it_cache = m_LabelCache.find(fid);
        if (it_cache != m_LabelCache.end())
        {
            //fid已经在cache中
            this->DrawCacheLabel(pDispCache, tmpid, it_cache->second);
        }
        else
        {
            trackcancelflag = CheckTrackCancel(pTC, tmpid);
            if (trackcancelflag)
            {
                break;
            }

            //需要从图层中取geometry
            //一边画一边构建cache
            m_pVL->GetFeature(fid, pFeature);
            string text;
            IFieldValuePtr pFieldValue;
            if (m_FieldIndex >= 0)
            {
                pFeature->GetFieldValue(m_FieldIndex, pFieldValue._ref());
                text = FieldValue2String(pFieldValue);
            }
            else
            {
                text = IntToStr(fid);
            }
            IGeometryPtr pGeometry;
            pFeature->GetGeometryRef(pGeometry._ref());
            this->DrawGeometryLabel(pDT, pDispCache, tmpid, fid, pGeometry, text);

        }
    }

    pDisplay->FinishDraw();
    pDispCache->CopyCacheToCache(tmpid, cacheid, 0, 0);
    pDispCache->DeleteCache(tmpid);

    pDT->SetReferenceScale(oldrefscale);

    if (trackcancelflag)
    {
        return LAYERDRAW_TRACKCANCEL;
    }
    else
    {
        if (pTC.Assigned() && pTC->CheckCancel())
        {
            return LAYERDRAW_TRACKCANCEL;
        }
        else
        {
            return LAYERDRAW_NORMAL;
        }
    }
}

void CLabelLayer::DrawGeometryLabel(const IDisplayTransformationPtr pDT,
    const IDisplayCachePtr pDisplayCache, const dword cacheid,
    const dword fid, const IGeometryPtr pGeometry, const string& text)
{
    //避免cache过大
    if (m_LabelCache.size() > MAX_LABELCACHE_COUNT)
    {
        m_LabelCache.clear();
    }

    //求出label点
    CGeometryLabelPtr pGeometryLabel = new CGeometryLabel;
    pGeometryLabel->SetGeometry(pGeometry);
    vector<WKSPoint> labelpoints;
    pGeometryLabel->GetLabelPoints(labelpoints);
    vector<WKSPoint>::const_iterator it_labelpoints = labelpoints.begin();
    while (it_labelpoints != labelpoints.end())
    {
        WKSPoint labelpoint = *it_labelpoints;
        RECT textenvelope;
        pDisplayCache->DrawCacheTextXY(cacheid, labelpoint.x, labelpoint.y,
            text.c_str(), textenvelope);

        //增加到textcache中
        LabelText labeltext;
        labeltext.text = text;
        map<dword, LabelText>::const_iterator it_cache = m_LabelCache.find(fid);
        if (it_cache != m_LabelCache.end())
        {
            labeltext = it_cache->second;
        }

        labeltext.labelpoints.push_back(labelpoint);
        m_LabelCache[fid] = labeltext;
        it_labelpoints++;
    }
}

void CLabelLayer::DrawCacheLabel(const IDisplayCachePtr pDisplayCache, const dword cacheid,
    const LabelText& label)
{
    vector<WKSPoint>::const_iterator it_labelpoints = label.labelpoints.begin();
    while (it_labelpoints != label.labelpoints.end())
    {
        RECT textenvelope;
        pDisplayCache->DrawCacheTextXY(cacheid, (*it_labelpoints).x,
            (*it_labelpoints).y, label.text.c_str(), textenvelope);
        it_labelpoints++;
    }
}

void CLabelLayer::GetLabelPoints(const IPointPtr pTempPoint, const IDisplayTransformationPtr pDT,
    const dword fid, const IGeometryPtr pGeometry, const string& text, vector<WKSPoint>& textlabelpoints)
{
    //避免cache过大
    if (m_LabelCache.size() > MAX_LABELCACHE_COUNT)
    {
        m_LabelCache.clear();
    }

    //求出label点
    CGeometryLabelPtr pGeometryLabel = new CGeometryLabel;
    pGeometryLabel->SetGeometry(pGeometry);
    vector<WKSPoint> labelpoints;
    pGeometryLabel->GetLabelPoints(labelpoints);

    vector<WKSPoint>::const_iterator it_labelpoints = labelpoints.begin();
    while (it_labelpoints != labelpoints.end())
    {
        WKSPoint labelpoint = *it_labelpoints;

        pTempPoint->SetX(labelpoint.x);
        pTempPoint->SetY(labelpoint.y);

        //增加到textcache中
        LabelText labeltext;
        labeltext.text = text;
        map<dword, LabelText>::const_iterator it_cache = m_LabelCache.find(fid);
        if (it_cache != m_LabelCache.end())
        {
            labeltext = it_cache->second;
        }

        labeltext.labelpoints.push_back(labelpoint);
        m_LabelCache[fid] = labeltext;

        textlabelpoints.push_back(labelpoint);
        it_labelpoints++;
    }
}

DrawResult __stdcall CLabelLayer::DrawSelection(const IDisplay* pDisplay,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    return LAYERDRAW_NOSUPPORT;
}

bool __stdcall CLabelLayer::GetExtent(WKSRect& fullext) const
{
    if (!m_pVL.Assigned())
    {
        return false;
    }

    m_pVL->GetExtent(fullext);
    return true;
}

MapUnits __stdcall CLabelLayer::GetMapUnit() const
{
    return m_pVL->GetMapUnit();
}

bool __stdcall CLabelLayer::GetBaseScale(double& scale) const
{
    return m_pVL->GetBaseScale(scale);
}

const char* __stdcall CLabelLayer::GetSpatialReference() const
{
    return m_pVL->GetSpatialReference();
}

dword __stdcall CLabelLayer::Select(const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    return 0;
}

dword CLabelLayer::Deselect(const WKSPoint& point)
{
    return 0;
}

dword __stdcall CLabelLayer::Deselect(const WKSRect& envelope, const bool partialselect)
{
    return 0;
}

dword __stdcall CLabelLayer::GetSelectCount() const
{
    return 0;
}

void __stdcall CLabelLayer::ClearSelection()
{
}

dword CLabelLayer::Select(const WKSPoint& point, const bool append)
{
    return 0;
}

bool __stdcall CLabelLayer::SetVectorLayer(const ILayer* pLayer)
{
    m_LabelCache.clear();
    m_FidsToSaved.clear();

    if (m_pVL.Assigned())
    {
        //解除对原有vectorlayer的所有监听
        UNREGISTER_ALLEVENT_TO_DISPATCHER(m_pVL)
        m_pVL.Clear();
    }

    if (!pLayer)
    {
        return false;
    }

    ILayerPtr pLayer1 = (ILayer*)pLayer;
    CAST_PTR(pLayer1, m_pVL, CVectorLayer)
    if (m_pVL.Assigned())
    {
        //注册监听事件到这个vectorlayer
        REGISTER_EVENT_TO_DISPATCHER(MESSAGE_VECTORLAYER_ADDED, m_pVL)
        REGISTER_EVENT_TO_DISPATCHER(MESSAGE_VECTORLAYER_DELETED, m_pVL)
        REGISTER_EVENT_TO_DISPATCHER(MESSAGE_VECTORLAYER_MODIFIED, m_pVL)
        REGISTER_EVENT_TO_DISPATCHER(MESSAGE_VECTORLAYER_EDITSAVED, m_pVL)
        REGISTER_EVENT_TO_DISPATCHER(MESSAGE_VECTORLAYER_EDITCANCELED, m_pVL)
        REGISTER_EVENT_TO_DISPATCHER(MESSAGE_VECTORLAYER_UNDO, m_pVL)
        REGISTER_EVENT_TO_DISPATCHER(MESSAGE_VECTORLAYER_REDO, m_pVL)
        REGISTER_EVENT_TO_DISPATCHER(MESSAGE_VECTORLAYER_INDEXRELOADED, m_pVL)
        return true;
    }

    return false;
}


bool __stdcall CLabelLayer::GetVectorLayer(ILayer** ppLayer) const
{
    if (_invalid(ppLayer))
        return false;
    assert(!*ppLayer);

    *ppLayer = m_pVL._p();
    if (_valid(*ppLayer))
    {
        (*ppLayer)->_AddRef();
        return true;
    }

    return false;
}

bool __stdcall CLabelLayer::SetFieldIndex(const long fieldindex)
{
    if (!m_pVL.Assigned())
    {
        return false;
    }

    CFieldsPtr pFields;
    m_pVL->GetFields(pFields);
    dword fieldcount = pFields->GetFieldCount();
    if (fieldindex >= fieldcount)
    {
        return false;
    }

    m_FieldIndex = fieldindex;
    return true;
}

long __stdcall CLabelLayer::GetFieldIndex()
{
    return m_FieldIndex;
}

bool __stdcall CLabelLayer::SetTextSymbol(const ITextSymbol* pTextSymbol)
{
    if (!pTextSymbol)
    {
        return false;
    }

    ITextSymbolPtr pTextSym = (ITextSymbol*)pTextSymbol;
    IObjPtr pObjTmp;
    CLONE_PTR(pTextSym, pObjTmp)
    CAST_PTR(pObjTmp, m_pTextSymbol, ITextSymbol)

    return true;
}

bool __stdcall CLabelLayer::GetTextSymbol(ITextSymbol** ppTextSymbol) const
{
    if (_invalid(ppTextSymbol))
        return false;
    assert(!*ppTextSymbol);

    IObjPtr pObjTmp;
    CLONE_PTR(m_pTextSymbol, pObjTmp)
    ITextSymbolPtr pTextSym;
    CAST_PTR(pObjTmp, pTextSym, ITextSymbol)
    *ppTextSymbol = pTextSym._p();
    (*ppTextSymbol)->_AddRef();
    return true;
}

bool __stdcall CLabelLayer::SetRefScale(const double scale)
{
    if (scale < -0.000000001)
    {
        return false;
    }

    m_RefScale = scale;
    this->ClearCache();
    return true;
}

double __stdcall CLabelLayer::GetRefScale() const
{
    return m_RefScale;
}

DrawResult __stdcall CLabelLayer::GetLabelText(IDoubleArray** ppTextPositions, IStringArray** ppLabelTexts,
    const IDisplayTransformation* pDT, const ITrackCancel* pTrackCancel, const WKSRect& visibleextent)
{
    if (!m_Visible) return LAYERDRAW_NOVISIBLE;
    if (!m_pVL.Assigned()) return LAYERDRAW_NOREADY;
    if (_invalid(ppTextPositions)) return LAYERDRAW_UNEXPECTED;
    if (_invalid(ppLabelTexts)) return LAYERDRAW_UNEXPECTED;
    if (_invalid(pDT)) return LAYERDRAW_UNEXPECTED;

    IDisplayTransformationPtr pTrans = (IDisplayTransformation*)pDT;
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;

    assert(!*ppLabelTexts);
    assert(!*ppTextPositions);
    *ppTextPositions = NULL;
    *ppLabelTexts = NULL;

    *ppTextPositions = new CDoubleArray();
    (*ppTextPositions)->_AddRef();
    *ppLabelTexts = new CStringArray();
    (*ppLabelTexts)->_AddRef();

    IDoubleArrayPtr pTextPositions = *ppTextPositions;
    IStringArrayPtr pLabelTexts = *ppLabelTexts;

    double mapscale;
    pTrans->GetMapScale(mapscale);

    if ((0.0001 < m_MaxVisualScale && (mapscale > m_MaxVisualScale))
        || (0.0001 < m_MinVisualScale && (mapscale < m_MinVisualScale)))
    {
        return false;
    }

    vector<dword> fids;
    m_pVL->ImpreciseSearch(visibleextent, fids);

    bool trackcancelflag = false;
    CheckTrackCancel(NULL, -1);

    IVectorFeaturePtr pFeature;
    for (dword i = 0; i < fids.size(); i++)
    {
        trackcancelflag = CheckTrackCancel(pTC, -1);
        if (trackcancelflag)
        {
            break;
        }

        dword fid = fids[i];
        map<dword, LabelText>::const_iterator it_cache = m_LabelCache.find(fid);
        if (it_cache != m_LabelCache.end())
        {
            //fid已经在cache中
            vector<WKSPoint>::const_iterator it_labelpoints = it_cache->second.labelpoints.begin();
            while (it_labelpoints != it_cache->second.labelpoints.end())
            {
                pLabelTexts->Add(it_cache->second.text.c_str());
                pTextPositions->Add((*it_labelpoints).x);
                pTextPositions->Add((*it_labelpoints).y);
                it_labelpoints++;
            }
        }
        else
        {
            //需要从图层中取geometry
            //一边画一边构建cache
            m_pVL->GetFeature(fid, pFeature);
            string text;
            IFieldValuePtr pFieldValue;
            if (m_FieldIndex >= 0)
            {
                pFeature->GetFieldValue(m_FieldIndex, pFieldValue._ref());
                text = FieldValue2String(pFieldValue);
            }
            else
            {
                text = IntToStr(fid);
            }
            IGeometryPtr pGeometry;
            pFeature->GetGeometryRef(pGeometry._ref());
            IPointPtr pPointTemp = new CPoint;
            vector<WKSPoint> textenvs;
            this->GetLabelPoints(pPointTemp, pTrans, fid, pGeometry, text, textenvs);
            vector<WKSPoint>::const_iterator it_textenvs = textenvs.begin();
            while (it_textenvs != textenvs.end())
            {
                pLabelTexts->Add(text.c_str());
                pTextPositions->Add((*it_textenvs).x);
                pTextPositions->Add((*it_textenvs).y);
                it_textenvs++;
            }
        }
    }

    return trackcancelflag ? LAYERDRAW_TRACKCANCEL : LAYERDRAW_NORMAL;
}

void __stdcall CLabelLayer::ClearCache()
{
    m_LabelCache.clear();
    m_FidsToSaved.clear();
}

}
