#include "CommonInclude.h"
#include "ShapeLayer.h"
#include "ShapeAux.h"
#include "StringFuncs.h"
#include "FileMapStream.h"
#include "..\\include\\Messages.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CShapeLayer)

CShapeLayer::CShapeLayer(const string& filename, const MapUnits mapunit, const double& basescale,
    const double& precision, const long indexlevel, const bool readonly)
{
//    INIT_REFCOUNT

    this->Init(filename, mapunit, basescale, precision, indexlevel, readonly);

    //准备好等待监听的事件类型
    this->InitEventTypes();
}

CShapeLayer::CShapeLayer()
{
//    INIT_REFCOUNT

    //准备好等待监听的事件类型
    this->InitEventTypes();

    m_hSHP = INVALID_HANDLE_VALUE;
    m_hSHX = INVALID_HANDLE_VALUE;
    m_hDBF = INVALID_HANDLE_VALUE;

}

CShapeLayer::~CShapeLayer()
{
    this->UnlockFile();
}


void CShapeLayer::InitEventTypes()
{
    //提供以下事件等待监听
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_ADDED);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_DELETED);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_MODIFIED);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_EDITSAVED);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_EDITCANCELED);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_UNDO);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_REDO);
}

bool CShapeLayer::Init(const string& filename, const MapUnits mapunit, const double& basescale,
    const double& precision, const long indexlevel, const bool readonly)
{
    m_hSHP = INVALID_HANDLE_VALUE;
    m_hSHX = INVALID_HANDLE_VALUE;
    m_hDBF = INVALID_HANDLE_VALUE;

    m_ReadOnly = readonly;

    //先用currdir＋保存的相对路径
    char currdir[2000];
    ::GetCurrentDirectory(1900, currdir);
    string dir = currdir;
    m_SHPName = dir + filename;

    if (this->LockFile())
    {
        this->UnlockFile();
    }
    else
    {
        //不行的话直接使用传进来的路径
        m_SHPName = filename;
        if (this->LockFile())
        {
            this->UnlockFile();
        }
        else
        {
            //不行的话再试试当前路径
            string nodir = RemoveDirectoryPart(RemoveExtNamePart(Trim(filename))) + string(".shp");
            m_SHPName = dir + "\\" + nodir;
        }
    }

    if (!this->LockFile())
    {
        m_SHPName = "";
        m_pSlimLayer.Clear();
        this->SetName("_unknown");
        m_pFields.Clear();
        return false;
    }
    this->UnlockFile();

    m_pSlimLayer.Clear();
    bool r = ImportShapeFile(m_SHPName, mapunit, basescale, precision, indexlevel,
        -1, "", m_pSlimLayer);
    if (r)
    {
        this->LockFile();

        BY_HANDLE_FILE_INFORMATION fileinfo;
        ::GetFileInformationByHandle(m_hSHP, &fileinfo);
        m_LastTime = fileinfo.ftLastWriteTime;

        string alias = RemoveDirectoryPart(RemoveExtNamePart(m_SHPName));
        m_pSlimLayer->SetAlias(alias);
        m_pSlimLayer->SetName(alias.c_str());
        this->SetName(alias.c_str());
    }
    else
    {
        m_SHPName = "";
        m_pSlimLayer.Clear();
        this->SetName("_unknown");
        m_pFields.Clear();
    }

    if (r)
    {
        
        m_pFields = new CFields;
        string info;
        m_pSlimLayer->GetFields(info);
        Strings ss(info.c_str());
        dword linecount = ss.GetLineCount();
        for (dword i = 0; i < linecount; i++)
        {
            string fieldname;
            ss.GetLine(i, fieldname);
            string s1 = fieldname.substr(0, 4);

            FieldType fieldtype = FIELDTYPE_STRING;
            if (s1 == "##i_")
            {
                fieldtype = FIELDTYPE_LONG;
            }
            else if (s1 == "##f_")
            {
                fieldtype = FIELDTYPE_DOUBLE;
            }

            //去掉前缀
            if ((s1 == "##i_") || (s1 == "##f_") || (s1 == "##s_"))
            {
                fieldname = fieldname.substr(4, fieldname.size() - 2);
            }

            CFieldPtr pField = new CField(fieldname, fieldtype);
            m_pFields->AddField(pField);
        }
    }

    return r;
}

bool CShapeLayer::LockFile()
{
    if (INVALID_HANDLE_VALUE != m_hSHP) {return false;}

    string noext = RemoveExtNamePart(m_SHPName);
    string shxname = noext + string(".shx");
    string dbfname = noext + string(".dbf");


    DWORD CreationDisposition   = OPEN_EXISTING;
    DWORD FlagsAndAttributes    = FILE_ATTRIBUTE_READONLY | FILE_FLAG_RANDOM_ACCESS;
    DWORD DesiredAccess;
    DWORD ShareMode;

    if (m_ReadOnly)
    {
        DesiredAccess = GENERIC_READ;
        ShareMode = FILE_SHARE_READ | FILE_SHARE_WRITE;
    }
    else
    {
        DesiredAccess = GENERIC_READ | GENERIC_WRITE;
        ShareMode = FILE_SHARE_READ;
    }

    m_hSHP = ::CreateFile(m_SHPName.c_str(), DesiredAccess, ShareMode,
        NULL, CreationDisposition, FlagsAndAttributes, NULL);
    if (!m_hSHP || INVALID_HANDLE_VALUE == m_hSHP)
    {
        m_hSHP = INVALID_HANDLE_VALUE;
        return false;
    }

    m_hSHX = ::CreateFile(shxname.c_str(), DesiredAccess, ShareMode,
        NULL, CreationDisposition, FlagsAndAttributes, NULL);
    if (!m_hSHX || INVALID_HANDLE_VALUE == m_hSHX)
    {
        m_hSHX = INVALID_HANDLE_VALUE;
    }

    m_hDBF = ::CreateFile(dbfname.c_str(), DesiredAccess, ShareMode,
        NULL, CreationDisposition, FlagsAndAttributes, NULL);
    if (!m_hDBF || INVALID_HANDLE_VALUE == m_hDBF)
    {
        m_hDBF = INVALID_HANDLE_VALUE;
    }
    return true;
}

bool CShapeLayer::UnlockFile()
{
    if (INVALID_HANDLE_VALUE == m_hSHP) {return false;}

    ::CloseHandle(m_hSHP);

    if (INVALID_HANDLE_VALUE != m_hSHX)
    {
        ::CloseHandle(m_hSHX);
    }

    if (INVALID_HANDLE_VALUE != m_hDBF)
    {
        ::CloseHandle(m_hDBF);
    }

    m_hSHP = INVALID_HANDLE_VALUE;
    m_hSHX = INVALID_HANDLE_VALUE;
    m_hDBF = INVALID_HANDLE_VALUE;
    return true;
}

bool CShapeLayer::ModifiedByOther()
{
    BY_HANDLE_FILE_INFORMATION fileinfo;
    ::GetFileInformationByHandle(m_hSHP, &fileinfo);
    if ((m_LastTime.dwHighDateTime == fileinfo.ftLastWriteTime.dwHighDateTime)
        && (m_LastTime.dwLowDateTime == fileinfo.ftLastWriteTime.dwLowDateTime))
    {
        return false;
    }
    else
    {
        m_LastTime.dwHighDateTime = fileinfo.ftLastWriteTime.dwHighDateTime;
        m_LastTime.dwLowDateTime = fileinfo.ftLastWriteTime.dwLowDateTime;
        return true;
    }
}

void CShapeLayer::RereadShapeFile()
{
    if (!this->ReadOnly() || !this->ModifiedByOther()) {return;}

    GeometryColumnInfo colinfo;
    m_pSlimLayer->GetGeometryColumnInfo(colinfo);

    ISymbolPtr pSymbol;
    m_pSlimLayer->GetDefaultSymbol(pSymbol);

    double referencescale;
    m_pSlimLayer->GetRefScale(referencescale);

    string lyrname = m_pSlimLayer->GetName();
    string alias = m_pSlimLayer->GetAlias();
    /////////////////////////////////////////////

    this->UnlockFile();
    bool r = this->Init(m_SHPName, colinfo.MapUnit, colinfo.BaseScale,
        colinfo.ToleranceXY, colinfo.SIParam1, m_ReadOnly);
    if (r)
    {
        this->LockFile();
    }
    else
    {
        return;
    }

    m_pSlimLayer->SetDefaultSymbol(pSymbol, true);
    m_pSlimLayer->SetRefScale(referencescale);
    m_pSlimLayer->SetName(lyrname.c_str());
    m_pSlimLayer->SetAlias(alias);
}

bool CShapeLayer::_GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if (0 == strcmp(interfacename, "CShapeLayer"))
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

dword CShapeLayer::PresaveInstance(CStreamPtr pStream, void* const assist) const
{
    if (!m_pSlimLayer.Assigned()) {return 0;}
    IStreamX* psx = (IStreamX*)pStream._p();

    dword oldpos = pStream->GetPos();

    //存储文件名，注意这里所做的手脚
    //尽量使用相对路径
    string filename = m_SHPName;
    char currdir[2000];
    ::GetCurrentDirectory(1900, currdir);
    string dir = currdir;
    string filedir = GetDirectoryPart(filename);
    filedir = filedir.substr(0, dir.size());
    if (dir == filedir)
    {
        //可以使用相对路径
        filename = filename.substr(dir.size(), filename.size() - dir.size());
    }

    pStream->Write(filename);

    GeometryColumnInfo colinfo;
    m_pSlimLayer->GetGeometryColumnInfo(colinfo);
    pStream->Write(&colinfo.MapUnit, sizeof(MapUnits));
    pStream->Write(colinfo.BaseScale);
    pStream->Write(colinfo.ToleranceXY);
    pStream->Write(colinfo.SIParam1);
    pStream->WriteBool(m_ReadOnly);

    ISymbolPtr pSymbol;
    m_pSlimLayer->GetDefaultSymbol(pSymbol);
    pSymbol->_DumpTo(psx, assist);

    dword symbolcount = m_pSlimLayer->m_SymbolMap.size();
    pStream->Write(symbolcount);
    map<string, ISymbolPtr>::const_iterator it = m_pSlimLayer->m_SymbolMap.begin();
    while (it != m_pSlimLayer->m_SymbolMap.end())
    {
        pStream->Write(it->first);
        it->second->Dump(psx);
        it++;
    }

    SlimRendererType renderertype = m_pSlimLayer->GetRendererType();
    pStream->Write(renderertype);

    bool showdefaultsymbol = m_pSlimLayer->GetShowDefaultSymbol();
    pStream->WriteBool(showdefaultsymbol);

    long rendererfield = m_pSlimLayer->GetRendererField();
    pStream->Write(rendererfield);

    double referencescale;
    m_pSlimLayer->GetRefScale(referencescale);
    pStream->Write(referencescale);

    byte alpha = m_pSlimLayer->GetAlpha();
    pStream->Write(alpha);

    double maxscale, minscale;
    m_pSlimLayer->GetScaleLimit(maxscale, minscale);
    pStream->Write(maxscale);
    pStream->Write(minscale);

    long displayfield = m_pSlimLayer->GetDisplayField();
    pStream->Write(displayfield);

    string lyrname = m_pSlimLayer->GetName();
    pStream->Write(lyrname);

    string alias = m_pSlimLayer->GetAlias();
    pStream->Write(alias);

    return pStream->GetPos() - oldpos;
}

dword CShapeLayer::PreloadInstance(CStreamPtr pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();

    string shpname;
    pStream->Read(shpname);
    MapUnits mapunit;
    pStream->Read(&mapunit, sizeof(MapUnits));
    double precision, basescale;
    pStream->Read(basescale);
    pStream->Read(precision);
    dword indexlevel;
    pStream->Read(indexlevel);
    bool readonly;
    pStream->ReadBool(readonly);

    CPersistPtr pPersist;
    ::easymap::CPersist::_InstantiateFrom(pStream, pPersist, assist);
    ISymbolPtr pDefaultSymbol;
    CAST_PTR(pPersist, pDefaultSymbol, ISymbol)

    map<string, ISymbolPtr> symbols;
    dword symbolcount;
    pStream->Read(symbolcount);
    for (dword i = 0; i < symbolcount; i++)
    {
        string key;
        pStream->Read(key);
        pPersist.Clear();
        CPersist::Instantiate(pStream, pPersist);
        ISymbolPtr pS;
        CAST_PTR(pPersist, pS, ISymbol)
        symbols[key] = pS;
    }

    SlimRendererType renderertype;
    pStream->Read(renderertype);

    bool showdefaultsymbol;
    pStream->ReadBool(showdefaultsymbol);

    long rendererfield;
    pStream->Read(rendererfield);

    double refscale;
    pStream->Read(refscale);

    byte alpha;
    pStream->Read(alpha);

    double maxscale, minscale;
    pStream->Read(maxscale);
    pStream->Read(minscale);

    long displayfield;
    pStream->Read(displayfield);

    string lyrname;
    pStream->Read(lyrname);

    string alias;
    pStream->Read(alias);

    this->Init(shpname, mapunit, basescale, precision, indexlevel, readonly);
    if (m_pSlimLayer.Assigned())
    {
        map<string, ISymbolPtr>::const_iterator it = symbols.begin();
        while (it != symbols.end())
        {
            m_pSlimLayer->m_SymbolMap[it->first] = it->second;
            it++;
        }

        m_pSlimLayer->SetRendererType(renderertype, false);
        m_pSlimLayer->SetRendererField(rendererfield, false);
        m_pSlimLayer->SetDefaultSymbol(pDefaultSymbol, false);
        m_pSlimLayer->SetShowDefaultSymbol(showdefaultsymbol, false);
        m_pSlimLayer->SetRefScale(refscale);
        m_pSlimLayer->SetScaleLimit(maxscale, minscale);
        m_pSlimLayer->SetAlpha(alpha);
        m_pSlimLayer->SetDisplayField(displayfield);
        m_pSlimLayer->SetName(lyrname.c_str());
        m_pSlimLayer->SetAlias(alias);
    }

    return pStream->GetPos() - oldpos;
}

DrawResult CShapeLayer::DrawLayerData(const CDisplayCachePtr pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    return LAYERDRAW_NOREADY;
}

DrawResult CShapeLayer::DrawLayerSelection(const CDisplayCachePtr pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    return LAYERDRAW_NOREADY;
}

void CShapeLayer::NoticeFeatureAdded(const dword fid) const
{
    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_ADDED, fid)
}

void CShapeLayer::NoticeFeatureModified(const dword fid) const
{
    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_MODIFIED, fid)
}

DrawResult CShapeLayer::DrawData1(const CDisplayCachePtr pDisplayCache, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    if (!m_pSlimLayer.Assigned()) {return LAYERDRAW_NOREADY;}

    (const_cast<CShapeLayer*>(this))->RereadShapeFile();
    return m_pSlimLayer->DrawData1(pDisplayCache, cacheid, pEnvelope, pTrackCancel);
}

DrawResult CShapeLayer::DrawSelection(const CDisplayCachePtr pDisplayCache, const long cacheid,
    const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    if (!m_pSlimLayer.Assigned()) {return LAYERDRAW_NOREADY;}
    return m_pSlimLayer->DrawSelection(pDisplayCache, cacheid, pEnvelope, pTrackCancel);
}

bool __stdcall CShapeLayer::GetExtent(WKSRect& fullext) const
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->GetExtent(fullext);
}

MapUnits __stdcall CShapeLayer::GetMapUnit() const
{
    if (!m_pSlimLayer.Assigned()) {return UNIT_M;}

    return m_pSlimLayer->GetMapUnit();
}

bool __stdcall CShapeLayer::GetBaseScale(double& scale) const
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->GetBaseScale(scale);
}

const char* __stdcall CShapeLayer::GetSpatialReference() const
{
    if (!m_pSlimLayer.Assigned()) {return NULL;}

    return m_pSlimLayer->GetSpatialReference();
}

void CShapeLayer::GetPrecision(double& precision) const
{
    if (!m_pSlimLayer.Assigned()) {return;}

    m_pSlimLayer->GetPrecision(precision);
}

void CShapeLayer::SetRefScale(const double& scale)
{
    if (!m_pSlimLayer.Assigned()) {return;}

    m_pSlimLayer->SetRefScale(scale);
}

void CShapeLayer::GetRefScale(double& scale) const
{
    if (!m_pSlimLayer.Assigned()) {return;}

    m_pSlimLayer->GetRefScale(scale);
}

void __stdcall CShapeLayer::SetAlpha(const byte alpha)
{
    if (!m_pSlimLayer._p())
    {
        return;
    }

    m_pSlimLayer->SetAlpha(alpha);
}

byte __stdcall CShapeLayer::GetAlpha() const
{
    if (!m_pSlimLayer._p())
    {
        return false;
    }

    return m_pSlimLayer->GetAlpha();
}

dword CShapeLayer::Select(const WKSPoint& point, const bool append)
{
    if (!m_pSlimLayer.Assigned()) {return 0;}

    return m_pSlimLayer->Select(point, append);
}

dword __stdcall CShapeLayer::Select(const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    if (!m_pSlimLayer.Assigned()) {return 0;}

    return m_pSlimLayer->Select(envelope, partialselect, append);
}

dword CShapeLayer::Select(const vector<dword>& fids, const bool append)
{
    if (!m_pSlimLayer.Assigned()) {return 0;}

    return m_pSlimLayer->Select(fids, append);
}

dword CShapeLayer::Deselect(const WKSPoint& point)
{
    if (!m_pSlimLayer.Assigned()) {return 0;}

    return m_pSlimLayer->Deselect(point);
}

dword __stdcall CShapeLayer::Deselect(const WKSRect& envelope, const bool partialselect)
{
    if (!m_pSlimLayer.Assigned()) {return 0;}

    return m_pSlimLayer->Deselect(envelope, partialselect);
}

dword CShapeLayer::Deselect(const vector<dword>& fids)
{
    if (!m_pSlimLayer.Assigned()) {return 0;}

    return m_pSlimLayer->Deselect(fids);
}

dword CShapeLayer::GetSelection(vector<dword>& fids) const
{
    if (!m_pSlimLayer.Assigned()) {return 0;}

    return m_pSlimLayer->GetSelection(fids);
}

dword __stdcall CShapeLayer::GetSelectCount() const
{
    if (!m_pSlimLayer.Assigned()) {return 0;}

    return m_pSlimLayer->GetSelectCount();
}

void __stdcall CShapeLayer::ClearSelection()
{
    if (!m_pSlimLayer.Assigned()) {return;}

    m_pSlimLayer->ClearSelection();
}

bool CShapeLayer::SetDefaultSymbol(const ISymbolPtr pSymbol, const bool save2esd)
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->SetDefaultSymbol(pSymbol, save2esd);
}

bool CShapeLayer::GetDefaultSymbol(ISymbolPtr& pSymbol) const
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->GetDefaultSymbol(pSymbol);
}

void CShapeLayer::SetRendererType(const SlimRendererType renderertype, const bool save2esd)
{
    m_pSlimLayer->SetRendererType(renderertype, save2esd);
}

SlimRendererType CShapeLayer::GetRendererType() const
{
    return m_pSlimLayer->GetRendererType();
}

bool CShapeLayer::SetSymbol(const string& key, const ISymbolPtr pSymbol)
{
    return m_pSlimLayer->SetSymbol(key, pSymbol);
}

bool CShapeLayer::GetSymbol(const string& key, ISymbolPtr& pSymbol) const
{
    return m_pSlimLayer->GetSymbol(key, pSymbol);
}

bool CShapeLayer::GetSymbolByIndex(const dword index, string& key, ISymbolPtr& pSymbol) const
{
    return m_pSlimLayer->GetSymbolByIndex(index, key, pSymbol);
}

dword CShapeLayer::GetSymbolCount() const
{
    return m_pSlimLayer->GetSymbolCount();
}

void CShapeLayer::ClearSymbols()
{
    m_pSlimLayer->ClearSymbols();
}

bool CShapeLayer::SetRendererField(const long fieldindex, const bool save2esd)
{
    return m_pSlimLayer->SetRendererField(fieldindex, save2esd);
}

long CShapeLayer::GetRendererField() const
{
    return m_pSlimLayer->GetRendererField();
}

void CShapeLayer::SetShowDefaultSymbol(const bool showdefaultsymbol, const bool save2esd)
{
    m_pSlimLayer->SetShowDefaultSymbol(showdefaultsymbol, save2esd);
}

bool CShapeLayer::GetShowDefaultSymbol() const
{
    return m_pSlimLayer->GetShowDefaultSymbol();
}

bool CShapeLayer::GetFids(vector<dword>& fids) const
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->GetFids(fids);
}

dword CShapeLayer::GetFeatureCount() const
{
    if (!m_pSlimLayer.Assigned()) {return 0;}

    return m_pSlimLayer->GetFeatureCount();
}

dword CShapeLayer::AddFeature(const IGeometryPtr pGeometry, const string& fieldvalues,
    const string& annotation)
{
    if (!m_pSlimLayer.Assigned()) {return 0;}
    if (m_ReadOnly) {return false;}

    dword fid = m_pSlimLayer->AddFeature(pGeometry, fieldvalues, annotation);
    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_ADDED, fid)
    return fid;
}

bool CShapeLayer::SetFeature(const dword fid, const IGeometryPtr pGeometry,
    const string& fieldvalues, const string& annotation)
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    if (!m_pSlimLayer->SetFeature(fid, pGeometry, fieldvalues, annotation))
    {
        return false;
    }

    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_MODIFIED, fid)
    return true;
}

bool CShapeLayer::GetFeature(const dword fid, IGeometryPtr& pGeometry,
    string& fieldvalues, string& annotation)
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    (const_cast<CShapeLayer*>(this))->RereadShapeFile();

    return m_pSlimLayer->GetFeature(fid, pGeometry, fieldvalues, annotation);
}

bool CShapeLayer::GetFeatureGeometry(const dword fid, IGeometryPtr& pGeometry) const
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    (const_cast<CShapeLayer*>(this))->RereadShapeFile();

    return m_pSlimLayer->GetFeatureGeometry(fid, pGeometry);
}

bool CShapeLayer::GetFeatureMBR(const dword fid, WKSRect& mbr) const
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->GetFeatureMBR(fid, mbr);
}

bool CShapeLayer::DeleteFeature(const dword fid)
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    if (!m_pSlimLayer->DeleteFeature(fid))
    {
        return false;
    }

    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_DELETED, fid)
    return true;
}

bool CShapeLayer::CreateFeature(IVectorFeaturePtr& pFeature)
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    IVectorFeaturePtr pVF;
    bool r = m_pSlimLayer->CreateFeature(pVF);
    if (!r || !pVF.Assigned()) {return false;}

    CShapeLayerPtr pShapeLayer = const_cast<CShapeLayer*>(this);
    CSlimFeaturePtr pSLF;
    CAST_PTR(pVF, pSLF, CSlimFeature)
    CShapeFeaturePtr pSHF = new CShapeFeature(pShapeLayer, pSLF);
    CAST_PTR(pSHF, pFeature, IVectorFeature)

    return true;
}

bool CShapeLayer::GetFeature(const dword fid, IVectorFeaturePtr& pFeature)
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    IVectorFeaturePtr pVF;
    bool r = m_pSlimLayer->GetFeature(fid, pVF);
    if (!r || !pVF.Assigned()) {return false;}

    CShapeLayerPtr pShapeLayer = const_cast<CShapeLayer*>(this);
    CSlimFeaturePtr pSLF;
    CAST_PTR(pVF, pSLF, CSlimFeature)
    CShapeFeaturePtr pSHF = new CShapeFeature(pShapeLayer, pSLF);
    CAST_PTR(pSHF, pFeature, IVectorFeature)

    return true;
}

bool CShapeLayer::RapidModifyPoint(const dword fid, const WKSPoint& point)
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->RapidModifyPoint(fid, point);
}

bool CShapeLayer::Identify(vector<dword>& fids, const WKSRect& envelope,
    const bool partialselect)
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    (const_cast<CShapeLayer*>(this))->RereadShapeFile();

    return m_pSlimLayer->Identify(fids, envelope, partialselect);
}

bool CShapeLayer::ImpreciseSearch(const WKSRect& extent, vector<dword>& fids)
{
    return m_pSlimLayer->ImpreciseSearch(extent, fids);
}

bool CShapeLayer::SetFields(const string& fields)
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->SetFields(fields);
}

void CShapeLayer::GetFields(string& fields) const
{
    fields = "";
    if (!m_pSlimLayer.Assigned()) {return;}

    m_pSlimLayer->GetFields(fields);
}

void CShapeLayer::GetFields(CFieldsPtr& pFields) const
{
    pFields.Clear();
    if (!m_pFields.Assigned()) {return;}

    pFields = new CFields(*m_pFields._p());
}

bool CShapeLayer::SetDisplayField(const long fieldindex)
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->SetDisplayField(fieldindex);
}

long CShapeLayer::GetDisplayField() const
{
    if (!m_pSlimLayer.Assigned()) {return -1;}
    return m_pSlimLayer->GetDisplayField();
}

bool CShapeLayer::SetAlias(const string& alias)
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->SetAlias(alias);
}

string CShapeLayer::GetAlias() const
{
    if (!m_pSlimLayer.Assigned()) {return "";}

    return m_pSlimLayer->GetAlias();
}

void CShapeLayer::GetGeometryColumnInfo(GeometryColumnInfo& geocolinfo) const
{
    if (!m_pSlimLayer.Assigned()) {return;}

    m_pSlimLayer->GetGeometryColumnInfo(geocolinfo);
}

bool CShapeLayer::SetSR(const string& sr)
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    return m_pSlimLayer->SetSR(sr);
}

string CShapeLayer::GetShapeFileName() const
{
    if (!m_pSlimLayer.Assigned()) {return "";}

    return m_SHPName;
}

bool CShapeLayer::ReadOnly() const
{
    return m_ReadOnly;
}

void CShapeLayer::SetNetTolerance(const double tolerance)
{
    m_pSlimLayer->SetNetTolerance(tolerance);
}

double CShapeLayer::GetNetTolerance() const
{
    return m_pSlimLayer->GetNetTolerance();
}

bool CShapeLayer::CreateNetTopo(const long field, const bool bidirectional)
{
    return m_pSlimLayer->CreateNetTopo(field, bidirectional);
}

bool CShapeLayer::CreateNetTopo2(const dword field_from_to, const dword field_to_from)
{
    return m_pSlimLayer->CreateNetTopo2(field_from_to, field_to_from);
}

bool CShapeLayer::AddNetRoute(const WKSPoint& route)
{
    return m_pSlimLayer->AddNetRoute(route);
}

bool CShapeLayer::RemoveNetRoute(const WKSPoint& route)
{
    return m_pSlimLayer->RemoveNetRoute(route);
}

bool CShapeLayer::GetNetRoutes(IMultiPoint** ppRoutes) const
{
    return m_pSlimLayer->GetNetRoutes(ppRoutes);
}

void CShapeLayer::ClearNetRoutes()
{
    m_pSlimLayer->ClearNetRoutes();
}

bool CShapeLayer::AddNetBarrierPoint(const WKSPoint& barrier)
{
    return m_pSlimLayer->AddNetBarrierPoint(barrier);
}

bool CShapeLayer::RemoveNetBarrierPoint(const WKSPoint& barrier)
{
    return m_pSlimLayer->RemoveNetBarrierPoint(barrier);
}

bool CShapeLayer::GetNetBarrierPoints(IMultiPoint** ppBarriers) const
{
    return m_pSlimLayer->GetNetBarrierPoints(ppBarriers);
}

void CShapeLayer::ClearNetBarrierPoints()
{
    m_pSlimLayer->ClearNetBarrierPoints();
}

bool CShapeLayer::AddNetBlockedBiEdge(const dword fid)
{
    return m_pSlimLayer->AddNetBlockedBiEdge(fid);
}

bool CShapeLayer::AddNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to)
{
    return m_pSlimLayer->AddNetBlockedSingleEdge(from, to);
}

bool CShapeLayer::RemoveNetBlockedBiEdge(const dword fid)
{
    return m_pSlimLayer->RemoveNetBlockedBiEdge(fid);
}

bool CShapeLayer::RemoveNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to)
{
    return m_pSlimLayer->RemoveNetBlockedSingleEdge(from, to);
}

dword CShapeLayer::GetNetBlockedEdgeCount() const
{
    return m_pSlimLayer->GetNetBlockedEdgeCount();
}

bool CShapeLayer::GetNetBlockedEdgeByIndex(const dword i, dword& fid,
    WKSPoint& from, WKSPoint& to) const
{
    return m_pSlimLayer->GetNetBlockedEdgeByIndex(i, fid, from, to);
}

bool CShapeLayer::GetNetBlockedEdgeIDs(vector<dword>& fids) const
{
    return m_pSlimLayer->GetNetBlockedEdgeIDs(fids);
}

void CShapeLayer::ClearNetBlockedEdges()
{
    m_pSlimLayer->ClearNetBlockedEdges();
}

bool CShapeLayer::DoBestPath(IPath** ppPath, IIntArray** ppFids)
{
    return m_pSlimLayer->DoBestPath(ppPath, ppFids);
}

void CShapeLayer::ClearNetTopo()
{
    m_pSlimLayer->ClearNetTopo();
}

bool CShapeLayer::StoreNetTopo()
{
    IObjPtr pObj;
    _FactoryManager::CreateInstance("CMemoryStream", pObj);
    CStreamPtr pStream;
    CAST_PTR(pObj, pStream, CStream)

    net::NetTopo2Stream(m_pSlimLayer->m_Net, pStream);
    string topofilename = RemoveExtNamePart(m_SHPName) + ".topo";
    return pStream->SaveToFile(topofilename.c_str());
}

bool CShapeLayer::RestoreNetTopo()
{
    string topofilename = RemoveExtNamePart(m_SHPName) + ".topo";

    WIN32_FIND_DATA finddata;
    HANDLE hfile = ::FindFirstFile(topofilename.c_str(), &finddata);
    if (_invalid(hfile) || (INVALID_HANDLE_VALUE == hfile))
    {
        return false;
    }

    ::FindClose(hfile);

    IObjPtr pObj;
    _FactoryManager::CreateInstance("CMemoryStream", pObj);
    CStreamPtr pStream;
    CAST_PTR(pObj, pStream, CStream)

    if (!pStream->LoadFromFile(topofilename.c_str()))
    {
        return false;
    }

    net::Stream2NetTopo(pStream, m_pSlimLayer->m_Net);
    return true;
}

bool CShapeLayer::Valid() const
{
    return m_pSlimLayer.Assigned() ? true : false;
}

bool CShapeLayer::SaveData2ESD(const string& esdfilename)
{
    if (!m_pSlimLayer.Assigned()) {return false;}

    string pathpart = GetDirectoryPart(esdfilename);
    CreateDirectory(pathpart.c_str(), NULL);
    CFileMapStreamPtr pFMS = new CFileMapStream(esdfilename.c_str(), false);
    string namegot = pFMS->GetMapFileName();
    if (namegot == "") {return false;}

    CStreamPtr pStream;
    CAST_PTR(pFMS, pStream, CStream)

    pStream->MovePos(0);
    return m_pSlimLayer->GetSlimData(pStream);
}

bool __stdcall CShapeLayer::SetUndoPoint()
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    return m_pSlimLayer->SetUndoPoint();
}

bool __stdcall CShapeLayer::EditUndoable() const
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    return m_pSlimLayer->EditUndoable();
}

bool __stdcall CShapeLayer::EditRedoable() const
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    return m_pSlimLayer->EditRedoable();
}

bool __stdcall CShapeLayer::EditUndo()
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    if (m_pSlimLayer->EditUndo())
    {
        //发送消息给每个listener
        DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_UNDO, -1)
        return true;
    }

    return false;
}

bool __stdcall CShapeLayer::EditRedo()
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    if (m_pSlimLayer->EditRedo())
    {
        //发送消息给每个listener
        DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_REDO, -1)
        return true;
    }

    return false;
}

bool __stdcall CShapeLayer::EditCancel()
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    if (m_pSlimLayer->EditCancel())
    {
        //发送消息给每个listener
        DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_EDITCANCELED, -1)
        return true;
    }

    return false;
}

bool __stdcall CShapeLayer::SaveData()
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    bool r = m_pSlimLayer->SaveData();
    if (r)
    {
        //写回到shapefile中
        this->UnlockFile();
        r = ExportShapeFile(m_pSlimLayer, m_SHPName);
        this->LockFile();

        //发送消息给每个listener
        DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_EDITSAVED, -1)
        return true;
    }

    return false;
}

bool __stdcall CShapeLayer::IsDirty() const
{
    if (!m_pSlimLayer.Assigned()) {return false;}
    if (m_ReadOnly) {return false;}

    return m_pSlimLayer->IsDirty();
}


CShapeFeature::CShapeFeature(const CShapeLayerPtr pShapeLayer,
    const CSlimFeaturePtr pSlimFeature)
{
    INIT_REFCOUNT
    ADD_EVENTTYPE(MESSAGE_VECTORFEATURE_ADDED);
    ADD_EVENTTYPE(MESSAGE_VECTORFEATURE_MODIFIED);

    m_pShapeLayer = pShapeLayer;
    if (!m_pShapeLayer.Assigned()) {return;}

    m_pSlimFeature = pSlimFeature;
    if (!m_pSlimFeature.Assigned())
    {
        m_pShapeLayer.Clear();
        return;
    }
}

CShapeFeature::~CShapeFeature()
{
}

bool __stdcall CShapeFeature::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IVectorFeature"))
        || (0 == strcmp(interfacename, "CShapeFeature")))
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

bool __stdcall CShapeFeature::Clone(IObj** ppObj) const
{
    return false;
}

dword __stdcall CShapeFeature::GetFID() const
{
    if (!m_pSlimFeature.Assigned()) {return 0;}

    return m_pSlimFeature->GetFID();
}

bool __stdcall CShapeFeature::GetMBR(WKSRect& mbr) const
{
    if (!m_pSlimFeature.Assigned()) {return false;}

    return m_pSlimFeature->GetMBR(mbr);
}

void CShapeFeature::GetLayer(CVectorLayerPtr& pLayer)
{
    pLayer.Clear();
    if (!m_pShapeLayer.Assigned()) {return;}

    CAST_PTR(m_pShapeLayer, pLayer, CVectorLayer);
}

bool CShapeFeature::SetGeometryRef(const IGeometryPtr pGeometry)
{
    if (!m_pSlimFeature.Assigned()) {return false;}

    return m_pSlimFeature->SetGeometryRef(pGeometry);
}

void CShapeFeature::GetGeometryRef(IGeometryPtr &pGeometry)
{
    pGeometry.Clear();
    if (!m_pSlimFeature.Assigned()) {return;}

    m_pSlimFeature->GetGeometryRef(pGeometry);
}

dword __stdcall CShapeFeature::GetFieldCount() const
{
    return m_pSlimFeature->GetFieldCount();
}

bool __stdcall CShapeFeature::SetFieldValue(const dword index, const char* const fieldvalue)
{
    return m_pSlimFeature->SetFieldValue(index, fieldvalue);
}

bool __stdcall CShapeFeature::GetFieldValue(const dword index, IFieldValue** ppFieldValue) const
{
    return m_pSlimFeature->GetFieldValue(index, ppFieldValue);
}

void __stdcall CShapeFeature::SetAnnotation(const char* const annotation)
{
    m_pSlimFeature->SetAnnotation(annotation);
}

const char* __stdcall CShapeFeature::GetAnnotation() const
{
    return m_pSlimFeature->GetAnnotation();
}

bool CShapeFeature::SetGeometry(const IGeometryPtr pGeometry)
{
    if (!m_pSlimFeature.Assigned()) {return false;}

    return m_pSlimFeature->SetGeometry(pGeometry);
}

void CShapeFeature::GetGeometry(IGeometryPtr &pGeometry)
{
    pGeometry.Clear();
    if (!m_pSlimFeature.Assigned()) {return;}

    m_pSlimFeature->GetGeometry(pGeometry);
}

void CShapeFeature::SetFieldValues(const string& fieldvalues)
{
    if (!m_pSlimFeature.Assigned()) {return;}

    m_pSlimFeature->SetFieldValues(fieldvalues);
}

string CShapeFeature::FieldValuesAsString() const
{
    if (!m_pSlimFeature.Assigned()) {return "";}

    return m_pSlimFeature->FieldValuesAsString();
}

bool __stdcall CShapeFeature::Delete()
{
    if (!m_pSlimFeature.Assigned()) {return false;}

    bool r = m_pSlimFeature->Delete();
    m_pSlimFeature.Clear();
    return r;
}

bool __stdcall CShapeFeature::Update()
{
    if (!m_pSlimFeature.Assigned())
    {
        return false;
    }

    bool newflag = true;
    if (m_pSlimFeature->GetFID() > 0)
    {
        newflag = false;
    }

    if (!m_pSlimFeature->Update())
    {
        return false;
    }

    dword fid = m_pSlimFeature->GetFID();
    if (newflag)
    {
        this->m_pShapeLayer->NoticeFeatureAdded(fid);
        DISPATCH_LONG_TOALL(MESSAGE_VECTORFEATURE_ADDED, fid)
    }
    else
    {
        this->m_pShapeLayer->NoticeFeatureModified(fid);
        DISPATCH_LONG_TOALL(MESSAGE_VECTORFEATURE_MODIFIED, fid)
    }

    return true;
}

void CShapeFeature::GetFields(CFieldsPtr& pFields) const
{
    pFields.Clear();
    if (!m_pShapeLayer.Assigned()) {return;}

    m_pShapeLayer->GetFields(pFields);
}

bool CShapeFeature::SetFieldValue(const dword index, const CFieldValuePtr pValue)
{
    if (!m_pSlimFeature.Assigned() || !pValue.Assigned()) {return false;}

    string fieldvalue;
    FieldType fieldtype = pValue->GetFieldType();
    switch (fieldtype)
    {
    case FIELDTYPE_SHORT:
    case FIELDTYPE_LONG:
        long intvalue;
        pValue->GetInteger(intvalue);
        fieldvalue = IntToStr(intvalue);
        break;

    case FIELDTYPE_SINGLE:
    case FIELDTYPE_DOUBLE:
        double floatvalue;
        pValue->GetFloat(floatvalue);
        fieldvalue = FloatToStr(floatvalue);
        break;

    default:
        fieldvalue = pValue->GetText();
    }

    return m_pSlimFeature->SetFieldValue(index, fieldvalue.c_str());
}

bool CShapeFeature::GetFieldValue(const dword index, CFieldValuePtr& pValue) const
{
    if (!m_pSlimFeature.Assigned()) {return false;}

    IFieldValue* pFieldValue = NULL;
    bool r = m_pSlimFeature->GetFieldValue(index, &pFieldValue);
    if (!r) return false;
    pValue = (CFieldValue*)pFieldValue;
    pFieldValue->_Release();

    return true;
}

void __stdcall CShapeFeature::GetLayer(ILayer** ppLayer)
{
    if (_invalid(ppLayer)) return;
    *ppLayer = NULL;
    CVectorLayerPtr pV;
    this->GetLayer(pV);
    if (pV.Assigned())
    {
        pV->GotoInterface("ILayer", (void**)ppLayer);
    }
}

bool __stdcall CShapeFeature::SetGeometryRef(const IGeometry* const pGeometry)
{
    IGeometryPtr pG = (IGeometry*)pGeometry;
    return this->SetGeometryRef(pG);
}

void __stdcall CShapeFeature::GetGeometryRef(IGeometry** ppGeometry)
{
    if (_invalid(ppGeometry)) return;
    *ppGeometry = NULL;
    IGeometryPtr pG;
    this->GetGeometryRef(pG);
    if (pG.Assigned())
    {
        *ppGeometry = (IGeometry*)pG._p();
        (*ppGeometry)->_AddRef();
    }
}

}