#include "CommonInclude.h"
#include "SlimLayer.h"
#include "MemoryStream.h"
#include "FileMapStream.h"
#include "SimpleFileStream.h"
#include "MultiSymbol.h"
#include "StringFuncs.h"
#include "MathLib.h"
#include "..\\include\\Messages.h"

namespace easymap
{

static const char* SLIMLAYERIDENTIFY = "SlimLayer";
const char SLIMLAYERMAINVERSION = 1;
const char SLIMLAYERSUBVERSION  = 5;

const dword OFFSET_GEOCOLINFO       = sizeof(CSlimLayer::SlimHeader);
const dword OFFSET_DATAEXTENT       = OFFSET_GEOCOLINFO + sizeof(GeometryColumnInfo);
const dword OFFSET_MAXFID           = OFFSET_DATAEXTENT + sizeof(WKSRect);
const dword OFFSET_FEATURECOUNT     = OFFSET_MAXFID + sizeof(dword);
const dword OFFSET_LASTMODIFYTIME   = OFFSET_FEATURECOUNT + sizeof(SlimRendererType);
const dword OFFSET_INDEXOFFSET      = OFFSET_LASTMODIFYTIME + sizeof(FILETIME);
const dword OFFSET_SYMBOLOFFSET     = OFFSET_INDEXOFFSET + sizeof(dword);
const dword OFFSET_NETTOPOOFFSET    = OFFSET_SYMBOLOFFSET + sizeof(dword);
const dword OFFSET_SR               = OFFSET_NETTOPOOFFSET + sizeof(dword);
const dword OFFSET_FIXEDHEADER      = OFFSET_SR + 3004*sizeof(char);

CLASS_FACTORY_INSTANCE(CSlimLayer)

CVectorLayer::CVectorLayer()
{
    INIT_REFCOUNT
};

CSlimLayer::CSlimLayer()
{
//    INIT_REFCOUNT 基类已经搞过了

    //准备好等待监听的事件类型
    this->InitEventTypes();

    //创建索引Stream
    m_pIndexStream = new CMemoryStream;

    CStreamPtr pStream = new CMemoryStream;
    this->InitialSlimLayer(pStream);
}

CSlimLayer::CSlimLayer(const ShapeType shapetype, const MapUnits mapunit,
    const double& basescale, const double& precision, const WKSRect& extent,
    const long indexlevel, const CFieldsPtr pFields, const bool annotation)
{
//    INIT_REFCOUNT 基类已经搞过了

    //准备好等待监听的事件类型
    this->InitEventTypes();

    //创建索引Stream
    m_pIndexStream = new CMemoryStream;

    CStreamPtr pStream = new CMemoryStream;
    this->InitialSlimLayer(pStream);

    m_GeoColumnInfo.ShpType      = shapetype;
    m_GeoColumnInfo.FeatureType  = annotation ? VECTORFEATURETYPE_TEXT : VECTORFEATURETYPE_GEOMETRY;
    m_GeoColumnInfo.MapUnit      = mapunit;
    m_GeoColumnInfo.BaseScale    = basescale;
    m_GeoColumnInfo.ToleranceXY  = precision;
    m_GeoColumnInfo.DomainXY     = extent;
    m_GeoColumnInfo.SIParam1     = indexlevel;

    this->UpdateFixedHeader();

    CCellQuadTree::CQTParams indexparam;
    indexparam.centerpoint.x = (extent.left + extent.right) / 2;
    indexparam.centerpoint.y = (extent.top + extent.bottom) / 2;
    double width = extent.right - extent.left;
    double height = extent.top - extent.bottom;
    indexparam.BoundSize = width > height ? width : height;
    if (UNIT_DEGREE == mapunit)
    {
        if (0.001 > indexparam.BoundSize)
        {
            indexparam.BoundSize = 0.001;
        }
    }
    else
    {
        if (50 > indexparam.BoundSize)
        {
            indexparam.BoundSize = 50;
        }
    }

    indexparam.treelevel = indexlevel;
    m_pSpatialIndex = new CMultiQuadTree(indexparam);
    this->Index2DataStream();

    this->Symbols2Stream();

    if (pFields.Assigned())
    {
        string fields;
        long fieldcount = (long)pFields->GetFieldCount();
        for (long i = 0; i < fieldcount; i++)
        {
            CFieldPtr pField;
            pFields->GetField(i, pField);
            string fieldname, sft;
            fieldname = pField->GetFieldName();
            FieldType fieldtype = pField->GetFieldType();
            switch (fieldtype)
            {
                case FIELDTYPE_SHORT:
                case FIELDTYPE_LONG:
                    sft = "##i_";
                    break;

                case FIELDTYPE_SINGLE:
                case FIELDTYPE_DOUBLE:
                    sft = "##f_";
                    break;

                default:
                    sft = "##s_";
            }
            fields = fields + sft + fieldname + "\n";
        }

        this->SetFields(fields);
    }

    this->SetNetTolerance(m_GeoColumnInfo.ToleranceXY);
}

CSlimLayer::~CSlimLayer()
{
}

void CSlimLayer::InitEventTypes()
{
    //提供以下事件等待监听
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_ADDED);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_DELETED);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_MODIFIED);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_EDITSAVED);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_EDITCANCELED);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_UNDO);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_REDO);
    ADD_EVENTTYPE(MESSAGE_VECTORLAYER_INDEXRELOADED);
}

bool CSlimLayer::CheckHeader() const
{
    if ((0 != ::strcmp(m_Header.Reserved, SLIMLAYERIDENTIFY))
        || (SLIMLAYERMAINVERSION != m_Header.MainVersion)
        || (SLIMLAYERSUBVERSION != m_Header.SubVersion))
    {
        return false;
    }

    return true;
}

void CSlimLayer::InitialSlimLayer(CStreamPtr pStream)
{
    m_pFeatureData = pStream;

    //----------------- 搞定数据文件头 ---------------------
    ::memset(&m_Header, 0, sizeof(SlimHeader));
    ::strcpy(m_Header.Reserved, SLIMLAYERIDENTIFY);
    ::strcpy(m_Header.Alias, "__pending");
    //主版本号
    m_Header.MainVersion = SLIMLAYERMAINVERSION;
    //次版本号
    m_Header.SubVersion = SLIMLAYERSUBVERSION;
    //每个index stream block 的长度
    m_Header.IndexBlockSize = 65536 - sizeof(dword);
    ::strcpy(m_Header.FieldsInfo, "");
    m_Header.DisplayField = -1;

    m_GeoColumnInfo.ShpType = SHAPETYPE_UNKNOWN;
    m_GeoColumnInfo.MapUnit = UNIT_M;
    m_GeoColumnInfo.BaseScale = 2000;
    m_GeoColumnInfo.ToleranceXY = 0.001;

    m_DataExtent.left = 0;
    m_DataExtent.right = 0;
    m_DataExtent.top = 0;
    m_DataExtent.bottom = 0;

    m_MaxFID = 0;
    m_FeatureCount = 0;
    m_ReferenceScale = 0;
    m_ShowDefaultSymbol = true;
    m_RendererType = SLIMRENDERERTYPE_SIMPLE;
    m_RendererField = -1;

    m_FastShortest = true;

    this->UpdateFixedHeader();
    //----------------- 搞定数据文件头 ---------------------

    //写入空白垃圾sr
    dword csr = 0;
    m_pFeatureData->MovePos(OFFSET_SR);
    m_pFeatureData->Write(csr);

    //在OFFSET_INDEXOFFSET、OFFSET_SYMBOLOFFSET、OFFSET_NETTOPOOFFSET写入初值0
    //代表此时还未向m_pFeatureData中写入indexstream、symbolstream和nettopo
    dword offset = 0;
    m_pFeatureData->MovePos(OFFSET_INDEXOFFSET);
    m_pFeatureData->Write(offset);
    m_pFeatureData->MovePos(OFFSET_SYMBOLOFFSET);
    m_pFeatureData->Write(offset);
    m_pFeatureData->MovePos(OFFSET_NETTOPOOFFSET);
    m_pFeatureData->Write(offset);

    this->WriteModifyTime();

    m_SaveDirty = false;
    m_EditProc.atoms.clear();
    m_UndoProcs.clear();

    m_ModifyDirty = false;

    this->SetNetTolerance(m_GeoColumnInfo.ToleranceXY);

    m_pAuxPoint = new CPoint;
}

CSlimLayer::CSlimLayer(const string& filename, const bool readonly,
    const bool filemap)
{
//    INIT_REFCOUNT 基类已经搞过了

    //准备好等待监听的事件类型
    this->InitEventTypes();

    //创建索引Stream
    m_pIndexStream = new CMemoryStream;

    this->FromFile(filename, readonly, filemap);
    //文件可能已经不存在了，那就只能搞个假的
    if (!m_pFeatureData.Assigned())
    {
        this->GotoHell();
    }

    string namegot = RemoveDirectoryPart(filename);
    string ext = LowerString(GetExtNamePart(namegot));
    if (ext == "esd")
    {
        namegot = RemoveExtNamePart(namegot);
    }
    this->SetName(namegot.c_str());
}

void CSlimLayer::FromFile(const string& filename, const bool readonly,
    const bool filemap)
{
    m_ModifyDirty = false;
    m_SaveDirty = false;
    m_ReferenceScale = 0;

    //管她娘的
    string namegot, dir = GetDirectoryPart(filename);
    ::CreateDirectory(dir.c_str(), NULL);

    CFileStreamPtr pFMS;
    if (filemap)
    {
        dword filelen = 0;
        HANDLE hfile = ::CreateFile(filename.c_str(), GENERIC_READ,
            FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
            FILE_ATTRIBUTE_READONLY, NULL);
        if (_valid(hfile) && (INVALID_HANDLE_VALUE != hfile))
        {
            filelen = ::GetFileSize(hfile, NULL);
            ::CloseHandle(hfile);
        }

        MEMORYSTATUS memorystatus;
        ::GlobalMemoryStatus(&memorystatus);

        //保险起见，文件大于100MB或物理内存小于100MB就不用映射文件方式
        if ((memorystatus.dwAvailPhys > 123456789) && (123456789 > filelen)
            && (memorystatus.dwAvailPhys > filelen))
        {
            pFMS = new CFileMapStream(filename.c_str(), readonly);
            namegot = pFMS->GetMapFileName();
            if (namegot == "")
            {
                //无法映射，用安全方式打开
                pFMS = new CSimpleFileStream(filename.c_str(), readonly);
            }
        }
        else
        {
            pFMS = new CSimpleFileStream(filename.c_str(), readonly);
        }
    }
    else
    {
        pFMS = new CSimpleFileStream(filename.c_str(), readonly);
    }

    namegot = pFMS->GetMapFileName();
    if (namegot == "")
    {
        m_pFeatureData.Clear();
        return;
    }

    ::memset(&m_Header, 0, sizeof(CSlimLayer::SlimHeader));

    CAST_PTR(pFMS, m_pFeatureData, CStream)

    this->ReloadFixedHeader();
    if (!this->CheckHeader())
    {
        m_pFeatureData = NULL;
        return;
    }

    this->DataStream2Index();
    this->Stream2Symbols();

    this->SetNetTolerance(m_GeoColumnInfo.ToleranceXY);

    m_pAuxPoint = new CPoint;
}

void CSlimLayer::GotoHell()
{
    CStreamPtr pStream = new CMemoryStream;
    this->InitialSlimLayer(pStream);
    m_GeoColumnInfo.ShpType = SHAPETYPE_UNKNOWN;
    m_GeoColumnInfo.FeatureType  = VECTORFEATURETYPE_UNKNOWN;
    this->UpdateFixedHeader();
}

void CSlimLayer::SelectObjectsByEnvelope(vector<dword>& resultfids,
    const WKSRect& envelope, const bool partialselect)
{
    CSpatialIndex::CSISearchResultPtr pSIResult;
    m_pSpatialIndex->Search(pSIResult, &envelope);
    dword delta = 0;
    while (pSIResult->Next(delta))
    {
        m_pFeatureData->MovePos(delta);
        Feature feature;
        this->Stream2Feature(feature);
        if (((partialselect && (EnvelopesTouched(envelope, feature.mbr)))
            || (!partialselect && (EnvelopesContented(envelope, feature.mbr)))))
        {
            if (feature.geometry->Select(envelope, partialselect))
            {
                resultfids.push_back(feature.fid);
            }
        }
    }
}

void CSlimLayer::Feature2Stream(const Feature& feature)
{
    m_pFeatureData->Write(feature.fid);
    m_pFeatureData->Write(&feature.mbr, sizeof(WKSRect));
    Geometry2Stream(feature.geometry, m_pFeatureData);
    m_pFeatureData->Write(feature.fieldvalues);
    m_pFeatureData->Write(feature.annotation);
}

void CSlimLayer::Stream2Feature(Feature& feature)
{
    m_pFeatureData->Read(feature.fid);
    m_pFeatureData->Read(&feature.mbr, sizeof(WKSRect));
    Stream2Geometry(m_pFeatureData, feature.geometry);
    m_pFeatureData->Read(feature.fieldvalues);
    m_pFeatureData->Read(feature.annotation);
}

void CSlimLayer::Index2Stream()
{
    if (this->ReadOnly())
    {
        return;
    }

    m_pIndexStream->SetSize(0);

    string indexstreamhead = "Index";
    m_pIndexStream->Write(indexstreamhead);

    dword count = m_FidIndex.size();
    m_pIndexStream->Write(count);
    map<dword, dword>::const_iterator it = m_FidIndex.begin();
    while (it != m_FidIndex.end())
    {
        dword fid = it->first;
        dword delta = it->second;
        m_pIndexStream->Write(fid);
        m_pIndexStream->Write(delta);
        it++;
    }

    IStreamX* psx = (IStreamX*)m_pIndexStream._p();
    m_pSpatialIndex->Dump(psx);
}

void CSlimLayer::Stream2Index()
{
    m_pIndexStream->MovePos(0);

    string indexstreamhead;
    m_pIndexStream->Read(indexstreamhead);

    m_FidIndex.clear();
    dword count;
    m_pIndexStream->Read(count);
    for (dword i = 0; i < count; i++)
    {
        dword fid, delta;
        m_pIndexStream->Read(fid);
        m_pIndexStream->Read(delta);
        m_FidIndex[fid] = delta;
    }

    CPersistPtr pPersist;
    CPersist::Instantiate(m_pIndexStream, pPersist);
    CAST_PTR(pPersist, m_pSpatialIndex, CMultiQuadTree)
}

void CSlimLayer::Index2DataStream()
{
    if (this->ReadOnly())
    {
        return;
    }

    this->Index2Stream();

    //--------------------------------------------------------------------------
    //将m_pIndexStream分块写入m_pFeatureData

    //index stream 的block长度
    dword blocksize = m_Header.IndexBlockSize;

    //记录indexstream的长度，整除以(blocksize)，
    //得到所需的block数目（可能差最后半个block）
    dword streamsize = m_pIndexStream->GetSize();
    dword blockcount = streamsize / (blocksize);
    //余数为最后半个block的有效长度
    dword dd = streamsize % (blocksize);

    //读取indexstream第一个block的偏移
    dword nextoffset = 0;
    m_pFeatureData->MovePos(OFFSET_INDEXOFFSET);
    m_pFeatureData->Read(nextoffset);
    //注意这等下用来在循环中存放上一轮循环中block末尾4bytes的偏移
    //该偏移处的4bytes用来记录本轮当前block首的偏移
    dword tailoffset = OFFSET_INDEXOFFSET;

    dword size = 0;

    //循环blockcount次，写入full的block，
    //如果余数不为零，则需要处理最后半个block
    m_pIndexStream->MovePos(0);
    for (dword i = 0; i < blockcount; i++)
    {
        if (0 == nextoffset)
        {
            //申请新block
            size = m_pFeatureData->GetSize();
            //记下新block的偏移到上一个block的末尾
            m_pFeatureData->MovePos(tailoffset);
            m_pFeatureData->Write(size);
            //写入stream的这个block
            m_pFeatureData->MovePos(size);
            m_pIndexStream->CopyDataTo(m_pFeatureData, blocksize);
            //记下这个block的最后4个bytes的偏移，准备用来记录下个block首的偏移
            tailoffset = m_pFeatureData->GetPos();
            //向这个block末端4bytes写入0，代表这是目前的最后一个block
            size = 0;
            m_pFeatureData->Write(size);
        }
        else
        {
            //这是个可用的block，直接写入
            m_pFeatureData->MovePos(nextoffset);
            m_pIndexStream->CopyDataTo(m_pFeatureData, blocksize);
            //记下这个block的最后4个bytes的偏移，用来记录下个block首的偏移
            //（如果为0，下一个需要申请的话）
            tailoffset = m_pFeatureData->GetPos();
            //读出最后4bytes（下一个block首的偏移），
            //如果为0代表这是最后一个block，下一次需要申请新block
            m_pFeatureData->Read(nextoffset);
        }
    }

    //如果余数不为0，代表要处理最后半个block
    if (0 != dd)
    {
        if (0 == nextoffset)
        {
            //记下新block的偏移，保存到上一个block的末尾
            size = m_pFeatureData->GetSize();
            m_pFeatureData->MovePos(tailoffset);
            m_pFeatureData->Write(size);
            //申请新block
            dword oldpos = m_pIndexStream->GetPos();
            m_pIndexStream->SetSize(size + blocksize + sizeof(dword));
            m_pIndexStream->MovePos(oldpos);
            //写入stream的这半个block
            m_pFeatureData->MovePos(size);
            m_pIndexStream->CopyDataTo(m_pFeatureData, blocksize);
            //向这个block末端4bytes写入0，代表这是最后一个block
            m_pIndexStream->MovePos(size + blocksize);
            size = 0;
            m_pFeatureData->Write(size);
        }
        else
        {
            //直接写入这半个block
            m_pFeatureData->MovePos(nextoffset);
            m_pIndexStream->CopyDataTo(m_pFeatureData, blocksize);
        }
    }

    m_UndoProcs.clear();
    EditProcess ep;
    m_UndoProcs.push_back(ep);  //写入一个joe
    m_UndoIndex = 0;
}

void CSlimLayer::DataStream2Index()
{
    //index stream 的block长度
    dword blocksize = m_Header.IndexBlockSize;

    m_pIndexStream->MovePos(0);
    m_pFeatureData->MovePos(OFFSET_INDEXOFFSET);
    dword nextoffset = 0;
    m_pFeatureData->Read(nextoffset);
    while (0 != nextoffset)
    {
        //转到当前block首的offset
        m_pFeatureData->MovePos(nextoffset);
        //读出该block的内容，到最后4 bytes
        m_pFeatureData->CopyDataTo(m_pIndexStream, blocksize);
        //读出下一个block的offset
        m_pFeatureData->Read(nextoffset);
    }

    this->Stream2Index();
    
    m_UndoProcs.clear();
    EditProcess ep;
    m_UndoProcs.push_back(ep);  //写入一个joe
    m_UndoIndex = 0;

    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_INDEXRELOADED, 0)
}

void _slim_create_default_symbol(const GeometryColumnInfo& colinfo, ISymbolPtr& pSymbol)
{
    if (VECTORFEATURETYPE_TEXT == colinfo.FeatureType)
    {
        pSymbol = new CSimpleTextSymbol;
    }
    else
    {
        switch (colinfo.ShpType)
        {
        case SHAPETYPE_POINT:
        case SHAPETYPE_MULTIPOINT:
            {
                IMultiPointSymbolPtr pMPS = new CMultiPointSymbol;
                pMPS->AddSimpleSymbol(RGB(30, 30, 200), 2);
                CAST_PTR(pMPS, pSymbol, ISymbol)
            }
            break;

        case SHAPETYPE_POLYLINE:
            {
                IMultiLineSymbolPtr pMLS = new CMultiLineSymbol;
                pMLS->AddSimpleSymbol(RGB(30, 30, 200), 0.3);
                CAST_PTR(pMLS, pSymbol, ISymbol)
            }
            break;

        case SHAPETYPE_POLYGON:
            {
                IMultiFillSymbolPtr pMFS = new CMultiFillSymbol;
                pMFS->AddSimpleSymbol(RGB(160, 180, 210));
//                IPointFillSymbolPtr pPFS = new CPointFillSymbol;
//                pPFS->SetColor(RGB(55, 60, 120));
//                pMFS->AddSymbol(pPFS._p());
                CAST_PTR(pMFS, pSymbol, ISymbol)
            }
            break;

        default:
            return;
        }
    }
}

void CSlimLayer::Symbols2Stream()
{
    if (this->ReadOnly()) {return;}

    //先写到一个临时MemoryStream中
    IStreamXPtr pTempStreamX = new CMemoryStream;
    CStreamPtr pTempStream;
    CAST_PTR(pTempStreamX, pTempStream, CStream)

    string symbolstreamhead = "Symbols";
    pTempStream->Write(symbolstreamhead);

    if (!m_pDefaultSymbol.Assigned())
    {
        //刚创建时还没有symbol，就创建一个
        _slim_create_default_symbol(m_GeoColumnInfo, m_pDefaultSymbol);
    }

    m_pDefaultSymbol->Dump(pTempStreamX._p());

    pTempStream->Write(&m_RendererType, sizeof(SlimRendererType));
    pTempStream->Write(m_RendererField);
    pTempStream->WriteBool(m_ShowDefaultSymbol);

    dword symbolcount = m_SymbolMap.size();
    pTempStream->Write(symbolcount);
    map<string, ISymbolPtr>::const_iterator it = m_SymbolMap.begin();
    while (it != m_SymbolMap.end())
    {
        pTempStream->Write(it->first);
        it->second->Dump(pTempStreamX._p());
        it++;
    }

    //记录下所占用的字节数
    //如果比原来预留的小，就直接写到原来的Block
    //否则就追加到文件尾
    dword symbolstreamsize = pTempStreamX->GetSize();

    m_pFeatureData->MovePos(OFFSET_SYMBOLOFFSET);
    dword symboloffset;
    m_pFeatureData->Read(symboloffset); //注意这里可能为0，代表没有存过符号
    m_pFeatureData->MovePos(symboloffset);
    dword reservedsize;
    //读出4个bytes的保留区长度，游标指向现有的symbolblock起始处
    m_pFeatureData->Read(reservedsize);
    if ((reservedsize < symbolstreamsize) || (symboloffset == 0))
    {
        //保留区长度不够，或者symboloffset == 0，在末尾申请新的保留区
        symboloffset = m_pFeatureData->GetSize();
        //注意别忘了修改OFFSET_SYMBOLOFFSET
        m_pFeatureData->MovePos(OFFSET_SYMBOLOFFSET);
        m_pFeatureData->Write(symboloffset);
        m_pFeatureData->MovePos(symboloffset);
        //写入4个bytes的保留区长度，游标指向新的symbolblock起始处
        m_pFeatureData->Write(symbolstreamsize);
    }

    //现在向保留区复制pTempStreamX的内容
    pTempStream->MovePos(0);
    pTempStream->CopyDataTo(m_pFeatureData, symbolstreamsize);
}

void CSlimLayer::Stream2Symbols()
{
    m_pFeatureData->MovePos(OFFSET_SYMBOLOFFSET);
    dword offset;
    m_pFeatureData->Read(offset);
    if (0 == offset)
    {
        _slim_create_default_symbol(m_GeoColumnInfo, m_pDefaultSymbol);
        return;
    }

    //注意头4个bytes是symbolblock长度，要跳过
    m_pFeatureData->MovePos(offset + sizeof(dword));

    string symbolstreamhead;
    m_pFeatureData->Read(symbolstreamhead);

    CPersistPtr pPersist;
    CPersist::Instantiate(m_pFeatureData, pPersist);
    CAST_PTR(pPersist, m_pDefaultSymbol, ISymbol)

    m_pFeatureData->Read(&m_RendererType, sizeof(SlimRendererType));
    m_pFeatureData->Read(m_RendererField);
    m_pFeatureData->ReadBool(m_ShowDefaultSymbol);

    m_SymbolMap.clear();
    dword symbolcount;
    m_pFeatureData->Read(symbolcount);
    for (dword i = 0; i < symbolcount; i++)
    {
        string key;
        m_pFeatureData->Read(key);
        pPersist.Clear();
        CPersist::Instantiate(m_pFeatureData, pPersist);
        ISymbolPtr pSymbol;
        CAST_PTR(pPersist, pSymbol, ISymbol)
        m_SymbolMap[key] = pSymbol;
    }
}

void CSlimLayer::UpdateFixedHeader()
{
    if (this->ReadOnly()) {return;}

    m_pFeatureData->MovePos(0);
    m_pFeatureData->Write(&m_Header, sizeof(SlimHeader));
    m_pFeatureData->Write(&m_GeoColumnInfo, sizeof(GeometryColumnInfo));
    m_pFeatureData->Write(&m_DataExtent, sizeof(WKSRect));
    m_pFeatureData->Write(m_MaxFID);
    m_pFeatureData->Write(m_FeatureCount);
    //注意，剩下几项不在这里更新

    //这里保证文件头的长度
    dword size = m_pFeatureData->GetSize();
    if (OFFSET_FIXEDHEADER >= size)
    {
        m_pFeatureData->SetSize(OFFSET_FIXEDHEADER + 1);
    }

    //想不开
    m_pFeatureData->MovePos(OFFSET_FIXEDHEADER);
    dword joe = 0;
    m_pFeatureData->Write(joe);
}

void CSlimLayer::ReloadFixedHeader()
{
    m_pFeatureData->MovePos(0);
    m_pFeatureData->Read(&m_Header, sizeof(SlimHeader));
    m_pFeatureData->Read(&m_GeoColumnInfo, sizeof(GeometryColumnInfo));
    m_pFeatureData->Read(&m_DataExtent, sizeof(WKSRect));
    m_pFeatureData->Read(m_MaxFID);
    m_pFeatureData->Read(m_FeatureCount);
    //注意，剩下几项不在这里更新
}

bool CSlimLayer::_GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if (0 == strcmp(interfacename, "CSlimLayer"))
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

dword CSlimLayer::PresaveInstance(CStreamPtr pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    long n;
    char c;
    string filename = this->GetFileName();

    if (filename == "")
    {
        //是内存数据源，全部存储到序列化流中，设定标志位为0
        c = 0;
        pStream->Write(c);
        //复制整个飞机
        n = m_pFeatureData->GetSize();
        pStream->Write(n);
        m_pFeatureData->MovePos(0);
        m_pFeatureData->CopyDataTo(pStream, n);
    }
    else
    {
        //是文件数据源，只存储文件名
        //注意标志位：1> SimpleFileStream, 2> FileMapStream
        c = 1;
        CFileMapStreamPtr pFileMapStream;
        CAST_PTR(m_pFeatureData, pFileMapStream, CFileMapStream)
        if (pFileMapStream.Assigned())
        {
            c = 2;
        }

        pStream->Write(c);
        //存储是否只读
        bool readonly = this->ReadOnly();
        pStream->WriteBool(readonly);

        //存储文件名，注意这里所做的手脚
        //尽量使用相对路径
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
    }

    //最后保存一些东东
    IStreamX* psx = (IStreamX*)pStream._p();
    ISymbolPtr pSymbol;
    this->GetDefaultSymbol(pSymbol);
    pSymbol->_DumpTo(psx, assist);

    dword symbolcount = this->m_SymbolMap.size();
    pStream->Write(symbolcount);
    map<string, ISymbolPtr>::const_iterator it = this->m_SymbolMap.begin();
    while (it != m_SymbolMap.end())
    {
        pStream->Write(it->first);
        it->second->Dump(psx);
        it++;
    }

    SlimRendererType renderertype = this->GetRendererType();
    pStream->Write(renderertype);

    bool showdefaultsymbol = this->GetShowDefaultSymbol();
    pStream->WriteBool(showdefaultsymbol);

    long rendererfield = this->GetRendererField();
    pStream->Write(rendererfield);

    double referencescale;
    this->GetRefScale(referencescale);
    pStream->Write(referencescale);

    byte alpha = this->GetAlpha();
    pStream->Write(alpha);

    double maxscale, minscale;
    this->GetScaleLimit(maxscale, minscale);
    pStream->Write(maxscale);
    pStream->Write(minscale);

    long displayfield = this->GetDisplayField();
    pStream->Write(displayfield);

    string lyrname = this->GetName();
    pStream->Write(lyrname);

    string alias = this->GetAlias();
    pStream->Write(alias);

    return pStream->GetPos() - oldpos;   
}

dword CSlimLayer::PreloadInstance(CStreamPtr pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();

    long n;
    //先判断标志位，0－data in memory
    //              1、2－data in file
    char c;
    pStream->Read(c);
    if (0 == c)
    {
        //内存数据源，从序列化stream中扣出一大块来搞定m_pFeatureData
        pStream->Read(n);
        m_pFeatureData->MovePos(0);
        long size1 = pStream->CopyDataTo(m_pFeatureData, n);
        if (n != size1)
        {
        }

        //恢复status
        this->ReloadFixedHeader();
        this->DataStream2Index();
        this->Stream2Symbols();
    }
    else
    {
        //文件数据源，首先是否只读
        bool readonly;
        pStream->ReadBool(readonly);
        //然后读取所保存的文件名
        string originalfilename;
        pStream->Read(originalfilename);
        string filename = originalfilename;

        char currdir[2000];
        ::GetCurrentDirectory(1900, currdir);
        string dir = currdir;

        //用currdir＋保存的相对路径
        filename = dir + filename;
        HANDLE hfile = ::CreateFile(filename.c_str(), GENERIC_READ,
            FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
            FILE_ATTRIBUTE_READONLY, NULL);

        //搞不定，用绝对路径试试
        if (_invalid(hfile) || (INVALID_HANDLE_VALUE == hfile))
        {
            filename = originalfilename;
            hfile = ::CreateFile(filename.c_str(), GENERIC_READ,
                FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
                FILE_ATTRIBUTE_READONLY, NULL);
        }

        if (_invalid(hfile) || (INVALID_HANDLE_VALUE == hfile))
        {
            //还是搞不定，试试workspace路径
            filename = dir + "\\" + RemoveDirectoryPart(filename);
            hfile = ::CreateFile(filename.c_str(), GENERIC_READ,
                FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
                FILE_ATTRIBUTE_READONLY, NULL);
        }

        if (_valid(hfile) && (INVALID_HANDLE_VALUE != hfile))
        {
            ::CloseHandle(hfile);
            bool filemap = true;
//            if (1 == c) {filemap = false;}
            this->FromFile(filename, readonly, filemap);
        }

        //文件可能已经不存在了，那就只能搞个假的
        if (!this->m_pFeatureData.Assigned())
        {
            this->GotoHell();
            this->SetName(filename.c_str());
        }
    }

    //最后搞定那些
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

    map<string, ISymbolPtr>::const_iterator it = symbols.begin();
    while (it != symbols.end())
    {
        this->m_SymbolMap[it->first] = it->second;
        it++;
    }

    this->SetRendererType(renderertype, false);
    this->SetRendererField(rendererfield, false);
    this->SetDefaultSymbol(pDefaultSymbol, false);
    this->SetShowDefaultSymbol(showdefaultsymbol, false);
    this->SetRefScale(refscale);
    this->SetScaleLimit(maxscale, minscale);
    this->SetAlpha(alpha);
    this->SetDisplayField(displayfield);
    this->SetName(lyrname.c_str());
    this->SetAlias(alias);

    m_SaveDirty = false;
    m_EditProc.atoms.clear();

    return pStream->GetPos() - oldpos;
}

bool CheckTrackCancel(const ITrackCancelPtr pTrackCancel, const long cacheid)
{
    static DWORD checklast = 0;
    static DWORD postlast = 0;
    if (!pTrackCancel.Assigned())
    {
        checklast = ::GetTickCount();
        postlast = ::GetTickCount();
        return false;
    }

    DWORD tick = ::GetTickCount();
    bool check = ((tick - checklast) > 200);
    bool post = ((tick - postlast) > 900);

    if (post)
    {
        pTrackCancel->PostProgress(cacheid);
        postlast = tick;
    }

    if (check)
    {
        checklast = tick;
        return pTrackCancel->CheckCancel();
    }
    else
    {
        return false;
    }
}

DrawResult CSlimLayer::DrawLayerData(const CDisplayCachePtr pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    if (!this->Valid()) {return LAYERDRAW_NOREADY;}

    this->RereadIndexStream();

    bool anno = (VECTORFEATURETYPE_TEXT == m_GeoColumnInfo.FeatureType);

    CDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, CDisplay)

    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);

    WKSRect viewextent;
    if (pEnvelope)
    {
        viewextent = *pEnvelope;
        CorrectEnvelope(viewextent);
    }
    else
    {
        pTrans->GetVisibleExtent(viewextent);
    }

    double oldrefscale;
    pTrans->GetReferenceScale(oldrefscale);
    if (0.01 < m_ReferenceScale)
    {
        pTrans->SetReferenceScale(m_ReferenceScale);
    }
    else
    {
        //如果是注记，强制使用原始比例尺作为参考比例尺
        if (anno)
        {
            pTrans->SetReferenceScale(m_GeoColumnInfo.BaseScale);
        }
    }

    bool trackcancelflag = false;

    long tmpid = pDisplayCache->CreateCache(RGB(255, 255, 255), m_Alpha);
    pDisplayCache->CopyCacheToCache(cacheid, tmpid, 0, 0);
    pDisplayCache->SetCacheSymbol(tmpid, m_pDefaultSymbol);
    if (!pDisplay->StartDraw())
    {
        pTrans->SetReferenceScale(oldrefscale);
        return LAYERDRAW_DISPLAYNOREADY;
    }

    CheckTrackCancel(NULL, -1);
    if (pTrackCancel.Assigned())
    {
        pTrackCancel->ShowHint(tmpid);
    }

    CSpatialIndex::CSISearchResultPtr pSIResult;
    m_pSpatialIndex->Search(pSIResult, &viewextent);
    dword delta = 0;
    while (pSIResult->Next(delta))
    {
        WKSRect mbr;
        m_pFeatureData->MovePos(delta + sizeof(dword));
        m_pFeatureData->Read(&mbr, sizeof(WKSRect));
        if (!EnvelopesTouched(mbr, viewextent))
        {
            continue;
        }

        Feature feature;
        RECT rect;
        string uniquevaluefield;
        bool simplerenderer = true;
        if ((SLIMRENDERERTYPE_UNIQUEVALUE == m_RendererType)
            || (SLIMRENDERERTYPE_GRADE == m_RendererType))
        {
            m_pFeatureData->MovePos(delta);
            ((CSlimLayer*)this)->Stream2Feature(feature);
            if (0 > m_RendererField)
            {
                //以fid为renderer字段
                uniquevaluefield = IntToStr(feature.fid);
            }
            else
            {
                Strings strings(feature.fieldvalues);
                if (strings.GetLineCount() > m_RendererField)
                {
                    strings.GetLine(m_RendererField, uniquevaluefield);
                }
            }

            ISymbolPtr pSymbol;
            map<string, ISymbolPtr>::const_iterator it;
            if (SLIMRENDERERTYPE_UNIQUEVALUE == m_RendererType)
            {
                it = m_SymbolMap.find(uniquevaluefield);
                if (it != m_SymbolMap.end())
                {
                    pSymbol = it->second;
                    simplerenderer = false;
                }
                else
                {
                    if (!m_ShowDefaultSymbol)
                    {
                        simplerenderer = false;
                    }
                }
            }
            else if (SLIMRENDERERTYPE_GRADE == m_RendererType)
            {
                it = m_SymbolMap.begin();
                while (it != m_SymbolMap.end())
                {
                    string key = Trim(it->first);
                    long space = FindFirstChar(key.c_str(), ' ');
                    double from = StrToFloat(key.substr(0, space));
                    double to = StrToFloat(key.substr(space+1, key.size()-1));
                    double gradefield = StrToFloat(uniquevaluefield);
                    if ((gradefield >= from) && (gradefield < to))
                    {
                        pSymbol = it->second;
                        break;
                    }
                    it++;
                }

                if (pSymbol.Assigned())
                {
                    simplerenderer = false;
                }
                else
                {
                    if (!m_ShowDefaultSymbol)
                    {
                        simplerenderer = false;
                    }
                }
            }

            if (pSymbol.Assigned())
            {
                pDisplayCache->SetCacheSymbol(tmpid, it->second);
                if (anno)
                {
                    pDisplayCache->DrawCacheText(tmpid, feature.geometry,
                        feature.annotation.c_str(), rect);
                }
                else
                {
                    pDisplayCache->DrawCacheGeometry(tmpid, feature.geometry);
                }

                pDisplayCache->SetCacheSymbol(tmpid, m_pDefaultSymbol);
            }
        }

        if (simplerenderer)
        {
            if (anno)
            {
                m_pFeatureData->MovePos(delta);
                ((CSlimLayer*)this)->Stream2Feature(feature);
                pDisplayCache->DrawCacheText(tmpid, feature.geometry,
                    feature.annotation.c_str(), rect);
            }
            else
            {
                m_pFeatureData->MovePos(delta + sizeof(dword) + sizeof(WKSRect));
                pDisplayCache->DrawCacheStream(tmpid, m_pFeatureData);
            }
        }

        trackcancelflag = CheckTrackCancel(pTrackCancel, tmpid);
        if (trackcancelflag)
        {
            break;
        }
    }

    pDisplay->FinishDraw();
    pDisplayCache->CopyCacheToCache(tmpid, cacheid, 0, 0);
    pDisplayCache->DeleteCache(tmpid);

    pTrans->SetReferenceScale(oldrefscale);
    if (trackcancelflag)
    {
        return LAYERDRAW_TRACKCANCEL;
    }
    else
    {
        if (pTrackCancel.Assigned() && pTrackCancel->CheckCancel())
        {
            return LAYERDRAW_TRACKCANCEL;
        }
        else
        {
            return LAYERDRAW_NORMAL;
        }
    }
}

DrawResult CSlimLayer::DrawLayerSelection(const CDisplayCachePtr pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancelPtr pTrackCancel)
{
    if (!this->Valid()) {return LAYERDRAW_NOREADY;}

    CDisplayPtr pDisplay;
    CAST_PTR(pDisplayCache, pDisplay, CDisplay)
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);

    WKSRect viewextent;
    if (pEnvelope)
    {
        viewextent = *pEnvelope;
        CorrectEnvelope(viewextent);
    }
    else
    {
        pTrans->GetVisibleExtent(viewextent);
    }

    if (!pDisplay->StartDraw())
    {
        return LAYERDRAW_DISPLAYNOREADY;
    }

    bool trackcancelflag = false;
    CheckTrackCancel(NULL, -1);
    if (pTrackCancel.Assigned())
    {
        pTrackCancel->ShowHint(cacheid);
    }

    list<dword>::const_iterator it_fid = m_SelectedFIDs.begin();
    while (it_fid != m_SelectedFIDs.end())
    {
        map<dword, dword>::const_iterator it_delta = m_FidIndex.find(*it_fid);
        if (it_delta != m_FidIndex.end())
        {
            m_pFeatureData->MovePos(it_delta->second);
            Feature feature;
            ((CSlimLayer*)this)->Stream2Feature(feature);
            if (EnvelopesTouched(viewextent, feature.mbr))
            {
                pDisplayCache->DrawCacheGeometry(cacheid, feature.geometry);
            }
        }
        it_fid++;

        trackcancelflag = CheckTrackCancel(pTrackCancel, cacheid);
        if (trackcancelflag)
        {
            break;
        }
    }

    pDisplay->FinishDraw();

    if (trackcancelflag)
    {
        return LAYERDRAW_TRACKCANCEL;
    }
    else
    {
        if (pTrackCancel.Assigned() && pTrackCancel->CheckCancel())
        {
            return LAYERDRAW_TRACKCANCEL;
        }
        else
        {
            return LAYERDRAW_NORMAL;
        }
    }
}

bool __stdcall CSlimLayer::GetExtent(WKSRect& fullext) const
{
    fullext = m_DataExtent;
    return true;
}

MapUnits __stdcall CSlimLayer::GetMapUnit() const
{
    return m_GeoColumnInfo.MapUnit; 
}

bool __stdcall CSlimLayer::GetBaseScale(double& scale) const
{
    scale = m_GeoColumnInfo.BaseScale;
    return true;
}

const char* __stdcall CSlimLayer::GetSpatialReference() const
{
    if (!this->Valid()) {return "";}

    string sr;
    m_pFeatureData->MovePos(OFFSET_SR);
    m_pFeatureData->Read(sr);
    (string)m_SR = sr;
    return m_SR.c_str();
}

void CSlimLayer::GetPrecision(double& precision) const
{
    precision = m_GeoColumnInfo.ToleranceXY;
}

void CSlimLayer::SetRefScale(const double& scale)
{
    m_ReferenceScale = scale;
}

void CSlimLayer::GetRefScale(double& scale) const
{
    scale = m_ReferenceScale;
}

bool CSlimLayer::SetDefaultSymbol(const ISymbolPtr pSymbol, const bool save2esd)
{
    if (!pSymbol.Assigned()) {return false;}

    SymbolType symtype = pSymbol->GetSymbolType();
    if (VECTORFEATURETYPE_TEXT == m_GeoColumnInfo.FeatureType)
    {
        if (SYMBOLTYPE_TEXT != symtype) {return false;}
    }
    else
    {
        switch (m_GeoColumnInfo.ShpType)
        {
        case SHAPETYPE_POINT:
        case SHAPETYPE_MULTIPOINT:
            if (SYMBOLTYPE_POINT != symtype) {return false;}
            break;

        case SHAPETYPE_POLYLINE:
            if (SYMBOLTYPE_LINE != symtype) {return false;}
            break;

        case SHAPETYPE_POLYGON:
            if (SYMBOLTYPE_FILL != symtype) {return false;}
            break;

        default:
            return false;
        }
    }

    IObjPtr pObj;
    CLONE_PTR(pSymbol, pObj)
    CAST_PTR(pObj, m_pDefaultSymbol, ISymbol)

    if (save2esd && !this->ReadOnly())
    {
        this->Symbols2Stream();
        this->WriteModifyTime();
    }

    return true;
}

bool CSlimLayer::GetDefaultSymbol(ISymbolPtr& pSymbol) const
{
    if (!m_pDefaultSymbol.Assigned()) {return false;}

    IObjPtr pObj;
    CLONE_PTR(m_pDefaultSymbol, pObj);
    CAST_PTR(pObj, pSymbol, ISymbol)

    return true;
}

void CSlimLayer::SetRendererType(const SlimRendererType renderertype, const bool save2esd)
{
    m_RendererType = renderertype;
    if (save2esd && !this->ReadOnly())
    {
        this->Symbols2Stream();
        this->WriteModifyTime();
    }
}

SlimRendererType CSlimLayer::GetRendererType() const
{
    return m_RendererType;
}

bool CSlimLayer::SetSymbol(const string& key, const ISymbolPtr pSymbol)
{
    if (!pSymbol.Assigned()) return false;
    IObjPtr pObj;
    CLONE_PTR(pSymbol, pObj)
    ISymbolPtr pS;
    CAST_PTR(pObj, pS, ISymbol)
    m_SymbolMap[key] = pS;

    return true;
}

bool CSlimLayer::GetSymbol(const string& key, ISymbolPtr& pSymbol) const
{
    pSymbol.Clear();
    map<string, ISymbolPtr>::const_iterator it = m_SymbolMap.find(key);
    if (it == m_SymbolMap.end()) return false;
    IObjPtr pObj;
    CLONE_PTR(it->second, pObj)
    CAST_PTR(pObj, pSymbol, ISymbol)
    return true;
}

bool CSlimLayer::GetSymbolByIndex(const dword index, string& key, ISymbolPtr& pSymbol) const
{
    pSymbol.Clear();
    if (m_SymbolMap.size() <= index) return false;
    map<string, ISymbolPtr>::const_iterator it = m_SymbolMap.begin();
    std::advance(it, index);
    key = it->first;
    IObjPtr pObj;
    CLONE_PTR(it->second, pObj)
    CAST_PTR(pObj, pSymbol, ISymbol)
    return true;
}

dword CSlimLayer::GetSymbolCount() const
{
    return m_SymbolMap.size();
}

void CSlimLayer::ClearSymbols()
{
    m_SymbolMap.clear();

    if (!this->ReadOnly())
    {
        this->Symbols2Stream();
        this->WriteModifyTime();
    }
}

bool CSlimLayer::SetRendererField(const long fieldindex, const bool save2esd)
{
    CFieldsPtr pFields;
    this->GetFields(pFields);
    if (pFields->GetFieldCount() <= fieldindex) return false;
    m_RendererField = fieldindex;

    if (save2esd && !this->ReadOnly())
    {
        this->Symbols2Stream();
        this->WriteModifyTime();
    }

    return true;
}

long CSlimLayer::GetRendererField() const
{
    return m_RendererField;
}

void CSlimLayer::SetShowDefaultSymbol(const bool showdefaultsymbol, const bool save2esd)
{
    m_ShowDefaultSymbol = showdefaultsymbol;

    if (save2esd && !this->ReadOnly())
    {
        this->Symbols2Stream();
        this->WriteModifyTime();
    }
}

bool CSlimLayer::GetShowDefaultSymbol() const
{
    return m_ShowDefaultSymbol;
}

bool CSlimLayer::GetFids(vector<dword>& fids) const
{
    fids.clear();
    if (!this->Valid()) {return false;}

    map<dword, dword>::const_iterator it = m_FidIndex.begin();
    while (it != m_FidIndex.end())
    {
        fids.push_back(it->first);
        it++;
    }

    return true;
}

dword CSlimLayer::GetFeatureCount() const
{
    return m_FeatureCount;
}

dword CSlimLayer::AddFeature(const IGeometryPtr pGeometry,
    const string& fieldvalues, const string& annotation)
{
    if (!pGeometry.Assigned() || this->ReadOnly()) {return 0;}
    if (!this->Valid()) {return false;}

    Feature newfeature;
    newfeature.geometry = pGeometry;
    bool mbrflag = true;
    if (!newfeature.geometry->GetMBR(newfeature.mbr))
    {
        mbrflag = false;
        newfeature.mbr.left = newfeature.mbr.top = newfeature.mbr.right = newfeature.mbr.bottom = 0;
    }
    newfeature.fieldvalues = fieldvalues;
    newfeature.annotation = annotation;
    newfeature.fid = ++m_MaxFID;

    //在block末端增加一个新feature
    dword delta = m_pFeatureData->GetSize();
    m_pFeatureData->MovePos(delta);
    this->Feature2Stream(newfeature);
    //搞定空间索引
    m_pSpatialIndex->AddItem(delta, newfeature.mbr);
    //搞定fid索引
    m_FidIndex[m_MaxFID] = delta;

    m_FeatureCount++;

    if (mbrflag)
    {
        if (1 == m_FeatureCount)
        {
            ::memcpy(&m_DataExtent, &newfeature.mbr, sizeof(WKSRect));
        }
        else
        {
            UpdateFullExtent(m_DataExtent, newfeature.mbr);
        }
    }

    m_pFeatureData->MovePos(OFFSET_DATAEXTENT);
    m_pFeatureData->Write(&m_DataExtent, sizeof(WKSRect));
    m_pFeatureData->MovePos(OFFSET_MAXFID);
    m_pFeatureData->Write(m_MaxFID);
    m_pFeatureData->MovePos(OFFSET_FEATURECOUNT);
    m_pFeatureData->Write(m_FeatureCount);

    //将这次原子操作添加到m_EditProc中
    EditAtom ea;
    ea.edittype = SLIMEDIT_NEW;
    ea.fid = m_MaxFID;
    ea.newdelta = delta;
    ea.newmbr = newfeature.mbr;
    m_EditProc.atoms.push_back(ea);

    m_ModifyDirty = true;

    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_ADDED, m_MaxFID)

    return m_MaxFID;
}

bool CSlimLayer::SetFeature(const dword fid, const IGeometryPtr pGeometry,
    const string& fieldvalues, const string& annotation)
{
    if (!pGeometry.Assigned() || this->ReadOnly()) {return false;}
    if (!this->Valid()) {return false;}

    map<dword, dword>::const_iterator it = m_FidIndex.find(fid);
    if (it == m_FidIndex.end()) {return false;}

    //读出老delta
    dword olddelta = it->second;
    m_pFeatureData->MovePos(olddelta + sizeof(dword));
    WKSRect oldmbr;
    m_pFeatureData->Read(&oldmbr, sizeof(WKSRect));

    //干掉空间索引中的老东东，等会记录到m_EditProc中
    m_pSpatialIndex->DeleteItem(it->second);

    Feature newfeature;
    newfeature.geometry = pGeometry;

    bool mbrflag = true;
    if (!newfeature.geometry->GetMBR(newfeature.mbr))
    {
        mbrflag = false;
        newfeature.mbr.left = newfeature.mbr.top = newfeature.mbr.right = newfeature.mbr.bottom = 0;
    }

    newfeature.fieldvalues = fieldvalues;
    newfeature.annotation = annotation;
    newfeature.fid = fid;

    //在block末端增加一个新feature
    dword delta = m_pFeatureData->GetSize();
    m_pFeatureData->MovePos(delta);
    this->Feature2Stream(newfeature);
    //搞定新的空间索引
    m_pSpatialIndex->AddItem(delta, newfeature.mbr);
    //搞定fid索引
    m_FidIndex[fid] = delta;

    if (mbrflag)
    {
        if (1 == m_FeatureCount)
        {
            ::memcpy(&m_DataExtent, &newfeature.mbr, sizeof(WKSRect));
        }
        else
        {
            UpdateFullExtent(m_DataExtent, newfeature.mbr);
        }
    }

    m_pFeatureData->MovePos(OFFSET_DATAEXTENT);
    m_pFeatureData->Write(&m_DataExtent, sizeof(WKSRect));

    //将这次原子操作添加到m_EditProc中
    EditAtom ea;
    ea.edittype = SLIMEDIT_MODIFY;
    ea.fid = fid;
    ea.olddelta = olddelta;
    ea.newdelta = delta;
    ea.oldmbr = oldmbr;
    ea.newmbr = newfeature.mbr;
    m_EditProc.atoms.push_back(ea);

    m_ModifyDirty = true;

    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_MODIFIED, fid)

    return true;
}

bool CSlimLayer::GetFeature(const dword fid, IGeometryPtr& pGeometry,
    string& fieldvalues, string& annotation)
{
    if (!this->Valid()) {return false;}

    map<dword, dword>::const_iterator it = m_FidIndex.find(fid);
    if (it == m_FidIndex.end()) {return false;}

    this->RereadIndexStream();

    dword delta = it->second;
    m_pFeatureData->MovePos(delta);
    Feature feature;
    (const_cast<CSlimLayer*>(this))->Stream2Feature(feature);

    pGeometry = feature.geometry;
    fieldvalues = feature.fieldvalues;
    annotation = feature.annotation;

    return true;
}

bool CSlimLayer::GetFeatureGeometry(const dword fid, IGeometryPtr& pGeometry) const
{
    pGeometry.Clear();
    if (!this->Valid()) {return false;}

    map<dword, dword>::const_iterator it = m_FidIndex.find(fid);
    if (it == m_FidIndex.end()) {return false;}

    (const_cast<CSlimLayer*>(this))->RereadIndexStream();

    dword delta = it->second;
    m_pFeatureData->MovePos(delta + sizeof(long) + sizeof(WKSRect));
    Stream2Geometry(m_pFeatureData, pGeometry);

    return true;
}

bool CSlimLayer::GetFeatureMBR(const dword fid, WKSRect& mbr) const
{
    if (!this->Valid()) {return false;}

    map<dword, dword>::const_iterator it = m_FidIndex.find(fid);
    if (it == m_FidIndex.end()) {return false;}

    dword delta = it->second;
    m_pFeatureData->MovePos(delta + sizeof(dword));
    m_pFeatureData->Read(&mbr, sizeof(WKSRect));
    return true;
}

bool CSlimLayer::DeleteFeature(const dword fid)
{
    if (!this->Valid() || this->ReadOnly()) {return false;}

    map<dword, dword>::iterator it = m_FidIndex.find(fid);
    if (it == m_FidIndex.end()) {return false;}

    dword olddelta = it->second;
    m_pFeatureData->MovePos(olddelta + sizeof(dword));
    WKSRect oldmbr;
    m_pFeatureData->Read(&oldmbr, sizeof(WKSRect));

    //干掉索引中的项就行了
    m_FidIndex.erase(it);
    m_pSpatialIndex->DeleteItem(olddelta, &oldmbr);

    //干掉选择集中的东东
    list<dword>::iterator first_it = m_SelectedFIDs.begin();
    list<dword>::iterator last_it = m_SelectedFIDs.end();
    list<dword>::iterator selectit= std::find(first_it, last_it, fid);
    if (selectit != last_it)
    {
        m_SelectedFIDs.erase(selectit);
    }

    m_pFeatureData->MovePos(OFFSET_FEATURECOUNT);
    m_pFeatureData->Write(--m_FeatureCount);

    //将这次原子操作添加到m_EditProc中
    EditAtom ea;
    ea.edittype = SLIMEDIT_DELETE;
    ea.fid = fid;
    ea.olddelta = olddelta;
    ea.oldmbr = oldmbr;
    m_EditProc.atoms.push_back(ea);

    m_ModifyDirty = true;

    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_DELETED, fid)

    return true;
}

bool CSlimLayer::GetFeature(const dword fid, IVectorFeaturePtr& pFeature)
{
    pFeature.Clear();
    if (!this->Valid()) {return false;}

    map<dword, dword>::const_iterator it = m_FidIndex.find(fid);
    if (it == m_FidIndex.end()) {return false;}

    dword delta = it->second;
    m_pFeatureData->MovePos(delta);
    Feature feature;
    (const_cast<CSlimLayer*>(this))->Stream2Feature(feature);

    CSlimFeaturePtr pSF = new CSlimFeature;
    pSF->m_fid = fid;
    pSF->m_geometry = feature.geometry._p();
    pSF->m_slimlayer = (const_cast<CSlimLayer*>(this));
    pSF->SetFieldValues(feature.fieldvalues);
    pSF->SetAnnotation(feature.annotation.c_str());
    CAST_PTR(pSF, pFeature, IVectorFeature)

    return true;
}

bool CSlimLayer::CreateFeature(IVectorFeaturePtr& pFeature)
{
    if (!this->Valid()) {return false;}
    if (this->ReadOnly()) {return false;}

    CSlimFeaturePtr pSF = new CSlimFeature;
    pSF->m_slimlayer = (const_cast<CSlimLayer*>(this));
    CFieldsPtr pFields;
    this->GetFields(pFields);
    pSF->m_pFieldValues = new CFieldValues(pFields);
    CAST_PTR(pSF, pFeature, CSlimFeature)

    return true;
}

bool __stdcall CSlimLayer::SetUndoPoint()
{
    if (!this->Valid()) {return false;}

    long listsize = m_UndoProcs.size();
    for (long i = m_UndoIndex + 1; i < listsize; i++)
    {
        m_UndoProcs.pop_back();
    }

    m_UndoProcs.push_back(m_EditProc);
    m_UndoIndex++;

    if (!m_SaveDirty)
    {
        m_SaveDirty = (m_EditProc.atoms.size() > 0) ? true : false;
    }

    m_EditProc.atoms.clear();
    return true;
}

bool __stdcall CSlimLayer::EditUndoable() const
{
    if (!this->Valid()) {return false;}

    return (0 < m_UndoIndex) ? true : false;
}

bool __stdcall CSlimLayer::EditRedoable() const
{
    if (!this->Valid()) {return false;}

    long ls = m_UndoProcs.size() - 1;
    return (ls > m_UndoIndex) ? true : false;
}

bool CSlimLayer::EditUndoRestoreIndex(const EditProcess& ep)
{
    for (dword i = 0; i < ep.atoms.size(); i++)
    {
        EditAtom ea = ep.atoms[i];
        switch(ea.edittype)
        {
        case SLIMEDIT_NEW:
            {
                map<dword, dword>::iterator new_it =  m_FidIndex.find(ea.fid);
                m_FidIndex.erase(new_it);
                m_pSpatialIndex->DeleteItem(ea.newdelta, &ea.newmbr);
            }
            break;

        case SLIMEDIT_MODIFY:
            {
                m_pSpatialIndex->DeleteItem(ea.newdelta, &ea.newmbr);
                m_pSpatialIndex->AddItem(ea.olddelta, ea.oldmbr);
                m_FidIndex[ea.fid] = ea.olddelta;
            }
            break;

        case SLIMEDIT_DELETE:
            {
                m_pSpatialIndex->AddItem(ea.olddelta, ea.oldmbr);
                m_FidIndex[ea.fid] = ea.olddelta;
            }
            break;

        default:
            {
                return false;
            }
        }
    }

    return true;
}

bool CSlimLayer::EditRedoRestoreIndex(const EditProcess& ep)
{
    for (dword i = 0; i < ep.atoms.size(); i++)
    {
        EditAtom ea = ep.atoms[i];
        switch(ea.edittype)
        {
        case SLIMEDIT_NEW:
            {
                m_pFeatureData->MovePos(ea.newdelta);
                CSlimLayer::Feature new_fea;
                this->Stream2Feature(new_fea);
                m_pSpatialIndex->AddItem(ea.newdelta, new_fea.mbr);
                m_FidIndex[ea.fid] = ea.newdelta;
            }
            break;

        case SLIMEDIT_MODIFY:
            {
                m_pSpatialIndex->DeleteItem(ea.olddelta, &ea.oldmbr);
                m_pSpatialIndex->AddItem(ea.newdelta, ea.newmbr);
                m_FidIndex[ea.fid] = ea.newdelta;
            }
            break;

        case SLIMEDIT_DELETE:
            {
                map<dword, dword>::iterator del_it =  m_FidIndex.find(ea.fid);
                m_FidIndex.erase(del_it);
                m_pSpatialIndex->DeleteItem(ea.olddelta, &ea.oldmbr);

            }
            break;

        default:
            {
                return false;
            }
        }
    }

    return true;
}

void CSlimLayer::CleanEditUndo()
{
    this->EditUndoRestoreIndex(m_EditProc);
    m_EditProc.atoms.clear();
}

void CSlimLayer::WriteModifyTime()
{
    SYSTEMTIME systemtime;
    ::GetSystemTime(&systemtime);
    FILETIME filetime;
    ::SystemTimeToFileTime(&systemtime, &filetime);
    m_pFeatureData->MovePos(OFFSET_LASTMODIFYTIME);
    m_pFeatureData->Write(&filetime, sizeof(FILETIME));
}

bool CSlimLayer::ModifiedByOther()
{
    if (!this->ReadOnly()) {return false;}

    FILETIME filetime;
    m_pFeatureData->MovePos(OFFSET_LASTMODIFYTIME);
    m_pFeatureData->Read(&filetime, sizeof(FILETIME));

    if ((m_LastTime.dwHighDateTime == filetime.dwHighDateTime)
        && ((m_LastTime.dwLowDateTime == filetime.dwLowDateTime)))
    {
        return false;
    }
    else
    {
        m_LastTime = filetime;
        return true;
    }
}

void CSlimLayer::RereadIndexStream()
{
    if (!this->ModifiedByOther())
    {
        return;
    }

    CFileStreamPtr pFMS;
    CAST_PTR(m_pFeatureData, pFMS, CFileStream)
    pFMS->ReMap();

    this->ReloadFixedHeader();
    this->DataStream2Index();

//    this->Stream2Symbols();
}

bool __stdcall CSlimLayer::EditUndo()
{
    if (!this->Valid()) {return false;}

    //注意，这里扔掉所有未setundopoint的
    if (0 < m_EditProc.atoms.size())
    {
        this->CleanEditUndo();
    }

    if (0 >= m_UndoIndex)
    {
        return false;
    }

    m_SelectedFIDs.clear();

    EditProcess ep = m_UndoProcs[m_UndoIndex--];
    this->EditUndoRestoreIndex(ep);

    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_UNDO, -1)

    return true;
}

bool __stdcall CSlimLayer::EditRedo()
{
    if (!this->Valid()) {return false;}

    //注意，这里扔掉所有未setundopoint的
    if (0 < m_EditProc.atoms.size())
    {
        this->CleanEditUndo();
    }

    m_SelectedFIDs.clear();

    long listsize = m_UndoProcs.size();
    if (listsize - 1 <= m_UndoIndex)
    {
        return false;
    }

    EditProcess ep = m_UndoProcs[++m_UndoIndex];
    this->EditRedoRestoreIndex(ep);

    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_REDO, -1)

    return true;
}

bool __stdcall CSlimLayer::EditCancel()
{
    if (!this->Valid()) {return false;}
    if (m_UndoProcs.size() <= 0) {return false;}

    m_SelectedFIDs.clear();

    if (m_ModifyDirty)
    {
        //只有修改过才需要刷新索引struct
        this->DataStream2Index();
        m_ModifyDirty = false;
    }

    m_UndoProcs.clear();
    EditProcess ep;
    m_UndoProcs.push_back(ep);
    m_UndoIndex = 0;

    //取消未SetUndoPoint的一切修改
    m_EditProc.atoms.clear();

    m_SaveDirty = false;

    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_EDITCANCELED, -1)

    return true;
}

bool __stdcall CSlimLayer::SaveData()
{
    if (!this->Valid()) {return false;}

    this->SetUndoPoint();
    if (m_ModifyDirty)
    {
        //只有修改过才需要刷新索引stream
        this->Index2DataStream();
        m_ModifyDirty = false;
    }

    m_UndoProcs.clear();
    EditProcess ep;
    m_UndoProcs.push_back(ep);
    m_UndoIndex = 0;

    this->WriteModifyTime();

    m_SaveDirty = false;

    //发送消息给每个listener
    DISPATCH_LONG_TOALL(MESSAGE_VECTORLAYER_EDITSAVED, -1)

    return true;
}

bool __stdcall CSlimLayer::IsDirty() const
{
    if (!this->EditUndoable())
    {
        //都undo到头了
//        return false;
    }

    return m_SaveDirty;
}

bool CSlimLayer::RapidModifyPoint(const dword fid, const WKSPoint& point)
{
    if (this->ReadOnly())
        return false;

    if (m_GeoColumnInfo.ShpType != SHAPETYPE_POINT)
        return false;

    map<dword, dword>::const_iterator it = m_FidIndex.find(fid);
    if (it == m_FidIndex.end()) 
        return false;

    m_pAuxPoint->SetX(point.x);
    m_pAuxPoint->SetY(point.y);
    IGeometryPtr pGeom = m_pAuxPoint._p();
    WKSRect mbr;
    mbr.right = mbr.left = point.x;
    mbr.top = mbr.bottom = point.y;

    dword delta = it->second;
    m_pFeatureData->MovePos(delta + sizeof(dword));
    m_pFeatureData->Write(&mbr, sizeof(WKSRect));
    Geometry2Stream(pGeom, m_pFeatureData);

    //干掉空间索引中的老东东
    m_pSpatialIndex->DeleteItem(delta);
    //搞定新的空间索引
    m_pSpatialIndex->AddItem(delta, mbr);

    return true;
}

dword CSlimLayer::Select(const WKSPoint& point, const bool append)
{
    if (!this->Valid()) {return 0;}

    WKSRect envelope;
    envelope.left = envelope.right = point.x;
    envelope.top = envelope.bottom = point.y;

    return this->Select(envelope, true, append);
}

dword __stdcall CSlimLayer::Select(const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    if (!this->Valid()) {return 0;}

    if (!append)
    {
        m_SelectedFIDs.clear();
    }

    WKSRect env = envelope;
    CorrectEnvelope(env);
    vector<dword> selection;
    this->SelectObjectsByEnvelope(selection, env, partialselect);
    this->Select(selection);

    return (dword)selection.size();
}

dword CSlimLayer::Select(const vector<dword>& fids, const bool append)
{
    if (!this->Valid()) {return 0;}
    if (!append) {m_SelectedFIDs.clear();}

    dword selectcount = 0;

    vector<dword>::const_iterator it_fids = fids.begin();
    while (it_fids != fids.end())
    {
        dword fid = *it_fids;
        map<dword, dword>::const_iterator it = m_FidIndex.find(fid);
        if (it != m_FidIndex.end())
        {
            selectcount++;
            list<dword>::const_iterator first_it = m_SelectedFIDs.begin();
            list<dword>::const_iterator last_it = m_SelectedFIDs.end();
            list<dword>::const_iterator found_it = std::find(first_it, last_it, fid);
            if (found_it == last_it)
            {
                //注意这里是对的
                m_SelectedFIDs.push_back(fid);
            }
        }
        it_fids++;
    }

    return selectcount;
}

void __stdcall CSlimLayer::ClearSelection()
{
    m_SelectedFIDs.clear();
}

dword CSlimLayer::GetSelection(vector<dword>& fids) const
{
    if (!this->Valid()) {return 0;}

    fids.clear();

    list<dword>::const_iterator it = m_SelectedFIDs.begin();
    while (it != m_SelectedFIDs.end())
    {
        fids.push_back(*it);
        it++;
    }

    return (dword)fids.size();
}

dword CSlimLayer::Deselect(const WKSPoint& point)
{
    if (!this->Valid()) {return 0;}

    WKSRect envelope;
    envelope.left = envelope.right = point.x;
    envelope.top = envelope.bottom = point.y;
    return this->Deselect(envelope, true);
}

dword __stdcall CSlimLayer::Deselect(const WKSRect& envelope, const bool partialselect)
{
    if (!this->Valid()) {return 0;}

    WKSRect env = envelope;
    CorrectEnvelope(env);
    vector<dword> resultfids;
    this->SelectObjectsByEnvelope(resultfids, env, partialselect);
    return this->Deselect(resultfids);
}

dword CSlimLayer::Deselect(const vector<dword>& fids)
{
    if (!this->Valid()) {return 0;}

    dword selectcount = 0;

    vector<dword>::const_iterator it_fids = fids.begin();
    while (it_fids != fids.end())
    {
        dword fid = *it_fids;
        list<dword>::iterator first_it = m_SelectedFIDs.begin();
        list<dword>::iterator last_it = m_SelectedFIDs.end();
        list<dword>::iterator found_it = std::find(first_it, last_it, fid);
        if (found_it != last_it)
        {
            m_SelectedFIDs.erase(found_it);
            selectcount++;
        }
        it_fids++;
    }
    return selectcount;
}

dword __stdcall CSlimLayer::GetSelectCount() const
{
    return (dword)m_SelectedFIDs.size();
}

bool CSlimLayer::Identify(vector<dword>& resultfids, const WKSRect& envelope,
    const bool partialselect)
{
    if (!this->Valid()) {return false;}
    (const_cast<CSlimLayer*>(this))->RereadIndexStream();

    WKSRect env = envelope;
    CorrectEnvelope(env);
    this->SelectObjectsByEnvelope(resultfids, env, partialselect);
    return true;
}

bool CSlimLayer::ImpreciseSearch(const WKSRect& extent, vector<dword>& fids)
{
    if (!this->Valid()) {return false;}
    (const_cast<CSlimLayer*>(this))->RereadIndexStream();

    WKSRect env = extent;
    CorrectEnvelope(env);

    CSpatialIndex::CSISearchResultPtr pSIResult;
    m_pSpatialIndex->Search(pSIResult, &env);
    dword delta = 0;
    while (pSIResult->Next(delta))
    {
        m_pFeatureData->MovePos(delta);
        dword fid;
        m_pFeatureData->Read(fid);
        WKSRect mbr;
        m_pFeatureData->Read(&mbr, sizeof(WKSRect));
        if (!EnvelopesTouched(mbr, env))
        {
            continue;
        }

        fids.push_back(fid);
    }

    return true;
}

bool CSlimLayer::SetFields(const string& fields)
{
    if (!this->Valid()) {return false;}
    if (this->ReadOnly()) {return false;}
    if (fields.size() > 1990) {return false;}

    ::strcpy(m_Header.FieldsInfo, fields.c_str());
    m_pFeatureData->MovePos(0);
    m_pFeatureData->Write(&m_Header, sizeof(SlimHeader));
    this->WriteModifyTime();

    return true;
}

void CSlimLayer::GetFields(string& fields) const
{
    fields = m_Header.FieldsInfo;
}

void CSlimLayer::GetFields(CFieldsPtr& pFields) const
{
    pFields = new CFields;
    string sa;
    this->GetFields(sa);
    Strings strings;
    strings.SetText(sa);
    for (long i = 0; i < (long)strings.GetLineCount(); i++)
    {
        string fieldname;
        strings.GetLine(i, fieldname);
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
        pFields->AddField(pField);
    }
}

bool CSlimLayer::SetDisplayField(const long fieldindex)
{
    if (!this->Valid()) {return false;}
    if (this->ReadOnly()) {return false;}

    m_Header.DisplayField = fieldindex;
    m_pFeatureData->MovePos(0);
    m_pFeatureData->Write(&m_Header, sizeof(SlimHeader));
    this->WriteModifyTime();

    return true;
}

long CSlimLayer::GetDisplayField() const
{
    return m_Header.DisplayField;
}

bool CSlimLayer::SetAlias(const string& alias)
{
    if (!this->Valid()) {return false;}
    if (this->ReadOnly()) {return false;}
    if (alias.size() > 100) {return false;}

    ::strcpy(m_Header.Alias, alias.c_str());
    m_pFeatureData->MovePos(0);
    m_pFeatureData->Write(&m_Header, sizeof(SlimHeader));
    this->WriteModifyTime();

    return true;
}

string CSlimLayer::GetAlias() const
{
    return m_Header.Alias;
}

void CSlimLayer::GetGeometryColumnInfo(GeometryColumnInfo& geocolinfo) const
{
    geocolinfo = m_GeoColumnInfo;
}

bool CSlimLayer::SetSR(const string& sr)
{
    if (!this->Valid()) {return false;}
    if (this->ReadOnly()) {return false;}

    dword size = sr.length();
    if (2990 < size) {return false;}

    m_pFeatureData->MovePos(OFFSET_SR);
    m_pFeatureData->Write(sr);
    this->WriteModifyTime();
    return true;
}

bool CSlimLayer::AttachToFile(const string& filename, const bool filemap)
{
    if (!this->Valid()) {return false;}

    CFileStreamPtr pFMS;
    if (filemap)
    {
        pFMS = new CFileMapStream(filename.c_str(), false);
    }
    else
    {
        pFMS = new CSimpleFileStream(filename.c_str(), false);
    }

    string namegot = pFMS->GetMapFileName();
    if (namegot == "") {return false;}

    this->WriteModifyTime();

    pFMS->MovePos(0);
    m_pFeatureData->MovePos(0);
    dword streamsize = m_pFeatureData->GetSize();
    CStreamPtr pStream;
    CAST_PTR(pFMS, pStream, CStream)
    dword size = m_pFeatureData->GetSize();
    dword size1 = m_pFeatureData->CopyDataTo(pStream, streamsize);
    if (size != size1) {return false;}

    if (!this->CheckHeader())
    {
        m_pFeatureData = NULL;
        return false;
    }

    m_pFeatureData = pStream;

    return true;
}

bool CSlimLayer::GetSlimData(CStreamPtr pStream)
{
    if (!this->Valid()) {return false;}
    if (!pStream.Assigned()) {return false;}

    m_pFeatureData->MovePos(0);
    dword streamsize = m_pFeatureData->GetSize();
    dword size = m_pFeatureData->GetSize();
    dword size1 = m_pFeatureData->CopyDataTo(pStream, streamsize);
    if (size != size1) {return false;}

    return true;
}

string CSlimLayer::GetFileName() const
{
    if (!this->Valid()) {return "";}

    CFileStreamPtr pFMS;
    CAST_PTR(m_pFeatureData, pFMS, CFileStream)
    if (!pFMS.Assigned()) {return "";}

    string filename = pFMS->GetMapFileName();
    return filename;
}

bool CSlimLayer::ReadOnly() const
{
    if (!this->Valid()) {return false;}

    return m_pFeatureData->ReadOnly();
}

bool _check_nettopo_field(const CFieldsPtr pFields, const long fieldindex)
{
    if (fieldindex < 0)
    {
        return true;
    }

    CFieldPtr pField;
    if (!pFields->GetField(fieldindex, pField))
    {
        return false;
    }

    if ((pField->GetFieldType() == FIELDTYPE_SHORT)
        || (pField->GetFieldType() == FIELDTYPE_LONG)
        || (pField->GetFieldType() == FIELDTYPE_SINGLE)
        || (pField->GetFieldType() == FIELDTYPE_DOUBLE))
    {
        return true;
    }

    return false;
}

double _get_nettopo_fieldvalue(const IVectorFeaturePtr pFeature, const dword fieldindex)
{
    IFieldValuePtr pFieldValue;
    if (!pFeature->GetFieldValue(fieldindex, pFieldValue._ref()))
    {
        return 0;
    }

    double weight = 0;
    if (!pFieldValue->GetFloat(weight))
    {
        long int_weight;
        pFieldValue->GetInteger(int_weight);
        weight = int_weight;
    }

    return weight;
}

void _get_nettopo_polyline(const IVectorFeaturePtr pFeature, IPolylinePtr& pPolyline)
{
    IGeometryPtr pGeometry;
    pFeature->GetGeometryRef(pGeometry._ref());
    CAST_PTR(pGeometry, pPolyline, IPolyline)
}

void _get_nettopo_from_to(const IPolylinePtr pPolyline, const dword index, 
    WKSPoint& from, WKSPoint& to, IPathPtr& pPath)
{
    pPolyline->GetPathRef(pPath._ref(), index);
    WKSPointZ pnt;
    pPath->GetPoint1(0, pnt);
    from = pnt;
    dword last = pPath->GetPointCount();
    pPath->GetPoint1(last - 1, pnt);
    to = pnt;
}

void CSlimLayer::SetNetTolerance(const double tolerance)
{
    m_Net.SetTolerance(tolerance);
}

double CSlimLayer::GetNetTolerance() const
{
    return m_Net.GetTolerance();
}

bool CSlimLayer::CreateNetTopo(const long field, const bool bidirectional)
{
    this->ClearNetTopo();

    GeometryColumnInfo geocolinfo;
    this->GetGeometryColumnInfo(geocolinfo);
    if (geocolinfo.ShpType != SHAPETYPE_POLYLINE)
    {
        return false;
    }

    CFieldsPtr pFields;
    this->GetFields(pFields);
    if (!_check_nettopo_field(pFields, field))
    {
        return false;
    }

    m_FastShortest = false;
    if (field < 0)
    {
        m_FastShortest = true;
    }

    vector<dword> fids;
    this->GetFids(fids);
    for (long i = 0; i < fids.size(); i++)
    {
        dword fid = fids[i];
        IVectorFeaturePtr pFeature;
        this->GetFeature(fid, pFeature);
        IPolylinePtr pPolyline;
        _get_nettopo_polyline(pFeature, pPolyline);
        dword pathcount = pPolyline->GetPathCount();
        for (dword j = 0; j < pathcount; j++)
        {
            WKSPoint from, to;
            IPathPtr pPath;
            _get_nettopo_from_to(pPolyline, j, from, to, pPath);
            double weight = 0;
            if (m_FastShortest)
            {
                pPath->GetLength(weight);
            }
            else
            {
                weight = _get_nettopo_fieldvalue(pFeature, field);
            }

            m_Net.add_edge(from, to, weight, fid, bidirectional);
        }
    }

    return true;
}

bool CSlimLayer::CreateNetTopo2(const dword field_from_to, const dword field_to_from)
{
    this->ClearNetTopo();

    GeometryColumnInfo geocolinfo;
    this->GetGeometryColumnInfo(geocolinfo);
    if (geocolinfo.ShpType != SHAPETYPE_POLYLINE)
    {
        return false;
    }

    CFieldsPtr pFields;
    this->GetFields(pFields);
    if (!_check_nettopo_field(pFields, field_from_to)
        || !_check_nettopo_field(pFields, field_to_from))
    {
        return false;
    }

    vector<dword> fids;
    this->GetFids(fids);
    for (long i = 0; i < fids.size(); i++)
    {
        dword fid = fids[i];
        IVectorFeaturePtr pFeature;
        this->GetFeature(fid, pFeature);
        IPolylinePtr pPolyline;
        _get_nettopo_polyline(pFeature, pPolyline);
        dword pathcount = pPolyline->GetPathCount();
        for (dword j = 0; j < pathcount; j++)
        {

            WKSPoint from, to;
            IPathPtr pPath;
            _get_nettopo_from_to(pPolyline, j, from, to, pPath);
            double weight = _get_nettopo_fieldvalue(pFeature, field_from_to);
            m_Net.add_edge(from, to, weight, fid, false);
            weight = _get_nettopo_fieldvalue(pFeature, field_to_from);
            m_Net.add_edge(to, from, weight, fid, false);
        }
    }

    m_FastShortest = false;
    return true;
}

bool CSlimLayer::AddNetRoute(const WKSPoint& route)
{
    return m_Net.add_route(route);
}

bool CSlimLayer::RemoveNetRoute(const WKSPoint& route)
{
    return m_Net.remove_route(route);
}

bool CSlimLayer::GetNetRoutes(IMultiPoint** ppRoutes) const
{
    if (_invalid(ppRoutes)) return false;
    assert(!*ppRoutes);

    *ppRoutes = new CMultiPoint;
    (*ppRoutes)->_AddRef();
    for (dword i = 0; i < m_Net.get_routecount(); i++)
    {
        Vertex v = m_Net.get_route(i);
        WKSPointZ point(v);;
        (*ppRoutes)->AddPoint(point);
    }

    return true;
}

void CSlimLayer::ClearNetRoutes()
{
    m_Net.clear_routes();
}

bool CSlimLayer::AddNetBarrierPoint(const WKSPoint& barrier)
{
    return m_Net.add_barrier_vertex(barrier);
}

bool CSlimLayer::RemoveNetBarrierPoint(const WKSPoint& barrier)
{
    return m_Net.remove_barrier_vertex(barrier);
}

bool CSlimLayer::GetNetBarrierPoints(IMultiPoint** ppBarriers) const
{
    if (_invalid(ppBarriers)) return false;
    assert(!*ppBarriers);

    *ppBarriers = new CMultiPoint;
    (*ppBarriers)->_AddRef();
    for (dword i = 0; i < m_Net.get_barrier_vertexcount(); i++)
    {
        Vertex v = m_Net.get_barrier_vertex(i);
        WKSPointZ point(v);;
        (*ppBarriers)->AddPoint(point);
    }

    return true;
}

void CSlimLayer::ClearNetBarrierPoints()
{
    m_Net.clear_barrier_vertexes();
}

bool CSlimLayer::AddNetBlockedBiEdge(const dword fid)
{
    dword r = m_Net.add_blocked_edges(fid);
    return (r > 0);
}

bool CSlimLayer::AddNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to)
{
    net::Edge e;
    return m_Net.add_blocked_edge(from, to, e);
}

bool CSlimLayer::RemoveNetBlockedBiEdge(const dword fid)
{
    if (m_Net.remove_blocked_edges(fid) > 0)
    {
        return true;
    }

    return false;
}

bool CSlimLayer::RemoveNetBlockedSingleEdge(const WKSPoint& from, const WKSPoint& to)
{
    return false;
}

dword CSlimLayer::GetNetBlockedEdgeCount() const
{
    return m_Net.get_blocked_edgecount();
}

bool CSlimLayer::GetNetBlockedEdgeByIndex(const dword i, dword& fid,
    WKSPoint& from, WKSPoint& to) const
{
    long _fid;
    bool r = m_Net.get_blocked_edge_byindex(i, _fid, from, to);
    fid = _fid;
    return r;
}

bool CSlimLayer::GetNetBlockedEdgeIDs(vector<dword>& fids) const
{
    dword count = m_Net.get_blocked_edgecount();
    for (dword i = 0; i < count; i++)
    {
        long fid;
        WKSPoint from, to;
        m_Net.get_blocked_edge_byindex(i, fid, from, to);
        fids.push_back(fid);
    }

    return true;
}

void CSlimLayer::ClearNetBlockedEdges()
{
    m_Net.clear_blocked_edges();
}

bool CSlimLayer::DoBestPath(IPath** ppPath, IIntArray** ppFids)
{
    if (_invalid(ppPath) || _invalid(ppFids)) return false;
    assert(!*ppPath);
    assert(!*ppFids);

    *ppPath = new CPath;
    (*ppPath)->_AddRef();

    *ppFids = new CIntArray;
    (*ppFids)->_AddRef();

    vector<Vertex> vv;
    vector<long> ee;
    bool r = m_Net.fastshortest(vv, ee);

    long i;
    for (i = 0; i < vv.size(); i++)
    {
        Vertex v = vv[i];
        WKSPointZ point = v;
        (*ppPath)->AddPoint(point);
    }

    for (i = 0; i < ee.size(); i++)
    {
        (*ppFids)->Add(ee[i]);
    }

    return r;
}

void CSlimLayer::ClearNetTopo()
{
    m_Net.ClearAll();
}

bool CSlimLayer::StoreNetTopo()
{
    //这里始终追加，懒得搞了
    dword netoffset = m_pFeatureData->GetSize();
    m_pFeatureData->MovePos(OFFSET_NETTOPOOFFSET);
    m_pFeatureData->Write(netoffset);
    m_pFeatureData->MovePos(netoffset);
    dword joe = 0;  //以后可以改进，参考OFFSET_SYMBOLOFFSET
    m_pFeatureData->Write(joe);
    net::NetTopo2Stream(m_Net, m_pFeatureData);
    return true;
}

bool CSlimLayer::RestoreNetTopo()
{
    m_pFeatureData->MovePos(OFFSET_NETTOPOOFFSET);
    dword netoffset;
    m_pFeatureData->Read(netoffset);
    if (0 == netoffset)
    {
        return false;
    }

    //注意头4个bytes是joe，要跳过
    m_pFeatureData->MovePos(netoffset + sizeof(dword));
    net::Stream2NetTopo(m_pFeatureData, m_Net);
    return true;
}

bool CSlimLayer::Valid() const
{
    if ((!this->CheckHeader())
        || (!m_pFeatureData.Assigned())
        || (SHAPETYPE_UNKNOWN == m_GeoColumnInfo.ShpType)
        || (0 > m_GeoColumnInfo.FeatureType))
    {
        return false;
    }

    return true;
}


CSlimFeature::CSlimFeature()
{
    INIT_REFCOUNT
    ADD_EVENTTYPE(MESSAGE_VECTORFEATURE_ADDED);
    ADD_EVENTTYPE(MESSAGE_VECTORFEATURE_MODIFIED);

    m_fid = 0;
}

CSlimFeature::~CSlimFeature()
{
}

bool __stdcall CSlimFeature::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "IVectorFeature"))
        || (0 == strcmp(interfacename, "CSlimFeature")))
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

bool __stdcall CSlimFeature::Clone(IObj** ppObj) const
{
    return false;
}

dword __stdcall CSlimFeature::GetFID() const
{
    return m_fid;
}

bool __stdcall CSlimFeature::GetMBR(WKSRect& mbr) const
{
    if (!m_geometry.Assigned()) {return false;}

    return m_geometry->GetMBR(mbr);
}

void CSlimFeature::GetLayer(CVectorLayerPtr& pLayer)
{
    pLayer.Clear();
    if (!m_slimlayer.Assigned()) {return;}

    CAST_PTR(m_slimlayer, pLayer, CVectorLayer)
}

bool CSlimFeature::SetGeometryRef(const IGeometryPtr pGeometry)
{
    if (!m_slimlayer._p()) {return false;}

    m_geometry = pGeometry;
    return true;
}

void CSlimFeature::GetGeometryRef(IGeometryPtr &pGeometry)
{
    pGeometry = m_geometry;
}

dword __stdcall CSlimFeature::GetFieldCount() const
{
    if (!m_pFieldValues.Assigned()) return 0;

    return m_pFieldValues->GetFieldCount();
}

bool __stdcall CSlimFeature::SetFieldValue(const dword index, const char* const fieldvalue)
{
    if (!m_pFieldValues.Assigned()) return false;
    if (m_pFieldValues->GetFieldCount() <= index) return false;

    FieldType fieldtype;
    m_pFieldValues->GetFieldType(index, fieldtype);
    switch (fieldtype)
    {
    case FIELDTYPE_SHORT:
    case FIELDTYPE_LONG:
        m_pFieldValues->SetInteger(index, StrToInt(fieldvalue));
        break;

    case FIELDTYPE_SINGLE:
    case FIELDTYPE_DOUBLE:
        m_pFieldValues->SetFloat(index, StrToFloat(fieldvalue));
        break;

    default:
        m_pFieldValues->SetText(index, fieldvalue);
    }

    return true;
}

bool __stdcall CSlimFeature::GetFieldValue(const dword index, IFieldValue** ppFieldValue) const
{
    if (!m_pFieldValues.Assigned()) return false;

    return m_pFieldValues->GetFieldValue(index, ppFieldValue);
}

void __stdcall CSlimFeature::SetAnnotation(const char* const annotation)
{
    m_Annotation = annotation;
}

const char* __stdcall CSlimFeature::GetAnnotation() const
{
    return m_Annotation.c_str();
}

bool CSlimFeature::SetGeometry(const IGeometryPtr pGeometry)
{
    if (!pGeometry.Assigned()) {return false;}

    IObjPtr pObj;
    CLONE_PTR(pGeometry, pObj)
    CAST_PTR(pObj, m_geometry, IGeometry);
    return true;
}

void CSlimFeature::GetGeometry(IGeometryPtr &pGeometry)
{
    pGeometry.Clear();
    if (!m_geometry.Assigned()) {return;}

    IObjPtr pObj;
    CLONE_PTR(m_geometry, pObj);
    CAST_PTR(pObj, pGeometry, IGeometry);
}

void CSlimFeature::SetFieldValues(const string& fieldvalues)
{
    if (!m_slimlayer.Assigned()) {return;}

    CFieldsPtr pFields;
    m_slimlayer->GetFields(pFields);
    m_pFieldValues = new CFieldValues(pFields);
    m_pFieldValues->LoadFromString(fieldvalues);
}

string CSlimFeature::FieldValuesAsString() const
{
    if (!m_pFieldValues.Assigned()) return "";
    return m_pFieldValues->SaveToString();
}

bool __stdcall CSlimFeature::Delete()
{
    if (!m_slimlayer._p() || (0 == m_fid)) {return false;}
    if (!m_slimlayer->DeleteFeature(m_fid)) {return false;}

    m_fid = 0;
    m_geometry.Clear();
    m_slimlayer.Clear();

    return true;
}

bool __stdcall CSlimFeature::Update()
{
    if (!m_slimlayer._p() || !m_geometry.Assigned()) return false;
    if (!m_pFieldValues.Assigned()) return false;

    if (0 == m_fid)
    {
        string attribtext;
        m_fid = m_slimlayer->AddFeature(m_geometry, this->FieldValuesAsString(), m_Annotation);
        if (m_fid > 0)
        {
            DISPATCH_LONG_TOALL(MESSAGE_VECTORFEATURE_ADDED, m_fid)
            return true;
        }
        else
        {
            return false;
        }
    }
    else
    {
        if (m_slimlayer->SetFeature(m_fid, m_geometry, this->FieldValuesAsString(), m_Annotation))
        {
            DISPATCH_LONG_TOALL(MESSAGE_VECTORFEATURE_MODIFIED, m_fid)
            return true;
        }
        else
        {
            return false;
        }
    }
}

void __stdcall CSlimFeature::GetLayer(ILayer** ppLayer)
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

bool __stdcall CSlimFeature::SetGeometryRef(const IGeometry* const pGeometry)
{
    IGeometryPtr pG = (IGeometry*)pGeometry;
    return this->SetGeometryRef(pG);
}

void __stdcall CSlimFeature::GetGeometryRef(IGeometry** ppGeometry)
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
