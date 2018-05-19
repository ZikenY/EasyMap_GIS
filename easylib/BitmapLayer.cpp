#include "CommonInclude.h"
#include "BitmapLayer.h"
#include "StringFuncs.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CBitmapLayer)

CBitmapLayer::CBitmapLayer()
{
    INIT_REFCOUNT

    m_hDib = NULL;
    m_Name = "BitmapLayer";
    m_DOMInfo.unit = UNIT_M;
    m_Visible = true;
    m_BaseScale = 1;
    m_Alpha = 255;
    m_Tag = 0;
    m_MaxVisualScale = 0;
    m_MinVisualScale = 0;
}

CBitmapLayer::~CBitmapLayer()
{
    if (m_hDib)
    {
        ::DestroyDIB(m_hDib);
    }
}

bool __stdcall CBitmapLayer::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ILayer"))
        || (0 == strcmp(interfacename, "IBitmapLayer"))
        || (0 == strcmp(interfacename, "CBitmapLayer")))
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

dword __stdcall CBitmapLayer::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_MaxVisualScale);
    ps->Write(m_MinVisualScale);
    ps->Write(m_Tag);
    ps->WriteBool(m_Visible);
    ps->Write(m_Alpha);
    ps->Write(m_Name);
    ps->Write(m_BaseScale);
    ps->Write(&m_DOMInfo, sizeof(DOMInfo));
    ps->Write(m_FileName);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CBitmapLayer::_LoadInstance(IStreamX* pStream, void* const assist)
{
    if (m_hDib) {::DestroyDIB(m_hDib);}

    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Read(m_MaxVisualScale);
    ps->Read(m_MinVisualScale);
    ps->Read(m_Tag);
    ps->ReadBool(m_Visible);
    ps->Read(m_Alpha);
    ps->Read(m_Name);
    ps->Read(m_BaseScale);
    ps->Read(&m_DOMInfo, sizeof(DOMInfo));
    ps->Read(m_FileName);

    string bmpname = m_FileName;
    if (!this->LoadBmpFile(bmpname.c_str(), true))
    {
        char currdir[2000];
        ::GetCurrentDirectory(1900, currdir);
        string dir = currdir;
        bmpname = dir + "\\" + RemoveDirectoryPart(bmpname);
        this->LoadBmpFile(bmpname.c_str(), true);
    }

    return pStream->GetPos() - oldpos;
}

void __stdcall CBitmapLayer::SetName(const char* const name)
{
    m_Name = name;
}

const char* __stdcall CBitmapLayer::GetName() const
{
    return m_Name.c_str();
}

void __stdcall CBitmapLayer::SetVisible(const bool visible)
{
    m_Visible = visible;
}

bool __stdcall CBitmapLayer::GetVisible() const
{
    return m_Visible;
}

void __stdcall CBitmapLayer::SetAlpha(const byte alpha)
{
    m_Alpha = alpha;
}

byte __stdcall CBitmapLayer::GetAlpha() const
{
    return m_Alpha;
}

void __stdcall CBitmapLayer::SetScaleLimit(const double maxscale, const double minscale)
{
    m_MaxVisualScale = maxscale;
    m_MinVisualScale = minscale;
}

void __stdcall CBitmapLayer::GetScaleLimit(double& maxscale, double& minscale) const
{
    maxscale = m_MaxVisualScale;
    minscale = m_MinVisualScale;
}

void __stdcall CBitmapLayer::SetTag(const long tag)
{
    m_Tag = tag;
}

long __stdcall CBitmapLayer::GetTag() const
{
    return m_Tag;
}

void __stdcall CBitmapLayer::SetSelectable(const bool selectable)
{
    return;
}

bool __stdcall CBitmapLayer::GetSelectable() const
{
    return false;
}

dword __stdcall CBitmapLayer::Select(const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    return 0;
}

dword __stdcall CBitmapLayer::Deselect(const WKSRect& envelope, const bool partialselect)
{
    return 0;
}

dword __stdcall CBitmapLayer::GetSelectCount() const
{
    return 0;
}

void __stdcall CBitmapLayer::ClearSelection()
{
}

bool __stdcall CBitmapLayer::Clone(IObj** ppObj) const
{
    return false;
}

void _getdevicerect(CDisplayPtr pDisplay, const DOMInfo& dominfo,
    CBitmapLayer* pBitmapLayer, tagRECT& rect)
{
    //×óÉÏ½Ç
    WKSPoint wkspnt;
    wkspnt.x = dominfo.Xr;
    wkspnt.y = dominfo.Yc;
    CDisplayTransformationPtr pDisplayTransformation;
    pDisplay->GetDisplayTransformation(pDisplayTransformation);
    tagPOINT lefttop, rightbottom;
    pDisplayTransformation->Map2Device(wkspnt, lefttop);

    //ÓÒÏÂ½Ç
    WKSRect ext;
    pBitmapLayer->GetExtent(ext);
    wkspnt.x = ext.right;
    wkspnt.y = ext.bottom;
    pDisplayTransformation->Map2Device(wkspnt, rightbottom);

    rect.bottom = rightbottom.y;
    rect.left = lefttop.x;
    rect.right = rightbottom.x;
    rect.top = lefttop.y;
}

DrawResult __stdcall CBitmapLayer::DrawData(const IDisplay* pDisplay,
    const long cacheid, const WKSRect* const pEnvelope,
    const ITrackCancel* pTrackCancel)
{
    if (!m_Visible) return LAYERDRAW_NOVISIBLE;
    if (_invalid(m_hDib)) return LAYERDRAW_UNEXPECTED;
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

    DrawResult r = this->DrawData1(pDisplayCache._p(), cacheid1, pEnvelope, pTrackCancel);

    if (cacheid < 0)
    {
        pDisplayCache->PostCacheToPrimary(cacheid1);
        pDisplayCache->DeleteCache(cacheid1);
    }

    return r;
}

DrawResult CBitmapLayer::DrawData1(const IDisplayCache* pDisplayCache,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    if (!m_Visible) return LAYERDRAW_NOVISIBLE;
    if (_invalid(m_hDib)) return LAYERDRAW_UNEXPECTED;
    if (_invalid(pDisplayCache)) return LAYERDRAW_UNEXPECTED;
    CDisplayCachePtr pCache = (CDisplayCache*)pDisplayCache;
    CDisplayPtr pDisp;
    CAST_PTR(pCache, pDisp, CDisplay)
    CDisplayTransformationPtr pDT;
    pDisp->GetDisplayTransformation(pDT);
    ITrackCancelPtr pTC = (ITrackCancel*)pTrackCancel;

    double mapscale;
    pDT->GetMapScale(mapscale);

    if ((0.0001 < m_MaxVisualScale && (mapscale > m_MaxVisualScale))
        || (0.0001 < m_MinVisualScale && (mapscale < m_MinVisualScale)))
    {
        return LAYERDRAW_EXCEEDLIMIT;
    }

    RECT dc_rect, dib_rect;
    dib_rect.left = 0;
    dib_rect.top = 0;
    GetDIBSize(m_hDib, dib_rect.right, dib_rect.bottom);
    _getdevicerect(pDisp, m_DOMInfo, (CBitmapLayer*)this, dc_rect);

    DrawResult r = LAYERDRAW_UNEXPECTED;
    HPALETTE hPal = ::CreateDIBPalette(m_hDib);
    long tmpid = pCache->CreateCache(RGB(255, 255, 255), m_Alpha);
    pCache->CopyCacheToCache(cacheid, tmpid, 0, 0);
    HDC dc;
    pCache->GetCacheDC(tmpid, dc);
    DWORD rop = R2_COPYPEN;
    ::PaintDIBOops(dc, &dc_rect, m_hDib, &dib_rect, hPal, rop);
    if (_valid(hPal))
    {
        ::DeleteObject(hPal);
    }

    pCache->CopyCacheToCache(tmpid, cacheid, 0, 0);
    pCache->DeleteCache(tmpid);

    return LAYERDRAW_NORMAL;
}

DrawResult __stdcall CBitmapLayer::DrawSelection(const IDisplay* pDisplay,
    const long cacheid, const WKSRect* const pEnvelope, const ITrackCancel* pTrackCancel)
{
    return LAYERDRAW_NOSUPPORT;
}

bool __stdcall CBitmapLayer::GetExtent(WKSRect& fullext) const
{
    if (_invalid(m_hDib)) return false;

    long bmpwidth = ::DIBWidth(m_hDib);
    long bmpheight = ::DIBHeight(m_hDib);
    fullext.left = m_DOMInfo.Xr;
    fullext.top = m_DOMInfo.Yc;
    fullext.right = fullext.left + bmpwidth*m_DOMInfo.Dr;
    fullext.bottom = fullext.top - bmpheight*m_DOMInfo.Dc;
    return true;
}

MapUnits __stdcall CBitmapLayer::GetMapUnit() const
{
    return m_DOMInfo.unit;
}

bool __stdcall CBitmapLayer::GetBaseScale(double& scale) const
{
    scale = m_BaseScale;
    return true;
}

const char* __stdcall CBitmapLayer::GetSpatialReference() const
{
    return m_SR.c_str();
}

bool _getdominfo(const string& dominfo, const string& key, string& value)
{
    long keylen = key.size();
    Strings info(dominfo);
    dword count = info.GetLineCount();
    for (dword i = 0; i < count; i++)
    {
        string line;
        info.GetLine(i, line);
        line = Trim(line);
        string left = LowerString(line.substr(0, keylen));
        if (key == left)
        {
            long linesize = line.size();
            value = LowerString(Trim(line.substr(keylen+1, linesize)));
            return true;
        }
    }

    return false;
}

bool __stdcall CBitmapLayer::LoadBmpFile(const char* const filename, const bool loaddom)
{
    if (_invalid(filename)) return false;
    m_FileName = filename;

    if (m_hDib)
    {
        ::DestroyDIB(m_hDib);
        m_hDib = NULL;
    }

    m_hDib = ::LoadDIB(m_FileName.c_str());

    if (m_hDib == NULL)
    {
        m_FileName = "";
        return false;
    }

    string noext = RemoveExtNamePart(m_FileName);
    string dominfo, domname = noext + ".dom";
    if (loaddom && File2String(domname, dominfo))
    {
        string v;
        if (_getdominfo(dominfo, "unit", v))
        {
            if ("degree" == v) {m_DOMInfo.unit = UNIT_DEGREE;}
            else {m_DOMInfo.unit = UNIT_M;}
        }

        if (_getdominfo(dominfo, "xr", v))
        {
            m_DOMInfo.Xr = StrToFloat(v);
        }

        if (_getdominfo(dominfo, "yc", v))
        {
            m_DOMInfo.Yc = StrToFloat(v);
        }

        if (_getdominfo(dominfo, "dr", v))
        {
            m_DOMInfo.Dr = StrToFloat(v);
        }

        if (_getdominfo(dominfo, "dc", v))
        {
            m_DOMInfo.Dc = StrToFloat(v);
        }
    }

    return true;
}

const char* __stdcall CBitmapLayer::GetBmpFileName() const
{
    return m_FileName.c_str();
}

void __stdcall CBitmapLayer::SetDomInfo(const DOMInfo& dominfo)
{
    m_DOMInfo = dominfo;
}

void __stdcall CBitmapLayer::GetDomInfo(DOMInfo& dominfo) const
{
    dominfo = m_DOMInfo;
}

}