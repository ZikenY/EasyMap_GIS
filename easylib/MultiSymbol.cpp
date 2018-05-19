#include "CommonInclude.h"
#include "MultiSymbol.h"
#include "DrawGeometry.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CMultiPointSymbol)
CLASS_FACTORY_INSTANCE(CMultiLineSymbol)
CLASS_FACTORY_INSTANCE(CMultiFillSymbol)

//================================================================================
//  CMultiPointSymbol
//================================================================================
CMultiPointSymbol::CMultiPointSymbol(const long code)
{
    INIT_REFCOUNT

    m_Code = code;
    m_Name = "undefined";
    m_Color = RGB(0, 0, 200);
    m_ColorLock = false;
    m_Angle = 0;
    m_OffsetX = m_OffsetY = 0;
    m_Size = 1;
    m_ROP2 = R2_COPYPEN;
}

CMultiPointSymbol::~CMultiPointSymbol()
{
}

dword __stdcall CMultiPointSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_Code);
    ps->Write(m_ROP2);
    ps->Write(m_Name);
    ps->Write(m_Color);
    ps->WriteBool(m_ColorLock);
    ps->Write(m_Angle);
    ps->Write(m_OffsetX);
    ps->Write(m_OffsetY);
    ps->Write(m_Size);

    dword simplepointcount = m_PSs.size();
    ps->Write(simplepointcount);
    for (dword i = 0; i < simplepointcount; i++)
    {
        IPointSymbolPtr pSym = this->GetSubSymbol(i);
        pSym->_DumpTo(pStream, assist);
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CMultiPointSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_PSs.clear();

    ps->Read(m_Code);
    ps->Read(m_ROP2);
    ps->Read(m_Name);
    ps->Read(m_Color);
    ps->ReadBool(m_ColorLock);
    ps->Read(m_Angle);
    ps->Read(m_OffsetX);
    ps->Read(m_OffsetY);
    ps->Read(m_Size);

    dword simplepointcount = 0;
    ps->Read(simplepointcount);
    for (dword i = 0; i < simplepointcount; i++)
    {
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom(ps, pPersist, assist);
        IPointSymbolPtr pPointSymbol;
        CAST_PTR(pPersist, pPointSymbol, IPointSymbol)
        pPointSymbol->_ParentOffset(WKSPoint(m_OffsetX, m_OffsetY), m_Angle, m_Size);
        m_PSs.push_back(pPointSymbol);
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CMultiPointSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "IPointSymbol"))
        || (0 == strcmp(interfacename, "IMultiPointSymbol"))
        || (0 == strcmp(interfacename, "CMultiPointSymbol")))
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

IPointSymbolPtr CMultiPointSymbol::GetSubSymbol(const dword index) const
{
    if ((m_PSs.size() <= index) || (0 > index)) {return NULL;}

    list<IPointSymbolPtr>::const_iterator it_sym = m_PSs.begin();
    std::advance(it_sym, index);

    return *it_sym;
}

bool __stdcall CMultiPointSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CMultiPointSymbol::Clone(IObjPtr& pObj) const
{
    CMultiPointSymbolPtr pSymbol = new CMultiPointSymbol;

    pSymbol->m_Code = m_Code;
    pSymbol->m_ROP2 = m_ROP2;
    pSymbol->m_Name = m_Name;
    pSymbol->m_Color = m_Color;
    pSymbol->m_ColorLock = m_ColorLock;
    pSymbol->m_Angle = m_Angle;
    pSymbol->m_OffsetX = m_OffsetX;
    pSymbol->m_OffsetY = m_OffsetY;
    pSymbol->m_Size = m_Size;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IPointSymbolPtr pSym = GetSubSymbol(i);
        IObjPtr pTmpObj;
        CLONE_PTR(pSym, pTmpObj)
        IPointSymbolPtr pTmpSym;
        CAST_PTR(pTmpObj, pTmpSym, IPointSymbol)
        pSymbol->AddSymbol(pTmpSym);
    }

    CAST_PTR(pSymbol, pObj, IObj)
    return true;
}

bool __stdcall CMultiPointSymbol::SetCode(const long& code)
{
    m_Code = code;
    return true;
}

long __stdcall CMultiPointSymbol::GetCode() const
{
    return m_Code;
}

bool __stdcall CMultiPointSymbol::SetName(const char* const name)
{
    m_Name = name;
    return true;
}

const char* __stdcall CMultiPointSymbol::GetName() const
{
    return m_Name.c_str();
}

bool __stdcall CMultiPointSymbol::GetDC(HDC& dc) const
{
    dc = m_DC;
    return true;
}

bool CMultiPointSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    pTrans = m_pTrans;
    return true;
}

bool __stdcall CMultiPointSymbol::GetROP2(long& rop2) const
{
    rop2 = m_ROP2;
    return true;
}

bool CMultiPointSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    m_DC = dc;
    m_pTrans = pTrans;
    m_ROP2 = rop2;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IPointSymbolPtr pSym = GetSubSymbol(i);
        pSym->Prepare(dc, pTrans._p(), rop2);
    }

    return true;
}

bool __stdcall CMultiPointSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    if (_invalid(ppTrans)) return false;
    assert(!*ppTrans);

    if (!m_pTrans.Assigned()) return false;
    *ppTrans = m_pTrans._p();
    if (_valid(*ppTrans))
    {
        (*ppTrans)->_AddRef();
        return true;
    }

    return false;
}

bool __stdcall CMultiPointSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    if (_invalid(pTrans)) return false;
    CDisplayTransformationPtr pt = (CDisplayTransformation*)pTrans;
    return this->Prepare(dc, pt, rop2);
}

bool __stdcall CMultiPointSymbol::Draw(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->Draw(pg);
}

dword __stdcall CMultiPointSymbol::DrawStream(const IStreamX* pStream) const
{
    if (_invalid(pStream)) return false;
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawStream(ps);
}

bool CMultiPointSymbol::Draw(const IGeometryPtr pGeometry) const
{
    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IPointSymbolPtr pSym = GetSubSymbol(i);
        pSym->Draw(pGeometry._p());
    }

    return true;
}

dword CMultiPointSymbol::DrawStream(const CStreamPtr pStream) const
{
    if (!m_pTrans.Assigned() || !pStream.Assigned()) {return 0;}
    dword oldpos = pStream->GetPos();

    dword symbolcount = m_PSs.size();
    if (0 >= symbolcount)
    {
        IGeometryPtr pJoe;
        Stream2Geometry(pStream, pJoe);
    }
    else
    {
        for (dword i = 0; i < symbolcount; i++)
        {
            pStream->MovePos(oldpos);
            IPointSymbolPtr pSym = GetSubSymbol(i);
            pSym->DrawStream(pStream._p());
        }
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CMultiPointSymbol::_ParentOffset(const WKSPoint& offset,
    const double angle, const double size)
{
    m_OffsetX = offset.x;
    m_OffsetY = offset.y;
    m_Angle = angle;
    m_Size = size;
    return true;
}

bool __stdcall CMultiPointSymbol::SetColor(const COLORREF color)
{
    m_Color = color;
    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IPointSymbolPtr pSym = GetSubSymbol(i);
        bool lock;
        pSym->GetColorLock(lock);
        if (!lock)
        {
            pSym->SetColor(color);
        }
    }

    return true;
}

bool __stdcall CMultiPointSymbol::GetColor(COLORREF& color) const
{
    color = m_Color;

    return true;
}

void __stdcall CMultiPointSymbol::SetColorLock(const bool color)
{
    m_ColorLock = color;
}

void __stdcall CMultiPointSymbol::GetColorLock(bool& color) const
{
    color = m_ColorLock;
}

SymbolType __stdcall CMultiPointSymbol::GetSymbolType() const
{
    return SYMBOLTYPE_POINT;
}

bool __stdcall CMultiPointSymbol::SetAngle(const double angle)
{
    m_Angle = angle;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IPointSymbolPtr pSym = GetSubSymbol(i);
        pSym->_ParentOffset(WKSPoint(m_OffsetX, m_OffsetY), m_Angle, m_Size);
    }

    return true;
}

bool __stdcall CMultiPointSymbol::GetAngle(double& angle) const
{
    angle = m_Angle;
    return true;
}

bool __stdcall CMultiPointSymbol::SetOffset(const double x, const double y)
{
    m_OffsetX = x;
    m_OffsetY = y;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IPointSymbolPtr pSym = GetSubSymbol(i);
        pSym->_ParentOffset(WKSPoint(m_OffsetX, m_OffsetY), m_Angle, m_Size);
    }

    return true;
}

bool __stdcall CMultiPointSymbol::GetOffset(double& x, double& y) const
{
    x = m_OffsetX;
    y = m_OffsetY;
    return true;
}

bool __stdcall CMultiPointSymbol::AddSymbol(const IPointSymbol* pSymbol)
{
    if (_invalid(pSymbol)) return false;
    IPointSymbolPtr pp = (IPointSymbol*)pSymbol;
    return this->AddSymbol(pp);
}

bool CMultiPointSymbol::AddSymbol(const IPointSymbolPtr pSymbol)
{
    if (!pSymbol.Assigned()) return false;
    CMultiPointSymbolPtr pMPS;
    CAST_PTR(pSymbol, pMPS, CMultiPointSymbol)
    if (pMPS.Assigned()) return false;  //不想支持树桩

    IObjPtr pObjTmp;
    CLONE_PTR(pSymbol, pObjTmp)
    IPointSymbolPtr pCloneSymbol;
    CAST_PTR(pObjTmp, pCloneSymbol, IPointSymbol)

    pCloneSymbol->_ParentOffset(WKSPoint(m_OffsetX, m_OffsetY), m_Angle, m_Size);
    m_PSs.push_back(pCloneSymbol);

    dword symbolcount = m_PSs.size();
    if (1 == symbolcount)
    {
        IPointSymbolPtr pSym = GetSubSymbol(0);
        pSym->GetColor(m_Color);
    }

    return true;
}

bool __stdcall CMultiPointSymbol::AddSimpleSymbol(const COLORREF color, const double diameter)
{
    CSimplePointSymbolPtr pSimpleSymbol = new CSimplePointSymbol;
    pSimpleSymbol->SetColor(color);
    pSimpleSymbol->SetDiameter(diameter);

    IPointSymbolPtr pPointSymbol;
    CAST_PTR(pSimpleSymbol, pPointSymbol, IPointSymbol)
    pPointSymbol->_ParentOffset(WKSPoint(m_OffsetX, m_OffsetY), m_Angle, m_Size);
    m_PSs.push_back(pPointSymbol);

    dword symbolcount = m_PSs.size();
    if (1 == symbolcount)
    {
        IPointSymbolPtr pSym = GetSubSymbol(0);
        pSym->GetColor(m_Color);
    }

    return true;
}

bool __stdcall CMultiPointSymbol::SetSymbolRef(const IPointSymbol* pSymbol, const dword index)
{
    if (_invalid(pSymbol)) return false;
    IPointSymbolPtr pp = (IPointSymbol*)pSymbol;
    return this->SetSymbolRef(pp, index);
}

bool CMultiPointSymbol::SetSymbolRef(const IPointSymbolPtr pSymbol, const dword index)
{
    dword symbolcount = m_PSs.size();
    if (index >= symbolcount) return false;
    if (!pSymbol.Assigned()) return false;

    list<IPointSymbolPtr>::iterator it = m_PSs.begin();
    std::advance(it, index);

    pSymbol->_ParentOffset(WKSPoint(m_OffsetX, m_OffsetY), m_Angle, m_Size);
    *it = pSymbol;

    if (1 == symbolcount)
    {
        pSymbol->GetColor(m_Color);
    }

    return true;
}

bool __stdcall CMultiPointSymbol::RemoveSymbol(const dword index)
{
    if (m_PSs.size() <= index) return false;
    list<IPointSymbolPtr>::iterator it = m_PSs.begin();
    std::advance(it, index);
    m_PSs.erase(it);
    return true;
}

bool __stdcall CMultiPointSymbol::GetSymbolRef(IPointSymbol** ppSymbol, const dword index) const
{
    if (_invalid(ppSymbol)) return false;
    assert(!*ppSymbol);

    *ppSymbol = NULL;
    IPointSymbolPtr pp;
    this->GetSymbolRef(pp, index);
    *ppSymbol = pp._p();
    if (_valid(*ppSymbol))
    {
        (*ppSymbol)->_AddRef();
        return true;
    }

    return false;
}

bool CMultiPointSymbol::GetSymbolRef(IPointSymbolPtr& pSymbol, const dword index) const
{
    dword symbolcount = m_PSs.size();
    if (index >= symbolcount) return false;

    pSymbol = GetSubSymbol(index);
    return true;
}

bool __stdcall CMultiPointSymbol::SetSymbolOrder(const IPointSymbol* pSymbol, const dword neworder)
{
    if (_invalid(pSymbol)) return false;
    IPointSymbolPtr pp = (IPointSymbol*)pSymbol;
    return this->SetSymbolOrder(pp, neworder);
}

bool CMultiPointSymbol::SetSymbolOrder(const IPointSymbolPtr pSymbol, const dword neworder)
{
    dword size = m_PSs.size();
    if (neworder >= size) return false;

    for (dword i = 0; i < size; i++)
    {
        IPointSymbolPtr pSymTmp;
        this->GetSymbolRef(pSymTmp, i);
        if (pSymTmp.Compare(pSymbol))
        {
            if (neworder == i) return true;
            list<IPointSymbolPtr>::iterator it;
            m_PSs.remove(pSymTmp);
            it = m_PSs.begin();
            std::advance(it, neworder);
            m_PSs.insert(it, pSymTmp);
            return true;
        }
    }

    return false;
}

dword __stdcall CMultiPointSymbol::GetSymbolCount() const
{
    return m_PSs.size();
}

void __stdcall CMultiPointSymbol::ClearSymbols()
{
    m_PSs.clear();
}

bool __stdcall CMultiPointSymbol::SetSize(const double size)
{
    if (0 > size) return false;
    m_Size = size;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IPointSymbolPtr pSym = GetSubSymbol(i);
        pSym->_ParentOffset(WKSPoint(m_OffsetX, m_OffsetY), m_Angle, m_Size);
    }

    return true;
}

bool __stdcall CMultiPointSymbol::GetSize(double& size) const
{
    size = m_Size;
    return true;
}
//================================================================================


//================================================================================
//  CMultiLineSymbol
//================================================================================
CMultiLineSymbol::CMultiLineSymbol(const long code)
{
    INIT_REFCOUNT

    m_Code = code;
    m_Name = "undefined";
    m_Color = RGB(20, 200, 10);
    m_ColorLock = false;
    m_Offset = 0;
    m_Size = 1;
    m_ROP2 = R2_COPYPEN;
}

CMultiLineSymbol::~CMultiLineSymbol()
{
}

dword __stdcall CMultiLineSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_Code);
    ps->Write(m_ROP2);
    ps->Write(m_Name);
    ps->Write(m_Color);
    ps->WriteBool(m_ColorLock);
    ps->Write(m_Offset);
    ps->Write(m_Size);

    dword simplepointcount = m_PSs.size();
    ps->Write(simplepointcount);
    for (dword i = 0; i < simplepointcount; i++)
    {
        ILineSymbolPtr pSym = this->GetSubSymbol(i);
        pSym->_DumpTo(pStream, assist);
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CMultiLineSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_PSs.clear();

    ps->Read(m_Code);
    ps->Read(m_ROP2);
    ps->Read(m_Name);
    ps->Read(m_Color);
    ps->ReadBool(m_ColorLock);
    ps->Read(m_Offset);
    ps->Read(m_Size);

    dword simplepointcount = 0;
    ps->Read(simplepointcount);
    for (dword i = 0; i < simplepointcount; i++)
    {
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom(ps, pPersist, assist);
        ILineSymbolPtr pLineSymbol;
        CAST_PTR(pPersist, pLineSymbol, ILineSymbol)
        pLineSymbol->_ParentOffset(m_Offset, m_Size);
        m_PSs.push_back(pLineSymbol);
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CMultiLineSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "ILineSymbol"))
        || (0 == strcmp(interfacename, "IMultiLineSymbol"))
        || (0 == strcmp(interfacename, "CMultiLineSymbol")))
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

ILineSymbolPtr CMultiLineSymbol::GetSubSymbol(const dword index) const
{
    if ((m_PSs.size() <= index) || (0 > index)) {return NULL;}

    list<ILineSymbolPtr>::const_iterator it_sym = m_PSs.begin();
    std::advance(it_sym, index);

    return *it_sym;
}

bool __stdcall CMultiLineSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CMultiLineSymbol::Clone(IObjPtr& pObj) const
{
    CMultiLineSymbolPtr pSymbol = new CMultiLineSymbol;

    pSymbol->m_Code = m_Code;
    pSymbol->m_ROP2 = m_ROP2;
    pSymbol->m_Name = m_Name;
    pSymbol->m_Color = m_Color;
    pSymbol->m_ColorLock = m_ColorLock;
    pSymbol->m_Offset = m_Offset;
    pSymbol->m_Size = m_Size;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        ILineSymbolPtr pSym = GetSubSymbol(i);
        IObjPtr pTmpObj;
        CLONE_PTR(pSym, pTmpObj)
        ILineSymbolPtr pTmpSym;
        CAST_PTR(pTmpObj, pTmpSym, ILineSymbol)
        pSymbol->AddSymbol(pTmpSym);
    }

    CAST_PTR(pSymbol, pObj, IObj)
    return true;
}

bool __stdcall CMultiLineSymbol::SetCode(const long& code)
{
    m_Code = code;
    return true;
}

long __stdcall CMultiLineSymbol::GetCode() const
{
    return m_Code;
}

bool __stdcall CMultiLineSymbol::SetName(const char* const name)
{
    m_Name = name;
    return true;
}

const char* __stdcall CMultiLineSymbol::GetName() const
{
    return m_Name.c_str();
}

bool __stdcall CMultiLineSymbol::GetDC(HDC& dc) const
{
    dc = m_DC;
    return true;
}

bool CMultiLineSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    pTrans = m_pTrans;
    return true;
}

bool __stdcall CMultiLineSymbol::GetROP2(long& rop2) const
{
    rop2 = m_ROP2;
    return true;
}

bool CMultiLineSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    m_DC = dc;
    m_pTrans = pTrans;
    m_ROP2 = rop2;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        ILineSymbolPtr pSym = GetSubSymbol(i);
        pSym->Prepare(dc, pTrans._p(), rop2);
    }

    return true;
}

bool __stdcall CMultiLineSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    if (_invalid(ppTrans)) return false;
    assert(!*ppTrans);

    if (!m_pTrans.Assigned()) return false;
    *ppTrans = m_pTrans._p();
    if (_valid(*ppTrans))
    {
        (*ppTrans)->_AddRef();
        return true;
    }

    return false;
}

bool __stdcall CMultiLineSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    if (_invalid(pTrans)) return false;
    CDisplayTransformationPtr pt = (CDisplayTransformation*)pTrans;
    return this->Prepare(dc, pt, rop2);
}

bool __stdcall CMultiLineSymbol::Draw(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->Draw(pg);
}

dword __stdcall CMultiLineSymbol::DrawStream(const IStreamX* pStream) const
{
    if (_invalid(pStream)) return false;
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawStream(ps);
}

bool CMultiLineSymbol::Draw(const IGeometryPtr pGeometry) const
{
    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        ILineSymbolPtr pSym = GetSubSymbol(i);
        pSym->Draw(pGeometry._p());
    }

    return true;
}

dword CMultiLineSymbol::DrawStream(const CStreamPtr pStream) const
{
    if (!m_pTrans.Assigned() || !pStream.Assigned()) {return 0;}
    dword oldpos = pStream->GetPos();

    dword symbolcount = m_PSs.size();
    if (0 >= symbolcount)
    {
        IGeometryPtr pJoe;
        Stream2Geometry(pStream, pJoe);
    }
    else
    {
        for (dword i = 0; i < symbolcount; i++)
        {
            pStream->MovePos(oldpos);
            ILineSymbolPtr pSym = GetSubSymbol(i);
            pSym->DrawStream(pStream._p());
        }
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CMultiLineSymbol::_ParentOffset(const double offset,
    const double size)
{
    m_Offset = offset;
    m_Size = size;
    return true;
}

bool __stdcall CMultiLineSymbol::SetColor(const COLORREF color)
{
    m_Color = color;
    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        ILineSymbolPtr pSym = GetSubSymbol(i);
        bool lock;
        pSym->GetColorLock(lock);
        if (!lock)
        {
            pSym->SetColor(color);
        }
    }

    return true;
}

bool __stdcall CMultiLineSymbol::GetColor(COLORREF& color) const
{
    color = m_Color;

    return true;
}

void __stdcall CMultiLineSymbol::SetColorLock(const bool color)
{
    m_ColorLock = color;
}

void __stdcall CMultiLineSymbol::GetColorLock(bool& color) const
{
    color = m_ColorLock;
}

SymbolType __stdcall CMultiLineSymbol::GetSymbolType() const
{
    return SYMBOLTYPE_LINE;
}

bool __stdcall CMultiLineSymbol::SetOffset(const double offset)
{
    m_Offset = offset;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        ILineSymbolPtr pSym = GetSubSymbol(i);
        pSym->_ParentOffset(m_Offset, m_Size);
    }

    return true;
}

bool __stdcall CMultiLineSymbol::GetOffset(double& offset) const
{
    offset = m_Offset;
    return true;
}

bool __stdcall CMultiLineSymbol::AddSymbol(const ILineSymbol* pSymbol)
{
    if (_invalid(pSymbol)) return false;
    ILineSymbolPtr pp = (ILineSymbol*)pSymbol;
    return this->AddSymbol(pp);
}

bool CMultiLineSymbol::AddSymbol(const ILineSymbolPtr pSymbol)
{
    if (!pSymbol.Assigned()) return false;
    CMultiLineSymbolPtr pMPS;
    CAST_PTR(pSymbol, pMPS, CMultiLineSymbol)
    if (pMPS.Assigned()) return false;  //不想支持树桩

    IObjPtr pObjTmp;
    CLONE_PTR(pSymbol, pObjTmp)
    ILineSymbolPtr pCloneSymbol;
    CAST_PTR(pObjTmp, pCloneSymbol, ILineSymbol)

    pCloneSymbol->_ParentOffset(m_Offset, m_Size);
    m_PSs.push_back(pCloneSymbol);

    dword symbolcount = m_PSs.size();
    if (1 == symbolcount)
    {
        ILineSymbolPtr pSym = GetSubSymbol(0);
        pSym->GetColor(m_Color);
    }

    return true;
}

bool __stdcall CMultiLineSymbol::AddSimpleSymbol(const COLORREF color, const double linewidth)
{
    CSimpleLineSymbolPtr pSimpleSymbol = new CSimpleLineSymbol;
    pSimpleSymbol->SetColor(color);
    pSimpleSymbol->SetWidth(linewidth);
    pSimpleSymbol->SetName("普通线");

    ILineSymbolPtr pLineSymbol;
    CAST_PTR(pSimpleSymbol, pLineSymbol, ILineSymbol)
    pLineSymbol->_ParentOffset(m_Offset, m_Size);
    m_PSs.push_back(pLineSymbol);

    dword symbolcount = m_PSs.size();
    if (1 == symbolcount)
    {
        ILineSymbolPtr pSym = GetSubSymbol(0);
        pSym->GetColor(m_Color);
    }

    return true;
}

bool __stdcall CMultiLineSymbol::SetSymbolRef(const ILineSymbol* pSymbol, const dword index)
{
    if (_invalid(pSymbol)) return false;
    ILineSymbolPtr pp = (ILineSymbol*)pSymbol;
    return this->SetSymbolRef(pp, index);
}

bool CMultiLineSymbol::SetSymbolRef(const ILineSymbolPtr pSymbol, const dword index)
{
    dword symbolcount = m_PSs.size();
    if (index >= symbolcount) return false;
    if (!pSymbol.Assigned()) return false;

    list<ILineSymbolPtr>::iterator it = m_PSs.begin();
    std::advance(it, index);

    pSymbol->_ParentOffset(m_Offset, m_Size);
    *it = pSymbol;

    if (1 == symbolcount)
    {
        pSymbol->GetColor(m_Color);
    }

    return true;
}

bool __stdcall CMultiLineSymbol::RemoveSymbol(const dword index)
{
    if (m_PSs.size() <= index) return false;
    list<ILineSymbolPtr>::iterator it = m_PSs.begin();
    std::advance(it, index);
    m_PSs.erase(it);
    return true;;
}

bool __stdcall CMultiLineSymbol::GetSymbolRef(ILineSymbol** ppSymbol, const dword index) const
{
    if (_invalid(ppSymbol)) return false;
    assert(!*ppSymbol);

    *ppSymbol = NULL;
    ILineSymbolPtr pp;
    this->GetSymbolRef(pp, index);
    *ppSymbol = pp._p();
    if (_valid(*ppSymbol))
    {
        (*ppSymbol)->_AddRef();
        return true;
    }

    return false;
}

bool CMultiLineSymbol::GetSymbolRef(ILineSymbolPtr& pSymbol, const dword index) const
{
    dword symbolcount = m_PSs.size();
    if (index >= symbolcount) return false;

    pSymbol = GetSubSymbol(index);
    return true;
}

bool __stdcall CMultiLineSymbol::SetSymbolOrder(const ILineSymbol* pSymbol, const dword neworder)
{
    if (_invalid(pSymbol)) return false;
    ILineSymbolPtr pp = (ILineSymbol*)pSymbol;
    return this->SetSymbolOrder(pp, neworder);
}

bool CMultiLineSymbol::SetSymbolOrder(const ILineSymbolPtr pSymbol, const dword neworder)
{
    dword size = m_PSs.size();
    if (neworder >= size) return false;

    for (dword i = 0; i < size; i++)
    {
        ILineSymbolPtr pSymTmp;
        this->GetSymbolRef(pSymTmp, i);
        if (pSymTmp.Compare(pSymbol))
        {
            if (neworder == i) return true;
            list<ILineSymbolPtr>::iterator it;
            m_PSs.remove(pSymTmp);
            it = m_PSs.begin();
            std::advance(it, neworder);
            m_PSs.insert(it, pSymTmp);
            return true;
        }
    }

    return false;
}

dword __stdcall CMultiLineSymbol::GetSymbolCount() const
{
    return m_PSs.size();
}

void __stdcall CMultiLineSymbol::ClearSymbols()
{
    m_PSs.clear();
}

bool __stdcall CMultiLineSymbol::SetSize(const double size)
{
    if (0 > size) return false;
    m_Size = size;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        ILineSymbolPtr pSym = GetSubSymbol(i);
        pSym->_ParentOffset(m_Offset, m_Size);
    }

    return true;
}

bool __stdcall CMultiLineSymbol::GetSize(double& size) const
{
    size = m_Size;
    return true;
}
//================================================================================


//================================================================================
//  CMultiFillSymbol
//================================================================================
CMultiFillSymbol::CMultiFillSymbol(const long code)
{
    INIT_REFCOUNT

    m_Code = code;
    m_Name = "undefined multifillsymbol";
    m_Color = RGB(20, 200, 10);
    m_ColorLock = false;
    m_ROP2 = R2_COPYPEN;
}

CMultiFillSymbol::~CMultiFillSymbol()
{
}

dword __stdcall CMultiFillSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_Code);
    ps->Write(m_ROP2);
    ps->Write(m_Name);
    ps->Write(m_Color);
    ps->WriteBool(m_ColorLock);

    dword simplepointcount = m_PSs.size();
    ps->Write(simplepointcount);
    for (dword i = 0; i < simplepointcount; i++)
    {
        IFillSymbolPtr pSym = this->GetSubSymbol(i);
        pSym->_DumpTo(pStream, assist);
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CMultiFillSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_PSs.clear();

    ps->Read(m_Code);
    ps->Read(m_ROP2);
    ps->Read(m_Name);
    ps->Read(m_Color);
    ps->ReadBool(m_ColorLock);

    dword simplepointcount = 0;
    ps->Read(simplepointcount);
    for (dword i = 0; i < simplepointcount; i++)
    {
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom(ps, pPersist, assist);
        IFillSymbolPtr pFillSymbol;
        CAST_PTR(pPersist, pFillSymbol, IFillSymbol)
        m_PSs.push_back(pFillSymbol);
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CMultiFillSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "IFillSymbol"))
        || (0 == strcmp(interfacename, "IMultiFillSymbol"))
        || (0 == strcmp(interfacename, "CMultiFillSymbol")))
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

IFillSymbolPtr CMultiFillSymbol::GetSubSymbol(const dword index) const
{
    if ((m_PSs.size() <= index) || (0 > index)) {return NULL;}

    list<IFillSymbolPtr>::const_iterator it_sym = m_PSs.begin();
    std::advance(it_sym, index);

    return *it_sym;
}

bool __stdcall CMultiFillSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CMultiFillSymbol::Clone(IObjPtr& pObj) const
{
    CMultiFillSymbolPtr pSymbol = new CMultiFillSymbol;

    pSymbol->m_Code = m_Code;
    pSymbol->m_ROP2 = m_ROP2;
    pSymbol->m_Name = m_Name;
    pSymbol->m_Color = m_Color;
    pSymbol->m_ColorLock = m_ColorLock;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IFillSymbolPtr pSym = GetSubSymbol(i);
        IObjPtr pTmpObj;
        CLONE_PTR(pSym, pTmpObj)
        IFillSymbolPtr pTmpSym;
        CAST_PTR(pTmpObj, pTmpSym, IFillSymbol)
        pSymbol->AddSymbol(pTmpSym);
    }

    CAST_PTR(pSymbol, pObj, IObj)
    return true;
}

bool __stdcall CMultiFillSymbol::SetCode(const long& code)
{
    m_Code = code;
    return true;
}

long __stdcall CMultiFillSymbol::GetCode() const
{
    return m_Code;
}

bool __stdcall CMultiFillSymbol::SetName(const char* const name)
{
    m_Name = name;
    return true;
}

const char* __stdcall CMultiFillSymbol::GetName() const
{
    return m_Name.c_str();
}

bool __stdcall CMultiFillSymbol::GetDC(HDC& dc) const
{
    dc = m_DC;
    return true;
}

bool CMultiFillSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    pTrans = m_pTrans;
    return true;
}

bool __stdcall CMultiFillSymbol::GetROP2(long& rop2) const
{
    rop2 = m_ROP2;
    return true;
}

bool CMultiFillSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    m_DC = dc;
    m_pTrans = pTrans;
    m_ROP2 = rop2;

    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IFillSymbolPtr pSym = GetSubSymbol(i);
        pSym->Prepare(dc, pTrans._p(), rop2);
    }

    return true;
}

bool __stdcall CMultiFillSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    if (_invalid(ppTrans)) return false;
    assert(!*ppTrans);

    if (!m_pTrans.Assigned()) return false;
    *ppTrans = m_pTrans._p();
    if (_valid(*ppTrans))
    {
        (*ppTrans)->_AddRef();
        return true;
    }

    return false;
}

bool __stdcall CMultiFillSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    if (_invalid(pTrans)) return false;
    CDisplayTransformationPtr pt = (CDisplayTransformation*)pTrans;
    return this->Prepare(dc, pt, rop2);
}

bool __stdcall CMultiFillSymbol::Draw(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->Draw(pg);
}

dword __stdcall CMultiFillSymbol::DrawStream(const IStreamX* pStream) const
{
    if (_invalid(pStream)) return false;
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawStream(ps);
}

bool CMultiFillSymbol::Draw(const IGeometryPtr pGeometry) const
{
    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IFillSymbolPtr pSym = GetSubSymbol(i);
        pSym->Draw(pGeometry._p());
    }

    return true;
}

dword CMultiFillSymbol::DrawStream(const CStreamPtr pStream) const
{
    if (!m_pTrans.Assigned() || !pStream.Assigned()) {return 0;}
    dword oldpos = pStream->GetPos();

    dword symbolcount = m_PSs.size();
    if (0 >= symbolcount)
    {
        IGeometryPtr pJoe;
        Stream2Geometry(pStream, pJoe);
    }
    else
    {
        for (dword i = 0; i < symbolcount; i++)
        {
            pStream->MovePos(oldpos);
            IFillSymbolPtr pSym = GetSubSymbol(i);
            pSym->DrawStream(pStream._p());
        }
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CMultiFillSymbol::SetColor(const COLORREF color)
{
    m_Color = color;
    dword symbolcount = m_PSs.size();
    for (dword i = 0; i < symbolcount; i++)
    {
        IFillSymbolPtr pSym = GetSubSymbol(i);
        bool lock;
        pSym->GetColorLock(lock);
        if (!lock)
        {
            pSym->SetColor(color);
        }
    }

    return true;
}

bool __stdcall CMultiFillSymbol::GetColor(COLORREF& color) const
{
    color = m_Color;

    return true;
}

void __stdcall CMultiFillSymbol::SetColorLock(const bool color)
{
    m_ColorLock = color;
}

void __stdcall CMultiFillSymbol::GetColorLock(bool& color) const
{
    color = m_ColorLock;
}

SymbolType __stdcall CMultiFillSymbol::GetSymbolType() const
{
    return SYMBOLTYPE_FILL;
}

bool __stdcall CMultiFillSymbol::AddSymbol(const IFillSymbol* pSymbol)
{
    if (_invalid(pSymbol)) return false;
    IFillSymbolPtr pp = (IFillSymbol*)pSymbol;
    return this->AddSymbol(pp);
}

bool CMultiFillSymbol::AddSymbol(const IFillSymbolPtr pSymbol)
{
    if (!pSymbol.Assigned()) return false;
    CMultiFillSymbolPtr pMPS;
    CAST_PTR(pSymbol, pMPS, CMultiFillSymbol)
    if (pMPS.Assigned()) return false;  //不想支持树桩

    IObjPtr pObjTmp;
    CLONE_PTR(pSymbol, pObjTmp)
    IFillSymbolPtr pCloneSymbol;
    CAST_PTR(pObjTmp, pCloneSymbol, IFillSymbol)
    m_PSs.push_back(pCloneSymbol);

    dword symbolcount = m_PSs.size();
    if (1 == symbolcount)
    {
        IFillSymbolPtr pSym = GetSubSymbol(0);
        pSym->GetColor(m_Color);
    }

    return true;
}

bool __stdcall CMultiFillSymbol::AddSimpleSymbol(const COLORREF color)
{
    CSimpleFillSymbolPtr pSimpleSymbol = new CSimpleFillSymbol;
    pSimpleSymbol->SetColor(color);
    pSimpleSymbol->SetName("简单面");

    IFillSymbolPtr pFillSymbol;
    CAST_PTR(pSimpleSymbol, pFillSymbol, IFillSymbol)
    m_PSs.push_back(pFillSymbol);

    dword symbolcount = m_PSs.size();
    if (1 == symbolcount)
    {
        IFillSymbolPtr pSym = GetSubSymbol(0);
        pSym->GetColor(m_Color);
    }

    return true;
}

bool __stdcall CMultiFillSymbol::SetSymbolRef(const IFillSymbol* pSymbol, const dword index)
{
    if (_invalid(pSymbol)) return false;
    IFillSymbolPtr pp = (IFillSymbol*)pSymbol;
    return this->SetSymbolRef(pp, index);
}

bool CMultiFillSymbol::SetSymbolRef(const IFillSymbolPtr pSymbol, const dword index)
{
    dword symbolcount = m_PSs.size();
    if (index >= symbolcount) return false;
    if (!pSymbol.Assigned()) return false;

    list<IFillSymbolPtr>::iterator it = m_PSs.begin();
    std::advance(it, index);

    *it = pSymbol;

    if (1 == symbolcount)
    {
        pSymbol->GetColor(m_Color);
    }

    return true;
}

bool __stdcall CMultiFillSymbol::RemoveSymbol(const dword index)
{
    if (m_PSs.size() <= index) return false;
    list<IFillSymbolPtr>::iterator it = m_PSs.begin();
    std::advance(it, index);
    m_PSs.erase(it);
    return true;;
}

bool __stdcall CMultiFillSymbol::GetSymbolRef(IFillSymbol** ppSymbol, const dword index) const
{
    if (_invalid(ppSymbol)) return false;
    assert(!*ppSymbol);

    *ppSymbol = NULL;
    IFillSymbolPtr pp;
    this->GetSymbolRef(pp, index);
    *ppSymbol = pp._p();
    if (_valid(*ppSymbol))
    {
        (*ppSymbol)->_AddRef();
        return true;
    }

    return false;
}

bool CMultiFillSymbol::GetSymbolRef(IFillSymbolPtr& pSymbol, const dword index) const
{
    dword symbolcount = m_PSs.size();
    if (index >= symbolcount) return false;

    pSymbol = GetSubSymbol(index);
    return true;
}

bool __stdcall CMultiFillSymbol::SetSymbolOrder(const IFillSymbol* pSymbol, const dword neworder)
{
    if (_invalid(pSymbol)) return false;
    IFillSymbolPtr pp = (IFillSymbol*)pSymbol;
    return this->SetSymbolOrder(pp, neworder);
}

bool CMultiFillSymbol::SetSymbolOrder(const IFillSymbolPtr pSymbol, const dword neworder)
{
    dword size = m_PSs.size();
    if (neworder >= size) return false;

    for (dword i = 0; i < size; i++)
    {
        IFillSymbolPtr pSymTmp;
        this->GetSymbolRef(pSymTmp, i);
        if (pSymTmp.Compare(pSymbol))
        {
            if (neworder == i) return true;
            list<IFillSymbolPtr>::iterator it;
            m_PSs.remove(pSymTmp);
            it = m_PSs.begin();
            std::advance(it, neworder);
            m_PSs.insert(it, pSymTmp);
            return true;
        }
    }

    return false;
}

dword __stdcall CMultiFillSymbol::GetSymbolCount() const
{
    return m_PSs.size();
}

void __stdcall CMultiFillSymbol::ClearSymbols()
{
    m_PSs.clear();
}

//================================================================================

}

