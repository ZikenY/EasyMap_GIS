#include "CommonInclude.h"
#include "SimpleSymbol.h"
#include "DrawGeometry.h"
#include "MathLib.h"
#include "GeometryLabel.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CSimplePointSymbol)
CLASS_FACTORY_INSTANCE(CEnvelopePointSymbol)
CLASS_FACTORY_INSTANCE(CPolyPointSymbol)
CLASS_FACTORY_INSTANCE(CLineSimpleTemplate)
CLASS_FACTORY_INSTANCE(CSimpleLineSymbol)
CLASS_FACTORY_INSTANCE(CPointLineSymbol)
CLASS_FACTORY_INSTANCE(CSimpleFillSymbol)
CLASS_FACTORY_INSTANCE(CPointFillSymbol)
CLASS_FACTORY_INSTANCE(CSimpleTextSymbol)

using namespace drawgeometry;
using namespace mathlib;

//================================================================================
//  _SimplePointSymbol
//================================================================================
_SimplePointSymbol::_SimplePointSymbol()
{
    INIT_REFCOUNT

    m_Code = -1;
    m_Name = "undefined";
    m_Diameter = 1;
    m_Color = RGB(0, 50, 250);
    m_ColorLock = false;
    m_Angle = 0;
    m_OffsetX = 0;
    m_OffsetY = 0;
    m_LineWidth = 0.5;
    m_Solid = true;

    m_ParentAngle = 0;
    m_ParentOffsetX = 0;
    m_ParentOffsetY = 0;
    m_ParentSize = 1;
}

dword __stdcall _SimplePointSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_Code);
    ps->Write(m_ROP2);
    ps->Write(m_Name);
    ps->Write(m_Color);
    ps->WriteBool(m_ColorLock);
    ps->Write(m_Diameter);
    ps->Write(m_Angle);
    ps->Write(m_OffsetX);
    ps->Write(m_OffsetY);
    ps->Write(m_LineWidth);
    ps->WriteBool(m_Solid);

    return pStream->GetPos() - oldpos;
}

dword __stdcall _SimplePointSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Read(m_Code);
    ps->Read(m_ROP2);
    ps->Read(m_Name);
    ps->Read(m_Color);
    ps->ReadBool(m_ColorLock);
    ps->Read(m_Diameter);
    ps->Read(m_Angle);
    ps->Read(m_OffsetX);
    ps->Read(m_OffsetY);
    ps->Read(m_LineWidth);
    ps->ReadBool(m_Solid);

    return pStream->GetPos() - oldpos;
}

bool __stdcall _SimplePointSymbol::SetCode(const long& code)
{
    m_Code = code;
    return true;
}

long __stdcall _SimplePointSymbol::GetCode() const
{
    return m_Code;
}

bool __stdcall _SimplePointSymbol::SetName(const char* const name)
{
    m_Name = name;
    return true;
}

const char* __stdcall _SimplePointSymbol::GetName() const
{
    return m_Name.c_str();
}

bool __stdcall _SimplePointSymbol::GetDC(HDC& dc) const
{
    dc = m_DC;
    return true;
}

bool _SimplePointSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    pTrans = m_pTrans;
    return true;
}

bool __stdcall _SimplePointSymbol::GetROP2(long& rop2) const
{
    rop2 = m_ROP2;
    return true;
}

bool _SimplePointSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    m_DC = dc;
    m_pTrans = pTrans;
    m_ROP2 = rop2;

    return true;
}

bool __stdcall _SimplePointSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    if (_invalid(ppTrans)) return false;
    assert(!*ppTrans);

    if (!m_pTrans.Assigned()) return false;
    *ppTrans = m_pTrans._p();
    if (_valid(*ppTrans)) (*ppTrans)->_AddRef();
    return true;
}

bool __stdcall _SimplePointSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    if (_invalid(pTrans))
        return false;
    m_DC = dc;
    m_pTrans = (CDisplayTransformation*)pTrans;
    m_ROP2 = rop2;
    return true;
}

bool __stdcall _SimplePointSymbol::Draw(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry))
        return false;

    IGeometryPtr pGeo = (IGeometry*)pGeometry;
    return this->Draw(pGeo);
}

dword __stdcall _SimplePointSymbol::DrawStream(const IStreamX* pStream) const
{
    if (_invalid(pStream))
        return false;
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawStream(ps);
}

bool _point_visible(const double& x, const double& y, CDisplayTransformationPtr pDT)
{
    double attitude, planerotate;
    pDT->GetAttitude(attitude);
    pDT->GetPlaneRotate(planerotate);
    if ((attitude < 0.1) && (planerotate < 0.1))
    {
        return true;
    }

    return pDT->PointInPlane(x, y);
}

bool _SimplePointSymbol::Draw(const IGeometryPtr pGeometry) const
{
    if (!m_pTrans.Assigned() || !pGeometry.Assigned())
        return false;

    CPointPtr pPoint;
    CAST_PTR(pGeometry, pPoint, CPoint)
    double x, y;
    if (pPoint.Assigned())
    {
        pPoint->GetX(x);
        pPoint->GetY(y);
        if (_point_visible(x, y, m_pTrans))
        {
            long X, Y;
            m_pTrans->Map2DeviceXY(x, y, X, Y);
            this->DrawSimplePoint(X, Y);
        }
    }
    else
    {
        CMultiPointPtr pMPoint;
        CAST_PTR(pGeometry, pMPoint, CMultiPoint)
        if (pMPoint.Assigned())
        {
            dword pntcnt = pMPoint->GetPointCount();
            for (dword i = 0; i < pntcnt; i++)
            {
                WKSPointZ pntz;
                pMPoint->GetPoint(pntz, i);
                if (_point_visible(pntz.x, pntz.y, m_pTrans))
                {
                    pPoint = new CPoint(pntz);
                    IGeometryPtr pGeo = (IGeometry*)pPoint._p();
                    this->Draw(pGeo);
                }
            }
        }
        else
        {
            return false;
        }
    }
    return true;
}

dword _SimplePointSymbol::DrawStream(const CStreamPtr pStream) const
{
    if (!m_pTrans.Assigned() || !pStream.Assigned()) {return 0;}

    WKSPointZ point;
    IGeometryPtr pGeo;
    CPointPtr pPoint;
    dword r = Stream2Geometry(pStream, pGeo);
    GeometryType geotype = pGeo->GetGeometryType();
    switch (geotype)
    {
    case GEOMETRYTYPE_POINT:
        {
            CAST_PTR(pGeo, pPoint, CPoint)
            pPoint->GetCoordinates(point);
            if (_point_visible(point.x, point.y, m_pTrans))
            {
                long X, Y;
                m_pTrans->Map2DeviceXY(point.x, point.y, X, Y);
                this->DrawSimplePoint(X, Y);
            }
        }
        break;

    case GEOMETRYTYPE_MULTIPOINT:
        {
            CMultiPointPtr pMPoint;
            CAST_PTR(pGeo, pMPoint, CMultiPoint)
            dword pntcnt = pMPoint->GetPointCount();
            for (dword i = 0; i < pntcnt; i++)
            {
                WKSPointZ pntz;
                pMPoint->GetPoint(pntz, i);
                if (_point_visible(pntz.x, pntz.y, m_pTrans))
                {
                    pPoint = new CPoint(pntz);
                    IGeometryPtr pGeo = (IGeometry*)pPoint._p();
                    this->Draw(pGeo);
                }
            }
        }
        break;

    default:
        return r;
    }

    return r;
}

bool __stdcall _SimplePointSymbol::_ParentOffset(const WKSPoint& offset,
    const double angle, const double size)
{
    m_ParentAngle = angle;
    m_ParentOffsetX = offset.x;
    m_ParentOffsetY = offset.y;
    m_ParentSize = size;
    return true;
}

bool __stdcall _SimplePointSymbol::SetColor(const COLORREF color)
{
    m_Color = color;
    return true;
}

bool __stdcall _SimplePointSymbol::GetColor(COLORREF& color) const
{
    color = m_Color;
    return true;
}

void __stdcall _SimplePointSymbol::SetColorLock(const bool lock)
{
    m_ColorLock = lock;
}

void __stdcall _SimplePointSymbol::GetColorLock(bool& color) const
{
    color = m_ColorLock;
}

SymbolType __stdcall _SimplePointSymbol::GetSymbolType() const
{
    return SYMBOLTYPE_POINT;
}

bool __stdcall _SimplePointSymbol::SetDiameter(const double diameter)
{
    m_Diameter = diameter;
    return true;
}

bool __stdcall _SimplePointSymbol::GetDiameter(double& diameter) const
{
    diameter = m_Diameter;
    return true;
}

bool __stdcall _SimplePointSymbol::SetLineWidth(const double width)
{
    m_LineWidth = width;
    return true;
}

bool __stdcall _SimplePointSymbol::GetLineWidth(double& width) const
{
    width = m_LineWidth;
    return true;
}

bool __stdcall _SimplePointSymbol::SetAngle(const double angle)
{
    m_Angle = angle;
    return true;
}

bool __stdcall _SimplePointSymbol::GetAngle(double& angle) const
{
    angle = m_Angle;
    return true;
}

bool __stdcall _SimplePointSymbol::SetOffset(const double x, const double y)
{
    m_OffsetX = x;
    m_OffsetY = y;
    return true;
}

bool __stdcall _SimplePointSymbol::GetOffset(double& x, double& y) const
{
    x = m_OffsetX;
    y = m_OffsetY;
    return true;
}

void __stdcall _SimplePointSymbol::SetSolid(const bool solid)
{
    m_Solid = solid;
}

void __stdcall _SimplePointSymbol::GetSolid(bool& solid) const
{
    solid = m_Solid;
}

void _SimplePointSymbol::BeforeDraw(HPEN& pensaved,
    HBRUSH& brushsaved, long& rop2saved, double& factor) const
{
    //计算象素->毫米对应关系
    long logpixelx, logpixely;
    m_pTrans->GetLogPixel(logpixelx, logpixely);
    double quotiety_x = logpixelx
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double quotiety_y = logpixely
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double quotiety = (quotiety_x + quotiety_y) / 2;

    double refscale;
    m_pTrans->GetReferenceScale(refscale);

    long pensize1;
    if (refscale > 0)
    {
        //使用参考比例尺，屏幕图元尺寸随着显示比例尺变化而变化
        double mapscale;
        m_pTrans->GetMapScale(mapscale);
        double scalequot = refscale / mapscale;

        factor = quotiety * scalequot;

        //将画笔尺寸换算为象素（pixel）单位
        pensize1 = (long)(m_LineWidth * factor);
    }
    else
    {
        //不使用参考比例尺，屏幕图元尺寸始终恒定，不随显示比例尺变化而变化

        factor = quotiety;

        //将画笔尺寸换算为象素（pixel）单位
        pensize1 = (long)(m_LineWidth * factor);
    }

    pensize1 *= m_ParentSize;
    if (0 >= pensize1)
    {
        pensize1 = 1;
    }

    //保存当前的场景，设置新的场景
    HPEN pen = ::CreatePen(PS_SOLID, pensize1, m_Color);
    pensaved = (HPEN)::SelectObject(m_DC, pen);
    LOGBRUSH longbrush;
    longbrush.lbColor = m_Color;
    if (m_Solid)
    {
        longbrush.lbStyle = BS_SOLID;
    }
    else
    {
        longbrush.lbStyle = BS_HOLLOW;
    }
    HBRUSH brush = ::CreateBrushIndirect(&longbrush);
    brushsaved = (HBRUSH)::SelectObject(m_DC, brush);
    rop2saved = ::SetROP2(m_DC, m_ROP2);
}

void _SimplePointSymbol::AfterDraw(const HPEN& pensaved,
    const HBRUSH& brushsaved, const long& rop2saved) const
{
    HPEN pen = (HPEN)::SelectObject(m_DC, pensaved);
    ::DeleteObject(pen);
    HBRUSH brush = (HBRUSH)::SelectObject(m_DC, brushsaved);
    ::DeleteObject(brush);
    ::SetROP2(m_DC, rop2saved);
}
//================================================================================


//================================================================================
//  CSimplePointSymbol
//================================================================================
CSimplePointSymbol::CSimplePointSymbol(const long code)
{
    INIT_REFCOUNT
    m_Code = code;
}

bool __stdcall CSimplePointSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "IPointSymbol"))
        || (0 == strcmp(interfacename, "ISimplePointSymbol"))
        || (0 == strcmp(interfacename, "CSimplePointSymbol")))
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

dword __stdcall CSimplePointSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    this->_SimplePointSymbol::_SaveInstance(pStream, assist);
    return pStream->GetPos() - oldpos;
}

dword __stdcall CSimplePointSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    this->_SimplePointSymbol::_LoadInstance(pStream, assist);
    return pStream->GetPos() - oldpos;
}

bool __stdcall CSimplePointSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CSimplePointSymbol::Clone(IObjPtr& pObj) const
{
    CSimplePointSymbolPtr pSSymbol = new CSimplePointSymbol;

    pSSymbol->m_Code = m_Code;
    pSSymbol->m_ROP2 = m_ROP2;
    pSSymbol->m_Name = m_Name;
    pSSymbol->m_Color = m_Color;
    pSSymbol->m_ColorLock = m_ColorLock;
    pSSymbol->m_Diameter = m_Diameter;
    pSSymbol->m_Angle = m_Angle;
    pSSymbol->m_OffsetX = m_OffsetX;
    pSSymbol->m_OffsetY = m_OffsetY;
    pSSymbol->m_LineWidth = m_LineWidth;
    pSSymbol->m_Solid = m_Solid;

    CAST_PTR(pSSymbol, pObj, IObj)

    return true;
}

void CSimplePointSymbol::DrawSimplePoint(const long X, const long Y) const
{
    HPEN pensaved;
    HBRUSH brushsaved;
    long rop2saved;
    double factor;
    this->BeforeDraw(pensaved, brushsaved, rop2saved, factor);
    long halfsize1 = long(m_Diameter * factor / 2 * m_ParentSize);
    if (1 > halfsize1) halfsize1 = 1;

    double tmpx = m_OffsetX * m_ParentSize;
    double tmpy = m_OffsetY * m_ParentSize;
    WKSPoint offsetabsolute(tmpx, tmpy);

    RotateDegreeInvert(offsetabsolute, WKSPoint(0, 0), m_ParentAngle);
    offsetabsolute.x += m_ParentOffsetX;
    offsetabsolute.y += m_ParentOffsetY;

    long offsetX = offsetabsolute.x * factor;
    long offsetY = -offsetabsolute.y * factor;
    long left = X - halfsize1 + offsetX;
    long top = Y - halfsize1 + offsetY;
    long right = X + halfsize1 + offsetX;
    long bottom = Y + halfsize1 + offsetY;

    ::Ellipse(m_DC, left, top, right, bottom);

    this->AfterDraw(pensaved, brushsaved, rop2saved);
}
//================================================================================


//================================================================================
//  _EnvelopePointSymbol
//================================================================================
_EnvelopePointSymbol::_EnvelopePointSymbol(const bool solid, const long code)
{
    INIT_REFCOUNT

    m_Width = 6;
    m_Height = 4;
    m_Solid = solid;
    m_Code = code;
}

bool __stdcall _EnvelopePointSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "IPointSymbol"))
        || (0 == strcmp(interfacename, "_EnvelopePointSymbol")))
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

dword __stdcall _EnvelopePointSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    this->_SimplePointSymbol::_SaveInstance(pStream, assist);

    pStream->WriteData(&m_Width, sizeof(double));
    pStream->WriteData(&m_Height, sizeof(double));

    return pStream->GetPos() - oldpos;
}

dword __stdcall _EnvelopePointSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    this->_SimplePointSymbol::_LoadInstance(pStream, assist);

    pStream->ReadData(&m_Width, sizeof(double));
    pStream->ReadData(&m_Height, sizeof(double));

    return pStream->GetPos() - oldpos;
}

bool __stdcall _EnvelopePointSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool _EnvelopePointSymbol::Clone(IObjPtr& pObj) const
{
    _EnvelopePointSymbolPtr pSymbol = new _EnvelopePointSymbol;

    pSymbol->m_Code = m_Code;
    pSymbol->m_ROP2 = m_ROP2;
    pSymbol->m_Name = m_Name;
    pSymbol->m_Color = m_Color;
    pSymbol->m_ColorLock = m_ColorLock;
    pSymbol->m_Diameter = m_Diameter;
    pSymbol->m_Angle = m_Angle;
    pSymbol->m_OffsetX = m_OffsetX;
    pSymbol->m_OffsetY = m_OffsetY;
    pSymbol->m_LineWidth = m_LineWidth;
    pSymbol->m_Solid = m_Solid;

    pSymbol->m_Width = m_Width;
    pSymbol->m_Height = m_Height;

    CAST_PTR(pSymbol, pObj, IObj)

    return true;
}

bool __stdcall _EnvelopePointSymbol::SetWidth(const double width)
{
    m_Width = width;
    return true;
}

bool __stdcall _EnvelopePointSymbol::GetWidth(double& width) const
{
    width = m_Width;
    return true;
}

bool __stdcall _EnvelopePointSymbol::SetHeight(const double height)
{
    m_Height = height;
    return true;
}

bool __stdcall _EnvelopePointSymbol::GetHeight(double& height) const
{
    height = m_Height;
    return true;
}

void _EnvelopePointSymbol::DrawSimplePoint(const long X, const long Y) const
{
    WKSPoint pnt1, pnt2, pnt3, pnt4;
    pnt1.x = -m_Width / 2;
    pnt1.y = -m_Height / 2;
    pnt2.x = pnt1.x;
    pnt2.y = m_Height / 2;
    pnt3.x = m_Width / 2;
    pnt3.y = pnt2.y;
    pnt4.x = pnt3.x;
    pnt4.y = pnt1.y;
    RotateDegreeInvert(pnt1, WKSPoint(0, 0), m_Angle);
    RotateDegreeInvert(pnt2, WKSPoint(0, 0), m_Angle);
    RotateDegreeInvert(pnt3, WKSPoint(0, 0), m_Angle);
    RotateDegreeInvert(pnt4, WKSPoint(0, 0), m_Angle);

    pnt1.x += m_OffsetX;
    pnt1.y += m_OffsetY;
    pnt2.x += m_OffsetX;
    pnt2.y += m_OffsetY;
    pnt3.x += m_OffsetX;
    pnt3.y += m_OffsetY;
    pnt4.x += m_OffsetX;
    pnt4.y += m_OffsetY;

    pnt1.x *=  m_ParentSize;
    pnt1.y *=  m_ParentSize;
    pnt2.x *=  m_ParentSize;
    pnt2.y *=  m_ParentSize;
    pnt3.x *=  m_ParentSize;
    pnt3.y *=  m_ParentSize;
    pnt4.x *=  m_ParentSize;
    pnt4.y *=  m_ParentSize;

    RotateDegreeInvert(pnt1, WKSPoint(0, 0), m_ParentAngle);
    RotateDegreeInvert(pnt2, WKSPoint(0, 0), m_ParentAngle);
    RotateDegreeInvert(pnt3, WKSPoint(0, 0), m_ParentAngle);
    RotateDegreeInvert(pnt4, WKSPoint(0, 0), m_ParentAngle);

    pnt1.x += m_ParentOffsetX;
    pnt1.y += m_ParentOffsetY;
    pnt2.x += m_ParentOffsetX;
    pnt2.y += m_ParentOffsetY;
    pnt3.x += m_ParentOffsetX;
    pnt3.y += m_ParentOffsetY;
    pnt4.x += m_ParentOffsetX;
    pnt4.y += m_ParentOffsetY;

    HPEN pensaved;
    HBRUSH brushsaved;
    long rop2saved;
    double factor;
    this->BeforeDraw(pensaved, brushsaved, rop2saved, factor);

    static POINT pnts[5];
    pnts[0].x = pnt1.x*factor + X;
    pnts[0].y = -pnt1.y*factor + Y;
    pnts[1].x = pnt2.x*factor + X;
    pnts[1].y = -pnt2.y*factor + Y;
    pnts[2].x = pnt3.x*factor + X;
    pnts[2].y = -pnt3.y*factor + Y;
    pnts[3].x = pnt4.x*factor + X;
    pnts[3].y = -pnt4.y*factor + Y;
    pnts[4] = pnts[0];

    if (m_Solid)
    {
        ::Polygon(m_DC, pnts, 5);
    }
    else
    {
        ::Polyline(m_DC, pnts, 5);
    }

    this->AfterDraw(pensaved, brushsaved, rop2saved);
}
//================================================================================


//================================================================================
//  CEnvelopePointSymbol
//================================================================================
CEnvelopePointSymbol::CEnvelopePointSymbol(const bool solid, const long code)
{
    INIT_REFCOUNT
    m_pEP = new _EnvelopePointSymbol(solid, code);
}

bool __stdcall CEnvelopePointSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "IPointSymbol"))
        || (0 == strcmp(interfacename, "IEnvelopePointSymbol"))
        || (0 == strcmp(interfacename, "CEnvelopePointSymbol")))
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

dword __stdcall CEnvelopePointSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    this->m_pEP->_SaveInstance(pStream, assist);
    return pStream->GetPos() - oldpos;
}

dword __stdcall CEnvelopePointSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    this->m_pEP->_LoadInstance(pStream, assist);
    return pStream->GetPos() - oldpos;
}

bool __stdcall CEnvelopePointSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CEnvelopePointSymbol::Clone(IObjPtr& pObj) const
{
    CEnvelopePointSymbolPtr pSymbol = new CEnvelopePointSymbol;
    IObjPtr pNewObj;
    this->m_pEP->Clone(pNewObj);
    CAST_PTR(pNewObj, pSymbol->m_pEP, _EnvelopePointSymbol)

    CAST_PTR(pSymbol, pObj, IObj)

    return true;
}

bool __stdcall CEnvelopePointSymbol::SetCode(const long& code)
{
    return this->m_pEP->SetCode(code);
}

long __stdcall CEnvelopePointSymbol::GetCode() const
{
    return this->m_pEP->GetCode();
}

bool __stdcall CEnvelopePointSymbol::SetName(const char* const name)
{
    return this->m_pEP->SetName(name);
}

const char* __stdcall CEnvelopePointSymbol::GetName() const
{
    return this->m_pEP->GetName();
}

bool __stdcall CEnvelopePointSymbol::GetDC(HDC& dc) const
{
    return this->m_pEP->GetDC(dc);
}

bool __stdcall CEnvelopePointSymbol::GetROP2(long& rop2) const
{
    return this->m_pEP->GetROP2(rop2);
}

bool CEnvelopePointSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    return this->m_pEP->Prepare(dc, pTrans, rop2);
}

bool __stdcall CEnvelopePointSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    return this->m_pEP->GetDisplayTransformation(ppTrans);
}

bool __stdcall CEnvelopePointSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    return this->m_pEP->Prepare(dc, pTrans, rop2);
}

bool __stdcall CEnvelopePointSymbol::Draw(const IGeometry* pGeometry) const
{
    return m_pEP->Draw(pGeometry);
}

dword __stdcall CEnvelopePointSymbol::DrawStream(const IStreamX* pStream) const
{
    return this->m_pEP->DrawStream(pStream);
}


bool CEnvelopePointSymbol::Draw(const IGeometryPtr pGeometry) const
{
    return this->m_pEP->Draw(pGeometry);
}

dword CEnvelopePointSymbol::DrawStream(const CStreamPtr pStream) const
{
    return this->m_pEP->DrawStream(pStream);
}

bool __stdcall CEnvelopePointSymbol::_ParentOffset(const WKSPoint& offset,
    const double angle, const double size)
{
    return this->m_pEP->_ParentOffset(offset, angle, size);
}

bool __stdcall CEnvelopePointSymbol::SetColor(const COLORREF color)
{
    return this->m_pEP->SetColor(color);
}

bool __stdcall CEnvelopePointSymbol::GetColor(COLORREF& color) const
{
    return this->m_pEP->GetColor(color);
}

void __stdcall CEnvelopePointSymbol::SetColorLock(const bool lock)
{
    this->m_pEP->SetColorLock(lock);
}

void __stdcall CEnvelopePointSymbol::GetColorLock(bool& color) const
{
    this->m_pEP->GetColorLock(color);
}

SymbolType __stdcall CEnvelopePointSymbol::GetSymbolType() const
{
    return this->m_pEP->GetSymbolType();
}

bool CEnvelopePointSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    return this->m_pEP->GetDisplayTransformation(pTrans);
}

bool __stdcall CEnvelopePointSymbol::SetLineWidth(const double width)
{
    this->m_pEP->m_LineWidth = width;
    return true;
}

bool __stdcall CEnvelopePointSymbol::GetLineWidth(double& width) const
{
    width = this->m_pEP->m_LineWidth;
    return true;
}

void __stdcall CEnvelopePointSymbol::SetSolid(const bool solid)
{
    this->m_pEP->m_Solid = solid;
}

void __stdcall CEnvelopePointSymbol::GetSolid(bool& solid) const
{
    solid = this->m_pEP->m_Solid;
}

bool __stdcall CEnvelopePointSymbol::SetAngle(const double angle)
{
    return this->m_pEP->SetAngle(angle);
}

bool __stdcall CEnvelopePointSymbol::GetAngle(double& angle) const
{
    return this->m_pEP->GetAngle(angle);
}

bool __stdcall CEnvelopePointSymbol::SetOffset(const double x, const double y)
{
    return this->m_pEP->SetOffset(x, y);
}

bool __stdcall CEnvelopePointSymbol::GetOffset(double& x, double& y) const
{
    return this->m_pEP->GetOffset(x, y);
}

bool __stdcall CEnvelopePointSymbol::SetWidth(const double width)
{
    return this->m_pEP->SetWidth(width);
}

bool __stdcall CEnvelopePointSymbol::GetWidth(double& width) const
{
    return this->m_pEP->GetWidth(width);
}

bool __stdcall CEnvelopePointSymbol::SetHeight(const double height)
{
    return this->m_pEP->SetHeight(height);
}

bool __stdcall CEnvelopePointSymbol::GetHeight(double& height) const
{
    return this->m_pEP->GetHeight(height);
}
//================================================================================


//================================================================================
//  _PolyPointSymbol
//================================================================================
_PolyPointSymbol::_PolyPointSymbol(const bool solid, const long code)
{
    INIT_REFCOUNT

    m_Solid = solid;
    m_Code = code;
}

bool __stdcall _PolyPointSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "IPointSymbol"))
        || (0 == strcmp(interfacename, "_PolyPointSymbol")))
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

dword __stdcall _PolyPointSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    this->_SimplePointSymbol::_SaveInstance(pStream, assist);
    dword pointcount = m_Points.size();
    pStream->WriteData(&pointcount, sizeof(dword));
    for (dword i = 0; i < pointcount; i++)
    {
        pStream->WriteData(&m_Points[i], sizeof(WKSPoint));
    }
    return pStream->GetPos() - oldpos;
}

dword __stdcall _PolyPointSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    m_Points.clear();
    dword oldpos = pStream->GetPos();
    this->_SimplePointSymbol::_LoadInstance(pStream, assist);
    dword pointcount;
    pStream->ReadData(&pointcount, sizeof(dword));
    for (dword i = 0; i < pointcount; i++)
    {
        WKSPoint pnt;
        pStream->ReadData(&pnt, sizeof(WKSPoint));
        m_Points.push_back(pnt);
    }
    return pStream->GetPos() - oldpos;
}

bool __stdcall _PolyPointSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool _PolyPointSymbol::Clone(IObjPtr& pObj) const
{
    _PolyPointSymbolPtr pSymbol = new _PolyPointSymbol;

    pSymbol->m_Code = m_Code;
    pSymbol->m_ROP2 = m_ROP2;
    pSymbol->m_Name = m_Name;
    pSymbol->m_Color = m_Color;
    pSymbol->m_ColorLock = m_ColorLock;
    pSymbol->m_Diameter = m_Diameter;
    pSymbol->m_Angle = m_Angle;
    pSymbol->m_OffsetX = m_OffsetX;
    pSymbol->m_OffsetY = m_OffsetY;
    pSymbol->m_LineWidth = m_LineWidth;
    pSymbol->m_Solid = m_Solid;

    dword pointcount = m_Points.size();
    for (dword i = 0; i < pointcount; i++)
    {
        pSymbol->m_Points.push_back(m_Points[i]);
    }

    CAST_PTR(pSymbol, pObj, IObj)

    return true;
}

bool _PolyPointSymbol::AddNode(const WKSPoint& node)
{
    m_Points.push_back(node);
    return true;
}

bool _PolyPointSymbol::MoveNode(const WKSPoint& node, const dword index)
{
    if ((m_Points.size() <= index) || (0 > index)) {return false;}

    m_Points[index] = node;
    return true;
}

bool _PolyPointSymbol::DeleteNode(const dword index)
{
    if ((m_Points.size() <= index) || (0 > index)) {return false;}

    vector<WKSPoint>::iterator it = m_Points.begin();
    it += index;
    m_Points.erase(it);
    return true;
}

bool _PolyPointSymbol::GetNode(const dword index, WKSPoint& node) const
{
    if ((m_Points.size() <= index) || (0 > index)) {return false;}

    node = m_Points[index];
    return true;
}

bool _PolyPointSymbol::SetNode(const dword index, const WKSPoint& node)
{
    if ((m_Points.size() <= index) || (0 > index)) {return false;}

    m_Points[index] = node;
    return true;
}

bool _PolyPointSymbol::SetNodeOrder(const dword index, const dword neworder)
{
    if ((m_Points.size() <= index) || (0 > index)) {return false;}
    if (neworder >= m_Points.size()) return false;

    if (neworder == index) return true;

    WKSPoint node = m_Points[index];
    vector<WKSPoint>::iterator it = m_Points.begin();
    std::advance(it, index);
    m_Points.erase(it);
    it = m_Points.begin();
    std::advance(it, neworder);
    m_Points.insert(it, node);
    return true;
}

dword _PolyPointSymbol::GetNodeCount() const
{
    return m_Points.size();
}

void _PolyPointSymbol::ClearNodes()
{
    m_Points.clear();
}

void _PolyPointSymbol::DrawSimplePoint(const long X, const long Y) const
{
    HPEN pensaved;
    HBRUSH brushsaved;
    long rop2saved;
    double factor;
    this->BeforeDraw(pensaved, brushsaved, rop2saved, factor);

    dword pntcount = m_Points.size();
    pntcount = pntcount <= 1000 ? pntcount : 1000;
    static POINT pnts[1000];
    for (dword i = 0; i < pntcount; i++)
    {
        WKSPoint pnt = m_Points[i];
        //先旋转再移动再放缩再旋转再移动
        RotateDegreeInvert(pnt, WKSPoint(0, 0), m_Angle);

        pnt.x += m_OffsetX;
        pnt.y += m_OffsetY;

        pnt.x *=  m_ParentSize;
        pnt.y *=  m_ParentSize;

        RotateDegreeInvert(pnt, WKSPoint(0, 0), m_ParentAngle);

        pnt.x += m_ParentOffsetX;
        pnt.y += m_ParentOffsetY;
        
        pnts[i].x = pnt.x * factor + X;
        pnts[i].y = -pnt.y * factor + Y;
    }

    if (m_Solid)
    {
        ::Polygon(m_DC, pnts, pntcount);
    }
    else
    {
        ::Polyline(m_DC, pnts, pntcount);
    }

    this->AfterDraw(pensaved, brushsaved, rop2saved);
}
//================================================================================


//================================================================================
//  CPolyPointSymbol
//================================================================================
CPolyPointSymbol::CPolyPointSymbol(const bool solid, const long code)
{
    INIT_REFCOUNT
    m_pPP = new _PolyPointSymbol(solid, code);
}

bool __stdcall CPolyPointSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "IPointSymbol"))
        || (0 == strcmp(interfacename, "IPolyPointSymbol"))
        || (0 == strcmp(interfacename, "CPolyPointSymbol")))
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

dword __stdcall CPolyPointSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    this->m_pPP->_SaveInstance(pStream, assist);
    return pStream->GetPos() - oldpos;
}

dword __stdcall CPolyPointSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    this->m_pPP->_LoadInstance(pStream, assist);
    return pStream->GetPos() - oldpos;
}

bool __stdcall CPolyPointSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CPolyPointSymbol::Clone(IObjPtr& pObj) const
{
    CPolyPointSymbolPtr pSymbol = new CPolyPointSymbol;
    IObjPtr pNewObj;
    this->m_pPP->Clone(pNewObj);
    CAST_PTR(pNewObj, pSymbol->m_pPP, _PolyPointSymbol)

    CAST_PTR(pSymbol, pObj, IObj)

    return true;
}

bool __stdcall CPolyPointSymbol::SetCode(const long& code)
{
    return this->m_pPP->SetCode(code);
}

long __stdcall CPolyPointSymbol::GetCode() const
{
    return this->m_pPP->GetCode();
}

bool __stdcall CPolyPointSymbol::SetName(const char* const name)
{
    return this->m_pPP->SetName(name);
}

const char* __stdcall CPolyPointSymbol::GetName() const
{
    return this->m_pPP->GetName();
}

bool __stdcall CPolyPointSymbol::GetDC(HDC& dc) const
{
    return this->m_pPP->GetDC(dc);
}

bool __stdcall CPolyPointSymbol::GetROP2(long& rop2) const
{
    return this->m_pPP->GetROP2(rop2);
}

bool CPolyPointSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    return this->m_pPP->Prepare(dc, pTrans, rop2);
}

bool __stdcall CPolyPointSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    return this->m_pPP->GetDisplayTransformation(ppTrans);
}

bool __stdcall CPolyPointSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    return this->m_pPP->Prepare(dc, pTrans, rop2);
}

bool __stdcall CPolyPointSymbol::Draw(const IGeometry* pGeometry) const
{
    return this->m_pPP->Draw(pGeometry);
}

dword __stdcall CPolyPointSymbol::DrawStream(const IStreamX* pStream) const
{
    return this->m_pPP->DrawStream(pStream);
}


bool CPolyPointSymbol::Draw(const IGeometryPtr pGeometry) const
{
    return this->m_pPP->Draw(pGeometry);
}

dword CPolyPointSymbol::DrawStream(const CStreamPtr pStream) const
{
    return this->m_pPP->DrawStream(pStream);
}

bool __stdcall CPolyPointSymbol::_ParentOffset(const WKSPoint& offset,
    const double angle, const double size)
{
    return this->m_pPP->_ParentOffset(offset, angle, size);
}

bool __stdcall CPolyPointSymbol::SetColor(const COLORREF color)
{
    return this->m_pPP->SetColor(color);
}

bool __stdcall CPolyPointSymbol::GetColor(COLORREF& color) const
{
    return this->m_pPP->GetColor(color);
}

void __stdcall CPolyPointSymbol::SetColorLock(const bool lock)
{
    this->m_pPP->SetColorLock(lock);
}

void __stdcall CPolyPointSymbol::GetColorLock(bool& color) const
{
    this->m_pPP->GetColorLock(color);
}

SymbolType __stdcall CPolyPointSymbol::GetSymbolType() const
{
    return this->m_pPP->GetSymbolType();
}

bool CPolyPointSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    return this->m_pPP->GetDisplayTransformation(pTrans);
}

bool __stdcall CPolyPointSymbol::AddNode(const WKSPoint& node)
{
    return this->m_pPP->AddNode(node);
}

bool __stdcall CPolyPointSymbol::MoveNode(const WKSPoint& node, const dword index)
{
    return this->m_pPP->MoveNode(node, index);
}

bool __stdcall CPolyPointSymbol::DeleteNode(const dword index)
{
    return this->m_pPP->DeleteNode(index);
}

bool __stdcall CPolyPointSymbol::GetNode(const dword index, WKSPoint& node) const
{
    return this->m_pPP->GetNode(index, node);
}

bool __stdcall CPolyPointSymbol::SetNode(const dword index, const WKSPoint& node)
{
    return this->m_pPP->SetNode(index, node);
}

bool __stdcall CPolyPointSymbol::SetNodeOrder(const dword index, const dword neworder)
{
    return this->m_pPP->SetNodeOrder(index, neworder);
}

dword __stdcall CPolyPointSymbol::GetNodeCount() const
{
    return this->m_pPP->GetNodeCount();
}

void __stdcall CPolyPointSymbol::ClearNodes()
{
    this->m_pPP->ClearNodes();
}

bool __stdcall CPolyPointSymbol::SetLineWidth(const double width)
{
    this->m_pPP->m_LineWidth = width;
    return true;
}

bool __stdcall CPolyPointSymbol::GetLineWidth(double& width) const
{
    width = this->m_pPP->m_LineWidth;
    return true;
}

void __stdcall CPolyPointSymbol::SetSolid(const bool solid)
{
    this->m_pPP->m_Solid = solid;
}

void __stdcall CPolyPointSymbol::GetSolid(bool& solid) const
{
    solid = this->m_pPP->m_Solid;
}

bool __stdcall CPolyPointSymbol::SetAngle(const double angle)
{
    return this->m_pPP->SetAngle(angle);
}

bool __stdcall CPolyPointSymbol::GetAngle(double& angle) const
{
    return this->m_pPP->GetAngle(angle);
}

bool __stdcall CPolyPointSymbol::SetOffset(const double x, const double y)
{
    return this->m_pPP->SetOffset(x, y);
}

bool __stdcall CPolyPointSymbol::GetOffset(double& x, double& y) const
{
    return this->m_pPP->GetOffset(x, y);
}
//================================================================================


//================================================================================
//  线模板
//================================================================================
CLineSimpleTemplate::CLineSimpleTemplate()
{
    INIT_REFCOUNT

    m_FirstMark = true;
    m_Factor = 1;
}

bool __stdcall CLineSimpleTemplate::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ILineSimpleTemplate"))
        || (0 == strcmp(interfacename, "CLineSimpleTemplate")))
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

dword __stdcall CLineSimpleTemplate::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_Sectors.size();
    ps->Write(size);
    vector<dword>::const_iterator it = m_Sectors.begin();
    while (it != m_Sectors.end())
    {
        ps->Write(*it);
        it++;
    }

    ps->WriteBool(m_FirstMark);
    ps->Write(m_Factor);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CLineSimpleTemplate::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size;
    ps->Read(size);
    m_Sectors.resize(size);
    for (long i = 0; i < size; i++)
    {
        dword mark;
        ps->Read(mark);
        m_Sectors[i] = mark;
    }

    ps->ReadBool(m_FirstMark);
    ps->Read(m_Factor);

    return pStream->GetPos() - oldpos;
}

bool __stdcall CLineSimpleTemplate::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CLineSimpleTemplate::Clone(IObjPtr& pObj) const
{
    CLineSimpleTemplatePtr pTemplate = new CLineSimpleTemplate;

    vector<dword>::const_iterator it = m_Sectors.begin();
    while (it != m_Sectors.end())
    {
        pTemplate->m_Sectors.push_back(*it);
        it++;
    }

    pTemplate->m_FirstMark = m_FirstMark;
    pTemplate->m_Factor = m_Factor;

    CAST_PTR(pTemplate, pObj, IObj)
    return true;
}

bool __stdcall CLineSimpleTemplate::Clear()
{
    m_Sectors.clear();
    return true;
}

bool __stdcall CLineSimpleTemplate::GetCount(dword& count) const
{
    count = m_Sectors.size();
    return true;
}

bool __stdcall CLineSimpleTemplate::SetFirstMark(const bool mark)
{
    m_FirstMark = mark;
    return true;
}

bool __stdcall CLineSimpleTemplate::GetFirstMark(bool& mark) const
{
    mark = m_FirstMark;
    return true;
}

bool __stdcall CLineSimpleTemplate::AddSector(const dword sector)
{
    if ((0 == sector) || (100 < sector)) return false;
    m_Sectors.push_back(sector);
    return true;
}

bool __stdcall CLineSimpleTemplate::GetSector(const dword index, dword& sector) const
{
    if (m_Sectors.size() <= index) return false;
    sector = m_Sectors[index];
    return true;
}

bool __stdcall CLineSimpleTemplate::SetFactor(const double factor)
{
    if (0.01 > factor) return false;
    m_Factor = factor;
    return true;
}

bool __stdcall CLineSimpleTemplate::GetFactor(double& factor) const
{
    factor = m_Factor;
    return true;
}
//================================================================================


//================================================================================
//  CSimpleLineSymbol
//================================================================================
CSimpleLineSymbol::CSimpleLineSymbol(const long code)
{
    INIT_REFCOUNT

    m_Code = code;
    m_Name = "undefined";
    m_Color = RGB(20, 200, 10);
    m_ColorLock = false;
    m_Width = 0.3;
    m_Offset = 0;
    m_ParentOffset = 0;
    m_ParentSize = 1;
}

bool __stdcall CSimpleLineSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "ILineSymbol"))
        || (0 == strcmp(interfacename, "ISimpleLineSymbol"))
        || (0 == strcmp(interfacename, "CSimpleLineSymbol")))
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

dword __stdcall CSimpleLineSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_Code);
    ps->Write(m_ROP2);
    ps->Write(m_Name);
    ps->Write(m_Color);
    ps->WriteBool(m_ColorLock);
    ps->Write(m_Width);
    ps->Write(m_Offset);

    bool usetemplate = m_pTemplate.Assigned();
    ps->WriteBool(usetemplate);
    if (usetemplate)
    {
        m_pTemplate->_DumpTo(pStream, assist);
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CSimpleLineSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_pTemplate.Clear();

    ps->Read(m_Code);
    ps->Read(m_ROP2);
    ps->Read(m_Name);
    ps->Read(m_Color);
    ps->ReadBool(m_ColorLock);
    ps->Read(m_Width);
    ps->Read(m_Offset);

    bool usetemplate;
    ps->ReadBool(usetemplate);
    if (usetemplate)
    {
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom(ps, pPersist, assist);
        CAST_PTR(pPersist, m_pTemplate, ILineSimpleTemplate)
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CSimpleLineSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CSimpleLineSymbol::Clone(IObjPtr& pObj) const
{
    CSimpleLineSymbolPtr pSymbol = new CSimpleLineSymbol;

    pSymbol->m_Code = m_Code;
    pSymbol->m_ROP2 = m_ROP2;
    pSymbol->m_Name = m_Name;
    pSymbol->m_Color = m_Color;
    pSymbol->m_ColorLock = m_ColorLock;
    pSymbol->m_Width = m_Width;
    pSymbol->m_Offset = m_Offset;
    if (m_pTemplate.Assigned())
    {
        IObjPtr pObjTmp;
        CLONE_PTR(m_pTemplate, pObjTmp)
        CAST_PTR(pObjTmp, pSymbol->m_pTemplate, ILineSimpleTemplate)
    }

    CAST_PTR(pSymbol, pObj, IObj)
    return true;
}

bool __stdcall CSimpleLineSymbol::SetCode(const long& code)
{
    m_Code = code;
    return true;
}

long __stdcall CSimpleLineSymbol::GetCode() const
{
    return m_Code;
}

bool __stdcall CSimpleLineSymbol::SetName(const char* const name)
{
    m_Name = name;
    return true;
}

const char* __stdcall CSimpleLineSymbol::GetName() const
{
    return m_Name.c_str();
}

bool __stdcall CSimpleLineSymbol::GetDC(HDC& dc) const
{
    dc = m_DC;
    return true;
}

bool CSimpleLineSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    pTrans = m_pTrans;
    return true;
}

bool __stdcall CSimpleLineSymbol::GetROP2(long& rop2) const
{
    rop2 = m_ROP2;
    return true;
}

bool CSimpleLineSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    m_DC = dc;
    m_pTrans = pTrans;
    m_ROP2 = rop2;

    return true;
}

bool __stdcall CSimpleLineSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    if (_invalid(ppTrans)) return false;
    assert(!*ppTrans);

    if (!m_pTrans.Assigned()) return false;
    *ppTrans = m_pTrans._p();
    if (_valid(*ppTrans)) (*ppTrans)->_AddRef();
    return true;
}

bool __stdcall CSimpleLineSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    m_DC = dc;
    m_pTrans = (CDisplayTransformation*)pTrans;
    m_ROP2 = rop2;
    return true;
}

bool __stdcall CSimpleLineSymbol::Draw(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->Draw(pg);
}

dword __stdcall CSimpleLineSymbol::DrawStream(const IStreamX* pStream) const
{
    if (_invalid(pStream)) return false;
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawStream(ps);
}

bool _draw_parallel_line(const IGeometryPtr pGeometry, const double& distance,
    const ILineSimpleTemplatePtr pTemplate, const HDC dc,
    const CDisplayTransformationPtr pDisplayTrans)
{
    bool r = false;
    CPathPtr pPath;
    dword i = 0;
    GeometryType gt = pGeometry->GetGeometryType();
    if (GEOMETRYTYPE_POLYLINE == gt)
    {
        CPolylinePtr pPL;
        CAST_PTR(pGeometry, pPL, CPolyline)
        dword pathcount = pPL->GetPathCount();
        for (i = 0; i < pathcount; i++)
        {
            pPL->GetPathRef(pPath, i);
            IGeometryPtr pGeoTmp;
            CAST_PTR(pPath, pGeoTmp, IGeometry)
            _draw_parallel_line(pGeoTmp, distance, pTemplate, dc, pDisplayTrans);
        }

        r = true;
    }
    else if (GEOMETRYTYPE_PATH == gt)
    {
        CAST_PTR(pGeometry, pPath, CPath)
        r = DrawPath(pPath, pDisplayTrans, dc, pTemplate, distance);
    }

    return r;
}

bool CSimpleLineSymbol::Draw(const IGeometryPtr pGeometry) const
{
    if (!m_pTrans.Assigned() || !pGeometry.Assigned())
        return false;

    IGeometryPtr pClippedGeometry = pGeometry;

    double attitude = 0.0;
    m_pTrans->GetAttitude(attitude);
    if (attitude > 0.0001)
    {
        pClippedGeometry = NULL;
        m_pTrans->ClipGeometry(pGeometry._p(), pClippedGeometry._ref());
    }

    if (!pClippedGeometry.Assigned())
    {
        return false;
    }

    bool r = false;
    HPEN pensaved;
    long rop2saved;
    this->BeforeDraw(pensaved, rop2saved);

    double absoluteoffset = m_Offset + m_ParentOffset + m_Offset*(m_ParentSize-1);
    r = _draw_parallel_line(pClippedGeometry, absoluteoffset, m_pTemplate,
        m_DC, m_pTrans);

    this->AfterDraw(pensaved, rop2saved);
    return r;
}

dword CSimpleLineSymbol::DrawStream(const CStreamPtr pStream) const
{
    if (!m_pTrans.Assigned() || !pStream.Assigned())
        return 0;

    IGeometryPtr pGeometry;
    dword r = 0;

    double attitude = 0.0;
    m_pTrans->GetAttitude(attitude);
    if ((attitude) > 0.0001)
    {
        //要切
        r = Stream2Geometry(pStream, pGeometry);
        if (pGeometry.Assigned())
        {
            this->Draw(pGeometry);
        }
    }
    else
    {
        double absoluteoffset = m_Offset + m_ParentOffset;
        if ((fabs(absoluteoffset) > 0.00001) || m_pTemplate.Assigned())
        {
            r = Stream2Geometry(pStream, pGeometry);
            if (!pGeometry.Assigned()) {return r;}
            this->Draw(pGeometry);
        }
        else
        {
            HPEN pensaved;
            long rop2saved;
            this->BeforeDraw(pensaved, rop2saved);
            r = DrawGeneralStream(pStream, m_pTrans, m_DC);
            this->AfterDraw(pensaved, rop2saved);
        }
    }

    return r;
}

bool __stdcall CSimpleLineSymbol::SetColor(const COLORREF color)
{
    m_Color = color;
    return true;
}

bool __stdcall CSimpleLineSymbol::GetColor(COLORREF& color) const
{
    color = m_Color;
    return true;
}

void __stdcall CSimpleLineSymbol::SetColorLock(const bool lock)
{
    m_ColorLock = lock;
}

void __stdcall CSimpleLineSymbol::GetColorLock(bool& color) const
{
    color = m_ColorLock;
}

SymbolType __stdcall CSimpleLineSymbol::GetSymbolType() const
{
    return SYMBOLTYPE_LINE;
}

bool __stdcall CSimpleLineSymbol::SetWidth(const double width)
{
    m_Width = width;
    return true;
}

bool __stdcall CSimpleLineSymbol::GetWidth(double& width) const
{
    width = m_Width;
    return true;
}

bool __stdcall CSimpleLineSymbol::SetTemplate(const ILineSimpleTemplate* const pTemplate)
{
    m_pTemplate = (ILineSimpleTemplate*)pTemplate;
    return true;
}

bool __stdcall CSimpleLineSymbol::GetTemplate(ILineSimpleTemplate** ppTemplate) const
{
    if (_invalid(ppTemplate)) return false;
    assert(!*ppTemplate);

    *ppTemplate = m_pTemplate._p();
    if (_valid(*ppTemplate))
    {
        (*ppTemplate)->_AddRef();
        return true;
    }

    return false;
}

bool __stdcall CSimpleLineSymbol::SetOffset(const double offset)
{
    m_Offset = offset;
    return true;
}

bool __stdcall CSimpleLineSymbol::GetOffset(double& offset) const
{
    offset = m_Offset;
    return true;
}

bool __stdcall CSimpleLineSymbol::_ParentOffset(const double offset,
    const double size)
{
    m_ParentOffset = offset;
    m_ParentSize = size;
    return true;
}

void CSimpleLineSymbol::BeforeDraw(HPEN& pensaved, long& rop2saved) const
{
    //计算象素->毫米对应关系
    long logpixelx, logpixely;
    m_pTrans->GetLogPixel(logpixelx, logpixely);
    double quotiety_x = logpixelx
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double quotiety_y = logpixely
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double quotiety = (quotiety_x + quotiety_y) / 2;

    double refscale;
    m_pTrans->GetReferenceScale(refscale);

    long pensize1;
    if (refscale > 0)
    {
        //使用参考比例尺，屏幕图元尺寸随着显示比例尺变化而变化
        double mapscale;
        m_pTrans->GetMapScale(mapscale);
        double scalequot = refscale / mapscale;

        //将画笔尺寸换算为象素（pixel）单位
        pensize1 = (long)(m_Width * quotiety * scalequot);
    }
    else
    {
        //不使用参考比例尺，屏幕图元尺寸始终恒定，不随显示比例尺变化而变化

        //将画笔尺寸换算为象素（pixel）单位
        pensize1 = (long)(m_Width * quotiety);
    }

    pensize1 *= m_ParentSize;
    if (0 >= pensize1)
    {
        pensize1 = 1;
    }

    //保存当前的场景，设置新的场景
    HPEN pen = ::CreatePen(PS_SOLID, pensize1, m_Color);
    pensaved = (HPEN)::SelectObject(m_DC, pen);
    rop2saved = ::SetROP2(m_DC, m_ROP2);
}

void CSimpleLineSymbol::AfterDraw(const HPEN& pensaved,
    const long& rop2saved) const
{
    HPEN pen = (HPEN)::SelectObject(m_DC, pensaved);
    ::DeleteObject(pen);
    ::SetROP2(m_DC, rop2saved);
}

bool CSimpleLineSymbol::SetTemplate(const ILineSimpleTemplatePtr pTemplate)
{
    return this->SetTemplate(pTemplate._p());
}

bool CSimpleLineSymbol::GetTemplate(ILineSimpleTemplatePtr& pTemplate) const
{
    pTemplate.Clear();
    if (!m_pTemplate.Assigned())
    {
        return false;
    }

    pTemplate = m_pTemplate;
    return true;
}
//================================================================================


//================================================================================
//  CPointLineSymbol
//================================================================================
CPointLineSymbol::CPointLineSymbol(const long code)
{
    INIT_REFCOUNT

    m_Code = code;
    m_Name = "点线";
    m_ColorLock = false;
    m_Offset = 0;
    m_ParentOffset = 0;
    m_pPointSymbol = new CSimplePointSymbol;
    m_pTemplate = new CLineSimpleTemplate;
    m_pTemplate->AddSector(1);
    m_pTemplate->AddSector(1);
    m_ParentSize = 1;
}

bool __stdcall CPointLineSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "ILineSymbol"))
        || (0 == strcmp(interfacename, "IPointLineSymbol"))
        || (0 == strcmp(interfacename, "CPointLineSymbol")))
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

dword __stdcall CPointLineSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_Code);
    ps->Write(m_ROP2);
    ps->Write(m_Name);
    ps->WriteBool(m_ColorLock);
    ps->Write(m_Offset);

    m_pPointSymbol->_DumpTo(pStream, assist);

    bool usetemplate = m_pTemplate.Assigned();
    ps->WriteBool(usetemplate);
    if (usetemplate)
    {
        m_pTemplate->_DumpTo(pStream, assist);
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CPointLineSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_pTemplate.Clear();

    ps->Read(m_Code);
    ps->Read(m_ROP2);
    ps->Read(m_Name);
    ps->ReadBool(m_ColorLock);
    ps->Read(m_Offset);

    CPersistPtr pPersist;
    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pPointSymbol, IPointSymbol)

    bool usetemplate;
    ps->ReadBool(usetemplate);
    if (usetemplate)
    {
        CPersist::_InstantiateFrom(ps, pPersist, assist);
        CAST_PTR(pPersist, m_pTemplate, ILineSimpleTemplate)
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CPointLineSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CPointLineSymbol::Clone(IObjPtr& pObj) const
{
    CPointLineSymbolPtr pSymbol = new CPointLineSymbol;

    pSymbol->m_Code = m_Code;
    pSymbol->m_ROP2 = m_ROP2;
    pSymbol->m_Name = m_Name;
    pSymbol->m_ColorLock = m_ColorLock;
    pSymbol->m_Offset = m_Offset;

    IObjPtr pObjTmp;
    CLONE_PTR(m_pPointSymbol, pObjTmp)
    CAST_PTR(pObjTmp, pSymbol->m_pPointSymbol, IPointSymbol)

    if (m_pTemplate.Assigned())
    {
        CLONE_PTR(m_pTemplate, pObjTmp)
        CAST_PTR(pObjTmp, pSymbol->m_pTemplate, ILineSimpleTemplate)
    }

    CAST_PTR(pSymbol, pObj, IObj)
    return true;
}

bool __stdcall CPointLineSymbol::SetCode(const long& code)
{
    m_Code = code;
    return true;
}

long __stdcall CPointLineSymbol::GetCode() const
{
    return m_Code;
}

bool __stdcall CPointLineSymbol::SetName(const char* const name)
{
    m_Name = name;
    return true;
}

const char* __stdcall CPointLineSymbol::GetName() const
{
    return m_Name.c_str();
}

bool __stdcall CPointLineSymbol::GetDC(HDC& dc) const
{
    dc = m_DC;
    return true;
}

bool CPointLineSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    pTrans = m_pTrans;
    return true;
}

bool __stdcall CPointLineSymbol::GetROP2(long& rop2) const
{
    rop2 = m_ROP2;
    return true;
}

bool CPointLineSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    m_DC = dc;
    m_pTrans = pTrans;
    m_ROP2 = rop2;

    IDisplayTransformationPtr pDT;
    CAST_PTR(pTrans, pDT, IDisplayTransformation)
    m_pPointSymbol->Prepare(m_DC, pDT._p(), m_ROP2);

    return true;
}

bool __stdcall CPointLineSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    if (_invalid(ppTrans)) return false;
    assert(!*ppTrans);

    if (!m_pTrans.Assigned()) return false;
    *ppTrans = m_pTrans._p();
    if (_valid(*ppTrans)) (*ppTrans)->_AddRef();
    return true;
}

bool __stdcall CPointLineSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    if (_invalid(pTrans)) return false;
    m_DC = dc;
    m_pTrans = (CDisplayTransformation*)pTrans;
    m_ROP2 = rop2;

    m_pPointSymbol->Prepare(m_DC, pTrans, m_ROP2);
    return true;
}

bool __stdcall CPointLineSymbol::Draw(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->Draw(pg);
}

dword __stdcall CPointLineSymbol::DrawStream(const IStreamX* pStream) const
{
    if (_invalid(pStream)) return false;
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawStream(ps);
}

IPointSymbolPtr _p_point_Symbol_line_mark;
void _draw_point_line_mark(const WKSPoint& from, const WKSPoint& to,
    const dword pointcount)
{
    IDisplayTransformation* pT = NULL;
    _p_point_Symbol_line_mark->GetDisplayTransformation(&pT);
    CDisplayTransformationPtr pTrans = (CDisplayTransformation*)pT;
    pT->_Release();
    HDC dc;
    _p_point_Symbol_line_mark->GetDC(dc);

    WKSPoint from_r, to_r;
    pTrans->Device2Map(from, from_r);
    pTrans->Device2Map(to, to_r);
    double dist = sqrt(from_r.dis2(to_r));

//    double angle;
//    _p_point_Symbol_line_mark->GetAngle(angle);
//    angle += atan(LineSlope(from, to)) * 180 / PI;

//    IObjPtr pObj;
//    CLONE_PTR(_p_point_Symbol_line_mark, pObj)
    IPointSymbolPtr pPS;
//    CAST_PTR(pObj, pPS, IPointSymbol)
//    pPS->SetAngle(-angle);
    pPS = _p_point_Symbol_line_mark;
    pPS->Prepare(dc, pT);

    CPointPtr pPoint = new CPoint;
    IGeometryPtr pGeo;
    CAST_PTR(pPoint, pGeo, IGeometry)
    pPoint->SetX(from_r.x);
    pPoint->SetY(from_r.y);
    pPS->Draw((IGeometry*)(pGeo._p()));//起点

    if (0 < pointcount)
    {
        double step = dist / pointcount;
        for (dword i = 1; i < pointcount; i++)
        {
            double ratio = step*i / dist;
            WKSPoint tmp;
            tmp.x = ratio * (to_r.x - from_r.x) + from_r.x;
            tmp.y = ratio * (to_r.y - from_r.y) + from_r.y;
            pPoint->SetX(tmp.x);
            pPoint->SetY(tmp.y);
            pPS->Draw((IGeometry*)(pGeo._p()));
        }
    }
}

bool _draw_parallel_pointline(const IGeometryPtr pGeometry, const double& distance,
    const ILineSimpleTemplatePtr pTemplate, const IPointSymbolPtr pPointSymbol)
{
    bool r = false;
    CPathPtr pPath;
    dword i = 0;
    GeometryType gt = pGeometry->GetGeometryType();
    if (GEOMETRYTYPE_POLYLINE == gt)
    {
        CPolylinePtr pPL;
        CAST_PTR(pGeometry, pPL, CPolyline)
        dword pathcount = pPL->GetPathCount();
        for (i = 0; i < pathcount; i++)
        {
            pPL->GetPathRef(pPath, i);
            IGeometryPtr pGeoTmp;
            CAST_PTR(pPath, pGeoTmp, IGeometry)
            r = _draw_parallel_pointline(pGeoTmp, distance, pTemplate, pPointSymbol);
        }
    }
    else if (GEOMETRYTYPE_PATH == gt)
    {
        CAST_PTR(pGeometry, pPath, CPath)
        long pointcount = pPath->GetPointCount();
        if (2 > pointcount) return false;

        _p_point_Symbol_line_mark = pPointSymbol;

        long markindex = 0;
        long markprogress = 0;
        IDisplayTransformation* pT = NULL;
        pPointSymbol->GetDisplayTransformation(&pT);
        CDisplayTransformationPtr pTrans = (CDisplayTransformation*)pT;
        pT->_Release();
        HDC dc;
        pPointSymbol->GetDC(dc);

        //计算象素->毫米对应关系
        long logpixelx, logpixely;
        pTrans->GetLogPixel(logpixelx, logpixely);
        double quotiety_x = logpixelx
            / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
        double quotiety_y = logpixely
            / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
        double quotiety = (quotiety_x + quotiety_y) / 2;

        double quo, refscale;
        pTrans->GetReferenceScale(refscale);
        if (refscale > 0)
        {
            //使用参考比例尺，屏幕图元尺寸随着显示比例尺变化而变化
            double mapscale;
            pTrans->GetMapScale(mapscale);
            double scalequot = refscale / mapscale;
            quo = quotiety * scalequot;
        }
        else
        {
            quo = quotiety;
        }

        WKSPointZ point;
        WKSPointZ wndpnt;
        if (0.000001 < fabs(distance))
        {
            //绘制平行线
            double dist = distance*quo;

            geo_path path;
            for (i = 0; i < pointcount; i++)
            {
                pPath->GetPoint1(i, point);
                pTrans->Map2Device(point, wndpnt);
                path.push_back(wndpnt);
            }

            geo_path parallel;
            path.parallel(parallel, dist);

            for (i = 0; i < parallel.size() - 1; i++)
            {
                draw_template_line(dc, quo, pTemplate, markindex, markprogress,
                    parallel[i], parallel[i+1], _draw_point_line_mark);
            }
        }
        else
        {
            //绘制原始的path（非平行线）

            for (i = 0; i < pointcount - 1; i++)
            {
                pPath->GetPoint1(i, point);
                pTrans->Map2Device(point, wndpnt);
                WKSPoint from, to;
                from = wndpnt;

                pPath->GetPoint1(i+1, point);
                pTrans->Map2Device(point, wndpnt);
                to = wndpnt;
                draw_template_line(dc, quo, pTemplate, markindex, markprogress,
                    from, to, _draw_point_line_mark);
            }
        }

        _p_point_Symbol_line_mark.Clear();
    }

    return r;
}

bool CPointLineSymbol::Draw(const IGeometryPtr pGeometry) const
{
    if (!m_pTrans.Assigned() || !pGeometry.Assigned()
        || !m_pPointSymbol.Assigned())
        return false;

    IGeometryPtr pClippedGeometry = pGeometry;
    double attitude = 0.0;
    m_pTrans->GetAttitude(attitude);
    if ((attitude) > 0.0001)
    {
        pClippedGeometry = NULL;
        m_pTrans->ClipGeometry(pGeometry._p(), pClippedGeometry._ref());
    }

    if (!pClippedGeometry.Assigned())
    {
        return false;
    }

    double absoluteoffset = m_Offset + m_ParentOffset + m_Offset*(m_ParentSize-1);
    return _draw_parallel_pointline(pClippedGeometry, absoluteoffset, m_pTemplate, m_pPointSymbol);
}

dword CPointLineSymbol::DrawStream(const CStreamPtr pStream) const
{
    if (!m_pTrans.Assigned() || !pStream.Assigned()
        || !m_pPointSymbol.Assigned() || !m_pTemplate.Assigned())
        return 0;

    IGeometryPtr pGeometry;
    dword r = Stream2Geometry(pStream, pGeometry);
    if (!pGeometry.Assigned())
    {
        return r;
    }

    this->Draw(pGeometry);

    return r;
}

bool __stdcall CPointLineSymbol::SetColor(const COLORREF color)
{
    if (!m_pPointSymbol.Assigned()) return false;

    return m_pPointSymbol->SetColor(color);
}

bool __stdcall CPointLineSymbol::GetColor(COLORREF& color) const
{
    if (!m_pPointSymbol.Assigned()) return false;

    return m_pPointSymbol->GetColor(color);
}

void __stdcall CPointLineSymbol::SetColorLock(const bool lock)
{
    m_ColorLock = lock;
}

void __stdcall CPointLineSymbol::GetColorLock(bool& color) const
{
    color = m_ColorLock;
}

SymbolType __stdcall CPointLineSymbol::GetSymbolType() const
{
    return SYMBOLTYPE_LINE;
}

bool __stdcall CPointLineSymbol::SetPointSymbol(const IPointSymbol* const pPointSymbol)
{
    m_pPointSymbol = NULL;
    if (!pPointSymbol)
    {
        return true;
    }

    IPointSymbolPtr pPS = (IPointSymbol*)pPointSymbol;
    IObjPtr pObjTmp;
    CLONE_PTR(pPS, pObjTmp)
    CAST_PTR(pObjTmp, m_pPointSymbol, IPointSymbol)

    return true;
}

bool __stdcall CPointLineSymbol::GetPointSymbol(IPointSymbol** ppPointSymbol) const
{
    if (_invalid(ppPointSymbol)) return false;
    assert(!*ppPointSymbol);

    if (!m_pPointSymbol.Assigned())
    {
        *ppPointSymbol = NULL;
        return true;
    }

    IObjPtr pObjTmp;
    CLONE_PTR(m_pPointSymbol, pObjTmp)
    IPointSymbolPtr pPS;
    CAST_PTR(pObjTmp, pPS, IPointSymbol)

    *ppPointSymbol = pPS._p();
    (*ppPointSymbol)->_AddRef();

    return true;
}

bool CPointLineSymbol::SetPointSymbol(const IPointSymbolPtr pPointSymbol)
{
    m_pPointSymbol = NULL;
    if (!pPointSymbol.Assigned())
    {
        return true;
    }

    IObjPtr pObjTmp;
    CLONE_PTR(pPointSymbol, pObjTmp)
    CAST_PTR(pObjTmp, m_pPointSymbol, IPointSymbol)
    return true;
}

bool CPointLineSymbol::GetPointSymbol(IPointSymbolPtr& pPointSymbol) const
{
    pPointSymbol = NULL;
    if (!m_pPointSymbol.Assigned())
    {
        return true;
    }

    IObjPtr pObjTmp;
    CLONE_PTR(m_pPointSymbol, pObjTmp)
    CAST_PTR(pObjTmp, pPointSymbol, IPointSymbol)
    return true;
}

bool __stdcall CPointLineSymbol::SetTemplate(const ILineSimpleTemplate* const pTemplate)
{
    if (_invalid(pTemplate)) return false;
    m_pTemplate = (ILineSimpleTemplate*)pTemplate;
    return true;
}

bool __stdcall CPointLineSymbol::GetTemplate(ILineSimpleTemplate** ppTemplate) const
{
    if (_invalid(ppTemplate)) return false;
    assert(!*ppTemplate);

    *ppTemplate = m_pTemplate._p();
    if (_valid(*ppTemplate))
    {
        (*ppTemplate)->_AddRef();
        return true;
    }

    return false;
}

bool CPointLineSymbol::SetTemplate(const ILineSimpleTemplatePtr pTemplate)
{
    if (pTemplate.Assigned()) return false;
    return this->SetTemplate(pTemplate._p());
}

bool CPointLineSymbol::GetTemplate(ILineSimpleTemplatePtr& pTemplate) const
{
    pTemplate.Clear();
    if (!m_pTemplate.Assigned())
    {
        return false;
    }

    pTemplate = m_pTemplate;
    return true;
}

bool __stdcall CPointLineSymbol::SetOffset(const double offset)
{
    m_Offset = offset;
    return true;
}

bool __stdcall CPointLineSymbol::GetOffset(double& offset) const
{
    offset = m_Offset;
    return true;
}

bool __stdcall CPointLineSymbol::_ParentOffset(const double offset,
    const double size)
{
    m_ParentOffset = offset;
    m_ParentSize = size;
    WKSPoint pnt(0, 0);
    m_pPointSymbol->_ParentOffset(pnt, 0, m_ParentSize);

    return true;
}
//================================================================================


//================================================================================
//  CSimpleFillSymbol
//================================================================================

CSimpleFillSymbol::CSimpleFillSymbol(const long code)
{
    INIT_REFCOUNT

    m_Code = code;
    m_Name = "点线";
    m_FillColor = RGB(150, 180, 220);
    m_FillStyle = BS_SOLID;
    m_FillHatch = HS_DIAGCROSS;
    m_BorderColor = RGB(80, 50, 100);
    m_BorderWidth = 0.3;
    m_ColorLock = false;
}

CSimpleFillSymbol::~CSimpleFillSymbol()
{
}

bool __stdcall CSimpleFillSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "IFillSymbol"))
        || (0 == strcmp(interfacename, "ISimpleFillSymbol"))
        || (0 == strcmp(interfacename, "CSimpleFillSymbol")))
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

dword __stdcall CSimpleFillSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_Code);
    ps->Write(m_ROP2);
    ps->Write(m_Name);
    ps->Write(m_FillColor);
    ps->Write(m_FillStyle);
    ps->Write(m_FillHatch);
    ps->Write(m_BorderColor);
    ps->Write(m_BorderWidth);
    ps->WriteBool(m_ColorLock);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CSimpleFillSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Read(m_Code);
    ps->Read(m_ROP2);
    ps->Read(m_Name);
    ps->Read(m_FillColor);
    ps->Read(m_FillStyle);
    ps->Read(m_FillHatch);
    ps->Read(m_BorderColor);
    ps->Read(m_BorderWidth);
    ps->ReadBool(m_ColorLock);

    return pStream->GetPos() - oldpos;
}

bool __stdcall CSimpleFillSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CSimpleFillSymbol::Clone(IObjPtr& pObj) const
{
    CSimpleFillSymbolPtr pSymbol = new CSimpleFillSymbol;

    pSymbol->m_Code = m_Code;
    pSymbol->m_ROP2 = m_ROP2;
    pSymbol->m_Name = m_Name;
    pSymbol->m_FillColor = m_FillColor;
    pSymbol->m_FillStyle = m_FillStyle;
    pSymbol->m_FillHatch = m_FillHatch;
    pSymbol->m_BorderColor = m_BorderColor;
    pSymbol->m_BorderWidth = m_BorderWidth;
    pSymbol->m_ColorLock = m_ColorLock;

    CAST_PTR(pSymbol, pObj, IObj)
    return true;
}


bool __stdcall CSimpleFillSymbol::SetCode(const long& code)
{
    m_Code = code;
    return true;
}

long __stdcall CSimpleFillSymbol::GetCode() const
{
    return m_Code;
}

bool __stdcall CSimpleFillSymbol::SetName(const char* const name)
{
    m_Name = name;
    return true;
}

const char* __stdcall CSimpleFillSymbol::GetName() const
{
    return m_Name.c_str();
}

bool __stdcall CSimpleFillSymbol::GetDC(HDC& dc) const
{
    dc = m_DC;
    return true;
}

bool CSimpleFillSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    pTrans = m_pTrans;
    return true;
}

bool __stdcall CSimpleFillSymbol::GetROP2(long& rop2) const
{
    rop2 = m_ROP2;
    return true;
}

bool CSimpleFillSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    m_DC = dc;
    m_pTrans = pTrans;
    m_ROP2 = rop2;

    return true;
}

bool __stdcall CSimpleFillSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    if (_invalid(ppTrans)) return false;
    assert(!*ppTrans);

    if (!m_pTrans.Assigned()) return false;
    *ppTrans = m_pTrans._p();
    if (_valid(*ppTrans)) (*ppTrans)->_AddRef();
    return true;
}

bool __stdcall CSimpleFillSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    if (_invalid(pTrans)) return false;
    m_DC = dc;
    m_pTrans = (CDisplayTransformation*)pTrans;
    m_ROP2 = rop2;
    return true;
}

bool __stdcall CSimpleFillSymbol::Draw(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->Draw(pg);
}

dword __stdcall CSimpleFillSymbol::DrawStream(const IStreamX* pStream) const
{
    if (_invalid(pStream)) return false;
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawStream(ps);
}

bool CSimpleFillSymbol::Draw(const IGeometryPtr pGeometry) const
{
    if (!m_pTrans.Assigned() || !pGeometry.Assigned())
        return false;

    IGeometryPtr pClippedGeometry = pGeometry;
    double attitude = 0.0;
    m_pTrans->GetAttitude(attitude);
    if ((attitude) > 0.0001)
    {
        pClippedGeometry = NULL;
        m_pTrans->ClipGeometry(pGeometry._p(), pClippedGeometry._ref());
    }

    if (!pClippedGeometry.Assigned())
    {
        return false;
    }

    HPEN pensaved;
    HBRUSH brushsaved;
    long rop2saved;
    this->BeforeDraw(pensaved, brushsaved, rop2saved);
    bool r = DrawGeneralGeometry(pClippedGeometry, m_pTrans, m_DC);
    this->AfterDraw(pensaved, brushsaved, rop2saved);

    return r;
}

dword CSimpleFillSymbol::DrawStream(const CStreamPtr pStream) const
{
    if (!m_pTrans.Assigned() || !pStream.Assigned())
        return 0;

    dword r = 0;
    double attitude = 0.0;
    m_pTrans->GetAttitude(attitude);
    if ((attitude) > 0.0001)
    {
        //切
        IGeometryPtr pGeometry;
        r = Stream2Geometry(pStream, pGeometry);
        if (pGeometry.Assigned())
        {
            this->Draw(pGeometry);
        }
    }
    else
    {
        HPEN pensaved;
        HBRUSH brushsaved;
        long rop2saved;
        this->BeforeDraw(pensaved, brushsaved, rop2saved);
        r = DrawGeneralStream(pStream, m_pTrans, m_DC);
        this->AfterDraw(pensaved, brushsaved, rop2saved);
    }

    return r;
}

bool __stdcall CSimpleFillSymbol::SetColor(const COLORREF color)
{
    m_FillColor = color;
    return true;
}

bool __stdcall CSimpleFillSymbol::GetColor(COLORREF& color) const
{
    color = m_FillColor;
    return true;
}

void __stdcall CSimpleFillSymbol::SetColorLock(const bool lock)
{
    m_ColorLock = lock;
}

void __stdcall CSimpleFillSymbol::GetColorLock(bool& color) const
{
    color = m_ColorLock;
}

SymbolType __stdcall CSimpleFillSymbol::GetSymbolType() const
{
    return SYMBOLTYPE_FILL;
}

bool __stdcall CSimpleFillSymbol::SetFillStyle(const long style)
{
    m_FillStyle = style;
    return true;
}

bool __stdcall CSimpleFillSymbol::GetFillStyle(long& style) const
{
    style = m_FillStyle;
    return true;
}

bool __stdcall CSimpleFillSymbol::SetFillHatch(const long hatch)
{
    m_FillHatch = hatch;
    return true;
}

bool __stdcall CSimpleFillSymbol::GetFillHatch(long& hatch) const
{
    hatch = m_FillHatch;
    return true;
}

bool __stdcall CSimpleFillSymbol::SetBorderColor(const COLORREF color)
{
    m_BorderColor = color;
    return true;
}

bool __stdcall CSimpleFillSymbol::GetBorderColor(COLORREF& color) const
{
    color = m_BorderColor;
    return true;
}

bool __stdcall CSimpleFillSymbol::SetBorderWidth(const double width)
{
    m_BorderWidth = width;
    return true;
}

bool __stdcall CSimpleFillSymbol::GetBorderWidth(double& width) const
{
    width = m_BorderWidth;
    return true;
}

void CSimpleFillSymbol::BeforeDraw(HPEN& pensaved, HBRUSH& brushsaved,
    long& rop2saved) const
{
    //计算象素->毫米对应关系
    long logpixelx, logpixely;
    m_pTrans->GetLogPixel(logpixelx, logpixely);
    double quotiety_x = logpixelx
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double quotiety_y = logpixely
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double quotiety = (quotiety_x + quotiety_y) / 2;

    double refscale;
    m_pTrans->GetReferenceScale(refscale);

    long pensize1;
    if (refscale > 0)
    {
        //使用参考比例尺，屏幕图元尺寸随着显示比例尺变化而变化
        double mapscale;
        m_pTrans->GetMapScale(mapscale);
        double scalequot = refscale / mapscale;

        //将画笔尺寸换算为象素（pixel）单位
        pensize1 = (long)(m_BorderWidth * quotiety * scalequot);
    }
    else
    {
        //不使用参考比例尺，屏幕图元尺寸始终恒定，不随显示比例尺变化而变化

        //将画笔尺寸换算为象素（pixel）单位
        pensize1 = (long)(m_BorderWidth * quotiety);
    }

    //保存当前的场景，设置新的场景
    HPEN pen;
    if (m_BorderWidth > 0.0001)
    {
        pen = ::CreatePen(PS_SOLID, pensize1, m_BorderColor);
    }
    else
    {
        pen = ::CreatePen(PS_NULL, pensize1, m_BorderColor);
    }
    pensaved = (HPEN)::SelectObject(m_DC, pen);
    LOGBRUSH logbrush;
    logbrush.lbColor = m_FillColor;
    logbrush.lbStyle = m_FillStyle;
    logbrush.lbHatch = m_FillHatch;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    brushsaved = (HBRUSH)::SelectObject(m_DC, brush);
    rop2saved = ::SetROP2(m_DC, m_ROP2);
}

void CSimpleFillSymbol::AfterDraw(const HPEN& pensaved, const HBRUSH& brushsaved,
    const long& rop2saved) const
{
    HPEN pen = (HPEN)::SelectObject(m_DC, pensaved);
    ::DeleteObject(pen);
    HBRUSH brush = (HBRUSH)::SelectObject(m_DC, brushsaved);
    ::DeleteObject(brush);
    ::SetROP2(m_DC, rop2saved);
}
//================================================================================


//================================================================================
//  CPointFillSymbol
//================================================================================

CPointFillSymbol::CPointFillSymbol(const long code)
{
    INIT_REFCOUNT

    m_Code = code;
    m_Name = "点面";

    ISimplePointSymbolPtr pSimplePointSymbol = new CSimplePointSymbol;
    pSimplePointSymbol->SetDiameter(1.1);
    pSimplePointSymbol->SetColor(RGB(35, 230, 50));
    m_pPointSymbol = pSimplePointSymbol._p();

    ISimpleLineSymbolPtr pSimpleLineSymbol = new CSimpleLineSymbol;
    pSimpleLineSymbol->SetColor(RGB(15, 55, 35));
    m_pBorderSymbol = pSimpleLineSymbol._p();

    m_PointsSpaceX = 2.8;
    m_PointsSpaceY = 3.8;
    m_FillStyle = POINTFILLSTYLE_REGULAR;
    m_OffsetX = 0;
    m_OffsetY = 0;
    m_ColorLock = false;
}

CPointFillSymbol::~CPointFillSymbol()
{
}

bool __stdcall CPointFillSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "IFillSymbol"))
        || (0 == strcmp(interfacename, "IPointFillSymbol"))
        || (0 == strcmp(interfacename, "CPointFillSymbol")))
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

dword __stdcall CPointFillSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_Code);
    ps->Write(m_ROP2);
    ps->Write(m_Name);

    m_pPointSymbol->_DumpTo(pStream, assist);
    m_pBorderSymbol->_DumpTo(pStream, assist);
    ps->Write(m_PointsSpaceX);
    ps->Write(m_PointsSpaceY);
    ps->Write(m_OffsetX);
    ps->Write(m_OffsetY);
    ps->Write(m_FillStyle);
    ps->WriteBool(m_ColorLock);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CPointFillSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Read(m_Code);
    ps->Read(m_ROP2);
    ps->Read(m_Name);

    CPersistPtr pPersist;
    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pPointSymbol, IPointSymbol)

    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pBorderSymbol, ILineSymbol)

    ps->Read(m_PointsSpaceX);
    ps->Read(m_PointsSpaceY);
    ps->Read(m_OffsetX);
    ps->Read(m_OffsetY);
    ps->Read(m_FillStyle);
    ps->ReadBool(m_ColorLock);

    return pStream->GetPos() - oldpos;
}

bool __stdcall CPointFillSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CPointFillSymbol::Clone(IObjPtr& pObj) const
{
    CPointFillSymbolPtr pSymbol = new CPointFillSymbol;

    pSymbol->m_Code = m_Code;
    pSymbol->m_ROP2 = m_ROP2;
    pSymbol->m_Name = m_Name;
    pSymbol->SetPointSymbol(NULL);
    pSymbol->SetBorderSymbol(NULL);

    IObjPtr pObjTmp;
    if (m_pPointSymbol.Assigned())
    {
        CLONE_PTR(m_pPointSymbol, pObjTmp)
        CAST_PTR(pObjTmp, pSymbol->m_pPointSymbol, IPointSymbol)
    }

    if (m_pBorderSymbol.Assigned())
    {
        CLONE_PTR(m_pBorderSymbol, pObjTmp)
        CAST_PTR(pObjTmp, pSymbol->m_pBorderSymbol, ILineSymbol)
    }

    pSymbol->m_PointsSpaceX = m_PointsSpaceX;
    pSymbol->m_PointsSpaceY = m_PointsSpaceY;
    pSymbol->m_OffsetX = m_OffsetX;
    pSymbol->m_OffsetY = m_OffsetY;
    pSymbol->m_FillStyle = m_FillStyle;
    pSymbol->m_ColorLock = m_ColorLock;

    CAST_PTR(pSymbol, pObj, IObj)
    return true;
}


bool __stdcall CPointFillSymbol::SetCode(const long& code)
{
    m_Code = code;
    return true;
}

long __stdcall CPointFillSymbol::GetCode() const
{
    return m_Code;
}

bool __stdcall CPointFillSymbol::SetName(const char* const name)
{
    m_Name = name;
    return true;
}

const char* __stdcall CPointFillSymbol::GetName() const
{
    return m_Name.c_str();
}

bool __stdcall CPointFillSymbol::GetDC(HDC& dc) const
{
    dc = m_DC;
    return true;
}

bool CPointFillSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    pTrans = m_pTrans;
    return true;
}

bool __stdcall CPointFillSymbol::GetROP2(long& rop2) const
{
    rop2 = m_ROP2;
    return true;
}

bool CPointFillSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    m_DC = dc;
    m_pTrans = pTrans;
    m_ROP2 = rop2;

    IDisplayTransformationPtr pDT;
    CAST_PTR(pTrans, pDT, IDisplayTransformation)

    if (m_pPointSymbol.Assigned())
    {
        m_pPointSymbol->Prepare(m_DC, pDT._p(), m_ROP2);
    }

    if (m_pBorderSymbol.Assigned())
    {
        m_pBorderSymbol->Prepare(m_DC, pDT._p(), m_ROP2);
    }

    return true;
}

bool __stdcall CPointFillSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    if (_invalid(ppTrans)) return false;
    assert(!*ppTrans);

    if (!m_pTrans.Assigned()) return false;
    *ppTrans = m_pTrans._p();
    if (_valid(*ppTrans)) (*ppTrans)->_AddRef();
    return true;
}

bool __stdcall CPointFillSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    if (_invalid(pTrans)) return false;
    m_DC = dc;
    m_pTrans = (CDisplayTransformation*)pTrans;
    m_ROP2 = rop2;

    if (m_pPointSymbol.Assigned())
    {
        m_pPointSymbol->Prepare(m_DC, pTrans, m_ROP2);
    }

    if (m_pBorderSymbol.Assigned())
    {
        m_pBorderSymbol->Prepare(m_DC, pTrans, m_ROP2);
    }

    return true;
}

bool __stdcall CPointFillSymbol::Draw(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->Draw(pg);
}

dword __stdcall CPointFillSymbol::DrawStream(const IStreamX* pStream) const
{
    if (_invalid(pStream)) return false;
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawStream(ps);
}

HRGN CreateRingRegion(CRingPtr pRing, CDisplayTransformationPtr pTrans)
{
    long pointcount = pRing->GetPointCount();
    if (3 > pointcount) return NULL;

    WKSRect visibleextent;
    pTrans->GetVisibleExtent(visibleextent);

    static POINT staticpoints[2000];
    POINT* pPoints = staticpoints;
    if (2000 < pointcount)
    {
        pPoints = new POINT[pointcount];
    }

    for (long i = 0; i < pointcount; i++)
    {
        WKSPointZ point;
        pRing->GetPoint1(i, point);
        point.x = (point.x > visibleextent.left) ? point.x : visibleextent.left;
        point.x = (point.x < visibleextent.right) ? point.x : visibleextent.right;
        point.y = (point.y < visibleextent.top) ? point.y : visibleextent.top;
        point.y = (point.y > visibleextent.bottom) ? point.y : visibleextent.bottom;

        pTrans->Map2DeviceXY(point.x, point.y, pPoints[i].x, pPoints[i].y);
    }

    HRGN rgn = ::CreatePolygonRgn(pPoints, pointcount, ALTERNATE);

    if (2000 < pointcount)
    {
        delete[] pPoints;
    }

    return rgn;
}

HRGN CreatePolygonRegion(CPolygonPtr pPolygon, CDisplayTransformationPtr pTrans)
{
    long ringcount = pPolygon->GetRingCount();
    int* pPolyCounts = new int[ringcount];
    if (!pPolyCounts)
    {
        return NULL;
    }

    WKSRect visibleextent;
    pTrans->GetVisibleExtent(visibleextent);

    //确定所有点的数量
    dword pntcount = 0;
    dword i = 0;
    for (; i < ringcount; i++)
    {
        CRingPtr pRing;
        pPolygon->GetRingRef(pRing, i);
        dword tmpcount = pRing->GetPointCount();
        pntcount += tmpcount;
        pPolyCounts[i] = tmpcount;//记录每一个Ring的点数量
    }
    //申请足够大的POINT数组
    POINT* pPnts = new POINT[pntcount];
    if (!pPnts)
    {
        delete[] pPolyCounts;
        return NULL;
    }

    //给点数组赋值
    dword pntindex = 0;
    for (i = 0; i < ringcount; i++)
    {
        CRingPtr pRing;
        pPolygon->GetRingRef(pRing, i);
        dword tmpcount = pRing->GetPointCount();
        for (dword j = 0; j < tmpcount; j++)
        {
            WKSPointZ wkspnt;
            pRing->GetPoint1(j, wkspnt);
            wkspnt.x = (wkspnt.x > visibleextent.left) ? wkspnt.x : visibleextent.left;
            wkspnt.x = (wkspnt.x < visibleextent.right) ? wkspnt.x : visibleextent.right;
            wkspnt.y = (wkspnt.y < visibleextent.top) ? wkspnt.y : visibleextent.top;
            wkspnt.y = (wkspnt.y > visibleextent.bottom) ? wkspnt.y : visibleextent.bottom;

            pTrans->Map2DeviceXY(wkspnt.x, wkspnt.y, pPnts[pntindex].x, pPnts[pntindex].y);
            pntindex++;
        }
    }

    HRGN rgn = ::CreatePolyPolygonRgn(pPnts, pPolyCounts, ringcount, ALTERNATE);
    delete[] pPnts;
    delete[] pPolyCounts;

    return rgn;
}

void CPointFillSymbol::DrawRegularPoints(const CPolygonPtr pPolygon,
    const CRingPtr pRing) const
{
    //先搞定region---------------------------------------------
    HRGN rgn = NULL;
    IGeometryPtr pGeometry;

    if (pRing.Assigned())
    {
        rgn = CreateRingRegion(pRing, m_pTrans);
        CAST_PTR(pRing, pGeometry, IGeometry)
    }
    else if (pPolygon.Assigned())
    {
        rgn = CreatePolygonRegion(pPolygon, m_pTrans);
        CAST_PTR(pPolygon, pGeometry, IGeometry)
    }
    else
    {
        return;
    }

    if (!rgn)
    {
        return;
    }
    //region搞定了---------------------------------------------


    //计算点的实地间距-----------------------------------------
    WKSRect mbr;
    pGeometry->GetMBR(mbr);
    WKSRect visibleextent;
    m_pTrans->GetVisibleExtent(visibleextent);
    mbr.left = (mbr.left > visibleextent.left) ? mbr.left : visibleextent.left;
    mbr.right = (mbr.right < visibleextent.right) ? mbr.right : visibleextent.right;
    mbr.bottom = (mbr.bottom > visibleextent.bottom) ? mbr.bottom : visibleextent.bottom;
    mbr.top = (mbr.top < visibleextent.top) ? mbr.top : visibleextent.top;

    long logpixelx, logpixely;
    m_pTrans->GetLogPixel(logpixelx, logpixely);
    double quotiety = logpixelx
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);

    double quo, refscale;
    m_pTrans->GetReferenceScale(refscale);
    if (refscale > 0)
    {
        double mapscale;
        m_pTrans->GetMapScale(mapscale);
        double scalequot = refscale / mapscale;
        quo = quotiety * scalequot;
    }
    else
    {
        quo = quotiety;
    }

    long distx_wnd = m_PointsSpaceX*quo;
    long disty_wnd = m_PointsSpaceY*quo;
    long offsetx_wnd = m_OffsetX*quo;
    long offsety_wnd = m_OffsetY*quo;
    double zero_x, zero_y, dist_x, dist_y;
    double offsetx = 0;
    double offsety = 0;

    m_pTrans->Device2MapXY(0, 0, zero_x, zero_y);
    m_pTrans->Device2MapXY(distx_wnd, disty_wnd, dist_x, dist_y);
    dist_x = dist_x - zero_x;
    dist_y = -dist_y + zero_y;

    if ((offsetx_wnd != 0) || (offsety_wnd != 0))
    {
        m_pTrans->Device2MapXY(offsetx_wnd, offsety_wnd, offsetx, offsety);
        offsetx = offsetx - zero_x;
        offsety = offsety - zero_y;
    }
    //计算点的实地间距-----------------------------------------

    double left = (mbr.right > mbr.left) ? mbr.left : mbr.right;
    double bottom = (mbr.top > mbr.bottom) ? mbr.bottom : mbr.top;
    double mbr_width = fabs(mbr.right - mbr.left);
    double mbr_height = fabs(mbr.top - mbr.bottom);
    long points_x = mbr_width / dist_x;
    long points_y = mbr_height / dist_y;

    for (long i = -1; i <= points_x; i++)
    {
        for (long j = -1; j <= points_y; j++)
        {
            WKSPoint wkspnt;
            wkspnt.x = left + dist_x*i + offsetx;
            wkspnt.y = bottom + dist_y*j + offsety;

            tagPOINT wndpnt, wndpnt1, wndpnt2, wndpnt3;
            m_pTrans->Map2Device(wkspnt, wndpnt);
            wndpnt1.x = wndpnt.x - 2;
            wndpnt1.y = wndpnt.y - 2;
            wndpnt2.x = wndpnt.x + 2;
            wndpnt2.y = wndpnt.y - 2;
            wndpnt3.x = wndpnt.x;
            wndpnt3.y = wndpnt.y + 3;

            if (PtInRegion(rgn, wndpnt1.x, wndpnt1.y)
                && PtInRegion(rgn, wndpnt2.x, wndpnt2.y)
                && PtInRegion(rgn, wndpnt3.x, wndpnt3.y))
            {
                IGeometryPtr pPointGeometry = new CPoint(wkspnt);
                m_pPointSymbol->Draw(pPointGeometry._p());
            }
        }
    }
    ::DeleteObject(rgn);

    return;
}

void CPointFillSymbol::DrawSingleLabelPoint(const CRingPtr pRing) const
{
    IPointPtr pLabelPoint = new CPoint;
    CGeometryLabelPtr pGeometryLabel = new CGeometryLabel();
    pGeometryLabel->SetGeometry(pRing._p());
    vector<WKSPoint> labelpoints;
    pGeometryLabel->GetLabelPoints(labelpoints);
    vector<WKSPoint>::const_iterator it_labelpoints = labelpoints.begin();
    while (it_labelpoints != labelpoints.end())
    {
        pLabelPoint->SetX((*it_labelpoints).x);
        pLabelPoint->SetY((*it_labelpoints).y);
        m_pPointSymbol->Draw(pLabelPoint._p());
        it_labelpoints++;
    }
}

void CPointFillSymbol::DrawLabelPoint(const CPolygonPtr pPolygon, const CRingPtr pRing) const
{
    if (pRing.Assigned())
    {
        this->DrawSingleLabelPoint(pRing);
    }
    else if (pPolygon.Assigned())
    {
        dword ringcount = pPolygon->GetRingCount();
        for (dword i = 0; i < ringcount; i++)
        {
            CRingPtr pRingTmp;
            pPolygon->GetRingRef(pRingTmp, i);
            this->DrawSingleLabelPoint(pRingTmp._p());
        }
    }
    else
    {
        return;
    }
}

void CPointFillSymbol::GetPolygon(const IGeometryPtr pGeometry,
    CPolygonPtr& pPolygon, CRingPtr& pRing) const
{
    CAST_PTR(pGeometry, pRing, CRing)
    if (!pRing.Assigned())
    {
        CAST_PTR(pGeometry, pPolygon, CPolygon)
        if (pPolygon.Assigned())
        {
            if (pPolygon->GetRingCount() == 1)
            {
                pPolygon->GetRingRef(pRing, 0);
            }
            else if (pPolygon->GetRingCount() > 1)
            {
                pRing = NULL;
            }
            else
            {
                pPolygon = NULL;
                pRing = NULL;
            }
        }
    }
}

bool CPointFillSymbol::Draw(const IGeometryPtr pGeometry) const
{
    if (!m_pTrans.Assigned() || !pGeometry.Assigned())
        return false;

    IGeometryPtr pClippedGeometry = pGeometry;
    double attitude = 0.0;
    m_pTrans->GetAttitude(attitude);
    if ((attitude) > 0.0001)
    {
        pClippedGeometry = NULL;
        m_pTrans->ClipGeometry(pGeometry._p(), pClippedGeometry._ref());
    }

    if (!pClippedGeometry.Assigned())
    {
        return false;
    }

    CPolygonPtr pPolygon;
    CRingPtr pRing;
    this->GetPolygon(pClippedGeometry, pPolygon, pRing);

    if (m_pPointSymbol.Assigned())
    {
        double planerotate, attitude;
        m_pTrans->GetAttitude(attitude);
        m_pTrans->GetPlaneRotate(planerotate);
        if ((attitude < 0.01) && (long(planerotate*10) % 3600 < 0.01)
            &&(m_FillStyle == POINTFILLSTYLE_REGULAR))
        {
            this->DrawRegularPoints(pPolygon, pRing);
        }
        else
        {
            this->DrawLabelPoint(pPolygon, pRing);
        }
    }

    if (m_pBorderSymbol.Assigned())
    {
        IGeometryPtr pGeoLine;
        if (pRing.Assigned())
        {
            IPathPtr pPath = Ring2Path((IRing*)pRing._p());
            CAST_PTR(pPath, pGeoLine, IGeometry)
        }
        else if (pPolygon.Assigned())
        {
            IPolylinePtr pPolyline = Polygon2Polyline((IPolygon*)pPolygon._p());
            CAST_PTR(pPolyline, pGeoLine, IGeometry)
        }

        if (pGeoLine.Assigned())
        {
            m_pBorderSymbol->Draw(pGeoLine._p());
        }
    }

    return true;
}

dword CPointFillSymbol::DrawStream(const CStreamPtr pStream) const
{
    if (!m_pTrans.Assigned() || !pStream.Assigned())
        return 0;

    IGeometryPtr pGeometry;
    dword r = Stream2Geometry(pStream, pGeometry);
    if (pGeometry.Assigned())
    {
        this->Draw(pGeometry);
    }

    return r;
}

bool __stdcall CPointFillSymbol::SetColor(const COLORREF color)
{
    if (m_pPointSymbol.Assigned())
    {
        m_pPointSymbol->SetColor(color);
    }

    if (m_pBorderSymbol.Assigned())
    {
        m_pBorderSymbol->SetColor(color);
    }

    return true;
}

bool __stdcall CPointFillSymbol::GetColor(COLORREF& color) const
{
    if (m_pPointSymbol.Assigned())
    {
        return m_pPointSymbol->GetColor(color);
    }
    else if (m_pBorderSymbol.Assigned())
    {
        return m_pBorderSymbol->GetColor(color);
    }

    return false;
}

void __stdcall CPointFillSymbol::SetColorLock(const bool lock)
{
    m_ColorLock = lock;
}

void __stdcall CPointFillSymbol::GetColorLock(bool& color) const
{
    color = m_ColorLock;
}

SymbolType __stdcall CPointFillSymbol::GetSymbolType() const
{
    return SYMBOLTYPE_FILL;
}

bool __stdcall CPointFillSymbol::SetPointSymbol(const IPointSymbol* const pPointSymbol)
{
    m_pPointSymbol = NULL;
    if (!pPointSymbol)
    {
        return true;
    }

    IPointSymbolPtr pPS = (IPointSymbol*)pPointSymbol;
    IObjPtr pObjTmp;
    CLONE_PTR(pPS, pObjTmp)
    CAST_PTR(pObjTmp, m_pPointSymbol, IPointSymbol)

    return true;
}

bool __stdcall CPointFillSymbol::GetPointSymbol(IPointSymbol** ppPointSymbol) const
{
    if (_invalid(ppPointSymbol)) return false;
    assert(!*ppPointSymbol);

    if (!m_pPointSymbol.Assigned())
    {
        *ppPointSymbol = NULL;
        return true;
    }

    IObjPtr pObjTmp;
    CLONE_PTR(m_pPointSymbol, pObjTmp)
    IPointSymbolPtr pPS;
    CAST_PTR(pObjTmp, pPS, IPointSymbol)

    *ppPointSymbol = pPS._p();
    (*ppPointSymbol)->_AddRef();

    return true;
}

bool CPointFillSymbol::SetPointSymbol(const IPointSymbolPtr pPointSymbol)
{
    m_pPointSymbol = NULL;
    if (!pPointSymbol.Assigned())
    {
        return true;
    }

    IObjPtr pObjTmp;
    CLONE_PTR(pPointSymbol, pObjTmp)
    CAST_PTR(pObjTmp, m_pPointSymbol, IPointSymbol)
    return true;
}

bool CPointFillSymbol::GetPointSymbol(IPointSymbolPtr& pPointSymbol) const
{
    pPointSymbol = NULL;
    if (!m_pPointSymbol.Assigned())
    {
        return true;
    }

    IObjPtr pObjTmp;
    CLONE_PTR(m_pPointSymbol, pObjTmp)
    CAST_PTR(pObjTmp, pPointSymbol, IPointSymbol)
    return true;
}

bool __stdcall CPointFillSymbol::SetPointsSpace(const double space_x, const double space_y)
{
    m_PointsSpaceX = space_x;
    m_PointsSpaceY = space_y;
    return true;
}

bool __stdcall CPointFillSymbol::GetPointsSpace(double& space_x, double& space_y) const
{
    space_x = m_PointsSpaceX;
    space_y = m_PointsSpaceY;
    return true;
}

bool __stdcall CPointFillSymbol::SetPointsOffset(const double offset_x, const double offset_y)
{
    m_OffsetX = offset_x;
    m_OffsetY = offset_y;
    return true;
}

bool __stdcall CPointFillSymbol::GetPointsOffset(double& offset_x, double& offset_y) const
{
    offset_x = m_OffsetX;
    offset_y = m_OffsetY;
    return true;
}

bool __stdcall CPointFillSymbol::SetFillStyle(const PointFillStyle fillstyle)
{
    m_FillStyle = fillstyle;
    return true;
}

bool __stdcall CPointFillSymbol::GetFillStyle(PointFillStyle& fillstyle) const
{
    fillstyle = m_FillStyle;
    return true;
}

bool __stdcall CPointFillSymbol::SetBorderSymbol(const ILineSymbol* const pLineSymbol)
{
    m_pBorderSymbol = NULL;
    if (!pLineSymbol)
    {
        return true;
    }

    ILineSymbolPtr pPS = (ILineSymbol*)pLineSymbol;
    IObjPtr pObjTmp;
    CLONE_PTR(pPS, pObjTmp)
    CAST_PTR(pObjTmp, m_pBorderSymbol, ILineSymbol)

    return true;
}

bool __stdcall CPointFillSymbol::GetBorderSymbol(ILineSymbol** ppLineSymbol) const
{
    if (_invalid(ppLineSymbol)) return false;
    assert(!*ppLineSymbol);

    if (!m_pBorderSymbol.Assigned())
    {
        *ppLineSymbol = NULL;
        return true;
    }

    IObjPtr pObjTmp;
    CLONE_PTR(m_pBorderSymbol, pObjTmp)
    ILineSymbolPtr pLS;
    CAST_PTR(pObjTmp, pLS, ILineSymbol)

    *ppLineSymbol = pLS._p();
    (*ppLineSymbol)->_AddRef();

    return true;
}

bool CPointFillSymbol::SetBorderSymbol(const ILineSymbolPtr pLineSymbol)
{
    return this->SetBorderSymbol(pLineSymbol);
}

bool CPointFillSymbol::GetBorderSymbol(ILineSymbolPtr& pLineSymbol) const
{
    pLineSymbol = NULL;
    if (!m_pBorderSymbol.Assigned())
    {
        return true;
    }

    IObjPtr pObjTmp;
    CLONE_PTR(m_pBorderSymbol, pObjTmp)
    CAST_PTR(pObjTmp, pLineSymbol, ILineSymbol)
    return true;
}

void CPointFillSymbol::BeforeDraw(HPEN& pensaved, HBRUSH& brushsaved,
    long& rop2saved) const
{
}

void CPointFillSymbol::AfterDraw(const HPEN& pensaved, const HBRUSH& brushsaved,
    const long& rop2saved) const
{
}
//================================================================================


//================================================================================
//  CSimpleTextSymbol
//================================================================================

CSimpleTextSymbol::CSimpleTextSymbol(const long code)
{
    INIT_REFCOUNT

    m_Code = code;
    m_Name = "你是猪";
    m_Color = RGB(35, 60, 30);
    m_ColorLock = false;
    ::memset(&m_Font, 0, sizeof(LOGFONT));
    ::strcpy(m_Font.lfFaceName, "微软雅黑");
    m_Width = 5;
    m_Height = 5;
    m_Angle = 0;
}

bool __stdcall CSimpleTextSymbol::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "ISymbol"))
        || (0 == strcmp(interfacename, "ITextSymbol"))
        || (0 == strcmp(interfacename, "ISimpleTextSymbol"))
        || (0 == strcmp(interfacename, "CSimpleTextSymbol")))
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

dword __stdcall CSimpleTextSymbol::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_Code);
    ps->Write(m_ROP2);
    ps->Write(m_Name);
    ps->Write(m_Text);
    ps->Write(m_Color);
    ps->WriteBool(m_ColorLock);
    ps->Write(&m_Font, sizeof(LOGFONTA));
    ps->Write(m_Width);
    ps->Write(m_Height);
    ps->Write(m_Angle);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CSimpleTextSymbol::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Read(m_Code);
    ps->Read(m_ROP2);
    ps->Read(m_Name);
    ps->Read(m_Text);
    ps->Read(m_Color);
    ps->ReadBool(m_ColorLock);
    ps->Read(&m_Font, sizeof(LOGFONTA));
    ps->Read(m_Width);
    ps->Read(m_Height);
    ps->Read(m_Angle);

    return pStream->GetPos() - oldpos;
}

bool __stdcall CSimpleTextSymbol::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CSimpleTextSymbol::Clone(IObjPtr& pObj) const
{
    CSimpleTextSymbolPtr pSymbol = new CSimpleTextSymbol;

    pSymbol->m_Code = m_Code;
    pSymbol->m_ROP2 = m_ROP2;
    pSymbol->m_Name = m_Name;
    pSymbol->m_Text = m_Text;
    pSymbol->m_Color = m_Color;
    pSymbol->m_ColorLock = m_ColorLock;
    pSymbol->m_Font = m_Font;
    pSymbol->m_Width = m_Width;
    pSymbol->m_Height = m_Height;
    pSymbol->m_Angle = m_Angle;

    CAST_PTR(pSymbol, pObj, IObj)
    return true;
}

bool __stdcall CSimpleTextSymbol::SetCode(const long& code)
{
    m_Code = code;
    return true;
}

long __stdcall CSimpleTextSymbol::GetCode() const
{
    return m_Code;
}

bool __stdcall CSimpleTextSymbol::SetName(const char* const name)
{
    m_Name = name;
    return true;
}

const char* __stdcall CSimpleTextSymbol::GetName() const
{
    return m_Name.c_str();
}

bool __stdcall CSimpleTextSymbol::GetDC(HDC& dc) const
{
    dc = m_DC;
    return true;
}

bool CSimpleTextSymbol::GetDisplayTransformation(CDisplayTransformationPtr& pTrans) const
{
    pTrans = m_pTrans;
    return true;
}

bool __stdcall CSimpleTextSymbol::GetROP2(long& rop2) const
{
    rop2 = m_ROP2;
    return true;
}

bool CSimpleTextSymbol::Prepare(const HDC dc, const CDisplayTransformationPtr pTrans,
    const long rop2)
{
    m_DC = dc;
    m_pTrans = pTrans;
    m_ROP2 = rop2;

    return true;
}

bool __stdcall CSimpleTextSymbol::GetDisplayTransformation(IDisplayTransformation** ppTrans) const
{
    if (_invalid(ppTrans)) return false;
    assert(!*ppTrans);

    if (!m_pTrans.Assigned()) return false;
    *ppTrans = m_pTrans._p();
    if (_valid(*ppTrans)) (*ppTrans)->_AddRef();
    return true;
}

bool __stdcall CSimpleTextSymbol::Prepare(const HDC dc, const IDisplayTransformation* pTrans,
    const long rop2)
{
    if (_invalid(pTrans)) return false;
    m_DC = dc;
    m_pTrans = (CDisplayTransformation*)pTrans;
    m_ROP2 = rop2;
    return true;
}

bool __stdcall CSimpleTextSymbol::Draw(const IGeometry* pGeometry) const
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->Draw(pg);
}

dword __stdcall CSimpleTextSymbol::DrawStream(const IStreamX* pStream) const
{
    if (_invalid(pStream)) return false;
    CStreamPtr ps = (CStream*)pStream;
    return this->DrawStream(ps);
}

bool CSimpleTextSymbol::Draw(const IGeometryPtr pGeometry) const
{
    if (!m_pTrans.Assigned() || !pGeometry.Assigned()) {return false;}

    RECT textenvelope;
    return this->Draw(pGeometry, textenvelope);
}

bool __stdcall CSimpleTextSymbol::Draw(const IGeometry* pGeometry, RECT& textenvelope) const
{
    if (_invalid(pGeometry)) return false;
    IGeometryPtr pg = (IGeometry*)pGeometry;
    return this->Draw(pg, textenvelope);
}

bool CSimpleTextSymbol::Draw(const IGeometryPtr pGeometry, RECT& textenvelope) const
{
    if (!m_pTrans.Assigned() || !pGeometry.Assigned()) {return false;}

    HFONT fontsaved;
    long bkmodesaved;
    long rop2saved;
    COLORREF textcolorsaved;
    this->BeforeDraw(fontsaved, bkmodesaved, textcolorsaved, rop2saved);
    bool r = DrawLabel(pGeometry, m_Text.c_str(), m_pTrans, m_DC, textenvelope);
    this->AfterDraw(fontsaved, bkmodesaved, textcolorsaved, rop2saved);

    return r;
}

dword CSimpleTextSymbol::DrawStream(const CStreamPtr pStream) const
{
    if (!m_pTrans.Assigned() || !pStream.Assigned()) {return 0;}
    dword oldpos = pStream->GetPos();

    IGeometryPtr pGeometry;
    Stream2Geometry(pStream, pGeometry);
    string text;
    pStream->Read(text);

    HFONT fontsaved;
    long bkmodesaved;
    long rop2saved;
    COLORREF textcolorsaved;
    this->BeforeDraw(fontsaved, bkmodesaved, textcolorsaved, rop2saved);
    RECT textenvelope;
    DrawLabel(pGeometry, m_Text.c_str(), m_pTrans, m_DC, textenvelope);
    this->AfterDraw(fontsaved, bkmodesaved, textcolorsaved, rop2saved);

    return pStream->GetPos() - oldpos;
}

bool __stdcall CSimpleTextSymbol::SetColor(const COLORREF color)
{
    m_Color = color;
    return true;
}

bool __stdcall CSimpleTextSymbol::GetColor(COLORREF& color) const
{
    color = m_Color;
    return true;
}

void __stdcall CSimpleTextSymbol::SetColorLock(const bool lock)
{
    m_ColorLock = lock;
}

void __stdcall CSimpleTextSymbol::GetColorLock(bool& color) const
{
    color = m_ColorLock;
}

SymbolType __stdcall CSimpleTextSymbol::GetSymbolType() const
{
    return SYMBOLTYPE_TEXT;
}

bool __stdcall CSimpleTextSymbol::SetFont(const LOGFONT& font)
{
    ::memcpy(&m_Font, &font, sizeof(LOGFONT));
    return true;
}

bool __stdcall CSimpleTextSymbol::GetFont(LOGFONT& font) const
{
    ::memcpy(&font, &m_Font, sizeof(LOGFONT));
    return true;
}

bool __stdcall CSimpleTextSymbol::SetWidth(const double width)
{
    m_Width = width;
    return true;
}

bool __stdcall CSimpleTextSymbol::GetWidth(double& width) const
{
    width = m_Width;
    return true;
}

bool __stdcall CSimpleTextSymbol::SetHeight(const double height)
{
    m_Height = height;
    return true;
}

bool __stdcall CSimpleTextSymbol::GetHeight(double& height) const
{
    height = m_Height;
    return true;
}

bool __stdcall CSimpleTextSymbol::SetAngle(const double angle)
{
    m_Angle = angle;
    return true;
}

bool __stdcall CSimpleTextSymbol::GetAngle(double& angle) const
{
    angle = m_Angle;
    return true;
}

bool __stdcall CSimpleTextSymbol::SetText(const char* const text)
{
    m_Text = text;
    return true;
}

const char* __stdcall CSimpleTextSymbol::GetText() const
{
    return m_Text.c_str();
}

void CSimpleTextSymbol::BeforeDraw(HFONT& fontsaved, long& bkmodesaved,
    COLORREF& textcolorsaved, long& rop2saved) const
{
    //计算象素->毫米对应关系
    long logpixelx, logpixely;
    m_pTrans->GetLogPixel(logpixelx, logpixely);
    double quotiety_x = logpixelx
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);
    double quotiety_y = logpixely
        / (CDisplayTransformation::GetMeterQuotiety(UNIT_INCH) * 1000);

    double refscale;
    m_pTrans->GetReferenceScale(refscale);

    LOGFONT font1;
    ::memcpy(&font1, &m_Font, sizeof(LOGFONT));
    if (refscale > 0)
    {
        //使用参考比例尺，屏幕图元尺寸随着显示比例尺变化而变化
        double mapscale;
        m_pTrans->GetMapScale(mapscale);
        double scalequot = refscale / mapscale;

        //将尺寸换算为象素（pixel）单位
        font1.lfWidth = (long)(m_Width * quotiety_x * scalequot / 2);
        font1.lfHeight = (long)(m_Height * quotiety_y * scalequot);
    }
    else
    {
        //不使用参考比例尺，屏幕图元尺寸始终恒定，不随显示比例尺变化而变化

        //将尺寸换算为象素（pixel）单位
        font1.lfWidth = (long)(m_Width * quotiety_x / 2);
        font1.lfHeight = (long)(m_Height * quotiety_y);
    }

    if (0 >= font1.lfWidth)
    {
        font1.lfWidth = 1;
    }

    if (0 >= font1.lfHeight)
    {
        font1.lfHeight = 2;
    }

    //保存当前的场景，设置新的场景
    HFONT font = ::CreateFontIndirect(&font1);
    fontsaved = (HFONT)::SelectObject(m_DC, font);
    bkmodesaved = ::SetBkMode(m_DC, TRANSPARENT);
    textcolorsaved = ::SetTextColor(m_DC, m_Color);
    rop2saved = ::SetROP2(m_DC, m_ROP2);
}

void CSimpleTextSymbol::AfterDraw(const HFONT& fontsaved, const long& bkmodesaved,
    const COLORREF& textcolorsaved, const long& rop2saved) const
{
    HFONT font = (HFONT)::SelectObject(m_DC, fontsaved);
    ::DeleteObject(font);
    ::SetBkMode(m_DC, bkmodesaved);
    ::SetTextColor(m_DC, textcolorsaved);
    ::SetROP2(m_DC, rop2saved);
}

//================================================================================

}

