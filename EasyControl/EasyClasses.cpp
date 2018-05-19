// EasyClasses.cpp : implementation file
//

#include "stdafx.h"
#include "EasyControl.h"
#include "EasyClasses.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#include "..\\easylib\\MemoryStream.h"
#include "..\\easylib\\SymbolLib.h"
#include "..\\easylib\\GroupLayer.h"
#include "..\\easylib\\SlimLayer.h"
#include "..\\easylib\\ShapeLayer.h"
#include "..\\easylib\\BitmapLayer.h"
#include "..\\easylib\\ElementLayer.h"
#include "..\\SymbolUI\\SymbolUI.h"
#include "..\\RendererUI\\RendererUI.h"


void Easy2Geometry(IUnknown* pEasyGeometry, easymap::IGeometryPtr& pGeometry)
{
    pGeometry.Clear();

    if (!pEasyGeometry)
        return;

    IEasyEnvelope* pEasyEnvelope = NULL;
    pEasyGeometry->QueryInterface(IID_IEasyEnvelope, (void**)&pEasyEnvelope);
    if (pEasyEnvelope)
    {
        pEasyGeometry->Release();
        easymap::IEnvelopePtr pEnvelope = (easymap::IEnvelope*)pEasyEnvelope->innergetrawgeometryptr(FALSE);
        CAST_PTR(pEnvelope, pGeometry, IGeometry)
        return;
    }

    IEasyPoint* pEasyPoint = NULL;
    pEasyGeometry->QueryInterface(IID_IEasyPoint, (void**)&pEasyPoint);
    if (pEasyPoint)
    {
        pEasyGeometry->Release();
        easymap::IPointPtr pPoint = (easymap::IPoint*)pEasyPoint->innergetrawgeometryptr(FALSE);
        CAST_PTR(pPoint, pGeometry, IGeometry)
        return;
    }

    IEasyPoints* pEasyPoints = NULL;
    pEasyGeometry->QueryInterface(IID_IEasyPoints, (void**)&pEasyPoints);
    if (pEasyPoints)
    {
        pEasyGeometry->Release();
        easymap::IMultiPointPtr pMultiPoint = (easymap::IMultiPoint*)pEasyPoints->innergetrawgeometryptr(FALSE);
        CAST_PTR(pMultiPoint, pGeometry, IGeometry)
        return;
    }

    IEasyPath* pEasyPath = NULL;
    pEasyGeometry->QueryInterface(IID_IEasyPath, (void**)&pEasyPath);
    if (pEasyPath)
    {
        pEasyGeometry->Release();
        easymap::IPathPtr pPath = (easymap::IPath*)pEasyPath->innergetrawgeometryptr(FALSE);
        CAST_PTR(pPath, pGeometry, IGeometry)
        return;
    }

    IEasyRing* pEasyRing = NULL;
    pEasyGeometry->QueryInterface(IID_IEasyRing, (void**)&pEasyRing);
    if (pEasyRing)
    {
        pEasyGeometry->Release();
        easymap::IRingPtr pRing = (easymap::IRing*)pEasyRing->innergetrawgeometryptr(FALSE);
        CAST_PTR(pRing, pGeometry, IGeometry)
        return;
    }

    IEasyPolyline* pEasyPolyline = NULL;
    pEasyGeometry->QueryInterface(IID_IEasyPolyline, (void**)&pEasyPolyline);
    if (pEasyPolyline)
    {
        pEasyGeometry->Release();
        easymap::IPolylinePtr pPolyline = (easymap::IPolyline*)pEasyPolyline->innergetrawgeometryptr(FALSE);
        CAST_PTR(pPolyline, pGeometry, IGeometry)
        return;
    }

    IEasyPolygon* pEasyPolygon = NULL;
    pEasyGeometry->QueryInterface(IID_IEasyPolygon, (void**)&pEasyPolygon);
    if (pEasyPolygon)
    {
        pEasyGeometry->Release();
        easymap::IPolygonPtr pPolygon = (easymap::IPolygon*)pEasyPolygon->innergetrawgeometryptr(FALSE);
        CAST_PTR(pPolygon, pGeometry, IGeometry)
        return;
    }

}

void Geometry2Easy(easymap::IGeometryPtr pGeometry, IUnknown** ppEasyGeometry)
{
    if (!pGeometry.Assigned() || !ppEasyGeometry)
        return;
    ASSERT(!*ppEasyGeometry);
    *ppEasyGeometry = NULL;

    easymap::CEnvelopePtr pEnvelope;
    easymap::CPointPtr pPoint;
    easymap::CMultiPointPtr pMultiPoint;
    easymap::CPathPtr pPath;
    easymap::CRingPtr pRing;
    easymap::CPolylinePtr pPolyline;
    easymap::CPolygonPtr pPolygon;
    IDispatch* pDispatch = NULL;

    CAST_PTR(pGeometry, pEnvelope, CEnvelope)
    if (pEnvelope.Assigned())
    {
	    CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyEnvelope);
        CEasyEnvelope* pEasyEnvelope = (class CEasyEnvelope*)(pRTC->CreateObject());

        pEasyEnvelope->m_pEnvelope = pEnvelope;

        pDispatch = pEasyEnvelope->GetIDispatch(FALSE);
        pDispatch->QueryInterface(IID_IUnknown, (void**)ppEasyGeometry);
        pDispatch->Release();
        return;
    }

    CAST_PTR(pGeometry, pPoint, CPoint)
    if (pPoint.Assigned())
    {
	    CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyPoint);
        CEasyPoint* pEasyPoint = (class CEasyPoint*)(pRTC->CreateObject());

        pEasyPoint->m_pPoint = pPoint;

        pDispatch = pEasyPoint->GetIDispatch(FALSE);
        pDispatch->QueryInterface(IID_IUnknown, (void**)ppEasyGeometry);
        pDispatch->Release();
        return;
    }

    CAST_PTR(pGeometry, pMultiPoint, CMultiPoint)
    if (pMultiPoint.Assigned())
    {
	    CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyPoints);
        CEasyPoints* pEasyPoints = (class CEasyPoints*)(pRTC->CreateObject());

        pEasyPoints->m_pMultiPoint = pMultiPoint;

        IDispatch* pDispatch = pEasyPoints->GetIDispatch(FALSE);
        pDispatch->QueryInterface(IID_IUnknown, (void**)ppEasyGeometry);
        pDispatch->Release();
        return;
    }

    CAST_PTR(pGeometry, pPath, CPath)
    if (pPath.Assigned())
    {
	    CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyPath);
        CEasyPath* pEasyPath = (class CEasyPath*)(pRTC->CreateObject());

        pEasyPath->m_pPath = pPath;

        pDispatch = pEasyPath->GetIDispatch(FALSE);
        pDispatch->QueryInterface(IID_IUnknown, (void**)ppEasyGeometry);
        pDispatch->Release();
        return;
    }

    CAST_PTR(pGeometry, pRing, CRing)
    if (pRing.Assigned())
    {
	    CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyRing);
        CEasyRing* pEasyRing = (class CEasyRing*)(pRTC->CreateObject());

        pEasyRing->m_pRing = pRing;

        pDispatch = pEasyRing->GetIDispatch(FALSE);
        pDispatch->QueryInterface(IID_IUnknown, (void**)ppEasyGeometry);
        pDispatch->Release();
        return;
    }

    CAST_PTR(pGeometry, pPolyline, CPolyline)
    if (pPolyline.Assigned())
    {
	    CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyPolyline);
        CEasyPolyline* pEasyPolyline = (class CEasyPolyline*)(pRTC->CreateObject());

        pEasyPolyline->m_pPolyline = pPolyline;

        pDispatch = pEasyPolyline->GetIDispatch(FALSE);
        pDispatch->QueryInterface(IID_IUnknown, (void**)ppEasyGeometry);
        pDispatch->Release();
        return;
    }

    CAST_PTR(pGeometry, pPolygon, CPolygon)
    if (pPolygon.Assigned())
    {
	    CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyPolygon);
        CEasyPolygon* pEasyPolygon = (class CEasyPolygon*)(pRTC->CreateObject());

        pEasyPolygon->m_pPolygon = pPolygon;

        pDispatch = pEasyPolygon->GetIDispatch(FALSE);
        pDispatch->QueryInterface(IID_IUnknown, (void**)ppEasyGeometry);
        pDispatch->Release();
        return;
    }
}


/////////////////////////////////////////////////////////////////////////////
// CEasyEnvelope

IMPLEMENT_DYNCREATE(CEasyEnvelope, CCmdTarget)

CEasyEnvelope::CEasyEnvelope()
{
	EnableAutomation();
	
	// To keep the application running as long as an OLE automation 
	//	object is active, the constructor calls AfxOleLockApp.
	
	AfxOleLockApp();

    EASYLIB_CREATEOBJ(CEnvelope, m_pEnvelope, CEnvelope)
}

CEasyEnvelope::~CEasyEnvelope()
{
	// To terminate the application when all objects created with
	// 	with OLE automation, the destructor calls AfxOleUnlockApp.
	
	AfxOleUnlockApp();
}


void CEasyEnvelope::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CEasyEnvelope, CCmdTarget)
	//{{AFX_MSG_MAP(CEasyEnvelope)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CEasyEnvelope, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CEasyEnvelope)
    DISP_FUNCTION(CEasyEnvelope, "SetCoordinates", SetCoordinates, VT_EMPTY, VTS_R8 VTS_R8 VTS_R8 VTS_R8)
    DISP_FUNCTION(CEasyEnvelope, "GetCoordinates", GetCoordinates, VT_EMPTY, VTS_PR8 VTS_PR8 VTS_PR8 VTS_PR8)
	DISP_FUNCTION(CEasyEnvelope, "innergetrawgeometryptr", innergetrawgeometryptr, VT_I4, VTS_BOOL)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CEasyEnvelope, CCmdTarget)
	INTERFACE_PART(CEasyEnvelope, IID_IEasyEnvelope, Dispatch)
END_INTERFACE_MAP()

// {5563121A-7787-4FCE-93F7-32234F92151C}
IMPLEMENT_OLECREATE(CEasyEnvelope, "EasyControl.EasyEnvelope", 0x5563121a, 0x7787, 0x4fce, 0x93, 0xf7, 0x32, 0x23, 0x4f, 0x92, 0x15, 0x1c)

/////////////////////////////////////////////////////////////////////////////
// CEasyEnvelope message handlers

void CEasyEnvelope::SetCoordinates(double minx, double miny, double maxx, double maxy) 
{
    m_pEnvelope->SetMinX(minx);
    m_pEnvelope->SetMinY(miny);
    m_pEnvelope->SetMaxX(maxx);
    m_pEnvelope->SetMaxY(maxy);
}

void CEasyEnvelope::GetCoordinates(double FAR* minx, double FAR* miny, double FAR* maxx, double FAR* maxy) 
{
    m_pEnvelope->GetMinX(*minx);
    m_pEnvelope->GetMinY(*miny);
    m_pEnvelope->GetMaxX(*maxx);
    m_pEnvelope->GetMaxY(*maxy);
}

long CEasyEnvelope::innergetrawgeometryptr(BOOL addref) 
{
    if (addref && m_pEnvelope.Assigned())
        m_pEnvelope->_AddRef();

    return (long)m_pEnvelope._p();
}

/////////////////////////////////////////////////////////////////////////////
// CEasyPoint

IMPLEMENT_DYNCREATE(CEasyPoint, CCmdTarget)

CEasyPoint::CEasyPoint()
{
	EnableAutomation();
	
	// To keep the application running as long as an OLE automation 
	//	object is active, the constructor calls AfxOleLockApp.
	
	AfxOleLockApp();

    EASYLIB_CREATEOBJ(CPoint, m_pPoint, CPoint)
}

CEasyPoint::~CEasyPoint()
{
	// To terminate the application when all objects created with
	// 	with OLE automation, the destructor calls AfxOleUnlockApp.
	
	AfxOleUnlockApp();
}


void CEasyPoint::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CEasyPoint, CCmdTarget)
	//{{AFX_MSG_MAP(CEasyPoint)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CEasyPoint, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CEasyPoint)
    DISP_FUNCTION(CEasyPoint, "SetCoordinates", SetCoordinates, VT_EMPTY, VTS_R8 VTS_R8 VTS_R8)
    DISP_FUNCTION(CEasyPoint, "GetCoordinates", GetCoordinates, VT_EMPTY, VTS_PR8 VTS_PR8 VTS_PR8)
	DISP_FUNCTION(CEasyPoint, "innergetrawgeometryptr", innergetrawgeometryptr, VT_I4, VTS_BOOL)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CEasyPoint, CCmdTarget)
	INTERFACE_PART(CEasyPoint, IID_IEasyPoint, Dispatch)
END_INTERFACE_MAP()

// {123ACC11-9497-4421-9D2A-C6D745873DE5}
IMPLEMENT_OLECREATE(CEasyPoint, "EasyControl.EasyPoint", 0x123acc11, 0x9497, 0x4421, 0x9d, 0x2a, 0xc6, 0xd7, 0x45, 0x87, 0x3d, 0xe5)

/////////////////////////////////////////////////////////////////////////////
// CEasyPoint message handlers

void CEasyPoint::SetCoordinates(double x, double y, double z) 
{
    m_pPoint->SetX(x);
    m_pPoint->SetY(y);
    m_pPoint->SetZ(z);
}

void CEasyPoint::GetCoordinates(double FAR* x, double FAR* y, double FAR* z) 
{
    m_pPoint->GetX(*x);
    m_pPoint->GetY(*y);
    m_pPoint->GetZ(*z);
}

long CEasyPoint::innergetrawgeometryptr(BOOL addref) 
{
    if (addref && m_pPoint.Assigned())
        m_pPoint->_AddRef();

    return (long)m_pPoint._p();
}

/////////////////////////////////////////////////////////////////////////////
// CEasyPoints

IMPLEMENT_DYNCREATE(CEasyPoints, CCmdTarget)

CEasyPoints::CEasyPoints()
{
	EnableAutomation();
	
	// To keep the application running as long as an OLE automation 
	//	object is active, the constructor calls AfxOleLockApp.
	
	AfxOleLockApp();

    EASYLIB_CREATEOBJ(CMultiPoint, m_pMultiPoint, CMultiPoint)
}

CEasyPoints::~CEasyPoints()
{
	// To terminate the application when all objects created with
	// 	with OLE automation, the destructor calls AfxOleUnlockApp.
	
	AfxOleUnlockApp();
}


void CEasyPoints::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CEasyPoints, CCmdTarget)
	//{{AFX_MSG_MAP(CEasyPoints)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CEasyPoints, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CEasyPoints)
	DISP_FUNCTION(CEasyPoints, "AddPoint", AddPoint, VT_EMPTY, VTS_R8 VTS_R8 VTS_R8)
	DISP_FUNCTION(CEasyPoints, "GetPoint", GetPoint, VT_BOOL, VTS_PR8 VTS_PR8 VTS_PR8 VTS_I4)
	DISP_FUNCTION(CEasyPoints, "GetPointCount", GetPointCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyPoints, "ClearPoints", ClearPoints, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyPoints, "innergetrawgeometryptr", innergetrawgeometryptr, VT_I4, VTS_BOOL)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CEasyPoints, CCmdTarget)
	INTERFACE_PART(CEasyPoints, IID_IEasyPoints, Dispatch)
END_INTERFACE_MAP()

// {6D82EB14-1A81-45CB-B289-A8F82F09B2D2}
IMPLEMENT_OLECREATE(CEasyPoints, "EasyControl.EasyPoints", 0x6d82eb14, 0x1a81, 0x45cb, 0xb2, 0x89, 0xa8, 0xf8, 0x2f, 0x9, 0xb2, 0xd2)

/////////////////////////////////////////////////////////////////////////////
// CEasyPoints message handlers

void CEasyPoints::AddPoint(double x, double y, double z) 
{
    easymap::WKSPointZ pntz(x, y, z);
    m_pMultiPoint->AddPoint(pntz);
}

BOOL CEasyPoints::GetPoint(double FAR* x, double FAR* y, double FAR* z, long index) 
{
    if ((index < 0) || (index >= m_pMultiPoint->GetPointCount()))
        return FALSE;

    easymap::WKSPointZ pntz;
    m_pMultiPoint->GetPoint(pntz, index);
    *x = pntz.x;
    *y = pntz.y;
    *z = pntz.z;
	return TRUE;
}

long CEasyPoints::GetPointCount() 
{
	return m_pMultiPoint->GetPointCount();
}

void CEasyPoints::ClearPoints() 
{
    m_pMultiPoint->ClearPoint();
}

long CEasyPoints::innergetrawgeometryptr(BOOL addref) 
{
    if (addref && m_pMultiPoint.Assigned())
        m_pMultiPoint->_AddRef();

    return (long)m_pMultiPoint._p();
}


/////////////////////////////////////////////////////////////////////////////
// CEasyPath

IMPLEMENT_DYNCREATE(CEasyPath, CCmdTarget)

CEasyPath::CEasyPath()
{
	EnableAutomation();
	
	// To keep the application running as long as an OLE automation 
	//	object is active, the constructor calls AfxOleLockApp.
	
	AfxOleLockApp();

    EASYLIB_CREATEOBJ(CPath, m_pPath, CPath)
}

CEasyPath::~CEasyPath()
{
	// To terminate the application when all objects created with
	// 	with OLE automation, the destructor calls AfxOleUnlockApp.
	
	AfxOleUnlockApp();
}


void CEasyPath::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CEasyPath, CCmdTarget)
	//{{AFX_MSG_MAP(CEasyPath)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CEasyPath, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CEasyPath)
	DISP_FUNCTION(CEasyPath, "AddPoint", AddPoint, VT_EMPTY, VTS_R8 VTS_R8 VTS_R8)
	DISP_FUNCTION(CEasyPath, "GetPoint", GetPoint, VT_BOOL, VTS_PR8 VTS_PR8 VTS_PR8 VTS_I4)
	DISP_FUNCTION(CEasyPath, "GetPointCount", GetPointCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyPath, "ClearPoints", ClearPoints, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyPath, "innergetrawgeometryptr", innergetrawgeometryptr, VT_I4, VTS_BOOL)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CEasyPath, CCmdTarget)
	INTERFACE_PART(CEasyPath, IID_IEasyPath, Dispatch)
END_INTERFACE_MAP()

// {8BFBC8FF-476D-4C67-A63E-0519384A9311}
IMPLEMENT_OLECREATE(CEasyPath, "EasyControl.EasyPath", 0x8bfbc8ff, 0x476d, 0x4c67, 0xa6, 0x3e, 0x5, 0x19, 0x38, 0x4a, 0x93, 0x11)

/////////////////////////////////////////////////////////////////////////////
// CEasyPath message handlers

void CEasyPath::AddPoint(double x, double y, double z) 
{
    easymap::WKSPointZ pntz(x, y, z);
    m_pPath->AddPoint(pntz);
}

BOOL CEasyPath::GetPoint(double FAR* x, double FAR* y, double FAR* z, long index) 
{
    if ((index < 0) || (index >= m_pPath->GetPointCount()))
        return FALSE;

    easymap::WKSPointZ pntz;
    m_pPath->GetPoint1(index, pntz);
    *x = pntz.x;
    *y = pntz.y;
    *z = pntz.z;
	return TRUE;
}

long CEasyPath::GetPointCount() 
{
	return m_pPath->GetPointCount();
}

void CEasyPath::ClearPoints() 
{
    m_pPath->ClearPoint();
}

long CEasyPath::innergetrawgeometryptr(BOOL addref) 
{
    if (addref && m_pPath.Assigned())
        m_pPath->_AddRef();

    return (long)m_pPath._p();
}


/////////////////////////////////////////////////////////////////////////////
// CEasyRing

IMPLEMENT_DYNCREATE(CEasyRing, CCmdTarget)

CEasyRing::CEasyRing()
{
	EnableAutomation();
	
	// To keep the application running as long as an OLE automation 
	//	object is active, the constructor calls AfxOleLockApp.
	
	AfxOleLockApp();

    EASYLIB_CREATEOBJ(CRing, m_pRing, CRing)
}

CEasyRing::~CEasyRing()
{
	// To terminate the application when all objects created with
	// 	with OLE automation, the destructor calls AfxOleUnlockApp.
	
	AfxOleUnlockApp();
}


void CEasyRing::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CEasyRing, CCmdTarget)
	//{{AFX_MSG_MAP(CEasyRing)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CEasyRing, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CEasyRing)
	DISP_FUNCTION(CEasyRing, "AddPoint", AddPoint, VT_EMPTY, VTS_R8 VTS_R8 VTS_R8)
	DISP_FUNCTION(CEasyRing, "GetPoint", GetPoint, VT_BOOL, VTS_PR8 VTS_PR8 VTS_PR8 VTS_I4)
	DISP_FUNCTION(CEasyRing, "GetPointCount", GetPointCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyRing, "ClearPoints", ClearPoints, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyRing, "innergetrawgeometryptr", innergetrawgeometryptr, VT_I4, VTS_BOOL)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CEasyRing, CCmdTarget)
	INTERFACE_PART(CEasyRing, IID_IEasyRing, Dispatch)
END_INTERFACE_MAP()

// {3631B8DC-0BE1-443F-A65B-ADB58F907EA2}
IMPLEMENT_OLECREATE(CEasyRing, "EasyControl.EasyRing", 0x3631b8dc, 0xbe1, 0x443f, 0xa6, 0x5b, 0xad, 0xb5, 0x8f, 0x90, 0x7e, 0xa2)

/////////////////////////////////////////////////////////////////////////////
// CEasyRing message handlers

void CEasyRing::AddPoint(double x, double y, double z) 
{
    easymap::WKSPointZ pntz(x, y, z);
    m_pRing->AddPoint(pntz);
}

BOOL CEasyRing::GetPoint(double FAR* x, double FAR* y, double FAR* z, long index) 
{
    if ((index < 0) || (index >= m_pRing->GetPointCount()))
        return FALSE;

    easymap::WKSPointZ pntz;
    m_pRing->GetPoint1(index, pntz);
    *x = pntz.x;
    *y = pntz.y;
    *z = pntz.z;
	return TRUE;
}

long CEasyRing::GetPointCount() 
{
	return m_pRing->GetPointCount();
}

void CEasyRing::ClearPoints() 
{
    m_pRing->ClearPoint();
}

long CEasyRing::innergetrawgeometryptr(BOOL addref) 
{
    if (addref && m_pRing.Assigned())
        m_pRing->_AddRef();

    return (long)m_pRing._p();
}


/////////////////////////////////////////////////////////////////////////////
// CEasyPolyline

IMPLEMENT_DYNCREATE(CEasyPolyline, CCmdTarget)

CEasyPolyline::CEasyPolyline()
{
	EnableAutomation();
	
	// To keep the application running as long as an OLE automation 
	//	object is active, the constructor calls AfxOleLockApp.
	
	AfxOleLockApp();

    EASYLIB_CREATEOBJ(CPolyline, m_pPolyline, CPolyline)
}

CEasyPolyline::~CEasyPolyline()
{
	AfxOleUnlockApp();
}


void CEasyPolyline::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CEasyPolyline, CCmdTarget)
	//{{AFX_MSG_MAP(CEasyPolyline)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CEasyPolyline, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CEasyPolyline)
	DISP_FUNCTION(CEasyPolyline, "AddPath", AddPath, VT_EMPTY, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyPolyline, "GetPath", GetPath, VT_UNKNOWN, VTS_I4)
	DISP_FUNCTION(CEasyPolyline, "GetPathCount", GetPathCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyPolyline, "ClearPath", ClearPath, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyPolyline, "innergetrawgeometryptr", innergetrawgeometryptr, VT_I4, VTS_BOOL)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CEasyPolyline, CCmdTarget)
	INTERFACE_PART(CEasyPolyline, IID_IEasyPolyline, Dispatch)
END_INTERFACE_MAP()

// {CD30F34E-892E-486E-A8F4-AF5D6F0AC9E3}
IMPLEMENT_OLECREATE(CEasyPolyline, "EasyControl.EasyPolyline", 0xcd30f34e, 0x892e, 0x486e, 0xa8, 0xf4, 0xaf, 0x5d, 0x6f, 0xa, 0xc9, 0xe3)

/////////////////////////////////////////////////////////////////////////////
// CEasyPolyline message handlers

void CEasyPolyline::AddPath(LPUNKNOWN path) 
{
    if (!path)
        return;
    IEasyPath* pEasyPath = NULL;
    path->QueryInterface(IID_IEasyPath, (void**)&pEasyPath);
    if (!pEasyPath)
        return;

    easymap::CPathPtr pPath = (easymap::CPath*)pEasyPath->innergetrawgeometryptr(FALSE);
    m_pPolyline->AddPathRef((easymap::IPath*)pPath._p());
}

LPUNKNOWN CEasyPolyline::GetPath(long index) 
{
    if ((index < 0) || (index >= m_pPolyline->GetPathCount()))
        return NULL;

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyPath);
    CEasyPath* pEasyPath = (class CEasyPath*)(pRTC->CreateObject());

    pEasyPath->m_pPath.Clear();
    easymap::IPathPtr pPath;
    m_pPolyline->GetPathRef(pPath._ref(), index);
    CAST_PTR(pPath, pEasyPath->m_pPath, CPath)

    IDispatch* pDispatch = pEasyPath->GetIDispatch(FALSE);
    LPUNKNOWN pResult = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pResult);
    pDispatch->Release();
    return pResult;
}

long CEasyPolyline::GetPathCount() 
{
    return m_pPolyline->GetPathCount();
}

void CEasyPolyline::ClearPath() 
{
    m_pPolyline->ClearPath();
}

long CEasyPolyline::innergetrawgeometryptr(BOOL addref) 
{
    if (addref && m_pPolyline.Assigned())
        m_pPolyline->_AddRef();

    return (long)m_pPolyline._p();
}


/////////////////////////////////////////////////////////////////////////////
// CEasyPolygon

IMPLEMENT_DYNCREATE(CEasyPolygon, CCmdTarget)

CEasyPolygon::CEasyPolygon()
{
	EnableAutomation();
	
	// To keep the application running as long as an OLE automation 
	//	object is active, the constructor calls AfxOleLockApp.
	
	AfxOleLockApp();
}

CEasyPolygon::~CEasyPolygon()
{
	AfxOleUnlockApp();
}


void CEasyPolygon::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CEasyPolygon, CCmdTarget)
	//{{AFX_MSG_MAP(CEasyPolygon)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CEasyPolygon, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CEasyPolygon)
	DISP_FUNCTION(CEasyPolygon, "AddRing", AddRing, VT_EMPTY, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyPolygon, "GetRing", GetRing, VT_UNKNOWN, VTS_I4)
	DISP_FUNCTION(CEasyPolygon, "GetRingCount", GetRingCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyPolygon, "ClearRings", ClearRings, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyPolygon, "innergetrawgeometryptr", innergetrawgeometryptr, VT_I4, VTS_BOOL)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CEasyPolygon, CCmdTarget)
	INTERFACE_PART(CEasyPolygon, IID_IEasyPolygon, Dispatch)
END_INTERFACE_MAP()

// {3177375A-4628-4761-806E-583BC09DE6B2}
IMPLEMENT_OLECREATE(CEasyPolygon, "EasyControl.EasyPolygon", 0x3177375a, 0x4628, 0x4761, 0x80, 0x6e, 0x58, 0x3b, 0xc0, 0x9d, 0xe6, 0xb2)

/////////////////////////////////////////////////////////////////////////////
// CEasyPolygon message handlers

void CEasyPolygon::AddRing(LPUNKNOWN ring) 
{
    if (!ring)
        return;
    IEasyRing* pEasyRing = NULL;
    ring->QueryInterface(IID_IEasyRing, (void**)&pEasyRing);
    if (!pEasyRing)
        return;

    easymap::IRingPtr pRing = (easymap::IRing*)pEasyRing->innergetrawgeometryptr(FALSE);
    m_pPolygon->AddRingRef(pRing._p());
}

LPUNKNOWN CEasyPolygon::GetRing(long index) 
{
    if ((index < 0) || (index >= m_pPolygon->GetRingCount()))
        return NULL;

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyRing);
    CEasyRing* pEasyRing = (class CEasyRing*)(pRTC->CreateObject());

    pEasyRing->m_pRing.Clear();
    m_pPolygon->GetRingRef((easymap::IRing**)pEasyRing->m_pRing._ref(), index);

    IDispatch* pDispatch = pEasyRing->GetIDispatch(FALSE);
    LPUNKNOWN pResult = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pResult);
    pDispatch->Release();
    return pResult;
}

long CEasyPolygon::GetRingCount() 
{
	return m_pPolygon->GetRingCount();
}

void CEasyPolygon::ClearRings() 
{
    m_pPolygon->ClearRing();
}

long CEasyPolygon::innergetrawgeometryptr(BOOL addref) 
{
    if (addref && m_pPolygon.Assigned())
        m_pPolygon->_AddRef();

    return (long)m_pPolygon._p();
}


/////////////////////////////////////////////////////////////////////////////
// CEasyLayer

IMPLEMENT_DYNCREATE(CEasyLayer, CCmdTarget)

CEasyLayer::CEasyLayer()
{
	EnableAutomation();
}

CEasyLayer::~CEasyLayer()
{
}


void CEasyLayer::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CEasyLayer, CCmdTarget)
	//{{AFX_MSG_MAP(CEasyLayer)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CEasyLayer, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CEasyLayer)
	DISP_FUNCTION(CEasyLayer, "GetExtent", GetExtent, VT_EMPTY, VTS_PR8 VTS_PR8 VTS_PR8 VTS_PR8)
	DISP_FUNCTION(CEasyLayer, "GetBascScale", GetBascScale, VT_R8, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "SetName", SetName, VT_EMPTY, VTS_BSTR)
	DISP_FUNCTION(CEasyLayer, "GetName", GetName, VT_BSTR, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "SetVisible", SetVisible, VT_EMPTY, VTS_BOOL)
	DISP_FUNCTION(CEasyLayer, "GetVisible", GetVisible, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "SetAlpha", SetAlpha, VT_EMPTY, VTS_I4)
	DISP_FUNCTION(CEasyLayer, "GetAlpha", GetAlpha, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "SetScaleLimit", SetScaleLimit, VT_EMPTY, VTS_R8 VTS_R8)
	DISP_FUNCTION(CEasyLayer, "GetScaleLimit", GetScaleLimit, VT_EMPTY, VTS_PR8 VTS_PR8)
	DISP_FUNCTION(CEasyLayer, "SetTag", SetTag, VT_EMPTY, VTS_I4)
	DISP_FUNCTION(CEasyLayer, "GetTag", GetTag, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "SetSelectable", SetSelectable, VT_EMPTY, VTS_BOOL)
	DISP_FUNCTION(CEasyLayer, "GetSelectable", GetSelectable, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "Select", Select, VT_I4, VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_BOOL VTS_BOOL)
	DISP_FUNCTION(CEasyLayer, "Deselect", Deselect, VT_I4, VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_BOOL)
	DISP_FUNCTION(CEasyLayer, "GetSelectCount", GetSelectCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "ClearSelection", ClearSelection, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "GetLayerType", GetLayerType, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "GetSubLayerCount", GetSubLayerCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "GetSubLayer", GetSubLayer, VT_UNKNOWN, VTS_I4)
	DISP_FUNCTION(CEasyLayer, "DeleteSubLayer", DeleteSubLayer, VT_BOOL, VTS_I4)
	DISP_FUNCTION(CEasyLayer, "AddSubLayer", AddSubLayer, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyLayer, "ClearLayers", ClearLayers, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "innergetrawlayerptr", innergetrawlayerptr, VT_I4, VTS_BOOL)
	DISP_FUNCTION(CEasyLayer, "GetAllSubLayerCount", GetAllSubLayerCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "DeleteSubLayerEx", DeleteSubLayerEx, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyLayer, "SetSubLayerOrder", SetSubLayerOrder, VT_BOOL, VTS_UNKNOWN VTS_I4)
	DISP_FUNCTION(CEasyLayer, "GetFeatureLayerType", GetFeatureLayerType, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "GetFeatureLayerInfo", GetFeatureLayerInfo, VT_BOOL, VTS_PR8 VTS_PI4 VTS_PR8)
	DISP_FUNCTION(CEasyLayer, "SetDefaultSymbolFromLib", SetDefaultSymbolFromLib, VT_BOOL, VTS_BSTR VTS_I4)
	DISP_FUNCTION(CEasyLayer, "SetDefaultSymbolFromUI", SetDefaultSymbolFromUI, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "SetDefaultSymbolParam", SetDefaultSymbolParam, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyLayer, "GetDefaultSymbolParam", GetDefaultSymbolParam, VT_UNKNOWN, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "GetAttribFieldCount", GetAttribFieldCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "GetAttribFieldType", GetAttribFieldType, VT_I4, VTS_I4)
	DISP_FUNCTION(CEasyLayer, "GetAttribFieldName", GetAttribFieldName, VT_BSTR, VTS_I4)
	DISP_FUNCTION(CEasyLayer, "GetSelection", GetSelection, VT_UNKNOWN, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "GetFeature", GetFeature, VT_UNKNOWN, VTS_I4)
	DISP_FUNCTION(CEasyLayer, "CreateFeature", CreateFeature, VT_UNKNOWN, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "DeleteSelectedObjects", DeleteSelectedObjects, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "MoveSelectedObject", MoveSelectedObject, VT_EMPTY, VTS_R8 VTS_R8)
	DISP_FUNCTION(CEasyLayer, "AddGeometryElement", AddGeometryElement, VT_I4, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyLayer, "AddTextElement", AddTextElement, VT_I4, VTS_R8 VTS_R8 VTS_BSTR)
	DISP_FUNCTION(CEasyLayer, "GetElement", GetElement, VT_UNKNOWN, VTS_I4 VTS_PBSTR VTS_PBOOL)
	DISP_FUNCTION(CEasyLayer, "SetElement", SetElement, VT_BOOL, VTS_I4 VTS_UNKNOWN VTS_BSTR)
	DISP_FUNCTION(CEasyLayer, "RemoveElement", RemoveElement, VT_BOOL, VTS_I4)
	DISP_FUNCTION(CEasyLayer, "Identify", Identify, VT_UNKNOWN, VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_BOOL)
	DISP_FUNCTION(CEasyLayer, "SelectAll", SelectAll, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "FindSubLayer", FindSubLayer, VT_UNKNOWN, VTS_BSTR)
	DISP_FUNCTION(CEasyLayer, "ModifyGPSPoint", ModifyGPSPoint, VT_BOOL, VTS_I4 VTS_R8 VTS_R8)
	DISP_FUNCTION(CEasyLayer, "GetMapEditable", GetMapEditable, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "SaveEdit", SaveEdit, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "DiscardEdit", DiscardEdit, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "SetUndoPoint", SetUndoPoint, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "Undo", Undo, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "Redo", Redo, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "Undoable", Undoable, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "Redoable", Redoable, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyLayer, "SetMapEditable", SetMapEditable, VT_BOOL, VTS_BOOL)
	DISP_FUNCTION(CEasyLayer, "ShowRendererUI", ShowRendererUI, VT_BOOL, VTS_NONE)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CEasyLayer, CCmdTarget)
	INTERFACE_PART(CEasyLayer, IID_IEasyLayer, Dispatch)
END_INTERFACE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEasyLayer message handlers

void CEasyLayer::GetExtent(double FAR* minx, double FAR* miny, double FAR* maxx, double FAR* maxy) 
{
    if (!m_pLayer.Assigned())
        return;

    easymap::WKSRect extent;
    m_pLayer->GetExtent(extent);
    *minx = extent.left;
    *miny = extent.bottom;
    *maxx = extent.right;
    *maxy = extent.top;
}

double CEasyLayer::GetBascScale() 
{
    if (!m_pLayer.Assigned())
        return 0;

	double scale = 0;
    m_pLayer->GetBaseScale(scale);
    return scale;
}

void CEasyLayer::SetName(LPCTSTR layername) 
{
    if (!m_pLayer.Assigned())
        return;

    m_pLayer->SetName(layername);
}

BSTR CEasyLayer::GetName() 
{
	CString strResult;
    if (m_pLayer.Assigned())
    {
        strResult = m_pLayer->GetName();
    }

	return strResult.AllocSysString();
}

void CEasyLayer::SetVisible(BOOL visible) 
{
    if (!m_pLayer.Assigned())
        return;

    bool v = visible ? true:false;
    m_pLayer->SetVisible(v);
}

BOOL CEasyLayer::GetVisible() 
{
    if (!m_pLayer.Assigned())
        return FALSE;

    return m_pLayer->GetVisible() ? TRUE:FALSE;
}

void CEasyLayer::SetAlpha(long alpha) 
{
    if (!m_pLayer.Assigned())
        return;

    m_pLayer->SetAlpha(alpha);
}

long CEasyLayer::GetAlpha() 
{
    if (!m_pLayer.Assigned())
        return -1;

	return m_pLayer->GetAlpha();
}

void CEasyLayer::SetScaleLimit(double maxscale, double minscale) 
{
    if (!m_pLayer.Assigned())
        return;

    m_pLayer->SetScaleLimit(maxscale, minscale);
}

void CEasyLayer::GetScaleLimit(double FAR* maxscale, double FAR* minscale) 
{
    if (!m_pLayer.Assigned())
        return;

    m_pLayer->GetScaleLimit(*maxscale, *minscale);
}

void CEasyLayer::SetTag(long tag) 
{
    if (!m_pLayer.Assigned())
        return;

    m_pLayer->SetTag(tag);
}

long CEasyLayer::GetTag() 
{
    if (!m_pLayer.Assigned())
        return 0;

    return m_pLayer->GetTag();
}

void CEasyLayer::SetSelectable(BOOL selectable) 
{
    if (!m_pLayer.Assigned())
        return;

    bool sa = selectable ? true:false;
    m_pLayer->SetSelectable(sa);
}

BOOL CEasyLayer::GetSelectable() 
{
    if (!m_pLayer.Assigned())
        return FALSE;

    return m_pLayer->GetSelectable() ? TRUE:FALSE;
}

long CEasyLayer::Select(double minx, double miny, double maxx, double maxy, BOOL partialselect, BOOL append) 
{
    if (!m_pLayer.Assigned())
        return -1;

    easymap::WKSRect env;
    env.left = minx;
    env.bottom = miny;
    env.right = maxx;
    env.top = maxy;
    return m_pLayer->Select(env, partialselect?true:false, append?true:false);
}

long CEasyLayer::Deselect(double minx, double miny, double maxx, double maxy, BOOL partialselect) 
{
    if (!m_pLayer.Assigned())
        return -1;

    easymap::WKSRect env;
    env.left = minx;
    env.bottom = miny;
    env.right = maxx;
    env.top = maxy;
    return m_pLayer->Deselect(env, partialselect?true:false);
}

long CEasyLayer::GetSelectCount() 
{
    if (!m_pLayer.Assigned())
        return -1;

	return 0;
}

void CEasyLayer::ClearSelection() 
{
    if (!m_pLayer.Assigned())
        return;

    m_pLayer->ClearSelection();
}

long CEasyLayer::GetLayerType() 
{
    if (!m_pLayer.Assigned())
    	return EASYLAYERTYPE_UNKNOWN;

    easymap::CGroupLayerPtr pGL;
    CAST_PTR(m_pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
        return EASYLAYERTYPE_GROUPLAYER;

    easymap::CSlimLayerPtr pSlimLayer;
    CAST_PTR(m_pLayer, pSlimLayer, CSlimLayer)
    if (pSlimLayer.Assigned())
        return EASYLAYERTYPE_SLIMLAYER;

    easymap::CShapeLayerPtr pShapeLayer;
    CAST_PTR(m_pLayer, pShapeLayer, CShapeLayer)
    if (pShapeLayer.Assigned())
        return EASYLAYERTYPE_SHAPELAYER;

    easymap::CBitmapLayerPtr pBL;
    CAST_PTR(m_pLayer, pBL, CBitmapLayer)
    if (pBL.Assigned())
        return EASYLAYERTYPE_BITMAPLAYER;

    easymap::CElementLayerPtr pEL;
    CAST_PTR(m_pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
        return EASYLAYERTYPE_ELEMENTLAYER;

	return EASYLAYERTYPE_UNKNOWN;
}

long CEasyLayer::GetSubLayerCount() 
{
    if (!m_pLayer.Assigned())
    	return -1;

    easymap::CGroupLayerPtr pGL;
    CAST_PTR(m_pLayer, pGL, CGroupLayer)
    if (!pGL.Assigned())
        return -1;

    return pGL->GetLayerCount();
}

LPUNKNOWN CEasyLayer::GetSubLayer(long index) 
{
    if (!m_pLayer.Assigned())
    	return NULL;

    easymap::CGroupLayerPtr pGL;
    CAST_PTR(m_pLayer, pGL, CGroupLayer)
    if (!pGL.Assigned())
        return NULL;

    long count = pGL->GetLayerCount();
    if ((index < 0) || (index >= count))
        return NULL;

    easymap::ILayerPtr pSubLayer;
    pGL->GetLayer(pSubLayer._ref(), index);

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyLayer);
    CEasyLayer* pEasySubLayer = (class CEasyLayer*)(pRTC->CreateObject());
    pEasySubLayer->m_pLayer = pSubLayer;
    IDispatch* pDispatch = pEasySubLayer->GetIDispatch(FALSE);
    LPUNKNOWN pOutLayer = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pOutLayer);
    pDispatch->Release();
    return pOutLayer;
}

BOOL CEasyLayer::DeleteSubLayer(long index) 
{
    if (!m_pLayer.Assigned())
    	return FALSE;
    easymap::CGroupLayerPtr pGL;
    CAST_PTR(m_pLayer, pGL, CGroupLayer)
    if (!pGL.Assigned())
        return FALSE;

    long count = pGL->GetLayerCount();
    if ((index < 0) || (index >= count))
        return FALSE;

    return pGL->DeleteLayer(index) ? TRUE:FALSE;
}

BOOL CEasyLayer::AddSubLayer(LPUNKNOWN layer) 
{
    if (!m_pLayer.Assigned())
    	return FALSE;
    if (!layer)
        return FALSE;

    easymap::CGroupLayerPtr pGL;
    CAST_PTR(m_pLayer, pGL, CGroupLayer)
    if (!pGL.Assigned())
        return FALSE;

    IEasyLayer* pEasyLayer = NULL;
    layer->QueryInterface(IID_IEasyLayer, (void**)&pEasyLayer);
    layer->Release();
    if (!pEasyLayer)
        return FALSE;

    easymap::ILayerPtr pRawLayer = (easymap::ILayer*)pEasyLayer->innergetrawlayerptr(FALSE);
    return pGL->AddLayer(pRawLayer._p()) ? TRUE:FALSE;
}

BOOL CEasyLayer::ClearLayers() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::CGroupLayerPtr pGL;
    CAST_PTR(m_pLayer, pGL, CGroupLayer)
    if (!pGL.Assigned())
        return FALSE;

    pGL->ClearLayers();
    return TRUE;
}

long CEasyLayer::innergetrawlayerptr(BOOL addref) 
{
    if (addref && m_pLayer.Assigned())
        m_pLayer->_AddRef();

	return (long)m_pLayer._p();
}

long CEasyLayer::GetAllSubLayerCount() 
{
    if (!m_pLayer.Assigned())
    	return -1;

    easymap::CGroupLayerPtr pGL;
    CAST_PTR(m_pLayer, pGL, CGroupLayer)
    if (!pGL.Assigned())
        return -1;

	return pGL->GetAllCount();
}

BOOL CEasyLayer::DeleteSubLayerEx(LPUNKNOWN sublayer) 
{
    if (!sublayer || !m_pLayer.Assigned())
    	return FALSE;

    IEasyLayer* pEasyLayer = NULL;
    sublayer->QueryInterface(IID_IEasyLayer, (void**)&pEasyLayer);
    if (!pEasyLayer)
        return FALSE;
    sublayer->Release();

    easymap::CGroupLayerPtr pGL;
    CAST_PTR(m_pLayer, pGL, CGroupLayer)
    if (!pGL.Assigned())
        return FALSE;

    easymap::ILayerPtr pSubLayer = (easymap::ILayer*)pEasyLayer->innergetrawlayerptr(FALSE);
    return pGL->DeleteLayerEx(pSubLayer._p()) ? TRUE:FALSE;
}

BOOL CEasyLayer::SetSubLayerOrder(LPUNKNOWN sublayer, long neworder) 
{
    if (!sublayer || !m_pLayer.Assigned())
    	return FALSE;

    IEasyLayer* pEasyLayer = NULL;
    sublayer->QueryInterface(IID_IEasyLayer, (void**)&pEasyLayer);
    if (!pEasyLayer)
        return FALSE;
    sublayer->Release();

    easymap::CGroupLayerPtr pGL;
    CAST_PTR(m_pLayer, pGL, CGroupLayer)
    if (!pGL.Assigned())
        return FALSE;

    easymap::ILayerPtr pSubLayer = (easymap::ILayer*)pEasyLayer->innergetrawlayerptr(FALSE);
    return pGL->SetLayerOrder(pSubLayer._p(), neworder) ? TRUE:FALSE;
}

long CEasyLayer::GetFeatureLayerType() 
{
    EasyLayerType r = EASYLAYERTYPE_UNKNOWN;
    if (!m_pLayer.Assigned())
    	return EASYLAYERTYPE_UNKNOWN;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return EASYLAYERTYPE_UNKNOWN;

    easymap::GeometryColumnInfo colinfo;
    pVL->GetGeometryColumnInfo(colinfo);
    switch (colinfo.FeatureType)
    {
    case easymap::VECTORFEATURETYPE_TEXT:
        return EASYFEATURELAYERTYPE_ANNOTATION;
        break;

    case easymap::VECTORFEATURETYPE_GEOMETRY:
        break;

    default:
        return EASYLAYERTYPE_UNKNOWN;
    }

    switch (colinfo.ShpType)
    {
    case easymap::SHAPETYPE_POINT:
        return EASYFEATURELAYERTYPE_POINT;
        break;

    case easymap::SHAPETYPE_MULTIPOINT:
        return EASYFEATURELAYERTYPE_MULTIPOINT;
        break;

    case easymap::SHAPETYPE_POLYLINE:
        return EASYFEATURELAYERTYPE_POLYLINE;
        break;

    case easymap::SHAPETYPE_POLYGON:
        return EASYFEATURELAYERTYPE_POLYGON;
        break;

    default:
        return EASYLAYERTYPE_UNKNOWN;
    }
}

BOOL CEasyLayer::GetFeatureLayerInfo(double FAR* basescale, long FAR* mapunit, double FAR* precision) 
{
    EasyLayerType r = EASYLAYERTYPE_UNKNOWN;
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return FALSE;

    easymap::GeometryColumnInfo colinfo;
    pVL->GetGeometryColumnInfo(colinfo);
    *basescale = colinfo.BaseScale;
    *mapunit = colinfo.MapUnit;
    *precision = colinfo.ToleranceXY;

	return TRUE;
}

BOOL CEasyLayer::ShowRendererUI() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return FALSE;

    easymap::IVectorLayerAgentPtr pVLA;
    EASYLIB_CREATEOBJ(CVectorLayerAgent, pVLA, IVectorLayerAgent)
    pVLA->SetLayer(m_pLayer._p());

    if (!easymap::rendereruidll::getDLLHandle())
    {
//        ::MessageBox((HWND)mainwnd, "RendererUI is not found.", "Oops!", MB_OK);
        return FALSE;
    }

    bool r = easymap::rendereruidll::SelectRenderer(pVLA._p());
    if (!r)
    {
        return FALSE;
    }

	return TRUE;
}

BOOL CEasyLayer::SetDefaultSymbolFromLib(LPCTSTR symbollibfile, long symbolindex) 
{
    if (!symbollibfile || (symbolindex < 0))
        return FALSE;

    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return FALSE;

    easymap::CSymbolLibPtr pSymbolLib;
    EASYLIB_CREATEOBJ(CSymbolLib, pSymbolLib, CSymbolLib)
    if (!pSymbolLib->LoadFromFile(symbollibfile))
        return FALSE;

    long symbolcount = pSymbolLib->GetSymbolCount();
    if (symbolindex >= symbolcount)
        return FALSE;

    easymap::ISymbolPtr pSymbol;
    pSymbolLib->GetSymbolRef(pSymbol._ref(), symbolindex);

    return pVL->SetDefaultSymbol((easymap::ISymbol*)pSymbol._p(), true) ? TRUE:FALSE;
}

BOOL CEasyLayer::SetDefaultSymbolFromUI() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return FALSE;

    if (!easymap::symboluidll::getDLLHandle())
    {
//        ::MessageBox((HWND)mainwnd, "SymbolUI is not found.", "Oops!", MB_OK);
        return FALSE;
    }

    easymap::ISymbolPtr pSymbol;
    pVL->GetDefaultSymbol(pSymbol);

    bool r = easymap::symboluidll::SelectSymbol((easymap::ISymbol**)pSymbol._ref());
    if (!r)
    {
        return FALSE;
    }

    return pVL->SetDefaultSymbol(pSymbol._p(), true) ? TRUE:FALSE;
}

BOOL CEasyLayer::SetDefaultSymbolParam(LPUNKNOWN param) 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return FALSE;

    ILayerDisplayParams* pParam = NULL;
    param->QueryInterface(IID_ILayerDisplayParams, (void**)&pParam);
    if (!pParam)
        return FALSE;
    pParam->Release();

    easymap::ISymbolPtr pSymbol;
    pVL->GetDefaultSymbol(pSymbol);
    pSymbol->SetColor(pParam->GetColor());

    easymap::IPointSymbolPtr pPointSymbol;
    CAST_PTR(pSymbol, pPointSymbol, IPointSymbol)
    if (pPointSymbol.Assigned())
    {
        pPointSymbol->SetAngle(pParam->GetPointAngle());
    }

    easymap::IMultiPointSymbolPtr pMultiPointSymbol;
    CAST_PTR(pSymbol, pMultiPointSymbol, IMultiPointSymbol)
    if (pMultiPointSymbol.Assigned())
    {
        pMultiPointSymbol->SetSize(pParam->GetPointSize());
    }

    easymap::IMultiLineSymbolPtr pMultiLineSymbol;
    CAST_PTR(pSymbol, pMultiLineSymbol, IMultiLineSymbol)
    if (pMultiLineSymbol.Assigned())
    {
        pMultiLineSymbol->SetSize(pParam->GetLineWidth());
    }

    easymap::IMultiFillSymbolPtr pMultiFillSymbol;
    CAST_PTR(pSymbol, pMultiFillSymbol, IMultiFillSymbol)
    if (pMultiFillSymbol.Assigned() && (pMultiFillSymbol->GetSymbolCount()>0))
    {
        easymap::IFillSymbolPtr pFillSymbol;
        pMultiFillSymbol->GetSymbolRef(pFillSymbol._ref(), 0);
        easymap::ISimpleFillSymbolPtr pSimpleFillSymbol;
        CAST_PTR(pFillSymbol, pSimpleFillSymbol, ISimpleFillSymbol)
        if (pSimpleFillSymbol.Assigned())
        {
            pSimpleFillSymbol->SetBorderColor(pParam->GetOuterLineColor());
            pSimpleFillSymbol->SetBorderWidth(pParam->GetOuterLineWidth());
        }
    }

    easymap::ISimpleTextSymbolPtr pTextSymbol;
    CAST_PTR(pSymbol, pTextSymbol, ISimpleTextSymbol)
    if (pTextSymbol.Assigned())
    {
        pTextSymbol->SetWidth(pParam->GetFontWidth());
        pTextSymbol->SetHeight(pParam->GetFontHeight());
    }

    pVL->SetDefaultSymbol(pSymbol, true);
	return TRUE;
}

LPUNKNOWN CEasyLayer::GetDefaultSymbolParam() 
{
    if (!m_pLayer.Assigned())
    	return NULL;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return NULL;

    easymap::ISymbolPtr pSymbol;
    pVL->GetDefaultSymbol(pSymbol);

	CRuntimeClass* pRTC = RUNTIME_CLASS(CLayerDisplayParams);
    CLayerDisplayParams* pParam = (class CLayerDisplayParams*)(pRTC->CreateObject());

    COLORREF color;
    pSymbol->GetColor(color);
    pParam->m_Color = color;

    easymap::IPointSymbolPtr pPointSymbol;
    CAST_PTR(pSymbol, pPointSymbol, IPointSymbol)
    if (pPointSymbol.Assigned())
    {
        pPointSymbol->GetAngle(pParam->m_PointAngle);
    }

    easymap::IMultiPointSymbolPtr pMultiPointSymbol;
    CAST_PTR(pSymbol, pMultiPointSymbol, IMultiPointSymbol)
    if (pMultiPointSymbol.Assigned())
    {
        pMultiPointSymbol->GetSize(pParam->m_PointSize);
    }

    easymap::IMultiLineSymbolPtr pMultiLineSymbol;
    CAST_PTR(pSymbol, pMultiLineSymbol, IMultiLineSymbol)
    if (pMultiLineSymbol.Assigned())
    {
        pMultiLineSymbol->GetSize(pParam->m_LineWidth);
    }

    easymap::IMultiFillSymbolPtr pMultiFillSymbol;
    CAST_PTR(pSymbol, pMultiFillSymbol, IMultiFillSymbol)
    if (pMultiFillSymbol.Assigned() && (pMultiFillSymbol->GetSymbolCount()>0))
    {
        easymap::IFillSymbolPtr pFillSymbol;
        pMultiFillSymbol->GetSymbolRef(pFillSymbol._ref(), 0);
        easymap::ISimpleFillSymbolPtr pSimpleFillSymbol;
        CAST_PTR(pFillSymbol, pSimpleFillSymbol, ISimpleFillSymbol)
        if (pSimpleFillSymbol.Assigned())
        {
            COLORREF bordercolor;
            pSimpleFillSymbol->GetBorderColor(bordercolor);
            pParam->m_OuterLineColor = bordercolor;
            pSimpleFillSymbol->GetBorderWidth(pParam->m_OuterLineWidth);
        }
    }

    easymap::ISimpleTextSymbolPtr pTextSymbol;
    CAST_PTR(pSymbol, pTextSymbol, ISimpleTextSymbol)
    if (pTextSymbol.Assigned())
    {
        pTextSymbol->GetWidth(pParam->m_FontWidth);
        pTextSymbol->GetHeight(pParam->m_FontHeight);
    }

    IDispatch* pDispatch = pParam->GetIDispatch(FALSE);
    LPUNKNOWN pOutParam = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pOutParam);
    pDispatch->Release();
    return pOutParam;
}

long CEasyLayer::GetAttribFieldCount() 
{
    if (!m_pLayer.Assigned())
    	return -1;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return -1;

    easymap::CFieldsPtr pFields;
    pVL->GetFields(pFields);
    return pFields->GetFieldCount();
}

long CEasyLayer::GetAttribFieldType(long fieldindex) 
{
    if (!m_pLayer.Assigned() || (fieldindex < 0))
    	return EASYFIELDTYPE_UNKNOWN;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return EASYFIELDTYPE_UNKNOWN;

    easymap::CFieldsPtr pFields;
    pVL->GetFields(pFields);
    long fieldcount = pFields->GetFieldCount();
    if (fieldindex >= fieldcount)
        return EASYFIELDTYPE_UNKNOWN;

    easymap::IFieldsPtr pFields1;
    CAST_PTR(pFields, pFields1, IFields)
    easymap::IFieldPtr pField;
    pFields1->GetField(fieldindex, pField._ref());
    return pField->GetFieldType();
}

BSTR CEasyLayer::GetAttribFieldName(long fieldindex) 
{
	CString strResult;

    if (!m_pLayer.Assigned() || (fieldindex < 0))
    	return strResult.AllocSysString();

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return strResult.AllocSysString();

    easymap::CFieldsPtr pFields;
    pVL->GetFields(pFields);
    long fieldcount = pFields->GetFieldCount();
    if (fieldindex >= fieldcount)
        return strResult.AllocSysString();

    easymap::IFieldsPtr pFields1;
    CAST_PTR(pFields, pFields1, IFields)
    easymap::IFieldPtr pField;
    pFields1->GetField(fieldindex, pField._ref());
    strResult = pField->GetFieldName();
	return strResult.AllocSysString();
}

LPUNKNOWN CEasyLayer::GetSelection() 
{
    if (!m_pLayer.Assigned())
    	return NULL;
    if ((this->GetLayerType() != EASYLAYERTYPE_SLIMLAYER)
        && (this->GetLayerType() != EASYLAYERTYPE_SHAPELAYER)
        && (this->GetLayerType() != EASYLAYERTYPE_ELEMENTLAYER))
        return NULL;

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyIntArray);
    CEasyIntArray* pEasyIntArray = (class CEasyIntArray*)(pRTC->CreateObject());
    vector<DWORD> selection;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        easymap::IVectorLayerAgentPtr pVLA;
        EASYLIB_CREATEOBJ(CVectorLayerAgent, pVLA, IVectorLayerAgent)
        pVLA->SetLayer(m_pLayer._p());
        easymap::IIntArrayPtr pSelection;
        pVLA->GetSelection(pSelection._ref());
        for (DWORD i = 0; i < pSelection->GetSize(); i++)
        {
            long item;
            pSelection->GetAt(i, item);
            selection.push_back(item);
        }
    }

    easymap::CElementLayerPtr pEL;
    CAST_PTR(m_pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        easymap::IElementLayerAgentPtr pELA;
        EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
        pELA->SetLayer(m_pLayer._p());
        easymap::IIntArrayPtr pSelection;
        pELA->GetSelectElements(pSelection._ref());
        for (DWORD i = 0; i < pSelection->GetSize(); i++)
        {
            long item;
            pSelection->GetAt(i, item);
            selection.push_back(item);
        }
    }

    vector<DWORD>::const_iterator it = selection.begin();
    while (it != selection.end())
    {
        pEasyIntArray->m_FIDs.push_back(*it);
        it++;
    }

    IDispatch* pDispatch = pEasyIntArray->GetIDispatch(FALSE);
    LPUNKNOWN pResult = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pResult);
    pDispatch->Release();
    return pResult;
}

LPUNKNOWN CEasyLayer::GetFeature(long fid) 
{
    if (!m_pLayer.Assigned())
    	return NULL;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return NULL;

    easymap::IVectorFeaturePtr pVF;
    pVL->GetFeature(fid, pVF);
    if (!pVF.Assigned())
        return NULL;

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyFeature);
    CEasyFeature* pEasyFeature = (class CEasyFeature*)(pRTC->CreateObject());

    pEasyFeature->m_pFeature = pVF;
    IDispatch* pDispatchLayer = this->GetIDispatch(FALSE);
    pDispatchLayer->QueryInterface(IID_IEasyLayer, (void**)&(pEasyFeature->m_pEasyLayer));

    IDispatch* pDispatch = pEasyFeature->GetIDispatch(FALSE);
    LPUNKNOWN pResult = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pResult);
    pDispatch->Release();
    return pResult;
}

LPUNKNOWN CEasyLayer::CreateFeature() 
{
    if (!m_pLayer.Assigned())
    	return NULL;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return NULL;

    easymap::IVectorFeaturePtr pVF;
    pVL->CreateFeature(pVF);
    if (!pVF.Assigned())
        return NULL;

    CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyFeature);
    CEasyFeature* pEasyFeature = (class CEasyFeature*)(pRTC->CreateObject());

    pEasyFeature->m_pFeature = pVF;
    IDispatch* pDispatchLayer = this->GetIDispatch(FALSE);
    pDispatchLayer->QueryInterface(IID_IEasyLayer, (void**)&(pEasyFeature->m_pEasyLayer));

    IDispatch* pDispatch = pEasyFeature->GetIDispatch(FALSE);
    LPUNKNOWN pResult = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pResult);
    pDispatch->Release();
    return pResult;
}

void CEasyLayer::DeleteSelectedObjects() 
{
    if (!m_pLayer.Assigned())
    	return;

    IUnknown* pU = NULL;
    long i;
    if (this->GetLayerType() == EASYLAYERTYPE_GROUPLAYER)
    {
        long layercount = this->GetSubLayerCount();
        for (i = 0; i < layercount; i++)
        {
            pU = this->GetSubLayer(i);
            IEasyLayer* pSubLayer = NULL;
            pU->QueryInterface(IID_IEasyLayer, (void**)&pSubLayer);
            pSubLayer->DeleteSelectedObjects();
            pSubLayer->Release();
            pU->Release();
        }

        return;
    }

    pU = this->GetSelection();
    if (!pU)
        return;

    IEasyIntArray* pFids = NULL;
    pU->QueryInterface(IID_IEasyIntArray, (void**)&pFids);
    pU->Release(); pU = NULL;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        for (i = 0; i < pFids->GetCount(); i++)
        {
            long fid;
            pFids->GetAt(i, &fid);
            pU = this->GetFeature(fid);
            IEasyFeature* pEasyFeature = NULL;
            pU->QueryInterface(IID_IEasyFeature, (void**)&pEasyFeature);
            pU->Release();
            pEasyFeature->Delete();
            pEasyFeature->Release();
        }
    }

    easymap::CElementLayerPtr pEL;
    CAST_PTR(m_pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        easymap::IElementLayerAgentPtr pELA;
        EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
        pELA->SetLayer(m_pLayer._p());
        pELA->RemoveSelectedElements();
    }

    pFids->Release();
}

void CEasyLayer::MoveSelectedObject(double deltax, double deltay) 
{
    if (!m_pLayer.Assigned())
    	return;

    IUnknown* pU = NULL;
    long i;
    if (this->GetLayerType() == EASYLAYERTYPE_GROUPLAYER)
    {
        long layercount = this->GetSubLayerCount();
        for (i = 0; i < layercount; i++)
        {
            pU = this->GetSubLayer(i);
            IEasyLayer* pSubLayer = NULL;
            pU->QueryInterface(IID_IEasyLayer, (void**)&pSubLayer);
            pSubLayer->MoveSelectedObject(deltax, deltay);
            pSubLayer->Release();
            pU->Release();
        }

        return;
    }

    pU = this->GetSelection();
    if (!pU)
        return;

    IEasyIntArray* pFids = NULL;
    pU->QueryInterface(IID_IEasyIntArray, (void**)&pFids);
    pU->Release(); pU = NULL;

    easymap::IGeometryPtr pGeometry;
    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        for (i = 0; i < pFids->GetCount(); i++)
        {
            long fid;
            pFids->GetAt(i, &fid);
            easymap::IVectorFeaturePtr pVF;
            pVL->GetFeature(fid, pVF);
            pGeometry.Clear();
            pVF->GetGeometryRef(pGeometry._ref());
            pGeometry->Move(deltax, deltay);
            pVF->Update();
        }
    }

    easymap::CElementLayerPtr pEL;
    CAST_PTR(m_pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        for (i = 0; i < pFids->GetCount(); i++)
        {
            long eid;
            pFids->GetAt(i, &eid);
            easymap::IElementPtr pElement;

            easymap::IElementLayerAgentPtr pELA;
            EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
            pELA->SetLayer(m_pLayer._p());
            pELA->GetElement(eid, pElement._ref());
            pGeometry.Clear();
            pElement->GetGeometry(pGeometry._ref());
            pGeometry->Move(deltax, deltay);
            pElement->SetGeometry(pGeometry._p());

            pELA->SetElement(eid, pElement._p());
        }
    }

    pFids->Release();
}

long CEasyLayer::AddGeometryElement(LPUNKNOWN geometry) 
{
    if (!m_pLayer.Assigned() || !geometry)
    	return -1;

    easymap::CElementLayerPtr pEL;
    CAST_PTR(m_pLayer, pEL, CElementLayer)
    if (!pEL.Assigned())
        return -1;

    easymap::IGeometryPtr pGeometry;
    Easy2Geometry(geometry, pGeometry);
    if (!pGeometry.Assigned())
        return -1;

    easymap::IElementPtr pElement;
    EASYLIB_CREATEOBJ(CGeometryElement, pElement, IElement)
    pElement->SetGeometry(pGeometry._p());

    easymap::IElementLayerAgentPtr pELA;
    EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
    pELA->SetLayer(m_pLayer._p());
    return pELA->AddElement(pElement._p());
}

long CEasyLayer::AddTextElement(double point_x, double point_y, LPCTSTR text) 
{
    if (!m_pLayer.Assigned() || !text)
    	return -1;

    easymap::CElementLayerPtr pEL;
    CAST_PTR(m_pLayer, pEL, CElementLayer)
    if (!pEL.Assigned())
        return -1;

    easymap::IPointPtr pPoint;
    EASYLIB_CREATEOBJ(CPoint, pPoint, IPoint)
    pPoint->SetX(point_x);
    pPoint->SetY(point_y);
    easymap::IGeometryPtr pGeometry;
    CAST_PTR(pPoint, pGeometry, IGeometry)

    easymap::CTextElementPtr pTextElement;
    EASYLIB_CREATEOBJ(CTextElement, pTextElement, CTextElement)
    easymap::IElementPtr pElement;
    CAST_PTR(pTextElement, pElement, IElement)
    pElement->SetGeometry(pGeometry._p());

    easymap::IElementLayerAgentPtr pELA;
    EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
    pELA->SetLayer(m_pLayer._p());
    return pELA->AddElement(pElement._p());
}


LPUNKNOWN CEasyLayer::GetElement(long id, BSTR FAR* text, BOOL FAR* istext) 
{
    if (!m_pLayer.Assigned())
    	return NULL;

    easymap::CElementLayerPtr pEL;
    CAST_PTR(m_pLayer, pEL, CElementLayer)
    if (!pEL.Assigned())
        return NULL;

    easymap::IElementLayerAgentPtr pELA;
    EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
    pELA->SetLayer(m_pLayer._p());

    easymap::IElementPtr pElement;
    pELA->GetElement(id, pElement._ref());
    if (!pElement.Assigned())
        return NULL;

    if (text)
    {
        CString cs = pElement->GetText();
	    *text = cs.AllocSysString();
    }

    if (istext)
    {
        easymap::ElementType elementtype = pElement->GetElementType();
        if (easymap::ELEMENTTYPE_TEXT == elementtype)
        {
            *istext = TRUE;
        }
        else
        {
            *istext = FALSE;
        }
    }

    easymap::IGeometryPtr pGeometry;
    pElement->GetGeometry(pGeometry._ref());
    IUnknown* pUnknown = NULL;
    Geometry2Easy(pGeometry, &pUnknown);
	return pUnknown;
}

BOOL CEasyLayer::SetElement(long id, LPUNKNOWN geometry, LPCTSTR text) 
{
    if (!m_pLayer.Assigned() || !geometry)
    	return FALSE;

    easymap::CElementLayerPtr pEL;
    CAST_PTR(m_pLayer, pEL, CElementLayer)
    if (!pEL.Assigned())
        return FALSE;

    easymap::IGeometryPtr pGeometry;
    Easy2Geometry(geometry, pGeometry);
    if (!pGeometry.Assigned())
        return FALSE;

    easymap::IElementLayerAgentPtr pELA;
    EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
    pELA->SetLayer(m_pLayer._p());

    easymap::IElementPtr pElement;
    pELA->GetElement(id, pElement._ref());
    if (!pElement.Assigned())
        return FALSE;

    pElement->SetGeometry(pGeometry._p());

    if (text)
    {
        pElement->SetText(text);
    }

    pELA->SetElement(id, pElement._p());

	return TRUE;
}

BOOL CEasyLayer::RemoveElement(long id) 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::CElementLayerPtr pEL;
    CAST_PTR(m_pLayer, pEL, CElementLayer)
    if (!pEL.Assigned())
        return FALSE;

    easymap::IElementLayerAgentPtr pELA;
    EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
    pELA->SetLayer(m_pLayer._p());
    return pELA->RemoveElement(id) ? TRUE:FALSE;
}

BOOL CEasyLayer::ModifyGPSPoint(long fid, double x, double y) 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
        return FALSE;

    easymap::GeometryColumnInfo colinfo;
    pVL->GetGeometryColumnInfo(colinfo);
    if (colinfo.ShpType != easymap::SHAPETYPE_POINT)
        return FALSE;

    return pVL->RapidModifyPoint(fid, easymap::WKSPoint(x, y));
}

LPUNKNOWN CEasyLayer::Identify(double minx, double miny, double maxx, double maxy, BOOL partialselect) 
{
    if (!m_pLayer.Assigned())
    	return NULL;
    if ((this->GetLayerType() != EASYLAYERTYPE_SLIMLAYER)
        && (this->GetLayerType() != EASYLAYERTYPE_SHAPELAYER)
        && (this->GetLayerType() != EASYLAYERTYPE_ELEMENTLAYER))
        return NULL;

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyIntArray);
    CEasyIntArray* pEasyIntArray = (class CEasyIntArray*)(pRTC->CreateObject());
    vector<DWORD> selection;

    easymap::WKSRect envelope;
    envelope.left = minx;
    envelope.bottom = miny;
    envelope.right = maxx;
    envelope.top = maxy;
    bool partial = partialselect ? TRUE:FALSE;

    easymap::CVectorLayerPtr pVL;
    CAST_PTR(m_pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        pVL->Identify(selection, envelope, partial);
    }

    easymap::CElementLayerPtr pEL;
    CAST_PTR(m_pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        easymap::IElementLayerAgentPtr pELA;
        EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
        pELA->SetLayer(m_pLayer._p());

        easymap::IIntArrayPtr pSelection;
        pELA->Identify(pSelection._ref(), envelope, partial);
        for (DWORD i = 0; i < pSelection->GetSize(); i++)
        {
            long item;
            pSelection->GetAt(i, item);
            selection.push_back(item);
        }
    }

    vector<DWORD>::const_iterator it = selection.begin();
    while (it != selection.end())
    {
        pEasyIntArray->m_FIDs.push_back(*it);
        it++;
    }

    IDispatch* pDispatch = pEasyIntArray->GetIDispatch(FALSE);
    LPUNKNOWN pResult = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pResult);
    pDispatch->Release();
    return pResult;
}

void CEasyLayer::SelectAll() 
{
    if (!m_pLayer.Assigned())
        return;

    easymap::IVectorLayerAgentPtr pVLA;
    EASYLIB_CREATEOBJ(CVectorLayerAgent, pVLA, IVectorLayerAgent)
    pVLA->SetLayer(m_pLayer._p());
    if (!pVLA.Assigned())
        return;

    easymap::IIntArrayPtr pFids;
    pVLA->GetFids(pFids._ref());
    pVLA->Select(pFids._p(), false);
}

LPUNKNOWN CEasyLayer::FindSubLayer(LPCTSTR layername) 
{
    if (!m_pLayer.Assigned())
    	return NULL;

    easymap::IGroupLayerPtr pGL;
    CAST_PTR(m_pLayer, pGL, IGroupLayer)
    if (!pGL.Assigned())
        return NULL;

    easymap::ILayerPtr pLayer;
    pGL->FindLayer(pLayer._ref(), layername, NULL);

    if (!pLayer.Assigned())
        return NULL;

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyLayer);
    CEasyLayer* pEasyLayer = (class CEasyLayer*)(pRTC->CreateObject());
    pEasyLayer->m_pLayer = pLayer;

    IDispatch* pDispatch = pEasyLayer->GetIDispatch(FALSE);
    LPUNKNOWN pOutLayer = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pOutLayer);
    pDispatch->Release();
    return pOutLayer;
}

BOOL CEasyLayer::SetMapEditable(BOOL editable) 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::IEditLayerPtr pEditLayer;
    CAST_PTR(m_pLayer, pEditLayer, IEditLayer)
    if (!pEditLayer.Assigned())
    	return FALSE;

    return pEditLayer->SetMapEditable(editable == TRUE);
}

BOOL CEasyLayer::GetMapEditable() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::IEditLayerPtr pEditLayer;
    CAST_PTR(m_pLayer, pEditLayer, IEditLayer)
    if (!pEditLayer.Assigned())
    	return FALSE;

    return pEditLayer->GetMapEditable();
}

BOOL CEasyLayer::SaveEdit() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::IEditLayerPtr pEditLayer;
    CAST_PTR(m_pLayer, pEditLayer, IEditLayer)
    if (!pEditLayer.Assigned())
    	return FALSE;

    return pEditLayer->SaveData();
}

BOOL CEasyLayer::DiscardEdit() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::IEditLayerPtr pEditLayer;
    CAST_PTR(m_pLayer, pEditLayer, IEditLayer)
    if (!pEditLayer.Assigned())
    	return FALSE;

    return pEditLayer->EditCancel();
}

BOOL CEasyLayer::SetUndoPoint() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::IEditLayerPtr pEditLayer;
    CAST_PTR(m_pLayer, pEditLayer, IEditLayer)
    if (!pEditLayer.Assigned())
    	return FALSE;

    return pEditLayer->SetUndoPoint();
}

BOOL CEasyLayer::Undo() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::IEditLayerPtr pEditLayer;
    CAST_PTR(m_pLayer, pEditLayer, IEditLayer)
    if (!pEditLayer.Assigned())
    	return FALSE;

    return pEditLayer->EditUndo();
}

BOOL CEasyLayer::Redo() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::IEditLayerPtr pEditLayer;
    CAST_PTR(m_pLayer, pEditLayer, IEditLayer)
    if (!pEditLayer.Assigned())
    	return FALSE;

    return pEditLayer->EditRedo();
}

BOOL CEasyLayer::Undoable() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::IEditLayerPtr pEditLayer;
    CAST_PTR(m_pLayer, pEditLayer, IEditLayer)
    if (!pEditLayer.Assigned())
    	return FALSE;

    return pEditLayer->EditUndoable();
}

BOOL CEasyLayer::Redoable() 
{
    if (!m_pLayer.Assigned())
    	return FALSE;

    easymap::IEditLayerPtr pEditLayer;
    CAST_PTR(m_pLayer, pEditLayer, IEditLayer)
    if (!pEditLayer.Assigned())
    	return FALSE;

    return pEditLayer->EditRedoable();
}


/////////////////////////////////////////////////////////////////////////////
// CLayerDisplayParams

IMPLEMENT_DYNCREATE(CLayerDisplayParams, CCmdTarget)

CLayerDisplayParams::CLayerDisplayParams()
{
	EnableAutomation();
}

CLayerDisplayParams::~CLayerDisplayParams()
{
}


void CLayerDisplayParams::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CLayerDisplayParams, CCmdTarget)
	//{{AFX_MSG_MAP(CLayerDisplayParams)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CLayerDisplayParams, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CLayerDisplayParams)
	DISP_PROPERTY_NOTIFY(CLayerDisplayParams, "Color", m_Color, OnColorChanged, VT_I4)
	DISP_PROPERTY_NOTIFY(CLayerDisplayParams, "OuterLineColor", m_OuterLineColor, OnOuterLineColorChanged, VT_I4)
	DISP_PROPERTY_NOTIFY(CLayerDisplayParams, "OuterLineWidth", m_OuterLineWidth, OnOuterLineWidthChanged, VT_R8)
	DISP_PROPERTY_NOTIFY(CLayerDisplayParams, "FontWidth", m_FontWidth, OnFontWidthChanged, VT_R8)
	DISP_PROPERTY_NOTIFY(CLayerDisplayParams, "FontHeight", m_FontHeight, OnFontHeightChanged, VT_R8)
	DISP_PROPERTY_NOTIFY(CLayerDisplayParams, "PointAngle", m_PointAngle, OnPointAngleChanged, VT_R8)
	DISP_PROPERTY_NOTIFY(CLayerDisplayParams, "PointSize", m_PointSize, OnPointSizeChanged, VT_R8)
	DISP_PROPERTY_NOTIFY(CLayerDisplayParams, "LineWidth", m_LineWidth, OnLineWidthChanged, VT_R8)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CLayerDisplayParams, CCmdTarget)
	INTERFACE_PART(CLayerDisplayParams, IID_ILayerDisplayParams, Dispatch)
END_INTERFACE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLayerDisplayParams message handlers

void CLayerDisplayParams::OnColorChanged() 
{
	// TODO: Add notification handler code

}

void CLayerDisplayParams::OnOuterLineColorChanged() 
{
	// TODO: Add notification handler code

}

void CLayerDisplayParams::OnOuterLineWidthChanged() 
{
	// TODO: Add notification handler code

}

void CLayerDisplayParams::OnFontWidthChanged() 
{
	// TODO: Add notification handler code

}

void CLayerDisplayParams::OnFontHeightChanged() 
{
	// TODO: Add notification handler code

}

void CLayerDisplayParams::OnPointAngleChanged() 
{
	// TODO: Add notification handler code

}

void CLayerDisplayParams::OnPointSizeChanged() 
{
	// TODO: Add notification handler code

}

void CLayerDisplayParams::OnLineWidthChanged() 
{
	// TODO: Add notification handler code

}


/////////////////////////////////////////////////////////////////////////////
// CEasyFeature

IMPLEMENT_DYNCREATE(CEasyFeature, CCmdTarget)

CEasyFeature::CEasyFeature()
{
	EnableAutomation();
    m_pEasyLayer = NULL;
}

CEasyFeature::~CEasyFeature()
{
    if (m_pEasyLayer)
    {
        m_pEasyLayer->Release();
    }
}


void CEasyFeature::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CEasyFeature, CCmdTarget)
	//{{AFX_MSG_MAP(CEasyFeature)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CEasyFeature, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CEasyFeature)
	DISP_FUNCTION(CEasyFeature, "SetGeometry", SetGeometry, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyFeature, "GetGeometry", GetGeometry, VT_UNKNOWN, VTS_NONE)
	DISP_FUNCTION(CEasyFeature, "GetFid", GetFid, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyFeature, "GetMBR", GetMBR, VT_UNKNOWN, VTS_NONE)
	DISP_FUNCTION(CEasyFeature, "GetLayer", GetLayer, VT_UNKNOWN, VTS_NONE)
	DISP_FUNCTION(CEasyFeature, "GetFieldCount", GetFieldCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyFeature, "SetFieldValue", SetFieldValue, VT_BOOL, VTS_I4 VTS_BSTR)
	DISP_FUNCTION(CEasyFeature, "GetFieldValue", GetFieldValue, VT_BSTR, VTS_I4)
	DISP_FUNCTION(CEasyFeature, "SetAnnotation", SetAnnotation, VT_BOOL, VTS_BSTR)
	DISP_FUNCTION(CEasyFeature, "GetAnnotation", GetAnnotation, VT_BSTR, VTS_NONE)
	DISP_FUNCTION(CEasyFeature, "Delete", Delete, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyFeature, "Update", Update, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyFeature, "innergetrawfeatureptr", innergetrawfeatureptr, VT_I4, VTS_BOOL)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CEasyFeature, CCmdTarget)
	INTERFACE_PART(CEasyFeature, IID_IEasyFeature, Dispatch)
END_INTERFACE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEasyFeature message handlers

BOOL CEasyFeature::SetGeometry(LPUNKNOWN geometry) 
{
    if (!geometry)
        return FALSE;

    easymap::IGeometryPtr pGeometry;
    Easy2Geometry(geometry, pGeometry);
    if (!pGeometry.Assigned())
        return FALSE;

    m_pFeature->SetGeometryRef(pGeometry._p());
	return TRUE;
}

LPUNKNOWN CEasyFeature::GetGeometry() 
{
    easymap::IGeometryPtr pGeometry;
    m_pFeature->GetGeometryRef(pGeometry._ref());
    if (!pGeometry.Assigned())
        return NULL;

    IUnknown* pResult = NULL;
    Geometry2Easy(pGeometry, &pResult);
    return pResult;
}

long CEasyFeature::GetFid() 
{
    return m_pFeature->GetFID();
}

LPUNKNOWN CEasyFeature::GetMBR() 
{
    easymap::IGeometryPtr pGeometry;
    m_pFeature->GetGeometryRef(pGeometry._ref());
    if (!pGeometry.Assigned())
        return NULL;

    easymap::WKSRect mbr;
    pGeometry->GetMBR(mbr);

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyEnvelope);
    CEasyEnvelope* pEasyEnvelope = (class CEasyEnvelope*)(pRTC->CreateObject());

    EASYLIB_CREATEOBJ(CEnvelope, pEasyEnvelope->m_pEnvelope, CEnvelope)
    pEasyEnvelope->m_pEnvelope->SetMinX(mbr.left);
    pEasyEnvelope->m_pEnvelope->SetMinY(mbr.bottom);
    pEasyEnvelope->m_pEnvelope->SetMaxX(mbr.right);
    pEasyEnvelope->m_pEnvelope->SetMaxY(mbr.top);

    IUnknown* pResult = NULL;
    IDispatch* pDispatch = pEasyEnvelope->GetIDispatch(FALSE);
    pDispatch->QueryInterface(IID_IUnknown, (void**)pResult);
    pDispatch->Release();

	return pResult;
}

LPUNKNOWN CEasyFeature::GetLayer() 
{
    if (!m_pEasyLayer)
        return NULL;

    IUnknown* pResult = NULL;
    m_pEasyLayer->QueryInterface(IID_IUnknown, (void**)&pResult);
	return pResult;
}

long CEasyFeature::GetFieldCount() 
{
    return m_pFeature->GetFieldCount();
}

BOOL CEasyFeature::SetFieldValue(long fieldindex, LPCTSTR fieldvalue) 
{
    long fieldcount = m_pFeature->GetFieldCount();
    if (!fieldvalue || (fieldindex < 0) || (fieldindex >= fieldcount))
        return FALSE;

    CString cs = fieldvalue;
    return m_pFeature->SetFieldValue(fieldindex, cs) ? TRUE:FALSE;
}

BSTR CEasyFeature::GetFieldValue(long fieldindex) 
{
    long fieldcount = m_pFeature->GetFieldCount();
    if ((fieldindex < 0) || (fieldindex >= fieldcount))
        return NULL;

    easymap::IFieldValuePtr pFieldValue;
    m_pFeature->GetFieldValue(fieldindex, pFieldValue._ref());
    easymap::IAnsiStringPtr pAS;
    pFieldValue->ToString(pAS._ref());
    if (!pAS.Assigned())
        return NULL;

    CString strResult = pAS->GetText();
    return strResult.AllocSysString();
}

BOOL CEasyFeature::SetAnnotation(LPCTSTR annotation) 
{
    CString cs = annotation;
    m_pFeature->SetAnnotation(cs);
	return TRUE;
}

BSTR CEasyFeature::GetAnnotation() 
{
	CString strResult = m_pFeature->GetAnnotation();
	return strResult.AllocSysString();
}

BOOL CEasyFeature::Delete() 
{
    if (!m_pFeature.Assigned())
        return FALSE;
    BOOL r = m_pFeature->Delete() ? TRUE:FALSE;
    m_pFeature.Clear();
    return r;
}

BOOL CEasyFeature::Update() 
{
    return m_pFeature->Update() ? TRUE:FALSE;
}

long CEasyFeature::innergetrawfeatureptr(BOOL addref) 
{
    if (!m_pFeature.Assigned()) return 0;
    if (addref) m_pFeature->_AddRef();
	return (long)(m_pFeature._p());
}


/////////////////////////////////////////////////////////////////////////////
// CEasyIntArray

IMPLEMENT_DYNCREATE(CEasyIntArray, CCmdTarget)

CEasyIntArray::CEasyIntArray()
{
	EnableAutomation();
	
	// To keep the application running as long as an OLE automation 
	//	object is active, the constructor calls AfxOleLockApp.
	
	AfxOleLockApp();
}

CEasyIntArray::~CEasyIntArray()
{
	// To terminate the application when all objects created with
	// 	with OLE automation, the destructor calls AfxOleUnlockApp.
	
	AfxOleUnlockApp();
}


void CEasyIntArray::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}


BEGIN_MESSAGE_MAP(CEasyIntArray, CCmdTarget)
	//{{AFX_MSG_MAP(CEasyIntArray)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CEasyIntArray, CCmdTarget)
	//{{AFX_DISPATCH_MAP(CEasyIntArray)
	DISP_FUNCTION(CEasyIntArray, "Add", Add, VT_I4, VTS_I4)
	DISP_FUNCTION(CEasyIntArray, "Remove", Remove, VT_BOOL, VTS_I4)
	DISP_FUNCTION(CEasyIntArray, "GetCount", GetCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyIntArray, "Clear", Clear, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyIntArray, "GetAt", GetAt, VT_BOOL, VTS_I4 VTS_PI4)
	DISP_FUNCTION(CEasyIntArray, "SetAt", SetAt, VT_BOOL, VTS_I4 VTS_I4)
	//}}AFX_DISPATCH_MAP
END_DISPATCH_MAP()

BEGIN_INTERFACE_MAP(CEasyIntArray, CCmdTarget)
	INTERFACE_PART(CEasyIntArray, IID_IEasyIntArray, Dispatch)
END_INTERFACE_MAP()

// {F98CE28A-524F-4239-B52C-844956725243}
IMPLEMENT_OLECREATE(CEasyIntArray, "EasyControl.EasyIntArray", 0xf98ce28a, 0x524f, 0x4239, 0xb5, 0x2c, 0x84, 0x49, 0x56, 0x72, 0x52, 0x43)

/////////////////////////////////////////////////////////////////////////////
// CEasyIntArray message handlers

long CEasyIntArray::Add(long newvalue) 
{
    m_FIDs.push_back(newvalue);
	return m_FIDs.size()-1;
}

BOOL CEasyIntArray::Remove(long index) 
{
    long count = m_FIDs.size();
    if ((index < 0) || (index >= count))
        return FALSE;

    vector<long>::iterator it = m_FIDs.begin();
    std::advance(it, index);
    m_FIDs.erase(it);
	return TRUE;
}

long CEasyIntArray::GetCount() 
{
	return m_FIDs.size();
}

void CEasyIntArray::Clear() 
{
    m_FIDs.clear();
}

BOOL CEasyIntArray::GetAt(long index, long FAR* value) 
{
    long count = m_FIDs.size();
    if ((index < 0) || (index >= count))
        return FALSE;

    *value = m_FIDs[index];
	return TRUE;
}

BOOL CEasyIntArray::SetAt(long index, long newvalue) 
{
    long count = m_FIDs.size();
    if ((index < 0) || (index >= count))
        return FALSE;

    m_FIDs[index] = newvalue;
	return TRUE;
}
