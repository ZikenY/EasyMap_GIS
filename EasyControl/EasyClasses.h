#if !defined(AFX_EASYCLASSES_H__29BA35AB_11D2_44AB_9858_07C64EF2AFE5__INCLUDED_)
#define AFX_EASYCLASSES_H__29BA35AB_11D2_44AB_9858_07C64EF2AFE5__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// EasyClasses.h : header file
//

#pragma warning(push)
#pragma warning(disable : 4192)
#import "EasyControl.tlb" no_namespace
#pragma warning(pop)

#include "..\\easylib\\CustomEditLayer.h"

//通过类名称字符串创建对象，并传出特定的接口类型
#define EASYLIB_CREATEOBJ(class_name, ptr, interface_name)\
    ptr.Clear();\
    {\
        easymap::IObjPtr pObj;\
        easymap::_FactoryManager::CreateInstance(#class_name, pObj);\
        CAST_PTR(pObj, ptr, interface_name);\
    }

// {B2C65505-F2E7-4035-8156-A4266E57CEA1}
static const IID IID_IEasyEnvelope =
{ 0xb2c65505, 0xf2e7, 0x4035, { 0x81, 0x56, 0xa4, 0x26, 0x6e, 0x57, 0xce, 0xa1 } };

// {3EB46055-E4DA-46BD-9BBB-A156D25F64C9}
static const IID IID_IEasyPoint =
{ 0x3eb46055, 0xe4da, 0x46bd, { 0x9b, 0xbb, 0xa1, 0x56, 0xd2, 0x5f, 0x64, 0xc9 } };

// {74C11A15-B516-4F45-97BD-C866BE2EB1F6}
static const IID IID_IEasyPoints =
{ 0x74c11a15, 0xb516, 0x4f45, { 0x97, 0xbd, 0xc8, 0x66, 0xbe, 0x2e, 0xb1, 0xf6 } };

// {976E1897-AE8C-4219-B7BD-6B65D41A0D7F}
static const IID IID_IEasyPath =
{ 0x976e1897, 0xae8c, 0x4219, { 0xb7, 0xbd, 0x6b, 0x65, 0xd4, 0x1a, 0xd, 0x7f } };

// {9C5AC2F8-B7A3-47FA-83A8-D918D09EE103}
static const IID IID_IEasyRing =
{ 0x9c5ac2f8, 0xb7a3, 0x47fa, { 0x83, 0xa8, 0xd9, 0x18, 0xd0, 0x9e, 0xe1, 0x3 } };

// {C01C9BAC-AFE2-4236-ACE7-7074ABE05FBA}
static const IID IID_IEasyPolyline =
{ 0xc01c9bac, 0xafe2, 0x4236, { 0xac, 0xe7, 0x70, 0x74, 0xab, 0xe0, 0x5f, 0xba } };

// {A19D8A1A-1421-4DD7-AB8D-E70ADFBA8A5F}
static const IID IID_IEasyPolygon =
{ 0xa19d8a1a, 0x1421, 0x4dd7, { 0xab, 0x8d, 0xe7, 0xa, 0xdf, 0xba, 0x8a, 0x5f } };

// {2304DC29-87E7-47B8-82C9-EC4C4002FA64}
static const IID IID_IEasyLayer =
{ 0x2304dc29, 0x87e7, 0x47b8, { 0x82, 0xc9, 0xec, 0x4c, 0x40, 0x2, 0xfa, 0x64 } };

// {6AD4690F-540A-4695-9AFF-E3877E059A84}
static const IID IID_ILayerDisplayParams =
{ 0x6ad4690f, 0x540a, 0x4695, { 0x9a, 0xff, 0xe3, 0x87, 0x7e, 0x5, 0x9a, 0x84 } };

// {22E4F803-AA0B-49BF-9804-D1E18F620DE1}
static const IID IID_IEasyFeature =
{ 0x22e4f803, 0xaa0b, 0x49bf, { 0x98, 0x4, 0xd1, 0xe1, 0x8f, 0x62, 0xd, 0xe1 } };

// {0A61DBBC-F43E-4622-B1CB-E2E91E823957}
static const IID IID_IEasyIntArray =
{ 0xa61dbbc, 0xf43e, 0x4622, { 0xb1, 0xcb, 0xe2, 0xe9, 0x1e, 0x82, 0x39, 0x57 } };


void Easy2Geometry(IUnknown* pEasyGeometry, easymap::IGeometryPtr& pGeometry);
void Geometry2Easy(easymap::IGeometryPtr pGeometry, IUnknown** ppEasyGeometry);


/////////////////////////////////////////////////////////////////////////////
// CEasyEnvelope command target

class CEasyEnvelope : public CCmdTarget
{
	DECLARE_DYNCREATE(CEasyEnvelope)

	CEasyEnvelope();           // protected constructor used by dynamic creation

// Attributes
public:
    easymap::CEnvelopePtr m_pEnvelope;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyEnvelope)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CEasyEnvelope();

	// Generated message map functions
	//{{AFX_MSG(CEasyEnvelope)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	DECLARE_OLECREATE(CEasyEnvelope)

	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CEasyEnvelope)
	afx_msg void SetCoordinates(double minx, double miny, double maxx, double maxy);
	afx_msg void GetCoordinates(double FAR* minx, double FAR* miny, double FAR* maxx, double FAR* maxy);
	afx_msg long innergetrawgeometryptr(BOOL addref);
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()
};

/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CEasyPoint command target

class CEasyPoint : public CCmdTarget
{
	DECLARE_DYNCREATE(CEasyPoint)

	CEasyPoint();           // protected constructor used by dynamic creation

// Attributes
public:
    easymap::CPointPtr m_pPoint;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyPoint)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CEasyPoint();

	// Generated message map functions
	//{{AFX_MSG(CEasyPoint)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	DECLARE_OLECREATE(CEasyPoint)

	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CEasyPoint)
	afx_msg void SetCoordinates(double x, double y, double z);
	afx_msg void GetCoordinates(double FAR* x, double FAR* y, double FAR* z);
	afx_msg long innergetrawgeometryptr(BOOL addref);
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()
};

/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CEasyPoints command target

class CEasyPoints : public CCmdTarget
{
	DECLARE_DYNCREATE(CEasyPoints)

	CEasyPoints();           // protected constructor used by dynamic creation

// Attributes
public:
    easymap::CMultiPointPtr m_pMultiPoint;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyPoints)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CEasyPoints();

	// Generated message map functions
	//{{AFX_MSG(CEasyPoints)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	DECLARE_OLECREATE(CEasyPoints)

	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CEasyPoints)
	afx_msg void AddPoint(double x, double y, double z);
	afx_msg BOOL GetPoint(double FAR* x, double FAR* y, double FAR* z, long index);
	afx_msg long GetPointCount();
	afx_msg void ClearPoints();
	afx_msg long innergetrawgeometryptr(BOOL addref);
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()
};

/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CEasyPath command target

class CEasyPath : public CCmdTarget
{
	DECLARE_DYNCREATE(CEasyPath)

	CEasyPath();           // protected constructor used by dynamic creation

// Attributes
public:
    easymap::CPathPtr m_pPath;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyPath)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CEasyPath();

	// Generated message map functions
	//{{AFX_MSG(CEasyPath)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	DECLARE_OLECREATE(CEasyPath)

	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CEasyPath)
	afx_msg void AddPoint(double x, double y, double z);
	afx_msg BOOL GetPoint(double FAR* x, double FAR* y, double FAR* z, long index);
	afx_msg long GetPointCount();
	afx_msg void ClearPoints();
	afx_msg long innergetrawgeometryptr(BOOL addref);
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()
};

/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CEasyRing command target

class CEasyRing : public CCmdTarget
{
	DECLARE_DYNCREATE(CEasyRing)

	CEasyRing();           // protected constructor used by dynamic creation

// Attributes
public:
    easymap::CRingPtr m_pRing;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyRing)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CEasyRing();

	// Generated message map functions
	//{{AFX_MSG(CEasyRing)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	DECLARE_OLECREATE(CEasyRing)

	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CEasyRing)
	afx_msg void AddPoint(double x, double y, double z);
	afx_msg BOOL GetPoint(double FAR* x, double FAR* y, double FAR* z, long index);
	afx_msg long GetPointCount();
	afx_msg void ClearPoints();
	afx_msg long innergetrawgeometryptr(BOOL addref);
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
// CEasyPolyline command target

class CEasyPolyline : public CCmdTarget
{
	DECLARE_DYNCREATE(CEasyPolyline)

	CEasyPolyline();           // protected constructor used by dynamic creation

// Attributes
public:
    easymap::CPolylinePtr m_pPolyline;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyPolyline)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CEasyPolyline();

	// Generated message map functions
	//{{AFX_MSG(CEasyPolyline)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	DECLARE_OLECREATE(CEasyPolyline)

	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CEasyPolyline)
	afx_msg void AddPath(LPUNKNOWN path);
	afx_msg LPUNKNOWN GetPath(long index);
	afx_msg long GetPathCount();
	afx_msg void ClearPath();
	afx_msg long innergetrawgeometryptr(BOOL addref);
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()
};

/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CEasyPolygon command target

class CEasyPolygon : public CCmdTarget
{
	DECLARE_DYNCREATE(CEasyPolygon)

	CEasyPolygon();           // protected constructor used by dynamic creation

// Attributes
public:
    easymap::CPolygonPtr m_pPolygon;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyPolygon)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CEasyPolygon();

	// Generated message map functions
	//{{AFX_MSG(CEasyPolygon)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	DECLARE_OLECREATE(CEasyPolygon)

	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CEasyPolygon)
	afx_msg void AddRing(LPUNKNOWN ring);
	afx_msg LPUNKNOWN GetRing(long index);
	afx_msg long GetRingCount();
	afx_msg void ClearRings();
	afx_msg long innergetrawgeometryptr(BOOL addref);
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
// CEasyLayer command target

class CEasyLayer : public CCmdTarget
{
	DECLARE_DYNCREATE(CEasyLayer)

	CEasyLayer();           // protected constructor used by dynamic creation

// Attributes
public:
    easymap::ILayerPtr m_pLayer;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyLayer)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CEasyLayer();

	// Generated message map functions
	//{{AFX_MSG(CEasyLayer)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CEasyLayer)
	afx_msg void GetExtent(double FAR* minx, double FAR* miny, double FAR* maxx, double FAR* maxy);
	afx_msg double GetBascScale();
	afx_msg void SetName(LPCTSTR layername);
	afx_msg BSTR GetName();
	afx_msg void SetVisible(BOOL visible);
	afx_msg BOOL GetVisible();
	afx_msg void SetAlpha(long alpha);
	afx_msg long GetAlpha();
	afx_msg void SetScaleLimit(double maxscale, double minscale);
	afx_msg void GetScaleLimit(double FAR* maxscale, double FAR* minscale);
	afx_msg void SetTag(long tag);
	afx_msg long GetTag();
	afx_msg void SetSelectable(BOOL selectable);
	afx_msg BOOL GetSelectable();
	afx_msg long Select(double minx, double miny, double maxx, double maxy, BOOL partialselect, BOOL append);
	afx_msg long Deselect(double minx, double miny, double maxx, double maxy, BOOL partialselect);
	afx_msg long GetSelectCount();
	afx_msg void ClearSelection();
	afx_msg long GetLayerType();
	afx_msg long GetSubLayerCount();
	afx_msg LPUNKNOWN GetSubLayer(long index);
	afx_msg BOOL DeleteSubLayer(long index);
	afx_msg BOOL AddSubLayer(LPUNKNOWN layer);
	afx_msg BOOL ClearLayers();
	afx_msg long innergetrawlayerptr(BOOL addref);
	afx_msg long GetAllSubLayerCount();
	afx_msg BOOL DeleteSubLayerEx(LPUNKNOWN sublayer);
	afx_msg BOOL SetSubLayerOrder(LPUNKNOWN sublayer, long neworder);
	afx_msg long GetFeatureLayerType();
	afx_msg BOOL GetFeatureLayerInfo(double FAR* basescale, long FAR* mapunit, double FAR* precision);
	afx_msg BOOL SetDefaultSymbolFromLib(LPCTSTR symbollibfile, long symbolindex);
	afx_msg BOOL SetDefaultSymbolFromUI();
	afx_msg BOOL SetDefaultSymbolParam(LPUNKNOWN param);
	afx_msg LPUNKNOWN GetDefaultSymbolParam();
	afx_msg long GetAttribFieldCount();
	afx_msg long GetAttribFieldType(long fieldindex);
	afx_msg BSTR GetAttribFieldName(long fieldindex);
	afx_msg LPUNKNOWN GetSelection();
	afx_msg LPUNKNOWN GetFeature(long fid);
	afx_msg LPUNKNOWN CreateFeature();
	afx_msg void DeleteSelectedObjects();
	afx_msg void MoveSelectedObject(double deltax, double deltay);
	afx_msg long AddGeometryElement(LPUNKNOWN geometry);
	afx_msg long AddTextElement(double point_x, double point_y, LPCTSTR text);
	afx_msg LPUNKNOWN GetElement(long id, BSTR FAR* text, BOOL FAR* istext);
	afx_msg BOOL SetElement(long id, LPUNKNOWN geometry, LPCTSTR text);
	afx_msg BOOL RemoveElement(long id);
	afx_msg LPUNKNOWN Identify(double minx, double miny, double maxx, double maxy, BOOL partialselect);
	afx_msg void SelectAll();
	afx_msg LPUNKNOWN FindSubLayer(LPCTSTR layername);
	afx_msg BOOL ModifyGPSPoint(long fid, double x, double y);
	afx_msg BOOL GetMapEditable();
	afx_msg BOOL SaveEdit();
	afx_msg BOOL DiscardEdit();
	afx_msg BOOL SetUndoPoint();
	afx_msg BOOL Undo();
	afx_msg BOOL Redo();
	afx_msg BOOL Undoable();
	afx_msg BOOL Redoable();
	afx_msg BOOL SetMapEditable(BOOL editable);
	afx_msg BOOL ShowRendererUI();
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()

friend class CEasyControlCtrl;
};

/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CLayerDisplayParams command target

class CLayerDisplayParams : public CCmdTarget
{
	DECLARE_DYNCREATE(CLayerDisplayParams)

	CLayerDisplayParams();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLayerDisplayParams)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CLayerDisplayParams();

	// Generated message map functions
	//{{AFX_MSG(CLayerDisplayParams)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CLayerDisplayParams)
	long m_Color;
	afx_msg void OnColorChanged();
	long m_OuterLineColor;
	afx_msg void OnOuterLineColorChanged();
	double m_OuterLineWidth;
	afx_msg void OnOuterLineWidthChanged();
	double m_FontWidth;
	afx_msg void OnFontWidthChanged();
	double m_FontHeight;
	afx_msg void OnFontHeightChanged();
	double m_PointAngle;
	afx_msg void OnPointAngleChanged();
	double m_PointSize;
	afx_msg void OnPointSizeChanged();
	double m_LineWidth;
	afx_msg void OnLineWidthChanged();
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()

friend class CEasyLayer;
};

/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CEasyFeature command target

class CEasyFeature : public CCmdTarget
{
	DECLARE_DYNCREATE(CEasyFeature)

	CEasyFeature();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyFeature)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CEasyFeature();

	// Generated message map functions
	//{{AFX_MSG(CEasyFeature)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CEasyFeature)
	afx_msg BOOL SetGeometry(LPUNKNOWN geometry);
	afx_msg LPUNKNOWN GetGeometry();
	afx_msg long GetFid();
	afx_msg LPUNKNOWN GetMBR();
	afx_msg LPUNKNOWN GetLayer();
	afx_msg long GetFieldCount();
	afx_msg BOOL SetFieldValue(long fieldindex, LPCTSTR fieldvalue);
	afx_msg BSTR GetFieldValue(long fieldindex);
	afx_msg BOOL SetAnnotation(LPCTSTR annotation);
	afx_msg BSTR GetAnnotation();
	afx_msg BOOL Delete();
	afx_msg BOOL Update();
	afx_msg long innergetrawfeatureptr(BOOL addref);
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()

private:
    IEasyLayer* m_pEasyLayer;
    easymap::IVectorFeaturePtr m_pFeature;

friend class CEasyLayer;
};

/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CEasyIntArray command target

class CEasyIntArray : public CCmdTarget
{
	DECLARE_DYNCREATE(CEasyIntArray)

	CEasyIntArray();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyIntArray)
	public:
	virtual void OnFinalRelease();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CEasyIntArray();

	// Generated message map functions
	//{{AFX_MSG(CEasyIntArray)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	DECLARE_OLECREATE(CEasyIntArray)

	// Generated OLE dispatch map functions
	//{{AFX_DISPATCH(CEasyIntArray)
	afx_msg long Add(long newvalue);
	afx_msg BOOL Remove(long index);
	afx_msg long GetCount();
	afx_msg void Clear();
	afx_msg BOOL GetAt(long index, long FAR* value);
	afx_msg BOOL SetAt(long index, long newvalue);
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()

private:
    vector<long> m_FIDs;

friend class CEasyLayer;
};

/////////////////////////////////////////////////////////////////////////////
//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EASYCLASSES_H__29BA35AB_11D2_44AB_9858_07C64EF2AFE5__INCLUDED_)
