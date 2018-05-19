#if !defined(AFX_EASYCONTROLCTL_H__92445FF8_2AFE_4EFD_AAB6_8DE806CBAE38__INCLUDED_)
#define AFX_EASYCONTROLCTL_H__92445FF8_2AFE_4EFD_AAB6_8DE806CBAE38__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "..\\easylib\\GeoMap.h"
#include "..\\easylib\\SlimLayer.h"
#include "..\\easylib\\ElementLayer.h"
#include "..\\easylib\\GeometryTracker.h"
#include "..\\easylib\\SupportClasses.h"
#include "EasyClasses.h"

// EasyControlCtl.h : Declaration of the CEasyControlCtrl ActiveX Control class.

/////////////////////////////////////////////////////////////////////////////
// CEasyControlCtrl : See EasyControlCtl.cpp for implementation.

typedef long TrackFeatureType;
const TrackFeatureType TrackFeatureType_UNKNOWN     = 0;
const TrackFeatureType TrackFeatureType_POINT       = 1;
const TrackFeatureType TrackFeatureType_MULTIPOINT  = 2;
const TrackFeatureType TrackFeatureType_POLYLINE    = 3;
const TrackFeatureType TrackFeatureType_POLYGON     = 4;
const TrackFeatureType TrackFeatureType_ANNOTATION  = 5;

class CEasyControlCtrl : public COleControl
{
	DECLARE_DYNCREATE(CEasyControlCtrl)

// Constructor
public:
	CEasyControlCtrl();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyControlCtrl)
	public:
	virtual void OnDraw(CDC* pdc, const CRect& rcBounds, const CRect& rcInvalid);
	virtual void DoPropExchange(CPropExchange* pPX);
	virtual void OnResetState();
	//}}AFX_VIRTUAL

// Implementation
protected:
	~CEasyControlCtrl();

	DECLARE_OLECREATE_EX(CEasyControlCtrl)    // Class factory and guid
	DECLARE_OLETYPELIB(CEasyControlCtrl)      // GetTypeInfo
	DECLARE_PROPPAGEIDS(CEasyControlCtrl)     // Property page IDs
	DECLARE_OLECTLTYPE(CEasyControlCtrl)		// Type name and misc status

// Message maps
	//{{AFX_MSG(CEasyControlCtrl)
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg BOOL OnMouseWheel(UINT nFlags, short zDelta, CPoint pt);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

// Dispatch maps
	//{{AFX_DISPATCH(CEasyControlCtrl)
	OLE_COLOR m_BackColor;
	afx_msg void OnBackColorChanged();
	BOOL m_PartialSelect;
	afx_msg void OnPartialSelectChanged();
	BOOL m_SelectAppend;
	afx_msg void OnSelectAppendChanged();
	BOOL m_WheelZoomable;
	afx_msg void OnWheelZoomableChanged();
	afx_msg BOOL NewWorkspace();
	afx_msg BOOL LoadWorkspace(LPCTSTR filename);
	afx_msg BOOL SaveWorkspace(LPCTSTR filename);
	afx_msg void UpdateView();
	afx_msg void FullExtentView();
	afx_msg void GetMapCenter(double x, double y);
	afx_msg void SetMapCenter(double x, double y);
	afx_msg void SetScale(double scale);
	afx_msg double GetScale();
	afx_msg void SetViewExtent(double minx, double miny, double maxx, double maxy);
	afx_msg void GetViewExtent(double FAR* minx, double FAR* miny, double FAR* maxx, double FAR* maxy);
	afx_msg void SetActionNone();
	afx_msg void SetActionZoomin();
	afx_msg void SetActionZoomout();
	afx_msg void SetActionPan();
	afx_msg void ClearLayers();
	afx_msg void GetFullExtent(double FAR* minx, double FAR* miny, double FAR* maxx, double FAR* maxy);
	afx_msg LPUNKNOWN GetLayer(long index);
	afx_msg void Window2Map(long lx, long ly, double FAR* dx, double FAR* dy);
	afx_msg void Map2Window(double dx, double dy, long FAR* lx, long FAR* ly);
	afx_msg long GetLayerCount();
	afx_msg long GetAllLayerCount();
	afx_msg LPUNKNOWN LoadSlimDataEx(LPUNKNOWN grouplayer, LPCTSTR filename, BOOL readonly);
	afx_msg LPUNKNOWN LoadShapeFileEx(LPUNKNOWN grouplayer, LPCTSTR filename, long mapunit, double basescale, double precision, long indexlevel, BOOL readonly);
	afx_msg LPUNKNOWN LoadOrientBmpEx(LPUNKNOWN grouplayer, LPCTSTR filename);
	afx_msg LPUNKNOWN NewGroupLayerEx(LPUNKNOWN parentgrouplayer, LPCTSTR newlayername);
	afx_msg BOOL LoadSlimData(LPCTSTR filename, BOOL readonly);
	afx_msg BOOL LoadShapeFile(LPCTSTR filename, long mapunit, double basescale, double precision, long indexlevel, BOOL readonly);
	afx_msg BOOL LoadOrientBmp(LPCTSTR filename);
	afx_msg BOOL NewElementLayer(LPCTSTR newlayername);
	afx_msg LPUNKNOWN NewElementLayerEx(LPUNKNOWN grouplayer, LPCTSTR newlayername);
	afx_msg void SetMapUnit(long mapunit);
	afx_msg long GetMapUnit();
	afx_msg void SetReferenceScale(double refscale);
	afx_msg double GetReferenceScale();
	afx_msg BOOL SetLayerOrder(LPUNKNOWN layer, long neworder);
	afx_msg void DeleteSelectObjects();
	afx_msg BOOL EditUndo();
	afx_msg BOOL EditRedo();
	afx_msg BOOL EditSave();
	afx_msg BOOL EditCancel();
	afx_msg BOOL IsEditDirty();
	afx_msg BOOL SetEditUndoPoint(LPCTSTR desc);
	afx_msg long GetCurrentAction();
	afx_msg BOOL EditTrackFeature(LPUNKNOWN layer, LPCTSTR text);
	afx_msg void SelectByPoint();
	afx_msg void SelectByEnvelope();
	afx_msg void DeselectByPoint();
	afx_msg void DeselectByEnvelope();
	afx_msg void MoveSelectedObjects();
	afx_msg BOOL EditTrackPointElement(LPUNKNOWN layer);
	afx_msg BOOL EditTrackEnvelopeElement(LPUNKNOWN layer);
	afx_msg BOOL EditTrackCircleElement(LPUNKNOWN layer);
	afx_msg BOOL EditTrackEllipseElement(LPUNKNOWN layer);
	afx_msg BOOL EditTrackPolylineElement(LPUNKNOWN layer);
	afx_msg BOOL EditTrackPolygonElement(LPUNKNOWN layer);
	afx_msg BOOL EditTrackFreehandLineElement(LPUNKNOWN layer);
	afx_msg BOOL EditTrackFreehandFillElement(LPUNKNOWN layer);
	afx_msg BOOL EditTrackTextElement(LPUNKNOWN layer, LPCTSTR text);
	afx_msg BOOL SetNewElementSymbolParams(LPUNKNOWN params);
	afx_msg LPUNKNOWN GetNewElementSymbolParams();
	afx_msg BOOL SetNewElementSymbolFromLib(LPCTSTR symbollibfile, long symbolindex);
	afx_msg void AddHighlight(LPUNKNOWN geometry);
	afx_msg LPUNKNOWN GetHighlight(long index);
	afx_msg BOOL DeleteHighlight(long index);
	afx_msg void ClearHighlight();
	afx_msg void RefreshWindow();
	afx_msg void SetHighlightParams(LPUNKNOWN params);
	afx_msg LPUNKNOWN GetHighlightParams();
	afx_msg void SetSlimDataMapping(BOOL mapping);
	afx_msg BOOL GetSlimDataMapping();
	afx_msg void ClearLabels();
	afx_msg long GetLabelLayerCount();
	afx_msg LPUNKNOWN GetLabelLayer(long index);
	afx_msg BOOL RemoveLabelLayer(long index);
	afx_msg BOOL LoadLabelLayer(LPCTSTR esd_filename, long labelfield);
	afx_msg BOOL SetLabelField(long labellayer, long labelfield);
	afx_msg long GetLabelField(long labellayer);
	afx_msg void EnableLabels(BOOL Enable);
	afx_msg BOOL LabelsEnabled();
	afx_msg BOOL LoadRapidDrawLayer(LPCTSTR esd_filename, BOOL readonly);
	afx_msg LPUNKNOWN GetRapidDrawLayer(long index);
	afx_msg BOOL RemoveRapidDraw(long index);
	afx_msg long GetRapidDrawLayerCount();
	afx_msg void ClearRapidDrawLayers();
	afx_msg BOOL AddBookmark(LPCTSTR text);
	afx_msg BOOL NextBookmark();
	afx_msg BOOL PreviousBookmark();
	afx_msg BOOL DeleteBookmark();
	afx_msg long BookmarkCount();
	afx_msg void ClearBookmarks();
	afx_msg LPUNKNOWN RapidDrawGPS();
	afx_msg LPUNKNOWN FindLayer(LPCTSTR layername);
	afx_msg void SetPlaneRotate(double degree);
	afx_msg double GetPlaneRotate();
	afx_msg void SetAttitude(double degree);
	afx_msg double GetAttitude();
	afx_msg void SetRotateCenter(double x, double y);
	afx_msg void GetRotateCenter(double FAR* x, double FAR* y);
	afx_msg void DisableActiveBookmarkShow();
	//}}AFX_DISPATCH
	DECLARE_DISPATCH_MAP()

	afx_msg void AboutBox();

// Event maps
	//{{AFX_EVENT(CEasyControlCtrl)
	void FireViewExtentChange(double minx, double miny, double maxx, double maxy)
		{FireEvent(eventidViewExtentChange,EVENT_PARAM(VTS_R8  VTS_R8  VTS_R8  VTS_R8), minx, miny, maxx, maxy);}
	void FireNewFeatureTracked(long fid, BOOL FAR* setundopoint)
		{FireEvent(eventidNewFeatureTracked,EVENT_PARAM(VTS_I4  VTS_PBOOL), fid, setundopoint);}
	void FireOnPaint(long dc)
		{FireEvent(eventidOnPaint,EVENT_PARAM(VTS_I4), dc);}
	void FireObjectsMoved(BOOL FAR* setundopoint)
		{FireEvent(eventidObjectsMoved,EVENT_PARAM(VTS_PBOOL), setundopoint);}
	void FireNewElementTracked(long id, BOOL FAR* setundopoint)
		{FireEvent(eventidNewElementTracked,EVENT_PARAM(VTS_I4  VTS_PBOOL), id, setundopoint);}
	//}}AFX_EVENT
	DECLARE_EVENT_MAP()

// Dispatch and event IDs
public:
	enum {
	//{{AFX_DISP_ID(CEasyControlCtrl)
	dispidBackColor = 1L,
	dispidPartialSelect = 2L,
	dispidSelectAppend = 3L,
	dispidWheelZoomable = 4L,
	dispidNewWorkspace = 5L,
	dispidLoadWorkspace = 6L,
	dispidSaveWorkspace = 7L,
	dispidUpdateView = 8L,
	dispidFullExtentView = 9L,
	dispidGetMapCenter = 10L,
	dispidSetMapCenter = 11L,
	dispidSetScale = 12L,
	dispidGetScale = 13L,
	dispidSetViewExtent = 14L,
	dispidGetViewExtent = 15L,
	dispidSetActionNone = 16L,
	dispidSetActionZoomin = 17L,
	dispidSetActionZoomout = 18L,
	dispidSetActionPan = 19L,
	dispidClearLayers = 20L,
	dispidGetFullExtent = 21L,
	dispidGetLayer = 22L,
	dispidWindow2Map = 23L,
	dispidMap2Window = 24L,
	dispidGetLayerCount = 25L,
	dispidGetAllLayerCount = 26L,
	dispidLoadSlimDataEx = 27L,
	dispidLoadShapeFileEx = 28L,
	dispidLoadOrientBmpEx = 29L,
	dispidNewGroupLayerEx = 30L,
	dispidLoadSlimData = 31L,
	dispidLoadShapeFile = 32L,
	dispidLoadOrientBmp = 33L,
	dispidNewElementLayer = 34L,
	dispidNewElementLayerEx = 35L,
	dispidSetMapUnit = 36L,
	dispidGetMapUnit = 37L,
	dispidSetReferenceScale = 38L,
	dispidGetReferenceScale = 39L,
	dispidSetLayerOrder = 40L,
	dispidDeleteSelectObjects = 41L,
	dispidEditUndo = 42L,
	dispidEditRedo = 43L,
	dispidEditSave = 44L,
	dispidEditCancel = 45L,
	dispidIsEditDirty = 46L,
	dispidSetEditUndoPoint = 47L,
	dispidGetCurrentAction = 48L,
	dispidEditTrackFeature = 49L,
	dispidSelectByPoint = 50L,
	dispidSelectByEnvelope = 51L,
	dispidDeselectByPoint = 52L,
	dispidDeselectByEnvelope = 53L,
	dispidMoveSelectedObjects = 54L,
	dispidEditTrackPointElement = 55L,
	dispidEditTrackEnvelopeElement = 56L,
	dispidEditTrackCircleElement = 57L,
	dispidEditTrackEllipseElement = 58L,
	dispidEditTrackPolylineElement = 59L,
	dispidEditTrackPolygonElement = 60L,
	dispidEditTrackFreehandLineElement = 61L,
	dispidEditTrackFreehandFillElement = 62L,
	dispidEditTrackTextElement = 63L,
	dispidSetNewElementSymbolParams = 64L,
	dispidGetNewElementSymbolParams = 65L,
	dispidSetNewElementSymbolFromLib = 66L,
	dispidAddHighlight = 67L,
	dispidGetHighlight = 68L,
	dispidDeleteHighlight = 69L,
	dispidClearHighlight = 70L,
	dispidRefreshWindow = 71L,
	dispidSetHighlightParams = 72L,
	dispidGetHighlightParams = 73L,
	dispidSetSlimDataMapping = 74L,
	dispidGetSlimDataMapping = 75L,
	dispidClearLabels = 76L,
	dispidGetLabelLayerCount = 77L,
	dispidGetLabelLayer = 78L,
	dispidRemoveLabelLayer = 79L,
	dispidLoadLabelLayer = 80L,
	dispidSetLabelField = 81L,
	dispidGetLabelField = 82L,
	dispidEnableLabels = 83L,
	dispidLabelsEnabled = 84L,
	dispidLoadRapidDrawLayer = 85L,
	dispidGetRapidDrawLayer = 86L,
	dispidRemoveRapidDraw = 87L,
	dispidGetRapidDrawLayerCount = 88L,
	dispidClearRapidDrawLayers = 89L,
	dispidAddBookmark = 90L,
	dispidNextBookmark = 91L,
	dispidPreviousBookmark = 92L,
	dispidDeleteBookmark = 93L,
	dispidBookmarkCount = 94L,
	dispidClearBookmarks = 95L,
	dispidRapidDrawGPS = 96L,
	dispidFindLayer = 97L,
	dispidSetPlaneRotate = 98L,
	dispidGetPlaneRotate = 99L,
	dispidSetAttitude = 100L,
	dispidGetAttitude = 101L,
	dispidSetRotateCenter = 102L,
	dispidGetRotateCenter = 103L,
	dispidDisableActiveBookmarkShow = 104L,
	eventidViewExtentChange = 1L,
	eventidNewFeatureTracked = 2L,
	eventidOnPaint = 3L,
	eventidObjectsMoved = 4L,
	eventidNewElementTracked = 5L,
	//}}AFX_DISP_ID
	};

private:
    easymap::CMapPtr                    m_pMap;
    easymap::CActiveViewPtr             m_pAV;
    easymap::CDisplayPtr                m_pDisplay;
    easymap::CDisplayTransformationPtr  m_pDT;
    easymap::CVectorLayerPtr            m_pTrackFeatureLayer;
    easymap::IVectorFeaturePtr          m_pTrackFeature;
    easymap::CMoveTrackerPtr            m_pMoveTracker;
    easymap::CElementLayerPtr           m_pTrackElementLayer;
    easymap::ISymbolPtr                 m_pTrackPointSymbol;
    easymap::ISymbolPtr                 m_pTrackLineSymbol;
    easymap::ISymbolPtr                 m_pTrackFillSymbol;
    easymap::ISymbolPtr                 m_pTrackTextSymbol;
    HDC                                 m_WindowDC;
    EasyMouseAction                     m_MouseAction;
    vector<tagPOINT>                    m_TrackPoints;
    TrackFeatureType                    m_TrackFeatureType;
    CString                             m_TrackAnnotation;
    bool                                m_InitOK;
    vector<easymap::IGeometryPtr>       m_Highlights;
    easymap::ISymbolPtr                 m_pHighlightPointSymbol;
    easymap::ISymbolPtr                 m_pHighlightLineSymbol;
    easymap::ISymbolPtr                 m_pHighlightFillSymbol;
    bool                                m_MappingLoad;

    void ReMap();
    void MainSpaceResize();
    BOOL AddLayerInner(LPUNKNOWN grouplayer, easymap::ILayerPtr pLayer,
        LPUNKNOWN FAR* newlayer);

    void TrackFeatureLButtonDown(UINT nFlags, CPoint point);
    void TrackFeatureRButtonDown(UINT nFlags, CPoint point);
    void TrackFeatureMouseMove(UINT nFlags, CPoint point);
    BOOL PreEditTrackElement(LPUNKNOWN layer);
    void NewPointOrTextElement(CPoint point, CString text);
    void NewElement();
    vector<easymap::WKSPointZ> TrackPoints2WKSPointZs() const;

    void MoveObjectsStart(CPoint point);
    void MoveObjectsMoving(CPoint point);
    void MoveObjectsFinish(CPoint point);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EASYCONTROLCTL_H__92445FF8_2AFE_4EFD_AAB6_8DE806CBAE38__INCLUDED)
