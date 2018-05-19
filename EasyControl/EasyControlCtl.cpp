// EasyControlCtl.cpp : Implementation of the CEasyControlCtrl ActiveX Control class.

#include "stdafx.h"
#include "EasyControl.h"
#include "EasyControlCtl.h"
#include "EasyControlPpg.h"
#include "..\\easylib\\MemoryStream.h"
#include "..\\easylib\\MultiSymbol.h"
#include "..\\easylib\\SymbolLib.h"
#include "..\\easylib\\ShapeLayer.h"
#include "..\\easylib\\BitmapLayer.h"
#include "..\\easylib\\GroupLayer.h"
#include "..\\easylib\\ElementLayer.h"
#include "..\\easylib\\easylib.h"
#include "..\\SymbolUI\\SymbolUI.h"
#include "CommonFuncs.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


void _easycontrol_create_default_symbol(const easymap::SymbolType symboltype,
    easymap::ISymbolPtr& pSymbol)
{
    easymap::IMultiPointSymbolPtr pMPS;
    easymap::IMultiLineSymbolPtr pMLS;
    easymap::IMultiFillSymbolPtr pMFS;
    COLORREF pointcolor =  RGB(230, 30, 20);
    COLORREF linecolor =  RGB(230, 30, 20);
    COLORREF fillcolor =  RGB(230, 30, 20);
    switch (symboltype)
    {
    case easymap::SYMBOLTYPE_POINT:
        EASYLIB_CREATEOBJ(CMultiPointSymbol, pMPS, IMultiPointSymbol)
        pMPS->AddSimpleSymbol(pointcolor, 2);
        CAST_PTR(pMPS, pSymbol, ISymbol)
        break;

    case easymap::SYMBOLTYPE_LINE:
        EASYLIB_CREATEOBJ(CMultiLineSymbol, pMLS, IMultiLineSymbol)
        pMLS->AddSimpleSymbol(linecolor, 2);
        CAST_PTR(pMLS, pSymbol, ISymbol)
        break;

    case easymap::SYMBOLTYPE_FILL:
        EASYLIB_CREATEOBJ(CMultiFillSymbol, pMFS, IMultiFillSymbol)
        pMFS->AddSimpleSymbol(fillcolor);
        CAST_PTR(pMFS, pSymbol, ISymbol)
        break;

    case easymap::SYMBOLTYPE_TEXT:
        EASYLIB_CREATEOBJ(CSimpleTextSymbol, pSymbol, ISymbol)
        break;

    default:
        return;
    }
}

void _easycontrol_setsymbolparams(ILayerDisplayParams* pParams,
    easymap::ISymbolPtr pSymbol)
{
    if (!pSymbol.Assigned() || !pParams)
        return;

    COLORREF color = pParams->GetColor();

    easymap::IMultiPointSymbolPtr pMPS;
    CAST_PTR(pSymbol, pMPS, IMultiPointSymbol)
    if (pMPS.Assigned())
    {
        pMPS->SetColor(color);
        pMPS->SetAngle(pParams->GetPointAngle());
        pMPS->SetSize(pParams->GetPointSize());
        return;
    }

    easymap::IMultiLineSymbolPtr pMLS;
    CAST_PTR(pSymbol, pMLS, IMultiLineSymbol)
    if (pMLS.Assigned())
    {
        pMLS->SetColor(color);
        pMLS->SetSize(pParams->GetLineWidth());
        return;
    }

    easymap::IMultiFillSymbolPtr pMFS;
    CAST_PTR(pSymbol, pMFS, IMultiFillSymbol)
    if (pMFS.Assigned())
    {
        pMFS->SetColor(color);
        if (pMFS->GetSymbolCount() < 1)
            return;
        easymap::IFillSymbolPtr pFillSymbol;
        pMFS->GetSymbolRef(pFillSymbol._ref(), 0);
        easymap::ISimpleFillSymbolPtr pSimpleFillSymbol;
        CAST_PTR(pFillSymbol, pSimpleFillSymbol, ISimpleFillSymbol)
        pSimpleFillSymbol->SetBorderColor(pParams->GetOuterLineColor());
        pSimpleFillSymbol->SetBorderWidth(pParams->GetOuterLineWidth());
        return;
    }

    easymap::ISimpleTextSymbolPtr pSTS;
    CAST_PTR(pSymbol, pSTS, ISimpleTextSymbol)
    if (pSTS.Assigned())
    {
        pSTS->SetColor(color);
        pSTS->SetHeight(pParams->GetFontHeight());
        pSTS->SetWidth(pParams->GetFontWidth());
        return;
    }
}

void _easycontrol_getsymbolparams(easymap::ISymbolPtr pSymbol,
    ILayerDisplayParams* pParams)
{
    if (!pSymbol.Assigned() || !pParams)
        return;

    easymap::IMultiPointSymbolPtr pMPS;
    easymap::IMultiLineSymbolPtr pMLS;
    easymap::IMultiFillSymbolPtr pMFS;
    easymap::ISimpleTextSymbolPtr pSTS;

    COLORREF color;
    pSymbol->GetColor(color);
    pParams->PutColor(color);

    CAST_PTR(pSymbol, pMPS, IMultiPointSymbol)
    if (pMPS.Assigned())
    {
        double pointangle = 0;
        pMPS->GetAngle(pointangle);
        pParams->PutPointAngle(pointangle);
        double pointsize = 0;
        pMPS->GetSize(pointsize);
        pParams->PutPointSize(pointsize);
        return;
    }

    CAST_PTR(pSymbol, pMLS, IMultiLineSymbol)
    if (pMLS.Assigned())
    {
        double linewidth = 0;
        pMLS->GetSize(linewidth);
        pParams->PutLineWidth(linewidth);
        return;
    }

    CAST_PTR(pSymbol, pMFS, IMultiFillSymbol)
    if (pMFS.Assigned())
    {
        if (pMFS->GetSymbolCount() < 1)
            return;
        easymap::IFillSymbolPtr pFillSymbol;
        pMFS->GetSymbolRef(pFillSymbol._ref(), 0);
        easymap::ISimpleFillSymbolPtr pSimpleFillSymbol;
        CAST_PTR(pFillSymbol, pSimpleFillSymbol, ISimpleFillSymbol)
        COLORREF bordercolor;
        pSimpleFillSymbol->GetBorderColor(bordercolor);
        pParams->PutOuterLineColor(bordercolor);
        double borderwidth = 0;
        pSimpleFillSymbol->GetBorderWidth(borderwidth);
        pParams->PutOuterLineWidth(borderwidth);
        return;
    }

    CAST_PTR(pSymbol, pSTS, ISimpleTextSymbol)
    if (pSTS.Assigned())
    {
        double fontheight = 0;
        double fontwidth = 0;
        pSTS->GetHeight(fontheight);
        pParams->PutFontHeight(fontheight);
        pSTS->GetWidth(fontwidth);
        pParams->PutFontWidth(fontwidth);
        return;
    }
}

IMPLEMENT_DYNCREATE(CEasyControlCtrl, COleControl)

/////////////////////////////////////////////////////////////////////////////
// Message map

BEGIN_MESSAGE_MAP(CEasyControlCtrl, COleControl)
    //{{AFX_MSG_MAP(CEasyControlCtrl)
    ON_WM_SIZE()
    ON_WM_SHOWWINDOW()
    ON_WM_LBUTTONDOWN()
    ON_WM_MOUSEMOVE()
    ON_WM_LBUTTONUP()
    ON_WM_MOUSEWHEEL()
	ON_WM_CREATE()
	ON_WM_RBUTTONDOWN()
	//}}AFX_MSG_MAP
    ON_OLEVERB(AFX_IDS_VERB_PROPERTIES, OnProperties)
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// Dispatch map

BEGIN_DISPATCH_MAP(CEasyControlCtrl, COleControl)
    //{{AFX_DISPATCH_MAP(CEasyControlCtrl)
	DISP_PROPERTY_NOTIFY(CEasyControlCtrl, "BackColor", m_BackColor, OnBackColorChanged, VT_COLOR)
	DISP_PROPERTY_NOTIFY(CEasyControlCtrl, "PartialSelect", m_PartialSelect, OnPartialSelectChanged, VT_BOOL)
	DISP_PROPERTY_NOTIFY(CEasyControlCtrl, "SelectAppend", m_SelectAppend, OnSelectAppendChanged, VT_BOOL)
	DISP_PROPERTY_NOTIFY(CEasyControlCtrl, "WheelZoomable", m_WheelZoomable, OnWheelZoomableChanged, VT_BOOL)
    DISP_FUNCTION(CEasyControlCtrl, "NewWorkspace", NewWorkspace, VT_BOOL, VTS_NONE)
    DISP_FUNCTION(CEasyControlCtrl, "LoadWorkspace", LoadWorkspace, VT_BOOL, VTS_BSTR)
    DISP_FUNCTION(CEasyControlCtrl, "SaveWorkspace", SaveWorkspace, VT_BOOL, VTS_BSTR)
    DISP_FUNCTION(CEasyControlCtrl, "UpdateView", UpdateView, VT_EMPTY, VTS_NONE)
    DISP_FUNCTION(CEasyControlCtrl, "FullExtentView", FullExtentView, VT_EMPTY, VTS_NONE)
    DISP_FUNCTION(CEasyControlCtrl, "GetMapCenter", GetMapCenter, VT_EMPTY, VTS_R8 VTS_R8)
    DISP_FUNCTION(CEasyControlCtrl, "SetMapCenter", SetMapCenter, VT_EMPTY, VTS_R8 VTS_R8)
    DISP_FUNCTION(CEasyControlCtrl, "SetScale", SetScale, VT_EMPTY, VTS_R8)
    DISP_FUNCTION(CEasyControlCtrl, "GetScale", GetScale, VT_R8, VTS_NONE)
    DISP_FUNCTION(CEasyControlCtrl, "SetViewExtent", SetViewExtent, VT_EMPTY, VTS_R8 VTS_R8 VTS_R8 VTS_R8)
    DISP_FUNCTION(CEasyControlCtrl, "GetViewExtent", GetViewExtent, VT_EMPTY, VTS_PR8 VTS_PR8 VTS_PR8 VTS_PR8)
    DISP_FUNCTION(CEasyControlCtrl, "SetActionNone", SetActionNone, VT_EMPTY, VTS_NONE)
    DISP_FUNCTION(CEasyControlCtrl, "SetActionZoomin", SetActionZoomin, VT_EMPTY, VTS_NONE)
    DISP_FUNCTION(CEasyControlCtrl, "SetActionZoomout", SetActionZoomout, VT_EMPTY, VTS_NONE)
    DISP_FUNCTION(CEasyControlCtrl, "SetActionPan", SetActionPan, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "ClearLayers", ClearLayers, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "GetFullExtent", GetFullExtent, VT_EMPTY, VTS_PR8 VTS_PR8 VTS_PR8 VTS_PR8)
	DISP_FUNCTION(CEasyControlCtrl, "GetLayer", GetLayer, VT_UNKNOWN, VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "Window2Map", Window2Map, VT_EMPTY, VTS_I4 VTS_I4 VTS_PR8 VTS_PR8)
	DISP_FUNCTION(CEasyControlCtrl, "Map2Window", Map2Window, VT_EMPTY, VTS_R8 VTS_R8 VTS_PI4 VTS_PI4)
	DISP_FUNCTION(CEasyControlCtrl, "GetLayerCount", GetLayerCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "GetAllLayerCount", GetAllLayerCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "LoadSlimDataEx", LoadSlimDataEx, VT_UNKNOWN, VTS_UNKNOWN VTS_BSTR VTS_BOOL)
	DISP_FUNCTION(CEasyControlCtrl, "LoadShapeFileEx", LoadShapeFileEx, VT_UNKNOWN, VTS_UNKNOWN VTS_BSTR VTS_I4 VTS_R8 VTS_R8 VTS_I4 VTS_BOOL)
	DISP_FUNCTION(CEasyControlCtrl, "LoadOrientBmpEx", LoadOrientBmpEx, VT_UNKNOWN, VTS_UNKNOWN VTS_BSTR)
	DISP_FUNCTION(CEasyControlCtrl, "NewGroupLayerEx", NewGroupLayerEx, VT_UNKNOWN, VTS_UNKNOWN VTS_BSTR)
	DISP_FUNCTION(CEasyControlCtrl, "LoadSlimData", LoadSlimData, VT_BOOL, VTS_BSTR VTS_BOOL)
	DISP_FUNCTION(CEasyControlCtrl, "LoadShapeFile", LoadShapeFile, VT_BOOL, VTS_BSTR VTS_I4 VTS_R8 VTS_R8 VTS_I4 VTS_BOOL)
	DISP_FUNCTION(CEasyControlCtrl, "LoadOrientBmp", LoadOrientBmp, VT_BOOL, VTS_BSTR)
	DISP_FUNCTION(CEasyControlCtrl, "NewElementLayer", NewElementLayer, VT_BOOL, VTS_BSTR)
	DISP_FUNCTION(CEasyControlCtrl, "NewElementLayerEx", NewElementLayerEx, VT_UNKNOWN, VTS_UNKNOWN VTS_BSTR)
	DISP_FUNCTION(CEasyControlCtrl, "SetMapUnit", SetMapUnit, VT_EMPTY, VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "GetMapUnit", GetMapUnit, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "SetReferenceScale", SetReferenceScale, VT_EMPTY, VTS_R8)
	DISP_FUNCTION(CEasyControlCtrl, "GetReferenceScale", GetReferenceScale, VT_R8, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "SetLayerOrder", SetLayerOrder, VT_BOOL, VTS_UNKNOWN VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "DeleteSelectObjects", DeleteSelectObjects, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "EditUndo", EditUndo, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "EditRedo", EditRedo, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "EditSave", EditSave, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "EditCancel", EditCancel, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "IsEditDirty", IsEditDirty, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "SetEditUndoPoint", SetEditUndoPoint, VT_BOOL, VTS_BSTR)
	DISP_FUNCTION(CEasyControlCtrl, "GetCurrentAction", GetCurrentAction, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "EditTrackFeature", EditTrackFeature, VT_BOOL, VTS_UNKNOWN VTS_BSTR)
	DISP_FUNCTION(CEasyControlCtrl, "SelectByPoint", SelectByPoint, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "SelectByEnvelope", SelectByEnvelope, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "DeselectByPoint", DeselectByPoint, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "DeselectByEnvelope", DeselectByEnvelope, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "MoveSelectedObjects", MoveSelectedObjects, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "EditTrackPointElement", EditTrackPointElement, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "EditTrackEnvelopeElement", EditTrackEnvelopeElement, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "EditTrackCircleElement", EditTrackCircleElement, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "EditTrackEllipseElement", EditTrackEllipseElement, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "EditTrackPolylineElement", EditTrackPolylineElement, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "EditTrackPolygonElement", EditTrackPolygonElement, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "EditTrackFreehandLineElement", EditTrackFreehandLineElement, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "EditTrackFreehandFillElement", EditTrackFreehandFillElement, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "EditTrackTextElement", EditTrackTextElement, VT_BOOL, VTS_UNKNOWN VTS_BSTR)
	DISP_FUNCTION(CEasyControlCtrl, "SetNewElementSymbolParams", SetNewElementSymbolParams, VT_BOOL, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "GetNewElementSymbolParams", GetNewElementSymbolParams, VT_UNKNOWN, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "SetNewElementSymbolFromLib", SetNewElementSymbolFromLib, VT_BOOL, VTS_BSTR VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "AddHighlight", AddHighlight, VT_EMPTY, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "GetHighlight", GetHighlight, VT_UNKNOWN, VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "DeleteHighlight", DeleteHighlight, VT_BOOL, VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "ClearHighlight", ClearHighlight, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "RefreshWindow", RefreshWindow, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "SetHighlightParams", SetHighlightParams, VT_EMPTY, VTS_UNKNOWN)
	DISP_FUNCTION(CEasyControlCtrl, "GetHighlightParams", GetHighlightParams, VT_UNKNOWN, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "SetSlimDataMapping", SetSlimDataMapping, VT_EMPTY, VTS_BOOL)
	DISP_FUNCTION(CEasyControlCtrl, "GetSlimDataMapping", GetSlimDataMapping, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "ClearLabels", ClearLabels, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "GetLabelLayerCount", GetLabelLayerCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "GetLabelLayer", GetLabelLayer, VT_UNKNOWN, VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "RemoveLabelLayer", RemoveLabelLayer, VT_BOOL, VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "LoadLabelLayer", LoadLabelLayer, VT_BOOL, VTS_BSTR VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "SetLabelField", SetLabelField, VT_BOOL, VTS_I4 VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "GetLabelField", GetLabelField, VT_I4, VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "EnableLabels", EnableLabels, VT_EMPTY, VTS_BOOL)
	DISP_FUNCTION(CEasyControlCtrl, "LabelsEnabled", LabelsEnabled, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "LoadRapidDrawLayer", LoadRapidDrawLayer, VT_BOOL, VTS_BSTR VTS_BOOL)
	DISP_FUNCTION(CEasyControlCtrl, "GetRapidDrawLayer", GetRapidDrawLayer, VT_UNKNOWN, VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "RemoveRapidDraw", RemoveRapidDraw, VT_BOOL, VTS_I4)
	DISP_FUNCTION(CEasyControlCtrl, "GetRapidDrawLayerCount", GetRapidDrawLayerCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "ClearRapidDrawLayers", ClearRapidDrawLayers, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "AddBookmark", AddBookmark, VT_BOOL, VTS_BSTR)
	DISP_FUNCTION(CEasyControlCtrl, "NextBookmark", NextBookmark, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "PreviousBookmark", PreviousBookmark, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "DeleteBookmark", DeleteBookmark, VT_BOOL, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "BookmarkCount", BookmarkCount, VT_I4, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "ClearBookmarks", ClearBookmarks, VT_EMPTY, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "RapidDrawGPS", RapidDrawGPS, VT_UNKNOWN, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "FindLayer", FindLayer, VT_UNKNOWN, VTS_BSTR)
	DISP_FUNCTION(CEasyControlCtrl, "SetPlaneRotate", SetPlaneRotate, VT_EMPTY, VTS_R8)
	DISP_FUNCTION(CEasyControlCtrl, "GetPlaneRotate", GetPlaneRotate, VT_R8, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "SetAttitude", SetAttitude, VT_EMPTY, VTS_R8)
	DISP_FUNCTION(CEasyControlCtrl, "GetAttitude", GetAttitude, VT_R8, VTS_NONE)
	DISP_FUNCTION(CEasyControlCtrl, "SetRotateCenter", SetRotateCenter, VT_EMPTY, VTS_R8 VTS_R8)
	DISP_FUNCTION(CEasyControlCtrl, "GetRotateCenter", GetRotateCenter, VT_EMPTY, VTS_PR8 VTS_PR8)
	DISP_FUNCTION(CEasyControlCtrl, "DisableActiveBookmarkShow", DisableActiveBookmarkShow, VT_EMPTY, VTS_NONE)
	//}}AFX_DISPATCH_MAP
    DISP_FUNCTION_ID(CEasyControlCtrl, "AboutBox", DISPID_ABOUTBOX, AboutBox, VT_EMPTY, VTS_NONE)
END_DISPATCH_MAP()


/////////////////////////////////////////////////////////////////////////////
// Event map

BEGIN_EVENT_MAP(CEasyControlCtrl, COleControl)
    //{{AFX_EVENT_MAP(CEasyControlCtrl)
    EVENT_CUSTOM("ViewExtentChange", FireViewExtentChange, VTS_R8  VTS_R8  VTS_R8  VTS_R8)
	EVENT_CUSTOM("NewFeatureTracked", FireNewFeatureTracked, VTS_I4  VTS_PBOOL)
	EVENT_CUSTOM("OnPaint", FireOnPaint, VTS_I4)
	EVENT_CUSTOM("ObjectsMoved", FireObjectsMoved, VTS_PBOOL)
	EVENT_CUSTOM("NewElementTracked", FireNewElementTracked, VTS_I4  VTS_PBOOL)
	EVENT_STOCK_MOUSEDOWN()
	EVENT_STOCK_MOUSEUP()
	EVENT_STOCK_MOUSEMOVE()
	EVENT_STOCK_KEYDOWN()
	EVENT_STOCK_KEYUP()
	//}}AFX_EVENT_MAP
END_EVENT_MAP()


/////////////////////////////////////////////////////////////////////////////
// Property pages

// TODO: Add more property pages as needed.  Remember to increase the count!
BEGIN_PROPPAGEIDS(CEasyControlCtrl, 1)
    PROPPAGEID(CEasyControlPropPage::guid)
END_PROPPAGEIDS(CEasyControlCtrl)


/////////////////////////////////////////////////////////////////////////////
// Initialize class factory and guid

IMPLEMENT_OLECREATE_EX(CEasyControlCtrl, "EASYCONTROL.EasyControlCtrl.1",
    0xfce3d833, 0x3fb6, 0x4503, 0x94, 0x1d, 0x5c, 0xa9, 0x89, 0x79, 0x8b, 0x89)


/////////////////////////////////////////////////////////////////////////////
// Type library ID and version

IMPLEMENT_OLETYPELIB(CEasyControlCtrl, _tlid, _wVerMajor, _wVerMinor)


/////////////////////////////////////////////////////////////////////////////
// Interface IDs

const IID BASED_CODE IID_DEasyControl =
        { 0xaedd17eb, 0xa11a, 0x4fed, { 0xa6, 0x8b, 0xfe, 0x7a, 0x3a, 0xc5, 0xc9, 0x3e } };
const IID BASED_CODE IID_DEasyControlEvents =
        { 0x2baf88e9, 0xc1d9, 0x4b54, { 0xb4, 0x66, 0xf5, 0x60, 0xe3, 0xd1, 0x9a, 0x79 } };


/////////////////////////////////////////////////////////////////////////////
// Control type information

static const DWORD BASED_CODE _dwEasyControlOleMisc =
    OLEMISC_ACTIVATEWHENVISIBLE |
    OLEMISC_SETCLIENTSITEFIRST |
    OLEMISC_INSIDEOUT |
    OLEMISC_CANTLINKINSIDE |
    OLEMISC_RECOMPOSEONRESIZE;

IMPLEMENT_OLECTLTYPE(CEasyControlCtrl, IDS_EASYCONTROL, _dwEasyControlOleMisc)


/////////////////////////////////////////////////////////////////////////////
// CEasyControlCtrl::CEasyControlCtrlFactory::UpdateRegistry -
// Adds or removes system registry entries for CEasyControlCtrl

BOOL CEasyControlCtrl::CEasyControlCtrlFactory::UpdateRegistry(BOOL bRegister)
{
    // TODO: Verify that your control follows apartment-model threading rules.
    // Refer to MFC TechNote 64 for more information.
    // If your control does not conform to the apartment-model rules, then
    // you must modify the code below, changing the 6th parameter from
    // afxRegApartmentThreading to 0.

    if (bRegister)
        return AfxOleRegisterControlClass(
            AfxGetInstanceHandle(),
            m_clsid,
            m_lpszProgID,
            IDS_EASYCONTROL,
            IDB_EASYCONTROL,
            afxRegApartmentThreading,
            _dwEasyControlOleMisc,
            _tlid,
            _wVerMajor,
            _wVerMinor);
    else
        return AfxOleUnregisterClass(m_clsid, m_lpszProgID);
}


/////////////////////////////////////////////////////////////////////////////
// CEasyControlCtrl::CEasyControlCtrl - Constructor

CEasyControlCtrl::CEasyControlCtrl()
{
    InitializeIIDs(&IID_DEasyControl, &IID_DEasyControlEvents);

    EASYLIB_CREATEOBJ(CMoveTracker, m_pMoveTracker, CMoveTracker)
    m_InitOK = false;
    m_WheelZoomable = TRUE;
    m_PartialSelect = TRUE;
    m_SelectAppend = FALSE;
    m_TrackFeatureType = TrackFeatureType_UNKNOWN;
    m_WindowDC = NULL;
    m_MappingLoad = true;
    EASYLIB_CREATEOBJ(CGeoMap, m_pMap, CMap)
    this->ReMap();

    _easycontrol_create_default_symbol(easymap::SYMBOLTYPE_POINT, m_pTrackPointSymbol);
    _easycontrol_create_default_symbol(easymap::SYMBOLTYPE_LINE, m_pTrackLineSymbol);
    _easycontrol_create_default_symbol(easymap::SYMBOLTYPE_FILL, m_pTrackFillSymbol);
    _easycontrol_create_default_symbol(easymap::SYMBOLTYPE_TEXT, m_pTrackTextSymbol);

    _easycontrol_create_default_symbol(easymap::SYMBOLTYPE_POINT, m_pHighlightPointSymbol);
    _easycontrol_create_default_symbol(easymap::SYMBOLTYPE_LINE, m_pHighlightLineSymbol);
    _easycontrol_create_default_symbol(easymap::SYMBOLTYPE_FILL, m_pHighlightFillSymbol);
}


/////////////////////////////////////////////////////////////////////////////
// CEasyControlCtrl::~CEasyControlCtrl - Destructor

CEasyControlCtrl::~CEasyControlCtrl()
{
    m_pMap->ClearAllData();
    m_pMap.Clear();
    if (m_WindowDC)
    {
        ::ReleaseDC(m_hWnd, m_WindowDC);
    }
}

void CEasyControlCtrl::ReMap()
{
    CAST_PTR(m_pMap, m_pAV, CActiveView)
    m_pAV->GetDisplay(m_pDisplay);
    m_pDisplay->GetDisplayTransformation(m_pDT);
    m_BackColor = m_pDisplay->GetBackgroundColor();
    m_pMoveTracker->ClearGeometry();
    m_pMoveTracker->ClearDisplay();
    m_pMoveTracker->SetDisplay(m_pDisplay._p());
}

/////////////////////////////////////////////////////////////////////////////
// CEasyControlCtrl::OnDraw - Drawing function

void CEasyControlCtrl::OnDraw(
            CDC* pdc, const CRect& rcBounds, const CRect& rcInvalid)
{
    m_pAV->RefreshWindow();
    HDC dc = ::GetDC(m_hWnd);

    //绘制高亮
    m_pHighlightPointSymbol->Prepare(dc, m_pDT._p());
    m_pHighlightLineSymbol->Prepare(dc, m_pDT._p());
    m_pHighlightFillSymbol->Prepare(dc, m_pDT._p());
    vector<easymap::IGeometryPtr>::const_iterator it = m_Highlights.begin();
    while (it != m_Highlights.end())
    {
        switch ((*it)->GetGeometryType())
        {
        case easymap::GEOMETRYTYPE_POINT:
        case easymap::GEOMETRYTYPE_MULTIPOINT:
            m_pHighlightPointSymbol->Draw((*it)._p());
            break;

        case easymap::GEOMETRYTYPE_PATH:
        case easymap::GEOMETRYTYPE_POLYLINE:
            m_pHighlightLineSymbol->Draw((*it)._p());
            break;

        case easymap::GEOMETRYTYPE_RING:
        case easymap::GEOMETRYTYPE_POLYGON:
        case easymap::GEOMETRYTYPE_ENVELOPE:
        case easymap::GEOMETRYTYPE_CIRCLE:
        case easymap::GEOMETRYTYPE_ELLIPSE:
            m_pHighlightFillSymbol->Draw((*it)._p());
            break;

        default:
            {}
        }
        it++;
    }

    this->FireOnPaint((long)dc);
    ::ReleaseDC(m_hWnd, dc);
}


/////////////////////////////////////////////////////////////////////////////
// CEasyControlCtrl::DoPropExchange - Persistence support

void CEasyControlCtrl::DoPropExchange(CPropExchange* pPX)
{
    ExchangeVersion(pPX, MAKELONG(_wVerMinor, _wVerMajor));
    COleControl::DoPropExchange(pPX);

    // TODO: Call PX_ functions for each persistent custom property.

}


/////////////////////////////////////////////////////////////////////////////
// CEasyControlCtrl::OnResetState - Reset control to default state

void CEasyControlCtrl::OnResetState()
{
    COleControl::OnResetState();  // Resets defaults found in DoPropExchange

    // TODO: Reset any other control state here.
}


/////////////////////////////////////////////////////////////////////////////
// CEasyControlCtrl::AboutBox - Display an "About" box to the user

void CEasyControlCtrl::AboutBox()
{
    CDialog dlgAbout(IDD_ABOUTBOX_EASYCONTROL);
    dlgAbout.DoModal();
}


/////////////////////////////////////////////////////////////////////////////
// CEasyControlCtrl message handlers

void CEasyControlCtrl::OnSize(UINT nType, int cx, int cy) 
{
    COleControl::OnSize(nType, cx, cy);
    
    this->MainSpaceResize();
}

void CEasyControlCtrl::MainSpaceResize()
{
    if (!m_InitOK)
    {
        return;
    }

    //重新绑定窗体大小，重建窗体/实地坐标映射
    tagRECT rect;
    ::GetWindowRect(this->m_hWnd, &rect);
    rect.right = rect.right - rect.left;
    rect.bottom = rect.bottom - rect.top;
    rect.left = 0;
    rect.top = 0;

    m_pAV->LostFocus();
    m_pAV->GainFocus(m_WindowDC, rect);

    this->UpdateView();
}

void CEasyControlCtrl::OnShowWindow(BOOL bShow, UINT nStatus) 
{
    COleControl::OnShowWindow(bShow, nStatus);

    m_WindowDC = ::GetDC(m_hWnd);
    m_InitOK = true;
    this->MainSpaceResize();
    this->SetActionNone();
}

BOOL CEasyControlCtrl::NewWorkspace()
{
    m_pMap->ClearAllData();
    m_pMap->SetName("My Map");
    this->UpdateView();
    return TRUE;
}

BOOL CEasyControlCtrl::LoadWorkspace(LPCTSTR filename) 
{
    string tws = filename;
    //从文件中读出stream，然后对象化成geomap
    easymap::CStreamPtr pStream;
    EASYLIB_CREATEOBJ(CMemoryStream, pStream, CStream)
    pStream->LoadFromFile(tws.c_str());

    //原先的干掉
    m_pMap->ClearAllData();
    m_pMap.Clear();

    easymap::CPersistPtr pPersist;
    easymap::CPersist::Instantiate(pStream, pPersist);
    CAST_PTR(pPersist, m_pMap, CMap)

    if (m_pMap.Assigned())
    {
        //重新搞一下
        this->ReMap();

        tagRECT rect;
        ::GetWindowRect(this->m_hWnd, &rect);
        rect.right = rect.right - rect.left;
        rect.bottom = rect.bottom - rect.top;
        rect.left = 0;
        rect.top = 0;

        m_pAV->LostFocus();
        m_pAV->GainFocus(m_WindowDC, rect);

        easymap::WKSRect extent;
        m_pDT->GetVisibleExtent(extent);
        this->FireViewExtentChange(extent.left, extent.bottom, extent.right, extent.top);

        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

BOOL CEasyControlCtrl::SaveWorkspace(LPCTSTR filename) 
{
    easymap::IStreamXPtr pStream;
    EASYLIB_CREATEOBJ(CMemoryStream, pStream, IStreamX)

    easymap::IPersistPtr pPersist;
    CAST_PTR(m_pMap, pPersist, IPersist)

    pPersist->Dump(pStream._p());
    bool r = pStream->SaveToFile(filename);

    return r ? TRUE:FALSE;
}

void CEasyControlCtrl::UpdateView() 
{
    //重新读取数据，刷新当前显示内容
    m_pAV->UpdateData();
    m_pAV->UpdateSelection();

    m_pAV->RefreshWindow();
    ::SendMessage(m_hWnd, WM_PAINT, 0, 0);
}

void CEasyControlCtrl::FullExtentView() 
{
    easymap::WKSRect extent;
    m_pMap->GetFullExtent(extent);
    m_pDT->SetVisibleExtent(extent);
    this->UpdateView();

    this->FireViewExtentChange(extent.left, extent.bottom, extent.right, extent.top);
}

void CEasyControlCtrl::GetMapCenter(double x, double y) 
{
    easymap::WKSPoint pnt;
    m_pDT->GetMapCenter(pnt);
    x = pnt.x;
    y = pnt.y;
}

void CEasyControlCtrl::SetMapCenter(double x, double y) 
{
    m_pDT->SetMapCenter(easymap::WKSPoint(x, y));

    easymap::WKSRect extent;
    m_pDT->GetVisibleExtent(extent);
    this->FireViewExtentChange(extent.left, extent.bottom, extent.right, extent.top);
}

void CEasyControlCtrl::SetScale(double scale) 
{
    m_pDT->SetMapScale(scale);

    easymap::WKSRect extent;
    m_pDT->GetVisibleExtent(extent);
    this->FireViewExtentChange(extent.left, extent.bottom, extent.right, extent.top);
}

double CEasyControlCtrl::GetScale() 
{
    double scale;
    m_pDT->GetMapScale(scale);
    return scale;
}

void CEasyControlCtrl::SetViewExtent(double minx, double miny, double maxx, double maxy) 
{
    easymap::WKSRect extent;
    extent.left = minx;
    extent.bottom = miny;
    extent.right = maxx;
    extent.top = maxy;
    m_pDT->SetVisibleExtent(extent);

    this->FireViewExtentChange(extent.left, extent.bottom, extent.right, extent.top);
}

void CEasyControlCtrl::GetViewExtent(double FAR* minx, double FAR* miny, double FAR* maxx, double FAR* maxy) 
{
    easymap::WKSRect extent;
    m_pDT->GetVisibleExtent(extent);

    *minx = extent.left;
    *miny = extent.bottom;
    *maxx = extent.right;
    *maxy = extent.top;
}

void CEasyControlCtrl::SetActionNone() 
{
    m_MouseAction = ActionDefault;
    m_TrackPoints.clear();
    m_pTrackFeatureLayer.Clear();
    m_pTrackFeature.Clear();
    m_TrackAnnotation = "";
    m_pTrackElementLayer.Clear();
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_NORMAL);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
    m_pMoveTracker->Finish();
    m_pMoveTracker->ClearGeometry();
    this->ReleaseCapture();
}

void CEasyControlCtrl::SetActionZoomin() 
{
    this->SetActionNone();
    m_MouseAction = ActionZoomIn;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_ZOOMIN);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CEasyControlCtrl::SetActionZoomout() 
{
    this->SetActionNone();
    m_MouseAction = ActionZoomOut;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_ZOOMOUT);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CEasyControlCtrl::SetActionPan() 
{
    this->SetActionNone();
    m_MouseAction = ActionPan;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_PAN);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);

}

void CEasyControlCtrl::OnLButtonDown(UINT nFlags, CPoint point) 
{
    if (!m_InitOK) return;

    easymap::WKSPoint wpnt;
    m_pDT->Device2Map(point, wpnt);
    easymap::CScreenBrowserPtr pScreenBrowser;
    CAST_PTR(m_pDisplay, pScreenBrowser, CScreenBrowser);
    HCURSOR cursor = NULL;

    switch(m_MouseAction)
    {
    case ActionPan:
        //pan拖动起点
        pScreenBrowser->PanStart(point);
        cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_PANNING);
        ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
        ::SetCursor(cursor);
        this->SetCapture();
        break;

    case ActionSelectByPoint:
        m_pMap->SelectByPoint(wpnt, m_SelectAppend ? true:false);
        m_TrackPoints.clear();
        this->UpdateView();
        break;

    case ActionDeselectByPoint:
        m_pMap->DeselectByPoint(wpnt);
        m_TrackPoints.clear();
        this->UpdateView();
        break;

    case ActionZoomIn:
    case ActionZoomOut:
    case ActionSelectByEnvelope:
    case ActionDeselectByEnvelope:
        m_TrackPoints.clear();
        m_TrackPoints.push_back(point);
        break;

    case ActionTrackFeature:
        this->TrackFeatureLButtonDown(nFlags, point);
        break;

    case ActionTrackPointElement:
        this->NewPointOrTextElement(point, "");
        break;

    case ActionTrackTextElement:
        this->NewPointOrTextElement(point, m_TrackAnnotation);
        break;

    case ActionMoveSelectedObjects:
        this->MoveObjectsStart(point);
        break;

    case ActionTrackEnvelopeElement:
    case ActionTrackCircleElement:
    case ActionTrackEllipseElement:
    case ActionTrackPolylineElement:
    case ActionTrackPolygonElement:
    case ActionTrackFreehandLineElement:
    case ActionTrackFreehandFillElement:
        m_TrackPoints.push_back(point);
        break;

    default:
        {}
    }

    COleControl::OnLButtonDown(nFlags, point);
}

void CEasyControlCtrl::OnMouseMove(UINT nFlags, CPoint point) 
{
    if (!m_InitOK) return;

    easymap::CScreenBrowserPtr pScreenBrowser;
    CAST_PTR(m_pDisplay, pScreenBrowser, CScreenBrowser)

    COLORREF linecolor =  RGB(0, 255, 0);
    COLORREF fillcolor =  RGB(220, 220, 160);

    switch(m_MouseAction)
    {
    case ActionPan:
        //pan拖动中...
        pScreenBrowser->PanMoveTo(point);
        break;

    case ActionZoomIn:
    case ActionZoomOut:
    case ActionSelectByEnvelope:
    case ActionDeselectByEnvelope:
        if (0 < m_TrackPoints.size())
        {
            COLORREF bgcolor = m_pDisplay->GetBackgroundColor();
            m_pAV->RefreshWindow();
            ::TrackEnvelope(m_WindowDC, InvertRGB(bgcolor), R2_COPYPEN,
                1, m_TrackPoints[0], point);
        }
        break;

    case ActionTrackFeature:
        this->TrackFeatureMouseMove(nFlags, point);
        break;

    case ActionMoveSelectedObjects:
        this->MoveObjectsMoving(point);
        break;

    case ActionTrackEnvelopeElement:
        if (m_TrackPoints.size() == 1)
        {
            m_pAV->RefreshWindow();
            ::DrawEnvelope(m_WindowDC, R2_COPYPEN, fillcolor, linecolor,
                1, m_TrackPoints[0], point);
        }
        break;

    case ActionTrackCircleElement:
        if (m_TrackPoints.size() == 1)
        {
            m_pAV->RefreshWindow();
            long a = (m_TrackPoints[0].x - point.x);
            double b = (m_TrackPoints[0].y - point.y);
            b = b*b + a*a;
            ::DrawCircle(m_WindowDC, R2_COPYPEN, fillcolor, linecolor,
                1, m_TrackPoints[0], ::sqrt(b));
        }
        break;

    case ActionTrackEllipseElement:
        if (m_TrackPoints.size() == 1)
        {
            m_pAV->RefreshWindow();
            ::DrawEllipse(m_WindowDC, R2_COPYPEN, fillcolor, linecolor,
                1, m_TrackPoints[0], point);
        }
        break;

    case ActionTrackPolylineElement:
    case ActionTrackPolygonElement:
        if (m_TrackPoints.size() > 0)
        {
            m_pAV->RefreshWindow();
            m_TrackPoints.push_back(point);
            if (ActionTrackPolylineElement == m_MouseAction)
            {
                DrawPolyline(m_WindowDC, R2_COPYPEN, linecolor, 1, m_TrackPoints);
            }
            else
            {
                DrawPolygon(m_WindowDC, R2_COPYPEN, fillcolor, linecolor, 2, m_TrackPoints);
            }
            m_TrackPoints.pop_back();
        }
        break;

    case ActionTrackFreehandLineElement:
    case ActionTrackFreehandFillElement:
        if (m_TrackPoints.size() >= 1)
        {
            m_TrackPoints.push_back(point);
            m_pAV->RefreshWindow();

            if (ActionTrackFreehandFillElement == m_MouseAction)
            {
                DrawPolygon(m_WindowDC, R2_COPYPEN, fillcolor, linecolor, 1, m_TrackPoints);
            }
            else
            {
                DrawPolyline(m_WindowDC, R2_COPYPEN, linecolor, 2, m_TrackPoints);
            }
        }
        break;

    default:
        {}
    }

    COleControl::OnMouseMove(nFlags, point);
}

void CEasyControlCtrl::OnLButtonUp(UINT nFlags, CPoint point) 
{
    if (!m_InitOK) return;

    easymap::CScreenBrowserPtr pScreenBrowser;
    CAST_PTR(m_pDisplay, pScreenBrowser, CScreenBrowser);

    tagRECT rect, rect1;
    easymap::WKSRect selectext, extent;
    if (m_TrackPoints.size() > 0)
    {
        rect.left = m_TrackPoints[0].x;
        rect.top = m_TrackPoints[0].y;
        rect.right = point.x;
        rect.bottom = point.y;

        rect1 = rect;
        if ((rect.left == rect.right) && (rect.top == rect.bottom))
        {
            rect1.left = rect.left - 2;
            rect1.top = rect.top - 2;
            rect1.right = rect.right + 2;
            rect1.bottom = rect.bottom + 2;
        }

        m_pDT->Device2Map_Envelope(rect1, selectext);
    }

    switch(m_MouseAction)
    {
    case ActionPan:
        {
            //结束漫游
            pScreenBrowser->PanMoveTo(point);
            pScreenBrowser->PanStop();
            HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_PAN);
            ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);            
            ::SetCursor(cursor);
            this->ReleaseCapture();
            this->UpdateView();
            m_pDT->GetVisibleExtent(extent);
            this->FireViewExtentChange(extent.left, extent.bottom, extent.right, extent.top);
        }
        break;

    case ActionZoomIn:
        if (m_TrackPoints.size() > 0)
        {
            pScreenBrowser->VisibleExtentIn(rect);
        }
        this->UpdateView();
        m_TrackPoints.clear();
        m_pDT->GetVisibleExtent(extent);
        this->FireViewExtentChange(extent.left, extent.bottom, extent.right, extent.top);
        break;

    case ActionZoomOut:
        if (m_TrackPoints.size() > 0)
        {
            pScreenBrowser->VisibleExtentOut(rect);
        }
        this->UpdateView();
        m_TrackPoints.clear();
        m_pDT->GetVisibleExtent(extent);
        this->FireViewExtentChange(extent.left, extent.bottom, extent.right, extent.top);
        break;

    case ActionSelectByEnvelope:
        m_pMap->Select(selectext, m_PartialSelect ? true:false, m_SelectAppend ? true:false);
        m_TrackPoints.clear();
        this->UpdateView();
        break;

    case ActionDeselectByEnvelope:
        m_pMap->Deselect(selectext, m_PartialSelect ? true:false);
        m_TrackPoints.clear();
        this->UpdateView();
        break;

    case ActionMoveSelectedObjects:
        this->MoveObjectsFinish(point);
        break;

    case ActionTrackEnvelopeElement:
    case ActionTrackCircleElement:
    case ActionTrackEllipseElement:
        if (m_TrackPoints.size() == 1)
        {
            m_TrackPoints.push_back(point);
            this->NewElement();
        }
        m_TrackPoints.clear();
        break;

    case ActionTrackFreehandLineElement:
    case ActionTrackFreehandFillElement:
        m_TrackPoints.push_back(point);
        this->NewElement();
        m_TrackPoints.clear();
        break;

    default:
        {}
    }

    COleControl::OnLButtonUp(nFlags, point);
}

void CEasyControlCtrl::OnRButtonDown(UINT nFlags, CPoint point) 
{
    switch(m_MouseAction)
    {
    case ActionTrackFeature:
        this->TrackFeatureRButtonDown(nFlags, point);
        break;

    case ActionTrackEnvelopeElement:
    case ActionTrackCircleElement:
    case ActionTrackEllipseElement:
    case ActionTrackFreehandLineElement:
    case ActionTrackFreehandFillElement:
        m_TrackPoints.clear();
        break;

    case ActionTrackPolylineElement:
    case ActionTrackPolygonElement:
        this->NewElement();
        m_TrackPoints.clear();
        break;

    default:
        {}
    }

	COleControl::OnRButtonDown(nFlags, point);
}

BOOL CEasyControlCtrl::OnMouseWheel(UINT nFlags, short zDelta, CPoint pt) 
{
    if (!m_InitOK) return FALSE;

    if (m_WheelZoomable)
    {
        double scale;
        m_pDT->GetMapScale(scale);
        if (0 < zDelta)
        {
            scale += scale/4;
        }
        else
        {
            scale -= scale/4;
        }

        m_pDT->SetMapScale(scale);
        this->UpdateView();
    }

    return COleControl::OnMouseWheel(nFlags, zDelta, pt);
}

void CEasyControlCtrl::ClearLayers() 
{
    m_pMap->ClearLayers();
}

void CEasyControlCtrl::GetFullExtent(double FAR* minx, double FAR* miny, double FAR* maxx, double FAR* maxy) 
{
    easymap::WKSRect extent;
    m_pMap->GetFullExtent(extent);
    *minx = extent.left;
    *miny = extent.bottom;
    *maxx = extent.right;
    *maxy = extent.top;
}

LPUNKNOWN CEasyControlCtrl::GetLayer(long index) 
{
    if ((index < 0) || (index >= m_pMap->GetLayerCount()))
        return NULL;

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyLayer);
    CEasyLayer* pEasyLayer = (class CEasyLayer*)(pRTC->CreateObject());
    m_pMap->GetLayer(pEasyLayer->m_pLayer._ref(), index);

    IDispatch* pDispatch = pEasyLayer->GetIDispatch(FALSE);
    LPUNKNOWN pOutLayer = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pOutLayer);
    pDispatch->Release();
    return pOutLayer;
}

void CEasyControlCtrl::Window2Map(long lx, long ly, double FAR* dx, double FAR* dy) 
{
    m_pDT->Device2MapXY(lx, ly, *dx, *dy);
}

void CEasyControlCtrl::Map2Window(double dx, double dy, long FAR* lx, long FAR* ly) 
{
    m_pDT->Map2DeviceXY(dx, dy, *lx, *ly);
}

void CEasyControlCtrl::OnBackColorChanged() 
{
    m_pDisplay->SetBackgroundColor(m_BackColor);
    m_pAV->RefreshWindow();

	SetModifiedFlag();
}

long CEasyControlCtrl::GetLayerCount() 
{
	return m_pMap->GetLayerCount();
}

long CEasyControlCtrl::GetAllLayerCount() 
{
	return m_pMap->GetAllCount();
}

BOOL CEasyControlCtrl::AddLayerInner(LPUNKNOWN grouplayer, easymap::ILayerPtr pLayer,
    LPUNKNOWN FAR* newlayer)
{
    *newlayer = NULL;
    CRuntimeClass* pRTC = NULL;
    CEasyLayer* pEasyNewLayer = NULL;
    IDispatch* pDispatch = NULL;
    IUnknown* pUnknown = NULL;
    BOOL r;
    if (grouplayer)
    {
        IEasyLayer* pEasyGroupLayer = NULL;
        grouplayer->QueryInterface(IID_IEasyLayer, (void**)&pEasyGroupLayer);
        grouplayer->Release();
        if (pEasyGroupLayer->GetLayerType() != EASYLAYERTYPE_GROUPLAYER)
        {
            return FALSE;
        }

        pRTC = RUNTIME_CLASS(CEasyLayer);
        pEasyNewLayer = (class CEasyLayer*)(pRTC->CreateObject());
        pEasyNewLayer->m_pLayer = pLayer;
        pDispatch = pEasyNewLayer->GetIDispatch(FALSE);
        pDispatch->QueryInterface(IID_IUnknown, (void**)&pUnknown);
        pDispatch->Release(); pDispatch = NULL;
        r = pEasyGroupLayer->AddSubLayer(pUnknown);
        if (r)
        {
            *newlayer = pUnknown;
        }
        else
        {
            pUnknown->Release();
        }
    }
    else
    {
        r = m_pMap->AddLayer(pLayer) ? TRUE:FALSE;
        if (r)
        {
            pRTC = RUNTIME_CLASS(CEasyLayer);
            pEasyNewLayer = (class CEasyLayer*)(pRTC->CreateObject());
            pEasyNewLayer->m_pLayer = pLayer;
            pDispatch = pEasyNewLayer->GetIDispatch(FALSE);
            pDispatch->QueryInterface(IID_IUnknown, (void**)&pUnknown);
            pDispatch->Release(); pDispatch = NULL;
            *newlayer = pUnknown;
        }
    }

    return r;
}

LPUNKNOWN CEasyControlCtrl::LoadSlimDataEx(LPUNKNOWN grouplayer, LPCTSTR filename,
    BOOL readonly) 
{
    if (!filename)
        return NULL;

    bool ro = readonly ? true:false;

    easymap::ILayerPtr pLayer;
    if (!easymap::LoadSlimData(filename, pLayer._ref(), ro, m_MappingLoad))
        return NULL;

    LPUNKNOWN pOutLayer = NULL;
	this->AddLayerInner(grouplayer, pLayer, &pOutLayer);
    return pOutLayer;
}

LPUNKNOWN CEasyControlCtrl::LoadShapeFileEx(LPUNKNOWN grouplayer, LPCTSTR filename, long mapunit,
    double basescale, double precision, long indexlevel, BOOL readonly) 
{
    if (!filename)
        return NULL;

    bool ro = readonly ? true:false;

    easymap::ILayerPtr pLayer;
    if (!easymap::LoadShapeFile(filename, pLayer._ref(), mapunit, basescale, precision,
        indexlevel, ro))
        return NULL;

    LPUNKNOWN pOutLayer = NULL;
    this->AddLayerInner(grouplayer, pLayer, &pOutLayer);
    return pOutLayer;
}

LPUNKNOWN CEasyControlCtrl::LoadOrientBmpEx(LPUNKNOWN grouplayer, LPCTSTR filename) 
{
    if (!filename)
        return NULL;

    easymap::CBitmapLayerPtr pBL;
    EASYLIB_CREATEOBJ(CBitmapLayer, pBL, CBitmapLayer)
    if (!pBL->LoadBmpFile(filename, true))
        return NULL;

    easymap::ILayerPtr pLayer;
    CAST_PTR(pBL, pLayer, ILayer)
    LPUNKNOWN pOutLayer = NULL;
    this->AddLayerInner(grouplayer, pLayer, &pOutLayer);
    return pOutLayer;
}

LPUNKNOWN CEasyControlCtrl::NewGroupLayerEx(LPUNKNOWN parentgrouplayer, LPCTSTR newlayername) 
{
    if (!newlayername)
        return NULL;

    easymap::CGroupLayerPtr pNewGL;
    EASYLIB_CREATEOBJ(CGroupLayer, pNewGL, CGroupLayer)
    easymap::ILayerPtr pLayer;
    CAST_PTR(pNewGL, pLayer, ILayer)
    string layername;
    if (newlayername) layername = newlayername;
    pLayer->SetName(layername.c_str());
    LPUNKNOWN pOutLayer = NULL;
    this->AddLayerInner(parentgrouplayer, pLayer, &pOutLayer);
    return pOutLayer;
}

BOOL CEasyControlCtrl::LoadSlimData(LPCTSTR filename, BOOL readonly) 
{
    if (!filename)
        return FALSE;

    bool ro = readonly ? true:false;

    easymap::ILayerPtr pLayer;
    if (!easymap::LoadSlimData(filename, pLayer._ref(), ro, m_MappingLoad))
        return FALSE;

    return m_pMap->AddLayer(pLayer) ? TRUE:FALSE;
}

BOOL CEasyControlCtrl::LoadShapeFile(LPCTSTR filename, long mapunit, double basescale, double precision, long indexlevel, BOOL readonly) 
{
    if (!filename)
        return FALSE;

    bool ro = readonly ? true:false;
    easymap::ILayerPtr pLayer;
    if (!easymap::LoadShapeFile(filename, pLayer._ref(), mapunit, basescale, precision,
        indexlevel, ro))
        return FALSE;

	return m_pMap->AddLayer(pLayer) ? TRUE:FALSE;
}

BOOL CEasyControlCtrl::LoadOrientBmp(LPCTSTR filename) 
{
    if (!filename)
        return FALSE;

    easymap::CBitmapLayerPtr pBL;
    EASYLIB_CREATEOBJ(CBitmapLayer, pBL, CBitmapLayer)
    if (!pBL->LoadBmpFile(filename, true))
        return FALSE;

    easymap::ILayerPtr pLayer;
    CAST_PTR(pBL, pLayer, ILayer)
	return m_pMap->AddLayer(pLayer) ? TRUE:FALSE;
}

BOOL CEasyControlCtrl::NewElementLayer(LPCTSTR newlayername) 
{
    if (!newlayername)
        return FALSE;

    easymap::CElementLayerPtr pNewEL;
    EASYLIB_CREATEOBJ(CElementLayer, pNewEL, CElementLayer)
    easymap::ILayerPtr pLayer;
    CAST_PTR(pNewEL, pLayer, ILayer)
    string layername;
    if (newlayername) layername = newlayername;
    pLayer->SetName(layername.c_str());
    return m_pMap->AddLayer(pLayer) ? TRUE:FALSE;
}

LPUNKNOWN CEasyControlCtrl::NewElementLayerEx(LPUNKNOWN grouplayer, LPCTSTR newlayername) 
{
    if (!newlayername)
        return NULL;

    easymap::CElementLayerPtr pNewEL;
    EASYLIB_CREATEOBJ(CElementLayer, pNewEL, CElementLayer)
    easymap::ILayerPtr pLayer;
    CAST_PTR(pNewEL, pLayer, ILayer)
    string layername;
    if (newlayername) layername = newlayername;
    pLayer->SetName(layername.c_str());
    LPUNKNOWN pOutLayer = NULL;
    this->AddLayerInner(grouplayer, pLayer, &pOutLayer);
    return pOutLayer;
}

void CEasyControlCtrl::SetMapUnit(long mapunit) 
{
    m_pDT->SetMapUnit(mapunit);
}

long CEasyControlCtrl::GetMapUnit() 
{
	return m_pDT->GetMapUnit();
}

void CEasyControlCtrl::SetReferenceScale(double refscale) 
{
    m_pDT->SetReferenceScale(refscale);
}

double CEasyControlCtrl::GetReferenceScale() 
{
    double refscale = 0;
    m_pDT->GetReferenceScale(refscale);
	return refscale;
}

BOOL CEasyControlCtrl::SetLayerOrder(LPUNKNOWN layer, long neworder) 
{
    if (!layer)
    	return FALSE;

    IEasyLayer* pEasyLayer = NULL;
    layer->QueryInterface(IID_IEasyLayer, (void**)&pEasyLayer);
    if (!pEasyLayer)
        return FALSE;
    layer->Release();
    easymap::ILayerPtr pLayer = (easymap::ILayer*)pEasyLayer->innergetrawlayerptr(FALSE);
    return m_pMap->SetLayerOrder(pLayer, neworder) ? TRUE:FALSE;
}

int CEasyControlCtrl::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (COleControl::OnCreate(lpCreateStruct) == -1)
		return -1;
	
    if (!easymap::symboluidll::getDLLHandle())
    {

        TCHAR fullname[MAX_PATH];
        ::GetModuleFileName(::AfxGetApp()->m_hInstance, fullname, MAX_PATH);
        CString dllname = fullname;
        int slashpos = dllname.ReverseFind('\\');
        dllname = dllname.Left(slashpos + 1) + "SymbolUI.dll";

        string symboluifile = dllname;
        easymap::symboluidll::loaddll(symboluifile.c_str());
    }
	
	return 0;
}

void CEasyControlCtrl::OnPartialSelectChanged() 
{
	// TODO: Add notification handler code

	SetModifiedFlag();
}

void CEasyControlCtrl::OnSelectAppendChanged() 
{
	// TODO: Add notification handler code

	SetModifiedFlag();
}

void CEasyControlCtrl::DeleteSelectObjects() 
{
    long layercount = this->GetLayerCount();
    for (long i = 0; i < layercount; i++)
    {
        IUnknown* pU = this->GetLayer(i);
        IEasyLayer* pEasyLayer = NULL;
        pU->QueryInterface(IID_IEasyLayer, (void**)&pEasyLayer);
        pU->Release();
        pEasyLayer->DeleteSelectedObjects();
        pEasyLayer->Release();
    }

    m_pMap->SetUndoPoint("delete selected objects");
}

BOOL CEasyControlCtrl::EditUndo() 
{
    BOOL r = m_pMap->EditUndo() ? TRUE:FALSE;
    if (r)
    {
        this->UpdateView();
    }
    return r;
}

BOOL CEasyControlCtrl::EditRedo() 
{
    BOOL r = m_pMap->EditRedo() ? TRUE:FALSE;
    if (r)
    {
        this->UpdateView();
    }
    return r;
}

BOOL CEasyControlCtrl::EditSave() 
{
    BOOL r = m_pMap->SaveData() ? TRUE:FALSE;
    if (r)
    {
        this->UpdateView();
    }
    return r;
}

BOOL CEasyControlCtrl::EditCancel() 
{
    BOOL r = m_pMap->EditCancel() ? TRUE:FALSE;
    if (r)
    {
        this->UpdateView();
    }
    return r;
}

BOOL CEasyControlCtrl::IsEditDirty() 
{
    BOOL r = m_pMap->IsDirty() ? TRUE:FALSE;
    if (r)
    {
        this->UpdateView();
    }
    return r;
}

BOOL CEasyControlCtrl::SetEditUndoPoint(LPCTSTR desc) 
{
    string s;
    if (desc)
    {
        s = CString(desc);
    }
    return m_pMap->SetUndoPoint(s.c_str()) ? TRUE:FALSE;
}

long CEasyControlCtrl::GetCurrentAction() 
{
    return this->m_MouseAction;
}

void CEasyControlCtrl::SelectByPoint() 
{
    this->SetActionNone();
    m_MouseAction = ActionSelectByPoint;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CEasyControlCtrl::SelectByEnvelope() 
{
    this->SetActionNone();
    m_MouseAction = ActionSelectByEnvelope;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CEasyControlCtrl::DeselectByPoint() 
{
    this->SetActionNone();
    m_MouseAction = ActionDeselectByPoint;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CEasyControlCtrl::DeselectByEnvelope() 
{
    this->SetActionNone();
    m_MouseAction = ActionDeselectByEnvelope;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

BOOL CEasyControlCtrl::EditTrackFeature(LPUNKNOWN layer, LPCTSTR text) 
{
    this->SetActionNone();
    m_pTrackFeatureLayer.Clear();
    m_TrackAnnotation = "";
    m_TrackFeatureType = TrackFeatureType_UNKNOWN;
    if (!layer || !text)
    	return FALSE;

    IEasyLayer* pEasyLayer = NULL;
    layer->QueryInterface(IID_IEasyLayer, (void**)&pEasyLayer);
    if (!pEasyLayer)
        return FALSE;

    pEasyLayer->Release();
    easymap::ILayerPtr pLayer = (easymap::ILayer*)pEasyLayer->innergetrawlayerptr(FALSE);
    CAST_PTR(pLayer, m_pTrackFeatureLayer, CVectorLayer)
    if (!m_pTrackFeatureLayer.Assigned())
        return FALSE;

    if (m_pTrackFeatureLayer->ReadOnly())
    {
        m_pTrackFeatureLayer.Clear();
        return FALSE;
    }

    m_MouseAction = ActionTrackFeature;
    easymap::GeometryColumnInfo colinfo;
    m_pTrackFeatureLayer->GetGeometryColumnInfo(colinfo);
    if (colinfo.FeatureType == easymap::VECTORFEATURETYPE_TEXT)
    {
        m_TrackAnnotation = text;
        if (m_TrackAnnotation == "")
        {
            m_pTrackFeatureLayer.Clear();
            return FALSE;
        }
        m_TrackFeatureType = TrackFeatureType_ANNOTATION;
    }
    else
    {
        switch (colinfo.ShpType)
        {
        case easymap::SHAPETYPE_POINT:
            m_TrackFeatureType = TrackFeatureType_POINT;
            break;

        case easymap::SHAPETYPE_MULTIPOINT:
            m_TrackFeatureType = TrackFeatureType_MULTIPOINT;
            break;

        case easymap::SHAPETYPE_POLYLINE:
            m_TrackFeatureType = TrackFeatureType_POLYLINE;
            break;

        case easymap::SHAPETYPE_POLYGON:
            m_TrackFeatureType = TrackFeatureType_POLYGON;
            break;

        default:
            m_pTrackFeatureLayer.Clear();
            return FALSE;
        }
    }

    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
	return TRUE;
}

void CEasyControlCtrl::MoveSelectedObjects() 
{
    this->SetActionNone();
    m_MouseAction = ActionMoveSelectedObjects;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CEasyControlCtrl::TrackFeatureLButtonDown(UINT nFlags, CPoint point)
{
    easymap::WKSPoint wpnt;
    m_pDT->Device2Map(point, wpnt);
    easymap::IGeometryPtr pGeometry;
    easymap::IPointPtr pPoint;
    long fid = -1;
    BOOL editsetundopoint = TRUE;
    CString editdesc;

    switch (m_TrackFeatureType)
    {
    case TrackFeatureType_POINT:
    case TrackFeatureType_ANNOTATION:
        m_pTrackFeature.Clear();
        m_pTrackFeatureLayer->CreateFeature(m_pTrackFeature);
        EASYLIB_CREATEOBJ(CPoint, pPoint, IPoint)
        pPoint->SetX(wpnt.x);
        pPoint->SetY(wpnt.y);
        CAST_PTR(pPoint, pGeometry, IGeometry)
        m_pTrackFeature->SetGeometryRef(pGeometry._p());
        m_pTrackFeature->SetAnnotation(m_TrackAnnotation);
        m_pTrackFeature->Update();
        this->UpdateView();
        fid = m_pTrackFeature->GetFID();
        editdesc = "track point feature";
        this->FireNewFeatureTracked(fid, &editsetundopoint);
        if (editsetundopoint)
        {
            m_pMap->SetUndoPoint(editdesc);
        }
        break;

    case TrackFeatureType_MULTIPOINT:
    case TrackFeatureType_POLYLINE:
    case TrackFeatureType_POLYGON:
        m_TrackPoints.push_back(point);
        break;
    }
}

void CEasyControlCtrl::TrackFeatureMouseMove(UINT nFlags, CPoint point)
{
    vector<tagPOINT> pntstmp;

    if (((m_TrackFeatureType == TrackFeatureType_POLYLINE)
        || (m_TrackFeatureType == TrackFeatureType_POLYGON))
        && (m_TrackPoints.size() > 0))
    {
        m_pAV->RefreshWindow();
        DrawPolyline(m_WindowDC, R2_COPYPEN, RGB(200, 30, 40), 1, m_TrackPoints);
        pntstmp.push_back(m_TrackPoints[m_TrackPoints.size()-1]);
        pntstmp.push_back(point);
        if (m_TrackFeatureType == TrackFeatureType_POLYGON)
        {
            pntstmp.push_back(m_TrackPoints[0]);
        }
        DrawPolyline(m_WindowDC, R2_COPYPEN, RGB(200, 30, 40), 1, pntstmp);
    }
}

void PointsToPolyline(vector<tagPOINT>& points, easymap::IGeometryPtr& pGeometry,
    easymap::CDisplayTransformationPtr pDT)
{
    easymap::IPathPtr pPath;
    EASYLIB_CREATEOBJ(CPath, pPath, IPath)
    for (long i = 0; i < points.size(); i++)
    {
        easymap::WKSPointZ pntz;
        pDT->Device2Map(points[i], pntz);
        pPath->AddPoint(pntz);
    }

    easymap::IPolylinePtr pPolyline;
    EASYLIB_CREATEOBJ(CPolyline, pPolyline, IPolyline)
    pPolyline->AddPathRef(pPath._p());
    CAST_PTR(pPolyline, pGeometry, IGeometry)
}

void PointsToPolygon(vector<tagPOINT>& points, easymap::IGeometryPtr& pGeometry,
    easymap::CDisplayTransformationPtr pDT)
{
    easymap::IRingPtr pRing;
    EASYLIB_CREATEOBJ(CRing, pRing, IRing)
    easymap::WKSPointZ pntz;
    for (long i = 0; i < points.size(); i++)
    {
        pDT->Device2Map(points[i], pntz);
        pRing->AddPoint(pntz);
    }
    pDT->Device2Map(points[0], pntz);
    pRing->AddPoint(pntz);

    easymap::IPolygonPtr pPolygon;
    EASYLIB_CREATEOBJ(CPolygon, pPolygon, IPolygon)
    pPolygon->AddRingRef(pRing._p());
    CAST_PTR(pPolygon, pGeometry, IGeometry)
}

void PointsToMultiPoint(vector<tagPOINT>& points, easymap::IGeometryPtr& pGeometry,
    easymap::CDisplayTransformationPtr pDT)
{
    easymap::IMultiPointPtr pMultiPoint;
    EASYLIB_CREATEOBJ(CMultiPoint, pMultiPoint, IMultiPoint)
    easymap::WKSPointZ pntz;
    for (long i = 0; i < points.size(); i++)
    {
        pDT->Device2Map(points[i], pntz);
        pMultiPoint->AddPoint(pntz);
    }

    CAST_PTR(pMultiPoint, pGeometry, IGeometry)
}

void CEasyControlCtrl::TrackFeatureRButtonDown(UINT nFlags, CPoint point)
{
    easymap::IGeometryPtr pGeometry;
    string undopointtext;
    long fid = -1;
    BOOL seteditundopoint = TRUE;

    m_pTrackFeature.Clear();
    if ((m_TrackFeatureType == TrackFeatureType_POLYLINE)
        && (m_TrackPoints.size() > 1))
    {
        m_pTrackFeatureLayer->CreateFeature(m_pTrackFeature);
        PointsToPolyline(m_TrackPoints, pGeometry, m_pDT);
        undopointtext = "track polyline feature";
    }
    else if ((m_TrackFeatureType == TrackFeatureType_POLYGON)
        && (m_TrackPoints.size() > 2))
    {
        m_pTrackFeatureLayer->CreateFeature(m_pTrackFeature);
        PointsToPolygon(m_TrackPoints, pGeometry, m_pDT);
        undopointtext = "track polygon feature";
    }
    else if ((m_TrackFeatureType == TrackFeatureType_MULTIPOINT)
        && (m_TrackPoints.size() > 0))
    {
        m_pTrackFeatureLayer->CreateFeature(m_pTrackFeature);
        PointsToMultiPoint(m_TrackPoints, pGeometry, m_pDT);
        undopointtext = "track multipoint feature";
    }

    if (m_pTrackFeature.Assigned() && pGeometry.Assigned())
    {
        m_pTrackFeature->SetGeometryRef(pGeometry._p());
        m_pTrackFeature->Update();
        this->UpdateView();
        fid = m_pTrackFeature->GetFID();
        this->FireNewFeatureTracked(fid, &seteditundopoint);
        if (seteditundopoint)
        {
            m_pMap->SetUndoPoint(undopointtext.c_str());
        }
    }

    m_TrackPoints.clear();
    m_pAV->RefreshWindow();
}

void CEasyControlCtrl::OnWheelZoomableChanged() 
{
	// TODO: Add notification handler code

	SetModifiedFlag();
}

void GetSelectedGeometries(easymap::ILayerPtr pLayer, easymap::CObjArrayPtr& pGeometryArray)
{
    long i;
    easymap::CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        long layercount = pGL->GetLayerCount();
        for (i = 0; i < layercount; i++)
        {
            easymap::ILayerPtr pSubLayer;
            pGL->GetLayer(pSubLayer._ref(), i);
            GetSelectedGeometries(pSubLayer, pGeometryArray);
        }
        return;
    }

    easymap::IObjPtr pObj;
    easymap::IGeometryPtr pGeometry;
    easymap::CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        vector<DWORD> fids;
        pVL->GetSelection(fids);
        long selectcount = fids.size();
        for (i = 0; i < selectcount; i++)
        {
            easymap::IVectorFeaturePtr pVF;
            pVL->GetFeature(fids[i], pVF);
            pGeometry.Clear();
            pVF->GetGeometryRef(pGeometry._ref());
            pObj.Clear();
            CLONE_PTR(pGeometry, pObj)
            pGeometryArray->Add(pObj._p());
        }

        return;
    }

    easymap::CElementLayerPtr pEL;
    CAST_PTR(pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        easymap::IElementLayerAgentPtr pELA;
        EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
        pELA->SetLayer(pLayer._p());

        easymap::IIntArrayPtr pSelection;
        pELA->GetSelectElements(pSelection._ref());
        long count = pSelection->GetSize();
        for (i = 0; i < count; i++)
        {
            easymap::IElementPtr pElement;
            easymap::IElementLayerAgentPtr pELA;
            EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
            pELA->SetLayer(pLayer._p());
            long item;
            pSelection->GetAt(i, item);
            pELA->GetElement(item, pElement._ref());
            pElement->GetGeometry(pGeometry._ref());
            pObj.Clear();
            CAST_PTR(pGeometry, pObj, IObj)
            pGeometryArray->Add(pObj._p());
        }

        return;
    }
}

void CEasyControlCtrl::MoveObjectsStart(CPoint point)
{
    m_TrackPoints.clear();
    m_TrackPoints.push_back(point);
    m_pMoveTracker->Finish();
    m_pMoveTracker->ClearGeometry();

    easymap::CObjArrayPtr pGeometryArray;
    EASYLIB_CREATEOBJ(CObjArray, pGeometryArray, CObjArray)
    long i = 0;
    long count = m_pMap->GetLayerCount();
    for (; i < count; i++)
    {
        easymap::ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer, i);
        GetSelectedGeometries(pLayer, pGeometryArray);
    }

    count = pGeometryArray->GetSize();
    for (i = 0; i < count; i++)
    {
        easymap::IObjPtr pObj;
        pGeometryArray->GetAt(i, pObj._ref());
        easymap::IGeometryPtr pGeometry;
        CAST_PTR(pObj, pGeometry, IGeometry);
        m_pMoveTracker->AddGeometryRef(pGeometry._p());
    }

    m_pMoveTracker->Start(point.x, point.y);
}

void CEasyControlCtrl::MoveObjectsMoving(CPoint point)
{
    if (m_TrackPoints.size() != 1)
        return;

    if (m_pMoveTracker->Started())
        m_pMoveTracker->MouseMove(point.x, point.y);
}

void CEasyControlCtrl::MoveObjectsFinish(CPoint point)
{
    if (m_TrackPoints.size() != 1)
    {
        m_TrackPoints.clear();
        return;
    }

    easymap::WKSPoint from_pnt, to_pnt;
    m_pDT->Device2Map(m_TrackPoints[0], from_pnt);
    m_pDT->Device2Map(point, to_pnt);
    double delta_x = to_pnt.x - from_pnt.x;
    double delta_y = to_pnt.y - from_pnt.y;

    m_TrackPoints.clear();
    m_pMoveTracker->MouseMove(point.x, point.y);
    m_pMoveTracker->Finish();

    long layercount = this->GetLayerCount();
    for (long i = 0; i < layercount; i++)
    {
        IUnknown* pU = this->GetLayer(i);
        IEasyLayer* pEasyLayer = NULL;
        pU->QueryInterface(IID_IEasyLayer, (void**)&pEasyLayer);
        pU->Release();
        pEasyLayer->MoveSelectedObject(delta_x, delta_y);
    }

    m_pMoveTracker->ClearGeometry();
    BOOL setundopoint = TRUE;
    this->FireObjectsMoved(&setundopoint);
    if (setundopoint)
    {
        m_pMap->SetUndoPoint("selected objects moved");
    }
    this->UpdateView();
}

BOOL CEasyControlCtrl::EditTrackPointElement(LPUNKNOWN layer) 
{
    if (!this->PreEditTrackElement(layer))
        return FALSE;

    m_MouseAction = ActionTrackPointElement;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
	return TRUE;
}

BOOL CEasyControlCtrl::EditTrackEnvelopeElement(LPUNKNOWN layer) 
{
    if (!this->PreEditTrackElement(layer))
        return FALSE;

    m_MouseAction = ActionTrackEnvelopeElement;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
	return TRUE;
}

BOOL CEasyControlCtrl::EditTrackCircleElement(LPUNKNOWN layer) 
{
    if (!this->PreEditTrackElement(layer))
        return FALSE;

    m_MouseAction = ActionTrackCircleElement;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
	return TRUE;
}

BOOL CEasyControlCtrl::EditTrackEllipseElement(LPUNKNOWN layer) 
{
    if (!this->PreEditTrackElement(layer))
        return FALSE;

    m_MouseAction = ActionTrackEllipseElement;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
	return TRUE;
}

BOOL CEasyControlCtrl::EditTrackPolylineElement(LPUNKNOWN layer) 
{
    if (!this->PreEditTrackElement(layer))
        return FALSE;

    m_MouseAction = ActionTrackPolylineElement;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
	return TRUE;
}

BOOL CEasyControlCtrl::EditTrackPolygonElement(LPUNKNOWN layer) 
{
    if (!this->PreEditTrackElement(layer))
        return FALSE;

    m_MouseAction = ActionTrackPolygonElement;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
	return TRUE;
}

BOOL CEasyControlCtrl::EditTrackFreehandLineElement(LPUNKNOWN layer) 
{
    if (!this->PreEditTrackElement(layer))
        return FALSE;

    m_MouseAction = ActionTrackFreehandLineElement;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
	return TRUE;
}

BOOL CEasyControlCtrl::EditTrackFreehandFillElement(LPUNKNOWN layer) 
{
    if (!this->PreEditTrackElement(layer))
        return FALSE;

    m_MouseAction = ActionTrackFreehandFillElement;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
	return TRUE;
}

BOOL CEasyControlCtrl::EditTrackTextElement(LPUNKNOWN layer, LPCTSTR text) 
{
    if (!this->PreEditTrackElement(layer) && text)
        return FALSE;

    m_TrackAnnotation = text;
    m_MouseAction = ActionTrackTextElement;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
	return TRUE;
}

BOOL CEasyControlCtrl::PreEditTrackElement(LPUNKNOWN layer) 
{
    this->SetActionNone();
    if (!layer)
    	return FALSE;

    IEasyLayer* pEasyLayer = NULL;
    layer->QueryInterface(IID_IEasyLayer, (void**)&pEasyLayer);
    if (!pEasyLayer)
        return FALSE;

    pEasyLayer->Release();
    easymap::ILayerPtr pLayer = (easymap::ILayer*)pEasyLayer->innergetrawlayerptr(FALSE);
    CAST_PTR(pLayer, m_pTrackElementLayer, CElementLayer)
    if (!m_pTrackElementLayer.Assigned())
        return FALSE;

    return TRUE;
}

void CEasyControlCtrl::NewPointOrTextElement(CPoint point, CString text)
{
    if (!m_pTrackElementLayer.Assigned())
        return;

    easymap::WKSPoint wpnt;
    m_pDT->Device2Map(point, wpnt);
    easymap::IPointPtr pPoint;
    EASYLIB_CREATEOBJ(CPoint, pPoint, IPoint)
    pPoint->SetX(wpnt.x);
    pPoint->SetY(wpnt.y);
    easymap::IGeometryPtr pGeometry;
    CAST_PTR(pPoint, pGeometry, IGeometry)

    string editdesc;
    easymap::IElementPtr pElement;
    if (text == "")
    {
        EASYLIB_CREATEOBJ(CGeometryElement, pElement, IElement)
        pElement->SetSymbol(m_pTrackPointSymbol._p());
        editdesc = "new point element tracked";
    }
    else
    {
        EASYLIB_CREATEOBJ(CTextElement, pElement, IElement)
        pElement->SetText(text.operator LPCTSTR());
        pElement->SetSymbol(m_pTrackPointSymbol._p());
        editdesc = "new text element tracked";
    }

    pElement->SetGeometry(pGeometry._p());
    easymap::IElementLayerAgentPtr pELA;
    EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
    pELA->SetLayer(m_pTrackElementLayer._p());
    long id = pELA->AddElement(pElement._p());

    BOOL editsetundopoint = TRUE;
    this->FireNewElementTracked(id, &editsetundopoint);
    if (editsetundopoint)
    {
        m_pMap->SetUndoPoint(editdesc.c_str());
    }

    this->UpdateView();
    this->ReleaseCapture();
}

vector<easymap::WKSPointZ> CEasyControlCtrl::TrackPoints2WKSPointZs() const
{
    vector<easymap::WKSPointZ> wkspoints;
    vector<tagPOINT>::const_iterator it = m_TrackPoints.begin();
    while (it != m_TrackPoints.end())
    {
        easymap::WKSPointZ wpntz;
        m_pDT->Device2MapXY(it->x, it->y, wpntz.x, wpntz.y);
        wpntz.z = 0;
        wkspoints.push_back(wpntz);
        it++;
    }

    return wkspoints;
}

void CEasyControlCtrl::NewElement()
{
    if (!m_pTrackElementLayer.Assigned())
        return;

    vector<easymap::WKSPointZ> wkspoints = this->TrackPoints2WKSPointZs();

    long i;
    double a, b;
    easymap::IEnvelopePtr pEnvelope;
    easymap::ICirclePtr pCircle;
    easymap::IEllipsePtr pEllipse;
    easymap::IPathPtr pPath;
    easymap::IRingPtr pRing;
    easymap::IPolylinePtr pPolyline;
    easymap::IPolygonPtr pPolygon;
    easymap::IGeometryPtr pGeometry;
    string editdesc;

    easymap::IElementPtr pElement;
    EASYLIB_CREATEOBJ(CGeometryElement, pElement, IElement)
    switch(m_MouseAction)
    {
    case ActionTrackEnvelopeElement:
        if (wkspoints.size() != 2)
            return;
        EASYLIB_CREATEOBJ(CEnvelope, pEnvelope, IEnvelope)
        EASYLIB_CREATEOBJ(CEnvelope, pEnvelope, IEnvelope)
        pEnvelope->SetMinX(wkspoints[0].x);
        pEnvelope->SetMaxY(wkspoints[0].y);
        pEnvelope->SetMaxX(wkspoints[1].x);
        pEnvelope->SetMinY(wkspoints[1].y);
        CAST_PTR(pEnvelope, pGeometry, IGeometry)
        pElement->SetSymbol(m_pTrackFillSymbol._p());
        editdesc = "new envelope element tracked";
        break;

    case ActionTrackCircleElement:
        if (wkspoints.size() != 2)
            return;
        a = (wkspoints[0].x - wkspoints[1].x);
        a = a*a;
        b = (wkspoints[0].y - wkspoints[1].y);
        b = b*b;
        if ((a + b) <= 0)
            return;
        EASYLIB_CREATEOBJ(CCircle, pCircle, ICircle)
        pCircle->SetCenter(wkspoints[0]);
        pCircle->SetRadius(::sqrt(a + b));
        CAST_PTR(pCircle, pGeometry, IGeometry)
        pElement->SetSymbol(m_pTrackFillSymbol._p());
        editdesc = "new circle element tracked";
        break;

    case ActionTrackEllipseElement:
        if (wkspoints.size() != 2)
            return;
        EASYLIB_CREATEOBJ(CEllipse, pEllipse, IEllipse)
        pEllipse->SetMinX(wkspoints[0].x);
        pEllipse->SetMaxY(wkspoints[0].y);
        pEllipse->SetMaxX(wkspoints[1].x);
        pEllipse->SetMinY(wkspoints[1].y);
        CAST_PTR(pEllipse, pGeometry, IGeometry)
        pElement->SetSymbol(m_pTrackFillSymbol._p());
        editdesc = "new ellipse element tracked";
        break;

    case ActionTrackPolylineElement:
    case ActionTrackFreehandLineElement:
        if (wkspoints.size() < 2)
            return;
        EASYLIB_CREATEOBJ(CPath, pPath, IPath)
        for (i = 0; i < wkspoints.size(); i++)
        {
            pPath->AddPoint(wkspoints[i]);
        }
        EASYLIB_CREATEOBJ(CPolyline, pPolyline, IPolyline)
        pPolyline->AddPathRef(pPath._p());
        CAST_PTR(pPolyline, pGeometry, IGeometry)
        pElement->SetSymbol(m_pTrackLineSymbol._p());
        editdesc = "new polyline element tracked";
        break;

    case ActionTrackPolygonElement:
    case ActionTrackFreehandFillElement:
        if (wkspoints.size() < 3)
            return;
        EASYLIB_CREATEOBJ(CRing, pRing, IRing)
        for (i = 0; i < wkspoints.size(); i++)
        {
            pRing->AddPoint(wkspoints[i]);
        }
        EASYLIB_CREATEOBJ(CPolygon, pPolygon, IPolygon)
        pPolygon->AddRingRef(pRing._p());
        CAST_PTR(pPolygon, pGeometry, IGeometry)
        pElement->SetSymbol(m_pTrackFillSymbol._p());
        editdesc = "new polygon element tracked";
        break;

    default:
        return;
    }

    pElement->SetGeometry(pGeometry._p());
    easymap::IElementLayerAgentPtr pELA;
    EASYLIB_CREATEOBJ(CElementLayerAgent, pELA, IElementLayerAgent)
    pELA->SetLayer(m_pTrackElementLayer._p());
    long id = pELA->AddElement(pElement._p());

    BOOL editsetundopoint = TRUE;
    this->FireNewElementTracked(id, &editsetundopoint);
    if (editsetundopoint)
    {
        m_pMap->SetUndoPoint(editdesc.c_str());
    }

    this->UpdateView();
    this->ReleaseCapture();
}

BOOL CEasyControlCtrl::SetNewElementSymbolParams(LPUNKNOWN params) 
{
    if (!params) return FALSE;

    ILayerDisplayParams* pDisplayParams = NULL;
    params->QueryInterface(IID_ILayerDisplayParams, (void**)&pDisplayParams);
    if (!pDisplayParams) return FALSE;

    _easycontrol_setsymbolparams(pDisplayParams, m_pTrackPointSymbol);
    _easycontrol_setsymbolparams(pDisplayParams, m_pTrackLineSymbol);
    _easycontrol_setsymbolparams(pDisplayParams, m_pTrackFillSymbol);
    _easycontrol_setsymbolparams(pDisplayParams, m_pTrackTextSymbol);

    pDisplayParams->Release();
	return TRUE;
}

LPUNKNOWN CEasyControlCtrl::GetNewElementSymbolParams() 
{
	CRuntimeClass* pRTC = RUNTIME_CLASS(CLayerDisplayParams);
    CLayerDisplayParams* pLayerDisplayParams = (class CLayerDisplayParams*)(pRTC->CreateObject());
    IDispatch* pDispatch = pLayerDisplayParams->GetIDispatch(FALSE);
    ILayerDisplayParams* pDisplayParams = NULL;
    pDispatch->QueryInterface(IID_ILayerDisplayParams, (void**)&pDisplayParams);
    pDispatch->Release();

    _easycontrol_getsymbolparams(m_pTrackPointSymbol, pDisplayParams);
    _easycontrol_getsymbolparams(m_pTrackLineSymbol, pDisplayParams);
    _easycontrol_getsymbolparams(m_pTrackFillSymbol, pDisplayParams);
    _easycontrol_getsymbolparams(m_pTrackTextSymbol, pDisplayParams);

    IUnknown* pU = NULL;
    pDisplayParams->QueryInterface(IID_IUnknown, (void**)&pU);
    pDisplayParams->Release();
	return pU;
}

BOOL CEasyControlCtrl::SetNewElementSymbolFromLib(LPCTSTR symbollibfile, long symbolindex) 
{
    if (!symbollibfile || (symbolindex < 0))
        return FALSE;

    easymap::CMemoryStreamPtr pMemoryStream;
    EASYLIB_CREATEOBJ(CMemoryStream, pMemoryStream, CMemoryStream)
    if (!pMemoryStream->LoadFromFile(symbollibfile))
        return FALSE;

    easymap::CStreamPtr pStream;
    CAST_PTR(pMemoryStream, pStream, CStream)

    easymap::CPersistPtr pPersist;
    easymap::CPersist::Instantiate(pStream, pPersist);
    if (!pPersist.Assigned())
        return FALSE;

    easymap::CSymbolLibPtr pSymbolLib;
    CAST_PTR(pPersist, pSymbolLib, CSymbolLib)
    if (!pSymbolLib.Assigned())
        return FALSE;

    long symbolcount = pSymbolLib->GetSymbolCount();
    if (symbolindex >= symbolcount)
        return FALSE;

    easymap::ISymbolPtr pSymbol;
    pSymbolLib->GetSymbolRef(pSymbol._ref(), symbolindex);

    if (!pSymbol.Assigned())
        return FALSE;

    switch (pSymbol->GetSymbolType())
    {
    case easymap::SYMBOLTYPE_POINT:
        m_pTrackPointSymbol = pSymbol;
        break;

    case easymap::SYMBOLTYPE_LINE:
        m_pTrackLineSymbol = pSymbol;
        break;

    case easymap::SYMBOLTYPE_FILL:
        m_pTrackFillSymbol = pSymbol;
        break;

    case easymap::SYMBOLTYPE_TEXT:
        m_pTrackTextSymbol = pSymbol;
        break;

    default:
        return FALSE;
    }

	return TRUE;
}

void CEasyControlCtrl::AddHighlight(LPUNKNOWN geometry) 
{
    if (!geometry) return;
    easymap::IGeometryPtr pGeometry, pGeoCloned;
    Easy2Geometry(geometry, pGeometry);
    if (!pGeometry.Assigned()) return;
    easymap::IObjPtr pObj;
    CLONE_PTR(pGeometry, pObj)
    CAST_PTR(pObj, pGeoCloned, IGeometry)
    m_Highlights.push_back(pGeoCloned);
}

LPUNKNOWN CEasyControlCtrl::GetHighlight(long index) 
{
	if ((index < 0) || (index >= m_Highlights.size()))
        return NULL;

    IUnknown* pResult = NULL;
    easymap::IGeometryPtr pGeometry = m_Highlights[index];
    Geometry2Easy(pGeometry, &pResult);
    return pResult;
}

BOOL CEasyControlCtrl::DeleteHighlight(long index) 
{
	if ((index < 0) || (index >= m_Highlights.size()))
        return FALSE;

    vector<easymap::IGeometryPtr>::iterator it = m_Highlights.begin();
    std::advance(it, index);
    m_Highlights.erase(it);
	return TRUE;
}

void CEasyControlCtrl::ClearHighlight() 
{
    m_Highlights.clear();
}

void CEasyControlCtrl::RefreshWindow() 
{
    m_pAV->RefreshWindow();
    ::SendMessage(m_hWnd, WM_PAINT, 0, 0);
}

void CEasyControlCtrl::SetHighlightParams(LPUNKNOWN params) 
{
    if (!params) return;

    ILayerDisplayParams* pDisplayParams = NULL;
    params->QueryInterface(IID_ILayerDisplayParams, (void**)&pDisplayParams);
    if (!pDisplayParams) return;

    _easycontrol_setsymbolparams(pDisplayParams, m_pHighlightPointSymbol);
    _easycontrol_setsymbolparams(pDisplayParams, m_pHighlightLineSymbol);
    _easycontrol_setsymbolparams(pDisplayParams, m_pHighlightFillSymbol);

    pDisplayParams->Release();
}

LPUNKNOWN CEasyControlCtrl::GetHighlightParams() 
{
	CRuntimeClass* pRTC = RUNTIME_CLASS(CLayerDisplayParams);
    CLayerDisplayParams* pLayerDisplayParams = (class CLayerDisplayParams*)(pRTC->CreateObject());
    IDispatch* pDispatch = pLayerDisplayParams->GetIDispatch(FALSE);
    ILayerDisplayParams* pDisplayParams = NULL;
    pDispatch->QueryInterface(IID_ILayerDisplayParams, (void**)&pDisplayParams);
    pDispatch->Release();

    _easycontrol_getsymbolparams(m_pHighlightPointSymbol, pDisplayParams);
    _easycontrol_getsymbolparams(m_pHighlightLineSymbol, pDisplayParams);
    _easycontrol_getsymbolparams(m_pHighlightFillSymbol, pDisplayParams);

    IUnknown* pU = NULL;
    pDisplayParams->QueryInterface(IID_IUnknown, (void**)&pU);
    pDisplayParams->Release();
	return pU;
}

void CEasyControlCtrl::SetSlimDataMapping(BOOL mapping) 
{
    m_MappingLoad = mapping ? true : false;

}

BOOL CEasyControlCtrl::GetSlimDataMapping() 
{
    return m_MappingLoad ? TRUE : FALSE;
}

void CEasyControlCtrl::ClearLabels() 
{
    easymap::ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)

    pLabelLayerManager->ClearLabelLayers();
}

long CEasyControlCtrl::GetLabelLayerCount() 
{
    easymap::ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)

    return pLabelLayerManager->GetLabelLayerCount();
}

LPUNKNOWN CEasyControlCtrl::GetLabelLayer(long index) 
{
    easymap::ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)

    if ((index < 0) || (index >= pLabelLayerManager->GetLabelLayerCount()))
        return NULL;

    easymap::ILabelLayerPtr pLabelLayer;
    pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), index);

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyLayer);
    CEasyLayer* pEasyLayer = (class CEasyLayer*)(pRTC->CreateObject());
    CAST_PTR(pLabelLayer, pEasyLayer->m_pLayer, ILayer)

    IDispatch* pDispatch = pEasyLayer->GetIDispatch(FALSE);
    LPUNKNOWN pOutLayer = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pOutLayer);
    pDispatch->Release();
    return pOutLayer;
}

BOOL CEasyControlCtrl::RemoveLabelLayer(long index) 
{
    easymap::ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)

    if ((index < 0) || (index >= pLabelLayerManager->GetLabelLayerCount()))
        return FALSE;

    return pLabelLayerManager->RemoveLabelLayer(index);
}

BOOL CEasyControlCtrl::LoadLabelLayer(LPCTSTR esd_filename, long labelfield) 
{
    easymap::ILayerPtr pLayer;
    if (!easymap::LoadSlimData(esd_filename, pLayer._ref(), true, true))
        return FALSE;

    easymap::ILabelLayerPtr pLL;
    EASYLIB_CREATEOBJ(CLabelLayer, pLL, ILabelLayer)
    pLL->SetVectorLayer(pLayer._p());
    pLL->SetFieldIndex(labelfield);

    easymap::ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)
    pLabelLayerManager->AddLabelLayer(pLL._p());
	return TRUE;
}

BOOL CEasyControlCtrl::SetLabelField(long labellayer, long labelfield) 
{
    easymap::ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)

    if ((labellayer < 0) || (labellayer >= pLabelLayerManager->GetLabelLayerCount()))
        return FALSE;

    easymap::ILabelLayerPtr pLabelLayer;
    pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), labellayer);

    return pLabelLayer->SetFieldIndex(labelfield);
}

long CEasyControlCtrl::GetLabelField(long labellayer) 
{
    easymap::ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)

    if ((labellayer < 0) || (labellayer >= pLabelLayerManager->GetLabelLayerCount()))
        return -1;

    easymap::ILabelLayerPtr pLabelLayer;
    pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), labellayer);

    return pLabelLayer->GetFieldIndex();
}

void CEasyControlCtrl::EnableLabels(BOOL Enable) 
{
    easymap::ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)

    pLabelLayerManager->EnableLabelDraw(Enable != FALSE);
}

BOOL CEasyControlCtrl::LabelsEnabled() 
{
    easymap::ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)

	return pLabelLayerManager->LabelDrawEnabled();
}

BOOL CEasyControlCtrl::LoadRapidDrawLayer(LPCTSTR esd_filename, BOOL readonly) 
{
    easymap::ILayerPtr pLayer;
    if (!easymap::LoadSlimData(esd_filename, pLayer._ref(), (readonly != FALSE), true))
        return FALSE;

    easymap::IRapidDrawPtr pRapidDraw;
    CAST_PTR(m_pMap, pRapidDraw, IRapidDraw)
    pRapidDraw->RD_AddLayer(pLayer._p());
	return TRUE;
}

LPUNKNOWN CEasyControlCtrl::GetRapidDrawLayer(long index) 
{
    easymap::IRapidDrawPtr pRapidDraw;
    CAST_PTR(m_pMap, pRapidDraw, IRapidDraw)

    easymap::ILayerPtr pLayer;
    if (!pRapidDraw->RD_GetLayer(pLayer._ref(), index))
    {
        return NULL;
    }

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyLayer);
    CEasyLayer* pEasyLayer = (class CEasyLayer*)(pRTC->CreateObject());
    CAST_PTR(pLayer, pEasyLayer->m_pLayer, ILayer)

    IDispatch* pDispatch = pEasyLayer->GetIDispatch(FALSE);
    LPUNKNOWN pOutLayer = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pOutLayer);
    pDispatch->Release();
    return pOutLayer;
}

BOOL CEasyControlCtrl::RemoveRapidDraw(long index) 
{
    easymap::IRapidDrawPtr pRapidDraw;
    CAST_PTR(m_pMap, pRapidDraw, IRapidDraw)

    easymap::ILayerPtr pLayer;
    if (!pRapidDraw->RD_RemoveLayer(index))
    {
        return FALSE;
    }

	return TRUE;
}

long CEasyControlCtrl::GetRapidDrawLayerCount() 
{
    easymap::IRapidDrawPtr pRapidDraw;
    CAST_PTR(m_pMap, pRapidDraw, IRapidDraw)

    return pRapidDraw->RD_GetLayerCount();
}

void CEasyControlCtrl::ClearRapidDrawLayers() 
{
    easymap::IRapidDrawPtr pRapidDraw;
    CAST_PTR(m_pMap, pRapidDraw, IRapidDraw)

    pRapidDraw->RD_ClearLayers();
}

BOOL CEasyControlCtrl::AddBookmark(LPCTSTR text) 
{
    easymap::IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)

    CString cstext;
    if (text)
    {
        cstext = text;
    }

    pPlaceBookmark->AddBookmark(cstext);

	return TRUE;
}

BOOL CEasyControlCtrl::NextBookmark() 
{
    easymap::IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)
    long r = pPlaceBookmark->NextBookmark();
    if (r >= 0)
    {
        pPlaceBookmark->SetViewToCurrentBookmark();
        return TRUE;
    }

	return FALSE;
}

BOOL CEasyControlCtrl::PreviousBookmark() 
{
    easymap::IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)
    long r = pPlaceBookmark->PreviousBookmark();
    if (r >= 0)
    {
        pPlaceBookmark->SetViewToCurrentBookmark();
        return TRUE;
    }

	return FALSE;
}

BOOL CEasyControlCtrl::DeleteBookmark() 
{
    easymap::IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)
    long id = pPlaceBookmark->GetCurrentBookmarkID();
    if (id < 0)
        return FALSE;

    return pPlaceBookmark->DeleteBookmark(id);
}

long CEasyControlCtrl::BookmarkCount() 
{
    easymap::IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)
	return pPlaceBookmark->GetBookmarkCount();
}

void CEasyControlCtrl::ClearBookmarks() 
{
    easymap::IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)
    pPlaceBookmark->ClearBookmarks();
}

void CEasyControlCtrl::DisableActiveBookmarkShow() 
{
    easymap::IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)
    pPlaceBookmark->DisableActiveBookmarkShow();
}


LPUNKNOWN CEasyControlCtrl::RapidDrawGPS() 
{
    easymap::ShapeType shapetype = easymap::SHAPETYPE_POINT;
    easymap::MapUnits mapunit = m_pDT->GetMapUnit();
    double basescale = 1000;
    double precision = 0.0001;
    long indexlevel = 3;

    easymap::WKSRect extent;
    m_pDT->GetVisibleExtent(extent);
    easymap::IFieldsPtr pFields;

    EASYLIB_CREATEOBJ(CFields, pFields, IFields)
    pFields->AddField2("gps_id", easymap::FIELDTYPE_LONG, easymap::FIELDINDEX_NOINDEX, true);
    pFields->AddField2("gps_type", easymap::FIELDTYPE_STRING, easymap::FIELDINDEX_NOINDEX, true);
    pFields->AddField2("gps_name", easymap::FIELDTYPE_STRING, easymap::FIELDINDEX_NOINDEX, true);
    pFields->AddField2("gps_description", easymap::FIELDTYPE_STRING, easymap::FIELDINDEX_NOINDEX, true);

    easymap::IVectorLayerAgentPtr pVLA;
    EASYLIB_CREATEOBJ(CVectorLayerAgent, pVLA, IVectorLayerAgent)
    pVLA->CreateSlimLayer(shapetype, mapunit, basescale, precision, extent,
        indexlevel, pFields._p(), false, NULL, true);
    easymap::ILayerPtr pLayer;
    pVLA->GetLayer(pLayer._ref());
    pLayer->SetName("GPS_Targets");

    //生成一个XX符号
    easymap::ISymbolPtr pGpsSymbol;
    _easycontrol_create_default_symbol(easymap::SYMBOLTYPE_POINT, pGpsSymbol);
    easymap::IMultiPointSymbolPtr pMPS;
    CAST_PTR(pGpsSymbol, pMPS, IMultiPointSymbol)
    easymap::IPointSymbolPtr pPointSymbol;
    pMPS->GetSymbolRef(pPointSymbol._ref(), 0);
    easymap::ISimplePointSymbolPtr pSPS;
    CAST_PTR(pPointSymbol, pSPS, ISimplePointSymbol)
    pSPS->SetDiameter(6);
    pSPS->SetColor(11111680);
    pSPS.Clear();
    EASYLIB_CREATEOBJ(CSimplePointSymbol, pSPS, ISimplePointSymbol)
    pSPS->SetDiameter(4);
    pSPS->SetColor(8454143);
    pMPS->AddSymbol(pSPS._p());

    pVLA->SetDefaultSymbol(pGpsSymbol._p());

    easymap::IRapidDrawPtr pRapidDraw;
    CAST_PTR(m_pMap, pRapidDraw, IRapidDraw)
    pRapidDraw->RD_AddLayer(pLayer._p());

	CRuntimeClass* pRTC = RUNTIME_CLASS(CEasyLayer);
    CEasyLayer* pEasyLayer = (class CEasyLayer*)(pRTC->CreateObject());
    pEasyLayer->m_pLayer = pLayer;

    IDispatch* pDispatch = pEasyLayer->GetIDispatch(FALSE);
    LPUNKNOWN pOutLayer = NULL;
    pDispatch->QueryInterface(IID_IUnknown, (void**)&pOutLayer);
    pDispatch->Release();
    return pOutLayer;
}

LPUNKNOWN CEasyControlCtrl::FindLayer(LPCTSTR layername) 
{
    easymap::ILayerPtr pLayer;
    m_pMap->FindLayer(pLayer._ref(), layername, NULL);

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

void CEasyControlCtrl::SetPlaneRotate(double degree) 
{
    m_pDT->SetPlaneRotate(degree);
}

double CEasyControlCtrl::GetPlaneRotate() 
{
    double degree;
    m_pDT->GetPlaneRotate(degree);
    return degree;
}

void CEasyControlCtrl::SetAttitude(double degree) 
{
	m_pDT->SetAttitude(degree);
}

double CEasyControlCtrl::GetAttitude() 
{
    double degree;
    m_pDT->GetAttitude(degree);
    return degree;
}

void CEasyControlCtrl::SetRotateCenter(double x, double y) 
{
    easymap::WKSPoint rotatecenter(x, y);
    m_pDT->SetRotateCenter(rotatecenter);

}

void CEasyControlCtrl::GetRotateCenter(double FAR* x, double FAR* y) 
{
	easymap::WKSPoint rotatecenter;
    m_pDT->GetRotateCenter(rotatecenter);
    *x = rotatecenter.x;
    *y = rotatecenter.y;
}

