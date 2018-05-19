// ChildView.cpp : implementation of the CChildView class
//

#include "stdafx.h"
#include "easyview.h"
#include "ChildView.h"
#include "CommonFuncs.h"
#include "..\\Include\\InterfaceLayerAgent.h"
#include "..\\Include\\InterfaceLabelLayer.h"
#include "..\\Include\\InterfaceBitmapLayer.h"
#include "..\\Include\\easylibdll.h"
#include "..\\RendererUI\\RendererUI.h"
#include "LoadSlimParams.h"
#include "StringUtils.h"
#include "FromE00.h"

typedef bool (__stdcall _SetMainWnd)(HWND wnd);
typedef bool (__stdcall _SelectSymbol)(ISymbol** ppSymbol);

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

const int TREEICONINDEX_ROOT            = 11;
const int TREEICONINDEX_GROUPLAYER      = 24;
const int TREEICONINDEX_GROUPLAYERN     = 25;
const int TREEICONINDEX_VECTORLAYER     = 21;
const int TREEICONINDEX_VECTORLAYERN    = 22;
const int TREEICONINDEX_ELEMENTLAYER    = 1;
const int TREEICONINDEX_ELEMENTLAYERN   = 2;
const int TREEICONINDEX_BITMAPLAYER     = 13;
const int TREEICONINDEX_BITMAPLAYERN    = 14;
const int TREEICONINDEX_UNKNOWNLAYER    = 4;
const int TREEICONINDEX_UNKNOWNLAYERN   = 5;

long GetTreeIconIndex(ILayerPtr pLayer)
{
    if (!pLayer.Assigned())
    {
        return -1;
    }

    IObjPtr pObj;
    easylibdll::CreateObj("CVectorLayerAgent", pObj);
    IVectorLayerAgentPtr pVectorLayerAgent;
    CAST_PTR(pObj, pVectorLayerAgent, IVectorLayerAgent);
    pVectorLayerAgent->SetLayer(pLayer._p());

    pObj.Clear();
    easylibdll::CreateObj("CElementLayerAgent", pObj);
    IElementLayerAgentPtr pElementLayerAgent;
    CAST_PTR(pObj, pElementLayerAgent, IElementLayerAgent);
    pElementLayerAgent->SetLayer(pLayer._p());

    IGroupLayerPtr pGroupLayer;
    CAST_PTR(pLayer, pGroupLayer, IGroupLayer)

    IEditLayerPtr pEditLayerPtr;
    CAST_PTR(pLayer, pEditLayerPtr, IEditLayer)

    IBitmapLayerPtr pBitmapLayer;
    CAST_PTR(pLayer, pBitmapLayer, IBitmapLayer)

    bool visible = pLayer->GetVisible();

    int treeiconindex = -1;
    if (pGroupLayer.Assigned())
    {
        if (visible)
        {
            treeiconindex = TREEICONINDEX_GROUPLAYER;
        }
        else
        {
            treeiconindex = TREEICONINDEX_GROUPLAYERN;
        }
    }
    else if (pEditLayerPtr.Assigned())
    {
        ILayerPtr pLayerTmp;
        pVectorLayerAgent->GetLayer(pLayerTmp._ref());
        if (pLayerTmp.Assigned())
        {
            if (visible)
            {
                treeiconindex = TREEICONINDEX_VECTORLAYER;
            }
            else
            {
                treeiconindex = TREEICONINDEX_VECTORLAYERN;
            }
        }
        else
        {
            pElementLayerAgent->GetLayer(pLayerTmp._ref());
            if (pLayerTmp.Assigned())
            {
                if (visible)
                {
                    treeiconindex = TREEICONINDEX_ELEMENTLAYER;
                }
                else
                {
                    treeiconindex = TREEICONINDEX_ELEMENTLAYERN;
                }
            }
            else
            {
                if (visible)
                {
                    treeiconindex = TREEICONINDEX_UNKNOWNLAYER;
                }
                else
                {
                    treeiconindex = TREEICONINDEX_UNKNOWNLAYERN;
                }
            }
        }
    }
    else if (pBitmapLayer.Assigned())
    {
        if (visible)
        {
            treeiconindex = TREEICONINDEX_BITMAPLAYER;
        }
        else
        {
            treeiconindex = TREEICONINDEX_BITMAPLAYERN;
        }
    }
    else
    {
        if (visible)
        {
            treeiconindex = TREEICONINDEX_UNKNOWNLAYER;
        }
        else
        {
            treeiconindex = TREEICONINDEX_UNKNOWNLAYERN;
        }
    }

    return treeiconindex;
}

void _stdcall VisibleExtentChangeHandle(const WKSRect& viewextent)
{
    //回调函数
    //每次显示范围发生改变后都会调用这里
};

/////////////////////////////////////////////////////////////////////////////
// CChildView

CChildView::CChildView()
{
    CWnd::CWnd();

    IObjPtr pObj;
    easylibdll::CreateObj("CMoveTracker", pObj);
    CAST_PTR(pObj, m_pMoveTracker, IMoveTracker);

    m_pLayerTree = NULL;
    m_InitOK = false;

    m_LoadData_Unit = UNIT_M;
    m_LoadData_Scale = 2000;
    m_LoadData_Precision = 0.01;
    m_LoadData_Indexlevel = 5;
    m_LoadData_Readonly = false;
}

CChildView::~CChildView()
{
    if (m_InitOK)
    {
        ::ReleaseDC(m_hWnd, m_WindowDC);
    }

    CWnd::~CWnd();
}


BEGIN_MESSAGE_MAP(CChildView,CWnd )
	//{{AFX_MSG_MAP(CChildView)
    ON_WM_SHOWWINDOW()
	ON_WM_PAINT()
    ON_WM_SIZE()
    ON_WM_MOUSEMOVE()
    ON_WM_LBUTTONDOWN()
    ON_WM_LBUTTONUP()
    ON_WM_MOUSEWHEEL()
	ON_COMMAND(ID_MENUITEM_WORKSPACENEW, OnMenuitemWorkspacenew)
	ON_COMMAND(ID_MENUITEM_WORKSPACEOPEN, OnMenuitemWorkspaceopen)
	ON_COMMAND(ID_MENUITEM_WORKSPACESAVE, OnMenuitemWorkspacesave)
	ON_COMMAND(ID_MENUITEM_WORKSPACESAVEAS, OnMenuitemWorkspacesaveas)
	ON_COMMAND(ID_MENUITEM_ZOOMIN, OnMenuitemZoomin)
	ON_COMMAND(ID_MENUITEM_ZOOMOUT, OnMenuitemZoomout)
	ON_COMMAND(ID_MENUITEM_ZOOMPAN, OnMenuitemZoompan)
	ON_COMMAND(ID_MENUITEM_ZOOMALL, OnMenuitemZoomall)
	ON_COMMAND(ID_MENUITEM_REFRESHVIEW, OnMenuitemRefreshview)
	ON_COMMAND(ID_MENUITEM_LOADDATA, OnMenuitemLoaddata)
	ON_COMMAND(ID_MENUITEM_IDENTIFY, OnMenuitemIdentify)
	ON_COMMAND(ID_MENUITEM_LAYERTREE, OnMenuitemLayertree)
	ON_COMMAND(ID_MENUITEM_UNDO, OnMenuitemUndo)
	ON_COMMAND(ID_MENUITEM_REDO, OnMenuitemRedo)
	ON_COMMAND(ID_MENUITEM_SAVEEDIT, OnMenuitemSaveedit)
	ON_COMMAND(ID_MENUITEM_CANCELEDIT, OnMenuitemCanceledit)
	ON_COMMAND(ID_MENUITEM_SELECT, OnMenuitemSelect)
	ON_COMMAND(ID_MENUITEM_DELETE, OnMenuitemDelete)
	ON_WM_RBUTTONDOWN()
	ON_WM_RBUTTONUP()
	ON_COMMAND(ID_MENUITEM_DESELECT, OnMenuitemDeselect)
	ON_COMMAND(ID_MENUITEM_ADDBOOKMARK, OnMenuitemAddbookmark)
	ON_COMMAND(ID_MENUITEM_NEXTBOOKMARK, OnMenuitemNextbookmark)
	ON_COMMAND(ID_MENUITEM_PREVIOUSBOOKMARK, OnMenuitemPreviousbookmark)
	ON_COMMAND(ID_MENUITEM_DELETECURRENTBOOKMARK, OnMenuitemDeletecurrentbookmark)
	ON_COMMAND(ID_MENUITEM_CLEARBOOKMARKS, OnMenuitemClearbookmarks)
    ON_MESSAGE(WM_MYTREE_KEYDOWN, OnLayerTreeKeyDown)
    ON_MESSAGE(WM_MYTREE_LBUTTONDOWN, OnLayerTreeLButtonDown)
    ON_MESSAGE(WM_MYTREE_LBUTTONUP, OnLayerTreeLButtonUp)
    ON_MESSAGE(WM_MYTREE_RBUTTONDOWN, OnLayerTreeRButtonDown)
    ON_MESSAGE(WM_MYTREE_RBUTTONUP, OnLayerTreeRButtonUp)
    ON_MESSAGE(WM_MYTREE_LBUTTONDBLCLK, OnLayerTreeLDoubleClick)
    ON_MESSAGE(WM_MYTREE_ITEMCHECK, OnLayerTreeItemCheck)
	ON_COMMAND(ID_MENUITEM32848, OnMenuitem32848)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

LRESULT CChildView::OnLayerTreeKeyDown(WPARAM wParam, LPARAM lParam)
{
    if (wParam == 46)
    {
        HTREEITEM pItem = m_pLayerTree->GetSelectedItem();
        if (pItem && (pItem != m_pTreeRoot))
        {
            ILayerPtr pLayer = (ILayer*)m_pLayerTree->GetItemData(pItem);
            m_pMap->DeleteLayerEx(pLayer._p());
            m_pLayerTree->DeleteItem(pItem);
            this->UpdateView();
        }
    }

    return 0;
}

LRESULT CChildView::OnLayerTreeLButtonDown(WPARAM wParam, LPARAM lParam)
{
    return 0;
}

LRESULT CChildView::OnLayerTreeLButtonUp(WPARAM wParam, LPARAM lParam)
{
    return 0;
}

LRESULT CChildView::OnLayerTreeRButtonDown(WPARAM wParam, LPARAM lParam)
{
    HTREEITEM pItem = m_pLayerTree->GetSelectedItem();
    if (m_pLayerTree->GetItemData(pItem) == 0)
        return 0;

    if (pItem && (pItem != m_pTreeRoot))
    {
        ILayerPtr pLayer = (ILayer*)m_pLayerTree->GetItemData(pItem);
        IObjPtr pObj;
        easylibdll::CreateObj("CVectorLayerAgent", pObj);
        IVectorLayerAgentPtr pAgent;
        CAST_PTR(pObj, pAgent, IVectorLayerAgent)
        if (!pAgent->SetLayer(pLayer._p()))
            return 0;

        if (rendereruidll::getDLLHandle())
        {
            rendereruidll::SetMainWnd(m_hWnd);
            rendereruidll::SelectRenderer(pAgent._p());
            this->UpdateView();
        }
        else
        {
            ::MessageBox(m_hWnd, "RendererUI.dll is not found.", "Oops！", MB_OK);
        }
    }

    return 0;
}

LRESULT CChildView::OnLayerTreeRButtonUp(WPARAM wParam, LPARAM lParam)
{
    return 0;
}

LRESULT CChildView::OnLayerTreeLDoubleClick(WPARAM wParam, LPARAM lParam)
{
    HTREEITEM pItem = m_pLayerTree->GetSelectedItem();
    if (pItem && (pItem != m_pTreeRoot))
    {
        ILayerPtr pLayer = (ILayer*)m_pLayerTree->GetItemData(pItem);
        bool visible = !pLayer->GetVisible();
        pLayer->SetVisible(visible);
        int treeiconindex = GetTreeIconIndex(pLayer);
        m_pLayerTree->SetItemImage(pItem, treeiconindex, treeiconindex);
        this->UpdateView();
    }

    return 0;
}

LRESULT CChildView::OnLayerTreeItemCheck(WPARAM wParam, LPARAM lParam)
{
    return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CChildView message handlers

BOOL CChildView::PreCreateWindow(CREATESTRUCT& cs) 
{
	if (!CWnd::PreCreateWindow(cs))
		return FALSE;

	cs.dwExStyle |= WS_EX_CLIENTEDGE;
	cs.style &= ~WS_BORDER;
	cs.lpszClass = AfxRegisterWndClass(CS_HREDRAW|CS_VREDRAW|CS_DBLCLKS, 
		::LoadCursor(NULL, IDC_ARROW), HBRUSH(COLOR_WINDOW+1), NULL);

	return TRUE;
}

void CChildView::OnPaint() 
{
    if (!m_InitOK) return;
	CPaintDC dc(this); // device context for painting

    this->GetAV()->RefreshWindow();
    m_pMoveTracker->Refresh();
}

void CChildView::OnShowWindow(BOOL bShow, UINT nStatus) 
{
    CWnd::OnShowWindow(bShow, nStatus);

    m_LayerTree.Create(IDD_DIALOG_LAYERTREE, this);
    m_pLayerTree = &m_LayerTree.m_LayerTree;
	m_ImageList1.Create(IDB_BITMAP1, 18, 1, RGB(0, 255, 0));
    m_pLayerTree->ModifyStyle(TVS_LINESATROOT | TVS_CHECKBOXES, TVS_SHOWSELALWAYS | TVS_HASBUTTONS);
	m_pLayerTree->SetImageList(&m_ImageList1, TVSIL_NORMAL);
    m_pLayerTree->m_pParent = this;
    m_pLayerTree->m_DisableDBLClickExpand = TRUE;
	this->RefreshLayerTree();

    m_IdentifyResult.Create(IDD_DIALOG_IDENTIFYRESULT, this);

    m_MouseAction = ActionDefault;
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_NORMAL);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
    m_WindowDC = ::GetDC(m_hWnd);

    m_InitOK = true;
    m_LayerTree.ShowWindow(SW_SHOWNORMAL);
}

void CChildView::OnSize(UINT nType, int cx, int cy) 
{
    CWnd::OnSize(nType, cx, cy);
    if (!m_InitOK)
    {
        return;
    }

    this->LayerTreePosition();
	this->MainSpaceResize();
}

void CChildView::LayerTreePosition()
{
    RECT rect, rect1;
    this->GetWindowRect(&rect);
    m_LayerTree.GetWindowRect(&rect1);
    m_LayerTree.MoveWindow(rect.right + rect1.left - rect1.right - 10, rect.top + 10,
        rect1.right - rect1.left, rect1.bottom - rect1.top);
//    m_LayerTree.ShowWindow(SW_SHOWNORMAL);
}

void CChildView::Identify(const WKSRect& envelope)
{
    this->ClearIdentify();
    CString &textout = m_IdentifyResult.m_Textout;

    IObjPtr pObj;
    easylibdll::CreateObj("CVectorLayerAgent", pObj);
    IVectorLayerAgentPtr pVectorLayerAgent;
    CAST_PTR(pObj, pVectorLayerAgent, IVectorLayerAgent);

    dword count = m_pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer._ref(), i);
        pVectorLayerAgent->SetLayer(pLayer._p());
        pLayer.Clear();
        pVectorLayerAgent->GetLayer(pLayer._ref());
        if (pLayer.Assigned())
        {
            CString layername = pLayer->GetName();
            textout = textout + "图层名：" + "\t\t" + layername + "\r\n\r\n";

            IIntArrayPtr pIntArray;
            pVectorLayerAgent->Identify(pIntArray._ref(), envelope, true);
            dword idcount = pIntArray->GetSize();
            for (dword j = 0; j < idcount; j++)
            {
                long fid;
                pIntArray->GetAt(j, fid);
                CString sfid = IntToStr(fid).c_str();
                textout = textout + "要素 fid：" + "\t" + sfid + "\r\n";

                IVectorFeaturePtr pVectorFeature;
                pVectorLayerAgent->GetFeature(fid, pVectorFeature._ref());
                dword fieldcount = pVectorFeature->GetFieldCount();
                for (dword k = 0; k < fieldcount; k++)
                {
                    IFieldValuePtr pFieldValue;
                    pVectorFeature->GetFieldValue(k, pFieldValue._ref());
                    IAnsiStringPtr pAnsiString;
                    pFieldValue->ToString(pAnsiString._ref());
                    CString csfv = pAnsiString->GetText();

                    textout = textout + "\t\t" + csfv + "\r\n";
                }

                textout = textout + "\r\n";
            }
            textout = textout + "---------------------------\r\n\r\n\r\n";
        }
    }

    m_IdentifyResult.UpdateData(FALSE);
    m_IdentifyResult.ShowWindow(SW_SHOWNORMAL);
}

void CChildView::ClearIdentify()
{
    m_IdentifyResult.ClearAll();
}

void CChildView::LoadBitmapLayer(const CString& bmpfile, ILayerPtr& pLayer)
{
    pLayer.Clear();

    IBitmapLayerPtr pBitmapLayer;
    IObjPtr pObj;
    easylibdll::CreateObj("CBitmapLayer", pObj);
    CAST_PTR(pObj, pBitmapLayer, IBitmapLayer)

    if (!pBitmapLayer->LoadBmpFile(bmpfile, true))
        return;

    m_pMap->AddLayer(pBitmapLayer._p());
    CAST_PTR(pBitmapLayer, pLayer, ILayer)
}

void CChildView::OnMouseMove(UINT nFlags, CPoint point) 
{
    if (!m_InitOK) return;

    IDisplayPtr pDisplay = this->GetDisplay();
    IScreenBrowserPtr pScreenBrowser;
    CAST_PTR(pDisplay, pScreenBrowser, IScreenBrowser);

    switch(m_MouseAction)
    {
    case ActionPan:
        //pan拖动中...
        pScreenBrowser->PanMoveTo(point);
        break;

    case ActionZoomIn:
    case ActionZoomOut:
    case ActionIdentify:
        if (0 < m_TrackPoints.size())
        {
            COLORREF bgcolor = this->GetDisplay()->GetBackgroundColor();
            ::SendMessage(this->m_hWnd, WM_PAINT, 0, 0);
            ::TrackEnvelope(m_WindowDC, InvertRGB(bgcolor), R2_COPYPEN,
                1, m_TrackPoints[0], point);
        }
        break;

    case ActionSelect:
        if (0 < m_TrackPoints.size() && nFlags == MK_LBUTTON)
        {
            COLORREF bgcolor = this->GetDisplay()->GetBackgroundColor();
            ::SendMessage(this->m_hWnd, WM_PAINT, 0, 0);
            ::TrackEnvelope(m_WindowDC, InvertRGB(bgcolor), R2_COPYPEN,
                1, m_TrackPoints[0], point);
        }
        else if (0 < m_TrackPoints.size() && nFlags == MK_RBUTTON)
        {
            m_pMoveTracker->MouseMove(point.x, point.y);
        }
        break;

    default:
        {
        }
    }

    CWnd::OnMouseMove(nFlags, point);
}

void CChildView::OnLButtonDown(UINT nFlags, CPoint point) 
{
    if (!m_InitOK) return;

    IDisplayPtr pDisplay = this->GetDisplay();
    IScreenBrowserPtr pScreenBrowser;
    CAST_PTR(pDisplay, pScreenBrowser, IScreenBrowser);

    switch(m_MouseAction)
    {
    case ActionPan:
        {
            //pan拖动起点
            pScreenBrowser->PanStart(point);
            HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_PANNING);
            ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
            ::SetCursor(cursor);
            SetCapture();
        }
        break;

    case ActionZoomIn:
    case ActionZoomOut:
    case ActionIdentify:
    case ActionSelect:
        if (0 == m_TrackPoints.size())
        {
            m_TrackPoints.push_back(point);
        }
        break;

    default:
        {
        }
    }

    CWnd::OnLButtonDown(nFlags, point);
}

void CChildView::OnLButtonUp(UINT nFlags, CPoint point) 
{
    if (!m_InitOK) return;

    IDisplayPtr pDisplay = this->GetDisplay();
    IScreenBrowserPtr pScreenBrowser;
    CAST_PTR(pDisplay, pScreenBrowser, IScreenBrowser);
    IDisplayTransformationPtr pTrans = this->GetDT();

    tagRECT rect, rect1;
    WKSRect selectext;
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

        pTrans->Device2MapXY(rect1.left, rect1.bottom, selectext.left, selectext.bottom);
        pTrans->Device2MapXY(rect1.right, rect1.top, selectext.right, selectext.top);
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
            ReleaseCapture();
            this->UpdateView();
        }
        break;

    case ActionZoomIn:
        if (m_TrackPoints.size() > 0)
        {
            pScreenBrowser->VisibleExtentIn(rect);
        }
        this->UpdateView();
        m_TrackPoints.clear();
        break;

    case ActionZoomOut:
        if (m_TrackPoints.size() > 0)
        {
            pScreenBrowser->VisibleExtentOut(rect);
        }
        this->UpdateView();
        m_TrackPoints.clear();
        break;

    case ActionIdentify:
        if (m_TrackPoints.size() > 0)
        {
            CRect rect;
            rect.left = m_TrackPoints[0].x;
            rect.top = m_TrackPoints[0].y;
            rect.right = point.x;
            rect.bottom = point.y;
            WKSRect envelope;
            this->GetDT()->Device2MapXY(rect.left, rect.bottom,
                envelope.left, envelope.bottom);
            this->GetDT()->Device2MapXY(rect.right, rect.top,
                envelope.right, envelope.top);
            this->Identify(envelope);
        }
        m_TrackPoints.clear();
        break;

    case ActionSelect:
        if (m_TrackPoints.size() > 0)
        {
            m_pMap->Select(selectext, true, false);
        }
        this->GetAV()->UpdateSelection();
        this->GetAV()->RefreshWindow();
        m_TrackPoints.clear();
        break;

    default:
        {
            m_TrackPoints.clear();
        }
    }

    CWnd::OnLButtonUp(nFlags, point);
}

void CChildView::OnRButtonDown(UINT nFlags, CPoint point) 
{
    switch(m_MouseAction)
    {
    case ActionSelect:
        //移动对象起点
        m_TrackPoints.clear();
        m_TrackPoints.push_back(point);

        this->AddGeometriesToTracker(NULL);
        m_pMoveTracker->Start(point.x, point.y);
        break;

    default:
        {}
    }

    CWnd ::OnRButtonDown(nFlags, point);
}

void CChildView::OnRButtonUp(UINT nFlags, CPoint point) 
{
    switch(m_MouseAction)
    {
    case ActionSelect:
        m_pMoveTracker->Finish();
        if ((m_TrackPoints.size() == 1)
            && ((m_TrackPoints[0].x != point.x) || (m_TrackPoints[0].y != point.y)))
        {
            this->UpdateMoveGeometries(NULL);
            m_pMap->SetUndoPoint("对象移动");
            this->UpdateView();
            m_pMoveTracker->ClearGeometry();
        }
        else
        {
            this->GetAV()->RefreshWindow();
        }

        m_TrackPoints.clear();
        break;

    default:
        {}
    }

    CWnd ::OnRButtonUp(nFlags, point);
}

BOOL CChildView::OnMouseWheel(UINT nFlags, short zDelta, CPoint pt) 
{
    if (!m_InitOK) return FALSE;

    double scale;
    IDisplayTransformationPtr pDT = this->GetDT();
    pDT->GetMapScale(scale);
    if (0 < zDelta)
    {
        scale += scale/4;
    }
    else
    {
        scale -= scale/4;
    }

    pDT->SetMapScale(scale);
    this->UpdateView();

    return CWnd::OnMouseWheel(nFlags, zDelta, pt);
}

BOOL CChildView::Create(LPCTSTR lpszClassName, LPCTSTR lpszWindowName, DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext) 
{
    m_InitOK = false;

    EASYLIB_CREATEOBJ(CGeoMap, m_pMap, IMap)

    this->GetDT()->SetVisibleExtentHandle(VisibleExtentChangeHandle);
	
	if (!CWnd::Create(lpszClassName, lpszWindowName, dwStyle, rect, pParentWnd, nID, pContext))
        return FALSE;

    return TRUE;
}

IActiveViewPtr CChildView::GetAV() const
{
    IActiveViewPtr pAV;
    CAST_PTR(m_pMap, pAV, CActiveView)
    return pAV;
}

IDisplayPtr CChildView::GetDisplay() const
{
    IDisplay* pDisplay = NULL;
    this->GetAV()->GetDisplay(&pDisplay);
    pDisplay->_Release();
    return pDisplay;
}

IDisplayTransformationPtr CChildView::GetDT() const
{
    IDisplayTransformation* pTrans = NULL;
    this->GetDisplay()->GetDisplayTransformation(&pTrans);
    pTrans->_Release();
    return pTrans;
}

void CChildView::MainSpaceResize()
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

    this->GetAV()->LostFocus();
    this->GetAV()->GainFocus(m_WindowDC, rect);
    m_pMoveTracker->SetDisplay(this->GetDisplay()._p());

    this->UpdateView();
}

void CChildView::UpdateView()
{
    //重新读取数据，刷新当前显示内容
    IActiveViewPtr pAV = this->GetAV();
    pAV->UpdateData();
    pAV->UpdateSelection();
    pAV->RefreshWindow();
}

void CChildView::ViewFullMap()
{
    WKSRect extent;
    m_pMap->GetFullExtent(extent);
    this->GetDT()->SetVisibleExtent(extent);
    this->UpdateView();
}

void CChildView::RefreshLayerTree()
{
    m_pLayerTree->DeleteAllItems();
	m_pTreeRoot = m_pLayerTree->InsertItem(_T("layers"), TREEICONINDEX_ROOT,
        TREEICONINDEX_ROOT, TVI_ROOT, TVI_LAST);
    m_pLayerTree->SetItemData(m_pTreeRoot, NULL);
    dword layercount = m_pMap->GetLayerCount();
    for (dword i = 0; i < layercount; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer._ref(), i);
        this->AddLayerTreeItem(m_pTreeRoot, pLayer);
    }

    m_pLayerTree->Expand(m_pTreeRoot, TVE_EXPAND);
}

void CChildView::AddLayerTreeItem(HTREEITEM pParentItem, ILayerPtr pLayer)
{
    IGroupLayerPtr pGroupLayer;
    CAST_PTR(pLayer, pGroupLayer, IGroupLayer)

    bool visible = pLayer->GetVisible();
    string layername = pLayer->GetName();

    HTREEITEM pTreeItem = NULL;
    int treeiconindex = GetTreeIconIndex(pLayer);
    if (pGroupLayer.Assigned())
    {
    	pTreeItem = m_pLayerTree->InsertItem(layername.c_str(), treeiconindex,
            treeiconindex, pParentItem, TVI_LAST);

        dword layercount = pGroupLayer->GetLayerCount();
        for (dword i = 0; i < layercount; i++)
        {
            ILayerPtr pL;
            pGroupLayer->GetLayer(pL._ref(), i);
            this->AddLayerTreeItem(pTreeItem, pL);
        }

        m_pLayerTree->Expand(pTreeItem, TVE_EXPAND);
    }
    else
    {
        pTreeItem = m_pLayerTree->InsertItem(layername.c_str(), treeiconindex,
            treeiconindex, pParentItem, TVI_LAST);
    }

    m_pLayerTree->SetItemData(pTreeItem, (DWORD)pLayer._p());
}

BOOL CChildView::DestroyWindow() 
{
    this->GetAV()->LostFocus();
    ::ReleaseDC(m_hWnd, m_WindowDC);
	
	return CWnd::DestroyWindow();
}

void CChildView::OnMenuitemWorkspacenew() 
{
    m_pMap->ClearAllData();

    this->RefreshLayerTree();
    m_EWS = "";
    m_pMap->SetName("My Map");
    this->UpdateView();
}

void CChildView::OnMenuitemWorkspaceopen() 
{
    static CFileDialog dialog(TRUE, NULL, NULL, OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
        "EasyMap WorkSpace (*.ews)|*.ews|", ::AfxGetMainWnd());
    if (IDOK != dialog.DoModal()) return;
    POSITION pos = dialog.GetStartPosition();
    if (pos)
    {

        //原先的干掉
        m_pMap->ClearAllData();
        m_pMap.Clear();

        m_EWS = dialog.GetNextPathName(pos);
        std::string tws = m_EWS;

        //从文件中读出stream，然后对象化成geomap
        IObjPtr pObj;
        easylibdll::CreateObj("CMemoryStream", pObj);
        IStreamXPtr pStream;
        CAST_PTR(pObj, pStream, IStreamX);
        pStream->LoadFromFile(tws.c_str());

        IPersistPtr pPersist;
        easylibdll::Instantiate(pStream, pPersist);
        CAST_PTR(pPersist, m_pMap, IMap)

        //由于map已经换了，所以要重新注册回调函数
        this->GetDT()->SetVisibleExtentHandle(VisibleExtentChangeHandle);
        this->RefreshLayerTree();
        this->MainSpaceResize();
    }


/*
    //=======================================================
    //测试自动标注，统一避让功能
    ILabelLayerManagerPtr pLLM;
    CAST_PTR(m_pMap, pLLM, ILabelLayerManager)
    pLLM->ClearLabelLayers();
    ILayerPtr pLayer1;
    m_pMap->FindLayer(pLayer1._ref(), "信息点", "CGroupLayer");
    if (pLayer1.Assigned())
    {
        IGroupLayerPtr pGL;
        CAST_PTR(pLayer1, pGL, IGroupLayer)
        long cc = pGL->GetLayerCount();
        for (long i = 0; i < cc; i++)
        {
            ILayerPtr pSub;
            pGL->GetLayer(pSub._ref(), i);
            IObjPtr pObj;
            easylibdll::CreateObj("CLabelLayer", pObj);
            ILabelLayerPtr pLL;
            CAST_PTR(pObj, pLL, ILabelLayer)
            pLL->SetVectorLayer(pSub._p());
            pLL->SetFieldIndex(0);
            pLLM->AddLabelLayer(pLL._p());
        }

        this->MainSpaceResize();
    }
    //=======================================================
*/

/*
    ILabelLayerManagerPtr pLLM;
    CAST_PTR(m_pMap, pLLM, ILabelLayerManager)
    pLLM->ClearLabelLayers();
    ILayerPtr pLayer1;
    IObjPtr pObj;
    ILabelLayerPtr pLL;

    m_pMap->FindLayer(pLayer1._ref(), "WHJTA_A_P", NULL);
    easylibdll::CreateObj("CLabelLayer", pObj);
    CAST_PTR(pObj, pLL, ILabelLayer)
    pLL->SetVectorLayer(pLayer1._p());
    pLL->SetFieldIndex(4);
    pLLM->AddLabelLayer(pLL._p());

    m_pMap->FindLayer(pLayer1._ref(), "WHJTA_A_MP", NULL);
    easylibdll::CreateObj("CLabelLayer", pObj);
    CAST_PTR(pObj, pLL, ILabelLayer)
    pLL->SetVectorLayer(pLayer1._p());
    pLL->SetFieldIndex(4);
    pLLM->AddLabelLayer(pLL._p());

    this->MainSpaceResize();
*/
}

void CChildView::OnMenuitemWorkspacesave() 
{
    string tws = m_EWS;
    tws = Trim(tws);
    if (tws == "")
    {
        ::AfxMessageBox("please call saveas");
        return;
    }

    //将geomap序列化后存成文件
    IObjPtr pObj;
    easylibdll::CreateObj("CMemoryStream", pObj);
    IStreamXPtr pStream;
    CAST_PTR(pObj, pStream, IStreamX);
    IPersistPtr pPersist;
    CAST_PTR(m_pMap, pPersist, IPersist)
    pPersist->Dump(pStream._p());
    pStream->SaveToFile(tws.c_str());
}

void CChildView::OnMenuitemWorkspacesaveas() 
{
    CFileDialog dialog(FALSE, NULL, NULL, OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
        "EasyMap WorkSpace (*.ews)|*.ews|", ::AfxGetMainWnd());
    if (IDOK != dialog.DoModal()) return;
    POSITION pos = dialog.GetStartPosition();
    if (pos)
    {
        m_EWS = dialog.GetNextPathName(pos);
        string tws = m_EWS;

        //这里确保扩展名为“.ews”
        //---------------------------
        tws = Trim(tws);
        if (tws == "")
        {
            return;
        }

        string filename = LowerString(GetExtNamePart(tws.c_str()));
        if (filename == ".ews")
        {
            filename = tws;
        }
        else
        {
            filename = RemoveExtNamePart(tws.c_str());
            filename = filename + ".ews";
        }
        //---------------------------

        m_EWS = filename.c_str();

        this->OnMenuitemWorkspacesave();
    }
}

void CChildView::OnMenuitemZoomin() 
{
    m_MouseAction = ActionZoomIn;
    m_TrackPoints.clear();
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_ZOOMIN);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CChildView::OnMenuitemZoomout() 
{
    m_MouseAction = ActionZoomOut;
    m_TrackPoints.clear();
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_ZOOMOUT);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CChildView::OnMenuitemZoompan() 
{
    m_MouseAction = ActionPan;
    m_TrackPoints.clear();
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_PAN);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CChildView::OnMenuitemZoomall() 
{
    this->ViewFullMap();	
}

void CChildView::OnMenuitemRefreshview() 
{
    this->UpdateView();

/*
//-----------------------------------------------------------------------
//测试图层查找功能
    ILayerPtr pFind;
    m_pMap->FindLayer(pFind._ref(), "河_region", "");
    if (pFind.Assigned())
    {
        CString layername = pFind->GetName();
        AfxMessageBox(layername);
    }
//-----------------------------------------------------------------------
*/
}

void CChildView::SetLoadSlimParams()
{
    static CLoadSlimParams LoadSlimParams;

    LoadSlimParams.m_Unit = m_LoadData_Unit;
    LoadSlimParams.m_Scale = m_LoadData_Scale;
    LoadSlimParams.m_Precision = m_LoadData_Precision;
    LoadSlimParams.m_Indexlevel = m_LoadData_Indexlevel;
    LoadSlimParams.m_Readonly = m_LoadData_Readonly ? TRUE:FALSE;

    if (LoadSlimParams.DoModal())
    {
        m_LoadData_Unit = LoadSlimParams.m_Unit;
        m_LoadData_Scale = LoadSlimParams.m_Scale;
        m_LoadData_Precision = LoadSlimParams.m_Precision;
        m_LoadData_Indexlevel = LoadSlimParams.m_Indexlevel;
        m_LoadData_Readonly = LoadSlimParams.m_Readonly ? true:false;
    }
}

void CChildView::OnMenuitemLoaddata() 
{
    DWORD layercountbefore = m_pMap->GetAllCount();

    static char BASED_CODE szFilter[] =
        "EasyMap SlimData(*.esd)|*.esd|"\
        "ArcGIS ShapeFile(*.shp)|*.shp|"\
        "ArcGIS E00(*.e00)|*.e00|"\
        "带dom信息的bmp文件(*.bmp)|*.bmp|";
    static CFileDialog dialog(TRUE, NULL, NULL, OFN_OVERWRITEPROMPT|OFN_ALLOWMULTISELECT,
        szFilter, ::AfxGetMainWnd());
    if (IDOK != dialog.DoModal()) return;
    POSITION pos = dialog.GetStartPosition();
    string err;
    bool paramok = false;
    while (pos)
    {
        CString filenamecs = dialog.GetNextPathName(pos);
        string filename = filenamecs;
        ILayerPtr pLayer;
        string ext = LowerString(GetExtNamePart(filename));
        if (ext == "esd")
        {
            if (!easylibdll::LoadSlimData(filename.c_str(), pLayer, false, true))
            {
                easylibdll::LoadSlimData(filename.c_str(), pLayer, true, true);
            }
        }
        else if (ext == "shp")
        {
            if (!paramok)
            {
                this->SetLoadSlimParams();
                paramok = true;
            }

            easylibdll::LoadShapeFile(
                filename.c_str(),
                pLayer,
                m_LoadData_Unit,
                m_LoadData_Scale,
                m_LoadData_Precision,
                m_LoadData_Indexlevel,
                m_LoadData_Readonly);
        }
        else if (ext == "bmp")
        {
            this->LoadBitmapLayer(filename.c_str(), pLayer);
        }
        else if (ext == "e00")
        {
            if (!paramok)
            {
                this->SetLoadSlimParams();
                paramok = true;
            }

            this->LoadE00(filename.c_str(), pLayer);
        }

        if (pLayer.Assigned())
        {
            m_pMap->AddLayer(pLayer._p());
/*
            ///////////////////////////////////////////////////////////
            //测试rapiddraw功能
            IRapidDrawPtr pRapidDraw;
            CAST_PTR(m_pMap, pRapidDraw, IRapidDraw)
            pRapidDraw->RD_AddLayer(pLayer._p());
            ///////////////////////////////////////////////////////////
*/


/*
            ///////////////////////////////////////////////////////////
            //测试自动标注图层
            IObjPtr pObj;
            easylibdll::CreateObj("CLabelLayer", pObj);
            ILabelLayerPtr pLabelLayer;
            CAST_PTR(pObj, pLabelLayer, ILabelLayer)
            if (pLabelLayer->SetVectorLayer(pLayer._p()))
            {
                pLabelLayer->SetFieldIndex(1);
                ILabelLayerManagerPtr pLabelLayerManager;
                CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)
                pLabelLayerManager->AddLabelLayer(pLabelLayer._p());
            }
            ///////////////////////////////////////////////////////////
*/
        }
        else
        {
            err = err + "无法加载该文件：" + filename + "\n";
        }
    }

    this->RefreshLayerTree();

    if (err != "")
    {
        AfxMessageBox(err.c_str());
    }

    if (0 == layercountbefore)
    {
        this->ViewFullMap();
    }
    else
    {
        this->UpdateView();
    }
}

void AddE00PointToSlimLayer(E00LabPoint* pE00Point, IVectorLayerAgentPtr pLayerAgent)
{
    if (_invalid(pE00Point)) return;
    IGeometryPtr pGeometry;
    easylibdll::CreateGeometry(GEOMETRYTYPE_POINT, pGeometry);
    IPointPtr pPoint;
    CAST_PTR(pGeometry, pPoint, IPoint)
    pPoint->SetX(pE00Point->x);
    pPoint->SetY(pE00Point->y);

    IVectorFeaturePtr pFeature;
    pLayerAgent->CreateFeature(pFeature._ref());
    pFeature->SetGeometryRef(pGeometry._p());
    pFeature->Update();
}

void AddE00LineToSlimLayer(E00Arc* pE00Arc, IVectorLayerAgentPtr pLayerAgent)
{
    if (_invalid(pE00Arc)) return;
    if (2 > pE00Arc->PointNums) return;
    IGeometryPtr pGeometry, pSubGeometry;
    easylibdll::CreateGeometry(GEOMETRYTYPE_POLYLINE, pGeometry);
    IPolylinePtr pPolyline;
    CAST_PTR(pGeometry, pPolyline, IPolyline)
    easylibdll::CreateGeometry(GEOMETRYTYPE_PATH, pSubGeometry);
    IPathPtr pPath;
    CAST_PTR(pSubGeometry, pPath, IPath)
    //这里都当成一条path处理了
    for (long i = 0; i < pE00Arc->PointNums; i++)
    {
        WKSPointZ pnt;
        pnt.x = pE00Arc->CoordData[i*2];
        pnt.y = pE00Arc->CoordData[i*2 + 1];
        pnt.z = 0;
        pPath->AddPoint(pnt);
    }

    pPolyline->AddPathRef(pPath._p());

    IVectorFeaturePtr pFeature;
    pLayerAgent->CreateFeature(pFeature._ref());
    pFeature->SetGeometryRef(pGeometry._p());
    pFeature->Update();
}

void AddE00RingToSlimLayer(E00Arc* pE00Arc, IVectorLayerAgentPtr pLayerAgent)
{
    if (_invalid(pE00Arc)) return;
    if (3 > pE00Arc->PointNums) return;
    IGeometryPtr pGeometry, pSubGeometry;
    easylibdll::CreateGeometry(GEOMETRYTYPE_POLYGON, pGeometry);
    IPolygonPtr pPolygon;
    CAST_PTR(pGeometry, pPolygon, IPolygon)
    easylibdll::CreateGeometry(GEOMETRYTYPE_RING, pSubGeometry);
    IRingPtr pRing;
    CAST_PTR(pSubGeometry, pRing, IRing)
    //这里都当成一条ring处理了
    WKSPointZ pnt;
    pnt.x = pE00Arc->CoordData[0];
    pnt.y = pE00Arc->CoordData[1];
    pnt.z = 0;
    pRing->AddPoint(pnt);
    for (long i = 1; i < pE00Arc->PointNums; i++)
    {
        pnt.x = pE00Arc->CoordData[i*2];
        pnt.y = pE00Arc->CoordData[i*2 + 1];
        pnt.z = 0;
        pRing->AddPoint(pnt);
    }

    pPolygon->AddRingRef(pRing._p());

    IVectorFeaturePtr pFeature;
    pLayerAgent->CreateFeature(pFeature._ref());
    pFeature->SetGeometryRef(pGeometry._p());
    pFeature->Update();
}

void AddE00PolygonToSlimLayer(CFromE00* pE00, E00Polygon* pE00Polygon, IVectorLayerAgentPtr pLayerAgent)
{
    long arccount = pE00Polygon->ArcOidArray.GetSize();
    for (long i = 0; i < arccount; i++)
    {
        long arcoid = pE00Polygon->ArcOidArray.GetAt(i);
        E00Arc* pE00Arc = NULL;
        if (pE00->m_pLineMap->Lookup(arcoid, pE00Arc))
        {
            AddE00RingToSlimLayer(pE00Arc, pLayerAgent);
        }
    }
}

void AddE00TextToSlimLayer(E00Text* pE00Text, IVectorLayerAgentPtr pLayerAgent)
{
    if (_invalid(pE00Text)) return;
    IGeometryPtr pGeometry;
    easylibdll::CreateGeometry(GEOMETRYTYPE_POINT, pGeometry);
    IPointPtr pPoint;
    CAST_PTR(pGeometry, pPoint, IPoint)
    pPoint->SetX(pE00Text->CoordData[0]);
    pPoint->SetY(pE00Text->CoordData[1]);

    IVectorFeaturePtr pFeature;
    pLayerAgent->CreateFeature(pFeature._ref());
    pFeature->SetGeometryRef(pGeometry._p());
    pFeature->SetAnnotation(pE00Text->text);
    pFeature->Update();
}

void GetE00ArcMBR(E00Arc* pE00Arc, WKSRect& mbr)
{
    if (_invalid(pE00Arc) || (pE00Arc->PointNums <= 0))
        return;

    mbr.left = mbr.right = pE00Arc->CoordData[0];
    mbr.top = mbr.bottom = pE00Arc->CoordData[1];
    for (long i = 1; i < pE00Arc->PointNums; i++)
    {
        WKSPointZ pnt;
        double x = pE00Arc->CoordData[i*2];
        double y = pE00Arc->CoordData[i*2 + 1];
        if (x < mbr.left) mbr.left = x;
        else if (x > mbr.right) mbr.right = x;
        if (y < mbr.bottom) mbr.bottom = y;
        else if (y > mbr.top) mbr.top = y;
    }
}

void GetE00PolygonMBR(CFromE00* pE00, WKSRect& mbr)
{
    POSITION pos = pE00->m_pLineMap->GetStartPosition();
    E00Arc* pE00Arc = NULL;
    long oid = 0;
    pE00->m_pLineMap->GetNextAssoc(pos, oid, pE00Arc);
    GetE00ArcMBR(pE00Arc, mbr);
}

void GetE00TextMBR(E00Text* pE00Text, WKSRect& mbr)
{
    if (_invalid(pE00Text))
        return;

    mbr.left = mbr.right = pE00Text->CoordData[0];
    mbr.top = mbr.bottom = pE00Text->CoordData[1];
}

void CChildView::LoadE00(const CString& e00file, ILayerPtr& pLayer)
{
    CString layername = CRemoveExtNamePart(CRemoveDirectoryPart(e00file));
    CFromE00 e00;
    e00.AddFileName(e00file);
    e00.BeginReadData();

    IObjPtr pObj;
    easylibdll::CreateObj("CGroupLayer", pObj);
    IGroupLayerPtr pGroupLayer;
    CAST_PTR(pObj, pGroupLayer, IGroupLayer);

    pObj.Clear();
    easylibdll::CreateObj("CVectorLayerAgent", pObj);
    IVectorLayerAgentPtr pLayerAgent;
    CAST_PTR(pObj, pLayerAgent, IVectorLayerAgent);

    ILayerPtr pDataLayer;

    WKSRect extent;
    long oid;
    POSITION pos;
    if (e00.m_pPointMap)
    {
        //点
        E00LabPoint *pPointObj = NULL;
        pos = e00.m_pPointMap->GetStartPosition();
        if (pos)
        {
            e00.m_pPointMap->GetNextAssoc(pos, oid, pPointObj);
            extent.left = extent.right = pPointObj->x;
            extent.bottom = extent.top = pPointObj->y;
            pLayerAgent->SetLayer(NULL);
            pLayerAgent->CreateSlimLayer(
                SHAPETYPE_POINT,
                m_LoadData_Unit,
                m_LoadData_Scale,
                m_LoadData_Precision,
                extent,
                m_LoadData_Indexlevel,
                NULL,
                false,
                NULL,
                true);

            AddE00PointToSlimLayer(pPointObj, pLayerAgent);

            while (pos)
            {
                e00.m_pPointMap->GetNextAssoc(pos, oid, pPointObj);
                AddE00PointToSlimLayer(pPointObj, pLayerAgent);
            }

            pDataLayer.Clear();
            pLayerAgent->GetLayer(pDataLayer._ref());
            pDataLayer->SetName("点");
            pGroupLayer->AddLayer(pDataLayer._p());
            pLayerAgent->SetLayer(NULL);
        }
    }

    if (e00.m_pLineMap)
    {
        //线
        E00Arc *pLineObj = NULL;
        pos = e00.m_pLineMap->GetStartPosition();
        if (pos)
        {
            e00.m_pLineMap->GetNextAssoc(pos, oid, pLineObj);
            GetE00ArcMBR(pLineObj, extent);
            pLayerAgent->SetLayer(NULL);
            pLayerAgent->CreateSlimLayer(
                SHAPETYPE_POLYLINE,
                m_LoadData_Unit,
                m_LoadData_Scale,
                m_LoadData_Precision,
                extent,
                m_LoadData_Indexlevel,
                NULL,
                false,
                NULL,
                true);

            AddE00LineToSlimLayer(pLineObj, pLayerAgent);

            while (pos)
            {
                e00.m_pLineMap->GetNextAssoc(pos, oid, pLineObj);
                AddE00LineToSlimLayer(pLineObj, pLayerAgent);
            }

            pDataLayer.Clear();
            pLayerAgent->GetLayer(pDataLayer._ref());
            pDataLayer->SetName("线");
            pGroupLayer->AddLayer(pDataLayer._p());
            pLayerAgent->SetLayer(NULL);
        }
    }

    if (false)
//    if (e00.m_pPolygonMap)
    {
        //多边形
        E00Polygon *pPolygonObj = NULL;
        pos = e00.m_pPolygonMap->GetStartPosition();
        if (pos)
        {
            e00.m_pPolygonMap->GetNextAssoc(pos, oid, pPolygonObj);
            GetE00PolygonMBR(&e00, extent);
            pLayerAgent->SetLayer(NULL);
            pLayerAgent->CreateSlimLayer(
                SHAPETYPE_POLYGON,
                m_LoadData_Unit,
                m_LoadData_Scale,
                m_LoadData_Precision,
                extent,
                m_LoadData_Indexlevel,
                NULL,
                false,
                NULL,
                true);

            AddE00PolygonToSlimLayer(&e00, pPolygonObj, pLayerAgent);

            while (pos)
            {
                e00.m_pPolygonMap->GetNextAssoc(pos, oid, pPolygonObj);
                AddE00PolygonToSlimLayer(&e00, pPolygonObj, pLayerAgent);
            }

            pDataLayer.Clear();
            pLayerAgent->GetLayer(pDataLayer._ref());
            pDataLayer->SetName("多边形");
            pGroupLayer->AddLayer(pDataLayer._p());
            pLayerAgent->SetLayer(NULL);
        }
    }

    if (e00.m_pTextMap)
    {
        //text
        E00Text *pTextObj = NULL;
        pos = e00.m_pTextMap->GetStartPosition();
        if (pos)
        {
            e00.m_pTextMap->GetNextAssoc(pos, oid, pTextObj);
            GetE00TextMBR(pTextObj, extent);
            pLayerAgent->SetLayer(NULL);
            pLayerAgent->CreateSlimLayer(
                SHAPETYPE_POINT,
                m_LoadData_Unit,
                m_LoadData_Scale,
                m_LoadData_Precision,
                extent,
                m_LoadData_Indexlevel,
                NULL,
                true,
                NULL,
                true);

            AddE00TextToSlimLayer(pTextObj, pLayerAgent);

            while (pos)
            {
                e00.m_pTextMap->GetNextAssoc(pos, oid, pTextObj);
                AddE00TextToSlimLayer(pTextObj, pLayerAgent);
            }

            pDataLayer.Clear();
            pLayerAgent->GetLayer(pDataLayer._ref());
            pDataLayer->SetName("注记");
            pGroupLayer->AddLayer(pDataLayer._p());
            pLayerAgent->SetLayer(NULL);
        }
    }

    CAST_PTR(pGroupLayer, pLayer, ILayer)
    pLayer->SetName(layername);
}

void CChildView::AddGeometriesToTracker(ILayerPtr pLayer)
{
    dword i, count;
    IGroupLayerPtr pGroupLayer;
    ILayerPtr pSubLayer, pTmpLayer;
    if (!pLayer.Assigned())
    {
        m_pMoveTracker->ClearGeometry();
        count = m_pMap->GetLayerCount();
        for (i = 0; i < count; i++)
        {
            pSubLayer.Clear();
            m_pMap->GetLayer(pSubLayer._ref(), i);
            CAST_PTR(pSubLayer, pGroupLayer, IGroupLayer)
            if (pGroupLayer.Assigned())
            {
                CAST_PTR(pGroupLayer, pTmpLayer, ILayer)
                this->AddGeometriesToTracker(pTmpLayer);
            }
            else
            {
                this->AddLayerGeometriesToTracker(pSubLayer);
            }
        }
    }
    else
    {
        CAST_PTR(pLayer, pGroupLayer, IGroupLayer)
        if (pGroupLayer.Assigned())
        {
            count = pGroupLayer->GetLayerCount();
            for (i = 0; i < count; i++)
            {
                pSubLayer.Clear();
                pGroupLayer->GetLayer(pSubLayer._ref(), i);
                IGroupLayerPtr pSubGroupLayer;
                CAST_PTR(pSubLayer, pSubGroupLayer, IGroupLayer)
                if (pSubGroupLayer.Assigned())
                {
                    CAST_PTR(pSubGroupLayer, pTmpLayer, ILayer)
                    this->AddGeometriesToTracker(pTmpLayer);
                }
                else
                {
                    this->AddLayerGeometriesToTracker(pSubLayer);
                }
            }
        }
        else
        {
            this->AddLayerGeometriesToTracker(pLayer);
        }
    }
}

void CChildView::AddLayerGeometriesToTracker(ILayerPtr pLayer)
{
    IIntArrayPtr pIDs;
    dword i, count;
    long id;
    IGeometryPtr pGeometry;
    IObjPtr pObj;

    easylibdll::CreateObj("CVectorLayerAgent", pObj);
    IVectorLayerAgentPtr pVecorLayerAgent;
    CAST_PTR(pObj, pVecorLayerAgent, IVectorLayerAgent);
    pObj.Clear();
    bool flag = pVecorLayerAgent->SetLayer(pLayer._p());
    if (flag)
    {
        pVecorLayerAgent->GetSelection(pIDs._ref());
        count = pIDs->GetSize();
        for (i = 0 ; i < count; i++)
        {
            pIDs->GetAt(i, id);
            IVectorFeaturePtr pFeature;
            pVecorLayerAgent->GetFeature(id, pFeature._ref());
            pGeometry.Clear();
            pFeature->GetGeometryRef(pGeometry._ref());
            CLONE_PTR(pGeometry, pObj);
            CAST_PTR(pObj, pGeometry, IGeometry)
            m_pMoveTracker->AddGeometryRef(pGeometry._p());
        }

        return;
    }

    easylibdll::CreateObj("CElementLayerAgent", pObj);
    IElementLayerAgentPtr pElementLayerAgent;
    CAST_PTR(pObj, pElementLayerAgent, IElementLayerAgent);
    pObj.Clear();
    pIDs.Clear();
    flag = pElementLayerAgent->SetLayer(pLayer._p());
    if (flag)
    {
        pElementLayerAgent->GetSelectElements(pIDs._ref());
        count = pIDs->GetSize();
        for (i = 0 ; i < count; i++)
        {
            pIDs->GetAt(i, id);
            IElementPtr pElement;
            pElementLayerAgent->GetElement(id, pElement._ref());
            pElement->GetGeometry(pGeometry._ref());
            m_pMoveTracker->AddGeometryRef(pGeometry._p());
        }
    }
}

void CChildView::UpdateMoveGeometries(ILayerPtr pLayer)
{
    dword i, count;
    IGroupLayerPtr pGroupLayer;
    ILayerPtr pSubLayer, pTmpLayer;
    if (!pLayer.Assigned())
    {
        m_pMoveTracker->ResetIterator();
        count = m_pMap->GetLayerCount();
        for (i = 0; i < count; i++)
        {
            pSubLayer.Clear();
            m_pMap->GetLayer(pSubLayer._ref(), i);
            CAST_PTR(pSubLayer, pGroupLayer, IGroupLayer)
            if (pGroupLayer.Assigned())
            {
                CAST_PTR(pGroupLayer, pTmpLayer, ILayer)
                this->UpdateMoveGeometries(pTmpLayer);
            }
            else
            {
                this->UpdateMoveLayerGeometries(pSubLayer);
            }
        }

        m_pMoveTracker->ClearGeometry();
    }
    else
    {
        CAST_PTR(pLayer, pGroupLayer, IGroupLayer)
        if (pGroupLayer.Assigned())
        {
            count = pGroupLayer->GetLayerCount();
            for (i = 0; i < count; i++)
            {
                pSubLayer.Clear();
                pGroupLayer->GetLayer(pSubLayer._ref(), i);
                IGroupLayerPtr pSubGroupLayer;
                CAST_PTR(pSubLayer, pSubGroupLayer, IGroupLayer)
                if (pSubGroupLayer.Assigned())
                {
                    CAST_PTR(pSubGroupLayer, pTmpLayer, ILayer)
                    this->UpdateMoveGeometries(pTmpLayer);
                }
                else
                {
                    this->UpdateMoveLayerGeometries(pSubLayer);
                }
            }
        }
        else
        {
            this->UpdateMoveLayerGeometries(pLayer);
        }
    }
}

void CChildView::UpdateMoveLayerGeometries(ILayerPtr pLayer)
{
    IIntArrayPtr pIDs;
    dword i, count;
    long id;
    IGeometryPtr pGeometry;
    IObjPtr pObj;

    easylibdll::CreateObj("CVectorLayerAgent", pObj);
    IVectorLayerAgentPtr pVecorLayerAgent;
    CAST_PTR(pObj, pVecorLayerAgent, IVectorLayerAgent);
    pObj.Clear();
    bool flag = pVecorLayerAgent->SetLayer(pLayer._p());
    if (flag)
    {
        pVecorLayerAgent->GetSelection(pIDs._ref());
        count = pIDs->GetSize();
        for (i = 0 ; i < count; i++)
        {
            pIDs->GetAt(i, id);
            IVectorFeaturePtr pFeature;
            pVecorLayerAgent->GetFeature(id, pFeature._ref());
            pGeometry.Clear();
            m_pMoveTracker->NextGeometryRef(pGeometry._ref());
            pFeature->SetGeometryRef(pGeometry._p());
            pFeature->Update();
        }

        return;
    }

    easylibdll::CreateObj("CElementLayerAgent", pObj);
    IElementLayerAgentPtr pElementLayerAgent;
    CAST_PTR(pObj, pElementLayerAgent, IElementLayerAgent);
    pObj.Clear();
    pIDs.Clear();
    flag = pElementLayerAgent->SetLayer(pLayer._p());
    if (flag)
    {
        pElementLayerAgent->GetSelectElements(pIDs._ref());
        count = pIDs->GetSize();
        for (i = 0 ; i < count; i++)
        {
            pIDs->GetAt(i, id);
            IElementPtr pElement;
            pElementLayerAgent->GetElement(id, pElement._ref());
            pGeometry.Clear();
            m_pMoveTracker->NextGeometryRef(pGeometry._ref());
            pElement->SetGeometry(pGeometry._p());
            pElementLayerAgent->SetElement(id, pElement._p());
        }
    }
}

void CChildView::OnMenuitemIdentify() 
{
    m_MouseAction = ActionIdentify;
    m_TrackPoints.clear();
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_INDETIFY);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CChildView::OnMenuitemLayertree() 
{
    if (!m_LayerTree.IsWindowVisible())
    {
        m_LayerTree.ShowWindow(SW_SHOWNORMAL);
    }
    else
    {
        m_LayerTree.ShowWindow(SW_HIDE);
    }
}

void CChildView::OnMenuitemUndo() 
{
	m_pMap->EditUndo();
    this->UpdateView();
}

void CChildView::OnMenuitemRedo() 
{
	m_pMap->EditRedo();
    this->UpdateView();
}

void CChildView::OnMenuitemSaveedit() 
{
	m_pMap->SaveData();
}

void CChildView::OnMenuitemCanceledit() 
{
	m_pMap->EditCancel();
    this->UpdateView();
}

void CChildView::OnMenuitemSelect() 
{
    m_MouseAction = ActionSelect;
    m_TrackPoints.clear();
    HCURSOR cursor = AfxGetApp()->LoadCursor(IDC_CURSOR_CROSS);
    ::SetClassLong(m_hWnd, GCL_HCURSOR, (long)cursor);
    ::SetCursor(cursor);
}

void CChildView::DeleteSelectedGeometries(ILayerPtr pLayer)
{
    dword i, count;
    IGroupLayerPtr pGroupLayer;
    ILayerPtr pSubLayer, pTmpLayer;
    if (!pLayer.Assigned())
    {
        m_pMoveTracker->ClearGeometry();
        count = m_pMap->GetLayerCount();
        for (i = 0; i < count; i++)
        {
            pSubLayer.Clear();
            m_pMap->GetLayer(pSubLayer._ref(), i);
            CAST_PTR(pSubLayer, pGroupLayer, IGroupLayer)
            if (pGroupLayer.Assigned())
            {
                CAST_PTR(pGroupLayer, pTmpLayer, ILayer)
                this->DeleteSelectedGeometries(pTmpLayer);
            }
            else
            {
                this->DeleteLayerSelectedGeometries(pSubLayer);
            }
        }
    }
    else
    {
        CAST_PTR(pLayer, pGroupLayer, IGroupLayer)
        if (pGroupLayer.Assigned())
        {
            count = pGroupLayer->GetLayerCount();
            for (i = 0; i < count; i++)
            {
                pSubLayer.Clear();
                pGroupLayer->GetLayer(pSubLayer._ref(), i);
                IGroupLayerPtr pSubGroupLayer;
                CAST_PTR(pSubLayer, pSubGroupLayer, IGroupLayer)
                if (pSubGroupLayer.Assigned())
                {
                    CAST_PTR(pSubGroupLayer, pTmpLayer, ILayer)
                    this->DeleteSelectedGeometries(pTmpLayer);
                }
                else
                {
                    this->DeleteLayerSelectedGeometries(pSubLayer);
                }
            }
        }
        else
        {
            this->DeleteLayerSelectedGeometries(pLayer);
        }
    }
}

void CChildView::DeleteLayerSelectedGeometries(ILayerPtr pLayer)
{
    IIntArrayPtr pIDs;
    dword i, count;
    long id;
    IGeometryPtr pGeometry;
    IObjPtr pObj;

    easylibdll::CreateObj("CVectorLayerAgent", pObj);
    IVectorLayerAgentPtr pVecorLayerAgent;
    CAST_PTR(pObj, pVecorLayerAgent, IVectorLayerAgent);
    pObj.Clear();
    bool flag = pVecorLayerAgent->SetLayer(pLayer._p());
    if (flag)
    {
        pVecorLayerAgent->GetSelection(pIDs._ref());
        count = pIDs->GetSize();
        for (i = 0 ; i < count; i++)
        {
            pIDs->GetAt(i, id);
            pVecorLayerAgent->DeleteFeature(id);
        }

        return;
    }

    easylibdll::CreateObj("CElementLayerAgent", pObj);
    IElementLayerAgentPtr pElementLayerAgent;
    CAST_PTR(pObj, pElementLayerAgent, IElementLayerAgent);
    pObj.Clear();
    pIDs.Clear();
    flag = pElementLayerAgent->SetLayer(pLayer._p());
    if (flag)
    {
        pElementLayerAgent->RemoveSelectedElements();
    }
}

void CChildView::OnMenuitemDelete() 
{
    this->DeleteSelectedGeometries(NULL);
    m_pMap->SetUndoPoint("删除对象");
    this->UpdateView();
}

void CChildView::OnMenuitemDeselect() 
{
    m_pMap->ClearSelection();
    this->GetAV()->UpdateSelection();
    this->GetAV()->RefreshWindow();
}

void CChildView::OnMenuitemAddbookmark() 
{
    IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)

    pPlaceBookmark->AddBookmark("");

    this->GetAV()->RefreshWindow();
}

void CChildView::OnMenuitemNextbookmark() 
{
    IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)

    pPlaceBookmark->NextBookmark();
    pPlaceBookmark->SetViewToCurrentBookmark();
    this->UpdateView();
}

void CChildView::OnMenuitemPreviousbookmark() 
{
    IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)

    pPlaceBookmark->PreviousBookmark();
    pPlaceBookmark->SetViewToCurrentBookmark();
    this->UpdateView();
}

void CChildView::OnMenuitemDeletecurrentbookmark() 
{
    IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)

    long id = pPlaceBookmark->GetCurrentBookmarkID();
    pPlaceBookmark->DeleteBookmark(id);
    this->GetAV()->RefreshWindow();
}

void CChildView::OnMenuitemClearbookmarks() 
{
    IPlaceBookmarkPtr pPlaceBookmark;
    CAST_PTR(m_pMap, pPlaceBookmark, IPlaceBookmark)

    pPlaceBookmark->ClearBookmarks();
    this->GetAV()->RefreshWindow();
}

void CChildView::OnMenuitem32848() 
{
    double attitude;
    this->GetDT()->GetAttitude(attitude);
    if (attitude > 0.0001)
    {
        this->GetDT()->SetAttitude(0);
    }
    else
    {
        this->GetDT()->SetAttitude(65);
    }

    this->UpdateView();

}
