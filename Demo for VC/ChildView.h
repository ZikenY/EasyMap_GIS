// ChildView.h : interface of the CChildView class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_CHILDVIEW_H__F51F2FB8_4D83_4C5E_8293_35EC4784A45A__INCLUDED_)
#define AFX_CHILDVIEW_H__F51F2FB8_4D83_4C5E_8293_35EC4784A45A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CChildView window

#include "LayerTree.h"
#include "IdentifyResult.h"

#pragma warning(disable: 4786)
#include <vector>
#include <string>
using namespace std;

#include "..\\Include\\InterfaceMap.h"
#include "..\\Include\\InterfaceActiveView.h"
#include "..\\Include\\InterfaceTracker.h"
using namespace easymap;

typedef long MouseAction;
const MouseAction ActionDefault = 0;
const MouseAction ActionZoomIn  = 1;
const MouseAction ActionZoomOut = 2;
const MouseAction ActionPan     = 3;
const MouseAction ActionIdentify = 4;
const MouseAction ActionSelect  = 5;

class CChildView : public CWnd
{
// Construction
public:
	CChildView();

// Attributes
public:

// Operations
public:
    IActiveViewPtr GetAV() const;
    IDisplayPtr GetDisplay() const;
    IDisplayTransformationPtr GetDT() const;
    void MainSpaceResize();
    void UpdateView();
    void ViewFullMap();
    void RefreshLayerTree();
    void AddLayerTreeItem(HTREEITEM pParentItem, ILayerPtr pLayer);
    void LayerTreePosition();
    void Identify(const WKSRect& envelope);
    void ClearIdentify();
    void LoadE00(const CString& e00file, ILayerPtr& pLayer);
    void LoadBitmapLayer(const CString& bmpfile, ILayerPtr& pLayer);
    void SetLoadSlimParams();

    //这两个函数用来把所选择的geometry加到tracker中，以便于绘制移动要素
    void AddGeometriesToTracker(ILayerPtr pLayer);
    void AddLayerGeometriesToTracker(ILayerPtr pLayer);

    //这两个函数用来更新移动之后的对象
    void UpdateMoveGeometries(ILayerPtr pLayer);
    void UpdateMoveLayerGeometries(ILayerPtr pLayer);

    //这两个函数用来删除所选择的对象
    void DeleteSelectedGeometries(ILayerPtr pLayer);
    void DeleteLayerSelectedGeometries(ILayerPtr pLayer);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CChildView)
	public:
	virtual BOOL Create(LPCTSTR lpszClassName, LPCTSTR lpszWindowName, DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext = NULL);
	virtual BOOL DestroyWindow();
	protected:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CChildView();

	// Generated message map functions
protected:
	//{{AFX_MSG(CChildView)
    afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	afx_msg void OnPaint();
    afx_msg void OnSize(UINT nType, int cx, int cy);
    afx_msg void OnMouseMove(UINT nFlags, CPoint point);
    afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
    afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
    afx_msg BOOL OnMouseWheel(UINT nFlags, short zDelta, CPoint pt);
	afx_msg void OnMenuitemWorkspacenew();
	afx_msg void OnMenuitemWorkspaceopen();
	afx_msg void OnMenuitemWorkspacesave();
	afx_msg void OnMenuitemWorkspacesaveas();
	afx_msg void OnMenuitemZoomin();
	afx_msg void OnMenuitemZoomout();
	afx_msg void OnMenuitemZoompan();
	afx_msg void OnMenuitemZoomall();
	afx_msg void OnMenuitemRefreshview();
	afx_msg void OnMenuitemLoaddata();
	afx_msg void OnMenuitemIdentify();
	afx_msg void OnMenuitemLayertree();
	afx_msg void OnMenuitemUndo();
	afx_msg void OnMenuitemRedo();
	afx_msg void OnMenuitemSaveedit();
	afx_msg void OnMenuitemCanceledit();
	afx_msg void OnMenuitemSelect();
	afx_msg void OnMenuitemDelete();
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMenuitemDeselect();
	afx_msg void OnMenuitemAddbookmark();
	afx_msg void OnMenuitemNextbookmark();
	afx_msg void OnMenuitemPreviousbookmark();
	afx_msg void OnMenuitemDeletecurrentbookmark();
	afx_msg void OnMenuitemClearbookmarks();
	afx_msg void OnMenuitem32848();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

    LRESULT OnLayerTreeKeyDown(WPARAM wParam, LPARAM lParam);
    LRESULT OnLayerTreeLButtonDown(WPARAM wParam, LPARAM lParam);
    LRESULT OnLayerTreeLButtonUp(WPARAM wParam, LPARAM lParam);
    LRESULT OnLayerTreeRButtonDown(WPARAM wParam, LPARAM lParam);
    LRESULT OnLayerTreeRButtonUp(WPARAM wParam, LPARAM lParam);
    LRESULT OnLayerTreeLDoubleClick(WPARAM wParam, LPARAM lParam);
    LRESULT OnLayerTreeItemCheck(WPARAM wParam, LPARAM lParam);

private:

    //
    IMapPtr         m_pMap;

    //图层树
    CLayerTree      m_LayerTree;
    CMyTreeCtrl*    m_pLayerTree;
    CImageList      m_ImageList1;
    HTREEITEM       m_pTreeRoot;

    CIdentifyResult m_IdentifyResult;

    //当前工作空间文件
    CString         m_EWS;

    //当前鼠标操作
    MouseAction     m_MouseAction;

    //鼠标按下记录
    vector<tagPOINT> m_TrackPoints;

    //自己的 dc
    HDC             m_WindowDC;

    //移动对象
    IMoveTrackerPtr m_pMoveTracker;

    //初始化搞定
    bool            m_InitOK;

    MapUnits        m_LoadData_Unit;
    double          m_LoadData_Scale;
    double          m_LoadData_Precision;
    long            m_LoadData_Indexlevel;
    bool            m_LoadData_Readonly;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CHILDVIEW_H__F51F2FB8_4D83_4C5E_8293_35EC4784A45A__INCLUDED_)
