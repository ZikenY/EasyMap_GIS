#if !defined(AFX_MYTREECTRL_H__7055B5DC_2A90_4649_8D64_3B18A73E3304__INCLUDED_)
#define AFX_MYTREECTRL_H__7055B5DC_2A90_4649_8D64_3B18A73E3304__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// MyTreeCtrl.h : header file
//

#include "stdafx.h"

#define WM_MYTREE_KEYDOWN       (WM_USER+110)
#define WM_MYTREE_LBUTTONDOWN   (WM_USER+111)
#define WM_MYTREE_LBUTTONUP     (WM_USER+112)
#define WM_MYTREE_RBUTTONDOWN   (WM_USER+113)
#define WM_MYTREE_RBUTTONUP     (WM_USER+114)
#define WM_MYTREE_LBUTTONDBLCLK (WM_USER+115)
#define WM_MYTREE_ITEMCHECK     (WM_USER+116)

long Point2Long(const CPoint& point);
CPoint Long2Point(const long lParam);

/////////////////////////////////////////////////////////////////////////////
// CMyTreeCtrl window

class CMyTreeCtrl : public CTreeCtrl
{
// Construction
public:
	CMyTreeCtrl();

// Attributes
public:
    CWnd* m_pParent;
    BOOL m_DisableDBLClickExpand;
    BOOL m_RightSelected;

// Operations

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMyTreeCtrl)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CMyTreeCtrl();

	// Generated message map functions
protected:
	//{{AFX_MSG(CMyTreeCtrl)
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()

private:
    void FireItemCheckEvent(HTREEITEM pItem);
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MYTREECTRL_H__7055B5DC_2A90_4649_8D64_3B18A73E3304__INCLUDED_)
