#if !defined(AFX_LAYERTREE_H__B578EB48_DC94_45FA_8394_2BE126EB35CE__INCLUDED_)
#define AFX_LAYERTREE_H__B578EB48_DC94_45FA_8394_2BE126EB35CE__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// LayerTree.h : header file
//

#include "MyTreeCtrl.h"

/////////////////////////////////////////////////////////////////////////////
// CLayerTree dialog

class CLayerTree : public CDialog
{
// Construction
public:
	CLayerTree(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CLayerTree)
	enum { IDD = IDD_DIALOG_LAYERTREE };
	CMyTreeCtrl	m_LayerTree;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLayerTree)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation

    BOOL m_InitOK;

protected:

	// Generated message map functions
	//{{AFX_MSG(CLayerTree)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_LAYERTREE_H__B578EB48_DC94_45FA_8394_2BE126EB35CE__INCLUDED_)
