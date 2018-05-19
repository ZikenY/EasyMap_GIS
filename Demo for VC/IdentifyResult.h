#if !defined(AFX_IDENTIFYRESULT_H__6EC09F19_75D1_4138_942B_E121ADE075F7__INCLUDED_)
#define AFX_IDENTIFYRESULT_H__6EC09F19_75D1_4138_942B_E121ADE075F7__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// IdentifyResult.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CIdentifyResult dialog

class CIdentifyResult : public CDialog
{
// Construction
public:
	CIdentifyResult(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CIdentifyResult)
	enum { IDD = IDD_DIALOG_IDENTIFYRESULT };
	CString	m_Textout;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CIdentifyResult)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CIdentifyResult)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

public:
    void ClearAll();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_IDENTIFYRESULT_H__6EC09F19_75D1_4138_942B_E121ADE075F7__INCLUDED_)
