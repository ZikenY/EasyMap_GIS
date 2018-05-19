#if !defined(AFX_LOADSLIMPARAMS_H__8FD8AAA3_8E1E_4C7B_B5DB_0DE403391416__INCLUDED_)
#define AFX_LOADSLIMPARAMS_H__8FD8AAA3_8E1E_4C7B_B5DB_0DE403391416__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// LoadSlimParams.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CLoadSlimParams dialog

class CLoadSlimParams : public CDialog
{
// Construction
public:
	CLoadSlimParams(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CLoadSlimParams)
	enum { IDD = IDD_DIALOG_LOADSLIMPARAMS };
	int		m_Unit;
	double	m_Scale;
	double	m_Precision;
	int		m_Indexlevel;
	BOOL	m_Readonly;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLoadSlimParams)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CLoadSlimParams)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
    BOOL m_InitOK;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_LOADSLIMPARAMS_H__8FD8AAA3_8E1E_4C7B_B5DB_0DE403391416__INCLUDED_)
