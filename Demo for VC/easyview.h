// easyview.h : main header file for the EASYVIEW application
//

#if !defined(AFX_EASYVIEW_H__FA27251F_B055_480E_BC90_9D37238BD64D__INCLUDED_)
#define AFX_EASYVIEW_H__FA27251F_B055_480E_BC90_9D37238BD64D__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CEasyviewApp:
// See easyview.cpp for the implementation of this class
//

class CEasyviewApp : public CWinApp
{
public:
	CEasyviewApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEasyviewApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

public:
	//{{AFX_MSG(CEasyviewApp)
	afx_msg void OnAppAbout();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EASYVIEW_H__FA27251F_B055_480E_BC90_9D37238BD64D__INCLUDED_)
