#if !defined(AFX_EASYCONTROL_H__D90DDB90_B578_4D56_BD18_1C958DF238C3__INCLUDED_)
#define AFX_EASYCONTROL_H__D90DDB90_B578_4D56_BD18_1C958DF238C3__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// EasyControl.h : main header file for EASYCONTROL.DLL

#if !defined( __AFXCTL_H__ )
	#error include 'afxctl.h' before including this file
#endif

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CEasyControlApp : See EasyControl.cpp for implementation.

class CEasyControlApp : public COleControlModule
{
public:
	BOOL InitInstance();
	int ExitInstance();
};

const CEasyControlApp* GetApp();

extern const GUID CDECL _tlid;
extern const WORD _wVerMajor;
extern const WORD _wVerMinor;

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EASYCONTROL_H__D90DDB90_B578_4D56_BD18_1C958DF238C3__INCLUDED)
