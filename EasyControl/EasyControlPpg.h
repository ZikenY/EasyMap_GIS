#if !defined(AFX_EASYCONTROLPPG_H__484C07F4_6AED_4175_A946_9D50146EF400__INCLUDED_)
#define AFX_EASYCONTROLPPG_H__484C07F4_6AED_4175_A946_9D50146EF400__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// EasyControlPpg.h : Declaration of the CEasyControlPropPage property page class.

////////////////////////////////////////////////////////////////////////////
// CEasyControlPropPage : See EasyControlPpg.cpp.cpp for implementation.

class CEasyControlPropPage : public COlePropertyPage
{
	DECLARE_DYNCREATE(CEasyControlPropPage)
	DECLARE_OLECREATE_EX(CEasyControlPropPage)

// Constructor
public:
	CEasyControlPropPage();

// Dialog Data
	//{{AFX_DATA(CEasyControlPropPage)
	enum { IDD = IDD_PROPPAGE_EASYCONTROL };
		// NOTE - ClassWizard will add data members here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA

// Implementation
protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

// Message maps
protected:
	//{{AFX_MSG(CEasyControlPropPage)
		// NOTE - ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EASYCONTROLPPG_H__484C07F4_6AED_4175_A946_9D50146EF400__INCLUDED)
