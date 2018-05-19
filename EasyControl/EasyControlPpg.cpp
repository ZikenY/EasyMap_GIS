// EasyControlPpg.cpp : Implementation of the CEasyControlPropPage property page class.

#include "stdafx.h"
#include "EasyControl.h"
#include "EasyControlPpg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


IMPLEMENT_DYNCREATE(CEasyControlPropPage, COlePropertyPage)


/////////////////////////////////////////////////////////////////////////////
// Message map

BEGIN_MESSAGE_MAP(CEasyControlPropPage, COlePropertyPage)
	//{{AFX_MSG_MAP(CEasyControlPropPage)
	// NOTE - ClassWizard will add and remove message map entries
	//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// Initialize class factory and guid

IMPLEMENT_OLECREATE_EX(CEasyControlPropPage, "EASYCONTROL.EasyControlPropPage.1",
	0x1080f464, 0x23aa, 0x4f3b, 0x90, 0xf2, 0xd2, 0x68, 0x9d, 0x77, 0xb3, 0xce)


/////////////////////////////////////////////////////////////////////////////
// CEasyControlPropPage::CEasyControlPropPageFactory::UpdateRegistry -
// Adds or removes system registry entries for CEasyControlPropPage

BOOL CEasyControlPropPage::CEasyControlPropPageFactory::UpdateRegistry(BOOL bRegister)
{
	if (bRegister)
		return AfxOleRegisterPropertyPageClass(AfxGetInstanceHandle(),
			m_clsid, IDS_EASYCONTROL_PPG);
	else
		return AfxOleUnregisterClass(m_clsid, NULL);
}


/////////////////////////////////////////////////////////////////////////////
// CEasyControlPropPage::CEasyControlPropPage - Constructor

CEasyControlPropPage::CEasyControlPropPage() :
	COlePropertyPage(IDD, IDS_EASYCONTROL_PPG_CAPTION)
{
	//{{AFX_DATA_INIT(CEasyControlPropPage)
	// NOTE: ClassWizard will add member initialization here
	//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA_INIT
}


/////////////////////////////////////////////////////////////////////////////
// CEasyControlPropPage::DoDataExchange - Moves data between page and properties

void CEasyControlPropPage::DoDataExchange(CDataExchange* pDX)
{
	//{{AFX_DATA_MAP(CEasyControlPropPage)
	// NOTE: ClassWizard will add DDP, DDX, and DDV calls here
	//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA_MAP
	DDP_PostProcessing(pDX);
}


/////////////////////////////////////////////////////////////////////////////
// CEasyControlPropPage message handlers
