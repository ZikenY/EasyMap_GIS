// IdentifyResult.cpp : implementation file
//

#include "stdafx.h"
#include "easyview.h"
#include "IdentifyResult.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CIdentifyResult dialog


CIdentifyResult::CIdentifyResult(CWnd* pParent /*=NULL*/)
	: CDialog(CIdentifyResult::IDD, pParent)
{
	//{{AFX_DATA_INIT(CIdentifyResult)
	m_Textout = _T("");
	//}}AFX_DATA_INIT
}


void CIdentifyResult::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CIdentifyResult)
	DDX_Text(pDX, IDC_EDIT1, m_Textout);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CIdentifyResult, CDialog)
	//{{AFX_MSG_MAP(CIdentifyResult)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIdentifyResult message handlers

void CIdentifyResult::ClearAll()
{
    m_Textout = "Oops!\r\n\r\n";
}
