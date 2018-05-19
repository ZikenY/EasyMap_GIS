// LoadSlimParams.cpp : implementation file
//

#include "stdafx.h"
#include "easyview.h"
#include "LoadSlimParams.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLoadSlimParams dialog


CLoadSlimParams::CLoadSlimParams(CWnd* pParent /*=NULL*/)
    : CDialog(CLoadSlimParams::IDD, pParent)
{
    //{{AFX_DATA_INIT(CLoadSlimParams)
    m_Unit = -1;
    m_Scale = 0.0;
    m_Precision = 0.0;
    m_Indexlevel = 0;
    m_Readonly = FALSE;
	//}}AFX_DATA_INIT

    m_InitOK = FALSE;
}


void CLoadSlimParams::DoDataExchange(CDataExchange* pDX)
{
    CDialog::DoDataExchange(pDX);
    //{{AFX_DATA_MAP(CLoadSlimParams)
    DDX_CBIndex(pDX, IDC_COMBO_UNIT, m_Unit);
    DDX_Text(pDX, IDC_EDIT_SCALE, m_Scale);
    DDX_Text(pDX, IDC_EDIT_PRECISION, m_Precision);
    DDX_Text(pDX, IDC_EDIT_INDEXLEVEL, m_Indexlevel);
    DDX_Check(pDX, IDC_CHECK_READONLY, m_Readonly);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CLoadSlimParams, CDialog)
    //{{AFX_MSG_MAP(CLoadSlimParams)
    ON_WM_CREATE()
    ON_WM_SHOWWINDOW()
    //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLoadSlimParams message handlers

int CLoadSlimParams::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
    if (CDialog::OnCreate(lpCreateStruct) == -1)
        return -1;

    return 0;
}

void CLoadSlimParams::OnShowWindow(BOOL bShow, UINT nStatus) 
{
    CDialog::OnShowWindow(bShow, nStatus);

    if (!m_InitOK)
    {
        m_Unit = 1;
        m_Scale = 2000;
        m_Precision = 0.01;
        m_Indexlevel = 5;
        m_Readonly = FALSE;
        m_InitOK = TRUE;
    }

    this->UpdateData(FALSE);
}

void CLoadSlimParams::OnOK() 
{
    this->UpdateData(TRUE);
        
    CDialog::OnOK();
}
