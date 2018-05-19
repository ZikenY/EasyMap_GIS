// LayerTree.cpp : implementation file
//

#include "stdafx.h"
#include "easyview.h"
#include "LayerTree.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLayerTree dialog


CLayerTree::CLayerTree(CWnd* pParent /*=NULL*/)
	: CDialog(CLayerTree::IDD, pParent)
{
	//{{AFX_DATA_INIT(CLayerTree)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT

    m_InitOK = FALSE;
}


void CLayerTree::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CLayerTree)
	DDX_Control(pDX, IDC_TREE_LAYERS, m_LayerTree);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CLayerTree, CDialog)
	//{{AFX_MSG_MAP(CLayerTree)
	ON_WM_SHOWWINDOW()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLayerTree message handlers

void CLayerTree::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CDialog::OnShowWindow(bShow, nStatus);
	
	m_InitOK = TRUE;	
}
