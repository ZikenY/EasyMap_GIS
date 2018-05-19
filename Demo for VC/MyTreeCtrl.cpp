// MyTreeCtrl.cpp : implementation file
//

#include "stdafx.h"
#include "MyTreeCtrl.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

long Point2Long(const CPoint& point)
{
    short posx = (short)point.x;
    short posy = (short)point.y;
    long tmpx = 0x0000FFFF & posx;
    long tmpy = posy << 16;
    return tmpy | tmpx;
}

CPoint Long2Point(const long lParam)
{
    //把lParam拆成高低各两个字节
    short x = (short)(lParam & 0x0000FFFF);
    short y = (short)((lParam >> 16));
    CPoint point(x, y);
    return point;
}

/////////////////////////////////////////////////////////////////////////////
// CMyTreeCtrl

CMyTreeCtrl::CMyTreeCtrl()
{
    m_pParent = NULL;
    m_DisableDBLClickExpand = FALSE;
    m_RightSelected = TRUE;
}

CMyTreeCtrl::~CMyTreeCtrl()
{
}

BEGIN_MESSAGE_MAP(CMyTreeCtrl, CTreeCtrl)
	//{{AFX_MSG_MAP(CMyTreeCtrl)
	ON_WM_KEYDOWN()
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_RBUTTONDOWN()
	ON_WM_RBUTTONUP()
	ON_WM_LBUTTONDBLCLK()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMyTreeCtrl message handlers

void CMyTreeCtrl::FireItemCheckEvent(HTREEITEM pItem)
{
    if (pItem && m_pParent)
    {
        BOOL check = this->GetCheck(pItem);
        m_pParent->PostMessage(WM_MYTREE_ITEMCHECK, (unsigned int)pItem, (long)check);
    }
}

void CMyTreeCtrl::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
    HTREEITEM pItem = this->GetSelectedItem();
    BOOL checked = FALSE;
    if (pItem)
    {
        checked = this->GetCheck(pItem);
    }

    CTreeCtrl::OnKeyDown(nChar, nRepCnt, nFlags);

    if (m_pParent)
    {
        m_pParent->PostMessage(WM_MYTREE_KEYDOWN, nChar, nFlags);
    }

    if (pItem && (this->GetCheck(pItem) != checked))
    {
        this->FireItemCheckEvent(pItem);
    }
}

void CMyTreeCtrl::OnLButtonDown(UINT nFlags, CPoint point) 
{
    HTREEITEM pItem = this->HitTest(point);
    BOOL checked;
    if (pItem)
    {
        checked = this->GetCheck(pItem);
    }

    CTreeCtrl::OnLButtonDown(nFlags, point);

    if (m_pParent)
    {
        m_pParent->PostMessage(WM_MYTREE_LBUTTONDOWN, nFlags, Point2Long(point));
    }

    if (pItem && (this->GetCheck(pItem) != checked))
    {
        this->FireItemCheckEvent(pItem);
    }
}

void CMyTreeCtrl::OnLButtonUp(UINT nFlags, CPoint point) 
{
    CTreeCtrl::OnLButtonUp(nFlags, point);
    if (m_pParent)
    {
        m_pParent->PostMessage(WM_MYTREE_LBUTTONUP, nFlags, Point2Long(point));
    }
}

void CMyTreeCtrl::OnRButtonDown(UINT nFlags, CPoint point) 
{
    CTreeCtrl::OnRButtonDown(nFlags, point);

    if (m_RightSelected)
    {
        HTREEITEM pItem = this->HitTest(point);
        if (pItem)
        {
            this->SelectItem(pItem);
        }
    }

    if (m_pParent)
    {
        m_pParent->PostMessage(WM_MYTREE_RBUTTONDOWN, nFlags, Point2Long(point));
    }
}

void CMyTreeCtrl::OnRButtonUp(UINT nFlags, CPoint point) 
{
    CTreeCtrl::OnRButtonUp(nFlags, point);
    if (m_pParent)
    {
        m_pParent->PostMessage(WM_MYTREE_RBUTTONUP, nFlags, Point2Long(point));
    }
}

void CMyTreeCtrl::OnLButtonDblClk(UINT nFlags, CPoint point) 
{
    CTreeCtrl::OnLButtonDblClk(nFlags, point);
    HTREEITEM pCurItem = this->GetSelectedItem();
    if (pCurItem)
    {
        this->Expand(pCurItem, TVE_TOGGLE);
    }

    if (m_pParent)
    {
        m_pParent->PostMessage(WM_MYTREE_LBUTTONDBLCLK, nFlags, Point2Long(point));
    }
}
