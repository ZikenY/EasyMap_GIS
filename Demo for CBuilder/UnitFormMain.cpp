//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "..\\easylib\\ElementLayer.h"
#include "..\\easylib\\SlimLayer.h"
#include "..\\easylib\\GroupLayer.h"
#include "..\\easylib\\MemoryStream.h"
#include "..\\easylib\\SimpleSymbol.h"
#include "..\\easylib\\StringFuncs.h"
#include "..\\easylib\\ShapeAux.h"
#include "..\\easylib\\ShapeLayer.h"

#include "UnitFormMain.h"
#include "UnitFormLabelManager.h"


//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TFormMain *FormMain;
//---------------------------------------------------------------------------
__fastcall TFormMain::TFormMain(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------

const long EDITSTATUS_DIRTY     = 0;
const long EDITSTATUS_UNDOABLE  = 1;
const long EDITSTATUS_REDOABLE  = 2;

//将颜色值取反
inline COLORREF InvertRGB(const COLORREF color)
{
    char rvalue = 255 - GetRValue(color);
    char gvalue = 255 - GetGValue(color);
    char bvalue = 255 - GetBValue(color);
    return RGB(rvalue, gvalue, bvalue);
};

inline COLORREF GetShitColor()
{
    ::srand((unsigned)time(NULL));
    static bool gg = false;
    gg = !gg;
    if (gg)
    {
        return RGB(long((1 - double(::rand()) / RAND_MAX) * 255),
            long((1- double(::rand()) / RAND_MAX) * 255),
            long((1 - double(::rand()) / RAND_MAX) * 255));
    }
    else
    {
        return RGB(long((double(::rand()) / RAND_MAX) * 255),
            long((double(::rand()) / RAND_MAX) * 255),
            long((double(::rand()) / RAND_MAX) * 255));
    }
}

//十字丝
inline void TrackCross(const HDC dc, const COLORREF color, const long rop2,
    const tagPOINT cnt, const long linelen = 10)
{
    HPEN pen = ::CreatePen(PS_SOLID, 1, color);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    long rop2saved = ::SetROP2(dc, rop2);
    ::MoveToEx(dc, cnt.x - linelen, cnt.y, NULL);
    ::LineTo(dc, cnt.x + linelen, cnt.y);
    ::MoveToEx(dc, cnt.x, cnt.y - linelen + 1, NULL);
    ::LineTo(dc, cnt.x, cnt.y + linelen - 1);
    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
}

//在dc上画矩形框
inline void TrackEnvelope(const HDC dc, const COLORREF color, const long rop2,
    const long pensize, const tagPOINT p1, const tagPOINT p2)
{
    HPEN pen = ::CreatePen(PS_SOLID, pensize, color);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    long rop2saved = ::SetROP2(dc, rop2);
    ::MoveToEx(dc, p1.x, p1.y, NULL);
    ::LineTo(dc, p2.x, p1.y);
    ::LineTo(dc, p2.x, p2.y);
    ::LineTo(dc, p1.x, p2.y);
    ::LineTo(dc, p1.x, p1.y);

    tagPOINT envcnt;
    envcnt.x = (p1.x + p2.x) / 2;
    envcnt.y = (p1.y + p2.y) / 2;
    TrackCross(dc, RGB(255, 0, 0), rop2, envcnt);

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
};

void TrackNode(const HDC dc, const COLORREF color, const long x, const long y)
{
    LOGBRUSH logbrush;
    logbrush.lbColor = color;
    logbrush.lbStyle = BS_SOLID;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    int rop2saved = ::SetROP2(dc, R2_COPYPEN);

    tagRECT node = {x - 4, y - 4, x + 4, y + 4};
    ::FillRect(dc, &node, brush);

    ::DeleteObject(brush);
    ::SetROP2(dc, rop2saved);
}

void DrawEnvelope(const HDC dc, const long rop2, const COLORREF fillcolor,
    const COLORREF linecolor, const long pensize, const tagPOINT p1, const tagPOINT p2)
{
    RECT rect;
    rect.left = p1.x;
    rect.top = p1.y;
    rect.right = p2.x;
    rect.bottom = p2.y;

    HPEN pen = ::CreatePen(PS_SOLID, pensize, linecolor);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    LOGBRUSH logbrush;
    logbrush.lbStyle = BS_SOLID;
    logbrush.lbColor = fillcolor;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    long rop2saved = ::SetROP2(dc, rop2);

    ::FillRect(dc, &rect, brush);
    ::MoveToEx(dc, p1.x, p1.y, NULL);
    ::LineTo(dc, p2.x, p1.y);
    ::LineTo(dc, p2.x, p2.y);
    ::LineTo(dc, p1.x, p2.y);
    ::LineTo(dc, p1.x, p1.y);

    ::SetROP2(dc, rop2saved);
    ::DeleteObject(brush);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
};

void DrawCircle(const HDC dc, const long rop2, const COLORREF fillcolor,
    const COLORREF linecolor, const long pensize, const tagPOINT center,
    const long radius)
{
    RECT rect;
    rect.left = center.x - radius;
    rect.top = center.y - radius;
    rect.right = center.x + radius;
    rect.bottom = center.y + radius;

    HPEN pen = ::CreatePen(PS_SOLID, pensize, linecolor);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    LOGBRUSH logbrush;
    logbrush.lbStyle = BS_SOLID;
    logbrush.lbColor = fillcolor;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    HBRUSH brushsaved = (HBRUSH)::SelectObject(dc, brush);
    long rop2saved = ::SetROP2(dc, rop2);

    ::Ellipse(dc, rect.left, rect.top, rect.right, rect.bottom);

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, brushsaved);
    ::DeleteObject(brush);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
};

void DrawEllipse(const HDC dc, const long rop2, const COLORREF fillcolor,
    const COLORREF linecolor, const long pensize, const tagPOINT p1, const tagPOINT p2)
{
    RECT rect;
    rect.left = p1.x;
    rect.top = p1.y;
    rect.right = p2.x;
    rect.bottom = p2.y;

    HPEN pen = ::CreatePen(PS_SOLID, pensize, linecolor);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    LOGBRUSH logbrush;
    logbrush.lbStyle = BS_SOLID;
    logbrush.lbColor = fillcolor;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    HBRUSH brushsaved = (HBRUSH)::SelectObject(dc, brush);
    long rop2saved = ::SetROP2(dc, rop2);

    ::Ellipse(dc, rect.left, rect.top, rect.right, rect.bottom);

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, brushsaved);
    ::DeleteObject(brush);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
};

void DrawPolyline(const HDC dc, const long rop2, const COLORREF linecolor,
    const long pensize, const vector<tagPOINT> points)
{
    HPEN pen = ::CreatePen(PS_SOLID, pensize, linecolor);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    long rop2saved = ::SetROP2(dc, rop2);

    ::MoveToEx(dc, points[0].x, points[0].y, NULL);
    for (dword i = 0; i < points.size() - 1; i++)
    {
        ::LineTo(dc, points[i + 1].x, points[i + 1].y);
    }

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
}

void DrawPolygon(const HDC dc, const long rop2, const COLORREF fillcolor,
    const COLORREF linecolor, const long pensize, const vector<tagPOINT> points)
{
    HPEN pen = ::CreatePen(PS_SOLID, pensize, linecolor);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    LOGBRUSH logbrush;
    logbrush.lbStyle = BS_SOLID;
    logbrush.lbColor = fillcolor;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    HBRUSH brushsaved = (HBRUSH)::SelectObject(dc, brush);
    long rop2saved = ::SetROP2(dc, rop2);
    long lastpoint = points.size() - 1;

    if (points.size() > 1)
    {
        dword i;
        ::BeginPath(dc);
        if (points.size() > 2)
        {
            ::MoveToEx(dc, points[0].x, points[0].y, NULL);
            for (i = 0; i < points.size() - 1; i++)
            {
                ::LineTo(dc, points[i + 1].x, points[i + 1].y);
            }
        }
        else
        {
            ::MoveToEx(dc, points[lastpoint].x, points[lastpoint].y, NULL);
        }

        ::LineTo(dc, points[0].x, points[0].y);
        ::EndPath(dc);
        ::FillPath(dc);

        if (points.size() > 2)
        {
            ::MoveToEx(dc, points[0].x, points[0].y, NULL);
            for (i = 0; i < points.size() - 1; i++)
            {
                ::LineTo(dc, points[i + 1].x, points[i + 1].y);
            }
        }
        else
        {
            ::MoveToEx(dc, points[lastpoint].x, points[lastpoint].y, NULL);
        }

        ::LineTo(dc, points[0].x, points[0].y);
    }

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, brushsaved);
    ::DeleteObject(brush);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
}

//将top和left变成0
inline tagRECT TrimRect(const tagRECT rect)
{
    tagRECT rect1;
    rect1.right = rect.right - rect.left;
    rect1.bottom = rect.bottom - rect.top;
    rect1.left = rect1.top = 0;
    return rect1;
};

void _stdcall VisibleExtentChangeHandle(const WKSRect& viewextent)
{
    FormMain->RefreshScaleDisplay();
    FormMain->PushViewExt(viewextent);
};

void LayerDataUpdate(ILayerPtr pLayer, const bool save)
{
    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            LayerDataUpdate(pL, save);
        }
        return;
    }

    IEditLayerPtr pEL;
    CAST_PTR(pLayer, pEL, IEditLayer)
//    if (pEL.Assigned() && pEL->IsDirty())
    if (pEL.Assigned())
    {
        save ? pEL->SaveData() : pEL->EditCancel();
    }

    return;
};

dword DeleteSelectedFeatures(ILayerPtr pLayer)
{
    dword result = 0;

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            result += DeleteSelectedFeatures(pL);
        }
        return result;
    }

    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        vector<dword> fids;
        pVL->GetSelection(fids);
        vector<dword>::const_iterator it = fids.begin();
        while (it != fids.end())
        {
            pVL->DeleteFeature(*(it++));
            result++;
        }
    }

    return result;
};

bool SelectFeatures(ILayerPtr pLayer, const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    bool result = false;

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            if (SelectFeatures(pL, envelope, partialselect, append))
            {
                result = true;
            }
        }
        return result;
    }

    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        if ((pVL->GetSelectable()) && (0 < pLayer->Select(envelope, partialselect, append)))
        {
            result = true;
        }
    }

    return result;
}

bool DeselectFeatures(ILayerPtr pLayer, const WKSRect& envelope, const bool partialselect)
{
    bool result = false;

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            if (DeselectFeatures(pL, envelope, partialselect))
            {
                result = true;
            }
        }
        return result;
    }

    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        if ((pVL->GetSelectable()) && (0 < pLayer->Deselect(envelope, partialselect)))
        {
            result = true;
        }
    }

    return result;
}

dword GetElementLayerCount(ILayerPtr pLayer)
{
    dword result = 0;

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            result += GetElementLayerCount(pL);
        }
        return result;
    }

    CElementLayerPtr pEL;
    CAST_PTR(pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        result = 1;
    }

    return result;
};

bool SelectElements(ILayerPtr pLayer, const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    bool result = false;

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            if (SelectElements(pL, envelope, partialselect, append))
            {
                result = true;
            }
        }
        return result;
    }

    CElementLayerPtr pEL;
    CAST_PTR(pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        if (0 < pLayer->Select(envelope, partialselect, append))
        {
            result = true;
        }
    }

    return result;
}

bool SelectElements(const CMapPtr pMap, const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    bool r = false;
    dword count = pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        pMap->GetLayer(pLayer, i);
        if (SelectElements(pLayer, envelope, partialselect, append))
        {
            r = true;
        }
    }

    return r;
}

bool DeselectElements(ILayerPtr pLayer, const WKSRect* pEnvelope, const bool partialselect)
{
    bool result = false;
    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            if (DeselectElements(pL, pEnvelope, partialselect))
            {
                result = true;
            }
        }
        return result;
    }

    CElementLayerPtr pEL;
    CAST_PTR(pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        if (pEnvelope)
        {
            if (0 < pLayer->Deselect(*pEnvelope, partialselect))
            {
                result = true;
            }
        }
        else
        {
            pLayer->ClearSelection();
            result = true;
        }
    }

    return result;
}

bool DeselectElements(const CMapPtr pMap, const WKSRect* pEnvelope, const bool partialselect)
{
    bool r = false;
    dword count = pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        pMap->GetLayer(pLayer, i);
        if (DeselectElements(pLayer, pEnvelope, partialselect))
        {
            r = true;
        }
    }

    return r;
}

bool DeleteElements(ILayerPtr pLayer)
{
    bool result = false;

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            if (DeleteElements(pL))
            {
                result = true;
            }
        }
        return result;
    }

    CElementLayerPtr pEL;
    CAST_PTR(pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        result = pEL->RemoveSelectedElements();
    }

    return result;
};

bool IsFeatureSelected(ILayerPtr pLayer)
{
    bool result = false;

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            result = IsFeatureSelected(pL);
            if (result)
            {
                break;
            }
        }
        return result;
    }

    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        if (pVL->GetSelectCount() > 0)
        {
            result = true;
        }
    }

    return result;
}

bool IsFeatureSelected(const CMapPtr pMap)
{
    dword count = pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        pMap->GetLayer(pLayer, i);
        if (IsFeatureSelected(pLayer))
        {
            return true;
        }
    }

    return false;    
}

bool CanMoveAndDeleteFeatures(ILayerPtr pLayer)
{
    bool result = false;

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            result = CanMoveAndDeleteFeatures(pL);
            if (result)
            {
                break;
            }
        }
        return result;
    }

    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        vector<dword> fids;
        if (!pVL->ReadOnly() && (pVL->GetSelectCount() > 0))
        {
            result = true;
        }
    }

    return result;
}

bool CanMoveAndDeleteFeatures(const CMapPtr pMap)
{
    dword count = pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        pMap->GetLayer(pLayer, i);
        if (CanMoveAndDeleteFeatures(pLayer))
        {
            return true;
        }
    }

    return false;
}

bool IsElementSelected(const ILayerPtr pLayer)
{
    bool result = false;

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            result = IsElementSelected(pL);
            if (result)
            {
                break;
            }
        }
        return result;
    }

    CElementLayerPtr pEL;
    CAST_PTR(pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        if (pEL->GetSelectCount() > 0)
        {
            result = true;
        }
    }

    return result;
}

bool IsElementSelected(const CMapPtr pMap)
{
    dword count = pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        pMap->GetLayer(pLayer, i);
        bool r = IsElementSelected(pLayer);
        if (r)
        {
            return true;
        }
    }

    return false;
}

bool IsLayerNameExist(ILayerPtr pLayer, const string& layername)
{
    bool result = false;

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            result = IsLayerNameExist(pL, layername);
            if (result)
            {
                return true;
            }
        }
    }

    string name1 = easymap::Trim(layername);
    string name2 = easymap::Trim(pLayer->GetName());
    return (name1 == name2) ? true : false;
}

bool TFormMain::EditStatus(const long statustype) const
{
//  statustype == 0 : is_dirty
//  statustype == 1 : undoable
//  statustype == 2 : redoable
    switch (statustype)
    {
    case 0:
        return m_pMap->IsDirty();
    case 1:
        return m_pMap->EditUndoable();
    case 2:
        return m_pMap->EditRedoable();
    default:
        {
            ShowMessage("去死吧！");
            return false;
        }
    }
}

bool TFormMain::PromptSaveEdit()
{
    if (this->EditStatus(EDITSTATUS_DIRTY))
    {
        int dlgr = MessageDlg("当前工作空间中编辑过的数据尚未保存，保存吗？", mtConfirmation,
            TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0);
        if (mrYes == dlgr)
        {
            this->NSaveEditClick(this);
        }
        else if (mrNo == dlgr)
        {
            this->NCancelEditClick(this);
        }
        else
        {
            return false;
        }
    }
    return true;
}

void TFormMain::InAction()
{
    SetMouseAction(m_MouseAction);
}

void TFormMain::ZoomIn()
{
    SetMouseAction(MouseActionZoomInOut);
}

void TFormMain::ZoomPan()
{
    SetMouseAction(MouseActionPan);
}

void TFormMain::ZoomCenter(const double quotiety)
{
    CDisplayPtr pDisplay;
    this->GetAV()->GetDisplay(pDisplay);
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);
    double scale;
    pTrans->GetMapScale(scale);
    pTrans->SetMapScale(scale * quotiety);
    this->UpdateView();
}

void TFormMain::ZoomAll()
{
    ViewFullMap();
}

void TFormMain::UpdateView()
{
    CActiveViewPtr pAV = this->GetAV();
    pAV->UpdateData();
    pAV->UpdateSelection();
    pAV->RefreshWindow();
}

void TFormMain::PartialUpdate(const WKSRect& env)
{
    CActiveViewPtr pAV = this->GetAV();
    pAV->DrawData(NULL, &env);
    pAV->RefreshWindow();
}

void TFormMain::UpdateSelection(const WKSRect* const pEnvelope)
{
    if (pEnvelope)
    {
        this->GetAV()->DrawSelection(pEnvelope);
    }
    else
    {
        this->GetAV()->UpdateSelection();
    }
}

void TFormMain::SelectFeaturesByEnvelope()
{
    SetMouseAction(MouseActionSelectByEnvelope);
}

void TFormMain::ViewFullMap()
{
    WKSRect extent;
    m_pMap->GetFullExtent(extent);
    CDisplayPtr pDisplay;
    this->GetAV()->GetDisplay(pDisplay);
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);
    pTrans->SetVisibleExtent(extent);

    this->UpdateView();
}

CActiveViewPtr TFormMain::GetAV() const
{
    CActiveViewPtr pAV;
    CAST_PTR(m_pMap, pAV, CActiveView)
    return pAV;
}

CDisplayPtr TFormMain::GetDisplay() const
{
    CDisplayPtr pDisplay;
    this->GetAV()->GetDisplay(pDisplay);
    return pDisplay;
}

CDisplayTransformationPtr TFormMain::GetDT() const
{
    CDisplayTransformationPtr pTrans;
    this->GetDisplay()->GetDisplayTransformation(pTrans);
    return pTrans;
}

bool TFormMain::LoadWorkspace(const char* pcTwsFile)
{
    CStreamPtr pStream = new CMemoryStream;
    pStream->LoadFromFile(pcTwsFile);

    m_pSlimQRFrm->ClearAll();
    m_pEditBar->ResetEditLayer();
    m_pDrawLayer.Clear();
    m_LeftMouseDown = false;

    m_pMap->ClearAllData();
    m_pMap.Clear();

    CPersistPtr pPersist;
    CPersist::Instantiate(pStream, pPersist);
    CAST_PTR(pPersist, m_pMap, CMap)

    CDisplayTransformationPtr pTrans = this->GetDT();
    pTrans->SetVisibleExtentHandle(VisibleExtentChangeHandle);

    m_pMainSpace->SetActiveView(this->GetAV());
    m_pSlimQRFrm->SetActiveView(this->GetAV());

    this->MainSpaceResize(NULL);

    return true;
}

bool TFormMain::SaveWorkspace(const char* pcTwsFile)
{
    IStreamX* pStream = new CMemoryStream;
    pStream->_AddRef();
    m_pMap->Dump(pStream);
    bool r = pStream->SaveToFile(pcTwsFile);
    pStream->_Release();
    return r;
}

void __fastcall TFormMain::FormShow(TObject *Sender)
{
    m_pMainSpace = new TPanelEx(MainSpace);
    m_pMainSpace->SetActiveView(this->GetAV());
    m_pMainSpace->Parent = MainSpace;
    m_pMainSpace->Align = alClient;
    m_pMainSpace->Show();

    m_dc = ::GetDC(m_pMainSpace->Handle);
    SetMouseAction(MouseActionDefault);
    m_TWSFileName = "";
    this->ResetViewExtentList();
    m_DoNotPushExt = false;

    m_TreeDrag = false;

    m_pEditBar = new TFormEditBar(this);
    TFrmMainMessager* pEBM = new TFrmMainMessager(this);
    m_pEditBar->SetTBM(pEBM);
    m_pEditBar->Show();

    m_pImportShpFrm = new TFormImportShape(this);
    m_pSlimQRFrm = new TFormSlimQueryResult(this);
    m_pSlimQRFrm->SetActiveView(this->GetAV());
    m_pSlimAttrib = new TFormSlimAttrib(this);
    m_pFormMapAttrib = new TFormMapAttrib(this);
    m_pFormCreateSlim = new TFormCreateSlim(this);

    ::PostMessage(MainSpace->Handle, WM_SIZE, 0, 0);

    this->TreeViewLayersClick(this);
    this->RefreshEditToolButton();

    this->PanelLeftResize(this);

    m_FillColor = RGB(237, 254, 218);
    m_OutlineWidth = 0.3;
    m_OutlineColor = clMoneyGreen;
    m_PathWidth = 1;
    m_PathColor = clTeal;
    m_PointColor = clSkyBlue;
    m_PointDiameter = 3;
    m_FontColor = clBlue;
    m_pMoveTracker = new CMoveTracker;
    m_LeftMouseDown = false;

    m_pFrmPos[0] = new TFormPos(this, "formmain", false, false);
    m_pFrmPos[1] = new TFormPos(m_pEditBar, "editbar", true);
    m_pFrmPos[2] = new TFormPos(m_pSlimQRFrm, "queryresult", false, false);
    m_pFrmPos[3] = new TFormPos(PanelLeft, "panelleft");

    //----------------------------------------------------
    string ininame, initxt, key, value;
    ininame = GetIniFileName();
    if (File2String(ininame, initxt))
    {
        key = "loaddatafiletype";
        if (ini_findkeyvalue(initxt, key, value))
        {
            LoadDataDlg->FilterIndex = StrToInt(value);
        }

        key = "drawelementtext";
        if (ini_findkeyvalue(initxt, key, value))
        {
            EditDrawText->Text = value.c_str();
        }

        key = "drawelementtextsize";
        if (ini_findkeyvalue(initxt, key, value))
        {
            EditDrawTextSize->Text = value.c_str();
        }

        key = "drawelementtextcolor";
        if (ini_findkeyvalue(initxt, key, value))
        {
            m_FontColor = StrToInt(value.c_str());
        }

        key = "drawelementfillcolor";
        if (ini_findkeyvalue(initxt, key, value))
        {
            m_FillColor = StrToInt(value.c_str());
        }

        key = "drawelementoutlinecolor";
        if (ini_findkeyvalue(initxt, key, value))
        {
            m_OutlineColor = StrToInt(value.c_str());
        }

        key = "drawelementoutlinewidth";
        if (ini_findkeyvalue(initxt, key, value))
        {
            m_OutlineWidth = StrToFloat(value.c_str());
        }

        key = "drawelementpathcolor";
        if (ini_findkeyvalue(initxt, key, value))
        {
            m_PathColor = StrToInt(value.c_str());
        }

        key = "drawelementpathwidth";
        if (ini_findkeyvalue(initxt, key, value))
        {
            m_PathWidth = StrToFloat(value.c_str());
        }

        key = "drawelementpointcolor";
        if (ini_findkeyvalue(initxt, key, value))
        {
            m_PointColor = StrToInt(value.c_str());
        }

        key = "drawelementpointsize";
        if (ini_findkeyvalue(initxt, key, value))
        {
            m_PointDiameter = StrToFloat(value.c_str());
        }
    }
    //----------------------------------------------------


    this->NewWorkspace();
    this->RefreshFrmShowButtons();
    this->RefreshEditToolButton();
    this->RefreshElementToolButton();

    m_pDownTB = NULL;
    m_InitOK = true;
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::FormClose(TObject *Sender, TCloseAction &Action)
{
    Action = caFree;

    for (long i = 0; i < 4; i++)
    {
        delete m_pFrmPos[i];
    }

    this->GetAV()->LostFocus();
    ::ReleaseDC(m_pMainSpace->Handle, m_dc);
    delete m_pFormCreateSlim;
    delete m_pFormMapAttrib;
    delete m_pImportShpFrm;
    delete m_pEditBar;
    delete m_pSlimAttrib;
    delete m_pSlimQRFrm;
    delete m_pMainSpace;

    string ininame, initxt;
    ininame = GetIniFileName();
    if (File2String(ininame, initxt))
    {
        string key = "loaddatafiletype";
        string value = easymap::IntToStr(LoadDataDlg->FilterIndex);
        ini_setkeyvalue(initxt, key, value);

        key = "drawelementtext";
        value = EditDrawText->Text.c_str();
        ini_setkeyvalue(initxt, key, value);

        key = "drawelementtextsize";
        value = EditDrawTextSize->Text.c_str();
        ini_setkeyvalue(initxt, key, value);

        key = "drawelementtextcolor";
        value = easymap::IntToStr(m_FontColor);
        ini_setkeyvalue(initxt, key, value);

        key = "drawelementfillcolor";
        value = easymap::IntToStr(m_FillColor);
        ini_setkeyvalue(initxt, key, value);

        key = "drawelementoutlinecolor";
        value = easymap::IntToStr(m_OutlineColor);
        ini_setkeyvalue(initxt, key, value);

        key = "drawelementoutlinewidth";
        value = easymap::FloatToStr(m_OutlineWidth);
        ini_setkeyvalue(initxt, key, value);

        key = "drawelementpathcolor";
        value = easymap::IntToStr(m_PathColor);
        ini_setkeyvalue(initxt, key, value);

        key = "drawelementpathwidth";
        value = easymap::FloatToStr(m_PathWidth);
        ini_setkeyvalue(initxt, key, value);

        key = "drawelementpointcolor";
        value = easymap::IntToStr(m_PointColor);
        ini_setkeyvalue(initxt, key, value);

        key = "drawelementpointsize";
        value = easymap::FloatToStr(m_PointDiameter);
        ini_setkeyvalue(initxt, key, value);

        String2File(initxt, ininame);
    }
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::N4Click(TObject *Sender)
{
    if (!this->PromptSaveEdit()) {return;}

    this->NewWorkspace();
}

void TFormMain::NewWorkspace()
{
    //新建工作空间
    SetMouseAction(MouseActionDefault);

    m_ElementLayerId = 1;
    m_pMap->ClearAllData();
    m_pSlimQRFrm->ClearAll();
    m_pEditBar->ResetEditLayer();
    m_pDrawLayer.Clear();
    m_LeftMouseDown = false;

    m_pTreeRoot->DeleteChildren();
    m_TWSFileName = "";
    m_pMap->SetName("My Map");
    m_pTreeRoot->Text = m_pMap->GetName();
    this->Caption = "EasyMap Desktop - 新建工作空间";
    this->ResetViewExtentList();
    this->TreeViewLayersClick(this);
    this->RefreshEditToolButton();

    this->RefreshLayerTree();

    this->UpdateView();
}

void SetLayerNodeIcon(const ILayerPtr pLayer, TTreeNode* const pNode)
{
    bool visible = pLayer->GetVisible();

    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        if (visible)
        {
            pNode->ImageIndex = 2;
            pNode->SelectedIndex = 3;
        }
        else
        {
            pNode->ImageIndex = 9;
            pNode->SelectedIndex = 10;
        }
        return;
    }

    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        GeometryColumnInfo colinfo;
        pVL->GetGeometryColumnInfo(colinfo);
        if (1 == colinfo.FeatureType)
        {
            if (visible)
            {
                pNode->ImageIndex = 15;
                pNode->SelectedIndex = 15;
            }
            else
            {
                pNode->ImageIndex = 19;
                pNode->SelectedIndex = 19;
            }
        }
        else
        {
            switch(colinfo.ShpType)
            {
                case SHAPETYPE_POINT:
                case SHAPETYPE_MULTIPOINT:
                {
                    if (visible)
                    {
                        pNode->ImageIndex = 12;
                        pNode->SelectedIndex = 12;
                    }
                    else
                    {
                        pNode->ImageIndex = 16;
                        pNode->SelectedIndex = 16;
                    }
                };
                break;

                case SHAPETYPE_POLYLINE:
                {
                    if (visible)
                    {
                        pNode->ImageIndex = 13;
                        pNode->SelectedIndex = 13;
                    }
                    else
                    {
                        pNode->ImageIndex = 17;
                        pNode->SelectedIndex = 17;
                    }
                };
                break;

                case SHAPETYPE_POLYGON:
                {
                    if (visible)
                    {
                        pNode->ImageIndex = 14;
                        pNode->SelectedIndex = 14;
                    }
                    else
                    {
                        pNode->ImageIndex = 18;
                        pNode->SelectedIndex = 18;
                    }
                };
                break;

                default:
                {
                    pNode->ImageIndex = 0;
                    pNode->SelectedIndex = 0;
                };

            }
        }

        return;
    }

    CElementLayerPtr pEL;
    CAST_PTR(pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        if (visible)
        {
            pNode->ImageIndex = 21;
            pNode->SelectedIndex = 21;
        }
        else
        {
            pNode->ImageIndex = 22;
            pNode->SelectedIndex = 22;
        }

        return;
    }

    pNode->ImageIndex = 0;
    pNode->SelectedIndex = 0;
}

void TFormMain::AddGroupSubNodes(const ILayerPtr pLayer, TTreeNode* const pNode)
{
    CGroupLayerPtr pGroupLayer;
    CAST_PTR(pLayer, pGroupLayer, CGroupLayer);
    if (pGroupLayer._p())
    {
        dword lyrcnt = pGroupLayer->GetLayerCount();
        for (dword i = 0; i < lyrcnt; i++)
        {
            ILayerPtr pSubLayer;
            pGroupLayer->GetLayer(pSubLayer, i);
            string layername = pSubLayer->GetName();
            TTreeNode* const pSubNode = TreeViewLayers->Items->AddChild(pNode, layername.c_str());
            SetLayerNodeIcon(pSubLayer, pSubNode);
            this->AddGroupSubNodes(pSubLayer, pSubNode);
        }
    }
}

void TFormMain::SelectFeaturesByEnvelope(const WKSRect& envelope, const bool partialselect,
    const bool append)
{
    ILayerPtr pLayer;
    long a = m_pEditBar->CBSelectTarget->ItemIndex;
    if (0 == a)
    {
        dword count = m_pMap->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            m_pMap->GetLayer(pLayer, i);
            SelectFeatures(pLayer, envelope, partialselect, append);
        }
    }
    else
    {
        this->GetSelectLayer(pLayer);
        if (pLayer.Assigned())
        {
            CVectorLayerPtr pVL;
            CAST_PTR(pLayer, pVL, CVectorLayer)
            if (pVL.Assigned())
            {
                pLayer->Select(envelope, partialselect, append);
            }
        }
    }

    this->RefreshEditToolButton();
}

void TFormMain::DeselectFeaturesByEnvelope(const WKSRect& envelope, const bool partialselect)
{
    ILayerPtr pLayer;
    long a = m_pEditBar->CBSelectTarget->ItemIndex;
    if (0 == a)
    {
        dword count = m_pMap->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            m_pMap->GetLayer(pLayer, i);
            DeselectFeatures(pLayer, envelope, partialselect);
        }
    }
    else
    {
        this->GetSelectLayer(pLayer);
        if (pLayer.Assigned())
        {
            CVectorLayerPtr pVL;
            CAST_PTR(pLayer, pVL, CVectorLayer)
            if (pVL.Assigned())
            {
                pLayer->Deselect(envelope, partialselect);
            }
        }
    }

    this->RefreshEditToolButton();
}

bool IdentifyVectorLayers(const ILayerPtr pLayer, const WKSRect& envelope,
    const bool partialselect, TFormSlimQueryResult* pFrmQR, bool topmost, bool alllayer)
{
    if (!pLayer.Assigned())
    {
        return false;
    }

    CGroupLayerPtr pGroupLayer;
    CAST_PTR(pLayer, pGroupLayer, CGroupLayer)
    if (pGroupLayer._p())
    {
        dword layercount = pGroupLayer->GetLayerCount();
        for (dword i = 0; i < layercount; i++)
        {
            ILayerPtr pLayer;
            pGroupLayer->GetLayer(pLayer, i);
            bool r = IdentifyVectorLayers(pLayer, envelope, partialselect,
                pFrmQR, topmost, alllayer);
            if (topmost && r)
            {
                return true;
            }
        }
    }

    vector<dword> fids;
    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (pVL.Assigned() && (alllayer || pVL->GetSelectable()))
    {
        pVL->Identify(fids, envelope, partialselect);
        if (fids.size() > 0)
        {
            pFrmQR->AddLayer(pVL, fids);
            return true;
        }
    }
    return false;
}

void TFormMain::QueryByEnvelope(const WKSRect& envelope, const bool partialselect)
{
    m_pSlimQRFrm->ClearAll();

    IGeometryPtr pLoc = (IGeometry*)(new CEnvelope(envelope));

    WKSRect env;
    memcpy(&env, &envelope, sizeof(WKSRect));
    CorrectEnvelope(env);

    ILayerPtr pLayer;

    bool topmost = false;
    bool alllayer = false;

    if (1 == m_pSlimQRFrm->CBLayers->ItemIndex)
    {
        //树上的当前图层
        this->GetSelectLayer(pLayer);
        IdentifyVectorLayers(pLayer, env, partialselect, m_pSlimQRFrm, topmost, true);
    }
    else
    {
        //是否只选择最上面的
        if (2 == m_pSlimQRFrm->CBLayers->ItemIndex)
        {
            topmost = true;
        }
        else if (3 == m_pSlimQRFrm->CBLayers->ItemIndex)
        {
            alllayer = true;
        }

        //遍历所有可以选择的
        dword layercount = m_pMap->GetLayerCount();
        for (dword i = 0; i < layercount; i++)
        {
            m_pMap->GetLayer(pLayer, i);
            bool r = IdentifyVectorLayers(pLayer, env, partialselect,
                m_pSlimQRFrm, topmost, alllayer);
            if (topmost && r)
            {
                break;
            }
        }
    }

    m_pSlimQRFrm->ExpandAll();
    m_pSlimQRFrm->SetLocation(pLoc);
    m_pSlimQRFrm->Show();
}

void TFormMain::ClearSelection()
{
    m_pMap->ClearSelection();
    this->RefreshEditToolButton();
}

void TFormMain::RefreshScaleDisplay()
{
    if (!m_InitOK)
    {
        return;
    }
    CDisplayPtr pDisplay;
    this->GetAV()->GetDisplay(pDisplay);
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);
    double mapscale;
    pTrans->GetMapScale(mapscale);
    FormMain->CBScale->Text = string("1:" + FloatToStr(mapscale)).c_str();
}

void TFormMain::PushViewExt(const WKSRect& rect)
{
    if (0 > m_CurrViewExt) return;
    if (m_DoNotPushExt)
    {
        m_DoNotPushExt = false;
        return;
    }

    WKSRect currect;
    currect = m_ViewExts[m_CurrViewExt];
    if ((0.0000001 > fabs(currect.left - rect.left)) &&
        (0.0000001 > fabs(currect.right - rect.right)) &&
        (0.0000001 > fabs(currect.top - rect.top)) &&
        (0.0000001 > fabs(currect.bottom - rect.bottom)))
    {
        return;
    }

    long size = m_ViewExts.size();
    for (long i = size - 1; i > m_CurrViewExt; i--)
    {
        m_ViewExts.pop_back();
    }
    m_ViewExts.push_back(rect);
    m_CurrViewExt++;
}

void TFormMain::RefreshLayerTree()
{
    m_pTreeRoot->DeleteChildren();
    dword layercount = m_pMap->GetLayerCount();
    for (dword i = 0; i < layercount; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer, i);
        CSlimLayerPtr pSL;
        CAST_PTR(pLayer, pSL, CSlimLayer)
        string layername;
        if (pSL.Assigned())
        {
            layername = pSL->GetAlias();
        }
        else
        {
            layername = pLayer->GetName();
        }
        TTreeNode* pNode = TreeViewLayers->Items->AddChild(m_pTreeRoot, layername.c_str());
        SetLayerNodeIcon(pLayer, pNode);
        this->AddGroupSubNodes(pLayer, pNode);
    }
    m_pTreeRoot->Expand(false);
}

//---------------------------------------------------------------------------
void __fastcall TFormMain::N2Click(TObject *Sender)
{
    if (!this->PromptSaveEdit())
    {
        return;
    }

    SetMouseAction(MouseActionDefault);

    OpenWorkSpaceDlg->FileName = m_TWSFileName.c_str();
    //打开工作空间
    if (!OpenWorkSpaceDlg->Execute())
    {
        return;
    }

    m_ElementLayerId = 0;
    this->LoadWorkspace(OpenWorkSpaceDlg->FileName.c_str());
    m_TWSFileName = OpenWorkSpaceDlg->FileName.c_str();
    this->Caption = "EasyMap Desktop - " + AnsiString(m_TWSFileName.c_str());
    m_pTreeRoot->Text = m_pMap->GetName();

    this->RefreshLayerTree();

    this->ResetViewExtentList();

    TTreeNode* pLayerNode1 = m_pTreeRoot->getFirstChild();
    if (pLayerNode1) pLayerNode1->Expand(true);

    this->TreeViewLayersClick(this);
    this->RefreshEditToolButton();
}

void TFormMain::ResetViewExtentList()
{
    m_ViewExts.clear();
    m_ViewExts.push_back();
    CDisplayTransformationPtr pDT = this->GetDT();
    WKSRect extent;
    pDT->GetVisibleExtent(extent);
    m_ViewExts.push_back(extent);
    m_CurrViewExt  = 1;
}

//---------------------------------------------------------------------------
void __fastcall TFormMain::MainSpaceResize(TObject *Sender)
{
    if (!m_InitOK)
    {
        return;
    }

    tagRECT rect;
    ::GetWindowRect(MainSpace->Handle, &rect);
    rect.right = rect.right - rect.left;
    rect.bottom = rect.bottom - rect.top;
    rect.left = 0;
    rect.top = 0;

    this->GetAV()->LostFocus();
    this->GetAV()->GainFocus(m_dc, rect);

    this->UpdateView();

    m_pMoveTracker->Resize();
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::N11Click(TObject *Sender)
{
    //显示全图
    this->ZoomAll();
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::N3Click(TObject *Sender)
{
    if (!this->PromptSaveEdit())
    {
        return;
    }

    //m_TWSFileName.c_str();

    WIN32_FIND_DATA FindFileData;
    HANDLE hFind = ::FindFirstFile(m_TWSFileName.c_str(), &FindFileData);
    if (INVALID_HANDLE_VALUE == hFind)
    {
        this->N9Click(Sender);
        return;
    }
    FindClose(hFind);

    this->SaveWorkspace(m_TWSFileName.c_str());
    this->RefreshEditToolButton();
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::N6Click(TObject *Sender)
{
    //退出
    this->Close();
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::FormCreate(TObject *Sender)
{
    m_InitOK = false;

    IObjPtr pObj;
    _FactoryManager::CreateInstance("CGeoMap", pObj);
    CAST_PTR(pObj, m_pMap, CMap)

    this->GetDT()->SetVisibleExtentHandle(VisibleExtentChangeHandle);

    m_pTreeRoot = TreeViewLayers->Items->AddChild(NULL, "My Map");
    m_pTreeRoot->ImageIndex = 20;
    m_pTreeRoot->SelectedIndex = 20;

    m_ElementLayerId = 0;
}
//---------------------------------------------------------------------------

bool TFormMain::OpenSHP(const CGroupLayerPtr pGroupLayer, const bool readonly)
{
    if (!this->PromptSaveEdit())
    {
        return false;
    }

    string faillist;

    m_pImportShpFrm->ShowModal();
    if (0 == m_pImportShpFrm->Tag)
    {
        m_pImportShpFrm->LBShapeFiles->Items->Clear();
        return false;
    }

    string slimdir;
    if (m_pImportShpFrm->CBSlim->Checked)
    {
        slimdir = m_pImportShpFrm->EditSlimDir->Text.c_str();
    }
    else
    {
        slimdir = "";
    }

    MapUnits mapunit = m_pImportShpFrm->GetMapUnit();
    double mapscale = StrToFloat(m_pImportShpFrm->EditScale->Text);
    double precision = StrToFloat(m_pImportShpFrm->EditPrecision->Text);
    long shpcount = m_pImportShpFrm->LBShapeFiles->Count;
    long annofield = -1;
    if (m_pImportShpFrm->CheckAA->Checked && m_pImportShpFrm->CheckAA->Enabled)
    {
        annofield = m_pImportShpFrm->CBAA->ItemIndex;
    }
    string sr = "";
    if (m_pImportShpFrm->RBEnterSR->Checked)
    {
        sr = m_pImportShpFrm->MemoSR->Text.c_str();
    }

    for (long i = 0; i < shpcount; i++)
    {
        string s = m_pImportShpFrm->LBShapeFiles->Items->Strings[i].c_str();
        long indexlevel = StrToInt(m_pImportShpFrm->CBIndexLevel->Text) + 1;
        bool r;
        if (m_pImportShpFrm->CBSlim->Checked)
        {
            r = this->ImportSHP(s, mapunit, mapscale, precision,
                indexlevel, annofield, pGroupLayer, slimdir, sr);
        }
        else
        {
            r = this->LoadShape(s, mapunit, mapscale, precision,
                indexlevel, readonly, pGroupLayer);
        }

        if (r)
        {
            m_pImportShpFrm->LBShapeFiles->Items->Delete(i--);
            shpcount = m_pImportShpFrm->LBShapeFiles->Count;
        }
        else
        {
            faillist = faillist + s + "\n";
        }
    }

    m_pImportShpFrm->LBShapeFiles->Items->Clear();
    if (faillist != "")
    {
        MessageBox(Handle, string("以下ShapeFile加载失败：\n" + faillist).c_str(), "提示", MB_OK);
    }

    this->TreeViewLayersClick(this);
    return true;
}

ShapeType CBIndexToShapeType(const long cbindex)
{
    switch(cbindex)
    {
        case 0:
            return SHAPETYPE_POINT;
        case 1:
            return SHAPETYPE_MULTIPOINT;
        case 2:
            return SHAPETYPE_POLYLINE;
        case 3:
            return SHAPETYPE_POLYGON;
    }

    return SHAPETYPE_UNKNOWN;
};

bool TFormMain::ImportSHP(const string& shapefile, const MapUnits mapunit,
    const double& basescale, const double& precision, const long indexlevel,
    const long annofield, CGroupLayerPtr pGroupLayer, const string& slimdir,
    const string& sr)
{
    CSlimLayerPtr pSlimLayer;
    bool r = ImportShapeFile(shapefile, mapunit, basescale, precision, indexlevel,
        annofield, slimdir, pSlimLayer);
    if (!r) {return false;}

    if (pGroupLayer._p())
    {
        pGroupLayer->AddLayer(pSlimLayer._p());
    }
    else
    {
        m_pMap->AddLayer(pSlimLayer._p());
    }

    return true;
}

bool TFormMain::LoadShape(const string& shapefile, const MapUnits mapunit,
    const double& basescale, const double& precision, const long indexlevel,
    const bool readonly, CGroupLayerPtr pGroupLayer)
{
    CShapeLayerPtr pShapeLayer = new CShapeLayer(shapefile, mapunit, basescale,
        precision, indexlevel, readonly);
    bool r = pShapeLayer->Valid();
    if (!r) {return false;}

    if (pGroupLayer._p())
    {
        pGroupLayer->AddLayer(pShapeLayer._p());
    }
    else
    {
        m_pMap->AddLayer(pShapeLayer._p());
    }

    return true;
}

bool TFormMain::CreateSilmData(const CGroupLayerPtr pGroupLayer)
{
    m_pFormCreateSlim->ShowModal();
    if (0 == m_pFormCreateSlim->Tag)
    {
        return false;
    }

    MapUnits mapunit = m_pFormCreateSlim->GetMapUnit();
    double basescale = ::StrToFloat(m_pFormCreateSlim->Edit2->Text);
    double precision = ::StrToFloat(m_pFormCreateSlim->Edit3->Text);
    CSlimLayerPtr pNewLayer;
    CCellQuadTree::CQTParams indexparam;

    WKSRect extent;
    extent.left = ::StrToFloat(m_pFormCreateSlim->EditLeft->Text);
    extent.right = ::StrToFloat(m_pFormCreateSlim->EditRight->Text);
    extent.top = ::StrToFloat(m_pFormCreateSlim->EditTop->Text);
    extent.bottom = ::StrToFloat(m_pFormCreateSlim->EditBottom->Text);

    long indexlevel = m_pFormCreateSlim->ComboBox2->ItemIndex;
    ShapeType shapetype = CBIndexToShapeType(m_pFormCreateSlim->CBGeoType->ItemIndex);
    bool anno = false;
    if (SHAPETYPE_UNKNOWN == shapetype)
    {
        shapetype = SHAPETYPE_POINT;
        anno = true;
    }

    CFieldsPtr pFieldsDummy;
    pNewLayer = new CSlimLayer(shapetype, mapunit, basescale, precision,
        extent, indexlevel, pFieldsDummy, anno);
    pNewLayer->SetAlias(m_pFormCreateSlim->EditName->Text.c_str());
    pNewLayer->SetName(pNewLayer->GetAlias().c_str());
    pNewLayer->SetFields(m_pFormCreateSlim->Memo1->Text.c_str());
    pNewLayer->SetDisplayField(StrToInt(m_pFormCreateSlim->Edit5->Text));

    bool mds = m_pFormCreateSlim->CheckMDS->Checked;
    if (!mds)
    {
        string filename = m_pFormCreateSlim->EditFileName->Text.c_str();
        if (!pNewLayer->AttachToFile(filename))
        {
            ::ShowMessage("创建失败，请检查文件路径是否正确，以及文件是否已经被打开。");
            return false;
        }

//        string namegot = RemoveDirectoryPart(pNewLayer->GetFileName());
//        string ext = LowerString(GetExtNamePart(namegot));
//        if (ext == "esd")
//        {
//            namegot = RemoveExtNamePart(namegot);
//        }
//        pNewLayer->SetName(namegot);
//        pNewLayer->SetAlias(namegot);
    }

    ILayerPtr pL;
    CAST_PTR(pNewLayer, pL, ILayer)

    if (!pGroupLayer.Assigned())
    {
        m_pMap->AddLayer(pL);
    }
    else
    {
        pGroupLayer->AddLayer(pL);
    }

    this->RefreshNodeSelectedPath();
    this->RefreshLayerTree();
    TTreeNode* pNode = this->GetLastNodeSelected();
    if (pNode)
    {
        pNode->Expand(false);
    }

    pNode = pNode->getFirstChild();
    if (pNode)
    {
        pNode->Selected = true;
    }

    TreeViewLayers->OnClick(this);

    return true;
}

bool TFormMain::CreateElementLayer(const CGroupLayerPtr pGroupLayer,
    CElementLayerPtr& pElementLayer)
{
    CElementLayerPtr pEL = new CElementLayer;

    bool mustrename = false;
    string layername = "Elements";
    dword i, count = m_pMap->GetLayerCount();
    for (i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer, i);
        if (IsLayerNameExist(pLayer, layername))
        {
            mustrename = true;
            break;
        }
    }

    if (mustrename)
    {
        layername = "Element layer_" + easymap::IntToStr(m_ElementLayerId++);
        mustrename = true;
        while (mustrename)
        {
            for (i = 0; i < count; i++)
            {
                ILayerPtr pLayer;
                m_pMap->GetLayer(pLayer, i);
                if (IsLayerNameExist(pLayer, layername))
                {
                    layername = "Element layer_" + easymap::IntToStr(m_ElementLayerId++);
                    break;
                }
                mustrename = false;
            }
        }
    }

    pEL->SetName(layername.c_str());
    if (pGroupLayer.Assigned())
    {
        pGroupLayer->AddLayer(pEL._p());
    }
    else
    {
        m_pMap->AddLayer(pEL._p());
    }

    this->RefreshNodeSelectedPath();
    this->RefreshLayerTree();
    TTreeNode* pNode = this->GetLastNodeSelected();
    if (pNode)
    {
        pNode->Expand(false);
        pNode = pNode->getFirstChild();
        if (pNode)
        {
            pNode->Selected = true;
        }
    }

    pElementLayer = pEL;

    TreeViewLayers->OnClick(this);
    
    return true;
}

bool TFormMain::LoadData(const CGroupLayerPtr pGroupLayer)
{
    bool zoomall = false;
    if (0 == m_pMap->GetLayerCount())
    {
        zoomall = true;
    }

    if (!LoadDataDlg->Execute())
    {
        return false;
    }

    SetMouseAction(MouseActionDefault);
    
    bool readonly = LoadDataDlg->Options.Contains(ofReadOnly);
    long filecount = LoadDataDlg->Files->Count;
    long i;
    bool r = false;

    if (1 == LoadDataDlg->FilterIndex)
    {
        //加载slimdata
        for (i = 0; i < filecount; i++)
        {
            string filename = LoadDataDlg->Files->Strings[i].c_str();
            r = this->LoadSlimData(pGroupLayer, filename, readonly) ? true : r;
        }
    }
    else if (2 == LoadDataDlg->FilterIndex)
    {
        //加载shpfile
        for (i = 0; i < filecount; i++)
        {
            m_pImportShpFrm->LBShapeFiles->Items->Add(LoadDataDlg->Files->Strings[i]);
        }

        r = this->OpenSHP(pGroupLayer, readonly);
    }

    if (!r)
    {
        return false;
    }

    this->RefreshNodeSelectedPath();
    this->RefreshLayerTree();
    TTreeNode* pNode = this->GetLastNodeSelected();
    if (pNode)
    {
        pNode->Expand(false);
    }

    pNode = pNode->getFirstChild();
    if (pNode)
    {
        pNode->Selected = true;
    }

    if (zoomall)
    {
        this->ViewFullMap();
    }
    else
    {
        this->UpdateView();
    }

    return true;
}

bool TFormMain::LoadSlimData(const CGroupLayerPtr pGroupLayer,
    const string& filename, bool readonly)
{
    CSlimLayerPtr pSL = new CSlimLayer(filename, readonly);
    if (!pSL->Valid())
    {
        ::ShowMessage("无法加载该文件，请确认文件结构是否正确，以及文件是否已经被打开。");
        return false;
    }

    ILayerPtr pL;
    CAST_PTR(pSL, pL, ILayer)
    if (pGroupLayer.Assigned())
    {
        pGroupLayer->AddLayer(pL);
    }
    else
    {
        m_pMap->AddLayer(pL);
    }

    return true;
}

void __fastcall TFormMain::ShapeFile1Click(TObject *Sender)
{
    //加载数据
    CGroupLayerPtr pGroupLayer;
    this->LoadData(pGroupLayer);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::N12Click(TObject *Sender)
{
    //刷新视图
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::NLayersClick(TObject *Sender)
{
    //图层管理
    PanelLeft->Visible = !PanelLeft->Visible;
    this->RefreshFrmShowButtons();
}
//---------------------------------------------------------------------------




void __fastcall TFormMain::PanelLeftResize(TObject *Sender)
{
    PanelCloseLeft->Left = PanelLeft->Width - 22;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::MainSpaceMouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
    if (!m_InitOK) return;

    CActiveViewPtr pAV = this->GetAV();
    CDisplayPtr pDisplay = this->GetDisplay();
    CScreenBrowserPtr pScreenBrowser;
    CAST_PTR(pDisplay, pScreenBrowser, CScreenBrowser);
    CDisplayCachePtr pCache;
    CAST_PTR(pDisplay, pCache, CDisplayCache)
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);
    tagPOINT point_dev;
    point_dev.x = X;
    point_dev.y = Y;
    WKSPoint point_map;
    pTrans->Device2MapXY(X, Y, point_map.x, point_map.y);
    WKSPointZ pointz_map(point_map.x, point_map.y, 0);

    dword i;

    COLORREF bgcolor = pDisplay->GetBackgroundColor();
    HPEN pen;
    HPEN pensaved;

    switch(m_MouseAction)
    {
    case MouseActionPan:
        {
            //pan拖动中...
            pScreenBrowser->PanMoveTo(point_dev);
        }
        break;

    case MouseActionZoomInOut:
    case MouseActionQueryByEnvelope:
        {
            if (m_RightButtonPan)
            {
                //pan拖动中...
                pScreenBrowser->PanMoveTo(point_dev);
            }
            else if (0 < m_TrackPoints.size())
            {
                ::SendMessage(m_pMainSpace->Handle, WM_PAINT, 0, 0);
                ::TrackEnvelope(m_dc, InvertRGB(bgcolor), R2_COPYPEN,
                    1, m_TrackPoints[0], point_dev);
            }
        }
        break;

    case MouseActionAddMultiPoint:
        {
            ::SendMessage(m_pMainSpace->Handle, WM_PAINT, 0, 0);
            pen = ::CreatePen(PS_SOLID, 1, InvertRGB(bgcolor));
            pensaved = (HPEN)::SelectObject(m_dc, pen);
            for (i = 0; i < m_TrackPoints.size(); i++)
            {
                TrackNode(m_dc, RGB(15, 230, 80), m_TrackPoints[i].x, m_TrackPoints[i].y);
            }
            ::SelectObject(m_dc, pensaved);
            ::DeleteObject(pen);
        }
        break;

    case MouseActionAddPolyline:
    case MouseActionAddPolygon:
        if (1 <= m_TrackWKSPoints.size())
        {
            ::SendMessage(m_pMainSpace->Handle, WM_PAINT, 0, 0);
            pen = ::CreatePen(PS_SOLID, 1, RGB(0, 0, 255));
            pensaved = (HPEN)::SelectObject(m_dc, pen);
            i = 0;
            if (2 <= m_TrackWKSPoints.size())
            {

                for (; i < m_TrackWKSPoints.size() - 1; i++)
                {
                    ::MoveToEx(m_dc, m_TrackPoints[i].x, m_TrackPoints[i].y, NULL);
                    ::LineTo(m_dc, m_TrackPoints[i + 1].x, m_TrackPoints[i + 1].y);
                    TrackNode(m_dc, RGB(15, 230, 80), m_TrackPoints[i].x, m_TrackPoints[i].y);
                    TrackNode(m_dc, RGB(15, 230, 80), m_TrackPoints[i + 1].x, m_TrackPoints[i + 1].y);
                }

            }

            double lastindex = m_TrackPoints.size() - 1;
            ::MoveToEx(m_dc, m_TrackPoints[lastindex].x, m_TrackPoints[lastindex].y, NULL);
            ::LineTo(m_dc, X, Y);

            if (MouseActionAddPolygon == m_MouseAction)
            {
                ::MoveToEx(m_dc, m_TrackPoints[0].x, m_TrackPoints[0].y, NULL);
                ::LineTo(m_dc, X, Y);
            }

            TrackNode(m_dc, RGB(15, 230, 80), X, Y);
            TrackNode(m_dc, RGB(15, 230, 80), m_TrackPoints[i].x, m_TrackPoints[i].y);

            ::SelectObject(m_dc, pensaved);
            ::DeleteObject(pen);
        }
        break;

    case MouseActionDefault:
    case MouseActionSelectByEnvelope:
        {
            if (0 < m_TrackPoints.size())
            {
                ::SendMessage(m_pMainSpace->Handle, WM_PAINT, 0, 0);
                ::TrackEnvelope(m_dc, InvertRGB(bgcolor), R2_COPYPEN,
                    1, m_TrackPoints[0], point_dev);
            }
        }
        break;

    case MouseActionMoveObjects:
    case MouseActionMoveFeatures:
        {
            if (m_TrackPoints.size() == 1)
            {
                m_pMoveTracker->MouseMove(X, Y);
            }
        }
        break;

    case MouseActionDrawEnvelope:
        {
            if (m_LeftMouseDown)
            {
                if (0 < m_TrackPoints.size())
                {
                    ::SendMessage(m_pMainSpace->Handle, WM_PAINT, 0, 0);
                    ::DrawEnvelope(m_dc, R2_COPYPEN, m_FillColor, m_OutlineColor,
                        1, m_TrackPoints[0], point_dev);
                }
            }
        }
        break;

    case MouseActionDrawPolygon:
    case MouseActionDrawPolyline:
        {
            if (m_TrackPoints.size() > 0)
            {
                ::SendMessage(m_pMainSpace->Handle, WM_PAINT, 0, 0);
                m_TrackPoints.push_back(point_dev);
                if (MouseActionDrawPolyline == m_MouseAction)
                {
                    DrawPolyline(m_dc, R2_COPYPEN, m_PathColor, 1, m_TrackPoints);
                }
                else
                {
                    DrawPolygon(m_dc, R2_COPYPEN, m_FillColor, m_OutlineColor, 1, m_TrackPoints);
                }
                m_TrackPoints.pop_back();
            }
        }
        break;

    case MouseActionDrawEllipse:
        {
            if (m_LeftMouseDown)
            {
                if (0 < m_TrackPoints.size())
                {
                    ::SendMessage(m_pMainSpace->Handle, WM_PAINT, 0, 0);
                    ::DrawEllipse(m_dc, R2_COPYPEN, m_FillColor, m_OutlineColor,
                        1, m_TrackPoints[0], point_dev);
                }
            }
        }
        break;

    case MouseActionDrawCircle:
        {
            if (m_LeftMouseDown)
            {
                if (0 < m_TrackPoints.size())
                {
                    long a = (m_TrackPoints[0].x - point_dev.x);
                    a = a*a;
                    long b = (m_TrackPoints[0].y - point_dev.y);
                    b = b*b;
                    long radius = ::sqrt(a + b);
                    ::SendMessage(m_pMainSpace->Handle, WM_PAINT, 0, 0);
                    ::DrawCircle(m_dc, R2_COPYPEN, m_FillColor, m_OutlineColor,
                        1, m_TrackPoints[0], radius);
                }
            }
        }
        break;

    case MouseActionDrawFreeLine:
    case MouseActionDrawFreeFace:
        {
            if (m_LeftMouseDown && (0 < m_TrackPoints.size()))
            {
                ::SendMessage(m_pMainSpace->Handle, WM_PAINT, 0, 0);
                m_TrackPoints.push_back(point_dev);
                m_TrackWKSPoints.push_back(pointz_map);
                if (MouseActionDrawFreeFace == m_MouseAction)
                {
                    DrawPolygon(m_dc, R2_COPYPEN, m_FillColor, m_OutlineColor, 1, m_TrackPoints);
                }
                else
                {
                    DrawPolyline(m_dc, R2_COPYPEN, m_PathColor, 1, m_TrackPoints);
                }
            }
        }
        break;

    default:
        {}
    }

    //取出鼠标光标所在的屏幕位置所对应的实际坐标
    double x, y;
    pTrans->Device2MapXY(X, Y, x, y);
    string coordsinfo = "x: " + FloatToStr(x) + "  y: " + FloatToStr(y);
    StatusBar1->Panels->Items[2]->Text = coordsinfo.c_str();
    //取出当前显示比例尺
    double scale;
    pTrans->GetMapScale(scale);
}
//---------------------------------------------------------------------------

void TFormMain::SetUndoPoint()
{
    m_pMap->SetUndoPoint("毛猫");
}

bool TFormMain::EditAddPoint(int X, int Y)
{
    CVectorLayerPtr pVL;
    m_pEditBar->GetEditLayer(pVL);
    if (!pVL.Assigned())
    {
        return false;
    }

    GeometryColumnInfo colinfo;
    pVL->GetGeometryColumnInfo(colinfo);
    if (0 != colinfo.FeatureType)
    {
        return false;
    }

    ShapeType shapetype;
    if (SHAPETYPE_POINT != colinfo.ShpType)
    {
        return false;
    }


    tagPOINT PNT;
    PNT.x = X; PNT.y = Y;
    WKSPoint point;
    CDisplayTransformationPtr pDT = GetDT();
    pDT->Device2Map(PNT, point);
    WKSPointZ pntz;
    pntz.x = point.x;
    pntz.y = point.y;
    pntz.z = 0;
    IGeometryPtr pGeo = (IGeometry*)(new CPoint(pntz));
    pVL->AddFeature(pGeo, "", "");
    this->SetUndoPoint();
    this->RefreshEditToolButton();

    CDisplayPtr pDisp = this->GetDisplay();
    ISymbolPtr pSym;
    pVL->GetDefaultSymbol(pSym);
    pDisp->SetSymbol(pSym);
    pDisp->StartDraw();
    pDisp->DrawGeometry(pGeo);
    pDisp->FinishDraw();
    CActiveViewPtr pAV = this->GetAV();
    pAV->RefreshWindow();

    return true;
}

bool TFormMain::EditAddFeature()
{
    ::SendMessage(m_pMainSpace->Handle, WM_PAINT, 0, 0);

    CVectorLayerPtr pVL;
    m_pEditBar->GetEditLayer(pVL);
    if (!pVL.Assigned())
    {
        m_TrackPoints.clear();
        m_TrackWKSPoints.clear();
        return false;
    }

    GeometryColumnInfo colinfo;
    pVL->GetGeometryColumnInfo(colinfo);
    if (0 != colinfo.FeatureType)
    {
        m_TrackPoints.clear();
        m_TrackWKSPoints.clear();
        return false;
    }

    ShapeType shapetype;
    if (MouseActionAddPolyline == m_MouseAction)
    {
        if (m_TrackWKSPoints.size() < 2)
        {
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
            return false;
        }
        if (SHAPETYPE_POLYLINE != colinfo.ShpType)
        {
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
            return false;
        }
    }
    else if (MouseActionAddPolygon == m_MouseAction)
    {
        if (m_TrackWKSPoints.size() < 3)
        {
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
            return false;
        }
        if (SHAPETYPE_POLYGON != colinfo.ShpType)
        {
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
            return false;
        }
    }
    else if (MouseActionAddMultiPoint == m_MouseAction)
    {
        if (m_TrackWKSPoints.size() < 1)
        {
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
            return false;
        }
        if (SHAPETYPE_MULTIPOINT != colinfo.ShpType)
        {
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
            return false;
        }
    }
    else
    {
        m_TrackPoints.clear();
        m_TrackWKSPoints.clear();
        return false;
    }

    IGeometryPtr pGeo;
    vector<WKSPointZ>::const_iterator it;
    if (MouseActionAddPolyline == m_MouseAction)
    {
        CPathPtr pPath = new CPath;
        it = m_TrackWKSPoints.begin();
        while (it != m_TrackWKSPoints.end())
        {
            pPath->AddPoint(*it);
            it++;
        }
        CPolylinePtr pPolyline = new CPolyline;
        pPolyline->AddPathRef(pPath);
        pGeo = (IGeometry*)pPolyline._p();
    }
    else if (MouseActionAddPolygon == m_MouseAction)
    {
        CRingPtr pRing = new CRing;
        it = m_TrackWKSPoints.begin();
        while (it != m_TrackWKSPoints.end())
        {
            pRing->AddPoint(*it);
            it++;
        }
        pRing->AddPoint(m_TrackWKSPoints[0]);
        CPolygonPtr pPolygon = new CPolygon;
        pPolygon->AddRingRef(pRing);
        pGeo = (IGeometry*)pPolygon._p();
    }
    else if (MouseActionAddMultiPoint == m_MouseAction)
    {
        CMultiPointPtr pMPoint = new CMultiPoint;
        it = m_TrackWKSPoints.begin();
        while (it != m_TrackWKSPoints.end())
        {
            pMPoint->AddPoint(*it);
            it++;
        }
        pGeo = (IGeometry*)pMPoint._p();
    }

    pVL->AddFeature(pGeo, "", "");
    this->SetUndoPoint();
    this->RefreshEditToolButton();

    m_TrackPoints.clear();
    m_TrackWKSPoints.clear();

    CDisplayPtr pDisp = this->GetDisplay();
    ISymbolPtr pSym;
    pVL->GetDefaultSymbol(pSym);
    pDisp->SetSymbol(pSym);
    pDisp->StartDraw();
    pDisp->DrawGeometry(pGeo);
    pDisp->FinishDraw();
    CActiveViewPtr pAV = this->GetAV();
    pAV->RefreshWindow();

    return true;
}

void TFormMain::AddElement(const CElementPtr pElement)
{
    if (!m_pDrawLayer.Assigned() || !pElement.Assigned()) {return;}

    m_pDrawLayer->AddElement((IElement*)pElement._p());
    this->SetUndoPoint();
    this->RefreshEditToolButton();
    this->RefreshElementToolButton();

}

void TFormMain::EditTrackVertex(int X, int Y)
{
    tagPOINT PNT;
    PNT.x = X; PNT.y = Y;
    WKSPoint point;
    CDisplayTransformationPtr pDT = GetDT();
    pDT->Device2Map(PNT, point);
    WKSPointZ pntz;
    pntz.x = point.x;
    pntz.y = point.y;
    pntz.z = 0;
    m_TrackWKSPoints.push_back(pntz);
    m_TrackPoints.push_back(PNT);
}

bool TFormMain::PrepareMoveFeatures(int X, int Y)
{
    if (m_pMoveTracker->Started())
    {
        return false;
    }

    dword count = m_pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer, i);
        this->PrepareMoveFeatures(pLayer);
    }

    if (!m_pMoveTracker->Start(X, Y))
    {
        m_pMoveTracker->ClearGeometry();
        return false;
    }

    return true;
}

void TFormMain::PrepareMoveFeatures(ILayerPtr pLayer)
{
    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            this->PrepareMoveFeatures(pL);
        }
        return;
    }

    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (pVL.Assigned() && !pVL->ReadOnly())
    {
        vector<dword> fids;
        pVL->GetSelection(fids);
        vector<dword>::const_iterator it = fids.begin();
        while (it != fids.end())
        {
            dword fid = *(it++);
            IGeometryPtr pGeo;
            IVectorFeaturePtr pFea;
            pVL->GetFeature(fid, pFea);
            pFea->GetGeometryRef(pGeo._ref());
            m_pMoveTracker->AddGeometryRef((IGeometry*)pGeo._p());
            m_MoveFeatures.push_back(pFea);
        }
    }
}

bool TFormMain::PrepareMoveElements(int X, int Y)
{
    m_MoveElementLayers.clear();

    if (m_pMoveTracker->Started())
    {
        return false;
    }

    dword count = m_pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer, i);
        this->PrepareMoveElements(pLayer);
    }

    if (!m_pMoveTracker->Start(X, Y))
    {
        m_pMoveTracker->ClearGeometry();
        return false;
    }

    CSimplePointSymbolPtr pPointSymbol = new CSimplePointSymbol;
    pPointSymbol->SetColor(RGB(200, 185, 130));
    pPointSymbol->SetDiameter(3.5);
    m_pMoveTracker->SetSymbol(pPointSymbol._p());

    CSimpleLineSymbolPtr pLineSymbol = new CSimpleLineSymbol;
    pLineSymbol->SetColor(RGB(200, 185, 130));
    pLineSymbol->SetWidth(1);
    m_pMoveTracker->SetSymbol(pLineSymbol._p());

    CSimpleFillSymbolPtr pFillSymbol = new CSimpleFillSymbol;
    pFillSymbol->SetColor(RGB(235, 255, 190));
    pFillSymbol->SetFillStyle(BS_SOLID);
    pFillSymbol->SetBorderColor(RGB(199, 215, 158));
    pFillSymbol->SetBorderWidth(0.8);
    m_pMoveTracker->SetSymbol(pFillSymbol._p());
    return true;
}

void TFormMain::PrepareMoveElements(ILayerPtr pLayer)
{
    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            this->PrepareMoveElements(pL);
        }
        return;
    }

    CElementLayerPtr pEL;
    CAST_PTR(pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        vector<dword> ids;
        pEL->GetSelectElements(ids);
        if (ids.size() > 0)
        {
            m_MoveElementLayers.push_back(pEL);
        }

        vector<dword>::const_iterator it = ids.begin();
        while (it != ids.end())
        {
            IGeometryPtr pGeometry;
            IElementPtr pElement;
            pEL->GetElement(*(it++), pElement);
            CTextElementPtr pTextElement;
            CAST_PTR(pElement, pTextElement, CTextElement)
            if (pTextElement.Assigned())
            {
                WKSRect textext;
                pTextElement->GetTextExtent(textext);
                pGeometry = (IGeometry*)(new CEnvelope(textext));
            }
            else
            {
                pElement->GetGeometry(pGeometry._ref());
            }
            m_pMoveTracker->AddGeometryRef((IGeometry*)pGeometry._p());
        }
    }
}

bool TFormMain::EditAddElement()
{
    this->PrepareElementLayer();
    
    WKSRect envelope;
    if ((MouseActionDrawEnvelope == m_MouseAction)
        || (MouseActionDrawEllipse == m_MouseAction))
    {
        envelope.left = m_TrackWKSPoints[0].x;
        envelope.top = m_TrackWKSPoints[0].y;
        envelope.right = m_TrackWKSPoints[1].x;
        envelope.bottom = m_TrackWKSPoints[1].y;
    }

    IFillSymbolPtr pFillSymbol;
    this->GetDrawFillSymbol(pFillSymbol);
    ILineSymbolPtr pLineSymbol;
    this->GetDrawLineSymbol(pLineSymbol);
    IPointSymbolPtr pPointSymbol;
    this->GetDrawPointSymbol(pPointSymbol);
    ITextSymbolPtr pTextSymbol;
    this->GetDrawTextSymbol(pTextSymbol);

    IGeometryPtr pGeo;
    CElementPtr pElement;
    if (MouseActionDrawText == m_MouseAction)
    {
        CTextElementPtr pTextElement = new CTextElement;
        CPointPtr pPoint = new CPoint;
        pPoint->SetX(m_TrackWKSPoints[0].x);
        pPoint->SetY(m_TrackWKSPoints[0].y);
        pGeo = (IGeometry*)(pPoint._p());
        pTextElement->SetGeometry(pGeo);
        pTextElement->SetText(EditDrawText->Text.c_str());

        pTextSymbol->SetColor(m_FontColor);
        pTextSymbol->SetWidth(m_FontSize);
        pTextSymbol->SetHeight(m_FontSize);

        double scale;
        m_pMap->GetMapScale(scale);
        double width, height;
        pTextSymbol->GetWidth(width);
        pTextSymbol->GetHeight(height);
        width = width*scale;
        height = height*scale;
        pTextSymbol->SetWidth(width);
        pTextSymbol->SetHeight(height);
        pTextElement->SetTextSymbol(pTextSymbol._p());
        pTextElement->SetReferenceScale(1);
        pElement = pTextElement._p();
    }
    else
    {
        vector<WKSPointZ>::const_iterator it;
        if (MouseActionDrawPolyline == m_MouseAction)
        {
            if (m_TrackPoints.size() < 2)
            {
                m_TrackPoints.clear();
                m_TrackWKSPoints.clear();
                return false;
            }

            CPathPtr pPath = new CPath;
            it = m_TrackWKSPoints.begin();
            while (it != m_TrackWKSPoints.end())
            {
                pPath->AddPoint(*it);
                it++;
            }
            CPolylinePtr pPolyline = new CPolyline;
            pPolyline->AddPathRef(pPath);
            pGeo = (IGeometry*)(pPolyline._p());
        }
        else if (MouseActionDrawPolygon == m_MouseAction)
        {
            if (m_TrackPoints.size() < 3)
            {
                m_TrackPoints.clear();
                m_TrackWKSPoints.clear();
                return false;
            }

            CRingPtr pRing = new CRing;
            it = m_TrackWKSPoints.begin();
            while (it != m_TrackWKSPoints.end())
            {
                pRing->AddPoint(*it);
                it++;
            }
            pRing->AddPoint(m_TrackWKSPoints[0]);
            CPolygonPtr pPolygon = new CPolygon;
            pPolygon->AddRingRef(pRing);
            pGeo = (IGeometry*)(pPolygon._p());
        }
        else if (MouseActionDrawEnvelope == m_MouseAction)
        {
            CEnvelopePtr pEnvelope = new CEnvelope(envelope);
            pGeo = (IGeometry*)pEnvelope._p();
        }
        else if (MouseActionDrawEllipse == m_MouseAction)
        {
            CEllipsePtr pEllipse = new CEllipse(envelope);
            pGeo = (IGeometry*)pEllipse._p();
        }
        else if (MouseActionDrawCircle == m_MouseAction)
        {
            WKSPointZ center;
            center.x = m_TrackWKSPoints[0].x;
            center.y = m_TrackWKSPoints[0].y;
            center.z = 0;
            double a = (center.x - m_TrackWKSPoints[1].x);
            a = a*a;
            double b = (center.y - m_TrackWKSPoints[1].y);
            b = b*b;
            if ((a + b) <= 0)
            {
                return false;
            }

            double radius = ::sqrt(a + b);
            CCirclePtr pCircle = new CCircle(center, radius);
            pGeo = (IGeometry*)pCircle._p();
        }
        else if (MouseActionDrawPoint == m_MouseAction)
        {
            CPointPtr pPoint = new CPoint(m_TrackWKSPoints[0]);
            pGeo = (IGeometry*)pPoint._p();
        }

        CGeometryElementPtr pGeoElement = new CGeometryElement;
        pGeoElement->SetGeometry(pGeo);
        pGeoElement->SetSymbol(pFillSymbol._p());
        pGeoElement->SetSymbol(pLineSymbol._p());
        pGeoElement->SetSymbol(pPointSymbol._p());
        pElement = pGeoElement._p();
    }

    CDisplayPtr pDisplay = this->GetDisplay();
    CActiveViewPtr pAV = this->GetAV();
    pDisplay->SetSymbol(pPointSymbol._p());
    pDisplay->SetSymbol(pLineSymbol._p());
    pDisplay->SetSymbol(pFillSymbol._p());
    pDisplay->SetSymbol(pTextSymbol._p());

    CDisplayTransformationPtr pDT = this->GetDT();
    double oldrs;
    pDT->GetReferenceScale(oldrs);
    pDT->SetReferenceScale(0);
    CTextElementPtr pTE;
    CAST_PTR(pElement, pTE, CTextElement)
    if (pTE.Assigned())
    {
        pElement->Draw(pDisplay);
    }
    else
    {
        pDisplay->StartDraw();
        pDisplay->DrawGeometry(pGeo);
        pDisplay->FinishDraw();
    }
    pDT->SetReferenceScale(oldrs);

    //注意textelement要画了以后extent才有效
    this->AddElement(pElement._p());
    m_TrackPoints.clear();
    m_TrackWKSPoints.clear();
    pAV->RefreshWindow();

    return true;
}

void __fastcall TFormMain::MainSpaceMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
    if (!m_InitOK) return;
    m_RightButtonPan = false;

    CDisplayPtr pDisplay = this->GetDisplay();
    CScreenBrowserPtr pScreenBrowser;
    CAST_PTR(pDisplay, pScreenBrowser, CScreenBrowser);
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);
    CActiveViewPtr pAV = this->GetAV();

    tagPOINT point_dev;
    point_dev.x = X;
    point_dev.y = Y;
    WKSPoint pnt_map;
    pTrans->Device2Map(point_dev, pnt_map);
    WKSPointZ pointz_map(pnt_map.x, pnt_map.y, 0);

    switch(m_MouseAction)
    {
    case MouseActionPan:
        {
            if (mbLeft == Button)
            {
                //pan拖动起点
                pScreenBrowser->PanStart(point_dev);
            }
        }
        break;

    case MouseActionDefault:
    case MouseActionZoomInOut:
    case MouseActionSelectByEnvelope:
    case MouseActionQueryByEnvelope:
        {
            if ((MouseActionZoomInOut == m_MouseAction)
                && (mbRight == Button))
            {
                //pan拖动起点
                pScreenBrowser->PanStart(point_dev);
                m_RightButtonPan = true;
            }
            else
            {
                if (0 == m_TrackPoints.size())
                {
                    m_TrackPoints.push_back(point_dev);
                }
            }

            if ((MouseActionSelectByEnvelope == m_MouseAction) && (mbRight == Button))
            {
                //不干了
                m_TrackPoints.clear();
            }
        }
        break;

    case MouseActionAddPoint:
        {
            if (mbLeft == Button)
            {
                this->EditAddPoint(X, Y);
            }
        }
        break;

    case MouseActionAddMultiPoint:
        {
            if (mbLeft == Button)
            {
                this->EditTrackVertex(X, Y);
            }
            else
            {
                this->EditAddFeature();
            }
        }
        break;

    case MouseActionAddPolyline:
    case MouseActionAddPolygon:
        {
            if (mbLeft == Button)
            {
                this->EditTrackVertex(X, Y);
            }
            else
            {
                this->EditAddFeature();
            }
        }
        break;

    case MouseActionMoveObjects:
    case MouseActionMoveFeatures:
        {
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
            if (mbLeft == Button)
            {
                this->EditTrackVertex(X, Y);

                m_MoveFeatures.clear();

                //由于会新建工作空间，所以每次都搞一下保险
                m_pMoveTracker->SetDisplay(this->GetDisplay());

                if (MouseActionMoveObjects == m_MouseAction)
                {
                    this->PrepareMoveFeatures(X, Y);
                }
                else if (MouseActionMoveFeatures == m_MouseAction)
                {
                    this->PrepareMoveElements(X, Y);
                }
            }
        }
        break;

    case MouseActionDrawPoint:
        {
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
            this->EditTrackVertex(X, Y);
            this->EditAddElement();
            m_LeftMouseDown = false;
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
        }
        break;

    case MouseActionDrawEnvelope:
    case MouseActionDrawEllipse:
    case MouseActionDrawCircle:
        {
            if (mbLeft == Button)
            {
                m_LeftMouseDown = true;
                m_TrackPoints.clear();
                m_TrackWKSPoints.clear();
                this->EditTrackVertex(X, Y);
            }
        }
        break;

    case MouseActionDrawPolygon:
    case MouseActionDrawPolyline:
        {
            if (mbLeft == Button)
            {
                this->EditTrackVertex(X, Y);
            }
            else
            {
                this->EditAddElement();
            }
        }
        break;

    case MouseActionDrawFreeLine:
    case MouseActionDrawFreeFace:
        {
            if (mbLeft == Button)
            {
                m_LeftMouseDown = true;
                m_TrackPoints.clear();
                m_TrackWKSPoints.clear();
                m_TrackPoints.push_back(point_dev);
                m_TrackWKSPoints.push_back(pointz_map);
            }
        }
        break;

    case MouseActionDrawText:
        {
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
            this->EditTrackVertex(X, Y);
            this->EditAddElement();
            m_LeftMouseDown = false;
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
        }
        break;

    default:
        {}
    }
}
//---------------------------------------------------------------------------

void TFormMain::SelectElements(const WKSRect& envelope, const bool append)
{
    ::SelectElements(m_pMap._p(), envelope, true, append);
    this->RefreshElementToolButton();
    CActiveViewPtr pAV = this->GetAV();
    pAV->UpdateSelection();
    pAV->RefreshWindow();
}

void TFormMain::DeselectElements(const WKSRect* pEnvelope)
{
    if (::DeselectElements(m_pMap._p(), pEnvelope, true))
    {
        this->RefreshElementToolButton();
        CActiveViewPtr pAV = this->GetAV();
        pAV->UpdateSelection();
        pAV->RefreshWindow();
    }
}

dword TFormMain::FinishMoveFeatures()
{
    if (!m_pMoveTracker.Assigned() || !m_pMoveTracker->Started()
        || (m_pMoveTracker->GetGeometryCount() != m_MoveFeatures.size()))
    {
        return 0;
    }

    if (!m_pMoveTracker->Finish())
    {
        return 0;
    }

    list<IVectorFeaturePtr>::const_iterator it = m_MoveFeatures.begin();
    dword count = 0;
    m_pMoveTracker->ResetIterator();
    IGeometryPtr pGeo;
    while (m_pMoveTracker->NextGeometryRef(pGeo))
    {
        IVectorFeaturePtr pFea = *(it++);
//        pFea->SetGeometry(pGeo);
        pFea->Update();

        count++;
    }

    m_pMoveTracker->ClearGeometry();
    m_MoveFeatures.clear();

    this->SetUndoPoint();

    this->RefreshEditToolButton();
    return count;
}

bool TFormMain::FinishMoveElements(const double delta_x, const double delta_y)
{
    if (!m_pMoveTracker.Assigned() || !m_pMoveTracker->Started())
    {
        return false;
    }

    if (!m_pMoveTracker->Finish())
    {
        return false;
    }

    if (m_MoveElementLayers.size() <= 0)
    {
        return false;
    }

    list<CElementLayerPtr>::const_iterator it = m_MoveElementLayers.begin();
    while (it != m_MoveElementLayers.end())
    {
        CElementLayerPtr pElementLayer = *(it++);
        pElementLayer->MoveSelectElements(delta_x, delta_y);
    }

    m_pMoveTracker->ClearGeometry();
    m_MoveElementLayers.clear();

    this->SetUndoPoint();

    this->RefreshEditToolButton();
    this->RefreshElementToolButton();
    return true;
}

void __fastcall TFormMain::MainSpaceMouseUp(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
    if (!m_InitOK) return;

    IFillSymbolPtr pFillSymbol;
    ILineSymbolPtr pLineSymbol;

    CActiveViewPtr pAV = this->GetAV();
    CDisplayPtr pDisplay = this->GetDisplay();
    CScreenBrowserPtr pScreenBrowser;
    CAST_PTR(pDisplay, pScreenBrowser, CScreenBrowser);
    CDisplayTransformationPtr pTrans;
    pDisplay->GetDisplayTransformation(pTrans);

    tagPOINT point_dev;
    point_dev.x = X;
    point_dev.y = Y;
    WKSPoint point_map;
    pTrans->Device2MapXY(X, Y, point_map.x, point_map.y);
    WKSPointZ pointz_map(point_map.x, point_map.y, 0);

    tagRECT rect, rect1;
    WKSRect selectext;
    if (m_TrackPoints.size() > 0)
    {
        rect.left = m_TrackPoints[0].x;
        rect.top = m_TrackPoints[0].y;
        rect.right = X;
        rect.bottom = Y;

        rect1 = rect;
        if ((rect.left == rect.right) && (rect.top == rect.bottom))
        {
            rect1.left = rect.left - 2;
            rect1.top = rect.top - 2;
            rect1.right = rect.right + 2;
            rect1.bottom = rect.bottom + 2;
        }

        pTrans->Device2MapXY(rect1.left, rect1.bottom, selectext.left, selectext.bottom);
        pTrans->Device2MapXY(rect1.right, rect1.top, selectext.right, selectext.top);
    }

    switch(m_MouseAction)
    {
    case MouseActionPan:
        {
            if (mbLeft == Button)
            {
                //结束漫游
                pScreenBrowser->PanMoveTo(point_dev);
                pScreenBrowser->PanStop();
                this->UpdateView();
            }
        }
        break;

    case MouseActionDefault:
    case MouseActionZoomInOut:
    case MouseActionSelectByEnvelope:
    case MouseActionQueryByEnvelope:
        {
            if (m_RightButtonPan)
            {
                //结束漫游
                pScreenBrowser->PanMoveTo(point_dev);
                pScreenBrowser->PanStop();
                this->UpdateView();
                m_RightButtonPan = false;
            }
            else if (m_TrackPoints.size() > 0)
            {
                if (MouseActionZoomInOut == m_MouseAction)
                {
                    if (mbLeft == Button)
                    {
                        if (ValidEnvelope(selectext))
                        {
                            pScreenBrowser->VisibleExtentIn(rect);
                        }
                        else
                        {
                            pScreenBrowser->VisibleExtentOut(rect);
                        }
                        this->UpdateView();
                    }
                }
                else if (MouseActionSelectByEnvelope == m_MouseAction)
                {
                    if (mbLeft == Button)
                    {
                        if (Shift.Contains(ssAlt))
                        {
                            this->DeselectFeaturesByEnvelope(selectext);
                            pAV->UpdateSelection();
                        }
                        else
                        {
                            if (Shift.Contains(ssCtrl))
                            {
                                this->SelectFeaturesByEnvelope(selectext, true, true);
                                this->UpdateSelection(&selectext);
                            }
                            else
                            {
                                this->SelectFeaturesByEnvelope(selectext, true, false);
                                this->UpdateSelection();
                            }
                        }
                    }
                    pAV->RefreshWindow();
                }
                else if (MouseActionQueryByEnvelope == m_MouseAction)
                {
                    if (mbLeft == Button)
                    {
                        this->QueryByEnvelope(selectext);
                        this->GetAV()->RefreshWindow();
                    }
                }
                else if (MouseActionDefault == m_MouseAction)
                {
                    if (mbLeft == Button)
                    {
                        if (Shift.Contains(ssAlt))
                        {
                            this->DeselectElements(&selectext);
                        }
                        else
                        {
                            if (Shift.Contains(ssCtrl))
                            {
                                this->SelectElements(selectext, true);
                            }
                            else
                            {
                                this->SelectElements(selectext, false);
                            }
                        }
                    }
                }
                m_TrackPoints.clear();
            }
        }
        break;

    case MouseActionMoveObjects:
    case MouseActionMoveFeatures:
        {
            if ((mbLeft == Button) && m_pMoveTracker->Started())
            {
                if (MouseActionMoveObjects == m_MouseAction)
                {
                    if (0 < this->FinishMoveFeatures())
                    {
                        this->UpdateView();
                    }
                }
                else if (MouseActionMoveFeatures == m_MouseAction)
                {
                    WKSPointZ moveto = m_TrackWKSPoints[0];
                    if (this->FinishMoveElements(point_map.x - moveto.x,
                        point_map.y - moveto.y))
                    {
                        this->UpdateView();
                    }
                }

                m_pMoveTracker->ClearDisplay();
                m_TrackPoints.clear();
                m_TrackWKSPoints.clear();
                pAV->RefreshWindow();
            }
        }
        break;

    case MouseActionDrawEnvelope:
        {
            if (m_LeftMouseDown && (m_TrackPoints.size() == 1))
            {
                this->EditTrackVertex(X, Y);
                this->EditAddElement();
            }

            m_LeftMouseDown = false;
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
        }
        break;

    case MouseActionDrawEllipse:
        {
            if (m_LeftMouseDown && (m_TrackPoints.size() == 1))
            {
                this->EditTrackVertex(X, Y);
                this->EditAddElement();
            }

            m_LeftMouseDown = false;
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
        }
        break;

    case MouseActionDrawCircle:
        {
            if (m_LeftMouseDown && (m_TrackPoints.size() == 1))
            {
                this->EditTrackVertex(X, Y);
                this->EditAddElement();
            }

            m_LeftMouseDown = false;
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
        }
        break;

    case MouseActionDrawFreeLine:
    case MouseActionDrawFreeFace:
        {
            if (m_LeftMouseDown && (m_TrackPoints.size() > 0))
            {
                m_TrackPoints.push_back(point_dev);
                m_TrackWKSPoints.push_back(pointz_map);
                if (MouseActionDrawFreeFace == m_MouseAction)
                {
                    m_MouseAction = MouseActionDrawPolygon;
                    this->EditAddElement();
                    m_MouseAction = MouseActionDrawFreeFace;
                }
                else
                {
                    m_MouseAction = MouseActionDrawPolyline;
                    this->EditAddElement();
                    m_MouseAction = MouseActionDrawFreeLine;
                }
            }

            m_LeftMouseDown = false;
            m_TrackPoints.clear();
            m_TrackWKSPoints.clear();
        }

    default:
        {}
    }
}
//---------------------------------------------------------------------------

void TFormMain::GetDrawLineSymbol(ILineSymbolPtr& pSymbol)
{
    CSimpleLineSymbolPtr pSimpleLineSym = new CSimpleLineSymbol;
    pSimpleLineSym->SetColor(m_PathColor);
    pSimpleLineSym->SetWidth(m_PathWidth);
    pSymbol = pSimpleLineSym._p();
}

void TFormMain::GetDrawFillSymbol(IFillSymbolPtr& pSymbol)
{
    CSimpleFillSymbolPtr pSimpleFillSym = new CSimpleFillSymbol;
    pSimpleFillSym->SetColor(m_FillColor);
    pSimpleFillSym->SetFillStyle(BS_SOLID);
    pSimpleFillSym->SetBorderColor(m_OutlineColor);
    pSimpleFillSym->SetBorderWidth(m_OutlineWidth);
    pSymbol = pSimpleFillSym._p();
}

void TFormMain::GetDrawPointSymbol(IPointSymbolPtr& pSymbol)
{
    CSimplePointSymbolPtr pSimplePointSym = new CSimplePointSymbol;
    pSimplePointSym->SetColor(m_PointColor);
    pSimplePointSym->SetDiameter(m_PointDiameter);
    pSymbol = pSimplePointSym._p();
}

void TFormMain::GetDrawTextSymbol(ITextSymbolPtr& pSymbol)
{
    CSimpleTextSymbolPtr pTextSymbol = new CSimpleTextSymbol();
    pSymbol = pTextSymbol._p();
}

void __fastcall TFormMain::N8Click(TObject *Sender)
{
    //放缩
    this->ZoomIn();
}
//---------------------------------------------------------------------------


void __fastcall TFormMain::N10Click(TObject *Sender)
{
    this->ZoomPan();
}
//---------------------------------------------------------------------------




void __fastcall TFormMain::About1Click(TObject *Sender)
{
    ::MessageBox(Handle, "超级猪头", ":D", MB_OK);
}
//---------------------------------------------------------------------------




void TFormMain::GetSelectLayer(ILayerPtr& pLayer) const
{
    pLayer = this->GetLayerByNode(TreeViewLayers->Selected);
}
//---------------------------------------------------------------------------

ILayerPtr TFormMain::GetLayerByNode(TTreeNode* pNode) const
{
    ILayerPtr pLayer;
    if ((NULL == pNode) || (0 == pNode->Level))
    {
        return pLayer;
    }

    if (1 == pNode->Level)
    {
        m_pMap->GetLayer(pLayer, pNode->Index);
        return pLayer;
    }
    //??
    vector<long> layerindexs;
    TTreeNode* pNode1 = pNode;
    while (1 <= pNode1->Level)
    {
        layerindexs.push_back(pNode1->Index);
        pNode1 = pNode1->Parent;
    }
    vector<long>::iterator first_it = layerindexs.begin();
    vector<long>::iterator last_it = layerindexs.end();
    std::reverse(first_it, last_it);

    ILayerPtr pTmpLayer;
    m_pMap->GetLayer(pTmpLayer, layerindexs[0]);
    CGroupLayerPtr pGroupLayer;
    CAST_PTR(pTmpLayer, pGroupLayer, CGroupLayer);
    if (!pGroupLayer._p()) return pLayer;
    for (dword i = 1; i < layerindexs.size() - 1; i++)
    {
        pGroupLayer->GetLayer(pTmpLayer, layerindexs[i]);
        CAST_PTR(pTmpLayer, pGroupLayer, CGroupLayer);
        if (!pGroupLayer._p()) return pLayer;
    }
    pGroupLayer->GetLayer(pLayer, layerindexs[layerindexs.size() - 1]);

    return pLayer;
}

void TFormMain::RefreshNodeSelectedPath()
{
    m_NodeSelectedPath.clear();
    TTreeNode* pNode = TreeViewLayers->Selected;

    while (pNode)
    {
        m_NodeSelectedPath.push_back(pNode->Index);
        pNode = pNode->Parent;
    }
}

TTreeNode* TFormMain::GetLastNodeSelected(const bool expand) const
{
    if (expand) m_pTreeRoot->Expand(false);

    long size = m_NodeSelectedPath.size();
    if (1 == size)
    {
        return m_pTreeRoot;
    }

    TTreeNode* pNode = m_pTreeRoot;
    for (long i = size - 2; i >= 0; i--)
    {
        long nodeindex = m_NodeSelectedPath[i];
        if (pNode->Count <= nodeindex)
        {
            return NULL;
        }

        pNode = pNode->operator [](nodeindex);
        if (!pNode)
        {
            return NULL;
        }

        if (expand)
        {
            pNode->Expand(false);
        }
    }

    return pNode;
}

void __fastcall TFormMain::TreeViewLayersDblClick(TObject *Sender)
{
    if (NULL == TreeViewLayers->Selected) return;

    TreeViewLayers->Selected->Expanded = !TreeViewLayers->Selected->Expanded;

    ILayerPtr pLayer;
    this->GetSelectLayer(pLayer);
    if (pLayer._p())
    {
        pLayer->SetVisible(!pLayer->GetVisible());
        SetLayerNodeIcon(pLayer, TreeViewLayers->Selected);
        this->UpdateView();
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::PMTreePopup(TObject *Sender)
{
    MINewSlim->Visible          = false;
    MINewGroupLyr->Visible      = false;
    MINewElements->Visible      = false;
    MILoadShape->Visible        = false;
    MIRemoveLayer->Visible      = false;
    MILayerMapAttr->Visible     = false;
    MIExport->Visible           = false;

    TTreeNode* pNode = TreeViewLayers->Selected;
    if (!pNode) return;

    if (0 == pNode->Level)
    {
        MINewSlim->Visible = true;
        MINewGroupLyr->Visible = true;
        MINewElements->Visible = true;
        MILoadShape->Visible = true;
        MILayerMapAttr->Visible = true;

        return;
    }

    ILayerPtr pLayer;
    this->GetSelectLayer(pLayer);
    if (pLayer.Assigned())
    {
        MIRemoveLayer->Visible = true;
        MILayerMapAttr->Visible = true;

        CGroupLayerPtr pGroupLayer;
        CAST_PTR(pLayer, pGroupLayer, CGroupLayer)
        if (pGroupLayer.Assigned())
        {
            MINewSlim->Visible = true;
            MINewGroupLyr->Visible = true;
            MINewElements->Visible = true;
            MILoadShape->Visible = true;
            MIExport->Visible = true;
        }

        CSlimLayerPtr pSL;
        CAST_PTR(pLayer, pSL, CSlimLayer)
        if (pSL.Assigned())
        {
            MIExport->Visible = true;
        }
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::MILayerMapAttrClick(TObject *Sender)
{
    TTreeNode* pNode = TreeViewLayers->Selected;
    if (!pNode) return;

    if (0 == pNode->Level)
    {
        m_pFormMapAttrib->Edit1->Text = m_pMap->GetName();
        double refscale;
        m_pMap->GetReferenceScale(refscale);
        m_pFormMapAttrib->Edit2->Text = ::FloatToStr(refscale).c_str();
        m_pFormMapAttrib->Memo1->Text = "猪头";
        m_pFormMapAttrib->ShowModal();
        if (1 == m_pFormMapAttrib->Tag)
        {
            m_pMap->SetName(m_pFormMapAttrib->Edit1->Text.c_str());
            m_pTreeRoot->Text = m_pMap->GetName();
            refscale = ::StrToFloat(m_pFormMapAttrib->Edit2->Text);
            CDisplayPtr pDisplay;
            this->GetAV()->GetDisplay(pDisplay);
            CDisplayTransformationPtr pTrans;
            pDisplay->GetDisplayTransformation(pTrans);
            pTrans->SetReferenceScale(refscale);
            this->UpdateView();
        }
        return;
    }

    ILayerPtr pLayer;
    this->GetSelectLayer(pLayer);
    if (!pLayer.Assigned()) return;

    CGroupLayerPtr pGroupLayer;
    CAST_PTR(pLayer, pGroupLayer, CGroupLayer)
    if (pGroupLayer.Assigned())
    {
        ::MessageBox(Handle, "猪头", ":D", MB_OK);
    }

    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (pVL.Assigned())
    {
        m_pSlimAttrib->SetVectorLayer(m_pMap, pVL);
        m_pSlimAttrib->ShowModal();
        if (1 == m_pSlimAttrib->Tag)
        {
            this->RefreshNodeSelectedPath();
            this->RefreshLayerTree();
            TTreeNode* pSelect = this->GetLastNodeSelected();
            if (pSelect)
            {
                pSelect->Selected = true;
            }
            this->UpdateView();
        }
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::TreeViewLayersMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
    TTreeNode* pNode = TreeViewLayers->GetNodeAt(X, Y);
    if (!pNode) return;

    if (mbRight == Button)
    {
        pNode->Selected = true;
        tagPOINT pnt;
        ::GetCursorPos(&pnt);
        PMTree->Popup(pnt.x, pnt.y);
    }
    else
    {
        if (0 < TreeViewLayers->Selected->Level)
        {
            m_TreeDrag = true;
        }
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::MIRemoveLayerClick(TObject *Sender)
{
    TTreeNode* pNode = TreeViewLayers->Selected;
    if (!pNode || (0 == pNode->Level)) return;

    ILayerPtr pLayer;
    this->GetSelectLayer(pLayer);
    if (!pLayer.Assigned()) return;

    IEditLayerPtr pEditLayer;
    CAST_PTR(pLayer, pEditLayer, IEditLayer)
    if (pEditLayer._p())
    {
        bool dirty = pEditLayer->IsDirty();
        if (dirty)
        {
            int dlgr = ::MessageDlg("该图层正在编辑中且尚未保存，保存吗？", mtConfirmation,
                TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0);
            if (mrYes == dlgr)
            {
                pEditLayer->SaveData();
            }
            else if (mrNo == dlgr)
            {
                pEditLayer->EditCancel();
            }
            else
            {
                return;
            }
        }
    }

    m_pDrawLayer.Clear();
    m_pMap->DeleteLayerEx(pLayer);

    TTreeNode* pNode1 = TreeViewLayers->Selected->GetPrev();
    if (!pNode1) pNode1 = TreeViewLayers->Selected->Parent;
    pNode1->Selected = true;
    this->RefreshNodeSelectedPath();
    this->RefreshLayerTree();
    pNode1 = this->GetLastNodeSelected();
    if (pNode1) pNode1->Selected = true;
    this->RefreshEditToolButton();
    this->UpdateView();

    SetMouseAction(MouseActionDefault);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::MINewGroupLyrClick(TObject *Sender)
{
    TTreeNode* pNode = TreeViewLayers->Selected;
    if (!pNode) return;

    ILayerPtr pLayer;
    CGroupLayerPtr pGroupLayer, pNewLayer;
    if (0 == pNode->Level)
    {
        pNewLayer = new CGroupLayer;
        CAST_PTR(pNewLayer, pLayer, ILayer)
        m_pMap->AddLayer(pLayer);
    }
    else
    {
        this->GetSelectLayer(pLayer);
        if (!pLayer.Assigned()) return;
        CAST_PTR(pLayer, pGroupLayer, CGroupLayer)
        if (!pGroupLayer.Assigned()) return;
        pNewLayer = new CGroupLayer;
        CAST_PTR(pNewLayer, pLayer, ILayer)
        pGroupLayer->AddLayer(pLayer);
    }

    pLayer->SetName("Layers");

    this->RefreshNodeSelectedPath();
    this->RefreshLayerTree();
    pNode = this->GetLastNodeSelected();
    if (pNode)
    {
        pNode->Expand(false);
    }

    pNode = pNode->getFirstChild();
    if (pNode)
    {
        pNode->Selected = true;
    }

    TreeViewLayers->OnClick(this);
    
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::TreeViewLayersDragOver(TObject *Sender,
      TObject *Source, int X, int Y, TDragState State, bool &Accept)
{
    Accept = false;

    if (!Source->ClassNameIs("TTreeView")) return;
    TTreeNode* pCurrent = TreeViewLayers->GetNodeAt(X, Y);
    if (!pCurrent || (0 == pCurrent->Level)) return;
    TTreeNode* pSelected = TreeViewLayers->Selected;
    if (!pSelected || (pCurrent == pSelected)
        || (pCurrent->Parent != pSelected->Parent)) return;

    ILayerPtr pSourceLayer;
    this->GetSelectLayer(pSourceLayer);
    if (!pSourceLayer.Assigned()) return;
    ILayerPtr pCurrentLayer = GetLayerByNode(pCurrent);
    if (!pCurrentLayer.Assigned()) return;

    Accept = true;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::TreeViewLayersEditing(TObject *Sender,
      TTreeNode *Node, bool &AllowEdit)
{
    AllowEdit = false;
    ILayerPtr pLayer = GetLayerByNode(Node);
    if (!pLayer.Assigned() && (0 != Node->Level))
    {
        return;
    }

    AllowEdit = true;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::TreeViewLayersEdited(TObject *Sender,
      TTreeNode *Node, AnsiString &S)
{
    ILayerPtr pLayer = GetLayerByNode(Node);
    if (pLayer.Assigned())
    {
        CSlimLayerPtr pSL;
        CAST_PTR(pLayer, pSL, CSlimLayer)
        if (pSL.Assigned())
        {
            if (!pSL->ReadOnly())
            {
                pSL->SetAlias(S.c_str());
                pLayer->SetName(S.c_str());
            }
            else
            {
                string alias = pSL->GetAlias();
                S = alias.c_str();
            }
        }
        else
        {
            pLayer->SetName(S.c_str());
        }
    }
    else if (0 == Node->Level)
    {
        m_pMap->SetName(S.c_str());
    }
    else
    {
    }
}
//---------------------------------------------------------------------------


void __fastcall TFormMain::TreeViewLayersMouseUp(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
    m_TreeDrag = false;    
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::TreeViewLayersMouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
    if (m_TreeDrag)
    {
        TreeViewLayers->BeginDrag(false);
        m_TreeDrag = false;
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::TreeViewLayersDragDrop(TObject *Sender,
      TObject *Source, int X, int Y)
{
    if (!Source->ClassNameIs("TTreeView")) return;
    TTreeNode* pCurrent = TreeViewLayers->GetNodeAt(X, Y);
    if (!pCurrent || (0 == pCurrent->Level)) return;
    TTreeNode* pSelected = TreeViewLayers->Selected;
    if (!pSelected || (pCurrent == pSelected)
        || (pCurrent->Parent != pSelected->Parent)) return;

    ILayerPtr pSourceLayer;
    this->GetSelectLayer(pSourceLayer);
    if (!pSourceLayer.Assigned()) return;
    ILayerPtr pCurrentLayer = GetLayerByNode(pCurrent);
    if (!pCurrentLayer.Assigned()) return;

    TTreeNode* pParentNode = pSelected->Parent;

    if (1 == pSelected->Level)
    {
        m_pMap->SetLayerOrder(pSourceLayer, pCurrent->Index);
    }
    else
    {
        ILayerPtr pParentLayer = GetLayerByNode(pSelected->Parent);
        CGroupLayerPtr pGroupLayer;
        CAST_PTR(pParentLayer, pGroupLayer, CGroupLayer)
        pGroupLayer->SetLayerOrder(pSourceLayer, pCurrent->Index);
    }

    pParentNode->Selected = true;
    this->RefreshNodeSelectedPath();
    this->RefreshLayerTree();
    pParentNode = GetLastNodeSelected();
    pParentNode->Expand(false);
    this->UpdateView();

    this->TreeViewLayersClick(this);
}
//---------------------------------------------------------------------------


void __fastcall TFormMain::N9Click(TObject *Sender)
{
    if (!this->PromptSaveEdit())
    {
        return;
    }

    SaveWorkSpaceDlg->FileName = m_TWSFileName.c_str();

    //保存工作空间
    if (!SaveWorkSpaceDlg->Execute())
    {
        return;
    }

    m_TWSFileName = SaveWorkSpaceDlg->FileName.c_str();

    const char* pc = SaveWorkSpaceDlg->FileName.c_str();
    string filename = LowerString(GetExtNamePart(pc));
    if (filename == ".ews")
    {
        filename = pc;
    }
    else
    {
        filename = RemoveExtNamePart(pc);
        filename = filename + ".ews";
    }
    this->SaveWorkspace(filename.c_str());
    m_TWSFileName = filename;
    this->Caption = "EasyMap Desktop - " + AnsiString(filename.c_str());
}
//---------------------------------------------------------------------------



void __fastcall TFormMain::N24Click(TObject *Sender)
{
    if (1 >= m_CurrViewExt)
    {
        return;
    }
    m_DoNotPushExt = true;
    this->GetDT()->SetVisibleExtent(m_ViewExts[--m_CurrViewExt]);
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::N25Click(TObject *Sender)
{
    if (long(m_ViewExts.size() - 1) == m_CurrViewExt)
    {
        return;
    }
    
    m_DoNotPushExt = true;
    this->GetDT()->SetVisibleExtent(m_ViewExts[++m_CurrViewExt]);
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::MILoadShapeClick(TObject *Sender)
{
    TTreeNode* pNode = TreeViewLayers->Selected;
    if (!pNode) return;

    ILayerPtr pLayer;
    CGroupLayerPtr pGroupLayer, pNewLayer;
    if (0 < pNode->Level)
    {
        this->GetSelectLayer(pLayer);
        if (!pLayer.Assigned())
        {
            return;
        }
        CAST_PTR(pLayer, pGroupLayer, CGroupLayer)
        if (!pGroupLayer.Assigned())
        {
            return;
        }
    }

    this->LoadData(pGroupLayer);

    this->RefreshNodeSelectedPath();
    this->RefreshLayerTree();
    pNode = this->GetLastNodeSelected();
    if (pNode)
    {
        pNode->Expand(false);
    }

    pNode = pNode->getFirstChild();
    if (pNode)
    {
        pNode->Selected = true;
    }
    
    this->RefreshEditToolButton();
    this->UpdateView();
}
//---------------------------------------------------------------------------


void __fastcall TFormMain::MINewSlimClick(TObject *Sender)
{
    TTreeNode* pNode = TreeViewLayers->Selected;
    if (!pNode) return;

    CGroupLayerPtr pGroupLayer;

    if (0 < pNode->Level)
    {
        ILayerPtr pLayer;
        this->GetSelectLayer(pLayer);
        if (!pLayer.Assigned()) return;
        CAST_PTR(pLayer, pGroupLayer, CGroupLayer)
        if (!pGroupLayer.Assigned()) return;
    }

    this->CreateSilmData(pGroupLayer);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::NEditToolsClick(TObject *Sender)
{
    if (!m_pEditBar->Showing)
    {
        m_pEditBar->Show();
    }
    else
    {
        m_pEditBar->Hide();
    }
}
//---------------------------------------------------------------------------

void TFormMain::CancelEditTask()
{
    m_TrackPoints.clear();
    m_TrackWKSPoints.clear();
    m_MouseAction = MouseActionDefault;

    bool refeditlyr = false;

    CVectorLayerPtr pVL;
    ILayerPtr pL1, pL2;
    m_pEditBar->GetEditLayer(pVL);
    this->GetSelectLayer(pL2);
    if (!pVL.Assigned())
    {
        refeditlyr = true;
    }
    else
    {
        CAST_PTR(pVL, pL1, ILayer)
        if (!pL1.Assigned())
        {
            refeditlyr = true;
        }
        else if (!pL1.Compare(pL2._p()))
        {
            refeditlyr = true;
        }
    }

    if (refeditlyr)
    {
        m_pEditBar->SetEditLayer(pL2);
    }
}

TFrmMainMessager::TFrmMainMessager(TFormMain* const pFormMain)
{
    m_pFormMain = pFormMain;
}

long TFrmMainMessager::Dispatch(const string& msg)
{
    if (0 == msg.compare("cancel edit task"))
    {
        //
        m_pFormMain->CancelEditTask();
        return 1;
    }

    if (0 == msg.compare("select objects"))
    {
        //选择对象
        m_pFormMain->NSelectClick(m_pFormMain);
        return 1;
    }

    if (0 == msg.compare("clear selection"))
    {
        //清除选择集
        m_pFormMain->NClearSelectClick(m_pFormMain);
        return 1;
    }

    if (0 == msg.compare("delete objects"))
    {
        //删除所选择的要素
        m_pFormMain->DeleteFeatures();
        return 1;
    }

    if (0 == msg.compare("add point"))
    {
        //添加点
        m_pFormMain->AddPoint();
        return 1;
    }

    if (0 == msg.compare("add polyline"))
    {
        //添加线
        m_pFormMain->AddPolyline();
        return 1;
    }

    if (0 == msg.compare("add polygon"))
    {
        //添加多边形
        m_pFormMain->AddPolygon();
        return 1;
    }

    if (0 == msg.compare("add annotation"))
    {
        //添加annotation
        m_pFormMain->AddAnno();
        return 1;
    }

    if (0 == msg.compare("move objects"))
    {
        //移动
        m_pFormMain->MoveFeatures();
        return 1;
    }

    if (0 == msg.compare("save edit"))
    {
        //保存编辑
        m_pFormMain->NSaveEditClick(m_pFormMain);
        return 1;
    }

    if (0 == msg.compare("cancel edit"))
    {
        //放弃编辑
        m_pFormMain->NCancelEditClick(m_pFormMain);
        return 1;
    }

    if (0 == msg.compare("edit undo"))
    {
        //undo
        m_pFormMain->Undo1Click(m_pFormMain);
        return 1;
    }

    if (0 == msg.compare("edit redo"))
    {
        //redo
        m_pFormMain->Redo1Click(m_pFormMain);
        return 1;
    }

    if (0 == msg.compare("editbar show/hide"))
    {
        //开关窗
        m_pFormMain->RefreshFrmShowButtons();
    }

    return 0;
}

void TFormMain::RefreshFrmShowButtons()
{
    NEditTools->Checked = m_pEditBar->Showing;
    TBEditTools->Down = m_pEditBar->Showing;

    NLayers->Checked = PanelLeft->Visible;
    TBLayers->Down = PanelLeft->Visible;
}


void TFormMain::DeleteFeatures()
{
    dword result = 0;

    //删除
    dword count = m_pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer, i);
        result += DeleteSelectedFeatures(pLayer);
    }

    this->SetUndoPoint();
    this->RefreshEditToolButton();
    if (0 < result)
    {
        this->UpdateView();
    }
}

void TFormMain::AddPoint()
{
    //添加点对象

    ILayerPtr pLayer;
    this->GetSelectLayer(pLayer);
    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
    {
        return;
    }

    GeometryColumnInfo colinfo;
    pVL->GetGeometryColumnInfo(colinfo);
    if (0 != colinfo.FeatureType)
    {
        return;
    }

    ShapeType shapetype;
    if (SHAPETYPE_POINT == colinfo.ShpType)
    {
        SetMouseAction(MouseActionAddPoint);
    }
    else if (SHAPETYPE_MULTIPOINT == colinfo.ShpType)
    {
        SetMouseAction(MouseActionAddMultiPoint);
    }
    else
    {
        return;
    }

    //记下当前编辑图层
    m_pEditBar->SetEditLayer(pLayer);
}
//---------------------------------------------------------------------------

void TFormMain::AddPolyline()
{
    //添加折线对象

    ILayerPtr pLayer;
    this->GetSelectLayer(pLayer);
    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
    {
        return;
    }

    GeometryColumnInfo colinfo;
    pVL->GetGeometryColumnInfo(colinfo);
    if (0 != colinfo.FeatureType)
    {
        return;
    }

    ShapeType shapetype;
    if (SHAPETYPE_POLYLINE == colinfo.ShpType)
    {
        SetMouseAction(MouseActionAddPolyline);
    }
    else
    {
        return;
    }

    //记下当前编辑图层
    m_pEditBar->SetEditLayer(pLayer);
}
//---------------------------------------------------------------------------

void TFormMain::SetTBDown(TToolButton* const pTB, const bool down)
{
    if (_valid(pTB))
    {
        pTB->Down = false;
    }
    
    m_pDownTB = NULL;
    if (down)
    {
        m_pDownTB = pTB;
        TimerDrawTBDown->Enabled = true;
    }
}

void TFormMain::SetMouseAction(const MouseAction mouseaction)
{
    if (!m_InitOK)
    {
        return;
    }

    this->CancelEditTask();
    m_pEditBar->ResetEditLayer();
    this->TreeViewLayersClick(this);
    m_MouseAction = mouseaction;
    m_LeftMouseDown = false;

    this->SetTBDown(TBDrawElement, false);
    this->SetTBDown(TBDrawText, false);
    m_pEditBar->ResetTB();
    switch (m_MouseAction)
    {
    case MouseActionDrawEnvelope:
    case MouseActionDrawCircle:
    case MouseActionDrawEllipse:
    case MouseActionDrawPolygon:
    case MouseActionDrawPolyline:
    case MouseActionDrawPoint:
    case MouseActionDrawFreeLine:
    case MouseActionDrawFreeFace:
        this->SetTBDown(TBDrawElement);
        break;

    case MouseActionDrawText:
        this->SetTBDown(TBDrawText);
        break;

    default:
        {
        }
    }
}

void TFormMain::AddPolygon()
{
    //添加多边形对象

    ILayerPtr pLayer;
    this->GetSelectLayer(pLayer);
    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    if (!pVL.Assigned())
    {
        return;
    }

    GeometryColumnInfo colinfo;
    pVL->GetGeometryColumnInfo(colinfo);
    if (0 != colinfo.FeatureType)
    {
        return;
    }

    ShapeType shapetype;
    if (SHAPETYPE_POLYGON == colinfo.ShpType)
    {
        SetMouseAction(MouseActionAddPolygon);
    }
    else
    {
        return;
    }

    //记下当前编辑图层
    m_pEditBar->SetEditLayer(pLayer);
}
//---------------------------------------------------------------------------


void TFormMain::MoveFeatures()
{
    //移动对象
    this->SetMouseAction(MouseActionMoveObjects);
}

void __fastcall TFormMain::NSaveEditClick(TObject *Sender)
{
    //保存编辑

/*
    dword count = m_pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer, i);
        LayerDataUpdate(pLayer, true);
    }
*/
    m_pMap->SaveData();

    this->RefreshEditToolButton();
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::NCancelEditClick(TObject *Sender)
{
    //取消编辑

/*
    dword count = m_pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer, i);
        LayerDataUpdate(pLayer, false);
    }
*/
    m_pMap->EditCancel();

    this->RefreshEditToolButton();
    this->RefreshElementToolButton();
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::Undo1Click(TObject *Sender)
{
    m_pMap->EditUndo();

    this->RefreshEditToolButton();
    this->RefreshElementToolButton();
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::Redo1Click(TObject *Sender)
{
    m_pMap->EditRedo();

    this->RefreshEditToolButton();
    this->RefreshElementToolButton();
    this->UpdateView();
}
//---------------------------------------------------------------------------

void TFormMain::RefreshEditToolButton()
{
    TBUndo->Enabled = false;
    TBRedo->Enabled = false;
    m_pEditBar->TBSaveEdit->Enabled = false;
    m_pEditBar->TBCancelEdit->Enabled = false;
    Undo1->Enabled = false;
    Redo1->Enabled = false;
    NSaveEdit->Enabled = false;
    NCancelEdit->Enabled = false;
    NClearSelect->Enabled = false;
    m_pEditBar->TBClearSelect->Enabled = false;
    m_pEditBar->TBDelete->Enabled = false;
    m_pEditBar->TBMove->Enabled = false;

    if (this->EditStatus(EDITSTATUS_DIRTY))
    {
        m_pEditBar->TBSaveEdit->Enabled = true;
        m_pEditBar->TBCancelEdit->Enabled = true;
        NSaveEdit->Enabled = true;
        NCancelEdit->Enabled = true;
    }

    if (this->EditStatus(EDITSTATUS_UNDOABLE))
    {
        TBUndo->Enabled = true;
        Undo1->Enabled = true;
    }

    if (this->EditStatus(EDITSTATUS_REDOABLE))
    {
        TBRedo->Enabled = true;
        Redo1->Enabled = true;
    }

    dword count = m_pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer, i);
        if (IsFeatureSelected(pLayer))
        {
            NClearSelect->Enabled = true;
            m_pEditBar->TBClearSelect->Enabled = true;
            
            if (CanMoveAndDeleteFeatures(pLayer))
            {
                m_pEditBar->TBDelete->Enabled = true;
                m_pEditBar->TBMove->Enabled = true;
                break;
            }
        }
    }

    if (!m_pEditBar->TBMove->Enabled && (MouseActionMoveObjects == m_MouseAction))
    {
        m_MouseAction = MouseActionDefault; 
    }
}

void TFormMain::RefreshElementToolButton()
{
    bool r = IsElementSelected(m_pMap._p());
    TBClearElementsSelection->Enabled = r;
    TBDeleteElements->Enabled = r;
    TBMoveElements->Enabled = r;
}

void __fastcall TFormMain::TreeViewLayersClick(TObject *Sender)
{
    ILayerPtr pLayer;
    this->GetSelectLayer(pLayer);

    if (!m_pEditBar->Adding())
    {
        m_pEditBar->SetEditLayer(pLayer);
    }

    if (!pLayer.Assigned())
    {
        return;
    }

    CElementLayerPtr pElementLayer;
    CAST_PTR(pLayer, pElementLayer, CElementLayer)
    if (pElementLayer.Assigned())
    {
        m_pDrawLayer = pElementLayer;
    }
}
//---------------------------------------------------------------------------



void TFormMain::AddAnno()
{
    //
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::FormCloseQuery(TObject *Sender, bool &CanClose)
{
    if (this->EditStatus(EDITSTATUS_DIRTY))
    {
        int dlgr = MessageDlg("当前工作空间中编辑过的数据尚未保存，保存吗？", mtConfirmation,
            TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0);
        if (mrYes == dlgr)
        {
            this->NSaveEditClick(this);
        }
        else if (mrNo == dlgr)
        {
            this->NCancelEditClick(this);
        }
        else
        {
            CanClose = false;
            return;
        }
    }

    CanClose = true;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::CreateSlim1Click(TObject *Sender)
{
    CGroupLayerPtr pGroupLayer;
    this->CreateSilmData(pGroupLayer);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::MIExportClick(TObject *Sender)
{
    //图层节点数据导出

    TTreeNode* pNode = TreeViewLayers->Selected;
    if (!pNode) return;

    if (0 == pNode->Level)
    {
        return;
    }

    ILayerPtr pLayer;
    this->GetSelectLayer(pLayer);
    if (!pLayer.Assigned()) return;

    CGroupLayerPtr pGroupLayer;
    CAST_PTR(pLayer, pGroupLayer, CGroupLayer)
    if (pGroupLayer.Assigned())
    {
        ::MessageBox(Handle, "猪头", ":D", MB_OK);
    }

    CSlimLayerPtr pSL;
    CAST_PTR(pLayer, pSL, CSlimLayer)
    if (pSL.Assigned())
    {
        string alias = pSL->GetAlias();
        ExportDialog->FileName = alias.c_str();
        bool r = false;
        if (!ExportDialog->Execute())
        {
            return;
        }
        string filename = ExportDialog->FileName.c_str();
        switch (ExportDialog->FilterIndex)
        {
        case 1: //shape
            r = ExportShapeFile(pSL._p(), filename);
            break;

        default:
            {
                ::ShowMessage("怪了还～");
                return;
            }
        }
        if (!r)
        {
            ::ShowMessage("数据转出失败。");
        }
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::CBScaleSelect(TObject *Sender)
{
    CDisplayTransformationPtr pDT = this->GetDT();
    double scale;
    switch (CBScale->ItemIndex)
    {
    case 0:
        scale = 1000;
        break;
    case 1:
        scale = 2000;
        break;
    case 2:
        scale = 10000;
        break;
    case 3:
        scale = 50000;
        break;
    case 4:
        scale = 250000;
        break;
    case 5:
        scale = 500000;
        break;
    case 6:
        scale = 1000000;
        break;
    case 7:
        scale = 4000000;
        break;
    case 8:
        scale = 8000000;
        break;
    case 9:
        scale = 10000000;
        break;
    default:
        return;
    }

    pDT->SetMapScale(scale);
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::NZoomInClick(TObject *Sender)
{
    double scale;
    CDisplayTransformationPtr pDT = this->GetDT();
    pDT->GetMapScale(scale);
    pDT->SetMapScale(scale / 2);
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::NZoomOutClick(TObject *Sender)
{
    double scale;
    CDisplayTransformationPtr pDT = this->GetDT();
    pDT->GetMapScale(scale);
    pDT->SetMapScale(scale * 2);
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::NSelectClick(TObject *Sender)
{
    //选择对象
    this->SelectFeaturesByEnvelope();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::NClearSelectClick(TObject *Sender)
{
    this->ClearSelection();
    this->RefreshEditToolButton();
    this->UpdateView();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::NIdentifyClick(TObject *Sender)
{
    SetMouseAction(MouseActionQueryByEnvelope);
//    m_pSlimQRFrm->Show();    
}
//---------------------------------------------------------------------------


void __fastcall TFormMain::PanelCloseLeftClick(TObject *Sender)
{
    PanelLeft->Visible = false;
    this->RefreshFrmShowButtons();
}
//---------------------------------------------------------------------------











void __fastcall TFormMain::MINewElementsClick(TObject *Sender)
{
    TTreeNode* pNode = TreeViewLayers->Selected;
    if (!pNode) return;

    CGroupLayerPtr pGroupLayer;

    if (0 < pNode->Level)
    {
        ILayerPtr pLayer;
        this->GetSelectLayer(pLayer);
        if (!pLayer.Assigned()) return;
        CAST_PTR(pLayer, pGroupLayer, CGroupLayer)
        if (!pGroupLayer.Assigned()) return;
    }

    CElementLayerPtr pEL;
    this->CreateElementLayer(pGroupLayer, pEL);
}
//---------------------------------------------------------------------------



void __fastcall TFormMain::MIDrawEnvelopeClick(TObject *Sender)
{
    TMenuItem* pItem = (TMenuItem*)Sender;
    TBDrawElement->ImageIndex = pItem->ImageIndex;
    TBDrawElement->Tag = pItem->Tag;
    TBDrawElement->OnClick(Sender);
}
//---------------------------------------------------------------------------

bool GetFirstElementLayer(ILayerPtr pLayer, CElementLayerPtr& pEL)
{
    CGroupLayerPtr pGL;
    CAST_PTR(pLayer, pGL, CGroupLayer)
    if (pGL.Assigned())
    {
        dword count = pGL->GetLayerCount();
        for (dword i = 0; i < count; i++)
        {
            ILayerPtr pL;
            pGL->GetLayer(pL, i);
            bool result = GetElementLayerCount(pL);
            if (result) return true;
        }

        return false;
    }

    CAST_PTR(pLayer, pEL, CElementLayer)
    if (pEL.Assigned())
    {
        return true;
    }

    return false;
};

void TFormMain::PrepareElementLayer()
{
    if (!m_pDrawLayer.Assigned())
    {
        CElementLayerPtr pElementLayer;
        dword i, elcount = 0;
        dword count = m_pMap->GetLayerCount();
        for (i = 0; i < count; i++)
        {
            ILayerPtr pLayer;
            m_pMap->GetLayer(pLayer, i);
            elcount += GetElementLayerCount(pLayer);
            CElementLayerPtr pEL;
            if (GetFirstElementLayer(pLayer, pEL))
            {
                pElementLayer = pEL;
            }
        }

        if (0 < elcount)
        {
            m_pDrawLayer = pElementLayer;
        }
        else
        {
            this->CreateElementLayer(NULL, m_pDrawLayer);
        }
    }
}

void __fastcall TFormMain::TBDrawElementClick(TObject *Sender)
{
    switch(TBDrawElement->Tag)
    {
    case 0:
        SetMouseAction(MouseActionDrawEnvelope);
        break;
    case 1:
        SetMouseAction(MouseActionDrawCircle);
        break;
    case 2:
        SetMouseAction(MouseActionDrawEllipse);
        break;
    case 3:
        SetMouseAction(MouseActionDrawPolygon);
        break;
    case 4:
        SetMouseAction(MouseActionDrawPolyline);
        break;
    case 5:
        SetMouseAction(MouseActionDrawPoint);
        break;
    case 6:
        SetMouseAction(MouseActionDrawFreeLine);
        break;
    case 7:
        SetMouseAction(MouseActionDrawFreeFace);
        break;
    default:
        {
            SetMouseAction(MouseActionDefault);
            return;
        }
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::N27Click(TObject *Sender)
{
    m_pTreeRoot->Selected = true;

    CElementLayerPtr pEL;
    this->CreateElementLayer(NULL, pEL);
}
//---------------------------------------------------------------------------





void __fastcall TFormMain::NSelectElementClick(TObject *Sender)
{
    if (MouseActionDefault != m_MouseAction)
    {
        m_pDrawLayer.Clear();
    }

    this->SetMouseAction(MouseActionDefault);    
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::NDeleteElementsClick(TObject *Sender)
{
    bool r = false;
    dword count = m_pMap->GetLayerCount();
    for (dword i = 0; i < count; i++)
    {
        ILayerPtr pLayer;
        m_pMap->GetLayer(pLayer, i);
        if (DeleteElements(pLayer))
        {
            r = true;
        }
    }

    if (r)
    {
        this->UpdateView();
    }
    
    this->SetUndoPoint();
    this->RefreshEditToolButton();
    this->RefreshElementToolButton();
}
//---------------------------------------------------------------------------





void __fastcall TFormMain::TBDrawTextClick(TObject *Sender)
{
    m_FontSize = StrToFloat(EditDrawTextSize->Text);
    SetMouseAction(MouseActionDrawText);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::TBMoveElementsClick(TObject *Sender)
{
    this->SetMouseAction(MouseActionMoveFeatures);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::TBClearElementsSelectionClick(TObject *Sender)
{
    this->DeselectElements(NULL);    
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::TimerDrawTBDownTimer(TObject *Sender)
{
    if (_valid(m_pDownTB))
    {
        m_pDownTB->Down = true;
        m_pDownTB = NULL;
    }

    TimerDrawTBDown->Enabled = false;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::N15Click(TObject *Sender)
{
    DrawElementColorDLG->Color = (TColor)m_FillColor;
    if (!DrawElementColorDLG->Execute()) {return;}
    m_FillColor = DrawElementColorDLG->Color;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::PMDrawColorOutlineClick(TObject *Sender)
{
    DrawElementColorDLG->Color = (TColor)m_OutlineColor;
    if (!DrawElementColorDLG->Execute()) {return;}
    m_OutlineColor = DrawElementColorDLG->Color;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::N28Click(TObject *Sender)
{
    DrawElementColorDLG->Color = (TColor)m_PointColor;
    if (!DrawElementColorDLG->Execute()) {return;}
    m_PointColor = DrawElementColorDLG->Color;
}
//---------------------------------------------------------------------------


void __fastcall TFormMain::PMDrawColorPathClick(TObject *Sender)
{
    DrawElementColorDLG->Color = (TColor)m_PathColor;
    if (!DrawElementColorDLG->Execute()) {return;}
    m_PathColor = DrawElementColorDLG->Color;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::TBDrawTextColorClick(TObject *Sender)
{
    DrawElementColorDLG->Color = (TColor)m_FontColor;
    if (!DrawElementColorDLG->Execute()) {return;}
    m_FontColor = DrawElementColorDLG->Color;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::EditDrawTextSizeChange(TObject *Sender)
{
    m_FontSize = StrToFloat(EditDrawTextSize->Text);
}
//---------------------------------------------------------------------------




void __fastcall TFormMain::LabelManagerClick(TObject *Sender)
{
    ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)
    TFormLabelManager *pFormLabelManager = new TFormLabelManager(this);
    pFormLabelManager->SetLabelLayerManager(pLabelLayerManager);
    pFormLabelManager->ShowModal();
    this->UpdateView();
}
//---------------------------------------------------------------------------

