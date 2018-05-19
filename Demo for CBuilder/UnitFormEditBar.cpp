//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "UnitFormEditBar.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TFormEditBar::TFormEditBar(TComponent* Owner)
    : TForm(Owner)
{
}

const ICON_ADDPOINT = 2;
const ICON_ADDPOLYLINE = 3;
const ICON_ADDPOLYGON = 4;
const ICON_ADDANNOTATION = 5;
const ICON_UNKNOWN = 18;

void TFormEditBar::SetTBM(TFrmMessager* pTBM)
{
    if (m_pTBM)
    {
        delete m_pTBM;
    }

    m_pTBM = pTBM;
}

bool TFormEditBar::Adding()
{
    return TBSketch->Down;
}

void TFormEditBar::ResetEditLayer()
{
    TBSketch->ImageIndex = ICON_UNKNOWN;
    TBSketch->Enabled = false;
    CBAddTarget->Items->Clear();
    m_pEditLayer.Clear();
    TBSketch->Down = false;
    TBSketch->Tag = 0;
}

void TFormEditBar::GetEditLayer(CVectorLayerPtr& pLayer) const
{
    pLayer = m_pEditLayer._p();
}

void TFormEditBar::SetEditLayer(const ILayerPtr pLayer)
{
    TBSketch->Enabled = true;
    CBAddTarget->Items->Clear();

    if (!pLayer.Assigned())
    {
        TBSketch->Enabled = false;
        return;
    }

    string layername = pLayer->GetName();

    CVectorLayerPtr pSL;
    CAST_PTR(pLayer, pSL, CVectorLayer)
    if (!pSL.Assigned())
    {
        TBSketch->Enabled = false;
        return;
    }

    TBSketch->Enabled = !pSL->ReadOnly();
    CBAddTarget->Enabled = !pSL->ReadOnly();
    CBAddTarget->Items->Add(layername.c_str());

    GeometryColumnInfo colinfo;
    pSL->GetGeometryColumnInfo(colinfo);
    if (1 == colinfo.FeatureType)
    {
        CBAddTarget->ItemsEx->ComboItems[0]->ImageIndex = ICON_ADDANNOTATION;
    }
    else
    {
        ShapeType shapetype = colinfo.ShpType;
        switch (shapetype)
        {
        case SHAPETYPE_POINT:
        case SHAPETYPE_MULTIPOINT:
            CBAddTarget->ItemsEx->ComboItems[0]->ImageIndex = ICON_ADDPOINT;
            break;

        case SHAPETYPE_POLYLINE:
            CBAddTarget->ItemsEx->ComboItems[0]->ImageIndex = ICON_ADDPOLYLINE;
            break;

        case SHAPETYPE_POLYGON:
            CBAddTarget->ItemsEx->ComboItems[0]->ImageIndex = ICON_ADDPOLYGON;
            break;

        default:
            {
            }
        }
    }
    CBAddTarget->ItemIndex = 0;

    m_pEditLayer = pSL._p();
    TBSketch->Down = false;
    TBSketch->Tag = 0;
}

//---------------------------------------------------------------------------
void __fastcall TFormEditBar::TBSelectClick(TObject *Sender)
{
    m_pTBM->Dispatch("select objects");
}
//---------------------------------------------------------------------------
void __fastcall TFormEditBar::FormCreate(TObject *Sender)
{
    m_pTBM = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TFormEditBar::FormDestroy(TObject *Sender)
{
    this->SetTBM(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TFormEditBar::TBClearSelectClick(TObject *Sender)
{
    m_pTBM->Dispatch("clear selection");
}
//---------------------------------------------------------------------------
void __fastcall TFormEditBar::TBDeleteClick(TObject *Sender)
{
    m_pTBM->Dispatch("delete objects");    
}
//---------------------------------------------------------------------------
void __fastcall TFormEditBar::TBMoveClick(TObject *Sender)
{
    m_pTBM->Dispatch("move objects");
}
//---------------------------------------------------------------------------
void __fastcall TFormEditBar::TBVertexEditClick(TObject *Sender)
{
    m_pTBM->Dispatch("vertex edit");
}
//---------------------------------------------------------------------------
void __fastcall TFormEditBar::TBModifyAnnoClick(TObject *Sender)
{
    m_pTBM->Dispatch("modify annotation");
}
//---------------------------------------------------------------------------
void __fastcall TFormEditBar::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    Action = caHide;
    m_pTBM->Dispatch("editbar show/hide");
}
//---------------------------------------------------------------------------


void __fastcall TFormEditBar::TBSketchClick(TObject *Sender)
{
    if (1 == TBSketch->Tag)
    {
        TBSketch->Down = false;
        TBSketch->Tag = 0;
        m_pTBM->Dispatch("cancel edit task");
        return;
    }

    if (0 >= CBAddTarget->Items->Count)
    {
        return;
    }

    long a = CBAddTarget->ItemsEx->ComboItems[0]->ImageIndex;
    switch(a)
    {
    case ICON_ADDPOINT:
        m_pTBM->Dispatch("add point");
        break;

    case ICON_ADDPOLYLINE:
        m_pTBM->Dispatch("add polyline");
        break;

    case ICON_ADDPOLYGON:
        m_pTBM->Dispatch("add polygon");
        break;

    case ICON_ADDANNOTATION:
        m_pTBM->Dispatch("add annotation");
        break;

    default:
        return;
    }

    this->SetTBDown(TBSketch);
    TBSketch->Tag = 1;
}
//---------------------------------------------------------------------------


void __fastcall TFormEditBar::FormHide(TObject *Sender)
{
    m_pTBM->Dispatch("editbar show/hide");
}
//---------------------------------------------------------------------------

void __fastcall TFormEditBar::FormShow(TObject *Sender)
{
    m_pTBM->Dispatch("editbar show/hide");
    m_pDownTB = NULL;    
}
//---------------------------------------------------------------------------

void __fastcall TFormEditBar::TBSaveEditClick(TObject *Sender)
{
    m_pTBM->Dispatch("save edit");
}
//---------------------------------------------------------------------------


void __fastcall TFormEditBar::TimerTBDownTimer(TObject *Sender)
{
    if (_valid(m_pDownTB))
    {
        m_pDownTB->Down = true;
        m_pDownTB = NULL;
    }

    TimerTBDown->Enabled = false;
}

void TFormEditBar::SetTBDown(TToolButton* const pTB, const bool down)
{
    if (_valid(pTB))
    {
        pTB->Down = false;
    }

    m_pDownTB = NULL;
    if (down)
    {
        m_pDownTB = pTB;
        TimerTBDown->Enabled = true;
    }
}

void TFormEditBar::ResetTB()
{
    TBSketch->Down = false;
}
//---------------------------------------------------------------------------

void __fastcall TFormEditBar::TBCancelEditClick(TObject *Sender)
{
    m_pTBM->Dispatch("cancel edit");    
}
//---------------------------------------------------------------------------

