//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "..\\easylib\\StringFuncs.h"
#include "..\\easylib\\DrawGeometry.h"
#include "..\\easylib\\ShapeAux.h"
using namespace easymap::drawgeometry;

#include "UnitFormSlimQueryResult.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TFormSlimQueryResult::TFormSlimQueryResult(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormSlimQueryResult::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    this->ClearAll();
    Action = caHide;
}
//---------------------------------------------------------------------------
void __fastcall TFormSlimQueryResult::FormCreate(TObject *Sender)
{
    m_pTreeRoot = TreeView1->Items->AddChild(NULL, "result");
    m_pTreeRoot->ImageIndex = 10;
    m_pTreeRoot->SelectedIndex = 10;
    this->ResetAttribGrid();
}
//---------------------------------------------------------------------------

void TFormSlimQueryResult::SetActiveView(const CActiveViewPtr pAV)
{
    m_pAV = pAV;
}

void TFormSlimQueryResult::ClearAll()
{
    m_pTreeRoot->DeleteChildren();
    m_Layers.clear();
    this->ResetAttribGrid();
    PanelLocation->Caption = "Location:";     
}

long GetImageIndexFromLayer(const CVectorLayerPtr pLayer)
{
    bool visible = pLayer->GetVisible();
    GeometryColumnInfo colinfo;
    pLayer->GetGeometryColumnInfo(colinfo);
    if (1 == colinfo.FeatureType)
    {
        return visible ? 9 : 14;
    }
    else
    {
        switch(colinfo.ShpType)
        {
        case SHAPETYPE_POINT:
        case SHAPETYPE_MULTIPOINT:
            return visible ? 6 : 11;

        case SHAPETYPE_POLYLINE:
            return visible ? 7 : 12;

        case SHAPETYPE_POLYGON:
            return visible ? 8 : 13;

        default:
            return 0;
        }
    }
};

void TFormSlimQueryResult::AddLayer(const CVectorLayerPtr pLayer,
    const vector<dword>& fids)
{
    m_Layers.push_back(pLayer);
    string layername = pLayer->GetName();
    TTreeNode* pLayerNode = TreeView1->Items->AddChild(m_pTreeRoot, layername.c_str());
    pLayerNode->ImageIndex = GetImageIndexFromLayer(pLayer);
    pLayerNode->SelectedIndex = GetImageIndexFromLayer(pLayer);

    TStringList* pSList = new TStringList;

    long displayfield = pLayer->GetDisplayField();

    vector<dword>::const_iterator it = fids.begin();
    while (it != fids.end())
    {
        IGeometryPtr pGeometry;
        string attrib, anno;
        pLayer->GetFeature(*it, pGeometry, attrib, anno);
        pSList->SetText((char*)attrib.c_str());
        AnsiString dispname;
        if ((pSList->Count > displayfield) && (0 <= displayfield))
        {
            dispname = pSList->Strings[displayfield] + " ";
        }
        AnsiString sitem = dispname + IntToStr(*it);
        TTreeNode* pObjNode = TreeView1->Items->AddChild(pLayerNode, sitem);
        pObjNode->ImageIndex = -1;
        pObjNode->SelectedIndex = 5;
        it++;
    }

    delete pSList;
}

void TFormSlimQueryResult::ExpandAll()
{
    m_pTreeRoot->Expand(true);
    TTreeNode* pLayerNode = m_pTreeRoot->getFirstChild();
    if (!pLayerNode)
    {
        m_pTreeRoot->Selected = true;
        this->TreeView1NodeChange(m_pTreeRoot);
        return;
    }
    TTreeNode* pFeatureNode = pLayerNode->getFirstChild();
    if (!pFeatureNode)
    {
        pLayerNode->Selected = true;
        this->TreeView1NodeChange(pLayerNode);
        return;
    }
    pFeatureNode->Selected = true;
    this->TreeView1NodeChange(pFeatureNode);
}

void TFormSlimQueryResult::SetLocation(const IGeometryPtr pLoc)
{
    WKSRect rect;
    pLoc->GetMBR(rect);
    double x = (rect.left + rect.right) / 2;
    double y = (rect.top + rect.bottom) / 2;
    PanelLocation->Caption = "Location: (" + Sysutils::FloatToStr(x) + ", "
        + Sysutils::FloatToStr(y) + ")";  
}

void TFormSlimQueryResult::TreeView1NodeChange(TTreeNode *Node)
{
    this->ResetAttribGrid();
    m_pAV->RefreshWindow();

    if (2 == Node->Level)
    {
        long lastpos = FindLastChar(Node->Text.c_str(), ' ');
        dword fid;
        if (0 > lastpos)
        {
            fid = StrToInt(Node->Text);
        }
        else
        {
            static char sfid[10];
            const char* pcfid = Node->Text.c_str() + lastpos;
            ::strcpy(sfid, pcfid);
            fid = StrToInt(sfid);
        }

        IGeometryPtr pGeometry;
        string fieldvalues, anno;
        m_Layers[Node->Parent->Index]->GetFeature(fid, pGeometry, fieldvalues, anno);
        string attrib;
        RestoreFeatureStringValue(fieldvalues, attrib);
        string TextInfo;
        m_Layers[Node->Parent->Index]->GetFields(TextInfo);
        if (pGeometry.Assigned())
        {
            Strings ssInfo;
            ssInfo.SetText(TextInfo.c_str());
            if (ssInfo.GetLineCount() > 0)
            {
                //属性信息，吓搞
                StringGrid1->RowCount = ssInfo.GetLineCount() + 1;
                Strings ssValue;
                ssValue.SetText(attrib.c_str());
                for (dword i = 0; i < ssValue.GetLineCount(); i++)
                {
                    string s;
                    ssInfo.GetLine(i, s);
                    //去掉前缀
                    string s1 = s.substr(0, 4);
                    if ((s1 == "##i_") || (s1 == "##f_") || (s1 == "##s_"))
                    {
                        s = s.substr(4, s.size() - 2);
                    }

                    StringGrid1->Cells[0][i+1] = s.c_str();
                    if (i < ssValue.GetLineCount())
                    {
                        string stmp;
                        ssValue.GetLine(i, stmp);
                        StringGrid1->Cells[1][i+1] = stmp.c_str();
                    }
                    else
                    {
                        StringGrid1->Cells[1][i+1] = "";
                    }
                }
            }

            //高亮显示geometry
            CDisplayPtr pDisplay;
            m_pAV->GetDisplay(pDisplay);
            CDisplayTransformationPtr pTrans;
            pDisplay->GetDisplayTransformation(pTrans);
            HDC dc;
            tagRECT rect;
            pDisplay->GetDC(dc);
            pDisplay->GetRect(rect);
            GeometryType geotype = pGeometry->GetGeometryType();
            int pensize = 1;
            switch(geotype)
            {
            case GEOMETRYTYPE_POINT:
            case GEOMETRYTYPE_MULTIPOINT:
                {
                    pensize = 6;
                    break;
                }
            case GEOMETRYTYPE_PATH:
            case GEOMETRYTYPE_POLYLINE:
                {
                    pensize = 3;
                    break;
                }
            case GEOMETRYTYPE_RING:
            case GEOMETRYTYPE_POLYGON:
                {
                    pensize = 1;
                    break;
                }
            }
            HPEN pen = ::CreatePen(PS_SOLID, pensize, RGB(255,0, 0));
            HPEN oldpen = (HPEN)::SelectObject(dc, pen);
            LOGBRUSH longbrush;
            longbrush.lbColor = RGB(80, 250, 200);
            longbrush.lbStyle = BS_SOLID;
            HBRUSH brush = ::CreateBrushIndirect(&longbrush);
            HBRUSH oldbrush = (HBRUSH)::SelectObject(dc, brush);
            DrawGeneralGeometry(pGeometry, pTrans, dc);
            ::SelectObject(dc, oldbrush);
            ::SelectObject(dc, oldpen);
            ::DeleteObject(brush);
            ::DeleteObject(pen);

            ::Sleep(200);
        }
    }
}

void TFormSlimQueryResult::ResetAttribGrid()
{
    StringGrid1->RowCount = 2;
    StringGrid1->Cells[0][0] = "field";
    StringGrid1->Cells[1][0] = "value";
    StringGrid1->Cells[0][1] = "";
    StringGrid1->Cells[1][1] = "";
}




void __fastcall TFormSlimQueryResult::TreeView1Change(TObject *Sender,
      TTreeNode *Node)
{
    this->TreeView1NodeChange(Node);
}
//---------------------------------------------------------------------------


void __fastcall TFormSlimQueryResult::TreeView1Click(TObject *Sender)
{
    if (NULL == TreeView1->Selected)
    {
        return;
    }
    this->TreeView1NodeChange(TreeView1->Selected);
}
//---------------------------------------------------------------------------



