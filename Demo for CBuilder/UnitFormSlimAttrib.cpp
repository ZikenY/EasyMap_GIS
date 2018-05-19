//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "..\\easylib\\MultiSymbol.h"
#include "..\\easylib\\LabelLayer.h"

#include "UnitFormSlimAttrib.h"
#include <string>
using namespace std;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TFormSlimAttrib::TFormSlimAttrib(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormSlimAttrib::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    m_pVL.Clear();
    Action = caHide;    
}

string MapUnit2Str(const MapUnits mapunit)
{
    return "猪头";
}

//---------------------------------------------------------------------------
void __fastcall TFormSlimAttrib::FormShow(TObject *Sender)
{
    this->Tag = 0;

    if (!m_pVL.Assigned())
    {
        ::MessageBox(Handle, "垃圾", "", MB_OK);
        this->Close();
        return;
    }

    string layername = m_pVL->GetName();
    Edit1->Text = layername.c_str();

    CheckBox1->Checked = m_pVL->GetVisible();
    CheckBox2->Checked = m_pVL->GetSelectable();

    double maxscale, minscale;
    m_pVL->GetScaleLimit(maxscale, minscale);
    Edit2->Text = FloatToStr(maxscale).c_str();
    Edit3->Text = FloatToStr(minscale).c_str();

    double refscale;
    m_pVL->GetRefScale(refscale);
    Edit4->Text = FloatToStr(refscale).c_str();

    BYTE alpha;
    alpha = m_pVL->GetAlpha();
    Edit6->Text = IntToStr(alpha);

    Memo1->Clear();
    MapUnits mapunit = m_pVL->GetMapUnit();
    Memo1->Lines->Add(AnsiString("坐标单位："));
    Memo1->Lines->Add("  "  + AnsiString(MapUnit2Str(mapunit).c_str()));

    Memo1->Lines->Add("");
    double basescale;
    m_pVL->GetBaseScale(basescale);
    Memo1->Lines->Add("原始比例尺：");
    Memo1->Lines->Add("  " + AnsiString(FloatToStr(basescale).c_str()));

    Memo1->Lines->Add("");
    double precision;
    m_pVL->GetPrecision(precision);
    Memo1->Lines->Add("坐标精度：");
    Memo1->Lines->Add("  " + AnsiString(FloatToStr(precision).c_str()));

    Memo1->Lines->Add("");
    WKSRect extent;
    m_pVL->GetExtent(extent);
    Memo1->Lines->Add("左上角坐标X：");
    Memo1->Lines->Add("  " + AnsiString(FloatToStr(extent.left).c_str()));
    Memo1->Lines->Add("左上角坐标Y：");
    Memo1->Lines->Add("  " + AnsiString(FloatToStr(extent.top).c_str()));
    Memo1->Lines->Add("右下角坐标X：");
    Memo1->Lines->Add("  " + AnsiString(FloatToStr(extent.right).c_str()));
    Memo1->Lines->Add("右下角坐标Y：");
    Memo1->Lines->Add("  " + AnsiString(FloatToStr(extent.bottom).c_str()));

    Memo1->Lines->Add("");
    Memo1->Lines->Add("单四叉树格网尺寸：");
    Memo1->Lines->Add("  猜猜看");

    Memo1->Lines->Add("");
    Memo1->Lines->Add("四叉树级数：");
    Memo1->Lines->Add("  不告诉你");

    Memo1->Lines->Add("");
    string sr;
    sr = m_pVL->GetSpatialReference();
    Memo1->Lines->Add("空间参考与坐标系统信息：");
    Memo1->Text = Memo1->Text + "  " + (sr.c_str());

    string TextInfo;
    m_pVL->GetFields(TextInfo);
    TStringList* pSList = new TStringList;
    pSList->SetText((char*)TextInfo.c_str());
    Memo2->Clear();
    for (long i = 0; i < pSList->Count; i++)
    {
        Memo2->Lines->Add(pSList->Strings[i]);
    }
    delete pSList;

    long displayfield = m_pVL->GetDisplayField();
    Edit5->Text = ::IntToStr(displayfield + 1);

    m_pVL->GetDefaultSymbol(m_pSym);

    CBLabelLayerField->Items->Clear();
    CVectorLayerPtr pVL;
    CAST_PTR(m_pVL, pVL, CVectorLayer)
    CFieldsPtr pFields;
    pVL->GetFields(pFields);
    for (long i = 0; i < pFields->GetFieldCount(); i++)
    {
        IFieldPtr pField;
        pFields->GetField(i, pField._ref());
        AnsiString fieldname = pField->GetFieldName();
        CBLabelLayerField->Items->Add(fieldname);
    }
}
//---------------------------------------------------------------------------
void __fastcall TFormSlimAttrib::Button1Click(TObject *Sender)
{
    m_pVL->SetName(Edit1->Text.c_str());

    m_pVL->SetVisible(CheckBox1->Checked);
    m_pVL->SetSelectable(CheckBox2->Checked);

    double maxscale, minscale;
    maxscale = StrToFloat(Edit2->Text);
    minscale = StrToFloat(Edit3->Text);
    m_pVL->SetScaleLimit(maxscale, minscale);

    double refscale = StrToFloat(Edit4->Text);
    m_pVL->SetRefScale(refscale);

    BYTE alpha = StrToFloat(Edit6->Text);
    m_pVL->SetAlpha(alpha);

    m_pVL->SetDisplayField(::StrToInt(Edit5->Text) - 1);

    m_pVL->SetDefaultSymbol(m_pSym, true);

    this->Tag = 1;
    this->Close();
}
//---------------------------------------------------------------------------
void __fastcall TFormSlimAttrib::Button2Click(TObject *Sender)
{
    this->Close();
}
//---------------------------------------------------------------------------


void __fastcall TFormSlimAttrib::Button3Click(TObject *Sender)
{
    ILabelLayerPtr pLabelLayer = new CLabelLayer;
    AnsiString layername = Edit1->Text + "_Label";
    pLabelLayer->SetName(layername.c_str());
    ILayerPtr pLayer;
    CAST_PTR(m_pVL, pLayer, ILayer)
    pLabelLayer->SetVectorLayer(pLayer._p());
    pLabelLayer->SetFieldIndex(CBLabelLayerField->ItemIndex);

    ILabelLayerManagerPtr pLabelLayerManager;
    CAST_PTR(m_pMap, pLabelLayerManager, ILabelLayerManager)
    pLabelLayerManager->AddLabelLayer(pLabelLayer._p()); 
}
//---------------------------------------------------------------------------

