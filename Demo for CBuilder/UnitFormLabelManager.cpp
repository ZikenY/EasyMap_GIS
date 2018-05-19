//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "UnitFormLabelManager.h"
#include "..\\easylib\\SlimLayer.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
__fastcall TFormLabelManager::TFormLabelManager(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::Button4Click(TObject *Sender)
{
    this->Close();    
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    Action = caFree;
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::FormShow(TObject *Sender)
{
    m_Flag = false;
    this->RefreshLayerList();
}

void TFormLabelManager::RefreshLayerList()
{
    LBLayerList->Items->Clear();
    EditLayerName->Text = "";
    CBFieldIndex->Items->Clear();
    EditFontHeight->Text = "";
    EditFontWidth->Text = "";
    PanelFontColor->Color = clBtnFace;

    if (!m_pLabelLayerManager.Assigned())
    {
        return;
    }

    for (long i = 0; i < m_pLabelLayerManager->GetLabelLayerCount(); i++)
    {
        ILabelLayerPtr pLabelLayer;
        m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), i);
        AnsiString layername = pLabelLayer->GetName();
        if (Trim(layername) == "")
        {
            layername = "วเอÜ";
        }

        LBLayerList->Items->Add(layername);
    }

    if (LBLayerList->Items->Count > 0)
    {
        LBLayerList->ItemIndex = 0;
    }

    this->LBLayerListClick(this);
}

//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::LBLayerListClick(TObject *Sender)
{
    m_Flag = true;

    EditLayerName->Text = "";
    CBFieldIndex->Items->Clear();
    EditFontHeight->Text = "";
    EditFontWidth->Text = "";
    PanelFontColor->Color = clBtnFace;

    if (LBLayerList->ItemIndex < 0)
    {
        m_Flag = false;
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), LBLayerList->ItemIndex);
    AnsiString layername = pLabelLayer->GetName();
    EditLayerName->Text = layername;

    CBVisible->Checked = pLabelLayer->GetVisible();
    double maxscale, minscale;
    pLabelLayer->GetScaleLimit(maxscale, minscale);
    EditMaxScale->Text = FloatToStr(maxscale).c_str();
    EditMinScale->Text = FloatToStr(minscale).c_str();

    double refscale;
    refscale = pLabelLayer->GetRefScale();
    EditRefScale->Text = FloatToStr(refscale).c_str();

    ILayerPtr pLayer;
    pLabelLayer->GetVectorLayer(pLayer._ref());
    CVectorLayerPtr pVL;
    CAST_PTR(pLayer, pVL, CVectorLayer)
    AnsiString vectorlayername = pVL->GetName();
    EditVectorLayer->Text = vectorlayername;
    CFieldsPtr pFields;
    pVL->GetFields(pFields);
    for (long i = 0; i < pFields->GetFieldCount(); i++)
    {
        IFieldPtr pField;
        pFields->GetField(i, pField._ref());
        AnsiString fieldname = pField->GetFieldName();
        CBFieldIndex->Items->Add(fieldname);
    }

    CBFieldIndex->ItemIndex = pLabelLayer->GetFieldIndex();

    ITextSymbolPtr pTextSymbol;
    pLabelLayer->GetTextSymbol(pTextSymbol._ref());
    double height, width;
    pTextSymbol->GetHeight(height);
    pTextSymbol->GetWidth(width);
    string sHeight = FloatToStr(height);
    string sWidth = FloatToStr(width);
    EditFontHeight->Text = sHeight.c_str();
    EditFontWidth->Text = sWidth.c_str();
    COLORREF textcolor;
    pTextSymbol->GetColor(textcolor);
    PanelFontColor->Color = textcolor;

    m_Flag = false;
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::Button3Click(TObject *Sender)
{
    if (LBLayerList->ItemIndex < 0)
    {
        return;
    }

    long oldindex = LBLayerList->ItemIndex;
    m_pLabelLayerManager->RemoveLabelLayer(LBLayerList->ItemIndex);
    this->RefreshLayerList();

    if (LBLayerList->Items->Count == 0)
    {
        return;
    }

    if (LBLayerList->Items->Count > oldindex)
    {
        LBLayerList->ItemIndex = oldindex;
    }
    else
    {
        LBLayerList->ItemIndex = LBLayerList->Items->Count - 1;
    }

    this->LBLayerListClick(this);
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::Button1Click(TObject *Sender)
{
    if (LBLayerList->ItemIndex < 0)
    {
        return;
    }

    long oldindex = LBLayerList->ItemIndex;
    if (oldindex < 1)
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), oldindex);
    m_pLabelLayerManager->SetLabelLayerOrder(pLabelLayer._p(), oldindex-1);
    this->RefreshLayerList();

    LBLayerList->ItemIndex = oldindex-1;
    this->LBLayerListClick(this);
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::Button2Click(TObject *Sender)
{
    if (LBLayerList->ItemIndex < 0)
    {
        return;
    }

    long oldindex = LBLayerList->ItemIndex;
    if (oldindex > LBLayerList->Items->Count - 2)
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), oldindex);
    m_pLabelLayerManager->SetLabelLayerOrder(pLabelLayer._p(), oldindex+1);
    this->RefreshLayerList();

    LBLayerList->ItemIndex = oldindex+1;
    this->LBLayerListClick(this);
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::EditLayerNameChange(TObject *Sender)
{
    long itemindex = LBLayerList->ItemIndex;
    if ((itemindex < 0) || (m_Flag))
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), itemindex);
    AnsiString layername = EditLayerName->Text;
    pLabelLayer->SetName(layername.c_str());
    LBLayerList->Items->Strings[LBLayerList->ItemIndex] = layername;
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::CBFieldIndexChange(TObject *Sender)
{
    long itemindex = LBLayerList->ItemIndex;
    if ((itemindex < 0) || (m_Flag))
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), itemindex);
    pLabelLayer->SetFieldIndex(CBFieldIndex->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::EditFontHeightChange(TObject *Sender)
{
    long itemindex = LBLayerList->ItemIndex;
    if ((itemindex < 0) || (m_Flag))
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), itemindex);
    ITextSymbolPtr pTextSymbol;
    pLabelLayer->GetTextSymbol(pTextSymbol._ref());
    double height = StrToFloat(EditFontHeight->Text);
    pTextSymbol->SetHeight(height);
    pLabelLayer->SetTextSymbol(pTextSymbol._p());
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::EditFontWidthChange(TObject *Sender)
{
    long itemindex = LBLayerList->ItemIndex;
    if ((itemindex < 0) || (m_Flag))
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), itemindex);
    ITextSymbolPtr pTextSymbol;
    pLabelLayer->GetTextSymbol(pTextSymbol._ref());
    double width = StrToFloat(EditFontWidth->Text);
    pTextSymbol->SetWidth(width);
    pLabelLayer->SetTextSymbol(pTextSymbol._p());
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::PanelFontColorClick(TObject *Sender)
{
    long itemindex = LBLayerList->ItemIndex;
    if ((itemindex < 0) || (m_Flag))
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), itemindex);
    ITextSymbolPtr pTextSymbol;
    pLabelLayer->GetTextSymbol(pTextSymbol._ref());
    pTextSymbol->GetColor(ColorDialog1->Color);

    if (!ColorDialog1->Execute())
    {
        return;
    }

    PanelFontColor->Color = ColorDialog1->Color;  
    pTextSymbol->SetColor(PanelFontColor->Color);
    pLabelLayer->SetTextSymbol(pTextSymbol._p());
}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::CBVisibleClick(TObject *Sender)
{
    long itemindex = LBLayerList->ItemIndex;
    if ((itemindex < 0) || (m_Flag))
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), itemindex);
    bool visible = CBVisible->Checked;
    pLabelLayer->SetVisible(visible);

}
//---------------------------------------------------------------------------
void __fastcall TFormLabelManager::EditMaxScaleChange(TObject *Sender)
{
    long itemindex = LBLayerList->ItemIndex;
    if ((itemindex < 0) || (m_Flag))
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), itemindex);

    double maxscale = StrToFloat(EditMaxScale->Text);
    double minscale = StrToFloat(EditMinScale->Text);
    pLabelLayer->SetScaleLimit(maxscale, minscale);
}
//---------------------------------------------------------------------------

void __fastcall TFormLabelManager::EditMinScaleChange(TObject *Sender)
{
    long itemindex = LBLayerList->ItemIndex;
    if ((itemindex < 0) || (m_Flag))
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), itemindex);

    double maxscale = StrToFloat(EditMaxScale->Text);
    double minscale = StrToFloat(EditMinScale->Text);
    pLabelLayer->SetScaleLimit(maxscale, minscale);
   
}
//---------------------------------------------------------------------------

void __fastcall TFormLabelManager::EditRefScaleChange(TObject *Sender)
{
    long itemindex = LBLayerList->ItemIndex;
    if ((itemindex < 0) || (m_Flag))
    {
        return;
    }

    ILabelLayerPtr pLabelLayer;
    m_pLabelLayerManager->GetLabelLayer(pLabelLayer._ref(), itemindex);

    double refscale = StrToFloat(EditRefScale->Text);
    pLabelLayer->SetRefScale(refscale);

}
//---------------------------------------------------------------------------

