//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "UnitFormImportShape.h"
#include "..\\easylib\\StringFuncs.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cdiroutl"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TFormImportShape::TFormImportShape(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormImportShape::Button2Click(TObject *Sender)
{
    this->Close();
}
//---------------------------------------------------------------------------
void __fastcall TFormImportShape::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    Action = caHide;
}
//---------------------------------------------------------------------------
void __fastcall TFormImportShape::Button3Click(TObject *Sender)
{
    //¼ÓÔØShapeFile
    if (!OpenShapeFileDlg->Execute())
    {
        return;
    }

    long filecount = OpenShapeFileDlg->Files->Count;
    for (long i = 0; i < filecount; i++)
    {
        LBShapeFiles->Items->Add(OpenShapeFileDlg->Files->Strings[i]);
    }
}
//---------------------------------------------------------------------------
void __fastcall TFormImportShape::Button4Click(TObject *Sender)
{
    LBShapeFiles->DeleteSelected();
}
//---------------------------------------------------------------------------
void __fastcall TFormImportShape::Button1Click(TObject *Sender)
{
    if (CBSlim->Checked)
    {
        string filename = Trim(string(EditSlimDir->Text.c_str()));
        if (filename == "")
        {
            ::ShowMessage("ÇëÊäÈëSlimData´æ·ÅÎ»ÖÃ¡£");
            return;
        }
    }

    Tag = 1;
    this->Close();
}
//---------------------------------------------------------------------------

MapUnits TFormImportShape::GetMapUnit() const
{
    switch(RGMapUnit->ItemIndex)
    {
    case 0:
        {
            return UNIT_M;
        }
    case 1:
        {
            return UNIT_KM;
        }
    case 2:
        {
            return UNIT_MILE;
        }
    case 3:
        {
            return UNIT_NAUTICALMILE;
        }
    case 4:
        {
            return UNIT_DEGREE;
        }
    }
    return UNIT_M;
}

void __fastcall TFormImportShape::FormShow(TObject *Sender)
{
    Tag = 0;    
}
//---------------------------------------------------------------------------
void __fastcall TFormImportShape::BTSRClick(TObject *Sender)
{
    ::MessageBox(Handle, "ºÙºÙ", ":D", MB_OK);    
}
//---------------------------------------------------------------------------
void __fastcall TFormImportShape::CBSlimClick(TObject *Sender)
{
    EditSlimDir->Enabled = CBSlim->Checked;

    CheckAA->Enabled = CBSlim->Checked;
    if (CheckAA->Checked)
    {
        CBAA->Enabled = CheckAA->Enabled;
    }
    else
    {
        CBAA->Enabled = false;
    }
}
//---------------------------------------------------------------------------



void __fastcall TFormImportShape::CheckAAClick(TObject *Sender)
{
    CBAA->Enabled = CheckAA->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TFormImportShape::RBEnterSRClick(TObject *Sender)
{
    MemoSR->Enabled = RBEnterSR->Checked;
    BTSR->Enabled = RBEnterSR->Checked;
}
//---------------------------------------------------------------------------



