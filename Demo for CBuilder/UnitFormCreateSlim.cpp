//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "UnitFormCreateSlim.h"
#include "..\\easylib\\StringFuncs.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TFormCreateSlim::TFormCreateSlim(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormCreateSlim::FormShow(TObject *Sender)
{
    Tag = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFormCreateSlim::Button1Click(TObject *Sender)
{
    if (!CheckMDS->Checked)
    {
        string filename = Trim(string(EditFileName->Text.c_str()));
        if (filename == "")
        {
            ::ShowMessage("请输入数据文件名。");
            return;
        }
    }

    Tag = 1;
    this->Close();
}
//---------------------------------------------------------------------------
void __fastcall TFormCreateSlim::Button2Click(TObject *Sender)
{
    this->Close();    
}
//---------------------------------------------------------------------------
void __fastcall TFormCreateSlim::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    Action = caHide;
}
//---------------------------------------------------------------------------

MapUnits TFormCreateSlim::GetMapUnit() const
{
    switch(ComboBox1->ItemIndex)
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

void __fastcall TFormCreateSlim::CheckMDSClick(TObject *Sender)
{
    EditFileName->Enabled = !CheckMDS->Checked;
    SBFileName->Enabled = !CheckMDS->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TFormCreateSlim::SBFileNameClick(TObject *Sender)
{
    if (!NewDlg->Execute())
    {
        return;
    }

    string filename = NewDlg->FileName.c_str();
    string ext = GetExtNamePart(filename);
    LowerString(ext);
    if (ext == "")
    {
        filename = RemoveExtNamePart(filename);
        filename = filename + ".esd";
    }

    EditFileName->Text = filename.c_str();
}
//---------------------------------------------------------------------------

