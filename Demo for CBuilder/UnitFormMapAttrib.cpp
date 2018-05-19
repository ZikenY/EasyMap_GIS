//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "UnitFormMapAttrib.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TFormMapAttrib::TFormMapAttrib(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMapAttrib::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    Action = caHide;    
}
//---------------------------------------------------------------------------
void __fastcall TFormMapAttrib::FormShow(TObject *Sender)
{
    Tag = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFormMapAttrib::Button1Click(TObject *Sender)
{
    Tag = 1;
    this->Close();
}
//---------------------------------------------------------------------------
void __fastcall TFormMapAttrib::Button2Click(TObject *Sender)
{
    this->Close();    
}
//---------------------------------------------------------------------------
