//---------------------------------------------------------------------------

#ifndef UnitFormSlimAttribH
#define UnitFormSlimAttribH

#include "..\\easylib\\SlimLayer.h"
#include "..\\easylib\\cMap.h"
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
using namespace easymap;

//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TFormSlimAttrib : public TForm
{
__published:	// IDE-managed Components
    TButton *Button1;
    TButton *Button2;
    TLabel *Label1;
    TEdit *Edit1;
    TCheckBox *CheckBox1;
    TCheckBox *CheckBox2;
    TGroupBox *GroupBox1;
    TLabel *Label2;
    TLabel *Label3;
    TEdit *Edit2;
    TEdit *Edit3;
    TLabel *Label4;
    TEdit *Edit4;
    TGroupBox *GroupBox2;
    TMemo *Memo1;
    TGroupBox *GroupBox3;
    TMemo *Memo2;
    TLabel *Label5;
    TEdit *Edit5;
    TLabel *Label6;
    TEdit *Edit6;
    TComboBox *CBLabelLayerField;
    TButton *Button3;
    TLabel *Label7;
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall Button1Click(TObject *Sender);
    void __fastcall Button2Click(TObject *Sender);
    void __fastcall Button3Click(TObject *Sender);
private:	// User declarations
    CMapPtr m_pMap;
    CVectorLayerPtr m_pVL;
    ISymbolPtr m_pSym;

public:		// User declarations
    void SetVectorLayer(const CMapPtr pMap, const CVectorLayerPtr pVL)
    {
        m_pMap = pMap;
        m_pVL = pVL;
    };

    __fastcall TFormSlimAttrib(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormSlimAttrib *FormSlimAttrib;
//---------------------------------------------------------------------------
#endif
