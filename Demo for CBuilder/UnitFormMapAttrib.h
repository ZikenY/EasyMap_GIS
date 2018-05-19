//---------------------------------------------------------------------------

#ifndef UnitFormMapAttribH
#define UnitFormMapAttribH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TFormMapAttrib : public TForm
{
__published:	// IDE-managed Components
    TButton *Button1;
    TButton *Button2;
    TLabel *Label1;
    TEdit *Edit1;
    TLabel *Label4;
    TEdit *Edit2;
    TGroupBox *GroupBox2;
    TMemo *Memo1;
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall Button1Click(TObject *Sender);
    void __fastcall Button2Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TFormMapAttrib(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMapAttrib *FormMapAttrib;
//---------------------------------------------------------------------------
#endif
