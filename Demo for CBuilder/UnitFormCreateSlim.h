//---------------------------------------------------------------------------

#ifndef UnitFormCreateSlimH
#define UnitFormCreateSlimH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <Dialogs.hpp>

#include "..\\easylib\\DisplayTransformation.h"
using namespace easymap;

//---------------------------------------------------------------------------
class TFormCreateSlim : public TForm
{
__published:	// IDE-managed Components
    TGroupBox *GroupBox1;
    TLabel *Label6;
    TLabel *Label7;
    TLabel *Label8;
    TEdit *EditLeft;
    TEdit *EditBottom;
    TEdit *EditTop;
    TButton *Button1;
    TButton *Button2;
    TGroupBox *GroupBox2;
    TMemo *Memo1;
    TLabel *Label10;
    TComboBox *ComboBox2;
    TLabel *Label5;
    TEdit *Edit5;
    TLabel *Label9;
    TEdit *EditRight;
    TGroupBox *GBFileName;
    TEdit *EditFileName;
    TSpeedButton *SBFileName;
    TCheckBox *CheckMDS;
    TGroupBox *GroupBox4;
    TLabel *Label1;
    TLabel *Label2;
    TLabel *Label3;
    TLabel *Label4;
    TLabel *Label11;
    TEdit *EditName;
    TComboBox *ComboBox1;
    TEdit *Edit2;
    TEdit *Edit3;
    TComboBox *CBGeoType;
    TSaveDialog *NewDlg;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall Button1Click(TObject *Sender);
    void __fastcall Button2Click(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall CheckMDSClick(TObject *Sender);
    void __fastcall SBFileNameClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    MapUnits GetMapUnit() const;

    __fastcall TFormCreateSlim(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormCreateSlim *FormCreateSlim;
//---------------------------------------------------------------------------
#endif
