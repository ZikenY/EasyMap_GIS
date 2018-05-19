//---------------------------------------------------------------------------

#ifndef UnitFormImportShapeH
#define UnitFormImportShapeH

#include <Classes.hpp>
#include <Controls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <StdCtrls.hpp>
#include <Buttons.hpp>
#include "cdiroutl.h"
#include <Grids.hpp>

#include "..\\easylib\\DisplayTransformation.h"
#include <ComCtrls.hpp>
using namespace easymap;

//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Dialogs.hpp>
//---------------------------------------------------------------------------
class TFormImportShape : public TForm
{
__published:	// IDE-managed Components
    TPageControl *PageControl1;
    TTabSheet *TabSheet1;
    TTabSheet *TabSheet2;
    TGroupBox *GroupBox1;
    TListBox *LBShapeFiles;
    TButton *Button3;
    TButton *Button4;
    TButton *Button6;
    TRadioGroup *RGMapUnit;
    TGroupBox *GroupBox2;
    TEdit *EditScale;
    TGroupBox *GroupBox3;
    TEdit *EditPrecision;
    TGroupBox *GroupBox4;
    TLabel *Label1;
    TComboBox *CBIndexLevel;
    TGroupBox *GroupBox5;
    TMemo *MemoSR;
    TButton *BTSR;
    TRadioButton *RBEnterSR;
    TRadioButton *RBReadSR;
    TGroupBox *GroupBox6;
    TEdit *EditSlimDir;
    TCheckBox *CBSlim;
    TOpenDialog *OpenShapeFileDlg;
    TPanel *Panel1;
    TPanel *Panel2;
    TButton *Button1;
    TButton *Button2;
    TGroupBox *GroupBox7;
    TLabel *Label2;
    TComboBox *CBAA;
    TCheckBox *CheckAA;
    void __fastcall Button2Click(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall Button3Click(TObject *Sender);
    void __fastcall Button4Click(TObject *Sender);
    void __fastcall Button1Click(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall BTSRClick(TObject *Sender);
    void __fastcall CBSlimClick(TObject *Sender);
    void __fastcall CheckAAClick(TObject *Sender);
    void __fastcall RBEnterSRClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    MapUnits GetMapUnit() const;

    __fastcall TFormImportShape(TComponent* Owner);
};
#endif
