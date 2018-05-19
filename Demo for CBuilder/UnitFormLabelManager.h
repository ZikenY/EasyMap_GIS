//---------------------------------------------------------------------------

#ifndef UnitFormLabelManagerH
#define UnitFormLabelManagerH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>

#include "..\\easylib\\LabelLayer.h"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>

using namespace easymap;

//---------------------------------------------------------------------------
class TFormLabelManager : public TForm
{
__published:	// IDE-managed Components
    TGroupBox *GroupBox1;
    TListBox *LBLayerList;
    TButton *Button1;
    TButton *Button2;
    TButton *Button3;
    TGroupBox *GroupBox2;
    TLabel *Label1;
    TEdit *EditLayerName;
    TLabel *Label2;
    TComboBox *CBFieldIndex;
    TLabel *Label3;
    TLabel *Label4;
    TPanel *PanelFontColor;
    TLabel *Label5;
    TEdit *EditFontHeight;
    TEdit *EditFontWidth;
    TButton *Button4;
    TEdit *EditVectorLayer;
    TLabel *Label6;
    TColorDialog *ColorDialog1;
    TCheckBox *CBVisible;
    TEdit *EditMaxScale;
    TLabel *Label7;
    TLabel *Label8;
    TEdit *EditMinScale;
    TLabel *Label9;
    TEdit *EditRefScale;
    void __fastcall Button4Click(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall LBLayerListClick(TObject *Sender);
    void __fastcall Button3Click(TObject *Sender);
    void __fastcall Button1Click(TObject *Sender);
    void __fastcall Button2Click(TObject *Sender);
    void __fastcall EditLayerNameChange(TObject *Sender);
    void __fastcall CBFieldIndexChange(TObject *Sender);
    void __fastcall EditFontHeightChange(TObject *Sender);
    void __fastcall EditFontWidthChange(TObject *Sender);
    void __fastcall PanelFontColorClick(TObject *Sender);
    void __fastcall CBVisibleClick(TObject *Sender);
    void __fastcall EditMaxScaleChange(TObject *Sender);
    void __fastcall EditMinScaleChange(TObject *Sender);
    void __fastcall EditRefScaleChange(TObject *Sender);
private:	// User declarations
    ILabelLayerManagerPtr m_pLabelLayerManager;
    bool m_Flag;

    void RefreshLayerList();

public:		// User declarations
    __fastcall TFormLabelManager(TComponent* Owner);
    void SetLabelLayerManager(ILabelLayerManagerPtr LabelLayerManager)
    {
        m_pLabelLayerManager = LabelLayerManager;
    }

};
//---------------------------------------------------------------------------
extern PACKAGE TFormLabelManager *FormLabelManager;
//---------------------------------------------------------------------------
#endif
