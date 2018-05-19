//---------------------------------------------------------------------------

#ifndef UnitFormEditBarH
#define UnitFormEditBarH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <ToolWin.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------

#include "UnitFrmPos.h"

#include "UnitFrmMessager.h"

#include "..\\easylib\\SlimLayer.h"
using namespace easymap;

class TFormEditBar : public TForm
{
__published:	// IDE-managed Components
    TToolBar *ToolBar1;
    TToolButton *TBSelect;
    TToolButton *TBClearSelect;
    TImageList *ImageList1;
    TToolButton *TBDelete;
    TToolButton *TBMove;
    TToolButton *TBVertexEdit;
    TToolButton *TBModifyAnno;
    TToolButton *ToolButton1;
    TToolButton *ToolButton2;
    TComboBox *CBSelectTarget;
    TToolButton *TBSketch;
    TToolButton *ToolButton3;
    TPanel *Panel1;
    TPanel *Panel2;
    TPopupMenu *PMSelect;
    TMenuItem *NSelect1;
    TComboBoxEx *CBAddTarget;
    TToolButton *TBSaveEdit;
    TToolButton *ToolButton6;
    TTimer *TimerTBDown;
    TPopupMenu *PMSketch;
    TMenuItem *N1;
    TToolButton *TBCancelEdit;
    void __fastcall TBSelectClick(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormDestroy(TObject *Sender);
    void __fastcall TBClearSelectClick(TObject *Sender);
    void __fastcall TBDeleteClick(TObject *Sender);
    void __fastcall TBMoveClick(TObject *Sender);
    void __fastcall TBVertexEditClick(TObject *Sender);
    void __fastcall TBModifyAnnoClick(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall TBSketchClick(TObject *Sender);
    void __fastcall FormHide(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall TBSaveEditClick(TObject *Sender);
    void __fastcall TimerTBDownTimer(TObject *Sender);
    void __fastcall TBCancelEditClick(TObject *Sender);

private:	// User declarations
    TFrmMessager*    m_pTBM;
    CVectorLayerPtr  m_pEditLayer;
    TToolButton*     m_pDownTB;

    void SetTBDown(TToolButton* const pTB, const bool down = true);

public:		// User declarations

    void SetTBM(TFrmMessager* pTBM);

    __fastcall TFormEditBar(TComponent* Owner);

    void SetEditLayer(const ILayerPtr pLayer);
    void GetEditLayer(CVectorLayerPtr& pLayer) const;
    void ResetEditLayer();
    bool Adding();
    void ResetTB();
};
//---------------------------------------------------------------------------
extern PACKAGE TFormEditBar *FormEditBar;
//---------------------------------------------------------------------------

#endif
