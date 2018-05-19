//---------------------------------------------------------------------------

#ifndef UnitFormSlimQueryResultH
#define UnitFormSlimQueryResultH
#include <Classes.hpp>
#include <ComCtrls.hpp>
#include <Controls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <StdCtrls.hpp>
#include <Grids.hpp>
#include <ToolWin.hpp>

#include "UnitFrmPos.h"

#include <vector>
#include <string>
using namespace std;

#include "..\\easylib\\SlimLayer.h"
#include "..\\easylib\\cMap.h"
#include "..\\easylib\\ActiveView.h"
using namespace easymap;

//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TFormSlimQueryResult : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TPanel *Panel3;
    TSplitter *Splitter1;
    TPanel *Panel4;
    TTreeView *TreeView1;
    TImageList *ImageList1;
    TStringGrid *StringGrid1;
    TToolBar *ToolBar1;
    TToolButton *TBExport;
    TImageList *ImageList2;
    TToolButton *ToolButton4;
    TComboBox *CBLayers;
    TPanel *Panel5;
    TPanel *PanelLocation;
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall TreeView1Change(TObject *Sender, TTreeNode *Node);
    void __fastcall TreeView1Click(TObject *Sender);
private:	// User declarations
    TTreeNode*                  m_pTreeRoot;
    vector<CVectorLayerPtr>     m_Layers;
    CActiveViewPtr              m_pAV;

    void TreeView1NodeChange(TTreeNode *Node);
    void ResetAttribGrid();

public:		// User declarations
    
    void SetActiveView(const CActiveViewPtr pAV);
    void ClearAll();
    void AddLayer(const CVectorLayerPtr pLayer, const vector<dword>& fids);
    void ExpandAll();
    void SetLocation(const IGeometryPtr pLoc);

    __fastcall TFormSlimQueryResult(TComponent* Owner);
};
//---------------------------------------------------------------------------
#endif
