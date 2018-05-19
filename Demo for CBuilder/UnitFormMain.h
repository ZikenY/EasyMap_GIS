//---------------------------------------------------------------------------

#ifndef UnitFormMainH
#define UnitFormMainH

#include <Classes.hpp>
#include <ComCtrls.hpp>
#include <Controls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include <StdCtrls.hpp>
#include <Buttons.hpp>

#include "..\\easylib\\GeoMap.h"
#include "..\\easylib\\GeometryTracker.h"
using namespace easymap;

#include <vector>
#include <list>
using namespace std;

#include "UnitFormEditBar.h"
#include "UnitFormImportShape.h"
#include "UnitFormSlimQueryResult.h"
#include "UnitFormSlimAttrib.h"
#include "UnitFormMapAttrib.h"
#include "UnitFormCreateSlim.h"
#include "UnitPanelEx.h"
#include "UnitFrmPos.h"

//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include <Dialogs.hpp>
//---------------------------------------------------------------------------

typedef long MouseAction;
const MouseActionDefault            = MouseAction(0);
const MouseActionZoomInOut          = MouseAction(1);
const MouseActionPan                = MouseAction(3);
const MouseActionSelectByEnvelope   = MouseAction(4);
const MouseActionQueryByEnvelope    = MouseAction(5);

const MouseActionAddPoint           = MouseAction(6);
const MouseActionAddMultiPoint      = MouseAction(7);
const MouseActionAddPolyline        = MouseAction(8);
const MouseActionAddPolygon         = MouseAction(9);
const MouseActionAddAnnotation      = MouseAction(10);
const MouseActionMoveObjects        = MouseAction(11);
const MouseActionVertexEdit         = MouseAction(12);
const MouseActionModifyAnnotation   = MouseAction(13);
const MouseActionDrawEnvelope       = MouseAction(14);
const MouseActionDrawCircle         = MouseAction(15);
const MouseActionDrawEllipse        = MouseAction(16);
const MouseActionDrawPolygon        = MouseAction(17);
const MouseActionDrawPolyline       = MouseAction(18);
const MouseActionDrawPoint          = MouseAction(19);
const MouseActionDrawFreeLine       = MouseAction(20);
const MouseActionDrawFreeFace       = MouseAction(21);
const MouseActionDrawText           = MouseAction(22);
const MouseActionMoveFeatures       = MouseAction(23);

class TFormMain : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TPanel *PanelLeft;
    TSplitter *Splitter2;
    TPanel *PanelMain;
    TStatusBar *StatusBar1;
    TMainMenu *MainMenu1;
    TMenuItem *N1;
    TMenuItem *N2;
    TMenuItem *N3;
    TMenuItem *N4;
    TMenuItem *N6;
    TImageList *ImageListToolBar;
    TMenuItem *N7;
    TMenuItem *N8;
    TMenuItem *N10;
    TMenuItem *N11;
    TMenuItem *N12;
    TPanel *Panel2;
    TPanel *Panel3;
    TPanel *Panel4;
    TPanel *Panel5;
    TPanel *MainSpace;
    TOpenDialog *OpenWorkSpaceDlg;
    TSaveDialog *SaveWorkSpaceDlg;
    TImageList *ImageListTreeLayer;
    TMenuItem *N13;
    TMenuItem *ShapeFile1;
    TMenuItem *N14;
    TMenuItem *NLayers;
    TMenuItem *N16;
    TPageControl *PageControl1;
    TTabSheet *TabSheetLayers;
    TTreeView *TreeViewLayers;
    TPanel *PanelCloseLeft;
    TMenuItem *N17;
    TMenuItem *About1;
    TMenuItem *N18;
    TMenuItem *NSelect;
    TMenuItem *NClearSelect;
    TPopupMenu *PMTree;
    TMenuItem *MILayerMapAttr;
    TMenuItem *MIRemoveLayer;
    TMenuItem *MILoadShape;
    TMenuItem *MINewGroupLyr;
    TMenuItem *MINewSlim;
    TMenuItem *N9;
    TMenuItem *N23;
    TMenuItem *N24;
    TMenuItem *N25;
    TMenuItem *NEditTools;
    TMenuItem *Undo1;
    TMenuItem *Redo1;
    TMenuItem *NSaveEdit;
    TMenuItem *NCancelEdit;
    TOpenDialog *LoadDataDlg;
    TMenuItem *CreateSlim1;
    TMenuItem *N22;
    TMenuItem *NZoomIn;
    TMenuItem *NZoomOut;
    TMenuItem *MIExport;
    TMenuItem *Export1;
    TSaveDialog *ExportDialog;
    TCoolBar *CoolBar1;
    TToolBar *ToolBar1;
    TToolButton *ToolButton1;
    TToolButton *ToolButton2;
    TToolButton *ToolButton3;
    TToolButton *ToolButton21;
    TToolButton *ToolButton8;
    TToolButton *ToolButton10;
    TToolButton *ToolButton11;
    TToolButton *ToolButton12;
    TToolButton *ToolButton9;
    TToolButton *ToolButton18;
    TToolButton *ToolButton6;
    TToolButton *TBIdentify;
    TToolButton *ToolButton22;
    TToolButton *TBEditTools;
    TToolButton *ToolButton24;
    TMenuItem *N5;
    TMenuItem *NIdentify;
    TToolButton *ToolButton4;
    TToolButton *ToolButton5;
    TToolButton *TBUndo;
    TToolButton *TBRedo;
    TToolButton *ToolButton26;
    TMenuItem *N19;
    TMenuItem *N20;
    TToolButton *TBLayers;
    TMenuItem *N26;
    TMenuItem *MINewElements;
    TToolButton *ToolButton17;
    TPopupMenu *PMDrawElement;
    TMenuItem *MIDrawEnvelope;
    TMenuItem *MIDrawCircle;
    TMenuItem *MIDrawEllipse;
    TMenuItem *MIDrawPolygon;
    TMenuItem *MIDrawPolyline;
    TMenuItem *MIDrawPoint;
    TColorDialog *DrawElementColorDLG;
    TComboBox *CBScale;
    TImageList *ImageListDrawElement;
    TToolButton *ToolButton20;
    TToolButton *ToolButton23;
    TToolButton *TBLayerAttrib;
    TToolButton *ToolButton14;
    TPanel *Panel7;
    TToolBar *ToolBar2;
    TToolButton *ToolButton15;
    TPanel *Panel6;
    TToolButton *TBDrawElement;
    TPanel *PanrlDrawFont;
    TEdit *EditDrawText;
    TToolButton *TBDrawText;
    TToolButton *ToolButton13;
    TToolButton *TBSelectElement;
    TToolButton *TBDeleteElements;
    TToolButton *ToolButton16;
    TToolButton *TBMoveElements;
    TToolButton *TBClearElementsSelection;
    TMenuItem *MIFreeHandLine;
    TMenuItem *MIFreeHandFace;
    TTimer *TimerDrawTBDown;
    TMenuItem *N21;
    TToolButton *TBDrawColor;
    TPopupMenu *PMDrawColor;
    TMenuItem *N15;
    TMenuItem *PMDrawColorOutline;
    TMenuItem *N28;
    TMenuItem *PMDrawColorPath;
    TPanel *Panel8;
    TEdit *EditDrawTextSize;
    TToolButton *TBDrawTextColor;
    TMenuItem *LabelManager;
    TMenuItem *N29;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall N4Click(TObject *Sender);
    void __fastcall N2Click(TObject *Sender);
    void __fastcall MainSpaceResize(TObject *Sender);
    void __fastcall N11Click(TObject *Sender);
    void __fastcall N3Click(TObject *Sender);
    void __fastcall N6Click(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall ShapeFile1Click(TObject *Sender);
    void __fastcall N12Click(TObject *Sender);
    void __fastcall NLayersClick(TObject *Sender);
    void __fastcall PanelLeftResize(TObject *Sender);
    void __fastcall MainSpaceMouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y);
    void __fastcall MainSpaceMouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
    void __fastcall MainSpaceMouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
    void __fastcall N8Click(TObject *Sender);
    void __fastcall N10Click(TObject *Sender);
    void __fastcall About1Click(TObject *Sender);
    void __fastcall TreeViewLayersDblClick(TObject *Sender);
    void __fastcall PMTreePopup(TObject *Sender);
    void __fastcall MILayerMapAttrClick(TObject *Sender);
    void __fastcall TreeViewLayersMouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
    void __fastcall MIRemoveLayerClick(TObject *Sender);
    void __fastcall MINewGroupLyrClick(TObject *Sender);
    void __fastcall TreeViewLayersDragOver(TObject *Sender,
          TObject *Source, int X, int Y, TDragState State, bool &Accept);
    void __fastcall TreeViewLayersEditing(TObject *Sender, TTreeNode *Node,
          bool &AllowEdit);
    void __fastcall TreeViewLayersEdited(TObject *Sender, TTreeNode *Node,
          AnsiString &S);
    void __fastcall TreeViewLayersMouseUp(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
    void __fastcall TreeViewLayersMouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
    void __fastcall TreeViewLayersDragDrop(TObject *Sender,
          TObject *Source, int X, int Y);
    void __fastcall N9Click(TObject *Sender);
    void __fastcall N24Click(TObject *Sender);
    void __fastcall N25Click(TObject *Sender);
    void __fastcall MILoadShapeClick(TObject *Sender);
    void __fastcall MINewSlimClick(TObject *Sender);
    void __fastcall NEditToolsClick(TObject *Sender);
    void __fastcall NSaveEditClick(TObject *Sender);
    void __fastcall NCancelEditClick(TObject *Sender);
    void __fastcall Undo1Click(TObject *Sender);
    void __fastcall Redo1Click(TObject *Sender);
    void __fastcall TreeViewLayersClick(TObject *Sender);
    void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
    void __fastcall CreateSlim1Click(TObject *Sender);
    void __fastcall MIExportClick(TObject *Sender);
    void __fastcall CBScaleSelect(TObject *Sender);
    void __fastcall NZoomInClick(TObject *Sender);
    void __fastcall NZoomOutClick(TObject *Sender);
    void __fastcall NSelectClick(TObject *Sender);
    void __fastcall NClearSelectClick(TObject *Sender);
    void __fastcall NIdentifyClick(TObject *Sender);
    void __fastcall PanelCloseLeftClick(TObject *Sender);
    void __fastcall MINewElementsClick(TObject *Sender);
    void __fastcall MIDrawEnvelopeClick(TObject *Sender);
    void __fastcall TBDrawElementClick(TObject *Sender);
    void __fastcall N27Click(TObject *Sender);
    void __fastcall NSelectElementClick(TObject *Sender);
    void __fastcall NDeleteElementsClick(TObject *Sender);
    void __fastcall TBDrawTextClick(TObject *Sender);
    void __fastcall TBMoveElementsClick(TObject *Sender);
    void __fastcall TBClearElementsSelectionClick(TObject *Sender);
    void __fastcall TimerDrawTBDownTimer(TObject *Sender);
    void __fastcall N15Click(TObject *Sender);
    void __fastcall PMDrawColorOutlineClick(TObject *Sender);
    void __fastcall N28Click(TObject *Sender);
    void __fastcall PMDrawColorPathClick(TObject *Sender);
    void __fastcall TBDrawTextColorClick(TObject *Sender);
    void __fastcall EditDrawTextSizeChange(TObject *Sender);
    void __fastcall LabelManagerClick(TObject *Sender);
private:	// User declarations
    bool                    m_InitOK;
    CMapPtr                 m_pMap;
    HDC                     m_dc;
    TTreeNode*              m_pTreeRoot;
    TPanelEx*               m_pMainSpace;
    MouseAction             m_MouseAction;
    bool                    m_RightButtonPan;
    bool                    m_TreeDrag;
    vector<WKSRect>         m_ViewExts;
    long                    m_CurrViewExt;
    bool                    m_DoNotPushExt;
    vector<long>            m_NodeSelectedPath;
    TFormEditBar*           m_pEditBar;
    TFormImportShape*       m_pImportShpFrm;
    TFormSlimQueryResult*   m_pSlimQRFrm;
    TFormSlimAttrib*        m_pSlimAttrib;
    TFormMapAttrib*         m_pFormMapAttrib;
    TFormCreateSlim*        m_pFormCreateSlim;
    string                  m_TWSFileName;
    vector<tagPOINT>        m_TrackPoints;
    vector<WKSPointZ>       m_TrackWKSPoints;
    CMoveTrackerPtr         m_pMoveTracker;
    list<IVectorFeaturePtr>   m_MoveFeatures;
    list<CElementLayerPtr>  m_MoveElementLayers;
    dword                   m_ElementLayerId;
    CElementLayerPtr        m_pDrawLayer;
    bool                    m_LeftMouseDown;
    COLORREF                m_FillColor;
    COLORREF                m_OutlineColor;
    double                  m_OutlineWidth;
    COLORREF                m_PathColor;
    double                  m_PathWidth;
    COLORREF                m_PointColor;
    double                  m_PointDiameter;
    long                    m_FontSize;
    COLORREF                m_FontColor;
    TFormPos*               m_pFrmPos[4];
    TToolButton*            m_pDownTB;

    void InAction();
    void ZoomIn();
    void ZoomPan();
    void ZoomCenter(const double quotiety);
    void ZoomAll();
    void UpdateView();
    void PartialUpdate(const WKSRect& env);
    void UpdateSelection(const WKSRect* const pEnvelope = NULL);
    void SelectFeaturesByEnvelope();

    void NewWorkspace();
    void ViewFullMap();
    void RefreshLayerTree();
    void GetSelectLayer(ILayerPtr& pLayer) const;
    ILayerPtr GetLayerByNode(TTreeNode* pNode) const;
    void RefreshNodeSelectedPath();
    TTreeNode* GetLastNodeSelected(const bool expand = true) const;
    bool CreateElementLayer(const CGroupLayerPtr pGroupLayer,
        CElementLayerPtr& pElementLayer);
    bool LoadData(const CGroupLayerPtr pGroupLayer);
    bool CreateSilmData(const CGroupLayerPtr pGroupLayer);
    bool LoadSlimData(const CGroupLayerPtr pGroupLayer, const string& filename,
        bool readonly);
    bool OpenSHP(const CGroupLayerPtr pGroupLayer, const bool readonly);
    bool ImportSHP(const string& shapefile, const MapUnits mapunit,
        const double& basescale, const double& precision, const long indexlevel,
        const long annofield, CGroupLayerPtr pGroupLayer, const string& slimdir,
        const string& sr);
    bool LoadShape(const string& shapefile, const MapUnits mapunit,
        const double& basescale, const double& precision, const long indexlevel,
        const bool readonly, CGroupLayerPtr pGroupLayer);

    bool LoadWorkspace(const char* pcTwsFile);
    bool SaveWorkspace(const char* pcTwsFile);
    void AddGroupSubNodes(const ILayerPtr pLayer, TTreeNode* const pNode);
    void SelectElements(const WKSRect& envelope, const bool append);
    void DeselectElements(const WKSRect* pEnvelope);
    void SelectFeaturesByEnvelope(const WKSRect& envelope, const bool partialselect = true,
        const bool append = false);
    void DeselectFeaturesByEnvelope(const WKSRect& envelope, const bool partialselect = true);
    void QueryByEnvelope(const WKSRect& envelope, const bool partialselect = true);
    void ClearSelection();

    bool EditAddPoint(int X, int Y);
    bool EditAddFeature();
    bool EditAddElement();
    void AddElement(const CElementPtr pElement);
    void EditTrackVertex(int X, int Y);
    bool PrepareMoveFeatures(int X, int Y);
    void PrepareMoveFeatures(ILayerPtr pLayer);
    bool PrepareMoveElements(int X, int Y);
    void PrepareMoveElements(ILayerPtr pLayer);
    dword FinishMoveFeatures();
    bool FinishMoveElements(const double delta_x, const double delta_y);
    void SetUndoPoint();

    bool EditStatus(const long statustype) const;   //  statustype == 0 : is_dirty
                                                    //  statustype == 1 : undoable
                                                    //  statustype == 2 : redoable
    void RefreshEditToolButton();
    void ClearDisableEditTool();
    void RefreshElementToolButton();
    bool PromptSaveEdit();

    void PlaceEditTool();

    void ResetViewExtentList();
    void GetDrawLineSymbol(ILineSymbolPtr& pSymbol);
    void GetDrawFillSymbol(IFillSymbolPtr& pSymbol);
    void GetDrawPointSymbol(IPointSymbolPtr& pSymbol);
    void GetDrawTextSymbol(ITextSymbolPtr& pSymbol);
    void PrepareElementLayer();

    CActiveViewPtr GetAV() const;
    CDisplayPtr GetDisplay() const;
    CDisplayTransformationPtr GetDT() const;

    void SetTBDown(TToolButton* const pTB, const bool down = true);

public:		// User declarations
    void RefreshScaleDisplay();
    void PushViewExt(const WKSRect& rect);

    void SetMouseAction(const MouseAction mouseaction);
    void CancelEditTask();  //再次点击了m_pEditBar->TBSketch
    void AddPoint();
    void AddPolyline();
    void AddPolygon();
    void AddAnno();
    void DeleteFeatures();
    void MoveFeatures();

    void RefreshFrmShowButtons();

    __fastcall TFormMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
//---------------------------------------------------------------------------

class TFrmMainMessager : public TFrmMessager
{
public:
    TFrmMainMessager(TFormMain* const pFormMain);
private:
    TFrmMainMessager();

private:
    TFormMain* m_pFormMain;

public:
    long Dispatch(const string& msg);
};

#endif
