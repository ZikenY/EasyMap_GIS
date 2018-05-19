//---------------------------------------------------------------------------

#ifndef UnitPanelExH
#define UnitPanelExH

#include "..\\easylib\\cMap.h"
#include "..\\easylib\\ActiveView.h"
using namespace easymap;

#include <ExtCtrls.hpp>

class TPanelEx : public TPanel
{
public:
    __fastcall TPanelEx(Extctrls::TPanel* AOwner) : TPanel(AOwner)
    {
        m_pDummy = AOwner;

        this->OnMouseMove = m_pDummy->OnMouseMove;
        this->OnMouseDown = m_pDummy->OnMouseDown;
        this->OnMouseUp = m_pDummy->OnMouseUp;
    };

    void SetActiveView(const CActiveViewPtr pAV)
    {
        m_pAV = pAV;
    };

private:
    TPanel* m_pDummy;
    CActiveViewPtr m_pAV;

    void __fastcall PaintWindow(HDC DC)
    {
        CDisplayPtr pDisplay;
        m_pAV->RefreshWindow();
    };
};

//---------------------------------------------------------------------------
#endif
