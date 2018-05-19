//---------------------------------------------------------------------------

#ifndef UnitFrmPosH
#define UnitFrmPosH

#include <string>
using namespace std;

#include <Forms.hpp>

string GetIniFileName();

class TFormPos
{
public:
    TFormPos(TWinControl * const pFrm, const string& frmname, bool fixsize = false,
        bool carevisible = true);
    ~TFormPos();

private:
    TFormPos();//disable;

private:
    TWinControl *m_pFrm;
    string m_FormName;
    bool m_FixSize;
    bool m_CareVisible;
};


//---------------------------------------------------------------------------
#endif
