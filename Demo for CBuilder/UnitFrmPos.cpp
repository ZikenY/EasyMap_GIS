//---------------------------------------------------------------------------


#pragma hdrstop

#include "UnitFrmPos.h"

#include "..\\easylib\\StringFuncs.h"
using namespace easymap;

//---------------------------------------------------------------------------

#pragma package(smart_init)

string GetIniFileName()
{
    string exename = Application->ExeName.c_str();
    return RemoveExtNamePart(exename) + ".ini";
}

TFormPos::TFormPos(TWinControl * const pFrm, const string& frmname, bool fixsize,
    bool carevisible)
{
    m_pFrm = pFrm;
    m_FixSize = fixsize;
    m_CareVisible = carevisible;
    m_FormName = easymap::Trim(frmname);
    if (!m_pFrm)
    {
        return;
    }

    string ininame = GetIniFileName();
    string initxt, key, value;
    if (!File2String(ininame, initxt))
    {
        return;
    }

    key = "formposition_" + m_FormName;
    bool r = ini_findkeyvalue(initxt, key, value);
    if (r)
    {
        long n = FindFirstChar(value.c_str(), ',');
        m_pFrm->Left = easymap::StrToInt(value.substr(0, n));
        value = easymap::Trim(value.substr(n+1, value.size()));
        if (m_FixSize)
        {
            m_pFrm->Top = easymap::StrToInt(value.substr(0, value.size()));
        }
        else
        {
            n = FindFirstChar(value.c_str(), ',');
            m_pFrm->Top = easymap::StrToInt(value.substr(0, n));
            value = easymap::Trim(value.substr(n+1, value.size()));

            n = FindFirstChar(value.c_str(), ',');
            m_pFrm->Width = easymap::StrToInt(value.substr(0, n));
            value = easymap::Trim(value.substr(n+1, value.size()));

            m_pFrm->Height = easymap::StrToInt(value.substr(0, value.size()));
        }
    }

    if (m_CareVisible)
    {
        key = "formshow_" + m_FormName;
        ini_findkeyvalue(initxt, key, value);
        if (value == "true")
        {
            m_pFrm->Show();
        }
        else
        {
            m_pFrm->Hide();
        }
    }
}

TFormPos::~TFormPos()
{
    if (!m_pFrm)
    {
        return;
    }

    string ininame = GetIniFileName();
    string initxt, value;
    File2String(ininame, initxt);

    value = easymap::IntToStr(m_pFrm->Left) + ",";
    value = value + easymap::IntToStr(m_pFrm->Top);
    if (!m_FixSize)
    {
        value += ",";
        value = value + easymap::IntToStr(m_pFrm->Width) + ",";
        value = value + easymap::IntToStr(m_pFrm->Height);
    }

    string key = "formposition_" + m_FormName;
    ini_setkeyvalue(initxt, key, value);

    if (m_CareVisible)
    {
        key = "formshow_" + m_FormName;
        value = m_pFrm->Showing ? "true" : "false";
        ini_setkeyvalue(initxt, key, value);
    }

    String2File(initxt, ininame);
}
