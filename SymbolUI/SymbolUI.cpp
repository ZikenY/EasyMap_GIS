#include "SymbolUI.h"

#pragma warning(disable: 4786)
#include <string>
using namespace std;

namespace easymap
{

symboluidll symboluidll::m_DLL;

symboluidll::symboluidll()
{
    m_hInstance = NULL;
    m_pSetMainWnd = NULL;
    m_pSelectSymbol = NULL;

    loaddll("SymbolUI.dll");
}

symboluidll::~symboluidll()
{
//    releasedll();
}

inline long _symui_findlastchar(const char* pc, const char find)
{
    char* p = (char*)pc;
    long offset = -1;
    while (0 != *p)
    {
        if (find == *p) offset = p - pc;
        p++;
    }

    return offset;
}

inline string _symui_getdirectorypart(const string& pathfilename)
{
    long offset = _symui_findlastchar(pathfilename.c_str(), '\\');
    if (0 > offset)
    {
        return string("");
    }

    return string(pathfilename.substr(0, offset));
}

bool symboluidll::loaddll(const char* dllfile)
{
    if (!dllfile)
        return false;

    if (getDLLHandle())
    {
        ::FreeLibrary(m_DLL.m_hInstance);
        m_DLL.m_hInstance = NULL;
        m_DLL.m_pSetMainWnd = NULL;
        m_DLL.m_pSelectSymbol = NULL;
    }

    string s = dllfile;
    s = _symui_getdirectorypart(s);
    ::SetCurrentDirectory(s.c_str());
    m_DLL.m_hInstance = ::LoadLibrary(dllfile);
    if (!symboluidll::m_DLL.m_hInstance)
        return false;

    m_DLL.m_pSetMainWnd = (_SetMainWnd*)::GetProcAddress(m_DLL.m_hInstance, "SetMainWnd");
    m_DLL.m_pSelectSymbol = (_SelectSymbol*)::GetProcAddress(m_DLL.m_hInstance, "SelectSymbol");
    return true;
}

void symboluidll::releasedll()
{
    if (m_DLL.m_hInstance) ::FreeLibrary(m_DLL.m_hInstance);
}

HINSTANCE symboluidll::getDLLHandle()
{
    return symboluidll::m_DLL.m_hInstance;
}

bool symboluidll::SetMainWnd(HWND wnd)
{
    return (*m_DLL.m_pSetMainWnd)(wnd);
}

bool symboluidll::SelectSymbol(ISymbol** ppSymbol)
{
    return (*m_DLL.m_pSelectSymbol)(ppSymbol);
}

}