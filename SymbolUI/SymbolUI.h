#if !defined(SYMBOLUI_INCLUDED_)
#define SYMBOLUI_INCLUDED_

#include "..\\Include\\InterfaceDisplay.h"

namespace easymap
{

class symboluidll
{
typedef bool (__stdcall _SetMainWnd)(HWND wnd);
typedef bool (__stdcall _SelectSymbol)(ISymbol** ppSymbol);

private:
    HINSTANCE m_hInstance;
    _SetMainWnd* m_pSetMainWnd;
    _SelectSymbol* m_pSelectSymbol;

    static symboluidll m_DLL;

private:
    symboluidll();
    symboluidll(const symboluidll& dll);
    symboluidll& operator=(const symboluidll& rhs);

public:
    ~symboluidll();

public:
    static bool loaddll(const char* dllfile);
    static void releasedll();
    static HINSTANCE getDLLHandle();
    static bool SetMainWnd(HWND wnd);
    static bool SelectSymbol(ISymbol** ppSymbol);
};

}

#endif
