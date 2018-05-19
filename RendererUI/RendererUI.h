#if !defined(RENDERERUI_INCLUDED_)
#define RENDERERUI_INCLUDED_

#include "..\\Include\\InterfaceLayerAgent.h"

namespace easymap
{

class rendereruidll
{
typedef bool (__stdcall _SetMainWnd)(HWND wnd);
typedef bool (__stdcall _SelectRenderer)(IVectorLayerAgent* pVectorLayerAgent);

private:
    HINSTANCE m_hInstance;
    _SetMainWnd* m_pSetMainWnd;
    _SelectRenderer* m_pSelectRenderer;

    static rendereruidll m_DLL;

private:
    rendereruidll();
    rendereruidll(const rendereruidll& dll);
    rendereruidll& operator=(const rendereruidll& rhs);

public:
    ~rendereruidll();

public:
    static bool loaddll(const char* dllfile);
    static void releasedll();
    static HINSTANCE getDLLHandle();
    static bool SetMainWnd(HWND wnd);
    static bool SelectRenderer(IVectorLayerAgent* pVectorLayerAgent);
};

}

#endif
