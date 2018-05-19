#if !defined(EXTENTAPI_INCLUDED_)
#define EXTENTAPI_INCLUDED_

#include "windows.h"

namespace easymap
{

namespace extentapi
{

// currentlly defined blend function
#if !defined(AC_SRC_OVER)
#define AC_SRC_OVER                 0x00
#endif

// alpha format flags
#if !defined(AC_SRC_NO_PREMULT_ALPHA)
#define AC_SRC_NO_PREMULT_ALPHA     0x01
#endif
#if !defined(AC_SRC_NO_ALPHA)
#define AC_SRC_NO_ALPHA             0x02
#endif
#if !defined(AC_DST_NO_PREMULT_ALPHA)
#define AC_DST_NO_PREMULT_ALPHA     0x10
#endif
#if !defined(AC_DST_NO_ALPHA)
#define AC_DST_NO_ALPHA             0x20
#endif

//这个单体类用来调用MSING32.dll中的API
class msimg32
{
typedef BOOL (WINAPI api_alphablend)(HDC, int, int, int, int, HDC, int,
    int, int, int, BLENDFUNCTION);
typedef BOOL (WINAPI api_transparentblt)(HDC, int, int, int, int, HDC, int,
    int, int, int, UINT);

private:
    HINSTANCE m_hMsimg32;
    api_alphablend* m_pAlphaBlend;
    api_transparentblt* m_pTransparentBlt;

    static msimg32 m_DLL;

private:
    msimg32();

    msimg32(const msimg32& dll);
    msimg32& operator=(const msimg32& rhs);

public:
    ~msimg32();

public:
    static HINSTANCE getDLLHandle();

    static BOOL AlphaBlend(
        HDC hdcDest,                // handle to destination DC
        int nXOriginDest,           // x-coord of upper-left corner
        int nYOriginDest,           // y-coord of upper-left corner
        int nWidthDest,             // destination width
        int nHeightDest,            // destination height
        HDC hdcSrc,                 // handle to source DC
        int nXOriginSrc,            // x-coord of upper-left corner
        int nYOriginSrc,            // y-coord of upper-left corner
        int nWidthSrc,              // source width
        int nHeightSrc,             // source height
        BLENDFUNCTION blendFunction // alpha-blending function
        );

    static BOOL TransparentBlt(
        HDC hdcDest,                // handle to destination DC
        int nXOriginDest,           // x-coord of destination upper-left corner
        int nYOriginDest,           // y-coord of destination upper-left corner
        int nWidthDest,             // width of destination rectangle
        int hHeightDest,            // height of destination rectangle
        HDC hdcSrc,                 // handle to source DC
        int nXOriginSrc,            // x-coord of source upper-left corner
        int nYOriginSrc,            // y-coord of source upper-left corner
        int nWidthSrc,              // width of source rectangle
        int nHeightSrc,             // height of source rectangle
        UINT crTransparent          // color to make transparent
        );
};

}
}

#endif