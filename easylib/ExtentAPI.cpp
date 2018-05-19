#include "ExtentAPI.h"

namespace easymap
{

namespace extentapi
{

//  msimg32
//----------------------------------------------------------------
msimg32 msimg32::m_DLL;

msimg32::msimg32()
{
    m_hMsimg32 = ::LoadLibrary("msimg32.dll");
    if (m_hMsimg32)
    {
        m_pAlphaBlend = (api_alphablend*)::GetProcAddress(m_hMsimg32,
            "AlphaBlend");
        m_pTransparentBlt = (api_transparentblt*)::GetProcAddress(m_hMsimg32,
            "TransparentBlt");
    }
    else
    {
        m_pTransparentBlt = NULL;
        m_pAlphaBlend = NULL;
    }
}

msimg32::~msimg32()
{
    if (m_hMsimg32) ::FreeLibrary(m_hMsimg32);
}

HINSTANCE msimg32::getDLLHandle()
{
    return msimg32::m_DLL.m_hMsimg32;
}

BOOL msimg32::AlphaBlend(HDC hdcDest, int nXOriginDest,
    int nYOriginDest, int nWidthDest, int nHeightDest, HDC hdcSrc,
    int nXOriginSrc, int nYOriginSrc, int nWidthSrc, int nHeightSrc,
    BLENDFUNCTION blendFunction)
{
    return (*m_DLL.m_pAlphaBlend)(hdcDest, nXOriginDest, nYOriginDest,
        nWidthDest, nHeightDest, hdcSrc, nXOriginSrc, nYOriginSrc,
        nWidthSrc, nHeightSrc, blendFunction);
}

BOOL msimg32::TransparentBlt(HDC hdcDest, int nXOriginDest,
    int nYOriginDest, int nWidthDest, int nHeightDest, HDC hdcSrc,
    int nXOriginSrc, int nYOriginSrc, int nWidthSrc, int nHeightSrc,
    UINT crTransparent)
{
    return (*m_DLL.m_pTransparentBlt)(hdcDest, nXOriginDest, nYOriginDest,
        nWidthDest, nHeightDest, hdcSrc, nXOriginSrc, nYOriginSrc,
        nWidthSrc, nHeightSrc, crTransparent);
}

//----------------------------------------------------------------

}
}