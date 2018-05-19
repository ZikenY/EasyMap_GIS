#include "CommonInclude.h"
#include "Plot3D.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CPlot3D)

#ifndef __sqr
#define __sqr(a) (a*a)
#endif

CPlot3D::CPlot3D()
{
    INIT_REFCOUNT

    m_Azim_View = 0;
    m_Azim_Obs = -180;

    m_Elev_View = -30;
    m_Elev_Obs = 30;

    m_Roll_View = 0;
    m_Dist_Obs = 500;
    m_Dist_Proj = 500;

    m_ScreenWidth = 0;
    m_ScreenHeight = 0;

    m_Changed = true;
}

bool __stdcall CPlot3D::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CPlot3D")))
    {
        *pp = this;
    }
    else
    {
        *pp = NULL;
        return false;
    }

    static_cast<IObj*>(*pp)->_AddRef();
    return true;
}

bool __stdcall CPlot3D::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj))
        return false;
    assert(!*ppObj);

    CPlot3DPtr pClonedPlot3D = new CPlot3D;

    pClonedPlot3D->m_Azim_Obs = m_Azim_Obs;
    pClonedPlot3D->m_Elev_Obs = m_Elev_Obs;
    pClonedPlot3D->m_Dist_Obs = m_Dist_Obs;

    pClonedPlot3D->m_Azim_View = m_Azim_View;
    pClonedPlot3D->m_Elev_View = m_Elev_View;
    pClonedPlot3D->m_Roll_View = m_Roll_View;
    pClonedPlot3D->m_Dist_Proj = m_Dist_Proj;

    pClonedPlot3D->m_ScreenWidth = m_ScreenWidth;
    pClonedPlot3D->m_ScreenHeight = m_ScreenHeight;

    pClonedPlot3D->_obs = _obs;
    pClonedPlot3D->_sinpsi = _sinpsi;
    pClonedPlot3D->_cospsi = _cospsi;
    pClonedPlot3D->_sinfi = _sinfi;
    pClonedPlot3D->_cosfi = _cosfi;
    pClonedPlot3D->_sinteta = _sinteta;
    pClonedPlot3D->_costeta = _costeta;
    pClonedPlot3D->_costeta_cospsi = _costeta_cospsi;
    pClonedPlot3D->_sinteta_sinfi_sinpsi = _sinteta_sinfi_sinpsi;
    pClonedPlot3D->_costeta_sinpsi = _costeta_sinpsi;
    pClonedPlot3D->_sinteta_sinfi_cospsi = _sinteta_sinfi_cospsi;
    pClonedPlot3D->_sinteta_cosfi = _sinteta_cosfi;
    pClonedPlot3D->_cosfi_sinpsi = _cosfi_sinpsi;
    pClonedPlot3D->_cosfi_cospsi = _cosfi_cospsi;
    pClonedPlot3D->_sinteta_cospsi = _sinteta_cospsi;
    pClonedPlot3D->_costeta_sinfi_sinpsi = _costeta_sinfi_sinpsi;
    pClonedPlot3D->_sinteta_sinpsi = _sinteta_sinpsi;
    pClonedPlot3D->_costeta_sinfi_cospsi = _costeta_sinfi_cospsi;
    pClonedPlot3D->_costeta_cosfi = _costeta_cosfi;
    pClonedPlot3D->m_Changed = m_Changed;

    *ppObj = pClonedPlot3D._p();
    (*ppObj)->_AddRef();
    return true;
}

dword __stdcall CPlot3D::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_Azim_Obs);
    ps->Write(m_Elev_Obs);
    ps->Write(m_Dist_Obs);
    ps->Write(m_Azim_View);
    ps->Write(m_Elev_View);
    ps->Write(m_Roll_View);
    ps->Write(m_Dist_Proj);
    ps->Write(m_ScreenWidth);
    ps->Write(m_ScreenHeight);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CPlot3D::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Read(m_Azim_Obs);
    ps->Read(m_Elev_Obs);
    ps->Read(m_Dist_Obs);
    ps->Read(m_Azim_View);
    ps->Read(m_Elev_View);
    ps->Read(m_Roll_View);
    ps->Read(m_Dist_Proj);
    ps->Read(m_ScreenWidth);
    ps->Read(m_ScreenHeight);

    m_Changed = true;

    return pStream->GetPos() - oldpos;
}

void CPlot3D::SetScreenBound(const double width, const double height)
{
    m_ScreenWidth = width;
    m_ScreenHeight = height;

    m_Changed = true;
}

void CPlot3D::GetScreenBound(double& width, double& height) const
{
    width = m_ScreenWidth;
    height = m_ScreenHeight;
}

void CPlot3D::SetCameraAzimuth(const double& value)
{
    m_Azim_Obs = value;

    m_Changed = true;
}

void CPlot3D::GetCameraAzimuth(double& value) const
{
    value = m_Azim_Obs;
}

void CPlot3D::SetCameraElevation(const double& value)
{
    m_Elev_Obs = value;

    m_Changed = true;
}

void CPlot3D::GetCameraElevation(double& value) const
{
    value = m_Elev_Obs;
}

void CPlot3D::SetCameraDistance(const double& value)
{
    m_Dist_Obs = value;

    m_Changed = true;
}

void CPlot3D::GetCameraDistance(double& value) const
{
    value = m_Dist_Obs;
}

void CPlot3D::SetViewAzimuth(const double& value)
{
    m_Azim_View = value;

    m_Changed = true;
}

void CPlot3D::GetViewAzimuth(double& value) const
{
    value = m_Azim_View;
}

void CPlot3D::SetViewElevation(const double& value)
{
    m_Elev_View = value;

    m_Changed = true;
}

void CPlot3D::GetViewElevation(double& value) const
{
    value = m_Elev_View;
}

void CPlot3D::SetViewRoll(const double& value)
{
    m_Roll_View = value;

    m_Changed = true;
}

void CPlot3D::GetViewRoll(double& value) const
{
    value = m_Roll_View;
}

void CPlot3D::SetProjPlaneDistance(const double& value)
{
    m_Dist_Proj = value;

    m_Changed = true;
}

void CPlot3D::GetProjPlaneDistance(double& value) const
{
    value = m_Dist_Proj;
}

bool CPlot3D::SetCameraPosition(const WKSPointZ campos)
{
    double elev, azim;

    m_Dist_Obs = ::sqrt(__sqr(campos.x) + __sqr(campos.y) + __sqr(campos.z));
    if (0 == m_Dist_Obs) return false;

    elev = ::asin(campos.z / m_Dist_Obs);

    m_Elev_Obs = elev / PI_RAD;

    azim = ::cos(elev);
    if (0 == azim) return false;
    azim = (campos.x / m_Dist_Obs) / azim;
    if ((1 <= azim) || (-1 >= azim)) return false;

    if (0 > campos.y)
    {
        m_Azim_Obs = -(PI + ::asin(azim)) / PI_RAD;
    }
    else
    {
        m_Azim_Obs = ::asin(azim) / PI_RAD;
    }


    m_Changed = true;
    return true;
}

void CPlot3D::GetCameraPosition(WKSPointZ& campos) const
{
    // The coordinates of camera position can be calculated from
    // spherical coordinates (for an East-North-Up 3D-world):
    // (I'm letting the user to change the camera's Azim, Elev and Dist
    // in another module)
    campos.x = m_Dist_Obs * ::cos(m_Elev_Obs * PI_RAD) * ::sin(m_Azim_Obs * PI_RAD);
    campos.y = m_Dist_Obs * ::cos(m_Elev_Obs * PI_RAD) * ::cos(m_Azim_Obs * PI_RAD);
    campos.z = m_Dist_Obs * ::sin(m_Elev_Obs * PI_RAD);
}

void CPlot3D::TransformPoint(const double& pos3d_x, const double& pos3d_y, const double& pos3d_z,
    double& plane_x, double& plane_y)
{

    if (m_Changed)
    {
        //calculate the camera position
        this->GetCameraPosition(_obs);

        // If we want the user to look at the origin of the world system
        // we should set Azim, Elev_View accordingly elsewhere
        _sinpsi      = ::sin(-m_Azim_View * PI_RAD);
        _cospsi      = ::cos(-m_Azim_View * PI_RAD);
        _sinfi       = ::sin(m_Elev_View * PI_RAD);
        _cosfi       = ::cos(m_Elev_View * PI_RAD);
        _sinteta     = ::sin(m_Roll_View * PI_RAD);
        _costeta     = ::cos(m_Roll_View * PI_RAD);

        _costeta_cospsi          = _costeta*_cospsi;
        _sinteta_sinfi_sinpsi    = _sinteta*_sinfi*_sinpsi;
        _costeta_sinpsi          = _costeta*_sinpsi;
        _sinteta_sinfi_cospsi    = _sinteta*_sinfi*_cospsi;
        _sinteta_cosfi           = _sinteta*_cosfi;
        _cosfi_sinpsi            = _cosfi*_sinpsi;
        _cosfi_cospsi            = _cosfi*_cospsi;
        _sinteta_cospsi          = _sinteta*_cospsi;
        _costeta_sinfi_sinpsi    = _costeta*_sinfi*_sinpsi;
        _sinteta_sinpsi          = _sinteta*_sinpsi;
        _costeta_sinfi_cospsi    = _costeta*_sinfi*_cospsi;
        _costeta_cosfi           = _costeta*_cosfi;

        m_Changed = false;
    }

// Steps 1 and 2 - Moving the origin of the world system to the
// origin of the projection system and
// rotating the world system by Azim, Elev e Roll_View
    double Xt1, Yt1, Zt1;
    Xt1 = (pos3d_x - _obs.x) * (_costeta_cospsi - _sinteta_sinfi_sinpsi) +
      (pos3d_y - _obs.y) * (_costeta_sinpsi + _sinteta_sinfi_cospsi) +
      (pos3d_z - _obs.z) * (-_sinteta_cosfi);

    Yt1 = (pos3d_x - _obs.x) *(-_cosfi_sinpsi) + (pos3d_y - _obs.y) *  _cosfi_cospsi +
      (pos3d_z - _obs.z) * _sinfi;

    Zt1 = (pos3d_x - _obs.x) * (_sinteta_cospsi + _costeta_sinfi_sinpsi) +
      (pos3d_y - _obs.y) * (_sinteta_sinpsi - _costeta_sinfi_cospsi) +
      (pos3d_z - _obs.z) * _costeta_cosfi;

// Step 3 - Rotating the resulting system to fit the projection system
// The following attributions are equivalent to
// rotate -90 degrees around the X-axis
    double Xt, Yt, Zt;
    Xt =  Xt1;
    Yt = -Zt1;
    Zt =  Yt1;

// Step 4 - making the 3D perspective projection ...
    if (fabs(Zt) < 1e-6)
    {
        plane_x = plane_y = 0;
    }
    else
    {
        plane_x = m_Dist_Proj * Xt / Zt;
        plane_y = m_Dist_Proj * Yt / Zt;
    };

// Let's center the image on the plot component
    plane_x += m_ScreenWidth / 2;
    plane_y += m_ScreenHeight / 2;
}

}
