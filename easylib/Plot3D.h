#if !defined(PLOT3D_INCLUDED_)
#define PLOT3D_INCLUDED_

#include "CommonInclude.h"
#include "Windows.h"

namespace easymap
{

class CPlot3D;
typedef TSmartPtr<CPlot3D> CPlot3DPtr;

class CPlot3D : public IPersist
{
CLASS_NAME(CPlot3D)
PERSIST_DUMP(CPlot3D)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CPlot3D();
private:
    ~CPlot3D(){};

private:
    double m_Azim_Obs;
    double m_Elev_Obs;
    double m_Dist_Obs;

    double m_Azim_View;
    double m_Elev_View;
    double m_Roll_View;
    double m_Dist_Proj;

    double m_ScreenWidth;
    double m_ScreenHeight;

    //临时存储，用于避免重复运算
    WKSPointZ _obs;
    double _sinpsi;
    double _cospsi;
    double _sinfi;
    double _cosfi;
    double _sinteta;
    double _costeta;
    double _costeta_cospsi;
    double _sinteta_sinfi_sinpsi;
    double _costeta_sinpsi;
    double _sinteta_sinfi_cospsi;
    double _sinteta_cosfi;
    double _cosfi_sinpsi;
    double _cosfi_cospsi;
    double _sinteta_cospsi;
    double _costeta_sinfi_sinpsi;
    double _sinteta_sinpsi;
    double _costeta_sinfi_cospsi;
    double _costeta_cosfi;
    bool m_Changed;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    void SetScreenBound(const double width, const double height);
    void GetScreenBound(double& width, double& height) const;

    void SetCameraAzimuth(const double& value);
    void GetCameraAzimuth(double& value) const;
    void SetCameraElevation(const double& value);
    void GetCameraElevation(double& value) const;
    void SetCameraDistance(const double& value);
    void GetCameraDistance(double& value) const;
    void SetViewAzimuth(const double& value);
    void GetViewAzimuth(double& value) const;
    void SetViewElevation(const double& value);
    void GetViewElevation(double& value) const;
    void SetViewRoll(const double& value);
    void GetViewRoll(double& value) const;
    void SetProjPlaneDistance(const double& value);
    void GetProjPlaneDistance(double& value) const;

    bool SetCameraPosition(const WKSPointZ campos);
    void GetCameraPosition(WKSPointZ& campos) const;

    void TransformPoint(
        const double& pos3d_x,
        const double& pos3d_y,
        const double& pos3d_z,
        double& plane_x,
        double& plane_y
        );
};

CLASS_FACTORY(CPlot3D)
}

#endif