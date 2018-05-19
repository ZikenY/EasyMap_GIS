#if !defined(INTERFACEBITMAPLAYER_INCLUDED_)
#define INTERFACEBITMAPLAYER_INCLUDED_

#include "InterfaceLayer.h"

namespace easymap
{

struct DOMInfo
{
    MapUnits unit;
    double Xr;
    double Yc;
    double Dr;
    double Dc;
};

class IBitmapLayer;
typedef TSmartPtr<IBitmapLayer> IBitmapLayerPtr;

class IBitmapLayer : public ILayer
{
public:
    virtual bool __stdcall LoadBmpFile(const char* const filename, const bool loaddom) = 0;
    virtual const char* __stdcall GetBmpFileName() const = 0;
    virtual void __stdcall SetDomInfo(const DOMInfo& dominfo) = 0;
    virtual void __stdcall GetDomInfo(DOMInfo& dominfo) const = 0;
};

}

#endif
