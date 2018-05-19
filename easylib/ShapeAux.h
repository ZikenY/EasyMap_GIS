#if !defined(SHAPEAUX_INCLUDED_)
#define SHAPEAUX_INCLUDED_

#include "SlimLayer.h"
#include "GroupLayer.h"
#include "shapefil.h"

namespace easymap
{

bool ImportShapeFile(const string& shapefile, const MapUnits mapunit,
    const double& basescale, const double& precision, const long indexlevel,
    const long annofield, const string& slimdir, CSlimLayerPtr& pSlimLayer);

bool ExportShapeFile(const CSlimLayerPtr pSL, const string& fn);

}

#endif
