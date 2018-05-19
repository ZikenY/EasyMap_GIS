#if !defined(GEOMETRYCOLUMNINFO_INCLUDED_)
#define GEOMETRYCOLUMNINFO_INCLUDED_

#include "InterfaceGeometry.h"
#include "MapUnits.h"

namespace easymap
{

typedef struct
{
    //为了符号化等其它需要，要素层必须有统一的几何类型
    ShapeType ShpType;

    //0 - 普通要素； //1 - 注记
    long FeatureType;

    //坐标单位
    MapUnits MapUnit;

    //图层的原始比例尺，即1:1显示的时候所使用的显示比例尺
    double BaseScale;

    //图幅范围和坐标精度
    WKSRect DomainXY;
    double ToleranceXY;
    double MaxZ;
    double MinZ;
    double ToleranceZ;

    //索引参数
    dword SIParam1;
    dword SIParam2;
    double SIParam3;
}GeometryColumnInfo;

}

#endif