#if !defined(MAPUNITS_INCLUDED_)
#define MAPUNITS_INCLUDED_

namespace easymap
{

//坐标单位（米、千米、英里...etc）
typedef dword MapUnits;
const MapUnits UNIT_DEGREE          = 0;         //“度”用于没有投影的经纬度坐标系
const MapUnits UNIT_M               = 1;
const MapUnits UNIT_KM              = 2;
const MapUnits UNIT_CM              = 3;
const MapUnits UNIT_MM              = 4;
const MapUnits UNIT_MILE            = 5;
const MapUnits UNIT_NAUTICALMILE    = 6;
const MapUnits UNIT_INCH            = 7;
const MapUnits UNIT_FOOT            = 8;

}

#endif
