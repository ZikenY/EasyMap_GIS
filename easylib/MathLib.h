#if !defined(MATHLIB_INCLUDED_)
#define MATHLIB_INCLUDED_

//主要用于平面几何的基础数学函数库

#pragma warning(disable: 4786)
#include <vector>
#include <algorithm>
using namespace std;

#include "..\\include\\WKSInclude.h"
#include "..\\include\\BasicType.h"

namespace easymap
{

namespace mathlib
{

inline bool equal(double x, double y)
{
    unsigned __int64 *px = (unsigned __int64*)&x;
    unsigned __int64 *py = (unsigned __int64*)&y;

    unsigned __int64 d = (px[0] | py[0]);
    d = d>>56;
    if((d & 127) == 0) //x, y均为绝对值很小的数
    {
        return true;
    }
    unsigned __int64 X = px[0]>>20;
    unsigned __int64 Y = py[0]>>20;
    if(X == Y) return true;
    if(X < Y)
    {
        return X + 1 == Y;
    }
    else
    {
        return Y + 1 == X;
    }
    //可以加入判断低位的语句
//    return true;
}

inline bool dinky(const double a)
{
    return equal(a, 0.0f);
}

inline long Round(const double& a)
{
    long i = (long)a;
    long d = long((a - i)*10);
    if (4 < d)
    {
        i++;
    }
    return i;
}

//用来实现一些线的算法
class geo_path : public std::vector<WKSPoint>
{
public:
    //道格拉斯压缩
    void dauglas(double tol2);
    //去掉无效的点，无效点就是临近的重复点
    bool remove_invalid(double tol2);
    //求平行线
    bool parallel(geo_path& parallel, const double& distance) const;
    //整理一下
    bool neat(const dword depth = 1);
};


//解二次方程
bool solve(double a, double b, double c, double &t1, double &t2);


//这里定义了两个几何对象的空间关系，
typedef short RelationshipType;
const RelationshipType RELATIONTYPE_DISJOINT    = (RelationshipType)0;  //分离
const RelationshipType RELATIONTYPE_CROSSES     = (RelationshipType)1;  //穿越
const RelationshipType RELATIONTYPE_OVERLAPS    = (RelationshipType)2;  //叠置
const RelationshipType RELATIONTYPE_WITHIN      = (RelationshipType)3;  //第一个在第二个内部
const RelationshipType RELATIONTYPE_CONTENTS    = (RelationshipType)4;  //第一个包含第二个
const RelationshipType RELATIONTYPE_EQUALS      = (RelationshipType)5;  //相等
//const RELATIONTYPE_TOUCHES      = (RelationshipType)6;  //接触


//※※※※※※※※※※※※※※※※※※※※※※※※
//※※※※※※※※※※※※※※※※※※※※※※※※

//-----------------------------------------------------------------------------------
//判断两点是否相等
//参数：point1, point2 为给定的两点
//返回值：％￥￥＃！～（×※
//-----------------------------------------------------------------------------------
bool PointEqual(const WKSPoint& point1, const WKSPoint& point2);

//-----------------------------------------------------------------------------------
//平面上任意两点的距离
//参数：point1, point2 为给定的两点
//返回值：距离
//-----------------------------------------------------------------------------------
double Distance(const WKSPoint& point1, const WKSPoint& point2);

//-----------------------------------------------------------------------------------
//给定两点，计算以第一点为原点，向右的水平方向为参考零方向，第二点与第一点连线与参考方向的夹角
//注：角度范围0~359
//返回值： -9999.f 有错
//         0~359 正常 
//-----------------------------------------------------------------------------------
double ComputeAngle(const WKSPoint& PointCen, const WKSPoint& PointMove);

//-----------------------------------------------------------------------------------
//给定两点，计算以第一点为原点，向右的水平方向为参考零方向，第二点与第一点连线与参考方向的夹角
//注：角度范围0~359
//返回值： -9999.f 有错
//         0~359 正常 
//-----------------------------------------------------------------------------------
double ComputeAngleInverse(const WKSPoint& PointCen, const WKSPoint& PointMove);

//-----------------------------------------------------------------------------------
//计算两条直线的夹角，这里角度的计算是以交点为原点，以交点和其中一条直线的另一个顶点的连线为X轴
//角度在0~360之间
//-----------------------------------------------------------------------------------
double ComputeTwoLineAngle(const WKSPoint& Line1Str, const WKSPoint& Line1End,
    const WKSPoint& Line2Str, const WKSPoint& Line2End);

//-----------------------------------------------------------------------------------
//功能：求直线方程,给定两条直线的端点，求直线方程
//参数：slope 斜率,constant常数
//返回值： false 表示与Y轴平行，斜率为无穷大，赋值为-9999.f
//         true 正常  
//-----------------------------------------------------------------------------------
bool LineEqution(const WKSPoint& LineFrom, const WKSPoint& LineTo,
    double* const slope, double* const constant);

//-----------------------------------------------------------------------------------
//功能：求直线斜率
//参数：LineFrom, LineTo 为给定直线的两点
//返回值：平面上直线斜率
//说明：  如果斜率为无穷大，返回-9999.0 
//-----------------------------------------------------------------------------------
double LineSlope(const WKSPoint& LineFrom, const WKSPoint& LineTo);

//-----------------------------------------------------------------------------------
//功能：给定任意两点及第三点，计算第三点到前两点所构成的直线的垂足
//参数：LineFrom，LineTo为给定的两点，point为第三点，point1垂足
//返回值：无
//注： 这里以X为参考移动
//-----------------------------------------------------------------------------------
void VerticalLine(const WKSPoint& LineFrom, const WKSPoint& LineTo, const WKSPoint& point,
    WKSPoint& point1);

//-----------------------------------------------------------------------------------
//功能：获得多边形的外接矩形
//参数：
//返回值：成功true，失败false
//-----------------------------------------------------------------------------------
bool GetPolygonMBR(const vector<WKSPoint>& points, WKSRect& mbr);



//===================================================================================
//点和线之间的关系
//===================================================================================

//-----------------------------------------------------------------------------------
//功能：点到直线的距离
//参数： LineFrom, LineTo 直线的两端点，point为给定的点
//返回值：点到直线的距离
//-----------------------------------------------------------------------------------
double PointToLine(const WKSPoint& LineFrom, const WKSPoint& LineTo, const WKSPoint& point);

//-----------------------------------------------------------------------------------
//功能：判断点是否在直线上
//参数： LineFrom, LineTo 直线的两端点，point为给定的点
//返回值：点在直线上为TRUE
//        点不在直线上为FALSE 
//-----------------------------------------------------------------------------------
bool IsPointOnLine(const WKSPoint& LineFrom, const WKSPoint& LineTo,
    const WKSPoint& point);

//-----------------------------------------------------------------------------------
//功能：点到线段的距离
//参数： LineFrom, LineTo 直线的两端点，point为给定的点
//返回值：点到线段的距离
//-----------------------------------------------------------------------------------
double PointToLineSegment(const WKSPoint& LineFrom, const WKSPoint& LineTo,
    const WKSPoint& point);

//-----------------------------------------------------------------------------------
//功能：判断点是否在线段上
//参数： LineFrom, LineTo 直线的两端点，point为给定的点
//返回值：点在线段上为true
//        点不在线段上为false
//-----------------------------------------------------------------------------------
bool IsPointOnLineSegment(const WKSPoint& LineFrom, const WKSPoint& LineTo,
    const WKSPoint& point);

//-----------------------------------------------------------------------------------
//功能：判断点是否在折线上
//参数：pPointArray是折线，point为给定的点
//返回值：点在线段上为TRUE
//        点不在线段上为FALSE 
//-----------------------------------------------------------------------------------
bool IsPointOnLineArray(const vector<WKSPoint>& points, const WKSPoint& point);

//-----------------------------------------------------------------------------------
//功能：求垂直于线段起点的点
//参数：start, end是折线，distance为点到线的距离
//      pnt1、pnt2为求得的两个点，一左一右
//返回值：always true
//        
//-----------------------------------------------------------------------------------
bool VerticalPointsToLine(const WKSPoint& start, const WKSPoint& end, const double& distance,
    WKSPoint& pnt1, WKSPoint& pnt2);


//===================================================================================
//线和线之间的关系
//===================================================================================

//-----------------------------------------------------------------------------------
//功能：线相交
//参数：Line1Str,Line1End,Line2Str,Line2End,两条直线的端点， point为交点
//返回值：0  没有交点
//        1  有交点  
//-----------------------------------------------------------------------------------
bool LineIntersectPoint(const WKSPoint Line1Str, const WKSPoint Line1End,
    const WKSPoint Line2Str, const WKSPoint Line2End, WKSPoint &point);

//-----------------------------------------------------------------------------------
//功能：线相交
//参数：Line1Str,Line1End,直线的端点，slope,constant为直线的斜率,point为交点
//返回值：0  没有交点
//        1  有交点  
//-----------------------------------------------------------------------------------
short LineIntersectPoint(const WKSPoint Line1Str, const WKSPoint Line1End,
    const double slope, const double constant, WKSPoint &point);

//-----------------------------------------------------------------------------------
//功能：给定两条直线的斜率和常数，求交点
//参数：slope1为第一条直线的斜率，constant1为第一条直线的常数
//      slope2为第一条直线的斜率，constant2为第一条直线的常数
//返回值：false： 无交点
//        true： 有交点
//-----------------------------------------------------------------------------------
bool LineIntersectPoint(const double slope1, const double constant1, const double slope2,
    const double constant2, double* const x, double* const y);

//-----------------------------------------------------------------------------------
//功能：判断两线段的关系：相交，相离，如相交，则返回交点
//参数：LineFrom1，LineTo1为第一条直线的两个端点，LineFrom2，LineTo2为第二条直线的两个端点，
//    IntersectPoint为返回的交点
//返回值：TRUE: 相交，如有交点，则将交点返回
//        FALSE： 相离
//-----------------------------------------------------------------------------------
bool GetRelationTwoLine(const WKSPoint LineFrom1, const WKSPoint LineTo1,
    const WKSPoint LineFrom2, const WKSPoint LineTo2, WKSPoint& IntersectPoint,
    bool& parallel);

//-----------------------------------------------------------------------------------
//功能：得到两个线目标（有多个线段组成）的关系，如相交，则返回交点
//参数：pLineCoordArray1为第一线目标，pLineCoordArray1为第一线目标，IntersectPoint为返回的交点
//返回值：  true: 相交，如有交点，则将交点返回
//          false： 相离
//-----------------------------------------------------------------------------------
bool GetRelationTwoLineArray(const vector<WKSPoint>& line1, const vector<WKSPoint>& line2,
    vector<WKSPoint>& intersectpoints);


//-----------------------------------------------------------------------------------
//判断点是否在某一个多边形内.
//返回值：
//      1   在边界上
//      >1  在内部
//      0   在外部
//      <0  出错
//-----------------------------------------------------------------------------------
long PointInPolygon(const WKSPoint& point, const vector<WKSPoint>& polygon);

//-----------------------------------------------------------------------------------
//  注意这个要保证pParallel有足够的空间
//-----------------------------------------------------------------------------------
bool GetParallelLine(const WKSPoint* const line, const dword pointcount, const double& distance,
    WKSPoint* const pParallel);

//旋转2d点，弧度
void RotatePI(WKSPoint &pnt, const WKSPoint &origin, const double& angle);

//旋转2d点，弧度
void RotatePIXY(double &pnt_x, double &pnt_y, const double &origin_x,
    const double &origin_y, const double& angle);

//旋转2d点，角度
void RotateDegree(WKSPoint &pnt, const WKSPoint &origin, const double& degree);

//顺时针的
void RotateDegreeInvert(WKSPoint &pnt, const WKSPoint &origin, const double& degree);

//用envelope裁切line
//返回值：
//      0 - 整条线都在extent外面，没有交点
//      1 - from在里面，to在外面且被裁切
//      2 - to在里面，from在外面且被裁切
//      3 - 两个点都在外面，但有两个交点，from和to都被裁切
//      4 - 两个点都在里面，不用裁切
long EnvelopeClipLine(const WKSRect& extent, double& x1, double& y1, double& x2, double& y2);

//设置当前运算精度
inline void SetMathLibPrecision(const double& precision);
//取得当前运算精度
inline void GetMathLibPrecision(double& precision);


#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif

#ifndef min
#define min(a,b) (((a) < (b)) ? (a) : (b))
#endif

}

}

#endif
