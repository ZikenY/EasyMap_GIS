#include "MathLib.h"
#include <math.h>

namespace easymap
{

namespace mathlib
{

const double PI = 3.1415926535897932385;
const double INVALIDSLOPE = -999999.f;

//运算精度
class CMathLibPrecision
{
public:
    CMathLibPrecision()
    {
        m_Precision = 0.000000001;
    };
    double m_Precision;
}_MathLibPrecision;

void SetMathLibPrecision(const double& precision)
{
    _MathLibPrecision.m_Precision = precision;
}
void GetMathLibPrecision(double& precision)
{
    precision = _MathLibPrecision.m_Precision;
}

//当前运算精度
#define PRECISION (_MathLibPrecision.m_Precision)

const dword _wkspoint_array_count = 10000;
static WKSPoint _wkspoint_array[_wkspoint_array_count];

//dauglas压缩
void _dauglas(WKSPoint *b, WKSPoint *e, double tol2)
{
    if (e-b < 2) return;
    double d,t;
    WKSPoint *s,*i;
    t = WKSLine(b[0], e[0]).dis2(b[1]);
    for (s=i=b+1; ++i<e;)
    {
        d = WKSLine(b[0], e[0]).dis2(*i);
        if(d > t)
        {
            t = d;
            s = i;
        }
    }
    if (t <= tol2)
    {
        for (i=b; ++i<e;) *i = *b;
        return;
    }
    _dauglas(b,s,tol2);
    _dauglas(s,e,tol2);
}

//描述：采用dauglas压缩法压缩一条折线
void geo_path::dauglas(double tol2)
{
    if (size()<3) return;

    _dauglas(&front(), &back(), tol2);
    remove_invalid(0);
}

//描述：删除一些结构上，几何上无效的东西
bool geo_path::remove_invalid(double tol2)
{
    if (empty()) return false;
/*
    WKSPoint pt = back();

    WKSPoint *p = begin();
    for (int i = size(); --i > 0; ++p)
    {
        if (p[0].dis2(p[1]) <= tol2) p[1] = p[0];
    }

    back() = pt;

//    erase(std::unique(begin(), end()), end());
*/

    std::vector<WKSPoint>::iterator it = end();
    while (it != begin())
    {
        std::vector<WKSPoint>::iterator tmp = --it;
        if ((*it).dis2(*tmp) <= tol2) erase(tmp);
    }

    return size() >= 2;
}

bool geo_path::parallel(geo_path& parallel, const double& distance) const
{
    parallel.resize(size());
    return GetParallelLine(&(*this)[0], size(), distance, &parallel[0]);
}

bool geo_path::neat(const dword depth)
{
    if (size() < 3) return false;

    std::vector<WKSPoint>::iterator it = begin();
    while (it < end() - 3)
    {
        double Slope1 = LineSlope(*it, *(it+1));
        double Slope2 = LineSlope(*(it+1), *(it+2));
        if (fabs(Slope1-Slope2) < PRECISION)
        {
            //跟下条线平行
            erase(it+1);
        }
        else
        {
            bool cross, para;
            WKSPoint intersect;
            cross = GetRelationTwoLine(*it, *(it+1), *(it+2), *(it+3),
                intersect, para);
            if (cross)
            {
                //跟下下条线相交
                *(it+1) = intersect;
                erase(it+2);
            }
        }
        it++;
    }

    return true;
}


bool solve(double a, double b, double c, double &t1, double &t2)
{
    double b2, qac, delta;
    b2 = b*b;
    qac = 4*a*c;
    delta = b2 - qac;
    if (delta < 0) return false;
    delta = sqrt(delta);
    t1 = (-b - delta)/2/a;
    t2 = (-b + delta)/2/a;

    return true;
}

bool PointEqual(const WKSPoint& point1, const WKSPoint& point2)
{
    if ((fabs(point1.x - point2.x) < PRECISION)
        && (fabs(point1.y - point2.y) < PRECISION))
    {
        return true;
    }
    else
    {
        return false;
    }
}

//平面上任意两点的距离
double Distance(const WKSPoint& point1, const WKSPoint& point2)
{
    double tmp = __sqr(point1.x - point2.x) + __sqr(point1.y - point2.y);
    if (fabs(tmp) < PRECISION)
    {
        return 0;
    }
    else
    {
        return sqrt(tmp);
    }
}

//给定两点，计算一第一点为原点，向右的水平方向为参考零方向，第二点与第一点连线与参考方向的夹角
double ComputeAngle(const WKSPoint& PointCen, const WKSPoint& PointMove)
{
    double Orignx, Origny, x, y, x1, y1;
    double Angle;

    Orignx = PointCen.x;
    Origny = PointCen.y;
    x1 = PointMove.x;
    y1 = PointMove.y;

    x = x1 - Orignx;
    y = y1 - Origny;

    y = -y;

    if( fabs(x) < PRECISION && fabs(y) < PRECISION ) return(0);
    if( fabs(x) < PRECISION && y > 0 ) return( PI*1.5 );
    if( fabs(x) < PRECISION && y < 0 ) return( PI/2 );

    Angle  = atan(y/x);

    if( x > 0  && y > 0) return (2*PI+Angle);
    if( x < 0 && y > 0)  return( PI+Angle);
    if( x < 0 && y < 0) return( PI+Angle);
    if( x > 0 && y < 0) return( Angle);

    return Angle;
}

//给定两点，计算一第一点为原点，向右的水平方向为参考零方向，第二点与第一点连线与参考方向的夹角
double ComputeAngleInverse(const WKSPoint& PointCen, const WKSPoint& PointMove)
{
    float Orignx,Origny,x,y,x1,y1;
    double Angle;

    Orignx = PointCen.x;
    Origny = PointCen.y;
    x1 = PointMove.x;
    y1 = PointMove.y;

    x = x1 - Orignx;
    y = y1 - Origny;

    if( fabs(x) < PRECISION && fabs(y) < PRECISION ) return(0);
    if( fabs(x) < PRECISION && y > 0 ) return( PI/2 );
    if( fabs(x) < PRECISION && y < 0 ) return( PI*1.5 );

    Angle  = atan(y/x);

    if( x > 0  && y > 0) return Angle;
    if( x < 0 && y > 0)  return( PI+Angle);
    if( x < 0 && y < 0) return( PI+Angle);
    if( x > 0 && y < 0) return( 2*PI+Angle);

    return Angle;
}

double ComputeTwoLineAngle(const WKSPoint& Line1Str, const WKSPoint& Line1End,
    const WKSPoint& Line2Str, const WKSPoint& Line2End)
//计算两条直线的夹角，这里角度的计算是以交点为原点，以交点和其中一条直线的另一个顶点的连线为X轴
//角度在0~360之间
{
       WKSPoint CrossPoint,Line1Point,Line2Point;
      //这说明两条直线的交点已知， 不需要求交点
      if( fabs( Line1End.x - Line2Str.x ) < PRECISION && fabs( Line1End.y - Line2Str.y ) < PRECISION )
      {
         CrossPoint.x = Line1End.x;
         CrossPoint.y = Line1End.y;
      }
     else
     {
        LineIntersectPoint( Line1Str,Line1End, Line2Str,Line2End,CrossPoint);
     }

     //排除直线的四个端点中有相同的因素
     if( fabs( Line1Str.x - CrossPoint.x ) < PRECISION && fabs( Line1Str.y - CrossPoint.y ) < PRECISION )  
     {
       Line1Point.x = Line1End.x ;
       Line1Point.y = Line1End.y ;
     }
     else if( fabs( Line1End.x - CrossPoint.x ) < PRECISION && fabs( Line1End.y - CrossPoint.y ) < PRECISION )  
     {
       Line1Point.x = Line1Str.x ;
       Line1Point.y = Line1Str.y ;
     }
     else
     {
        Line1Point.x = Line1End.x ;
        Line1Point.y = Line1End.y ;
     }

     if( fabs( Line2Str.x - CrossPoint.x ) < PRECISION && fabs( Line2Str.y - CrossPoint.y ) < PRECISION )  
     {
       Line2Point.x = Line2End.x ;
       Line2Point.y = Line2End.y ;
     }
     else if( fabs( Line2Str.x - CrossPoint.x ) < PRECISION && fabs( Line2Str.y - CrossPoint.y ) < PRECISION )   
     {
       Line2Point.x = Line2Str.x ;
       Line2Point.y = Line2Str.y ;
     }
     else
     {
        Line2Point.x = Line2End.x ;
        Line2Point.y = Line2End.y ;
     }

     double a,b,c,s,area;
     double Angle,SinAngle;

     a = Distance( CrossPoint,Line1Point );
     b = Distance( CrossPoint,Line2Point );
     c = Distance( Line2Point,Line1Point );
     s = ( a+b+c )/2;
     area = sqrt( s*(s-a)*(s-b)*(s-c) )/2;
     SinAngle = 2*area/(a*b); 

     Angle = asin( SinAngle );  //-pai/2 ~ pai/2

     return Angle;
}

double LineSlope(const WKSPoint& LineFrom, const WKSPoint& LineTo)
{
    if (fabs(LineTo.x - LineFrom.x) < PRECISION)
        return INVALIDSLOPE;

    return (LineTo.y - LineFrom.y) / (LineTo.x - LineFrom.x);
}

bool LineEqution(const WKSPoint& LineFrom, const WKSPoint& LineTo,
    double* const slope, double* const constant)
{
    double t;
    double x1 = LineFrom.x;
    double y1 = LineFrom.y;
    double x2 = LineTo.x;
    double y2 = LineTo.y;

    if(fabs(x1 - x2) < PRECISION)
    {
        *slope = INVALIDSLOPE;
        *constant = x1;
        return false;
    }

    t = *slope = (double)(y2 - y1) / (x2 - x1);
    *constant = (double)(y1 - x1*t);

    return true;
}

void VerticalLine(const WKSPoint& LineFrom, const WKSPoint& LineTo,
    const WKSPoint& point, WKSPoint& point1)
{
    double slope1, constant1;
    double slope, constant;

    LineEqution(LineFrom, LineTo, &slope1, &constant1);

    if (slope1 == INVALIDSLOPE)
    {
        point1.x = LineFrom.x;
        point1.y = point.y;     
        return;
    }

    else if (fabs(slope1) <= PRECISION)
    {
        point1.y = LineFrom.y;
        point1.x = point.x;             
        return;
    }

    double x = point.x;
    double y = point.y;
    slope = -1.f/slope1;
    constant = (double)y - slope*(double)x;

    double x1,y1;

    x1 = (constant - constant1) / (slope1 - slope);
    y1 = slope1*x1 + constant1;

    point1.x =  x1;
    point1.y =  y1;

    return;
}

//功能：获得多边形的外接矩形
bool GetPolygonMBR(const vector<WKSPoint>& points, WKSRect& mbr)
{
    dword count = points.size();
    if (0 > count) return false;

    WKSPoint pnt = points[0];
    mbr.left = pnt.x;
    mbr.right = pnt.x;
    mbr.top = pnt.y;
    mbr.bottom = pnt.y;

    for (dword i = 1; i < count; i++)
    {
        pnt = points[i];
        if (pnt.x < mbr.left)
        {
            mbr.left = pnt.x;
        }
        else if (pnt.x > mbr.right)
        {
            mbr.right = pnt.x;
        }
        if (pnt.y < mbr.bottom)
        {
            mbr.bottom = pnt.y;
        }
        else if (pnt.y > mbr.top)
        {
            mbr.top = pnt.y;
        }
    }

    return true;
}

//点到直线的距离
// LineFrom, LineTo 直线的两端点，point为给定的点
double PointToLine(const WKSPoint& LineFrom, const WKSPoint& LineTo,
    const WKSPoint& point)
{  
    double x1,y1,x2,y2,x,y;
    double distance;
    
    x1 = LineFrom.x;
    y1 = LineFrom.y;
    x2 = LineTo.x;
    y2 = LineTo.y;
    x = point.x;
    y = point.y;
  
    double dis = sqrt( (x1-x2)*(x1-x2) + ( y1 - y2 )*( y1-y2) );
    if( dis < PRECISION ) //直线上的两个点非常接近，可认为是同一个点
    {
        dis = sqrt( (x-x1)*(x-x1) + ( y - y1 )*( y - y1) );
        return dis;
    }
    if( fabs( x1-x2 ) < PRECISION )  //如果是垂直，则计算Y的值
    {
        distance = fabs( x-x1 );
        return distance;
    }
    if( fabs( y1-y2 ) < PRECISION )  //如果是水平，则计算X的值
    {
        distance = fabs( y-y1 );
        return distance;
    }
  
    double slope,constant;
    if( !LineEqution(  LineFrom, LineTo,&slope,&constant ) )
    {
        distance = fabs( x-x1 );
        return distance; 
    }

    distance = fabs( slope*x+constant-y)/sqrt( slope*slope+1);  
    return distance;                             
}

//判断点是否在线上
bool IsPointOnLine(const WKSPoint& LineFrom, const WKSPoint& LineTo,
    const WKSPoint& point)
{
    if( fabs( point.x-LineFrom.x ) < PRECISION  && fabs( point.y-LineFrom.y ) < PRECISION  )
        return true;
    else if( fabs( point.x-LineTo.x ) < PRECISION && fabs( point.y-LineTo.y ) < PRECISION  )
        return true;

    double slope,constant;
    if( !LineEqution( LineFrom,LineTo,&slope,&constant ) )        
    {
      if( fabs( point.x-LineFrom.x ) < PRECISION )
            return true;      
    }

    double y = point.x*slope+constant;
    if( fabs( y-point.y) < PRECISION )     //on line
    {
          return true;
    }
    return false;
}

//点到线段的距离
// LineFrom, LineTo 直线的两端点，point为给定的点
double PointToLineSegment(const WKSPoint& LineFrom, const WKSPoint& LineTo,
    const WKSPoint& point)
{
    double x1,y1,x2,y2,x,y;
    double distance;

    x1 = LineFrom.x;
    y1 = LineFrom.y;
    x2 = LineTo.x;
    y2 = LineTo.y;
    x = point.x;
    y = point.y;

    double dis =  Distance( LineTo,LineFrom );
    if( dis < PRECISION ) //直线上的两个点非常接近，可认为是同一个点
    {
         dis = sqrt( (x-x1)*(x-x1) + ( y - y1 )*( y - y1) );
        return dis;
    }
    double dis1 = Distance( point,LineTo );
    double dis2 = Distance( point,LineFrom );

    if( fabs( x1-x2 ) < PRECISION )  //如果是垂直，则计算Y的值
    {
         distance = fabs( x-x1 );
    }  
    else if( fabs( y1-y2 ) < PRECISION )  //如果是水平，则计算X的值
    {
         distance = fabs( y-y1 );
    }
    else 
    {
        double slope,constant;
        if( !LineEqution(  LineFrom, LineTo,&slope,&constant ) )
        {
            distance = fabs( x-x1 );
        }
        else
        {
            distance = fabs( slope*x+constant-y)/sqrt( slope*slope+1);  
        }
    }

    WKSPoint  point1;
    VerticalLine( LineFrom,LineTo, point, point1 );
    double minx = min( LineFrom.x,LineTo.x );
    double maxx = max( LineFrom.x,LineTo.x );
    double miny = min( LineFrom.y,LineTo.y );
    double maxy = max( LineFrom.y,LineTo.y );
    if( point1.x > maxx || point1.x < minx ||   //垂点不在线段上
        point1.y > maxy || point1.y < miny )
    {
          if( dis1 < dis2 ) return dis1;
          else return dis2;
    }
    else
    {
        if( dis1 < dis2 && dis1 < distance )
            return dis1;
        if( dis2 < dis1 && dis2 < distance )
            return dis2;
        if( distance < dis1 && distance < dis2 )
            return distance;
    }
    return distance;
}

//判断点是否在线段上
bool IsPointOnLineSegment(const WKSPoint& LineFrom, const WKSPoint& LineTo,
    const WKSPoint& point)
{
    if (fabs(point.x - LineFrom.x) < PRECISION && fabs(point.y - LineFrom.y) < PRECISION)
        return true;
    else if (fabs(point.x - LineTo.x) < PRECISION && fabs(point.y - LineTo.y) < PRECISION)
        return true;

    double dis = PointToLineSegment(LineFrom, LineTo, point);
    if (dis < PRECISION) return true;

    return true;
}

//判断点是否在一折线上
bool IsPointOnLineArray(const vector<WKSPoint>& points, const WKSPoint& point)
{
    if (1 >= points.size()) return false;

    for(dword i = 0; i < points.size() - 1; i++)
    {
        WKSPoint LineFrom = points[i];
        WKSPoint LineTo = points[i + 1];
        if (IsPointOnLineSegment(LineFrom, LineTo, point))
            return true;
    }
    return false;
}

bool VerticalPointsToLine(const WKSPoint& start, const WKSPoint& end, const double& distance,
    WKSPoint& pnt1, WKSPoint& pnt2)
{
    if (equal(start.x, end.x))
    {
        //垂直
        if (start.y > end.y)
        {
            pnt1.x = start.x + distance;
            pnt2.x = start.x - distance;
        }
        else
        {
            pnt1.x = start.x - distance;
            pnt2.x = start.x + distance;
        }

        pnt1.y = start.y;
        pnt2.y = start.y;
        return true;
    }

    else if (equal(start.y, end.y))
    {
        //平行
        if (start.x > end.x)
        {
            pnt1.y = start.y - distance;
            pnt2.y = start.y + distance;
        }
        else
        {
            pnt1.y = start.y + distance;
            pnt2.y = start.y - distance;
        }

        pnt1.x = start.x;
        pnt2.x = start.x;
        return true;
    }


    double angle = ComputeAngle(start, end);
    pnt1.x = distance*cos(PI/2-angle) + start.x;
    pnt1.y = distance*sin(PI/2-angle) + start.y;
    pnt2.x = distance*cos(-PI/2-angle) + start.x;
    pnt2.y = distance*sin(-PI/2-angle) + start.y;
    return true;
}

//线相交
bool LineIntersectPoint(const WKSPoint Line1Str, const WKSPoint Line1End,
    const WKSPoint Line2Str, const WKSPoint Line2End, WKSPoint &point)
{
      double a1,b1,a2,b2;

    LineEqution(Line1Str,Line1End,&a1,&b1 );
    LineEqution(Line2Str,Line2End,&a2,&b2 );         

    if(  fabs( a1 - a2 ) < PRECISION ) return false;

    if( fabs( Line1Str.x - Line1End.x  ) < PRECISION )
    {
       point.x =  Line1Str.x;
       double temp = (a2*Line1Str.x+b2);
       point.y =  temp ;
       return true;
    }
    else if( fabs( Line2Str.x - Line2End.x ) < PRECISION )
    {
       point.x =  Line2Str.x;
       double temp = (a1*Line2Str.x+b1);
       point.y =  temp ;
       return true;
    }

    point.x = (b2-b1)/(a1-a2);
    point.y = point.x*a1+b1 ;

    return true;
}

short LineIntersectPoint(const WKSPoint Line1Str, const WKSPoint Line1End,
    const double slope, const double constant, WKSPoint &point)
//线相交
 {
      double slope1,constant1;

    LineEqution(Line1Str,Line1End,&slope1,&constant1 );    

    if( fabs( slope1 - slope ) < PRECISION )   return -1;

    point.x = (constant-constant1)/( slope1-slope );
    point.y = slope1*(point.x)+constant1;    

    return 1;
}

//给定两条直线的斜率和常数，求交点
bool LineIntersectPoint(const double slope1, const double constant1,
    const double slope2, const double constant2, double* const x, double* const y)
{
    if(fabs(slope1 - slope2) < PRECISION ) return false;

    *x = (constant2-constant1) / ( slope1 - slope2);
    *y = slope1 * (*x) + constant1;
    return true;
}

bool GetRelationTwoLine(const WKSPoint LineFrom1, const WKSPoint LineTo1,
    const WKSPoint LineFrom2, const WKSPoint LineTo2, WKSPoint& IntersectPoint,
    bool& parallel)
{
    bool r = false;
    parallel = false;

    WKSRect rect1,rect2;
    rect1.left = min( LineFrom1.x ,LineTo1.x );
    rect1.top = min( LineFrom1.y ,LineTo1.y );
    rect1.right = max( LineFrom1.x ,LineTo1.x );
    rect1.bottom = max( LineFrom1.y ,LineTo1.y );
    rect2.left = min( LineFrom2.x ,LineTo2.x );
    rect2.top = min( LineFrom2.y ,LineTo2.y );
    rect2.right = max( LineFrom2.x ,LineTo2.x );
    rect2.bottom = max( LineFrom2.y ,LineTo2.y );
    if( rect1.left > rect2.right || 
        rect1.top > rect2.bottom ||
        rect2.left > rect1.right ||
        rect2.top > rect1.bottom )
    {
        r = false;
    }

    if (fabs(LineFrom1.x - LineTo1.x) < PRECISION && fabs(LineFrom1.y - LineTo1.y) < PRECISION)
    {
        IntersectPoint = LineFrom1;
        return r;
    }
    else if (fabs(LineFrom2.x - LineTo2.x) < PRECISION && fabs(LineFrom2.y - LineTo2.y) < PRECISION)
    {
        IntersectPoint = LineFrom2;
        return r;
    }

    if (fabs(LineFrom1.x - LineFrom2.x) < PRECISION && fabs(LineFrom1.y - LineFrom2.y) < PRECISION)
    {
        IntersectPoint = LineFrom1;
        return true;
    }
    else if (fabs(LineFrom1.x - LineTo2.x) < PRECISION && fabs(LineFrom1.y - LineTo2.y) < PRECISION)
    {
        IntersectPoint = LineFrom1;
        return true;
    }
    if (fabs(LineTo1.x - LineFrom2.x) < PRECISION && fabs(LineTo1.y - LineFrom2.y) < PRECISION)
    {
        IntersectPoint = LineTo1;
        return true;
    }
    else if (fabs(LineTo1.x - LineTo2.x) < PRECISION && fabs(LineTo1.y - LineTo2.y) < PRECISION)
    {
        IntersectPoint = LineTo1;
        return true;
    }

    double Slope1 = LineSlope(LineFrom1, LineTo1);
    double Slope2 = LineSlope(LineFrom2, LineTo2);

    if (fabs(Slope1-Slope2) < PRECISION)
    {
        //平行
        parallel = true;
        return false;
    }

    if (!LineIntersectPoint( LineFrom1, LineTo1,LineFrom2,LineTo2,IntersectPoint))
    {
        r = false;
    }

    double dis11 = Distance( LineFrom1,IntersectPoint );
    double dis12 = Distance( LineTo1,IntersectPoint );
    double dis21 = Distance( LineFrom2,IntersectPoint );
    double dis22 = Distance( LineTo2,IntersectPoint );  
    double dis1 = Distance( LineFrom1,LineTo1 );
    double dis2 = Distance( LineFrom2,LineTo2 );  
    if( fabs( dis1-dis11-dis12 ) <= PRECISION && fabs( dis2-dis21-dis22 ) <= PRECISION )
    {
        r = true;
    }

    return r;
}

//得到两个线目标（有多个线段组成）的关系，如相交，则返回交点
bool GetRelationTwoLineArray(const vector<WKSPoint>& line1, const vector<WKSPoint>& line2,
    vector<WKSPoint>& intersectpoints)
{
    if (line1.size() <= 0 || line2.size() <= 0) throw;

    WKSRect FirstRect,SecondRect;
    if (!GetPolygonMBR(line1, FirstRect)) throw;
    if (!GetPolygonMBR(line2, SecondRect)) throw;
    //如果mbr是分离的，则肯定两多边形是分离的。
    if(FirstRect.left > SecondRect.right || FirstRect.top > SecondRect.bottom ||
        FirstRect.right < SecondRect.left || FirstRect.bottom < SecondRect.top)
        return false;
    if(SecondRect.left > FirstRect.right || SecondRect.top > FirstRect.bottom ||
        SecondRect.right < FirstRect.left || SecondRect.bottom < FirstRect.top)
        return false;

    intersectpoints.clear();
    WKSPoint IntersectPoint;
    WKSPoint LineFrom1, LineTo1, LineFrom2, LineTo2;
    dword PointNum1 = line1.size();
    dword PointNum2 = line2.size();
    for (dword i = 0; i < PointNum1 - 1; i++)
    {
        LineFrom1 = line1[i];
        LineTo1 = line1[i + 1];
     
        for (dword j = 0; j <PointNum2 - 1; j++)
        {
            LineFrom2 = line2[j];
            LineTo2 = line2[j + 1];

            bool parallel;
            if (GetRelationTwoLine(LineFrom1, LineTo1, LineFrom2, LineTo2, IntersectPoint, parallel))
            {
                intersectpoints.push_back(IntersectPoint);
            }
        }
    }  

    if (0 < intersectpoints.size()) return true;

    return false;
}

long PointInPolygon(const WKSPoint& point, const vector<WKSPoint>& polygon)
{
    WKSPoint ptStart, ptEnd;
    double nDist;
    int nCross = 0, nTempCross;
    dword nNum = polygon.size();

    ptStart = polygon[0];

    for (dword i = 1; i < nNum; i++)
    {
        ptEnd = polygon[i];

        if (fabs(ptStart.x - ptEnd.x) <= PRECISION)//竖线
        {
            if (fabs(ptStart.y - ptEnd.y) <= PRECISION) continue; // 重复点
            if (fabs(ptStart.x - point.x) <= PRECISION &&
                ((point.y <= ptStart.y + PRECISION && point.y >= ptEnd.y - PRECISION) ||
                (point.y >= ptStart.y - PRECISION && point.y <= ptEnd.y + PRECISION))
                )
            {
                return 1;       // on boundary
            }
        }
        else
        {
            nTempCross = 2;
            if (((ptStart.x <= point.x && ptEnd.x >= point.x) ||
                (ptStart.x >= point.x && ptEnd.x <= point.x))
                )
            {
                if(fabs(ptStart.x - point.x) <= PRECISION || fabs(ptEnd.x - point.x) <= PRECISION)
                {
                    nTempCross = 1;
                }

                nDist = (point.y - ptStart.y) * (ptEnd.x - ptStart.x) -
                    (point.x - ptStart.x) * (ptEnd.y - ptStart.y);
                double tmp = ((ptEnd.y - ptStart.y) * (ptEnd.y - ptStart.y) -
                    (ptEnd.x - ptStart.x ) * ( ptEnd.x - ptStart.x));
                if (PRECISION < fabs(tmp))
                {
                    if (fabs(nDist * nDist / tmp) < PRECISION)
                    {
                        return 1;   //on boundary
                    }
                    else
                    {
                        if(nDist > 0.00001)
                        {
                            nTempCross = -nTempCross;
                        }
                        nCross += nTempCross;
                    }
                }
                else
                {
                    if(nDist > 0.00001)
                    {
                        nTempCross = -nTempCross;
                    }
                    nCross += nTempCross;
                }
            }
        }
        ptStart = ptEnd ;
    }
    return nCross;
}

bool GetParallelLine(const WKSPoint* const line, const dword pointcount, const double& distance,
    WKSPoint* const pParallel)
{
    if (!line || !pParallel || (2 > pointcount)) return false;

    WKSPoint start1, end1, start2, end2;
    if (2 == pointcount)
    {
        VerticalPointsToLine(line[0], line[1], distance, start1, start2);
        VerticalPointsToLine(line[1], line[0], distance, end2, end1);
        pParallel[0] = start1;
        pParallel[1] = end1;

        return true;
    }

    WKSPoint *tmppara = NULL;
    dword doublepointcount = 2*pointcount;
    if (_wkspoint_array_count < doublepointcount)
    {
        tmppara = new WKSPoint[doublepointcount];
    }
    else
    {
        tmppara = _wkspoint_array;
    }

    dword pntcounttmp = 0;
    dword i = 0;
    for (; i < pointcount - 1; i++)
    {
        VerticalPointsToLine(line[i], line[i+1], distance, start1, start2);
        VerticalPointsToLine(line[i+1], line[i], distance, end2, end1);
        tmppara[pntcounttmp] = start1;
        tmppara[pntcounttmp+1] = end1;
        pntcounttmp += 2;
    }

    WKSPoint intersectpoint;

    dword parapntcount = 0;
    intersectpoint = tmppara[0];
    pParallel[parapntcount] = intersectpoint;
    parapntcount++;
    for (i = 0; i < pntcounttmp - 3; i++)
    {
        bool parallel;
        GetRelationTwoLine(tmppara[i], tmppara[i+1], tmppara[i+2], tmppara[i+3],
            intersectpoint, parallel);

        pParallel[parapntcount] = intersectpoint;
        parapntcount++;
        i++;
    }
    pParallel[parapntcount] = tmppara[pntcounttmp-1];
//    parapntcount += 1;

    if (_wkspoint_array_count < doublepointcount)
    {
        delete[] tmppara;
    }

    return true;
}

void RotatePI(WKSPoint &pnt, const WKSPoint &origin, const double& angle)
{
    RotatePIXY(pnt.x, pnt.y, origin.x, origin.y, angle);
}

void RotatePIXY(double &pnt_x, double &pnt_y, const double &origin_x,
    const double &origin_y, const double& angle)
{
    if (fabs(angle) < PRECISION)
        return;

    WKSPoint d;
    d.x = pnt_x - origin_x;
    d.y = pnt_y - origin_y;
    double cosx = cos(angle);
    double siny = sin(angle);
    pnt_x = d.x*cosx - d.y*siny + origin_x;
    pnt_y = d.y*cosx + d.x*siny + origin_y;
}

void RotateDegree(WKSPoint &pnt, const WKSPoint &origin, const double& degree)
{
    double angle = degree*PI*2.0/360.0;
    RotatePIXY(pnt.x, pnt.y, origin.x, origin.y, angle);
}

void RotateDegreeInvert(WKSPoint &pnt, const WKSPoint &origin, const double& degree)
{
    long _degree = long(degree * 10000);
    _degree = _degree % 3600000;
    double __degree = 360 - _degree / 10000;
    double angle = __degree*PI*2.0/360.0;
    RotatePIXY(pnt.x, pnt.y, origin.x, origin.y, angle);
}

long EnvelopeClipLine(const WKSRect& extent, double& x1, double& y1, double& x2, double& y2)
{
    bool from_in = PointInEnvelope(x1, y1, extent);
    bool to_in = PointInEnvelope(x2, y2, extent);
    if (from_in && to_in)
    {
        //整条线都在屏幕内，不用裁切
        return 4;
    }

    static WKSPoint from;
    from.x = x1;
    from.y = y1;
    static WKSPoint to;
    to.x = x2;
    to.y = y2;

    static WKSPoint left_bottom;
    left_bottom.x = extent.left;
    left_bottom.y = extent.bottom;
    static WKSPoint right_bottom;
    right_bottom.x = extent.right;
    right_bottom.y = extent.bottom;
    static WKSPoint right_top;
    right_top.x = extent.right;
    right_top.y = extent.top;
    static WKSPoint left_top;
    left_top.x = extent.left;
    left_top.y = extent.top;

    static WKSPoint intersect_left;
    static WKSPoint intersect_right;
    static WKSPoint intersect_top;
    static WKSPoint intersect_bottom;

    WKSPoint *pIntersect1 = NULL;
    WKSPoint *pIntersect2 = NULL;

    bool joe;
    bool has_bottom = GetRelationTwoLine(from, to, left_bottom, right_bottom, intersect_bottom, joe);
    bool has_right = GetRelationTwoLine(from, to, right_bottom, right_top, intersect_right, joe);
    bool has_top = GetRelationTwoLine(from, to, right_top, left_top, intersect_top, joe);
    bool has_left = GetRelationTwoLine(from, to, left_top, left_bottom, intersect_left, joe);

    //-------------------------------------------------------
    //注意现在没有考虑3、4个交点的情况
    if (has_bottom)
    {
        pIntersect1 = &intersect_bottom;
    }

    if (has_right)
    {
        if (pIntersect1)
        {
            pIntersect2 = &intersect_right;
        }
        else
        {
            pIntersect1 = &intersect_right;
        }
    }

    if (has_top)
    {
        if (pIntersect1)
        {
            pIntersect2 = &intersect_top;
        }
        else
        {
            pIntersect1 = &intersect_top;
        }
    }

    if (has_left)
    {
        if (pIntersect1)
        {
            pIntersect2 = &intersect_left;
        }
        else
        {
            pIntersect1 = &intersect_left;
        }
    }
    //-------------------------------------------------------

    if (pIntersect1 && pIntersect2)
    {
        //有两个交点
        double dist1 = Distance(from, *pIntersect1);
        double dist2 = Distance(from, *pIntersect2);
        if (dist1 < dist2)
        {
            x1 = (*pIntersect1).x;
            y1 = (*pIntersect1).y;
            x2 = (*pIntersect2).x;
            y2 = (*pIntersect2).y;
        }
        else
        {
            x1 = (*pIntersect2).x;
            y1 = (*pIntersect2).y;
            x2 = (*pIntersect1).x;
            y2 = (*pIntersect1).y;
        }

        return 3;
    }

    if (pIntersect1 && !pIntersect2)
    {
        //有一个交点
        if (from_in)
        {
            x2 = (*pIntersect1).x;
            y2 = (*pIntersect1).y;
            return 1;
        }

        x1 = (*pIntersect1).x;
        y1 = (*pIntersect1).y;
        return 2;
    }

    //整条线都在屏幕外，无交点
    return 0;
}

}

}