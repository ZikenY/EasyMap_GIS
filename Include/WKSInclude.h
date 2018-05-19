#if !defined(WKSINCLUDE_INCLUDED_)
#define WKSINCLUDE_INCLUDED_

namespace easymap
{

struct WKSPoint
{
    double x;
    double y;

    inline WKSPoint() {};
    inline WKSPoint(const double& _x, const double& _y) {x = _x; y = _y;};
    inline WKSPoint(const WKSPoint& pnt) {x = pnt.x; y = pnt.y;};
    inline WKSPoint& operator=(const WKSPoint& rhs)
    {
        if (this == &rhs) return *this;
        x = rhs.x;
        y = rhs.y;
        return *this;
    };

    //距离的平方
    inline double dis2(const WKSPoint &o) const
    {
        double a = x - o.x, b = y - o.y;
        return a*a + b*b;
    };
};

struct WKSLine
{
    WKSPoint me[2];

    inline WKSLine() {};
    inline WKSLine(const WKSPoint &a, const WKSPoint &b)
    {
        me[0] = a;
        me[1] = b; 
    };

    //返回点pt的最近点在this上的参数，参数可能在取值区间之外
    inline double nearest_ex(const WKSPoint &pt) const
    {
        double dx,dy,x1,y1;
        dx = me[1].x - me[0].x;
        dy = me[1].y - me[0].y;
        x1 = pt.x - me[0].x;
        y1 = pt.y - me[0].y;

        if (dx==0 && dy==0) return 0;

        return (x1*dx + y1*dy) / (dx*dx + dy*dy);
    };

    //返回this和pt距离的平方
    inline double dis2(const WKSPoint &pt) const
    {
        double t = nearest_ex(pt);
        if (t <= 0) return me[0].dis2(pt);
        if (t >= 1) return me[1].dis2(pt);
    
        double dx,dy;
        dx = me[0].x + (me[1].x - me[0].x)*t - pt.x;
        dy = me[0].y + (me[1].y - me[0].y)*t - pt.y;
        return dx*dx+dy*dy;
    };
};

struct WKSPointZ : public WKSPoint
{
    double z;

    WKSPointZ() : WKSPoint(0, 0) {};
    WKSPointZ(const double& _x, const double& _y, const double& _z) : WKSPoint(_x, _y) {z = _z;};
    WKSPointZ(const WKSPointZ& pnt) : WKSPoint(pnt) {z = pnt.z;};
    WKSPointZ(const WKSPoint& pnt) : WKSPoint(pnt) {z = 0;};
    inline WKSPointZ& operator=(const WKSPointZ& rhs)
    {
        if (this == &rhs) return *this;
        x = rhs.x;
        y = rhs.y;
        z = rhs.z;
        return *this;
    };
};

typedef struct WKSSize
{
    double dblcx;
    double dblcy;
}WKSSize;

typedef struct WKSRect
{
    double left;
    double top;
    double right;
    double bottom;
}WKSRect;

inline bool ValidEnvelope(WKSRect& envelope)
{
    if ((envelope.left <= envelope.right)
        && (envelope.bottom <= envelope.top))
    {
        return true;
    }
    return false;
}

inline void CorrectEnvelope(WKSRect& envelope)
{
    double tmp;
    if (envelope.left > envelope.right)
    {
        tmp = envelope.left;
        envelope.left = envelope.right;
        envelope.right = tmp;
    }
    if (envelope.bottom > envelope.top)
    {
        tmp = envelope.bottom;
        envelope.bottom = envelope.top;
        envelope.top = tmp;
    }
};

inline bool PointInEnvelope(const double& PointX, const double& PointY,
    const WKSRect& env)
{
    if (PointX < env.left) return false;
    if (PointX > env.right) return false;
    if (PointY < env.bottom) return false;
    if (PointY > env.top) return false;
    return true;
}

//两个env是否有区域重叠（或相互包含）
inline bool EnvelopesTouched(const WKSRect& env1, const WKSRect& env2)
{
    return (env1.left > env2.right
        || env1.right < env2.left
        || env1.bottom > env2.top
        || env1.top < env2.bottom) ? false : true;
};

//两个env是否分离
inline bool EnvelopesSeparated(const WKSRect& env1, const WKSRect& env2)
{
    return !EnvelopesTouched(env1, env2);
};

//container是否完全包含env
inline bool EnvelopesContented(const WKSRect& container, const WKSRect& env)
{
    return (container.left < env.left
        && container.right > env.right
        && container.bottom < env.bottom
        && container.top > env.top) ? true : false;
};

//
inline void UpdateFullExtent(WKSRect& fullextent, const WKSRect& newextent)
{
    if (newextent.left < fullextent.left)
        fullextent.left = newextent.left;

    if (newextent.right > fullextent.right)
        fullextent.right = newextent.right;

    if (newextent.top > fullextent.top)
        fullextent.top = newextent.top;

    if (newextent.bottom < fullextent.bottom)
        fullextent.bottom = newextent.bottom;
}

inline void GetRectCenter(const WKSRect& rect, WKSPoint& center)
{
    center.x = (rect.left + rect.right) / 2;
    center.y = (rect.top + rect.bottom) / 2;
}

inline WKSPoint GetRectCenter(const WKSRect& rect)
{
    WKSPoint center;
    GetRectCenter(rect, center);
    return center;
}

inline void MoveRectTo(WKSRect& rect, const WKSPoint& newcenter)
{
    double width = rect.right - rect.left;
    double height = rect.top - rect.bottom;
    rect.left = newcenter.x - width / 2;
    rect.right = newcenter.x + width / 2;
    rect.top = newcenter.y + height / 2;
    rect.bottom = newcenter.y - height / 2;
}

inline WKSPoint GetRectLeftTop(const WKSRect& rect)
{
    return WKSPoint(rect.left, rect.top);
}

inline WKSPoint GetRectRightTop(const WKSRect& rect)
{
    return WKSPoint(rect.right, rect.top);
}

inline WKSPoint GetRectLeftBottom(const WKSRect& rect)
{
    return WKSPoint(rect.left, rect.bottom);
}

inline WKSPoint GetRectRightBottom(const WKSRect& rect)
{
    return WKSPoint(rect.right, rect.bottom);
}

inline void Down2WKSPoint(const WKSPointZ& pointz, WKSPoint& point)
{
    point.x = pointz.x;
    point.y = pointz.y;
}

inline WKSPoint Down2WKSPoint(const WKSPointZ& pointz)
{
    WKSPoint point;
    Down2WKSPoint(pointz, point);
    return point;
}

}

#endif
