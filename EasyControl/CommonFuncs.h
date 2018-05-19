#if !defined(COMMONFUNCS_INCLUDED_)
#define COMMONFUNCS_INCLUDED_

inline COLORREF InvertRGB(const COLORREF color)
{
    char rvalue = 255 - GetRValue(color);
    char gvalue = 255 - GetGValue(color);
    char bvalue = 255 - GetBValue(color);
    return RGB(rvalue, gvalue, bvalue);
};

inline COLORREF GetRandomColor()
{
    ::srand((unsigned)time(NULL));
    static bool gg = false;
    gg = !gg;
    if (gg)
    {
        return RGB(long((1 - double(::rand()) / RAND_MAX) * 255),
            long((1- double(::rand()) / RAND_MAX) * 255),
            long((1 - double(::rand()) / RAND_MAX) * 255));
    }
    else
    {
        return RGB(long((double(::rand()) / RAND_MAX) * 255),
            long((double(::rand()) / RAND_MAX) * 255),
            long((double(::rand()) / RAND_MAX) * 255));
    }
}

inline void TrackCross(const HDC dc, const COLORREF color, const long rop2,
    const tagPOINT cnt, const long linelen = 10)
{
    HPEN pen = ::CreatePen(PS_SOLID, 1, color);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    long rop2saved = ::SetROP2(dc, rop2);
    ::MoveToEx(dc, cnt.x - linelen, cnt.y, NULL);
    ::LineTo(dc, cnt.x + linelen, cnt.y);
    ::MoveToEx(dc, cnt.x, cnt.y - linelen + 1, NULL);
    ::LineTo(dc, cnt.x, cnt.y + linelen - 1);
    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
}

inline void TrackEnvelope(const HDC dc, const COLORREF color, const long rop2,
    const long pensize, const tagPOINT p1, const tagPOINT p2)
{
    HPEN pen = ::CreatePen(PS_SOLID, pensize, color);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    long rop2saved = ::SetROP2(dc, rop2);
    ::MoveToEx(dc, p1.x, p1.y, NULL);
    ::LineTo(dc, p2.x, p1.y);
    ::LineTo(dc, p2.x, p2.y);
    ::LineTo(dc, p1.x, p2.y);
    ::LineTo(dc, p1.x, p1.y);

    tagPOINT envcnt;
    envcnt.x = (p1.x + p2.x) / 2;
    envcnt.y = (p1.y + p2.y) / 2;
    TrackCross(dc, RGB(255, 0, 0), rop2, envcnt);

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
};

inline void TrackNode(const HDC dc, const COLORREF color, const long x, const long y)
{
    LOGBRUSH logbrush;
    logbrush.lbColor = color;
    logbrush.lbStyle = BS_SOLID;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    int rop2saved = ::SetROP2(dc, R2_COPYPEN);

    tagRECT node = {x - 4, y - 4, x + 4, y + 4};
    ::FillRect(dc, &node, brush);

    ::DeleteObject(brush);
    ::SetROP2(dc, rop2saved);
}

inline void DrawEnvelope(const HDC dc, const long rop2, const COLORREF fillcolor,
    const COLORREF linecolor, const long pensize, const tagPOINT p1, const tagPOINT p2)
{
    RECT rect;
    rect.left = p1.x;
    rect.top = p1.y;
    rect.right = p2.x;
    rect.bottom = p2.y;

    HPEN pen = ::CreatePen(PS_SOLID, pensize, linecolor);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    LOGBRUSH logbrush;
    logbrush.lbStyle = BS_SOLID;
    logbrush.lbColor = fillcolor;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    long rop2saved = ::SetROP2(dc, rop2);

    ::FillRect(dc, &rect, brush);
    ::MoveToEx(dc, p1.x, p1.y, NULL);
    ::LineTo(dc, p2.x, p1.y);
    ::LineTo(dc, p2.x, p2.y);
    ::LineTo(dc, p1.x, p2.y);
    ::LineTo(dc, p1.x, p1.y);

    ::SetROP2(dc, rop2saved);
    ::DeleteObject(brush);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
};

inline void DrawCircle(const HDC dc, const long rop2, const COLORREF fillcolor,
    const COLORREF linecolor, const long pensize, const tagPOINT center,
    const long radius)
{
    RECT rect;
    rect.left = center.x - radius;
    rect.top = center.y - radius;
    rect.right = center.x + radius;
    rect.bottom = center.y + radius;

    HPEN pen = ::CreatePen(PS_SOLID, pensize, linecolor);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    LOGBRUSH logbrush;
    logbrush.lbStyle = BS_SOLID;
    logbrush.lbColor = fillcolor;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    HBRUSH brushsaved = (HBRUSH)::SelectObject(dc, brush);
    long rop2saved = ::SetROP2(dc, rop2);

    ::Ellipse(dc, rect.left, rect.top, rect.right, rect.bottom);

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, brushsaved);
    ::DeleteObject(brush);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
};

inline void DrawEllipse(const HDC dc, const long rop2, const COLORREF fillcolor,
    const COLORREF linecolor, const long pensize, const tagPOINT p1, const tagPOINT p2)
{
    RECT rect;
    rect.left = p1.x;
    rect.top = p1.y;
    rect.right = p2.x;
    rect.bottom = p2.y;

    HPEN pen = ::CreatePen(PS_SOLID, pensize, linecolor);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    LOGBRUSH logbrush;
    logbrush.lbStyle = BS_SOLID;
    logbrush.lbColor = fillcolor;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    HBRUSH brushsaved = (HBRUSH)::SelectObject(dc, brush);
    long rop2saved = ::SetROP2(dc, rop2);

    ::Ellipse(dc, rect.left, rect.top, rect.right, rect.bottom);

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, brushsaved);
    ::DeleteObject(brush);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
};

inline void DrawPolyline(const HDC dc, const long rop2, const COLORREF linecolor,
    const long pensize, const vector<tagPOINT> points)
{
    HPEN pen = ::CreatePen(PS_SOLID, pensize, linecolor);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    long rop2saved = ::SetROP2(dc, rop2);

    ::MoveToEx(dc, points[0].x, points[0].y, NULL);
    for (long i = 0; i < points.size() - 1; i++)
    {
        ::LineTo(dc, points[i + 1].x, points[i + 1].y);
    }

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
}

inline void DrawPolygon(const HDC dc, const long rop2, const COLORREF fillcolor,
    const COLORREF linecolor, const long pensize, const vector<tagPOINT> points)
{
    HPEN pen = ::CreatePen(PS_SOLID, pensize, linecolor);
    HPEN pensaved = (HPEN)::SelectObject(dc, pen);
    LOGBRUSH logbrush;
    logbrush.lbStyle = BS_SOLID;
    logbrush.lbColor = fillcolor;
    HBRUSH brush = ::CreateBrushIndirect(&logbrush);
    HBRUSH brushsaved = (HBRUSH)::SelectObject(dc, brush);
    long rop2saved = ::SetROP2(dc, rop2);
    long lastpoint = points.size() - 1;

    if (points.size() > 1)
    {
        long i;
        ::BeginPath(dc);
        if (points.size() > 2)
        {
            ::MoveToEx(dc, points[0].x, points[0].y, NULL);
            for (i = 0; i < points.size() - 1; i++)
            {
                ::LineTo(dc, points[i + 1].x, points[i + 1].y);
            }
        }
        else
        {
            ::MoveToEx(dc, points[lastpoint].x, points[lastpoint].y, NULL);
        }

        ::LineTo(dc, points[0].x, points[0].y);
        ::EndPath(dc);
        ::FillPath(dc);

        if (points.size() > 2)
        {
            ::MoveToEx(dc, points[0].x, points[0].y, NULL);
            for (i = 0; i < points.size() - 1; i++)
            {
                ::LineTo(dc, points[i + 1].x, points[i + 1].y);
            }
        }
        else
        {
            ::MoveToEx(dc, points[lastpoint].x, points[lastpoint].y, NULL);
        }

        ::LineTo(dc, points[0].x, points[0].y);
    }

    ::SetROP2(dc, rop2saved);
    ::SelectObject(dc, brushsaved);
    ::DeleteObject(brush);
    ::SelectObject(dc, pensaved);
    ::DeleteObject(pen);
}

//将top和left变成0
inline tagRECT TrimRect(const tagRECT rect)
{
    tagRECT rect1;
    rect1.right = rect.right - rect.left;
    rect1.bottom = rect.bottom - rect.top;
    rect1.left = rect1.top = 0;
    return rect1;
};

#endif