#if !defined(BASICTYPE_INCLUDED_)
#define BASICTYPE_INCLUDED_

namespace easymap
{

#if !defined(NULL)
    #define NULL 0
#endif

#if !defined(byte)
    typedef unsigned char byte;
#endif

#if !defined(word)
    typedef unsigned short word;
#endif

#if !defined(dword)
    typedef unsigned long dword;
#endif

#if !defined(qword)
    typedef unsigned __int64 qword;
#endif

union easy_variant
{
    short   value_short;
    long    value_long;
    __int64 value_int64;
    float   value_float;
    double  value_double;
};

inline bool _valid(const void* const p)
{
    return p ? true : false;
};

inline bool _invalid(const void* const p)
{
    return p ? false : true;
};

//最大值
#if !defined(__max)
template <typename T>
inline const T& __max(const T& a, const T& b)
{
    return a < b ? b : a;
}
#endif

//最小值
#if !defined(__min)
template <typename T>
inline const T& __min(const T& a, const T& b)
{
    return a < b ? b : a;
}
#endif

//平方
#if !defined(__sqr)
template <typename T>
inline const T __sqr(const T& a)
{
    return a*a;
}
#endif

#if !defined(PI)
const double PI = 3.1415927;
#endif

#if !defined(PI_RAD)
const double PI_RAD = 0.01745329252;    //PI in radian
#endif

}

#endif
