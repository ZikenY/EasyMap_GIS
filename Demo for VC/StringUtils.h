#if !defined(STRINGUTILS_INCLUDED_)
#define STRINGUTILS_INCLUDED_

#include <afxwin.h>         // MFC core and standard components

#pragma warning(disable: 4786)
#include <string>
using namespace std;

inline string CStrToSStr(const CString& a)
{
    return a.operator LPCTSTR();
}

inline CString SStrToCStr(const string& a)
{
    return a.c_str();
}

//----------------------------------------------------------------
//下面一些字符串处理函数，用来操作CString类型
//----------------------------------------------------------------
inline CString IntToCStr(long a)
{
    TCHAR cBuff[10];
    _itoa(a, cBuff, 10);
    return (CString)cBuff;
};

inline long CStrToInt(CString str)
{
    return atoi(str.GetBuffer(str.GetLength()));
};

inline CString FloatToCStr(double a)
{
    
//    TCHAR cBuff[30];    
//    sprintf(cBuff, "%f", a);
//    return (CString)cBuff;

    CString sResult;
    sResult.Format(_T("%.7f"), a);
    return sResult;
};

inline CString FloatToCStr(double a, int digit)
{
    CString sResult;

    if (digit < 0)
    {
        sResult.Format(_T("%.0f"), a);
        return sResult;
    }

    if (digit > 31)
    {
        sResult.Format(_T("%.31f"), a);
        return sResult;
    }

    TCHAR cBuff[2];
    _itoa(digit, cBuff, 10);
    CString sTmp = (CString)cBuff;
    sTmp.TrimLeft(); sTmp.TrimRight();
    sResult.Format(_T("%." + sTmp + "f"), a);
    return sResult;
};


inline double CStrToFloat(CString str)
{
    return atof(str.GetBuffer(str.GetLength()));
};

//将string转成double，忽略其中的非数字字符
inline double CStrToFloatSafe(CString sInput)
{
    sInput.TrimLeft(); sInput.TrimRight();

    CString sFloatOK;
    TCHAR cGot;
    BOOL bGotPoint = FALSE;
    long i, nStart;

    if (!sInput.Compare("") || !sInput.Compare("-"))
        return 0.0;

    //先拿出符号
    cGot = sInput.GetAt(0);
    if (cGot == '-')
        nStart = 1;
    else
        nStart = 0;

    //如果第一个字符是'.'就在前面加'0'
    if (cGot == '.')
        sInput = "0" + sInput;

    for (i = nStart; i < sInput.GetLength(); i++)
    {
        cGot = sInput.GetAt(i);
        switch (cGot)
        {
        case '0':
            sFloatOK = sFloatOK + "0";
            break;
        case '1':
            sFloatOK = sFloatOK + "1";
            break;
        case '2':
            sFloatOK = sFloatOK + "2";
            break;
        case '3':
            sFloatOK = sFloatOK + "3";
            break;
        case '4':
            sFloatOK = sFloatOK + "4";
            break;
        case '5':
            sFloatOK = sFloatOK + "5";
            break;
        case '6':
            sFloatOK = sFloatOK + "6";
            break;
        case '7':
            sFloatOK = sFloatOK + "7";
            break;
        case '8':
            sFloatOK = sFloatOK + "8";
            break;
        case '9':
            sFloatOK = sFloatOK + "9";
            break;
        case '.':
            {
                if (bGotPoint)
                    break;
                bGotPoint = TRUE;
                sFloatOK = sFloatOK + ".";
                break;
            }
        }
    }

    //加上符号
    if (nStart)
        sFloatOK = "-" + sFloatOK;

    if (sFloatOK.GetLength())
        return atof(sFloatOK.GetBuffer(sFloatOK.GetLength()));
    else
        return 0.0;
};

//将string转成double，忽略其中的非数字字符，并支持检错
inline BOOL CStrToFloatSafe(CString sInput, double* pfResult)
{

    BOOL bResult = TRUE;

    sInput.TrimLeft(); sInput.TrimRight();

    CString sFloatOK;
    TCHAR cGot;
    BOOL bGotPoint = FALSE;
    long i, nStart;
    
    if (pfResult == NULL)
    {
        ASSERT(0);
        return FALSE;
    }

    if ((!sInput.Compare("")) || (!sInput.Compare("-")))
    {
        *pfResult = 0.0;
        return FALSE;
    }

    //先拿出符号
    cGot = sInput.GetAt(0);
    if (cGot == '-')
    {
        nStart = 1;
    }
    else
    {
        nStart = 0;
    }

    //如果第一个字符是'.'就在前面加'0'
    if (cGot == '.')
    {
        sInput = "0" + sInput;
    }

    for (i = nStart; i < sInput.GetLength(); i++)
    {
        cGot = sInput.GetAt(i);
        switch (cGot)
        {
        case '0':
            sFloatOK = sFloatOK + "0";
            break;
        case '1':
            sFloatOK = sFloatOK + "1";
            break;
        case '2':
            sFloatOK = sFloatOK + "2";
            break;
        case '3':
            sFloatOK = sFloatOK + "3";
            break;
        case '4':
            sFloatOK = sFloatOK + "4";
            break;
        case '5':
            sFloatOK = sFloatOK + "5";
            break;
        case '6':
            sFloatOK = sFloatOK + "6";
            break;
        case '7':
            sFloatOK = sFloatOK + "7";
            break;
        case '8':
            sFloatOK = sFloatOK + "8";
            break;
        case '9':
            sFloatOK = sFloatOK + "9";
            break;
        case '.':
            {
                if (bGotPoint)
                {
                    bResult = FALSE;
                    break;
                }
                bGotPoint = TRUE;
                sFloatOK = sFloatOK + ".";
                break;
            }
        default:
            bResult = FALSE;

        }
    }

    //加上符号
    if (nStart)
        sFloatOK = "-" + sFloatOK;

    if (sFloatOK.GetLength())
        *pfResult = atof(sFloatOK.GetBuffer(sFloatOK.GetLength()));
    else
        *pfResult = 0.0;

    return bResult;
};


//--------------------------------------------------------------------------------
//  去掉左边的空格
//--------------------------------------------------------------------------------
inline string TrimLeft(const string& s)
{
    long offset = -1;
    long size = s.size();
    for (long i = 0; i < size; i++)
    {
        if (' ' != s[i])
        {
            offset = i;
            break;
        }
    }

    if (0 > offset)
    {
        return string("");
    }

    return s.substr(offset, size);
}

//--------------------------------------------------------------------------------
//  去掉右边的空格
//--------------------------------------------------------------------------------
inline string TrimRight(const string& s)
{
    long offset = -1;
    for (long i = s.size() - 1; i >= 0; i--)
    {
        if (' ' != s[i])
        {
            offset = i;
            break;
        }
    }

    if (0 > offset)
    {
        return string("");
    }

    return s.substr(0, offset + 1);
}

//--------------------------------------------------------------------------------
//  去掉两端的空格
//--------------------------------------------------------------------------------
inline string Trim(const string& s)
{
    return TrimLeft(TrimRight(s));
}

//--------------------------------------------------------------------------------
//  变成大写
//--------------------------------------------------------------------------------
inline string UpperString(const string& s)
{
    return string(::strupr((char*)s.c_str()));
}

//--------------------------------------------------------------------------------
//  变成小写
//--------------------------------------------------------------------------------
inline string LowerString(const string& s)
{
    return string(::strlwr((char*)s.c_str()));
}

//--------------------------------------------------------------------------------
//  整形    ->  字符串
//--------------------------------------------------------------------------------
inline string IntToStr(const long i)
{
    char cBuff[10];
    ::itoa(i, cBuff, 10);
    return string(cBuff);
}

//--------------------------------------------------------------------------------
//  字符串  ->  整形
//--------------------------------------------------------------------------------
inline long StrToInt(const string& s)
{
    return ::atoi(s.c_str());
}

//--------------------------------------------------------------------------------
//  浮点型  ->  字符串
//--------------------------------------------------------------------------------
inline string FloatToStr(const double f)
{
    char cBuff[30];    
    ::sprintf(cBuff, "%f", f);
    return string(cBuff);
}

//--------------------------------------------------------------------------------
//  字符串  ->  浮点型
//--------------------------------------------------------------------------------
inline double StrToFloat(const string& s)
{
    return ::atof(s.c_str());
}

//--------------------------------------------------------------------------------
//  找到最后一个'find'字符
//--------------------------------------------------------------------------------
inline long FindLastChar(const char* pc, const char find)
{
    char* p = (char*)pc;
    long offset = -1;
    while (0 != *p)
    {
        if (find == *p) offset = p - pc;
        p++;
    }

    return offset;
}

//--------------------------------------------------------------------------------
//  取出路径部分（去掉最后的文件名）
//--------------------------------------------------------------------------------
inline string GetDirectoryPart(const string& pathfilename)
{
    long offset = FindLastChar(pathfilename.c_str(), '\\');
    if (0 > offset)
    {
        return string("");
    }

    return string(pathfilename.substr(0, offset));
}

//--------------------------------------------------------------------------------
//  取出最后的文件名（去掉路径部分）
//--------------------------------------------------------------------------------
inline string RemoveDirectoryPart(const string& pathfilename)
{
    long offset = FindLastChar(pathfilename.c_str(), '\\');
    if (0 > offset)
    {
        return pathfilename;
    }

    return string(pathfilename.substr(offset + 1, pathfilename.length()));
}

//--------------------------------------------------------------------------------
//  取出扩展名
//--------------------------------------------------------------------------------
inline string GetExtNamePart(const string& filename)
{
    long offset = FindLastChar(filename.c_str(), '.');
    if (0 > offset)
    {
        return string("");
    }

    return string(filename.substr(offset + 1, filename.length()));
}

//--------------------------------------------------------------------------------
//  去掉扩展名
//--------------------------------------------------------------------------------
inline string RemoveExtNamePart(const string& filename)
{
    long offset = FindLastChar(filename.c_str(), '.');
    if (0 > offset)
    {
        return filename;
    }

    return string(filename.substr(0, offset));
}

inline CString CGetDirectoryPart(const CString& pathfilename)
{
    return SStrToCStr(GetDirectoryPart(CStrToSStr(pathfilename)));
}

inline CString CRemoveDirectoryPart(const CString& pathfilename)
{
    return SStrToCStr(RemoveDirectoryPart(CStrToSStr(pathfilename)));
}

inline CString CGetExtNamePart(const CString& filename)
{
    return SStrToCStr(GetExtNamePart(CStrToSStr(filename)));
}

inline CString CRemoveExtNamePart(const CString& filename)
{
    return SStrToCStr(RemoveExtNamePart(CStrToSStr(filename)));
}

//----------------- 以下函数用来处理带汉字的情况 -----------------

//分解汉字
inline BOOL GetSpeclialAnnoWidth(CString annoText,long charWidth,long & Width)
{
   if( annoText.IsEmpty() != 0 ) return FALSE;

   int subStrPos = annoText.Find(_T("//"));
   if( subStrPos <= 0 ) return FALSE;

   int len = annoText.GetLength();
   CString str = annoText.Left(subStrPos);  //tOP TEXT;
   annoText = annoText.Right(len-subStrPos-2);   //BOTTOM TEXT;
   if( str.GetLength() < annoText.GetLength() )
        Width =  (annoText.GetLength()*charWidth);
   else Width = (str.GetLength()*charWidth);       

   return TRUE;            
}

//全角转半角
inline void AllAngleToHalfAngle( CString & AnnoText )
{
   int count = 0;
   int length = AnnoText.GetLength();
   for(int i = 0;i < length ; i++)
   {
       if(AnnoText[i] == -93)
      {    
        if(i > 0 && AnnoText[i - 1]& 128 )
            return;
        AnnoText.SetAt(i, AnnoText[i + 1] - 128);
        int j = i + 1;
        for(;j< AnnoText.GetLength() - 1; j++)
            AnnoText.SetAt(j, AnnoText[j + 1]);
          AnnoText.SetAt(j, ' ');
       }
   }
   AnnoText.TrimRight( );
}       

//获取字符串中的字符个数
inline int GetCharCount(CString stAnnoString)
{ 
    if( stAnnoString.IsEmpty() != 0 ) return (0);    
    int count = 0;
    int numChar = stAnnoString.GetLength();
    for(int i = 0;i< numChar; i++)
    {
        if(stAnnoString[i] & 128)
        {
            count++;
            i++;
        }
        else
            count++;
    }
    return count;
}

//从一个字符串中取得第n个字符（基于1）
inline CString GetChar(int n,CString str)
{
    if( str.IsEmpty() != 0 ) return (_T(""));    
    if(n > GetCharCount(str))
        return _T("");
    int count = 0;
    int numChar = str.GetLength();
    CString midStr = _T("");
    for(int i = 0;i< numChar; i++)
    {
        if(str[i] & 128)
        {
            count++;
            if(count == n)
            {
                midStr = str.Mid(i,2);
                break;
            }
            i++;
        }
        else
        {
            count++;
            if(count == n)
            {
                midStr = str.Mid(i,1);
                break;
            }
        }
    }
    return midStr;
}

//获取一个字符串的左边的n个字符
inline CString Left(int n,CString str)
{
    if( str.IsEmpty() != 0 ) return (_T(""));    
    if(n > GetCharCount(str))
        return str;
    int count = 0;
    int numChar = str.GetLength();
    CString leftStr = _T("");
    int i = 0;
    for(;i< numChar; i++)
    {
        if(str[i] & 128)
        {
            count++;
            i++;
            if(count == n)
            {
                break;
            }
        }
        else
        {
            count++;
            if(count == n)
            {
                break;
            }
        }
    }
    return str.Left(i + 1);
}

//删除字符串中第n个位置的字符（可能汉字）, 0 - based string
inline CString RemoveAt(int n, CString str)
{
    if(str.IsEmpty()) return (_T(""));    
    if(n > GetCharCount(str)-1)    return str;
    int count = 0;
    int numChar = str.GetLength();
    int i = 0;
    for(;i< numChar; i++,count++)
    {
        if(count == n)break;
        if(str[i] & 128) i++;
    }
    if(str[i] & 128)
        return str.Left(i) + str.Right(numChar - i - 2);
    else
        return str.Left(i) + str.Right(numChar - i - 1);
}

//在字符串第n个位置插入一个子串, 0 - based String
inline CString InsertAt(int n, CString str, CString insertStr)
{
    if(str.IsEmpty()) return (_T(""));    
    if(n>=GetCharCount(str))//n>=GetCharCount(str)时，是Append
        return str + insertStr;
    int count = 0;
    int numChar = str.GetLength();
    int i = 0;
    for(;i< numChar; i++,count++)
    {
        if(count == n)break;
        if(str[i] & 128) i++;
    }
    return str.Left(i) + insertStr + str.Right(numChar-i);
}

//获取一个字符串的右边的n个字符
inline CString Right(int n,CString str)
{
    if( str.IsEmpty() != 0 ) return (_T(""));
    if(n > GetCharCount(str))
        return str;
    int count = 0;
    int numChar = str.GetLength();
    CString RightStr = _T("");
    int i = numChar-1;
    for(;i >= 0 ; i--)
    {
        if(str[i] & 128)        //汉字
        {
            count++;
            i++;
            if(count == n)
            {
                break;
            }
        }
        else
        {
            count++;
            if(count == n)
            {
                break;
            }
        }
    }
    return str.Right(i + 1);
}

//根据给定的字节位置 获取字符串中的字符位置,主要为了注意有汉字出现的地方
inline int GetCharPos( CString AnnoString, int Pos ) 
{ 
  int Length = AnnoString.GetLength();

  if( Length <= 0 ) return -9999;
  if( Pos > Length ) return -9999;

  if( Pos == 0 )  return 0;
  
  int count = 0;
  int numChar = AnnoString.GetLength();
  for( int i = 0; i < numChar;i++ )
  {
     if(AnnoString[i] & 128)        //汉字
     {
        count++;        
        i++;
        if( i == Pos )    break;        
      }
    else
    {
       count++;
       if(i == Pos )    break;
    }      
  }
  return count;
}

#endif //#define STRINGUTILS_INCLUDED_