#if !defined(STREAM_INCLUDED_)
#define STREAM_INCLUDED_

#include "..\\include\\InterfaceStream.h"

#pragma warning(disable: 4786)
#include <string>
using namespace std;

namespace easymap
{

class CStream;
typedef TSmartPtr<CStream> CStreamPtr;
class CFileStream;
typedef TSmartPtr<CFileStream> CFileStreamPtr;

#define STREAM_RW_BT(basetype)\
    inline dword Read(basetype& val){return this->Read(&val, sizeof(basetype));};\
    inline dword Write(const basetype& val){return this->Write(&val, sizeof(basetype));};

//================================================================================
//  流的鸡肋
//================================================================================
class CStream : public IStreamX
{
public:
    bool __stdcall Eof() const;
    bool __stdcall Bof() const;

    dword __stdcall ReadData(void* const p, const dword count);
    dword __stdcall WriteData(const void* const p, const dword count);

    virtual dword Read(void* const p, const dword count) = 0;
    virtual dword Write(const void* const p, const dword count) = 0;

    //  从当前游标位置开始复制size长度的数据到pDesCStream的当前位置上
    virtual dword CopyDataTo(CStreamPtr const pDesCStream,
        const dword count) = 0;

public:
    //用来简化基本数据类型的read/write操作
    //超级BT的代码，因为搞不定template
    //注意不可overload bool类型，否则会导致指针类型的隐式转换
    STREAM_RW_BT(char)
    STREAM_RW_BT(byte)
    STREAM_RW_BT(short)
    STREAM_RW_BT(word)
    STREAM_RW_BT(long)
    STREAM_RW_BT(dword)
    STREAM_RW_BT(float)
    STREAM_RW_BT(double)
    STREAM_RW_BT(__int64)

    inline dword ReadBool(bool& b){return this->Read(&b, sizeof(bool));};
    inline dword WriteBool(const bool& b){return this->Write(&b, sizeof(bool));};

    dword Read(string& s);
    dword Write(const string& s);
};
//================================================================================

//----------------------------------------
//FileStream磁盘文件的版本信息
typedef struct
{
    dword Description[20];
    dword MainVersion;
    dword SubVersion;
    dword Build;
    dword Reserved[30];
}FileStreamHeader;

const char FILEMAPSTREAMMAINVERSION     = 1;
const char FILEMAPSTREAMSUBVERSION      = 1;
const char FILEMAPSTREAMBUILDVERSION    = 4;
static const char* FILEMAPSTREAMIDENTIFY = "YYCat FileMapStream";
const dword FILEMAPHEADERLENGTH = sizeof(FileStreamHeader) + sizeof(dword);

//================================================================================
//  CFileMapStream和CSimpleFileStream的基类
//  直接作用于磁盘文件的Stream
//================================================================================
class CFileStream : public CStream
{
public:
    //  获得映射文件名
    virtual const char* GetMapFileName() const = 0;

    //  检查映射文件是否被别的程序更改（检查文件的修改时间）
    virtual bool ModifiedByOther() const = 0;

    //  用于同步修改时间的记录，ModifiedByOther()方法
    //  依靠检查最后修改时间是否和所记录的一致来工作
    virtual void SynchronizeModifyTime() = 0;

    //将缓冲区写回磁盘文件
    virtual void Flush() const = 0;

    //重新映射，以同步别人更新的内容
    virtual bool ReMap() = 0;

    //将文件连接信息存储在其它的stream中
    virtual dword LoadFromInfo(CStreamPtr pStream) = 0;
    virtual dword SaveInfo(CStreamPtr pStream) const = 0;

};

}

#endif
