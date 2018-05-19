#if !defined(SIMPLEFILESTREAM_INCLUDED_)
#define SIMPLEFILESTREAM_INCLUDED_

#include "Stream.h"
#include "windows.h"

#pragma warning(disable: 4786)
#include <list>
#include <map>
#include <string>

namespace easymap
{

class CSimpleFileStream;
typedef TSmartPtr<CSimpleFileStream> CSimpleFileStreamPtr;

//================================================================================
//  这是用随机读写磁盘文件的方法搞定的Stream
//  所需内存小，且与文件实际大小无关，因此可以打开很大的文件（ <2GB ）
//  但速度比CFileMapStream慢很多
//================================================================================
class CSimpleFileStream : public CFileStream
{
CLASS_NAME(CSimpleFileStream)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

public:
    CSimpleFileStream(const char* pcFileName, const bool readonly = false);

private:
    CSimpleFileStream();
    ~CSimpleFileStream();

private:
    char m_MapFileName[3000];   //映射文件路径全名

    bool m_ReadOnly;            //是否只读

    HANDLE m_hMapFile;          //映射文件句柄

    dword m_Pos;				//流的游标位置，
                                //注：有效stream在文件中的起始位置为sizeof(FileStreamHeader)+4

    FILETIME m_LastModifyTime;  //文件被本Stream对象上次的修改时间，
                                //用于ModifiedByOther()的判断依据

private:
    //  将filemapstream对象设置为有效状态
    void Initial(const char* pcFileName, const bool readonly);

    //  将filemapstream对象设置为无效状态
    void Unload();

    //  ~
    void WhenShitHappen();

    //  将流的实际长度写到文件头
    void UpdateStreamLengthToFile();

public:
    bool __stdcall ReadOnly() const;
    dword __stdcall MovePos(const long delta, const StreamOffsetOrigin origin = SOFROMBEGINNING);
    dword __stdcall GetPos() const;
    dword Read(void* const p, const dword count);
    dword Write(const void* const p, const dword count);
    bool __stdcall SetSize(const dword size);
    dword __stdcall GetSize() const;
    bool __stdcall SaveToFile(const char* const filename);
    bool __stdcall LoadFromFile(const char* const filename);
    bool Clone(CStreamPtr& pStream) const;
    dword CopyDataTo(CStreamPtr const pDesCStream, const dword count);

public:
    //  获得文件名
    const char* GetMapFileName() const;

    //  检查文件是否被别的程序更改（检查文件的修改时间）
    bool ModifiedByOther() const;

    //  用于同步修改时间的记录，ModifiedByOther()方法
    //  依靠检查最后修改时间是否和所记录的一致来工作
    void SynchronizeModifyTime();

    //将缓冲区写回磁盘文件
    void Flush() const;

    bool ReMap();

    dword LoadFromInfo(CStreamPtr pStream);
    dword SaveInfo(CStreamPtr pStream) const;

    //  获得映射文件句柄，特殊用途
    const HANDLE GetMapFileHandle() const;
};
//================================================================================

}

#endif