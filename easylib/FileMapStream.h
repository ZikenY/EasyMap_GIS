#if !defined(FILEMAPSTREAM_INCLUDED_)
#define FILEMAPSTREAM_INCLUDED_

#include "Stream.h"
#include "windows.h"

#pragma warning(disable: 4786)
#include <list>
#include <map>
#include <string>

namespace easymap
{

class CFileMapStream;
typedef TSmartPtr<CFileMapStream> CFileMapStreamPtr;

//================================================================================
//  用内存映射文件搞定的stream，用来快速存取数据文件，速度很快
//  为了简化编程，这里一次性映射整个文件，比较消耗内存，因此文件不能太大
//  如果要操作大文件请使用SimpleFileStream
//================================================================================
class CFileMapStream : public CFileStream
{
CLASS_NAME(CFileMapStream)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

public:
    CFileMapStream(const char* pcFileName, const bool readonly = false);

private:
    CFileMapStream();
    ~CFileMapStream();

private:
    char m_MapFileName[3000];   //映射文件路径全名

    bool m_ReadOnly;            //是否只读

    dword m_MapFileSize;        //文件映射大小(CreateFileMapping()的大小)

    dword m_MapFileIncreasSize; //文件映射自动增长大小

    HANDLE m_hMapFile;          //映射文件句柄

    HANDLE m_hMappingObject;    //文件映射对象句柄

    char* m_pViewBase;          //映射窗口起始位置

    char* m_pStreamBase;        //流的起始位置，
                                //为m_pViewBase+sizeof(FileMapStreamHeader)+4

    char* m_pStreamPos;         //流的工作游标

    dword m_StreamSize;         //流的有效长度（不是映射文件长度！！）

    FILETIME m_LastModifyTime;  //文件被本Stream对象上次的修改时间，
                                //用于ModifiedByOther()的判断依据

private:
    //  将filemapstream对象设置为有效状态
    void Initial(const char* pcFileName, const bool readonly);

    //  将filemapstream对象设置为无效状态
    void Unload();

    //  ~
    void WhenShitHappen();

    //  映射文件，注意调用此函数之前要保证：
    //      1.文件已经打开(即m_hMapFile有效)
    //      2.文件没有被映射(
    //          即m_hMappingObject和m_pViewBase等无效)
    bool MappingFile();

    //  增加size之前先检查文件映射大小，保证映射大小大于流长度，
    //  【注意】这个函数并没有修改m_StreamSize
    bool IncreaseMappingSize(const dword delta);

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
    //  获得映射文件名
    const char* GetMapFileName() const;

    //  设置映射文件的自动增长大小（最小值）
    bool SetIncreasSize(const dword size);

    //  得到映射文件的自动增长大小（最小值）
    dword GetIncreasSize() const;

    //  检查映射文件是否被别的程序更改（检查文件的修改时间）
    bool ModifiedByOther() const;

    //  用于同步修改时间的记录，ModifiedByOther()方法
    //  依靠检查最后修改时间是否和所记录的一致来工作
    void SynchronizeModifyTime();

    void Flush() const;

    //  重新映射
    bool ReMap();

    dword LoadFromInfo(CStreamPtr pStream);
    dword SaveInfo(CStreamPtr pStream) const;

    //  获得映射文件句柄，特殊用途
    const HANDLE GetMapFileHandle() const;
};
//================================================================================

}

#endif