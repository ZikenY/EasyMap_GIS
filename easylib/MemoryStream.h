#if !defined(MEMORYSTREAM_INCLUDED_)
#define MEMORYSTREAM_INCLUDED_

#include "Stream.h"
#include "ClassFactory.h"

#pragma warning(disable: 4786)
#include <list>
#include <map>
#include <string>

namespace easymap
{

class CMemoryStream;
typedef TSmartPtr<CMemoryStream> CMemoryStreamPtr;

const dword MEMSTREAM_MINIMAL_EXPAND_SIZE = 65536;
//================================================================================
//  这是最基本最典型的stream实现，用于在内存中处理小数据量
//
//  为了能够加快反复写入小数据量的速度，调用Write()时如果buffer长度不够会申请较多
//  的内存以减少反复申请的次数，因此内存中的buffer大小一般大于stream的有效长度，详
//  见Write()的定义
//================================================================================
class CMemoryStream : public CStream
{
CLASS_NAME(CMemoryStream)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CMemoryStream();
    CMemoryStream(const dword size);
    CMemoryStream(const CMemoryStream& memorystream);

private:
    ~CMemoryStream();

private:
    char*   m_pBuffer;          //point to the memory block header
    char*   m_pPos;             //working cursor
    dword   m_BufferSize;       //length of the buffer
    dword   m_AssignedSize;     //the accurate size of stream

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

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
    dword CopyDataTo(CStreamPtr const pDesCStream, const dword count);
    const void* _GetRawPos() const;

public:
    //  手工扩展buffer
    bool ExpandBuffer(const dword delta);
};
//================================================================================

CLASS_FACTORY(CMemoryStream)

}

#endif