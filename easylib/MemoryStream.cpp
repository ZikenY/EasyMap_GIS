#include "MemoryStream.h"

#include <stdio.h>
#include <memory.h>
#include <malloc.h>

namespace easymap
{

const char _memorystream_identify_str[] = "easymap memorystream v1.1 ";

CLASS_FACTORY_INSTANCE(CMemoryStream)

CMemoryStream::CMemoryStream()
{
    INIT_REFCOUNT

    m_pBuffer       = NULL;
    m_BufferSize    = 0;
    m_AssignedSize  = 0;
    m_pPos          = m_pBuffer;
}

CMemoryStream::CMemoryStream(const dword size)
{
    INIT_REFCOUNT

    if (0 < size)
    {
        m_pBuffer = (char*)::malloc(size);
        if (!m_pBuffer)
        {
            //内存溢出
            m_BufferSize = m_AssignedSize = 0;
            m_pPos = m_pBuffer;
            return;
        }
        ::memset(m_pBuffer, 0, size);
    }
    else
    {
        m_pBuffer = NULL;
    }
    m_BufferSize = m_AssignedSize = size;
    m_pPos = m_pBuffer;
}

CMemoryStream::CMemoryStream(const CMemoryStream& memorystream)
{
    INIT_REFCOUNT

    m_BufferSize = m_AssignedSize = memorystream.m_AssignedSize;
    if (0 < m_BufferSize)
    {
        m_pBuffer = (char*)::malloc(m_BufferSize);
        if (!m_pBuffer)
        {
            //内存溢出
            m_BufferSize = m_AssignedSize = 0;
            m_pPos = m_pBuffer;
            return;
        }
        ::memcpy(m_pBuffer, memorystream.m_pBuffer,
            m_AssignedSize);
    }
    else
    {
        m_pBuffer = NULL;
    }
    this->MovePos(memorystream.GetPos());
}

CMemoryStream::~CMemoryStream()
{
    ::free(m_pBuffer);
}

bool __stdcall CMemoryStream::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IStreamX"))
        || (0 == strcmp(interfacename, "CStream"))
        || (0 == strcmp(interfacename, "CMemoryStream")))
    {
        *pp = this;
    }
    else
    {
        *pp = NULL;
        return false;
    }

    static_cast<IObj*>(*pp)->_AddRef();
    return true;
}

bool __stdcall CMemoryStream::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
//    if (_valid(*ppObj)) (*ppObj)->_Release();
    assert(!*ppObj);

    *ppObj = new CMemoryStream(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool CMemoryStream::Clone(IObjPtr& pObj) const
{
    pObj = new CMemoryStream(*this);
    return true;
}

bool __stdcall CMemoryStream::ReadOnly() const
{
    return false;
}

dword __stdcall CMemoryStream::MovePos(const long delta,
    const StreamOffsetOrigin origin)
{
    dword posdelta;
    switch(origin)
    {
    case SOFROMBEGINNING:
        posdelta = (dword)delta;
        break;
    case SOFROMCURRENT:
        posdelta = (dword)(m_pPos - m_pBuffer + delta);
        break;
    case SOFROMEND:
        posdelta = m_AssignedSize - (dword)delta;
        break;
    default: return 0;
    }
    if ((0 > posdelta) || (posdelta > m_AssignedSize))
        return 0;

    m_pPos = m_pBuffer + posdelta;

    return m_pPos - m_pBuffer;
}

dword __stdcall CMemoryStream::GetPos() const
{
    return m_pPos - m_pBuffer;
}

dword CMemoryStream::Read(void* const p, const dword count)
{
    if (_invalid(p)) throw;
    if (0 >= m_AssignedSize) return 0;

    dword realcount;
    dword remain = m_AssignedSize - (m_pPos - m_pBuffer);
    if (remain >= count)
    {
        realcount = count;
    }
    else
    {
        realcount = remain;
    }
    ::memcpy(p, m_pPos, realcount);
    m_pPos += realcount;
    return realcount;
}

dword CMemoryStream::Write(const void* const p, const dword count)
{
    if (_invalid(p)) throw;
    if (0 >= count) return 0;

    //这里的内存分配策略不是很好，但在大多数情况下都可以高效的工作

    dword oldpos = m_pPos - m_pBuffer;
    //检查内存中buffer的长度，如果不够就扩展
    dword remain = m_BufferSize - oldpos;
    if (remain < count)
    {
        //buffer长度不够，申请扩展之
        //注意申请的内存大小有3种可能：
        //    1. count
        //    2. m_BufferSize
        //    3. m_MinRequestSize
        dword requestsize;
        if (count > m_BufferSize)
        {
            requestsize = count;
        }
        else
        {
            requestsize = m_BufferSize;
        }

        if (requestsize < MEMSTREAM_MINIMAL_EXPAND_SIZE)
        {
            requestsize = MEMSTREAM_MINIMAL_EXPAND_SIZE;
        }

        dword buffersize = oldpos + requestsize;
        if (m_pBuffer)
        {
            char* pTmp = (char*)::realloc(m_pBuffer,
                buffersize);
            if (!pTmp)
            {
                //内存溢出
                return false;
            }
            m_pBuffer = pTmp;   //有可能内存块的首地址已经发生了改变
        }
        else
        {
            m_pBuffer = (char*)::malloc(buffersize);
            if (!m_pBuffer)
            {
                //内存溢出
                return false;
            }
        }

        m_BufferSize = buffersize;
        m_pPos = m_pBuffer + oldpos;//别忘了将游标指向正确的位置
    }

    //检查m_AssignedSize的值，确保正确的有效长度
    if (m_AssignedSize - oldpos < count)
    {
        m_AssignedSize = oldpos + count;
    }

    ::memcpy(m_pPos, p, count);
    m_pPos += count;
    return count;
}

bool __stdcall CMemoryStream::SetSize(const dword size)
{
    if (0 > size) return false;

    //这里有个设计缺陷
    //buffer长度和有效长度将被设为同样的值：m_AssignedSize = m_BufferSize = size
    //这很重要，LoadFormFile()依赖这一点才能正确工作

    dword oldpos = m_pPos - m_pBuffer;
    if (m_pBuffer)
    {
        char* pTmp = (char*)::realloc(m_pBuffer, size);
        if (0 == size)
        {
            m_pBuffer = NULL;
        }
        else if (!pTmp)
        {
            //内存溢出
            return false;
        }
        //有可能内存块的首地址已经发生了改变
        m_pBuffer = pTmp;
    }
    else if (0 < size)
    {
        m_pBuffer = (char*)::malloc(size);
        if (!m_pBuffer)
        {
            //内存溢出
            return false;
        }
    }

    if (size > m_AssignedSize)
    {
//        CharType* p = m_pBuffer + m_AssignedSize;
//        ::memset(p, 0, (size - m_AssignedSize));
        m_pPos = m_pBuffer + oldpos;
    }
    else
    {
        if (size > oldpos)
        {
            m_pPos = m_pBuffer + oldpos;
        }
        else
        {
            m_pPos = m_pBuffer + size;
        }
    }
    m_BufferSize = m_AssignedSize = size;
    return true;
}

dword __stdcall CMemoryStream::GetSize() const
{
    //这里返回的是有效长度
    return m_AssignedSize;
}

bool __stdcall CMemoryStream::SaveToFile(const char* const filename)
{
    FILE* pFile = ::fopen(filename, "wb");
    if (!pFile)
    {
        return false;
    }

    //写入头
    ::fwrite(&_memorystream_identify_str, sizeof(char),
        sizeof(_memorystream_identify_str), pFile);

    //先写入4bytes的长度信息
    ::fwrite(&m_AssignedSize, sizeof(dword), 1, pFile);
    dword numwritten = 0;
    if (0 < m_AssignedSize)
    {
        numwritten = ::fwrite(m_pBuffer, sizeof(char),
            m_AssignedSize, pFile);
    }
    ::fclose(pFile);
    if (numwritten != m_AssignedSize)
    {
        //可能磁盘空间不够
        throw;
    }
    return true;
}

bool __stdcall CMemoryStream::LoadFromFile(const char* const filename)
{
    this->SetSize(0);//必须这样

    FILE* pFile = ::fopen(filename, "rb");
    if (!pFile)
    {
        return false;
    }

    char identify[sizeof(_memorystream_identify_str)];
    ::fread(identify, sizeof(char), sizeof(_memorystream_identify_str), pFile);
    if (::strcmp(identify, _memorystream_identify_str) != 0)
    {
        //文件不对
        return false;
    }

    //先读出4bytes的长度信息
    dword numreaded = ::fread(&m_AssignedSize, sizeof(dword), 1, pFile);
    if (numreaded < 1)
    {
        return false;
    }

    m_BufferSize = m_AssignedSize;
    numreaded = 0;
    if (0 < m_AssignedSize)
    {
        m_pBuffer = (char*)::malloc(m_AssignedSize);
        if (!m_pBuffer)
        {
            //内存溢出
            ::fclose(pFile);
            m_pPos = m_pBuffer;
            m_BufferSize = m_AssignedSize = 0;
            return false;
        }
        numreaded = ::fread(m_pBuffer, sizeof(char),
            m_AssignedSize, pFile);
    }
    else
    {
        //该做的SetSize(0)都已经做了
    }
    ::fclose(pFile);
    m_pPos = m_pBuffer;
    if (numreaded != m_AssignedSize)
    {
        throw;
    }

    return true;
}

dword CMemoryStream::CopyDataTo(CStreamPtr const pDesCStream, const dword count)
{
    dword realcount;
    dword remain = m_AssignedSize - (m_pPos - m_pBuffer);
    if (remain >= count)
    {
        realcount = count;
    }
    else
    {
        realcount = remain;
    }

    dword wrotebytes = pDesCStream->Write(m_pPos, realcount);
    m_pPos += wrotebytes;
    return wrotebytes;
}

const void* CMemoryStream::_GetRawPos() const
{
    return m_pPos;
}

bool CMemoryStream::ExpandBuffer(const dword delta)
{
    return false;
}

}
