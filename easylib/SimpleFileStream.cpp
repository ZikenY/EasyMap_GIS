#include "SimpleFileStream.h"

#include <stdio.h>

namespace easymap
{

const dword BUFF_BLOCKSIZE = 8192;

CSimpleFileStream::CSimpleFileStream(const char* pcFileName,
    const bool readonly)
{
    INIT_REFCOUNT

    m_hMapFile                = INVALID_HANDLE_VALUE;
    m_MapFileName[0]        = 0;
    m_ReadOnly              = readonly;
    m_Pos                    = 0;

    this->Initial(pcFileName, readonly);
}

CSimpleFileStream::CSimpleFileStream()
{
    INIT_REFCOUNT

    m_hMapFile                = INVALID_HANDLE_VALUE;
    m_MapFileName[0]        = 0;
    m_Pos                    = 0;
}

CSimpleFileStream::~CSimpleFileStream()
{
    this->Unload();
}

bool __stdcall CSimpleFileStream::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IStreamX"))
        || (0 == strcmp(interfacename, "CStream"))
        || (0 == strcmp(interfacename, "CFileStream"))
        || (0 == strcmp(interfacename, "CSimpleFileStream")))
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

bool __stdcall CSimpleFileStream::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
//    if (_valid(*ppObj)) (*ppObj)->_Release();
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    (*ppObj) = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CSimpleFileStream::Clone(IObjPtr& pObj) const
{
    pObj.Clear();
    CSimpleFileStreamPtr pNewStream = new CSimpleFileStream(m_MapFileName, m_ReadOnly);
    if (0 == pNewStream->m_MapFileName[0])
    {
        return false;
    }
    else
    {
        pObj = pNewStream._p();
        return true;
    }
}

void CSimpleFileStream::Initial(const char* pcFileName,
    const bool readonly)
{
    this->Unload();

    m_ReadOnly = readonly;

    //调用CreateFile()打开一个文件
    dword DesiredAccess;
    dword ShareMode;
    dword CreationDisposition;
    dword FlagsAndAttributes;
    if (m_ReadOnly)
    {
        DesiredAccess       = GENERIC_READ;
        ShareMode           = FILE_SHARE_READ | FILE_SHARE_WRITE;
        CreationDisposition = OPEN_EXISTING;
        FlagsAndAttributes  = FILE_ATTRIBUTE_READONLY | FILE_FLAG_RANDOM_ACCESS;
    }
    else
    {
        DesiredAccess       = GENERIC_READ | GENERIC_WRITE;
        ShareMode           = FILE_SHARE_READ;// | FILE_SHARE_WRITE;
        CreationDisposition = OPEN_ALWAYS;
        FlagsAndAttributes  = FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS;
    }

    m_hMapFile = ::CreateFile(pcFileName, DesiredAccess, ShareMode,
        NULL, CreationDisposition, FlagsAndAttributes, NULL);
    if (_invalid(m_hMapFile) || (INVALID_HANDLE_VALUE == m_hMapFile))
    {
        this->WhenShitHappen();
        return;
    }

    //文件要么是新创建的，要么size >= FILEMAPHEADERLENGTH

    DWORD mapfilesize = ::GetFileSize(m_hMapFile, NULL);

    //------------- 初步文件类型是否正确 -------------
    //不是新建的文件，关注xxB
    bool xxA = 0 < mapfilesize;
    //文件长度不可能短于文件头
    bool xxB = FILEMAPHEADERLENGTH > mapfilesize;
    //只读的时候不能新建文件
    bool xxC = (0 == mapfilesize) && m_ReadOnly;
    if ((xxA&&xxB)||xxC)
    {
        ::CloseHandle(m_hMapFile);
        this->WhenShitHappen();
        return;
    }
    //------------------------------------------------

    if (0 == mapfilesize)
    {
        //新建的文件，先写入FileStreamHeader
        FileStreamHeader header;
        ::memset(&header, 0, sizeof(FileStreamHeader));
        ::strcpy((char*)header.Description, FILEMAPSTREAMIDENTIFY);
        ::strcat((char*)header.Description, "  by Summer Y - Copyrignt 2005");
        header.MainVersion = FILEMAPSTREAMMAINVERSION;  //主版本号
        header.SubVersion = FILEMAPSTREAMSUBVERSION;    //次版本号
        header.Build = FILEMAPSTREAMBUILDVERSION;       //创建号
        dword written;
        if (!::WriteFile(m_hMapFile, (void*)&header,
            sizeof(FileStreamHeader), &written, NULL))
        {
            ::CloseHandle(m_hMapFile);
            this->WhenShitHappen();
            return;
        }
        //现在流的长度为0
        dword streamsize = 0;
        ::WriteFile(m_hMapFile, (void*)&streamsize,
            sizeof(dword), &written, NULL);
        ::SetEndOfFile(m_hMapFile);
    }
    else
    {
        //检查文件类型和版本是否符合
        FileStreamHeader header;
        dword readed;
        ::ReadFile(m_hMapFile, (void*)&header,
            sizeof(FileStreamHeader), &readed, NULL);
        char desc[20];
        ::memcpy(desc, header.Description, 19);
        desc[19] = 0;
        if ((::strcmp(desc, FILEMAPSTREAMIDENTIFY))
            || (FILEMAPSTREAMMAINVERSION != header.MainVersion)
            || (FILEMAPSTREAMSUBVERSION != header.SubVersion)
            || (FILEMAPSTREAMBUILDVERSION != header.Build))
        {
            ::CloseHandle(m_hMapFile);
            this->WhenShitHappen();
            return;
        }
    }

    strcpy(m_MapFileName, pcFileName);

    this->SynchronizeModifyTime();
}

void CSimpleFileStream::Unload()
{
    if (m_hMapFile && (INVALID_HANDLE_VALUE != m_hMapFile))
    {
        ::CloseHandle(m_hMapFile);
    }

    this->WhenShitHappen();
}

void CSimpleFileStream::WhenShitHappen()
{
    m_hMapFile          = INVALID_HANDLE_VALUE;
    m_MapFileName[0]    = 0;
    m_Pos                = 0;
}

void CSimpleFileStream::UpdateStreamLengthToFile()
{
    dword oldpos = m_Pos;

    ::SetFilePointer(m_hMapFile, sizeof(FileStreamHeader),
        NULL, FILE_BEGIN);
    dword streamsize = this->GetSize();
    dword written;
    ::WriteFile(m_hMapFile, (void*)(&streamsize), sizeof(dword),
        &written, NULL);

    this->MovePos(oldpos, SOFROMBEGINNING);
}

bool __stdcall CSimpleFileStream::ReadOnly() const
{
    return m_ReadOnly;
}

dword __stdcall CSimpleFileStream::MovePos(const long delta,
    const StreamOffsetOrigin origin)
{
    dword oldpos = m_Pos;
    DWORD dwMoveMethod;
    LONG lDistanceToMove;
    switch(origin)
    {
    case SOFROMBEGINNING:
        dwMoveMethod = FILE_BEGIN;
        lDistanceToMove = FILEMAPHEADERLENGTH + delta;
        m_Pos = delta;
        break;

    case SOFROMCURRENT:
        dwMoveMethod = FILE_CURRENT;
        lDistanceToMove = delta;
        m_Pos += delta;
        break;

    case SOFROMEND:
        dwMoveMethod = FILE_BEGIN;
        m_Pos = this->GetSize() - delta;
        lDistanceToMove = FILEMAPHEADERLENGTH + m_Pos;
        break;

    default:
        return 0;
    }

    DWORD r = ::SetFilePointer(m_hMapFile, lDistanceToMove, NULL, dwMoveMethod);
    if (0xFFFFFFFF == r)
    {
        m_Pos = oldpos;
        return m_Pos;
    }

    return m_Pos;
}

dword __stdcall CSimpleFileStream::GetPos() const
{
    return m_Pos;
}

dword CSimpleFileStream::Read(void* const p, const dword count)
{
    dword readed;
    if (!::ReadFile(m_hMapFile, p, count, &readed, NULL))
    {
        return 0;
    }

    m_Pos += readed;
    return readed;
}

dword CSimpleFileStream::Write(const void* const p, const dword count)
{
    if (m_ReadOnly)
    {
        return 0;
    }

    dword oldsize = this->GetSize();

    dword written;
    if (!::WriteFile(m_hMapFile, p, count, &written, NULL))
    {
        return 0;
    }

    m_Pos += written;

    if (m_Pos >= oldsize)
    {
        ::SetEndOfFile(m_hMapFile);
    }

    this->UpdateStreamLengthToFile();
    this->SynchronizeModifyTime();
    return written;
}

bool __stdcall CSimpleFileStream::SetSize(const dword size)
{
    if (m_ReadOnly)
    {
        return false;
    }

    dword oldpos = m_Pos;
    dword oldsize = this->GetSize();
    dword inc = size - oldsize;
    if (0 > inc)
    {
        this->MovePos(size, SOFROMBEGINNING);
        ::SetEndOfFile(m_hMapFile);
        if (oldpos > size)
        {
            m_Pos = size;
        }
    }
    else
    {
        dword written;
        this->MovePos(oldsize, SOFROMBEGINNING);
        if (inc <= BUFF_BLOCKSIZE)
        {
            static char stuff[BUFF_BLOCKSIZE];
            ::WriteFile(m_hMapFile, stuff, inc, &written, NULL);
        }
        else
        {
            static char buff[BUFF_BLOCKSIZE];
            dword blockcount = inc / BUFF_BLOCKSIZE;
            dword residue = inc % BUFF_BLOCKSIZE;
            for (dword i = 0; i < blockcount; i++)
            {
                ::WriteFile(m_hMapFile, buff, BUFF_BLOCKSIZE, &written, NULL);
            }

            if (0 != residue)
            {
                ::WriteFile(m_hMapFile, buff, residue, &written, NULL);
            }
        }

        ::SetEndOfFile(m_hMapFile);
        ::SetFilePointer(m_hMapFile, oldpos+FILEMAPHEADERLENGTH, NULL, FILE_BEGIN);
    }

    this->UpdateStreamLengthToFile();
    this->SynchronizeModifyTime();
    return true;
}

dword __stdcall CSimpleFileStream::GetSize() const
{
    return ::GetFileSize(m_hMapFile, NULL) - FILEMAPHEADERLENGTH;
}

bool __stdcall CSimpleFileStream::SaveToFile(const char* const filename)
{
    throw;
}

bool __stdcall CSimpleFileStream::LoadFromFile(const char* const filename)
{
    throw;
}

dword CSimpleFileStream::CopyDataTo(CStreamPtr const pDesCStream,
    const dword count)
{
    dword realcount;
    dword remain = this->GetSize() - m_Pos;
    if (remain >= count)
    {
        realcount = count;
    }
    else
    {
        realcount = remain;
    }

    static char buff[BUFF_BLOCKSIZE];
    dword blockcount = realcount / BUFF_BLOCKSIZE;
    dword residue = realcount % BUFF_BLOCKSIZE;
    for (dword i = 0; i < blockcount; i++)
    {
        this->Read(buff, BUFF_BLOCKSIZE);
        pDesCStream->Write(buff, BUFF_BLOCKSIZE);
    }

    if (0 != residue)
    {
        this->Read(buff, residue);
        pDesCStream->Write(buff, residue);
    }

    m_Pos += realcount;
    return realcount;
}

const char* CSimpleFileStream::GetMapFileName() const
{
    return m_MapFileName;
}

bool CSimpleFileStream::ModifiedByOther() const
{
    BY_HANDLE_FILE_INFORMATION fileinfo;
    ::GetFileInformationByHandle(m_hMapFile, &fileinfo);
    if ((m_LastModifyTime.dwHighDateTime == fileinfo.ftLastWriteTime.dwHighDateTime)
        && (m_LastModifyTime.dwLowDateTime == fileinfo.ftLastWriteTime.dwLowDateTime))
    {
        return false;
    }
    else
    {
        return true;
    }
}

void CSimpleFileStream::SynchronizeModifyTime()
{
    //记下映射文件的最后修改时间
    BY_HANDLE_FILE_INFORMATION fileinfo;
    ::GetFileInformationByHandle(m_hMapFile, &fileinfo);
    m_LastModifyTime.dwHighDateTime = fileinfo.ftLastWriteTime.dwHighDateTime;
    m_LastModifyTime.dwLowDateTime = fileinfo.ftLastWriteTime.dwLowDateTime;
}

void CSimpleFileStream::Flush() const
{
    ::FlushFileBuffers(m_hMapFile);
}

bool CSimpleFileStream::ReMap()
{
    return true;
}

const HANDLE CSimpleFileStream::GetMapFileHandle() const
{
    return m_hMapFile;
}

dword CSimpleFileStream::LoadFromInfo(CStreamPtr pStream)
{
    dword oldpos = pStream->GetPos();

    string filename;
    pStream->Read(filename);
    bool readonly = false;
    pStream->Read(&readonly, sizeof(bool));
    dword pos = 0;
    pStream->Read(&pos, sizeof(dword));
    dword offset = pStream->GetPos() - oldpos;

    if (filename == "")
    {
        this->WhenShitHappen();
        return offset;
    }

    this->Initial(filename.c_str(), readonly);
    this->MovePos(pos);

    return offset;
}

dword CSimpleFileStream::SaveInfo(CStreamPtr pStream) const
{
    dword oldpos = pStream->GetPos();

    string filename = m_MapFileName;
    pStream->Write(filename);
    pStream->Write(&m_ReadOnly, sizeof(bool));
    dword pos = this->GetPos();
    pStream->Write(&pos, sizeof(dword));

    return pStream->GetPos() - oldpos;
}

}
