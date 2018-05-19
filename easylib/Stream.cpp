#include "Stream.h"
#include "StringFuncs.h"

namespace easymap
{

bool __stdcall CStream::Eof() const
{
    return (this->GetPos() == this->GetSize());
}

bool __stdcall CStream::Bof() const
{
    return (0 == this->GetPos());
}

dword __stdcall CStream::ReadData(void* const p, const dword count)
{
    return this->Read(p, count);
}

dword __stdcall CStream::WriteData(const void* const p, const dword count)
{
    return this->Write(p, count);
}

dword CStream::Read(string& s)
{
    dword oldpos = this->GetPos();

    long n = 0;
    this->Read(&n, sizeof(long));
    if (0 < n)
    {
        static char buff[300000];
        if (299999 > n)
        {
            this->Read(buff, n*sizeof(char));
            buff[n] = 0;
            s = buff;
        }
        else
        {
            char* pc = new char[n + 1];
            this->Read(pc, n*sizeof(char));
            pc[n] = 0;
            s = pc;
            delete[] pc;
        }
    }

    return this->GetPos() - oldpos;
}

dword CStream::Write(const string& s)
{
    dword oldpos = this->GetPos();

    long n = s.size();
    this->Write(&n, sizeof(long));
    if (0 < n) this->Write(s.c_str(), n*sizeof(char));

    return this->GetPos() - oldpos;
}

}
