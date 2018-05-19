#if !defined(INTERFACESTREAM_INCLUDED_)
#define INTERFACESTREAM_INCLUDED_

#include "InterfaceObj.h"

namespace easymap
{

typedef dword StreamOffsetOrigin;
const StreamOffsetOrigin SOFROMBEGINNING    = 0;
const StreamOffsetOrigin SOFROMCURRENT      = 1;
const StreamOffsetOrigin SOFROMEND          = 2;

class IStreamX;
typedef TSmartPtr<IStreamX> IStreamXPtr;

class IStreamX : public IObj
{
public:
    virtual bool __stdcall ReadOnly() const = 0;

    virtual dword __stdcall MovePos(const long delta, const StreamOffsetOrigin origin = SOFROMBEGINNING) = 0;
    virtual dword __stdcall GetPos() const = 0;

    virtual bool __stdcall Eof() const = 0;
    virtual bool __stdcall Bof() const = 0;

    virtual dword __stdcall ReadData(void* const p, const dword count) = 0;
    virtual dword __stdcall WriteData(const void* const p, const dword count) = 0;

    virtual bool __stdcall SetSize(const dword size) = 0;
    virtual dword __stdcall GetSize() const = 0;

    virtual bool __stdcall SaveToFile(const char* const filename) = 0;
    virtual bool __stdcall LoadFromFile(const char* const filename) = 0;
};

}

#endif
