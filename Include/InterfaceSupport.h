#if !defined(INTERFACESUPPORT_INCLUDED_)
#define INTERFACESUPPORT_INCLUDED_

#include "InterfacePersist.h"

namespace easymap
{

class IAnsiString;
class IIntArray;
class IDoubleArray;
class IStringArray;
class IPersistArray;
class IIntMapString;
class IIntMapInt;
class IIntMapDouble;
class IStringMapInt;
class IStringMapDouble;
class IIntMapObj;
class IIntMapPersist;
class IStringMapObj;
class IStringMapPersist;

typedef TSmartPtr<IAnsiString> IAnsiStringPtr;
typedef TSmartPtr<IIntArray> IIntArrayPtr;
typedef TSmartPtr<IDoubleArray> IDoubleArrayPtr;
typedef TSmartPtr<IStringArray> IStringArrayPtr;
typedef TSmartPtr<IPersistArray> IPersistArrayPtr;
typedef TSmartPtr<IIntMapString> IIntMapStringPtr;
typedef TSmartPtr<IIntMapInt> IIntMapIntPtr;
typedef TSmartPtr<IIntMapDouble> IIntMapDoublePtr;
typedef TSmartPtr<IStringMapInt> IStringMapIntPtr;
typedef TSmartPtr<IStringMapDouble> IStringMapDoublePtr;
typedef TSmartPtr<IIntMapObj> IIntMapObjPtr;
typedef TSmartPtr<IIntMapPersist> IIntMapPersistPtr;
typedef TSmartPtr<IStringMapObj> IStringMapObjPtr;
typedef TSmartPtr<IStringMapPersist> IStringMapPersistPtr;


class IAnsiString : public IPersist
{
public:
    virtual void __stdcall SetText(const char* const text) = 0;
    virtual const char* __stdcall GetText() const = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IIntArray : public IPersist
{
public:
    virtual bool __stdcall Add(const long value) = 0;
    virtual bool __stdcall SetAt(const dword index, const long value) = 0;
    virtual bool __stdcall GetAt(const dword index, long& value) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual bool __stdcall Resize(const dword newsize) = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IDoubleArray : public IPersist
{
public:
    virtual bool __stdcall Add(const double value) = 0;
    virtual bool __stdcall SetAt(const dword index, const double value) = 0;
    virtual bool __stdcall GetAt(const dword index, double& value) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual bool __stdcall Resize(const dword newsize) = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IStringArray : public IPersist
{
public:
    virtual bool __stdcall Add(const char* const text) = 0;
    virtual bool __stdcall SetAt(const dword index, const char* const text) = 0;
    virtual bool __stdcall GetAt(const dword index, IAnsiString** ppString) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual bool __stdcall Resize(const dword newsize) = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IPersistArray : public IPersist
{
public:
    virtual bool __stdcall Add(const IPersist* pObj) = 0;
    virtual bool __stdcall SetAt(const dword index, const IPersist* const pObj) = 0;
    virtual bool __stdcall GetAt(const dword index, IPersist** ppObj) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual bool __stdcall Resize(const dword newsize) = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IIntMapString : public IPersist
{
public:
    virtual bool __stdcall Set(const long key, const char* const value) = 0;
    virtual bool __stdcall Get(const long key, IAnsiString** value) const = 0;
    virtual bool __stdcall GetAt(const dword index, long& key, IAnsiString** value) const = 0;
    virtual void __stdcall GetKeys(IIntArray** ppKeys) const = 0;
    virtual void __stdcall GetValues(IStringArray** ppValues) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IIntMapInt : public IPersist
{
public:
    virtual bool __stdcall Set(const long key, const long value) = 0;
    virtual bool __stdcall Get(const long key, long& value) const = 0;
    virtual bool __stdcall GetAt(const dword index, long& key, long& value) const = 0;
    virtual void __stdcall GetKeys(IIntArray** ppKeys) const = 0;
    virtual void __stdcall GetValues(IIntArray** ppValues) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IIntMapDouble : public IPersist
{
public:
    virtual bool __stdcall Set(const long key, const double value) = 0;
    virtual bool __stdcall Get(const long key, double& value) const = 0;
    virtual bool __stdcall GetAt(const dword index, long& key, double& value) const = 0;
    virtual void __stdcall GetKeys(IIntArray** ppKeys) const = 0;
    virtual void __stdcall GetValues(IDoubleArray** ppValues) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IStringMapInt : public IPersist
{
public:
    virtual bool __stdcall Set(const char* const key, const long value) = 0;
    virtual bool __stdcall Get(const char* const key, long& value) const = 0;
    virtual bool __stdcall GetAt(const dword index, IAnsiString** key, long& value) const = 0;
    virtual void __stdcall GetKeys(IStringArray** ppKeys) const = 0;
    virtual void __stdcall GetValues(IIntArray** ppValues) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IStringMapDouble : public IPersist
{
public:
    virtual bool __stdcall Set(const char* const key, const double value) = 0;
    virtual bool __stdcall Get(const char* const key, double& value) const = 0;
    virtual bool __stdcall GetAt(const dword index, IAnsiString** key, double& value) const = 0;
    virtual void __stdcall GetKeys(IStringArray** ppKeys) const = 0;
    virtual void __stdcall GetValues(IDoubleArray** ppValues) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IIntMapObj : public IObj
{
public:
    virtual bool __stdcall Set(const long key, const IObj* const pValue) = 0;
    virtual bool __stdcall Get(const long key, IObj** ppValue) const = 0;
    virtual bool __stdcall GetAt(const dword index, long& key, IObj** ppValue) const = 0;
    virtual void __stdcall GetKeys(IIntArray** ppKeys) const = 0;
    virtual void __stdcall GetValues(IObjArray** ppValues) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IIntMapPersist : public IPersist
{
public:
    virtual bool __stdcall Set(const long key, const IPersist* const pValue) = 0;
    virtual bool __stdcall Get(const long key, IPersist** ppValue) const = 0;
    virtual bool __stdcall GetAt(const dword index, long& key, IPersist** ppValue) const = 0;
    virtual void __stdcall GetKeys(IIntArray** ppKeys) const = 0;
    virtual void __stdcall GetValues(IPersistArray** ppValues) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IStringMapObj : public IObj
{
public:
    virtual bool __stdcall Set(const char* const key, const IObj* const pValue) = 0;
    virtual bool __stdcall Get(const char* const key, IObj** ppValue) const = 0;
    virtual bool __stdcall GetAt(const dword index, IAnsiString** key, IObj** ppValue) const = 0;
    virtual void __stdcall GetKeys(IStringArray** ppKeys) const = 0;
    virtual void __stdcall GetValues(IObjArray** ppValues) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual dword __stdcall GetSize() const = 0;
};

class IStringMapPersist : public IPersist
{
public:
    virtual bool __stdcall Set(const char* const key, const IPersist* const pValue) = 0;
    virtual bool __stdcall Get(const char* const key, IPersist** ppValue) const = 0;
    virtual bool __stdcall GetAt(const dword index, IAnsiString** key, IPersist** ppValue) const = 0;
    virtual void __stdcall GetKeys(IStringArray** ppKeys) const = 0;
    virtual void __stdcall GetValues(IPersistArray** ppValues) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual dword __stdcall GetSize() const = 0;
};

}

#endif