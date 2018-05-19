#if !defined(SUPPORTCLASSES_INCLUDED_)
#define SUPPORTCLASSES_INCLUDED_

#include "..\\include\\InterfaceSupport.h"
#include "Persist.h"
#include "ClassFactory.h"
#pragma warning(disable: 4786)
#include <vector>
#include <string>
using namespace std;

namespace easymap
{

class CAnsiString;
class CIntArray;
class CDoubleArray;
class CStringArray;
class CObjArray;
class CPersistArray;
class CIntMapString;
class CIntMapInt;
class CIntMapDouble;
class CStringMapInt;
class CStringMapDouble;
class CIntMapObj;
class CIntMapPersist;
class CStringMapObj;
class CStringMapPersist;

typedef TSmartPtr<CAnsiString> CAnsiStringPtr;
typedef TSmartPtr<CIntArray> CIntArrayPtr;
typedef TSmartPtr<CDoubleArray> CDoubleArrayPtr;
typedef TSmartPtr<CStringArray> CStringArrayPtr;
typedef TSmartPtr<CObjArray> CObjArrayPtr;
typedef TSmartPtr<CPersistArray> CPersistArrayPtr;
typedef TSmartPtr<CIntMapString> CIntMapStringPtr;
typedef TSmartPtr<CIntMapInt> CIntMapIntPtr;
typedef TSmartPtr<CIntMapDouble> CIntMapDoublePtr;
typedef TSmartPtr<CStringMapInt> CStringMapIntPtr;
typedef TSmartPtr<CStringMapDouble> CStringMapDoublePtr;
typedef TSmartPtr<CIntMapObj> CIntMapObjPtr;
typedef TSmartPtr<CIntMapPersist> CIntMapPersistPtr;
typedef TSmartPtr<CStringMapObj> CStringMapObjPtr;
typedef TSmartPtr<CStringMapPersist> CStringMapPersistPtr;


class CAnsiString : public IAnsiString
{
CLASS_NAME(CAnsiString)
PERSIST_DUMP(CAnsiString)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CAnsiString();
private:
    ~CAnsiString();
private:
    string m_text;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    void __stdcall SetText(const char* const text);
    const char* __stdcall GetText() const;
    dword __stdcall GetSize() const;
};


class CIntArray : public IIntArray
{
CLASS_NAME(CIntArray)
PERSIST_DUMP(CIntArray)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CIntArray();
private:
    ~CIntArray();
private:
    vector<long> m_vector;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Add(const long value);
    bool __stdcall SetAt(const dword index, const long value);
    bool __stdcall GetAt(const dword index, long& value) const;
    void __stdcall Clear();
    bool __stdcall Resize(const dword newsize);
    dword __stdcall GetSize() const;
};


class CDoubleArray : public IDoubleArray
{
CLASS_NAME(CDoubleArray)
PERSIST_DUMP(CDoubleArray)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CDoubleArray();
private:
    ~CDoubleArray();
private:
    vector<double> m_vector;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Add(const double value);
    bool __stdcall SetAt(const dword index, const double value);
    bool __stdcall GetAt(const dword index, double& value) const;
    void __stdcall Clear();
    bool __stdcall Resize(const dword newsize);
    dword __stdcall GetSize() const;
};


class CStringArray : public IStringArray
{
CLASS_NAME(CStringArray)
PERSIST_DUMP(CStringArray)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CStringArray();
private:
    ~CStringArray();
private:
    vector<string> m_vector;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Add(const char* const text);
    bool __stdcall SetAt(const dword index, const char* const text);
    bool __stdcall GetAt(const dword index, IAnsiString** ppString) const;
    void __stdcall Clear();
    bool __stdcall Resize(const dword newsize);
    dword __stdcall GetSize() const;
};


class CObjArray : public IObjArray
{
CLASS_NAME(CObjArray)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CObjArray();
private:
    ~CObjArray();
private:
    vector<IObjPtr> m_vector;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

public:
    bool __stdcall Add(const IObj* pObj);
    bool __stdcall SetAt(const dword index, const IObj* const pObj);
    bool __stdcall GetAt(const dword index, IObj** ppObj) const;
    void __stdcall Clear();
    bool __stdcall Resize(const dword newsize);
    dword __stdcall GetSize() const;
};


class CPersistArray : public IPersistArray
{
CLASS_NAME(CPersistArray)
PERSIST_DUMP(CPersistArray)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CPersistArray();
private:
    ~CPersistArray();
private:
    vector<IPersistPtr> m_vector;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Add(const IPersist* pObj);
    bool __stdcall SetAt(const dword index, const IPersist* const pObj);
    bool __stdcall GetAt(const dword index, IPersist** ppObj) const;
    void __stdcall Clear();
    bool __stdcall Resize(const dword newsize);
    dword __stdcall GetSize() const;
};


class CIntMapString : public IIntMapString
{
CLASS_NAME(CIntMapString)
PERSIST_DUMP(CIntMapString)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CIntMapString();
private:
    ~CIntMapString();
private:
    map<long, string> m_map;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Set(const long key, const char* const value);
    bool __stdcall Get(const long key, IAnsiString** value) const;
    bool __stdcall GetAt(const dword index, long& key, IAnsiString** value) const;
    void __stdcall GetKeys(IIntArray** ppKeys) const;
    void __stdcall GetValues(IStringArray** ppValues) const;
    void __stdcall Clear();
    dword __stdcall GetSize() const;
};


class CIntMapInt : public IIntMapInt
{
CLASS_NAME(CIntMapInt)
PERSIST_DUMP(CIntMapInt)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CIntMapInt();
private:
    ~CIntMapInt();
private:
    map<long, long> m_map;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Set(const long key, const long value);
    bool __stdcall Get(const long key, long& value) const;
    bool __stdcall GetAt(const dword index, long& key, long& value) const;
    void __stdcall GetKeys(IIntArray** ppKeys) const;
    void __stdcall GetValues(IIntArray** ppValues) const;
    void __stdcall Clear();
    dword __stdcall GetSize() const;
};


class CIntMapDouble : public IIntMapDouble
{
CLASS_NAME(CIntMapDouble)
PERSIST_DUMP(CIntMapDouble)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CIntMapDouble();
private:
    ~CIntMapDouble();
private:
    map<long, double> m_map;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Set(const long key, const double value);
    bool __stdcall Get(const long key, double& value) const;
    bool __stdcall GetAt(const dword index, long& key, double& value) const;
    void __stdcall GetKeys(IIntArray** ppKeys) const;
    void __stdcall GetValues(IDoubleArray** ppValues) const;
    void __stdcall Clear();
    dword __stdcall GetSize() const;
};


class CStringMapInt : public IStringMapInt
{
CLASS_NAME(CStringMapInt)
PERSIST_DUMP(CStringMapInt)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CStringMapInt();
private:
    ~CStringMapInt();
private:
    map<string, long> m_map;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Set(const char* const key, const long value);
    bool __stdcall Get(const char* const key, long& value) const;
    bool __stdcall GetAt(const dword index, IAnsiString** key, long& value) const;
    void __stdcall GetKeys(IStringArray** ppKeys) const;
    void __stdcall GetValues(IIntArray** ppValues) const;
    void __stdcall Clear();
    dword __stdcall GetSize() const;
};


class CStringMapDouble : public IStringMapDouble
{
CLASS_NAME(CStringMapDouble)
PERSIST_DUMP(CStringMapDouble)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CStringMapDouble();
private:
    ~CStringMapDouble();
private:
    map<string, double> m_map;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Set(const char* const key, const double value);
    bool __stdcall Get(const char* const key, double& value) const;
    bool __stdcall GetAt(const dword index, IAnsiString** key, double& value) const;
    void __stdcall GetKeys(IStringArray** ppKeys) const;
    void __stdcall GetValues(IDoubleArray** ppValues) const;
    void __stdcall Clear();
    dword __stdcall GetSize() const;
};


class CIntMapObj : public IIntMapObj
{
CLASS_NAME(CIntMapObj)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CIntMapObj();
private:
    ~CIntMapObj();
private:
    map<long, IObjPtr> m_map;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

public:
    bool __stdcall Set(const long key, const IObj* const pValue);
    bool __stdcall Get(const long key, IObj** ppValue) const;
    bool __stdcall GetAt(const dword index, long& key, IObj** ppValue) const;
    void __stdcall GetKeys(IIntArray** ppKeys) const;
    void __stdcall GetValues(IObjArray** ppValues) const;
    void __stdcall Clear();
    dword __stdcall GetSize() const;
};


class CIntMapPersist : public IIntMapPersist
{
CLASS_NAME(CIntMapPersist)
PERSIST_DUMP(CIntMapPersist)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CIntMapPersist();
private:
    ~CIntMapPersist();
private:
    map<long, IPersistPtr> m_map;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Set(const long key, const IPersist* const pValue);
    bool __stdcall Get(const long key, IPersist** ppValue) const;
    bool __stdcall GetAt(const dword index, long& key, IPersist** ppValue) const;
    void __stdcall GetKeys(IIntArray** ppKeys) const;
    void __stdcall GetValues(IPersistArray** ppValues) const;
    void __stdcall Clear();
    dword __stdcall GetSize() const;
};


class CStringMapObj : public IStringMapObj
{
CLASS_NAME(CStringMapObj)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CStringMapObj();
private:
    ~CStringMapObj();
private:
    map<string, IObjPtr> m_map;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

public:
    bool __stdcall Set(const char* const key, const IObj* const pValue);
    bool __stdcall Get(const char* const key, IObj** ppValue) const;
    bool __stdcall GetAt(const dword index, IAnsiString** key, IObj** ppValue) const;
    void __stdcall GetKeys(IStringArray** ppKeys) const;
    void __stdcall GetValues(IObjArray** ppValues) const;
    void __stdcall Clear();
    dword __stdcall GetSize() const;
};


class CStringMapPersist : public IStringMapPersist
{
CLASS_NAME(CStringMapPersist)
PERSIST_DUMP(CStringMapPersist)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CStringMapPersist();
private:
    ~CStringMapPersist();
private:
    map<string, IPersistPtr> m_map;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;

private:
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall Set(const char* const key, const IPersist* const pValue);
    bool __stdcall Get(const char* const key, IPersist** ppValue) const;
    bool __stdcall GetAt(const dword index, IAnsiString** key, IPersist** ppValue) const;
    void __stdcall GetKeys(IStringArray** ppKeys) const;
    void __stdcall GetValues(IPersistArray** ppValues) const;
    void __stdcall Clear();
    dword __stdcall GetSize() const;
};


CLASS_FACTORY(CAnsiString)
CLASS_FACTORY(CIntArray)
CLASS_FACTORY(CDoubleArray)
CLASS_FACTORY(CStringArray)
CLASS_FACTORY(CObjArray)
CLASS_FACTORY(CPersistArray)
CLASS_FACTORY(CIntMapString)
CLASS_FACTORY(CIntMapInt)
CLASS_FACTORY(CIntMapDouble)
CLASS_FACTORY(CStringMapInt)
CLASS_FACTORY(CStringMapDouble)
CLASS_FACTORY(CIntMapObj)
CLASS_FACTORY(CIntMapPersist)
CLASS_FACTORY(CStringMapObj)
CLASS_FACTORY(CStringMapPersist)

}

#endif