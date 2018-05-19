#include "SupportClasses.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CAnsiString)
CLASS_FACTORY_INSTANCE(CIntArray)
CLASS_FACTORY_INSTANCE(CDoubleArray)
CLASS_FACTORY_INSTANCE(CStringArray)
CLASS_FACTORY_INSTANCE(CObjArray)
CLASS_FACTORY_INSTANCE(CPersistArray)
CLASS_FACTORY_INSTANCE(CIntMapString)
CLASS_FACTORY_INSTANCE(CIntMapInt)
CLASS_FACTORY_INSTANCE(CIntMapDouble)
CLASS_FACTORY_INSTANCE(CStringMapInt)
CLASS_FACTORY_INSTANCE(CStringMapDouble)
CLASS_FACTORY_INSTANCE(CIntMapObj)
CLASS_FACTORY_INSTANCE(CIntMapPersist)
CLASS_FACTORY_INSTANCE(CStringMapObj)
CLASS_FACTORY_INSTANCE(CStringMapPersist)

CAnsiString::CAnsiString()
{
    INIT_REFCOUNT
}

CAnsiString::~CAnsiString()
{
}

bool __stdcall CAnsiString::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IAnsiString"))
        || (0 == strcmp(interfacename, "CAnsiString")))
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

bool __stdcall CAnsiString::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CAnsiString* pNew = new CAnsiString;
    pNew->m_text = m_text;
    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();
    return true;
}

dword __stdcall CAnsiString::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;
    ps->Write(m_text);
    return pStream->GetPos() - oldpos;
}

dword __stdcall CAnsiString::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;
    ps->Read(m_text);
    return pStream->GetPos() - oldpos;
}

void __stdcall CAnsiString::SetText(const char* const text)
{
    m_text = text;
}

const char* __stdcall CAnsiString::GetText() const
{
    return m_text.c_str();
}

dword __stdcall CAnsiString::GetSize() const
{
    return m_text.size();
}


CIntArray::CIntArray()
{
    INIT_REFCOUNT
}

CIntArray::~CIntArray()
{
}

bool __stdcall CIntArray::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IIntArray"))
        || (0 == strcmp(interfacename, "CIntArray")))
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

bool __stdcall CIntArray::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CIntArray* pNew = new CIntArray;
    pNew->m_vector.resize(m_vector.size());
    for (dword i = 0; i < m_vector.size(); i++)
    {
        pNew->m_vector.push_back(m_vector[i]);
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CIntArray::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_vector.size();
    ps->Write(size);
    for (dword i = 0; i < m_vector.size(); i++)
    {
        ps->Write(m_vector[i]);
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CIntArray::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size;
    ps->Read(size);
    for (dword i = 0; i < m_vector.size(); i++)
    {
        long value;
        ps->Read(value);
        m_vector.push_back(value);
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CIntArray::Add(const long value)
{
    m_vector.push_back(value);
    return true;
}

bool __stdcall CIntArray::SetAt(const dword index, const long value)
{
    if (m_vector.size() <= index) return false;
    m_vector[index] = value;
    return true;
}

bool __stdcall CIntArray::GetAt(const dword index, long& value) const
{
    if (m_vector.size() <= index) return false;
    value = m_vector[index];
    return true;
}

void __stdcall CIntArray::Clear()
{
    m_vector.clear();
}

bool __stdcall CIntArray::Resize(const dword newsize)
{
    m_vector.resize(newsize);
    return true;
}

dword __stdcall CIntArray::GetSize() const
{
    return m_vector.size();
}


CDoubleArray::CDoubleArray()
{
    INIT_REFCOUNT
}

CDoubleArray::~CDoubleArray()
{
}

bool __stdcall CDoubleArray::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IDoubleArray"))
        || (0 == strcmp(interfacename, "CDoubleArray")))
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

bool __stdcall CDoubleArray::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CDoubleArray* pNew = new CDoubleArray;
    pNew->m_vector.resize(m_vector.size());
    for (dword i = 0; i < m_vector.size(); i++)
    {
        pNew->m_vector.push_back(m_vector[i]);
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CDoubleArray::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_vector.size();
    ps->Write(size);
    for (dword i = 0; i < m_vector.size(); i++)
    {
        ps->Write(m_vector[i]);
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CDoubleArray::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size;
    ps->Read(size);
    for (dword i = 0; i < m_vector.size(); i++)
    {
        double value;
        ps->Read(value);
        m_vector.push_back(value);
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CDoubleArray::Add(const double value)
{
    m_vector.push_back(value);
    return true;
}

bool __stdcall CDoubleArray::SetAt(const dword index, const double value)
{
    if (m_vector.size() <= index) return false;
    m_vector[index] = value;
    return true;
}

bool __stdcall CDoubleArray::GetAt(const dword index, double& value) const
{
    if (m_vector.size() <= index) return false;
    value = m_vector[index];
    return true;
}

void __stdcall CDoubleArray::Clear()
{
    m_vector.clear();
}

bool __stdcall CDoubleArray::Resize(const dword newsize)
{
    m_vector.resize(newsize);
    return true;
}

dword __stdcall CDoubleArray::GetSize() const
{
    return m_vector.size();
}


CStringArray::CStringArray()
{
    INIT_REFCOUNT
}

CStringArray::~CStringArray()
{
}

bool __stdcall CStringArray::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IStringArray"))
        || (0 == strcmp(interfacename, "CStringArray")))
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

bool __stdcall CStringArray::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CStringArray* pNew = new CStringArray;
    pNew->m_vector.resize(m_vector.size());
    for (dword i = 0; i < m_vector.size(); i++)
    {
        pNew->m_vector.push_back(m_vector[i]);
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CStringArray::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_vector.size();
    ps->Write(size);
    for (dword i = 0; i < m_vector.size(); i++)
    {
        ps->Write(m_vector[i]);
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CStringArray::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size;
    ps->Read(size);
    for (dword i = 0; i < m_vector.size(); i++)
    {
        string text;
        ps->Read(text);
        m_vector.push_back(text);
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CStringArray::Add(const char* const text)
{
    m_vector.push_back(string(text));
    return true;
}

bool __stdcall CStringArray::SetAt(const dword index, const char* const text)
{
    if (m_vector.size() <= index) return false;
    m_vector[index] = string(text);
    return true;
}

bool __stdcall CStringArray::GetAt(const dword index, IAnsiString** ppString) const
{
    if (_invalid(ppString)) return false;
    assert(!*ppString);

    if (m_vector.size() <= index) return false;

    *ppString = new CAnsiString;
    (*ppString)->SetText(m_vector[index].c_str());
    (*ppString)->_AddRef();
    return true;
}

void __stdcall CStringArray::Clear()
{
    m_vector.clear();
}

bool __stdcall CStringArray::Resize(const dword newsize)
{
    m_vector.resize(newsize);
    return true;
}

dword __stdcall CStringArray::GetSize() const
{
    return m_vector.size();
}


CObjArray::CObjArray()
{
    INIT_REFCOUNT
}

CObjArray::~CObjArray()
{
}

bool __stdcall CObjArray::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IObjArray"))
        || (0 == strcmp(interfacename, "CObjArray")))
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

bool __stdcall CObjArray::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CObjArray* pNew = new CObjArray;
    pNew->m_vector.resize(m_vector.size());
    for (dword i = 0; i < m_vector.size(); i++)
    {
        IObjPtr pNewObj;
        CLONE_PTR(m_vector[i], pNewObj)
        pNew->m_vector.push_back(pNewObj);
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

bool __stdcall CObjArray::Add(const IObj* pObj)
{
    IObjPtr pO = (IObj*)pObj;
    m_vector.push_back(pO);
    return true;
}

bool __stdcall CObjArray::SetAt(const dword index, const IObj* const pObj)
{
    if (m_vector.size() <= index) return false;
    m_vector[index] = (IObj*)pObj;
    return true;
}

bool __stdcall CObjArray::GetAt(const dword index, IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);
    if (m_vector.size() <= index) return false;

    *ppObj = m_vector[index]._p();
    if (_valid(*ppObj)) (*ppObj)->_AddRef();
    return true;
}

void __stdcall CObjArray::Clear()
{
    m_vector.clear();
}

bool __stdcall CObjArray::Resize(const dword newsize)
{
    m_vector.resize(newsize);
    return false;
}

dword __stdcall CObjArray::GetSize() const
{
    return m_vector.size();
}


CPersistArray::CPersistArray()
{
    INIT_REFCOUNT
}

CPersistArray::~CPersistArray()
{
}

bool __stdcall CPersistArray::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersistArray"))
        || (0 == strcmp(interfacename, "CPersistArray")))
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

bool __stdcall CPersistArray::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CPersistArray* pNew = new CPersistArray;
    pNew->m_vector.resize(m_vector.size());
    for (dword i = 0; i < m_vector.size(); i++)
    {
        IObjPtr pNewObj;
        CLONE_PTR(m_vector[i], pNewObj)
        IPersistPtr pNewPersist;
        CAST_PTR(pNewObj, pNewPersist, IPersist)
        pNew->m_vector.push_back(pNewPersist);
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CPersistArray::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_vector.size();
    ps->Write(size);
    for (dword i = 0; i < m_vector.size(); i++)
    {
        bool flag = true;
        if (m_vector[i].Assigned())
        {
            ps->WriteBool(flag);
            m_vector[i]->_DumpTo(pStream, assist);
        }
        else
        {
            flag = false;
            ps->WriteBool(flag);
        }
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CPersistArray::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size;
    ps->Read(size);
    for (dword i = 0; i < m_vector.size(); i++)
    {
        bool flag;
        ps->ReadBool(flag);
        IPersistPtr pObj;
        if (flag)
        {
            CPersistPtr pPersist;
            CPersist::_InstantiateFrom(ps, pPersist, assist);
            pObj = (IPersist*)pPersist._p();
        }
        m_vector.push_back(pObj);
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CPersistArray::Add(const IPersist* pObj)
{
    IPersistPtr pPersist = (IPersist*)pObj;
    m_vector.push_back(pPersist);
    return true;
}

bool __stdcall CPersistArray::SetAt(const dword index, const IPersist* const pObj)
{
    if (m_vector.size() <= index) return false;
    m_vector[index] = (IPersist*)pObj;
    return true;
}

bool __stdcall CPersistArray::GetAt(const dword index, IPersist** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);
    if (m_vector.size() <= index) return false;

    *ppObj = m_vector[index]._p();
    if (_valid(*ppObj)) (*ppObj)->_AddRef();
    return true;
}

void __stdcall CPersistArray::Clear()
{
    m_vector.clear();
}

bool __stdcall CPersistArray::Resize(const dword newsize)
{
    m_vector.resize(newsize);
    return false;
}

dword __stdcall CPersistArray::GetSize() const
{
    return m_vector.size();
}


CIntMapString::CIntMapString()
{
    INIT_REFCOUNT
}

CIntMapString::~CIntMapString()
{
}

bool __stdcall CIntMapString::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IIntMapString"))
        || (0 == strcmp(interfacename, "CIntMapString")))
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

bool __stdcall CIntMapString::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CIntMapString* pNew = new CIntMapString;
    map<long, string>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        pNew->m_map[it->first] = it->second;
        it++;
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CIntMapString::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_map.size();
    ps->Write(size);
    map<long, string>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        ps->Write(it->first);
        ps->Write(it->second);
        it++;
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CIntMapString::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_map.clear();
    long size;
    ps->Read(size);
    for (long i = 0; i < size; i++)
    {
        long key;
        ps->Read(key);
        string value;
        ps->Read(value);
        m_map[key] = value;
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CIntMapString::Set(const long key, const char* const value)
{
    m_map[key] = string(value);
    return true;
}

bool __stdcall CIntMapString::Get(const long key, IAnsiString** value) const
{
    if (_invalid(value)) return false;
    assert(!*value);
    map<long, string>::const_iterator it = m_map.find(key);
    if (it == m_map.end()) return false;
    *value = new CAnsiString;
    (*value)->SetText(string(it->second).c_str());
    (*value)->_AddRef();
    return true;
}

bool __stdcall CIntMapString::GetAt(const dword index, long& key, IAnsiString** value) const
{
    if (_invalid(value)) return false;
    assert(!*value);
    if (m_map.size() <= index) return false;
    map<long, string>::const_iterator it = m_map.begin();
    std::advance(it, index);
    key = it->first;
    *value = new CAnsiString;
    (*value)->SetText(string(it->second).c_str());
    (*value)->_AddRef();
    return true;
}

void __stdcall CIntMapString::GetKeys(IIntArray** ppKeys) const
{
    if (_invalid(ppKeys)) return;
    assert(!*ppKeys);
    *ppKeys = new CIntArray;
    map<long, string>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppKeys)->Add(it->first);
        it++;
    }

    (*ppKeys)->_AddRef();
}

void __stdcall CIntMapString::GetValues(IStringArray** ppValues) const
{
    if (_invalid(ppValues)) return;
    assert(!*ppValues);
    *ppValues = new CStringArray;
    map<long, string>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppValues)->Add(string(it->second).c_str());
        it++;
    }

    (*ppValues)->_AddRef();
}

void __stdcall CIntMapString::Clear()
{
    m_map.clear();
}

dword __stdcall CIntMapString::GetSize() const
{
    return m_map.size();
}


CIntMapInt::CIntMapInt()
{
    INIT_REFCOUNT
}

CIntMapInt::~CIntMapInt()
{
}

bool __stdcall CIntMapInt::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IIntMapInt"))
        || (0 == strcmp(interfacename, "CIntMapInt")))
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

bool __stdcall CIntMapInt::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CIntMapInt* pNew = new CIntMapInt;
    map<long, long>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        pNew->m_map[it->first] = it->second;
        it++;
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CIntMapInt::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_map.size();
    ps->Write(size);
    map<long, long>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        ps->Write(it->first);
        ps->Write(it->second);
        it++;
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CIntMapInt::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_map.clear();
    long size;
    ps->Read(size);
    for (long i = 0; i < size; i++)
    {
        long key, value;
        ps->Read(key);
        ps->Read(value);
        m_map[key] = value;
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CIntMapInt::Set(const long key, const long value)
{
    m_map[key] = value;
    return true;
}

bool __stdcall CIntMapInt::Get(const long key, long& value) const
{
    map<long, long>::const_iterator it = m_map.find(key);
    if (it == m_map.end()) return false;
    value = it->second;
    return true;
}

bool __stdcall CIntMapInt::GetAt(const dword index, long& key, long& value) const
{
    if (m_map.size() <= index) return false;
    map<long, long>::const_iterator it = m_map.begin();
    std::advance(it, index);
    key = it->first;
    value = it->second;
    return true;
}

void __stdcall CIntMapInt::GetKeys(IIntArray** ppKeys) const
{
    if (_invalid(ppKeys)) return;
    assert(!*ppKeys);
    *ppKeys = new CIntArray;
    map<long, long>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppKeys)->Add(it->first);
        it++;
    }

    (*ppKeys)->_AddRef();
}

void __stdcall CIntMapInt::GetValues(IIntArray** ppValues) const
{
    if (_invalid(ppValues)) return;
    assert(!*ppValues);
    *ppValues = new CIntArray;
    map<long, long>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppValues)->Add(it->second);
        it++;
    }

    (*ppValues)->_AddRef();
}

void __stdcall CIntMapInt::Clear()
{
    m_map.clear();
}

dword __stdcall CIntMapInt::GetSize() const
{
    return m_map.size();
}


CIntMapDouble::CIntMapDouble()
{
    INIT_REFCOUNT
}

CIntMapDouble::~CIntMapDouble()
{
}

bool __stdcall CIntMapDouble::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IIntMapDouble"))
        || (0 == strcmp(interfacename, "CIntMapDouble")))
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

bool __stdcall CIntMapDouble::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CIntMapDouble* pNew = new CIntMapDouble;
    map<long, double>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        pNew->m_map[it->first] = it->second;
        it++;
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CIntMapDouble::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_map.size();
    ps->Write(size);
    map<long, double>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        ps->Write(it->first);
        ps->Write(it->second);
        it++;
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CIntMapDouble::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_map.clear();
    long size;
    ps->Read(size);
    for (long i = 0; i < size; i++)
    {
        long key;
        double value;
        ps->Read(key);
        ps->Read(value);
        m_map[key] = value;
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CIntMapDouble::Set(const long key, const double value)
{
    m_map[key] = value;
    return true;
}

bool __stdcall CIntMapDouble::Get(const long key, double& value) const
{
    map<long, double>::const_iterator it = m_map.find(key);
    if (it == m_map.end()) return false;
    value = it->second;
    return true;
}

bool __stdcall CIntMapDouble::GetAt(const dword index, long& key, double& value) const
{
    if (m_map.size() <= index) return false;
    map<long, double>::const_iterator it = m_map.begin();
    std::advance(it, index);
    key = it->first;
    value = it->second;
    return true;
}

void __stdcall CIntMapDouble::GetKeys(IIntArray** ppKeys) const
{
    if (_invalid(ppKeys)) return;
    assert(!*ppKeys);
    *ppKeys = new CIntArray;
    map<long, double>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppKeys)->Add(it->first);
        it++;
    }

    (*ppKeys)->_AddRef();
}

void __stdcall CIntMapDouble::GetValues(IDoubleArray** ppValues) const
{
    if (_invalid(ppValues)) return;
    assert(!*ppValues);
    *ppValues = new CDoubleArray;
    map<long, double>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppValues)->Add(it->second);
        it++;
    }

    (*ppValues)->_AddRef();
}

void __stdcall CIntMapDouble::Clear()
{
    m_map.clear();
}

dword __stdcall CIntMapDouble::GetSize() const
{
    return m_map.size();
}


CStringMapInt::CStringMapInt()
{
    INIT_REFCOUNT
}

CStringMapInt::~CStringMapInt()
{
}

bool __stdcall CStringMapInt::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IStringMapInt"))
        || (0 == strcmp(interfacename, "CStringMapInt")))
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

bool __stdcall CStringMapInt::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CStringMapInt* pNew = new CStringMapInt;
    map<string, long>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        pNew->m_map[it->first] = it->second;
        it++;
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CStringMapInt::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_map.size();
    ps->Write(size);
    map<string, long>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        ps->Write(it->first);
        ps->Write(it->second);
        it++;
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CStringMapInt::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_map.clear();
    long size;
    ps->Read(size);
    for (long i = 0; i < size; i++)
    {
        string key;
        long value;
        ps->Read(key);
        ps->Read(value);
        m_map[key] = value;
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CStringMapInt::Set(const char* const key, const long value)
{
    m_map[key] = value;
    return true;
}

bool __stdcall CStringMapInt::Get(const char* const key, long& value) const
{
    map<string, long>::const_iterator it = m_map.find(key);
    if (it == m_map.end()) return false;
    value = it->second;
    return true;
}

bool __stdcall CStringMapInt::GetAt(const dword index, IAnsiString** key, long& value) const
{
    if (_invalid(key)) return false;
    assert(!*key);
    *key = NULL;

    if (m_map.size() <= index) return false;
    map<string, long>::const_iterator it = m_map.begin();
    std::advance(it, index);
    *key = new CAnsiString;
    (*key)->SetText(it->first.c_str());
    (*key)->_AddRef();
    value = it->second;
    return true;
}

void __stdcall CStringMapInt::GetKeys(IStringArray** ppKeys) const
{
    if (_invalid(ppKeys)) return;
    assert(!*ppKeys);

    *ppKeys = new CStringArray;
    map<string, long>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppKeys)->Add(it->first.c_str());
        it++;
    }

    (*ppKeys)->_AddRef();
}

void __stdcall CStringMapInt::GetValues(IIntArray** ppValues) const
{
    if (_invalid(ppValues)) return;
    assert(!*ppValues);

    *ppValues = new CIntArray;
    map<string, long>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppValues)->Add(it->second);
        it++;
    }

    (*ppValues)->_AddRef();
}

void __stdcall CStringMapInt::Clear()
{
    m_map.clear();
}

dword __stdcall CStringMapInt::GetSize() const
{
    return m_map.size();
}


CStringMapDouble::CStringMapDouble()
{
    INIT_REFCOUNT
}

CStringMapDouble::~CStringMapDouble()
{
}

bool __stdcall CStringMapDouble::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IStringMapDouble"))
        || (0 == strcmp(interfacename, "CStringMapDouble")))
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

bool __stdcall CStringMapDouble::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CStringMapDouble* pNew = new CStringMapDouble;
    map<string, double>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        pNew->m_map[it->first] = it->second;
        it++;
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CStringMapDouble::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_map.size();
    ps->Write(size);
    map<string, double>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        ps->Write(it->first);
        ps->Write(it->second);
        it++;
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CStringMapDouble::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_map.clear();
    long size;
    ps->Read(size);
    for (long i = 0; i < size; i++)
    {
        string key;
        double value;
        ps->Read(key);
        ps->Read(value);
        m_map[key] = value;
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CStringMapDouble::Set(const char* const key, const double value)
{
    m_map[key] = value;
    return true;
}

bool __stdcall CStringMapDouble::Get(const char* const key, double& value) const
{
    map<string, double>::const_iterator it = m_map.find(key);
    if (it == m_map.end()) return false;
    value = it->second;
    return true;
}

bool __stdcall CStringMapDouble::GetAt(const dword index, IAnsiString** key, double& value) const
{
    if (_invalid(key)) return false;
    assert(!*key);
    *key = NULL;

    if (m_map.size() <= index) return false;
    map<string, double>::const_iterator it = m_map.begin();
    std::advance(it, index);
    *key = new CAnsiString;
    (*key)->SetText(it->first.c_str());
    (*key)->_AddRef();
    value = it->second;
    return true;
}

void __stdcall CStringMapDouble::GetKeys(IStringArray** ppKeys) const
{
    if (_invalid(ppKeys)) return;
    assert(!*ppKeys);

    *ppKeys = new CStringArray;
    map<string, double>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppKeys)->Add(it->first.c_str());
        it++;
    }

    (*ppKeys)->_AddRef();
}

void __stdcall CStringMapDouble::GetValues(IDoubleArray** ppValues) const
{
    if (_invalid(ppValues)) return;
    assert(!*ppValues);

    *ppValues = new CDoubleArray;
    map<string, double>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppValues)->Add(it->second);
        it++;
    }

    (*ppValues)->_AddRef();
}

void __stdcall CStringMapDouble::Clear()
{
    m_map.clear();
}

dword __stdcall CStringMapDouble::GetSize() const
{
    return m_map.size();
}


CIntMapObj::CIntMapObj()
{
    INIT_REFCOUNT
}

CIntMapObj::~CIntMapObj()
{
}

bool __stdcall CIntMapObj::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IIntMapObj"))
        || (0 == strcmp(interfacename, "CIntMapObj")))
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

bool __stdcall CIntMapObj::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CIntMapObj* pNew = new CIntMapObj;
    map<long, IObjPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        IObjPtr pNewObj;
        CLONE_PTR(it->second, pNewObj)
        pNew->m_map[it->first] = pNewObj;
        it++;
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

bool __stdcall CIntMapObj::Set(const long key, const IObj* const pValue)
{
    m_map[key] = (IObj*)pValue;
    return true;
}

bool __stdcall CIntMapObj::Get(const long key, IObj** ppValue) const
{
    if (_invalid(ppValue)) return false;
    assert(!*ppValue);
    *ppValue = NULL;

    map<long, IObjPtr>::const_iterator it = m_map.find(key);
    if (it == m_map.end()) return false;
    if (it->second.Assigned())
    {
        *ppValue = it->second._p();
        (*ppValue)->_AddRef();
    }

    return true;
}

bool __stdcall CIntMapObj::GetAt(const dword index, long& key, IObj** ppValue) const
{
    if (_invalid(ppValue)) return false;
    assert(!*ppValue);
    *ppValue = NULL;

    if (m_map.size() <= index) return false;
    map<long, IObjPtr>::const_iterator it = m_map.begin();
    std::advance(it, index);
    key = it->first;
    if (it->second.Assigned())
    {
        *ppValue = it->second._p();
        (*ppValue)->_AddRef();
    }
    return true;
}

void __stdcall CIntMapObj::GetKeys(IIntArray** ppKeys) const
{
    if (_invalid(ppKeys)) return;
    assert(!*ppKeys);

    *ppKeys = new CIntArray;
    map<long, IObjPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppKeys)->Add(it->first);
        it++;
    }

    (*ppKeys)->_AddRef();
}

void __stdcall CIntMapObj::GetValues(IObjArray** ppValues) const
{
    if (_invalid(ppValues)) return;
    assert(!*ppValues);

    *ppValues = new CObjArray;
    map<long, IObjPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppValues)->Add(it->second._p());
        it++;
    }

    (*ppValues)->_AddRef();
}

void __stdcall CIntMapObj::Clear()
{
    m_map.clear();
}

dword __stdcall CIntMapObj::GetSize() const
{
    return m_map.size();
}


CIntMapPersist::CIntMapPersist()
{
    INIT_REFCOUNT
}

CIntMapPersist::~CIntMapPersist()
{
}

bool __stdcall CIntMapPersist::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IIntMapPersist"))
        || (0 == strcmp(interfacename, "CIntMapPersist")))
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

bool __stdcall CIntMapPersist::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CIntMapPersist* pNew = new CIntMapPersist;
    map<long, IPersistPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        IObjPtr pNewObj;
        CLONE_PTR(it->second, pNewObj)
        IPersistPtr pNewPersist;
        CAST_PTR(pNewObj, pNewPersist, IPersist)
        pNew->m_map[it->first] = pNewPersist;
        it++;
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CIntMapPersist::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_map.size();
    ps->Write(size);
    map<long, IPersistPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        ps->Write(it->first);

        bool flag = true;
        if (it->second.Assigned())
        {
            ps->WriteBool(flag);
            it->second->_DumpTo(pStream, assist);
        }
        else
        {
            flag = false;
            ps->WriteBool(flag);
        }

        it++;
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CIntMapPersist::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_map.clear();
    long size;
    ps->Read(size);
    for (long i = 0; i < size; i++)
    {
        long key;
        ps->Read(key);

        bool flag;
        ps->ReadBool(flag);
        IPersistPtr pObj;
        if (flag)
        {
            CPersistPtr pPersist;
            CPersist::_InstantiateFrom(ps, pPersist, assist);
            pObj = (IPersist*)pPersist._p();
        }

        m_map[key] = pObj;
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CIntMapPersist::Set(const long key, const IPersist* const pValue)
{
    IPersistPtr pObj = (IPersist*)pValue;
    m_map[key] = pObj;
    return true;
}

bool __stdcall CIntMapPersist::Get(const long key, IPersist** ppValue) const
{
    if (_invalid(ppValue)) return false;
    assert(!*ppValue);
    *ppValue = NULL;

    map<long, IPersistPtr>::const_iterator it = m_map.find(key);
    if (it == m_map.end()) return false;
    if (it->second.Assigned())
    {
        *ppValue = it->second._p();
        (*ppValue)->_AddRef();
    }

    return true;
}

bool __stdcall CIntMapPersist::GetAt(const dword index, long& key, IPersist** ppValue) const
{
    if (_invalid(ppValue)) return false;
    assert(!*ppValue);
    *ppValue = NULL;

    if (m_map.size() <= index) return false;
    map<long, IPersistPtr>::const_iterator it = m_map.begin();
    std::advance(it, index);
    key = it->first;
    if (it->second.Assigned())
    {
        *ppValue = it->second._p();
        (*ppValue)->_AddRef();
    }
    return true;
}

void __stdcall CIntMapPersist::GetKeys(IIntArray** ppKeys) const
{
    if (_invalid(ppKeys)) return;
    assert(!*ppKeys);

    *ppKeys = new CIntArray;
    map<long, IPersistPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppKeys)->Add(it->first);
        it++;
    }

    (*ppKeys)->_AddRef();
}

void __stdcall CIntMapPersist::GetValues(IPersistArray** ppValues) const
{
    if (_invalid(ppValues)) return;
    assert(!*ppValues);

    *ppValues = new CPersistArray;
    map<long, IPersistPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppValues)->Add(it->second._p());
        it++;
    }

    (*ppValues)->_AddRef();
}

void __stdcall CIntMapPersist::Clear()
{
    m_map.clear();
}

dword __stdcall CIntMapPersist::GetSize() const
{
    return m_map.size();
}


CStringMapObj::CStringMapObj()
{
    INIT_REFCOUNT
}

CStringMapObj::~CStringMapObj()
{
}

bool __stdcall CStringMapObj::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IStringMapObj"))
        || (0 == strcmp(interfacename, "CStringMapObj")))
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

bool __stdcall CStringMapObj::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CStringMapObj* pNew = new CStringMapObj;
    map<string, IObjPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        IObjPtr pNewObj;
        CLONE_PTR(it->second, pNewObj)
        pNew->m_map[it->first] = pNewObj;
        it++;
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

bool __stdcall CStringMapObj::Set(const char* const key, const IObj* const pValue)
{
    IObjPtr pObj = (IObj*)pValue;
    m_map[key] = pObj;
    return true;
}

bool __stdcall CStringMapObj::Get(const char* const key, IObj** ppValue) const
{
    if (_invalid(ppValue)) return false;
    assert(!*ppValue);
    *ppValue = NULL;

    map<string, IObjPtr>::const_iterator it = m_map.find(key);
    if (it == m_map.end()) return false;
    if (it->second.Assigned())
    {
        *ppValue = it->second._p();
        (*ppValue)->_AddRef();
    }

    return true;
}

bool __stdcall CStringMapObj::GetAt(const dword index, IAnsiString** key, IObj** ppValue) const
{
    if (_invalid(key) || _invalid(ppValue)) return false;
    assert(!*key);
    assert(!*ppValue);
    *key = NULL;
    *ppValue = NULL;

    if (m_map.size() <= index) return false;
    map<string, IObjPtr>::const_iterator it = m_map.begin();
    std::advance(it, index);
    *key = new CAnsiString;
    (*key)->SetText(it->first.c_str());
    (*key)->_AddRef();
    if (it->second.Assigned())
    {
        *ppValue = it->second._p();
        (*ppValue)->_AddRef();
    }
    return true;
}

void __stdcall CStringMapObj::GetKeys(IStringArray** ppKeys) const
{
    if (_invalid(ppKeys)) return;
    assert(!*ppKeys);

    *ppKeys = new CStringArray;
    map<string, IObjPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppKeys)->Add(it->first.c_str());
        it++;
    }

    (*ppKeys)->_AddRef();
}

void __stdcall CStringMapObj::GetValues(IObjArray** ppValues) const
{
    if (_invalid(ppValues)) return;
    assert(!*ppValues);

    *ppValues = new CObjArray;
    map<string, IObjPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppValues)->Add(it->second._p());
        it++;
    }

    (*ppValues)->_AddRef();
}

void __stdcall CStringMapObj::Clear()
{
    m_map.clear();
}

dword __stdcall CStringMapObj::GetSize() const
{
    return m_map.size();
}


CStringMapPersist::CStringMapPersist()
{
    INIT_REFCOUNT
}

CStringMapPersist::~CStringMapPersist()
{
}

bool __stdcall CStringMapPersist::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IStringMapPersist"))
        || (0 == strcmp(interfacename, "CStringMapPersist")))
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

bool __stdcall CStringMapPersist::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    CStringMapPersist* pNew = new CStringMapPersist;
    map<string, IPersistPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        IObjPtr pNewObj;
        CLONE_PTR(it->second, pNewObj)
        IPersistPtr pNewPersist;
        CAST_PTR(pNewObj, pNewPersist, IPersist)
        pNew->m_map[it->first] = pNewPersist;
        it++;
    }

    *ppObj = (IObj*)pNew;
    (*ppObj)->_AddRef();

    return true;
}

dword __stdcall CStringMapPersist::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    long size = m_map.size();
    ps->Write(size);
    map<string, IPersistPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        ps->Write(it->first);

        bool flag = true;
        if (it->second.Assigned())
        {
            ps->WriteBool(flag);
            it->second->_DumpTo(pStream, assist);
        }
        else
        {
            flag = false;
            ps->WriteBool(flag);
        }

        it++;
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CStringMapPersist::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_map.clear();
    long size;
    ps->Read(size);
    for (long i = 0; i < size; i++)
    {
        string key;
        ps->Read(key);

        bool flag;
        ps->ReadBool(flag);
        IPersistPtr pObj;
        if (flag)
        {
            CPersistPtr pPersist;
            CPersist::_InstantiateFrom(ps, pPersist, assist);
            pObj = (IPersist*)pPersist._p();
        }

        m_map[key] = pObj;
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CStringMapPersist::Set(const char* const key, const IPersist* const pValue)
{
    IPersistPtr pObj = (IPersist*)pValue;
    m_map[key] = pObj;
    return true;
}

bool __stdcall CStringMapPersist::Get(const char* const key, IPersist** ppValue) const
{
    if (_invalid(ppValue)) return false;
    assert(!*ppValue);
    *ppValue = NULL;

    map<string, IPersistPtr>::const_iterator it = m_map.find(key);
    if (it == m_map.end()) return false;
    if (it->second.Assigned())
    {
        *ppValue = it->second._p();
        (*ppValue)->_AddRef();
    }

    return true;
}

bool __stdcall CStringMapPersist::GetAt(const dword index, IAnsiString** key, IPersist** ppValue) const
{
    if (_invalid(key) || _invalid(ppValue)) return false;
    assert(!*key);
    assert(!*ppValue);
    *key = NULL;
    *ppValue = NULL;

    if (m_map.size() <= index) return false;
    map<string, IPersistPtr>::const_iterator it = m_map.begin();
    std::advance(it, index);
    *key = new CAnsiString;
    (*key)->SetText(it->first.c_str());
    (*key)->_AddRef();
    if (it->second.Assigned())
    {
        *ppValue = it->second._p();
        (*ppValue)->_AddRef();
    }
    return true;
}

void __stdcall CStringMapPersist::GetKeys(IStringArray** ppKeys) const
{
    if (_invalid(ppKeys)) return;
    assert(!*ppKeys);

    *ppKeys = new CStringArray;
    map<string, IPersistPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppKeys)->Add(it->first.c_str());
        it++;
    }

    (*ppKeys)->_AddRef();
}

void __stdcall CStringMapPersist::GetValues(IPersistArray** ppValues) const
{
    if (_invalid(ppValues)) return;
    assert(!*ppValues);

    *ppValues = new CPersistArray;
    map<string, IPersistPtr>::const_iterator it = m_map.begin();
    while (it != m_map.end())
    {
        (*ppValues)->Add(it->second._p());
        it++;
    }

    (*ppValues)->_AddRef();
}

void __stdcall CStringMapPersist::Clear()
{
    m_map.clear();
}

dword __stdcall CStringMapPersist::GetSize() const
{
    return m_map.size();
}

}