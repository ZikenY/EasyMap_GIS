#include "Persist.h"
#include "ClassFactory.h"

namespace easymap
{

dword CPersist::_InstantiateFrom(CStreamPtr pStream, CPersistPtr& pPersist, void* const assist)
{
    dword oldpos = pStream->GetPos();

    dword objid;
    pStream->Read(objid);

    _persistloadassist* pa = (_persistloadassist*)assist;
    if (_valid(assist) && (0 < objid))
    {
        pPersist = pa->GetInstance(objid);
        if (pPersist.Assigned())
        {
            return sizeof(dword);
        }
    }

    long len = 0;
    pStream->Read(len);

    static char buff[1000];
    pStream->Read(buff, len*sizeof(char));
    buff[len] = 0;

    IObjPtr pObj;
    _FactoryManager::CreateInstance(buff, pObj);
    CAST_PTR(pObj, pPersist, CPersist)
    pPersist->_LoadInstance((IStreamX*)pStream._p(), assist);

    if (0 < objid)
    {
        pa->Add(objid, pPersist);
    }

    return pStream->GetPos() - oldpos;
}

dword CPersist::Instantiate(CStreamPtr pStream, CPersistPtr& pPersist)
{
    _persistloadassist assist;

    dword r = CPersist::_InstantiateFrom(pStream, pPersist, &assist);
    return r;
}

_persistsaveassist::_persistsaveassist()
{
    m_NextID = 1;
}

_persistsaveassist::~_persistsaveassist()
{
    this->Clear();
}

dword _persistsaveassist::Add(const CPersistPtr pPersist)
{
    map<CPersist*, dword>::const_iterator it = m_InstanceMap.find(pPersist._p());
    if (it != m_InstanceMap.end())
        return 0;

    m_InstanceMap[pPersist._p()] = m_NextID;
    pPersist->_AddRef();
    return m_NextID++;
}

dword _persistsaveassist::GetID(const CPersistPtr pPersist) const
{
    map<CPersist*, dword>::const_iterator it = m_InstanceMap.find(pPersist._p());
    if (it != m_InstanceMap.end())
    {
        return it->second;
    }
    else
    {
        return 0;
    }
}

void _persistsaveassist::Clear()
{
    map<CPersist*, dword>::iterator it = m_InstanceMap.begin();
    while (it != m_InstanceMap.end())
    {
        it->first->_Release();
        it++;
    }

    m_InstanceMap.clear();
}

_persistloadassist::_persistloadassist()
{
}

_persistloadassist::~_persistloadassist()
{
    this->Clear();
}

bool _persistloadassist::Add(const dword id, const CPersistPtr pPersist)
{
    if (0 == id) return false;//该对象流不考虑重复引用问题

    map<dword, CPersist*>::const_iterator it = m_InstanceMap.find(id);
    if (it != m_InstanceMap.end())
        return false;

    m_InstanceMap[id] = pPersist._p();
    pPersist->_AddRef();
    return true;
}

CPersistPtr _persistloadassist::GetInstance(const dword id) const
{
    map<dword, CPersist*>::const_iterator it = m_InstanceMap.find(id);
    if (it != m_InstanceMap.end())
    {
        return it->second;
    }
    else
    {
        return 0;
    }
}

void _persistloadassist::Clear()
{
    map<dword, CPersist*>::iterator it = m_InstanceMap.begin();
    while (it != m_InstanceMap.end())
    {
        it->second->_Release();
        it++;
    }

    m_InstanceMap.clear();
}

}
