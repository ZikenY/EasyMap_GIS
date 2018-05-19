#include "ClassFactory.h"

namespace easymap
{

const char* const _ClassFactory::_GetClassName() const
{
    return m_ClassName;
}

_FactoryManager::_FactoryManager()
{
}

_FactoryManager::~_FactoryManager()
{
}

bool _FactoryManager::RegisterClassFactory(_ClassFactory& factory)
{
    if (factory.m_Registered) return false;

    string classname = factory.m_ClassName;
    m_Factorys[classname] = &factory;
    return true;
}

long _FactoryManager::GetRegisteredClassCount() const
{
    return m_Factorys.size();
}

void _FactoryManager::GetRegisteredClassNames(vector<string>& classnames) const
{
    classnames.clear();
    map<string, _ClassFactory*>::const_iterator it = m_Factorys.begin();
    while (it != m_Factorys.end())
    {
        classnames.push_back(it->first);
    }
}

_ClassFactory* _FactoryManager::GetObjectFactory(const char* classname) const
{
    map<string, _ClassFactory*>::const_iterator it = m_Factorys.find(classname);
    if (it == m_Factorys.end())
    {
        return NULL;
    }
    return it->second;
}

_FactoryManager& _FactoryManager::GetFactoryPool()
{
    static _FactoryManager pool;
    return pool;
}

void _FactoryManager::CreateInstance(const char* classname, IObjPtr& pObj)
{
    _FactoryManager& pool = _FactoryManager::GetFactoryPool();

    map<string, _ClassFactory*>::const_iterator it = pool.m_Factorys.find(classname);
    if (it == pool.m_Factorys.end())
    {
        pObj.Clear();
        return;
    }

    it->second->CreateInstance(pObj);
}

}