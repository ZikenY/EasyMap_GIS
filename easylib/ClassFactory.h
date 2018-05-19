#if !defined(CLASSFACTORY_INCLUDE_)
#define CLASSFACTORY_INCLUDE_

//================================================================================
//  这个单元用来提供对象的自动创建功能（通过类名字符串创建对象）
//      ※ 在头文件中加入CLASS_FACTORY(class1)
//      ※ 在cpp文件中加入CLASS_FACTORY_INSTANCE(class1)
//      ※ 就可以让class1支持自动创建
//      创建方法是：_FactoryManager::CreateInstance("class1", pObj);
//================================================================================

#include "..\\include\\InterfaceObj.h"

#pragma warning(disable: 4786)
#include <map>
#include <string>
#include <vector>
using namespace std;

namespace easymap
{

class _FactoryManager;

//类工厂的基类
class _ClassFactory
{
protected:
    char* m_ClassName;  //本类工厂要生成的class名称
    bool m_Registered;  //是否已经注册到_FactoryManager中

public:
    //取得对应的类名称（字符串）
    const char* const _GetClassName() const;

    //虫灾此方法，完成对象的创建
    virtual void CreateInstance(IObjPtr& pObj) const = 0;

friend class _FactoryManager;
};

//这个宏用在头文件中，自动生成classname的类工厂class，派生自_ClassFactory
//类工厂为单体类型，其构造函数自动将自己注册到_FactoryManager中
#define CLASS_FACTORY(classname)\
class _ClassFactory_##classname : public _ClassFactory\
{\
    static _ClassFactory_##classname classname##instance;\
    _ClassFactory_##classname();\
    void CreateInstance(IObjPtr& pObj) const;\
};

//这个宏用在cpp文件中，用来生成类工厂的实例
#define CLASS_FACTORY_INSTANCE(classname)\
_ClassFactory_##classname::_ClassFactory_##classname()\
{\
    m_Registered = false;\
    m_ClassName = #classname;\
    _FactoryManager& pool = _FactoryManager::GetFactoryPool();\
    m_Registered = pool.RegisterClassFactory(*this);\
}\
void _ClassFactory_##classname::CreateInstance(IObjPtr& pObj) const\
{\
    classname* p = new classname;\
    IObj* pO = NULL;\
    p->GotoInterface("IObj", (void**)&pO);\
    pObj = pO;\
    pObj->_Release();\
}\
_ClassFactory_##classname _ClassFactory_##classname::classname##instance;

//单体类型，用来管理所有已注册的类工厂，并提供自动创建对象的函数
class _FactoryManager
{
private:
    _FactoryManager();
public:
    ~_FactoryManager();

private:
    map<string, _ClassFactory*> m_Factorys;

public:
    bool RegisterClassFactory(_ClassFactory& factory);
    long GetRegisteredClassCount() const;
    void GetRegisteredClassNames(vector<string>& classnames) const;
    _ClassFactory* GetObjectFactory(const char* classname) const;

    //此函数保证了唯一实例
    static _FactoryManager& GetFactoryPool();
    //传入类名创建对象
    static void CreateInstance(const char* classname, IObjPtr& pObj);
};

}

#endif
