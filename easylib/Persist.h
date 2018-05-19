#if !defined(PERSIST_INCLUDE_)
#define PERSIST_INCLUDE_

//================================================================================
//  这个单元用来提供对象持久化功能
//  派生类改写_LoadInstance()和_SaveInstance函数存取实际对象内容
//  _DumpTo()和_InstantiateFrom()实现了对象的自动序列化和实例化功能
//  _DumpTo()先写入对象id和类型信息，然后调用_SaveInstance()写入实际对象内容
//  _InstantiateFrom()先读取对象id和类型信息，自动创建对象，然后调用_LoadInstance()
//  设置对象内容
//
//  _persistsaveassist和_persistloadassist利用“对象id”解决一个对象的重复引用问题
//  注意：“对象id”在每一次完整的序列化动作或实例化动作中保持唯一，约定“对象id”为0
//  则代表该对象流不考虑对象的重复引用问题
//
//  对象存储的流格式为：4（对象id）+ 2(类名称长度) + n(类名称) + 实际对象流
//
//  注意，CPersist类并没有增加任何虚函数或成员变量，因此可以当成IPersist来用
//================================================================================

#include "..\\include\\InterfacePersist.h"
#include "Stream.h"
#include <map>

namespace easymap
{

class CPersist;
typedef TSmartPtr<CPersist> CPersistPtr;

class _persistsaveassist;
class _persistloadassist;
class CPersist : public IPersist
{
public:
    //================================================================================
    //  本函数由父对象调用，最终客户端不要调用
    //  由父对象将assist传递到子对象，解决对象的重复引用问题
    //  先读取对象ID、类型信息，并自动创建对象，然后调用_LoadInstance()函数读取实际的对象数据
    //================================================================================
    static dword _InstantiateFrom(CStreamPtr pStream, CPersistPtr& pPersist,
        void* const assist);

    //================================================================================
    //  本函数由客户端调用，代表一个完整的实例化过程
    //  本函数会创建一个_persistloadassist传递给子对象
    //  自动处理子对象的重复引用问题
    //================================================================================
    static dword Instantiate(CStreamPtr pStream, CPersistPtr& pPersist);
};

//用来解决对象的重复引用问题
class _persistsaveassist
{
public:
    _persistsaveassist();
    ~_persistsaveassist();

    //  对象地址 < - > id映射
    std::map<CPersist*, dword> m_InstanceMap;
    dword m_NextID;

    //  返回0代表失败，该对象已经added
    dword Add(const CPersistPtr pPersist);

    //  返回0代表该对象还未add
    dword GetID(const CPersistPtr pPersist) const;

    void Clear();
};

//用来解决对象的重复引用问题
class _persistloadassist
{
public:
    _persistloadassist();
    ~_persistloadassist();

    //  id < - > 对象地址映射
    std::map<dword, CPersist*> m_InstanceMap;

    //  如果id == 0代表该对象流不考虑对象重复引用问题
    //  返回false代表该id已经added
    bool Add(const dword id, const CPersistPtr pPersist);

    //  返回null代表该id还未add
    CPersistPtr GetInstance(const dword id) const;

    void Clear();
};


//这个宏放在派生类声明中，帮助派生类改写_DumpTo()和Dump()
#define PERSIST_DUMP(classname)\
public:\
    dword __stdcall _DumpTo(IStreamX* pStream, void* const assist) const\
    {\
        dword oldpos = pStream->GetPos();\
        dword objid = 0;\
        if (_valid(assist))\
        {\
            CPersistPtr pPersist;\
            const_cast<classname*>(this)->GotoInterface("CPersist", (void**)pPersist._ref());\
            _persistsaveassist* pa = (_persistsaveassist*)assist;\
            objid = pa->GetID(pPersist);\
            if (0 == objid)\
            {\
                objid = pa->Add(pPersist);\
            }\
            else\
            {\
                pStream->WriteData(&objid, sizeof(dword));\
                return sizeof(dword);\
            }\
        }\
        pStream->WriteData(&objid, sizeof(dword));\
        const char* const pcClassName = this->_GetClassName();\
        long len = strlen(pcClassName);\
        pStream->WriteData(&len, sizeof(long));\
        pStream->WriteData(pcClassName, len*sizeof(char));\
        this->_SaveInstance(pStream, assist);\
        return pStream->GetPos() - oldpos;\
    };\
    dword __stdcall Dump(IStreamX* pStream) const\
    {\
        _persistsaveassist assist;\
        dword r = this->_DumpTo(pStream, &assist);\
        return r;\
    };

}

#endif
