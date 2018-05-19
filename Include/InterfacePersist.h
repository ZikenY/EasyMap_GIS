#if !defined(INTERFACEPERSIST_INCLUDE_)
#define INTERFACEPERSIST_INCLUDE_

#include "InterfaceStream.h"

namespace easymap
{

class IPersist;
typedef TSmartPtr<IPersist> IPersistPtr;

class IPersist : public IObj
{
protected:
    //  本函数不管类型信息，直接写入对象内容
    virtual dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const = 0;

    //  本函数不读类型信息，直接读取对象内容
    virtual dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist) = 0;

public:
    //================================================================================
    //  本函数由父对象调用，最终客户端不要调用，
    //  由父对象将pAssist传递到子对象，解决对象的重复引用问题
    //  先向stream中写入对象ID、类型信息前缀，然后调用_SaveInstance()写入实际的对象数据
    //  如果pAssist == NULL则代表本对象不考虑重复引用问题
    //================================================================================
    virtual dword __stdcall _DumpTo(IStreamX* pStream, void* const assist) const = 0;

    //================================================================================
    //  本函数由客户端调用，代表一个完整的序列化过程
    //  本函数会创建一个_persistsaveassist传递给子对象
    //  自动处理子对象的重复引用问题
    //================================================================================
    virtual dword __stdcall Dump(IStreamX* pStream) const = 0;
};

}

#endif
