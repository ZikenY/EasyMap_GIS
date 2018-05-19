#if !defined(INTERFACEOBJ_INCLUDE_)
#define INTERFACEOBJ_INCLUDE_

//================================================================================
//                                                                               *
//  IObj为所有接口的原型，定义两件事：                                           *
//      1> 引用计数自管理；                                                      *
//      2> 自定义事件的分派与监听；                                              *
//                                                                               *
//  为了兼容Delphi的接口，IObj没有定义任何成员，所有虚函数的通用实现代码用宏的   *
//      形式安插到后面实现类的定义中；                                           *
//  IObj的第一个虚函数只是占位用，无意义，目的也是为了兼容Delphi的接口           *
//                                                                               *
//================================================================================

#include "BasicType.h"
#include <assert.h>

#pragma warning(disable: 4786)

namespace easymap
{

//================================================================================
//  智能指针
//================================================================================
template<typename T>
struct TSmartPtr
{
    __stdcall TSmartPtr(){obj_pointed_to = NULL;};

    __stdcall TSmartPtr(T* pObj){obj_pointed_to = pObj; this->_AddRef_();};

    __stdcall TSmartPtr(const TSmartPtr& obj){
        obj_pointed_to = obj.obj_pointed_to;
        this->_AddRef_();};

    __stdcall ~TSmartPtr(){this->_Release_();};

    inline TSmartPtr<T>& __stdcall operator=(const TSmartPtr<T>& rhs){
        if (this == &rhs) return *this;
        this->_Release_();
        obj_pointed_to = rhs.obj_pointed_to;
        this->_AddRef_();
        return *this;};

    inline TSmartPtr<T>& __stdcall operator=(T* pObj){
        if (obj_pointed_to == pObj) return *this;
        this->_Release_();
        obj_pointed_to = pObj;
        this->_AddRef_();
        return *this;};

    inline T* __stdcall operator->() const{return obj_pointed_to;};

    inline T& __stdcall operator*() const{return *obj_pointed_to;};

    inline bool __stdcall operator==(const TSmartPtr<T>& obj) const{
        return (obj_pointed_to == obj.obj_pointed_to) ? true : false;};

    inline bool __stdcall operator==(const T* const p) const{
        return (p == obj_pointed_to) ? true : false;};

    inline bool __stdcall operator!=(const TSmartPtr<T>& obj) const{
        return (obj_pointed_to != obj.obj_pointed_to) ? true : false;};

    inline bool __stdcall operator!=(const T* const p) const{
        return (p != obj_pointed_to) ? true : false;};

    inline void __stdcall Clear(){
        this->_Release_();
        obj_pointed_to = NULL;};

    inline bool __stdcall Assigned() const{return (NULL != obj_pointed_to) ? true : false;};

    inline T* __stdcall _p() const{return obj_pointed_to;};

    inline T** __stdcall _ref(){return &obj_pointed_to;};

    inline bool __stdcall Compare(const TSmartPtr<T>& obj) const{
        return (this->obj_pointed_to == obj.obj_pointed_to) ? true : false;};

private:
    inline dword __stdcall _AddRef_(){
        if (NULL != obj_pointed_to) return obj_pointed_to->_AddRef();
        else return 0;};

    inline dword __stdcall _Release_(){
        if (NULL != obj_pointed_to){
            long r = obj_pointed_to->_Release();
            if (0 == r) obj_pointed_to = NULL;
            return r;}
        else return 0;};

    T* obj_pointed_to;
};
//================================================================================


typedef long event_identity;    //事件（消息）类型
const event_identity EVENT_IDENTITY_NULL   = 0;


class IObj;
typedef TSmartPtr<IObj> IObjPtr;
class IObjArray;
typedef TSmartPtr<IObjArray> IObjArrayPtr;


//================================================================================
//  用引用计数来控制派生类的生死
//  解决对象之间消息传递的问题
//================================================================================
class IObj
{
protected:
    struct _donottouchme
    {
        dword   Data1;
        word    Data2;
        word    Data3;
        byte    Data4[8];
    };
public:
    //  占位用，为了兼容Delphi的接口
    virtual long __stdcall _placeholder_queryinterface(const _donottouchme iid, void** pp) = 0;

    //-----------------------------------------------------------------------------------
    //  引用计数管理
    //  一般情况下不用手工虫灾，只需将CLASS_NAME(classname)宏加在具体类声明中即可
    //-----------------------------------------------------------------------------------
    virtual dword __stdcall _AddRef() = 0;
    virtual dword __stdcall _Release() = 0;
    virtual dword __stdcall _Debug() = 0;
    //-----------------------------------------------------------------------------------


    //-----------------------------------------------------------------------------------
    //  基于类名称（字符串）的接口查询
    virtual bool __stdcall GotoInterface(const char* const interfacename, void** pp) = 0;
    //-----------------------------------------------------------------------------------


    //-----------------------------------------------------------------------------------
    //  得到类名称“字符串”
    //  一般情况下不用手工虫灾，只需将CLASS_NAME(classname)宏加在具体类声明中即可
    virtual const char* const __stdcall _GetClassName() const = 0;
    //-----------------------------------------------------------------------------------


    //-----------------------------------------------------------------------------------
    //  作为[被监听者]，管理[监听者]列表，在派生类声明中加入EVENTS_DISPATCHER宏来改写这些函数；
    //
    //  注意[被监听者]保存每个[监听者]的raw reference（without ref-counting），目的是避免
    //      循环引用；
    //
    //  每个[监听者]对象中都保存一个[被监听者]的引用（with ref-counting）；
    //  每个[监听者]必须在析构函数中将自己从[被监听者]的监听列表中移出
    //-----------------------------------------------------------------------------------

    //  得到所有待监听的事件数量
    virtual dword __stdcall GetEventsCount() const
    {
        return 0;           //等待被EVENTS_DISPATCHER改写
    };

    //  增加事件
    virtual bool __stdcall AddEvent(const event_identity ei)
    {
        return false;       //等待被EVENTS_DISPATCHER改写
    };

    //  删除事件，注意如果有监听者监听该事件，就不能删除
    virtual bool __stdcall RemoveEvent(const event_identity ei)
    {
        return false;           //等待被EVENTS_DISPATCHER改写
    };

    //  得到待监听的事件
    virtual event_identity __stdcall GetEvent(const dword index) const
    {
        return EVENT_IDENTITY_NULL; //等待被EVENTS_DISPATCHER改写
    };

    //  注册事件的监听者，事件类型由ei区分。
    //  由监听者对象的RegisterToDispatcher()方法调用
    virtual bool __stdcall _RegisterListener(const event_identity ei,
        const IObj* const pListener)
    {
        return false;       //等待被EVENTS_DISPATCHER改写
    };

    //  反注册事件的监听者，事件类型由ei区分。
    //  由监听者对象的UnregisterFromDispatcher()方法调用
    virtual bool __stdcall _UnregisterListener(const event_identity ei,
        const IObj* const pListener)
    {
        return false;       //等待被EVENTS_DISPATCHER改写
    };

    //  得到监听某一事件的全部监听者
    //  如果事件不存在，返回false
    virtual bool __stdcall _GetListenersByEvent(const event_identity ei,
        IObjArray** ppListeners) const
    {
        return 0;
    }

    //  获得某一监听者所监听的所有事件数量。
    //  由监听者调用，用于决定是否能将被监听者从监听者的m_Dispatchers列表中清除
    virtual dword __stdcall _GetEventCountByListener(const IObj* const pListener) const
    {
        return 0;           //等待被EVENTS_DISPATCHER改写
    };

    //  踢掉某一监听者的所有监听，由监听者析构函数调用
    virtual bool __stdcall _KickAssListener(const IObj* const pListener)
    {
        return false;       //等待被EVENTS_DISPATCHER改写
    };
    //-----------------------------------------------------------------------------------



    //-----------------------------------------------------------------------------------
    //  作为[监听者]，在派生类声明中加入EVENTS_LISTENER宏来改写下列函数；
    //
    //  注意[监听者]保存了一个[被监听者]列表m_Dispatchers（with ref-counting）， 析构时会
    //      向每个[被监听者]发出kick-my-ass命令，这一点非常重要，必须将KICKASS_LISTENER_FROM_DISPATCHERS
    //      宏添加到每一个[监听者]的析构函数中
    //-----------------------------------------------------------------------------------

    //  调用pDispatcher->_RegisterListener()，将监听者对象注册到被监听者的某一事件中
    //  同时将pDispatcher的引用保存到m_Dispatchers列表中（with ref-counting）
    virtual bool __stdcall RegisterToDispatcher(const event_identity ei, IObj* pDispatcher)
    {
        return false;       //等待被EVENTS_LISTENER改写
    };

    //  调用pDispatcher->_KickListener()，将监听者对象从被监听者的某一事件中移除
    //  如果pDispatcher不再有任何事件需要监听，则将pDispatcher移出m_Dispatchers列表
    virtual bool __stdcall UnregisterFromDispatcher(const event_identity ei, IObj* pDispatcher)
    {
        return false;       //等待被EVENTS_LISTENER改写
    };

    //  将监听者对象从被监听者的所有事件中移除，并将pDispatcher移出m_Dispatchers列表
    //  用于解除监听者与被监听者的所有监听关系
    virtual bool __stdcall UnregisterAllFromDispatcher(IObj* pDispatcher)
    {
        return false;       //等待被EVENTS_LISTENER改写
    };

    //  由派生类自行改写此函数，接收[被监听者]的调用
    //      ei                  -       区分事件类型，自行定义
    //      pMessage            -       消息内容
    //      tag                 -       用于传出单个数值
    //      message_description -       消息描述，用于传出简单的信息
    virtual bool __stdcall DispatchMessage(const event_identity ei, const IObj* pMessage,
        const easy_variant& tag, const char* const message_description)
    {
        return false;       //===  等待用户自己改写  ====
    };
    //-----------------------------------------------------------------------------------


    //-----------------------------------------------------------------------------------
    //  克隆自己，如果不支持就返回false和null对象
    virtual bool __stdcall Clone(IObj** ppObj) const = 0;
    //-----------------------------------------------------------------------------------
};
//================================================================================


//================================================================================
//  对象数组，用于在接口参数中传出一组对象
//================================================================================
class IObjArray : public IObj
{
public:
    virtual bool __stdcall Add(const IObj* pObj) = 0;
    virtual bool __stdcall SetAt(const dword index, const IObj* const pObj) = 0;
    virtual bool __stdcall GetAt(const dword index, IObj** ppObj) const = 0;
    virtual void __stdcall Clear() = 0;
    virtual bool __stdcall Resize(const dword newsize) = 0;
    virtual dword __stdcall GetSize() const = 0;
};
//================================================================================


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//1.    这个宏加在每个具体类声明中，用来自动改写_GetClassName()函数
//      并且允许类厂访问具体类的私有成员，有点bt
//2.    实现引用计数，注意，刚创建完后引用计数是0
#define CLASS_NAME(classname) \
protected:\
    dword m_RefCount;\
public:\
    long __stdcall _placeholder_queryinterface(const _donottouchme iid, void** pp)\
    {\
        throw;\
    };\
    const char* const __stdcall _GetClassName() const\
    {\
        return #classname;\
    };\
    dword __stdcall _AddRef()\
    {\
        return ++m_RefCount;\
    };\
    dword __stdcall _Release()\
    {\
        m_RefCount--;\
        if (m_RefCount)\
        {\
            return m_RefCount;\
        }\
        else\
        {\
            delete this;\
            return 0;\
        }\
    };\
    dword __stdcall _Debug()\
    {\
        return m_RefCount;\
    }\
friend class _ClassFactory_##classname;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//注意这个要放在派生类的[每个]构造函数中
//确保对象初始化的时候引用计数为0
#define INIT_REFCOUNT this->m_RefCount = 0;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//放在类声明中，加入事件发布者（被监听者）功能
#define EVENTS_DISPATCHER \
private:\
    std::map<event_identity, std::list<const IObj*> > m_EventListeners;\
public:\
    dword __stdcall GetEventsCount() const\
    {\
        return m_EventListeners.size();\
    };\
    bool __stdcall AddEvent(const event_identity ei)\
    {\
        std::map<event_identity, std::list<const IObj*> >::const_iterator it_events = m_EventListeners.find(ei);\
        if (it_events != m_EventListeners.end()) return false;\
        std::list<const IObj*> objlist;\
        m_EventListeners[ei] = objlist;\
        return true;\
    };\
    bool __stdcall RemoveEvent(const event_identity ei)\
    {\
        std::map<event_identity, std::list<const IObj*> >::iterator it_events = m_EventListeners.find(ei);\
        if (it_events == m_EventListeners.end()) return false;\
        m_EventListeners.erase(it_events);\
        return true;\
    };\
    event_identity __stdcall GetEvent(const dword index) const\
    {\
        if (m_EventListeners.size() <= index) return EVENT_IDENTITY_NULL;\
        std::map<event_identity, std::list<const IObj*> >::const_iterator it_events = m_EventListeners.begin();\
        std::advance(it_events, index);\
        return it_events->first;\
    };\
    bool __stdcall _RegisterListener(const event_identity ei, const IObj* const pListener)\
    {\
        if (!pListener) return false;\
        std::map<event_identity, std::list<const IObj*> >::const_iterator it_events = m_EventListeners.find(ei);\
        if (it_events == m_EventListeners.end()) return false;\
        std::list<const IObj*> objlist = it_events->second;\
        std::list<const IObj*>::const_iterator it_objlist = objlist.begin();\
        while (it_objlist != objlist.end())\
        {\
            if (*it_objlist == pListener)\
            {\
                return false;\
            }\
            it_objlist++;\
        }\
        objlist.push_back(pListener);\
        m_EventListeners[ei] = objlist;\
        return true;\
    };\
    bool __stdcall _UnregisterListener(const event_identity ei, const IObj* const pListener)\
    {\
        if (!pListener) return false;\
        std::map<event_identity, std::list<const IObj*> >::const_iterator it_events = m_EventListeners.find(ei);\
        if (it_events == m_EventListeners.end()) return false;\
        std::list<const IObj*> objlist = it_events->second;\
        std::list<const IObj*>::iterator it_objlist = objlist.begin();\
        while (it_objlist != objlist.end())\
        {\
            if (*it_objlist == pListener)\
            {\
                objlist.erase(it_objlist);\
                m_EventListeners[ei] = objlist;\
                return true;\
            }\
            it_objlist++;\
        }\
        return false;\
    };\
    bool __stdcall _GetListenersByEvent(const event_identity ei, IObjArray** ppListeners) const\
    {\
        if (!ppListeners) return false;\
        *ppListeners = NULL;\
        std::map<event_identity, std::list<const IObj*> >::const_iterator it_events = m_EventListeners.find(ei);\
        if (it_events == m_EventListeners.end()) return false;\
        IObjPtr pObj;\
        _FactoryManager::CreateInstance("CObjArray", pObj);\
        pObj->GotoInterface("IObjArray", (void**)ppListeners);\
        std::list<const IObj*> objlist = it_events->second;\
        std::list<const IObj*>::iterator it_objlist = objlist.begin();\
        while (it_objlist != objlist.end())\
        {\
            (*ppListeners)->Add(*it_objlist);\
            it_objlist++;\
        }\
        return true;\
    };\
    dword __stdcall _GetEventCountByListener(const IObj* const pListener) const\
    {\
        if (!pListener) return 0;\
        dword eventcount = 0;\
        std::map<event_identity, std::list<const IObj*> >::const_iterator it_events = m_EventListeners.begin();\
        while (it_events != m_EventListeners.end())\
        {\
            std::list<const IObj*> objlist = it_events->second;\
            std::list<const IObj*>::iterator it_objlist = objlist.begin();\
            while (it_objlist != objlist.end())\
            {\
                if (*it_objlist == pListener)\
                {\
                    eventcount++;\
                    break;\
                }\
                it_objlist++;\
            }\
            it_events++;\
        }\
        return eventcount;\
    };\
    bool __stdcall _KickAssListener(const IObj* const pListener)\
    {\
        bool r = false;\
        std::map<event_identity, std::list<const IObj*> >::const_iterator it_events = m_EventListeners.begin();\
        while (it_events != m_EventListeners.end())\
        {\
            bool flag = false;\
            std::list<const IObj*> objlist = it_events->second;\
            std::list<const IObj*>::iterator it_objlist = objlist.begin();\
            while (it_objlist != objlist.end())\
            {\
                if (*it_objlist == pListener)\
                {\
                    objlist.erase(it_objlist);\
                    flag = r = true;\
                    break;\
                }\
                it_objlist++;\
            }\
            if (flag) m_EventListeners[it_events->first] = objlist;\
            it_events++;\
        }\
        return r;\
    };
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//“被监听者”（发布者）增加一个监听事件类型
#define ADD_EVENTTYPE(ei) this->AddEvent(ei);

//向所有已注册的“监听者”发布一个事件消息
#define DISPATCH_MESSAGE_TOALL(ei, messageobj, tag, description) \
    {\
        IObjArrayPtr pListeners;\
        this->_GetListenersByEvent(ei, pListeners._ref());\
        assert(pListeners._p());\
        if (pListeners._p())\
        {\
            for (dword i = 0; i < pListeners->GetSize(); i++)\
            {\
                IObjPtr pListener;\
                pListeners->GetAt(i, pListener._ref());\
                pListener->DispatchMessage(ei, messageobj, tag, description);\
            }\
        }\
    }

//long tag
#define DISPATCH_LONG_TOALL(ei, tag_long) \
    {\
        easy_variant vtag;\
        vtag.value_long = tag_long;\
        DISPATCH_MESSAGE_TOALL(ei, NULL, vtag, NULL)\
    }

//double tag
#define DISPATCH_DOUBLE_TOALL(ei, tag_double) \
    {\
        easy_variant vtag;\
        vtag.value_double = tag_double;\
        DISPATCH_MESSAGE_TOALL(ei, NULL, vtag, NULL)\
    }

//不打算让对象拥有事件发布（被监听）功能
#define NO_EVENTS_DISPATCHER


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//放在类声明中，加入监听者功能，在每个监听者对象中保存被监听者的智能指针列表
//注意：这个宏必须和析构函数中的KICKASS_LISTENER_FROM_DISPATCHERS配对使用
#define EVENTS_LISTENER \
private:\
    std::list<IObjPtr> m_Dispatchers;\
public:\
    bool __stdcall RegisterToDispatcher(const event_identity ei, IObj* pDispatcher)\
    {\
        if (!ei || !pDispatcher) return false;\
        IObj* pThisObj = NULL;\
        this->GotoInterface("IObj", (void**)&pThisObj);\
        this->_Release();\
        if (!pDispatcher->_RegisterListener(ei, pThisObj)) return false;\
        std::list<IObjPtr>::const_iterator it = m_Dispatchers.begin();\
        while (it != m_Dispatchers.end())\
        {\
            if ((*it)._p() == pDispatcher) return true;\
            it++;\
        }\
        IObjPtr pD = pDispatcher;\
        m_Dispatchers.push_back(pD);\
        return true;\
    };\
    bool __stdcall UnregisterFromDispatcher(const event_identity ei, IObj* pDispatcher)\
    {\
        if (!ei || !pDispatcher) return false;\
        IObj* pThisObj = NULL;\
        this->GotoInterface("IObj", (void**)&pThisObj);\
        this->_Release();\
        pDispatcher->_UnregisterListener(ei, pThisObj);\
        if (pDispatcher->_GetEventCountByListener(pThisObj) > 0) return true;\
        std::list<IObjPtr>::iterator it = m_Dispatchers.begin();\
        while (it != m_Dispatchers.end())\
        {\
            if ((*it)._p() == pDispatcher)\
            {\
                m_Dispatchers.erase(it);\
                it = m_Dispatchers.begin();\
                continue;\
            }\
            it++;\
        }\
        return true;\
    };\
    bool __stdcall UnregisterAllFromDispatcher(IObj* pDispatcher)\
    {\
        for (dword i = 0; i < pDispatcher->GetEventsCount(); i++)\
        {\
            event_identity ei = pDispatcher->GetEvent(i);\
            this->UnregisterFromDispatcher(ei, pDispatcher);\
        }\
        return true;\
    };
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//向发布者注册监听某一事件
#define REGISTER_EVENT_TO_DISPATCHER(ei, dispatch)\
    {\
        IObjPtr pDispatcher;\
        CAST_PTR(dispatch, pDispatcher, IObj)\
        this->RegisterToDispatcher(ei, pDispatcher._p());\
    }

//反注册到发布者某一事件的监听
#define UNREGISTER_EVENT_TO_DISPATCHER(ei, dispatch)\
    {\
        IObjPtr pDispatcher;\
        CAST_PTR(dispatch, pDispatcher, IObj)\
        this->UnregisterFromDispatcher(ei, pDispatcher._p());\
    }

//反注册到发布者所有事件的监听，即切断listener和dispatcher之间的联系
#define UNREGISTER_ALLEVENT_TO_DISPATCHER(dispatch)\
    {\
        IObjPtr pDispatcher;\
        CAST_PTR(dispatch, pDispatcher, IObj)\
        this->UnregisterAllFromDispatcher(pDispatcher._p());\
    }

//不打算让对象拥有事件接收（监听）功能
#define NO_EVENTS_LISTENER


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//放在每个监听者析构函数中，使监听者对象在析构时向每个被监听者发出kick-my-ass命令
#define KICKASS_LISTENER_FROM_DISPATCHERS \
    IObj* pThisObj = NULL;\
    this->GotoInterface("IObj", (void**)&pThisObj);\
    this->m_RefCount--;\
    list<IObjPtr>::const_iterator it = m_Dispatchers.begin();\
    while (it != m_Dispatchers.end())\
    {\
        (*it)->_KickAssListener(pThisObj);\
        it++;\
    }\
    m_Dispatchers.clear();
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//通过接口名称查询接口
#define CAST_PTR(from_ptr, to_ptr, interface_name)\
    to_ptr.Clear();\
    from_ptr->GotoInterface(#interface_name, (void**)to_ptr._ref());
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//克隆操作，输出的是IObjPtr
#define CLONE_PTR(source_ptr, dest_IObjptr)\
    dest_IObjptr.Clear();\
    {\
        easymap::IObj* _point_to_iobj_ = NULL;\
        source_ptr->Clone(&_point_to_iobj_);\
        if (easymap::_valid(_point_to_iobj_))\
        {\
            dest_IObjptr = _point_to_iobj_;\
            _point_to_iobj_->_Release();\
        }\
    }
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#define RELEASE(p) if (_valid(p)) {p->_Release(); p = NULL;}


}

#endif