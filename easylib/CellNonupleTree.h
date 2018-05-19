#if !defined(CELLNONUPLETREE_INCLUDED_)
#define CELLNONUPLETREE_INCLUDED_

#include "..\\include\\WKSInclude.h"
#include "ClassFactory.h"
#include "Persist.h"

#pragma warning(disable: 4786)
#include <map>
#include <list>
using namespace std;

namespace easymap
{

class CCellNonupleTree;
typedef TSmartPtr<CCellNonupleTree> CCellNonupleTreePtr;

//================================================================================
//  九宫格(九叉树)检索算法，用来搜索矩形范围内的对象
//  这个算法会优先搜索矩形的中心区域，这是和四叉树不同的地方
//================================================================================
class CCellNonupleTree : public IPersist
{
CLASS_NAME(CCellNonupleTree)
PERSIST_DUMP(CCellNonupleTree)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    typedef struct
    {
        WKSRect     extent;
        long        level;
    }CNTParams;

    typedef enum
    {
        NODELEAF_CENTER             = 1,
        NODELEAF_UP                 = 2,
        NODELEAF_DOWN               = 3,
        NODELEAF_LEFT               = 4,
        NODELEAF_RIGHT              = 5,
        NODELEAF_LEFTUP             = 6,
        NODELEAF_RIGHTUP            = 7,
        NODELEAF_LEFTDOWN           = 8,
        NODELEAF_RIGHTDOWN          = 9,
    }NodeLeafEnum;

    class CNonupleNode;

    //查询结果，具体返回什么东西由派生类决定
    class CCellNonupleResult : public IObj
    {
    protected:
        virtual ~CCellNonupleResult(){};
    };
    typedef TSmartPtr<CCellNonupleResult> CCellNonupleResultPtr;

    //查询过滤器，实现Search方法来决定具体查询方法
    //注意要重载Clone()方法
    class CCellNonupleFilter : public IPersist
    {
    protected:
        virtual ~CCellNonupleFilter(){};

    private:
        //  由CNonupleNode::Search()调用，判断查询点和extent是否match
        //  返回值  true：  在extent中，不必转向同级的下一节点
        //          false:  不再extent中，应检索同级的下一节点
        virtual bool CheckExtent(const WKSRect& extent, const long level) const = 0;

        //  判断在该Cell中是否查询成功，由CNonupleNode::Search()调用
        //  返回查询结果
        virtual void Search(const double& x, const double& y, const long level,
            CCellNonupleResultPtr& pResult) const = 0;

    friend class CNonupleNode;
    };
    typedef TSmartPtr<CCellNonupleFilter> CCellNonupleFilterPtr;

    //  九叉树节点
    class CNonupleNode
    {
        typedef struct
        {
            CNonupleNode* pLeaf_Center;
            CNonupleNode* pLeaf_Up;
            CNonupleNode* pLeaf_Down;
            CNonupleNode* pLeaf_Left;
            CNonupleNode* pLeaf_Right;
            CNonupleNode* pLeaf_LeftUp;
            CNonupleNode* pLeaf_RightUp;
            CNonupleNode* pLeaf_LeftDown;
            CNonupleNode* pLeaf_RightDown;

            WKSRect Extent_Center;
            WKSRect Extent_Up;
            WKSRect Extent_Down;
            WKSRect Extent_Left;
            WKSRect Extent_Right;
            WKSRect Extent_LeftUp;
            WKSRect Extent_RightUp;
            WKSRect Extent_LeftDown;
            WKSRect Extent_RightDown;
        }CNodeLeafs;    //节点下的9个叶节点

        CNonupleNode();
        CNonupleNode(const CNonupleNode& nonuplenode);
        CNonupleNode(const long level, const WKSRect& extent, const WKSPoint& region_center);
        ~CNonupleNode();

        //节点的属性
        long            m_Level;
        WKSRect         m_Extent;
        CNodeLeafs      m_Leafs;
        double          m_Dist_to_center;   //该节点距离区域中心的距离

        //  将本节点的m_Extent一分为9，增加9个子节点。
        //  如果sublevel>1，其子节点就根据sublevel - 1大小
        //  继续分下去。注意如果本节点不是最终叶节点（已经包
        //  含子节点）或sublevel<1，则本函数调用失败
        void SplitNode(const long sublevel, const WKSPoint& region_center);

        //  用本节点的m_Extent调用CCellNonupleFilter::Search()
        //  region_center：整个区域的中心
        //  pSearchResult为空：查询失败
        void Search(const CCellNonupleFilterPtr pFilter, CCellNonupleResultPtr& pResult,
            const bool dosearch) const;

    friend class CCellNonupleTree;
    };

private:
    CCellNonupleTree();

public:
    CCellNonupleTree(const CNTParams& paramerters);

private:
    ~CCellNonupleTree();

    CNTParams               m_Parameters;
    CNonupleNode*           m_pRoot;
    CCellNonupleFilterPtr   m_pFilter;

private:
    //  干掉所有的咚咚，由LoadFromStream调用
    void Reset();

    //  由构造函数调用，构建九叉树内存结构
    bool Initial(const CNTParams& paramerters);

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

    void GetParameters(CNTParams& paramerters) const;
    void SetFilter(const CCellNonupleFilterPtr pFilter);
    void Search(CCellNonupleResultPtr& pResult) const;

};
//================================================================================

CLASS_FACTORY(CCellNonupleTree)
}

#endif
