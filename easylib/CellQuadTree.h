#if !defined(CELLQUADTREE_INCLUDED_)
#define CELLQUADTREE_INCLUDED_

//CSingleQuadTree是基于固定范围/固定格网划分的四叉树空间索引
//CMultiQuadTree是单层格网索引，每个格网由一个CSingleQuadTree组成

#include "SpatialIndex.h"

#pragma warning(disable: 4786)
#include <map>
#include <list>
using namespace std;

namespace easymap
{

class CCellQuadTree;
class CSingleQuadTree;
class CMultiQuadTree;

typedef TSmartPtr<CCellQuadTree> CCellQuadTreePtr;
typedef TSmartPtr<CSingleQuadTree> CSingleQuadTreePtr;
typedef TSmartPtr<CMultiQuadTree> CMultiQuadTreePtr;

//================================================================================
//  四叉树基类
//================================================================================
class CCellQuadTree : public CSpatialIndex
{
public:
    //四叉树空间索引的参数
    typedef struct
    {
        WKSPoint    centerpoint;    //整个索引范围的中心点
        double      BoundSize;      //索引区域的宽度
        long        treelevel;      //索引的级数
    }CQTParams;

protected:
    //--------------------------------------------------
    //  四叉树的查询结果
    class CQTreeSearchResult : public CSpatialIndex::CSISearchResult
    {
    CLASS_NAME(CQTreeSearchResult)
    NO_EVENTS_DISPATCHER
    NO_EVENTS_LISTENER

    public:
        CQTreeSearchResult();
        ~CQTreeSearchResult(){};

        bool __stdcall GotoInterface(const char* const interfacename, void** pp);
        bool __stdcall Clone(IObj** ppObj) const;

        list<SIItem> m_ItemIDs;
        list<SIItem>::const_iterator m_Pos;

        bool Next(SIItem& item);
        void First();
        dword GetCount() const;
        bool IsEof() const;

    friend class CQuadNode;
    friend class CSingleQuadTree;
    friend class CMultiQuadTree;
    };
    typedef TSmartPtr<CQTreeSearchResult> CQTreeSearchResultPtr;
    //--------------------------------------------------

    //--------------------------------------------------
    //  四叉树节点，四叉树拆分/遍历等基本算法都在这个类中实现
    class CQuadNode
    {
        //四个同根的子节点
        typedef struct
        {
            CQuadNode* pLeaf1;  //左上
            CQuadNode* pLeaf2;  //右上
            CQuadNode* pLeaf3;  //右下
            CQuadNode* pLeaf4;  //左下

            WKSRect Extent1;                    //在本节点上记录子节点
            WKSRect Extent2;                    //的范围是为了加快遍历
            WKSRect Extent3;                    //速度，否则要通过计算
            WKSRect Extent4;                    //才能求出子节点的范围
        }CQuadNodeLeafs;

        //----------------- disabled -----------------
        CQuadNode();
        CQuadNode(const CQuadNode& quadnode);
        //----------------- disabled -----------------

        CQuadNode(const WKSRect& extent);
        CQuadNode(CStreamPtr pStream);
        ~CQuadNode();//会自动干掉其所有子节点

        WKSRect         m_Extent;       //本节点的范围
        CQuadNodeLeafs  m_Leafs;        //本节点下级的4个叶节点
        list<SIItem>    m_ItemList;     //挂在本节点上的itemID

        //  将本节点的extent一分为四，增加四个子节点。
        //  如果sublevel>1，其子节点就根据sublevel - 1大小
        //  继续分下去。注意如果本节点不是最终叶节点（已经包
        //  含子节点）或sublevel<1，则本函数调用失败
        void SplitNode(const long sublevel);

        //  如果mbr被某一个子节点完全包含，就调用该子节点
        //  的additem，如此递规，直到到达叶节点或没有子节
        //  点能完全包含mbr为止，将itemid挂在那个节点上
        void AddItem(const SIItem& item, const WKSRect& mbr);

        //  类似additem，在找到节点后遍历节点中的itemid列表，
        //  如果没有找到则代表有问题
        bool DeleteItem(const SIItem& item, const WKSRect* const pMBR);

        //  获得本节点及其所有子节点的oid总数
        dword GetItemCount();

        //  遍历被pEnvelope “touch” 到的每一个节点，取得挂
        //  在这些节点上的itemid
        void Search(const WKSRect* pEnvelope,
            CQTreeSearchResultPtr pSearchResult) const;

        //  这个函数只会被构造函数调用
        dword SaveToStream(CStreamPtr pStream) const;

        //  这个函数只会被构造函数调用
        dword LoadFromStream(CStreamPtr pStream);

    friend class CSingleQuadTree;
    };
    //--------------------------------------------------

protected:
    virtual ~CCellQuadTree(){};

protected:
    //四叉树的参数（如果是MultiQuadTree，就是Grid ID为0的那个X@#%%$&*）
    CQTParams m_Parameters;

public:
    //  级数、范围等参数
    virtual void GetParameters(CQTParams& paramerters) const = 0;

    bool __stdcall Clone(IObj** ppObj) const;

friend class CQuadNode;
};
//================================================================================


//================================================================================
//  基于固定格网划分的简单四叉树算法
//================================================================================
class CSingleQuadTree : public CCellQuadTree
{
CLASS_NAME(CSingleQuadTree)
PERSIST_DUMP(CSingleQuadTree)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

private:
    CSingleQuadTree();

public:
    CSingleQuadTree(const CQTParams& paramerters);

private:
    ~CSingleQuadTree();

private:
    //最大范围，其实就是根节点的范围
    WKSRect m_MaxExtent;

    //根节点
    CQuadNode* m_pRoot;

private:
    //  干掉所有的咚咚，由LoadFromStream调用
    void Reset();

    //  由CMultiQuadTree使用
    void Search(const WKSRect* pEnvelope,
        CCellQuadTree::CQTreeSearchResultPtr pSearchResult) const;

    //  由构造函数调用，构建四叉树内存结构
    bool Initial(const CQTParams& paramerters);

private:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool GetParameter(const dword index, double& param);
    bool AddItem(const SIItem& item, const WKSRect& mbr);
    bool DeleteItem(const SIItem& item, const WKSRect* const pMBR = NULL);
    void ClearItems();
    bool Search(CSpatialIndex::CSISearchResultPtr& pSearchResult,
        const WKSRect* const pEnvelope) const;
    dword GetItemCount() const;

    void GetParameters(CQTParams& paramerters) const;

friend class CMultiQuadTree;
};
//================================================================================


//================================================================================
//  这个索引由一级格网组成，每个格网是一个四叉树，格网个数可以自动扩展
//  目的是为了解决单个四叉树的范围限制问题
//  p.s.把单个四叉树（格网）范围定为图幅大小比较合适
//================================================================================
class CMultiQuadTree : public CCellQuadTree
{
CLASS_NAME(CMultiQuadTree)
PERSIST_DUMP(CMultiQuadTree)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

private:
    CMultiQuadTree();

public:
    CMultiQuadTree(const CQTParams& paramerters);

private:
    ~CMultiQuadTree();

private:
    //第一个格网的左下角坐标，方便
    WKSPoint m_Origin;

    typedef CSingleQuadTreePtr SQTreePtr;
    typedef map<long, SQTreePtr> SQTreeMap;
    typedef SQTreeMap::iterator SQTreeMap_it;
    typedef SQTreeMap::const_iterator SQTreeMap_cit;
    //已经创建的四叉树，key是由高低16位X/Y序号组成的32位格网ID
    SQTreeMap m_QuadTrees;

    typedef map<SIItem, WKSRect> ItemMbrMap;
    typedef ItemMbrMap::iterator ItemMbrMap_it;
    typedef ItemMbrMap::const_iterator ItemMbrMap_cit;
    //跨四叉树边界的item（无法用单个四叉树来管理的）全部存放在这里，
    //每次查询都会遍历这个列表，如果四叉树范围定义得合理，列表中的元素就不应该有太多
    ItemMbrMap m_CrossBoundItems;

private:
    //  由LoadFromStream调用，干掉所有的咚咚
    void Reset();

    //  由AddItem()函数调用，用于计算MBR所在格网的ID，
    //  如果返回false表示MBR跨格网边界，需要存放在m_CrossBoundItems中
    bool CalcGridID(const WKSRect& mbr, long& gridid) const;

    //  当需要创建一个新四叉树的时候，通过gridid计算索引参数
    CCellQuadTree::CQTParams GetParamsFromGridID(const long gridid) const;

    //  由构造函数调用，构建四叉树内存结构
    bool Initial(const CQTParams& paramerters);

private:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool GetParameter(const dword index, double& param);
    bool AddItem(const SIItem& item, const WKSRect& mbr);
    bool DeleteItem(const SIItem& item, const WKSRect* const pMBR = NULL);
    void ClearItems();
    bool Search(CSpatialIndex::CSISearchResultPtr& pSearchResult,
        const WKSRect* const pEnvelope) const;
    dword GetItemCount() const;

    //  注意在这里是第一个四叉树（ID为0）的参数，作为整个格网的参照物
    void GetParameters(CQTParams& paramerters) const;
};
//================================================================================

CLASS_FACTORY(CSingleQuadTree)
CLASS_FACTORY(CMultiQuadTree)

}

#endif