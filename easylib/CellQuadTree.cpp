#include "CommonInclude.h"
#include "CellQuadTree.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CSingleQuadTree)
CLASS_FACTORY_INSTANCE(CMultiQuadTree)

//辅助函数，顺时针0,1,2,3拆分envelope
inline bool QuadSplitEnvelope(const long subid, const WKSRect& originenvelope,
    WKSRect& subenvelope)
{
    double centerx = (originenvelope.right + originenvelope.left) / 2;
    double centery = (originenvelope.top + originenvelope.bottom) / 2;

    switch(subid)
    {
    case 0:
        {
            subenvelope = originenvelope;
            subenvelope.right = centerx;
            subenvelope.bottom = centery;
        }
        break;
    case 1:
        {
            subenvelope = originenvelope;
            subenvelope.left = centerx;
            subenvelope.bottom = centery;
        }
        break;
    case 2:
        {
            subenvelope = originenvelope;
            subenvelope.left = centerx;
            subenvelope.top = centery;
        }
        break;
    case 3:
        {
            subenvelope = originenvelope;
            subenvelope.right = centerx;
            subenvelope.top = centery;
        }
        break;
    default:
        return false;
    }
    return true;
}


//-----------------------------------------------------------------------
//  以下是CQTreeSearchResult的定义
//-----------------------------------------------------------------------

CCellQuadTree::CQTreeSearchResult::CQTreeSearchResult()
{
    INIT_REFCOUNT
}

bool __stdcall CCellQuadTree::CQTreeSearchResult::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "CSpatialIndex"))
        || (0 == strcmp(interfacename, "CQTreeSearchResult")))
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

bool __stdcall CCellQuadTree::Clone(IObj** ppObj) const
{
    return false;
}

bool CCellQuadTree::CQTreeSearchResult::Next(SIItem& item)
{
    if (m_ItemIDs.size() <= 0) return false;
    if (m_Pos == m_ItemIDs.end()) return false;
    item = *(m_Pos++);
    return true;
}

void CCellQuadTree::CQTreeSearchResult::First()
{
    m_Pos == m_ItemIDs.begin();
}

dword CCellQuadTree::CQTreeSearchResult::GetCount() const
{
    return m_ItemIDs.size();
}

bool CCellQuadTree::CQTreeSearchResult::IsEof() const
{
    if ((m_ItemIDs.size() <= 0) || (m_Pos == m_ItemIDs.end()))
    {
        return true;
    }

    return false;
}

bool __stdcall CCellQuadTree::CQTreeSearchResult::Clone(IObj** ppObj) const
{
    return false;
}


//-----------------------------------------------------------------------
//  以下是CQuadNode的定义
//-----------------------------------------------------------------------

CCellQuadTree::CQuadNode::CQuadNode(const WKSRect& extent) : m_Extent(extent)
{
    m_Leafs.pLeaf1 = NULL;
    m_Leafs.pLeaf2 = NULL;
    m_Leafs.pLeaf3 = NULL;
    m_Leafs.pLeaf4 = NULL;
}

CCellQuadTree::CQuadNode::CQuadNode(CStreamPtr pStream)
{
    this->LoadFromStream(pStream);
}

CCellQuadTree::CQuadNode::~CQuadNode()
{
    if (m_Leafs.pLeaf1)
    {
        delete m_Leafs.pLeaf1;
    }

    if (m_Leafs.pLeaf2)
    {
        delete m_Leafs.pLeaf2;
    }

    if (m_Leafs.pLeaf3)
    {
        delete m_Leafs.pLeaf3;
    }

    if (m_Leafs.pLeaf4)
    {
        delete m_Leafs.pLeaf4;
    }
};

void CCellQuadTree::CQuadNode::SplitNode(const long sublevel)
{
    if (m_Leafs.pLeaf1 || (1 >= sublevel))
    {
        return;//非叶节点，拆分失败
    }

    WKSRect subextent;

    //左上
    QuadSplitEnvelope(0, m_Extent, subextent);
    m_Leafs.pLeaf1 = new CQuadNode(subextent);
    m_Leafs.pLeaf1->SplitNode(sublevel - 1);
    m_Leafs.Extent1 = subextent;

    //右上
    QuadSplitEnvelope(1, m_Extent, subextent);
    m_Leafs.pLeaf2 = new CQuadNode(subextent);
    m_Leafs.pLeaf2->SplitNode(sublevel - 1);
    m_Leafs.Extent2 = subextent;

    //右下
    QuadSplitEnvelope(2, m_Extent, subextent);
    m_Leafs.pLeaf3 = new CQuadNode(subextent);
    m_Leafs.pLeaf3->SplitNode(sublevel - 1);
    m_Leafs.Extent3 = subextent;

    //左下
    QuadSplitEnvelope(3, m_Extent, subextent);
    m_Leafs.pLeaf4 = new CQuadNode(subextent);
    m_Leafs.pLeaf4->SplitNode(sublevel - 1);
    m_Leafs.Extent4 = subextent;
}

void CCellQuadTree::CQuadNode::AddItem(const SIItem& item, const WKSRect& mbr)
{
    if (m_Leafs.pLeaf1)
    {
        if (EnvelopesContented(m_Leafs.Extent1, mbr))
        {
            //左上
            m_Leafs.pLeaf1->AddItem(item, mbr);
            return;
        }

        if (EnvelopesContented(m_Leafs.Extent2, mbr))
        {
            //右上
            m_Leafs.pLeaf2->AddItem(item, mbr);
            return;
        }

        if (EnvelopesContented(m_Leafs.Extent3, mbr))
        {
            //右下
            m_Leafs.pLeaf3->AddItem(item, mbr);
            return;
        }

        if (EnvelopesContented(m_Leafs.Extent4, mbr))
        {
            //左下
            m_Leafs.pLeaf4->AddItem(item, mbr);
            return;
        }
    }

    //4个子节点都无法完全包含mbr，或已经到他妈的叶节点了，
    //所以就将itemid挂在这里
    m_ItemList.push_back(item);
}

bool CCellQuadTree::CQuadNode::DeleteItem(const SIItem& item, const WKSRect* const pMBR)
{
    //在本节点内查找
    list<SIItem>::iterator it = m_ItemList.begin();
    while (it != m_ItemList.end())
    {
        if (*it == item)
        {
            //咩鱼
            m_ItemList.erase(it);
            return true;
        }
        it++;
    }

    if (!m_Leafs.pLeaf1)
    {
        return false; //居然没有，有问题
    }

    if (_valid(pMBR))
    {
        if (EnvelopesTouched(m_Leafs.pLeaf1->m_Extent, *pMBR))
        {
            //左上
            if (m_Leafs.pLeaf1->DeleteItem(item, pMBR))
            {
                return true;
            }
        }
        else if (EnvelopesTouched(m_Leafs.pLeaf2->m_Extent, *pMBR))
        {
            //右上
            if (m_Leafs.pLeaf2->DeleteItem(item, pMBR))
            {
                return true;
            }
        }
        else if (EnvelopesTouched(m_Leafs.pLeaf3->m_Extent, *pMBR))
        {
            //右下
            if (m_Leafs.pLeaf3->DeleteItem(item, pMBR))
            {
                return true;
            }
        }
        else if (EnvelopesTouched(m_Leafs.pLeaf4->m_Extent, *pMBR))
        {
            //左下
            if (m_Leafs.pLeaf4->DeleteItem(item, pMBR))
            {
                return true;
            }
        }
        else
        {
            return false; //输入的mbr有问题
        }
    }
    else
    {
        //不用mbr，直接检索每一个extent

        //左上
        if (m_Leafs.pLeaf1->DeleteItem(item, pMBR))
        {
            return true;
        }

        //右上
        if (m_Leafs.pLeaf2->DeleteItem(item, pMBR))
        {
            return true;
        }

        //右下
        if (m_Leafs.pLeaf3->DeleteItem(item, pMBR))
        {
            return true;
        }

        //左下
        if (m_Leafs.pLeaf4->DeleteItem(item, pMBR))
        {
            return true;
        }
    }

    return false;
}

dword CCellQuadTree::CQuadNode::GetItemCount()
{
    dword count = this->m_ItemList.size();
    if (this->m_Leafs.pLeaf1)
    {
        count += m_Leafs.pLeaf1->GetItemCount();
        count += m_Leafs.pLeaf2->GetItemCount();
        count += m_Leafs.pLeaf3->GetItemCount();
        count += m_Leafs.pLeaf4->GetItemCount();
    }
    return count;
}

void CCellQuadTree::CQuadNode::Search(const WKSRect* pEnvelope,
    CQTreeSearchResultPtr pSearchResult) const
{
    if (m_Leafs.pLeaf1)
    {
        //遍历子节点
        if (pEnvelope)
        {
            WKSRect subextent;
            QuadSplitEnvelope(0, m_Extent, subextent);
            if (EnvelopesTouched(subextent, *pEnvelope))//左上
            {
                m_Leafs.pLeaf1->Search(pEnvelope, pSearchResult);
            }

            QuadSplitEnvelope(1, m_Extent, subextent);
            if (EnvelopesTouched(subextent, *pEnvelope))//右上
            {
                m_Leafs.pLeaf2->Search(pEnvelope, pSearchResult);
            }

            QuadSplitEnvelope(2, m_Extent, subextent);
            if (EnvelopesTouched(subextent, *pEnvelope))//右下
            {
                m_Leafs.pLeaf3->Search(pEnvelope, pSearchResult);
            }

            QuadSplitEnvelope(3, m_Extent, subextent);
            if (EnvelopesTouched(subextent, *pEnvelope))//左下
            {
                m_Leafs.pLeaf4->Search(pEnvelope, pSearchResult);
            }
        }
        else
        {
            WKSRect subextent;
            QuadSplitEnvelope(0, m_Extent, subextent);
            m_Leafs.pLeaf1->Search(NULL, pSearchResult);

            QuadSplitEnvelope(1, m_Extent, subextent);
            m_Leafs.pLeaf2->Search(NULL, pSearchResult);

            QuadSplitEnvelope(2, m_Extent, subextent);
            m_Leafs.pLeaf3->Search(NULL, pSearchResult);

            QuadSplitEnvelope(3, m_Extent, subextent);
            m_Leafs.pLeaf4->Search(NULL, pSearchResult);
        }
    }

    //返回本节点的冬冬
    list<SIItem>::const_iterator it = m_ItemList.begin();
    while (it != m_ItemList.end())
    {
        pSearchResult->m_ItemIDs.push_back(*it);
        it++;
    }
}

dword CCellQuadTree::CQuadNode::SaveToStream(CStreamPtr pStream) const
{
    dword oldpos = pStream->GetPos();

    pStream->Write(&m_Extent, sizeof(WKSRect));

    //存储本节点的东东
    dword itemcount = m_ItemList.size();
    pStream->Write(itemcount);
    list<SIItem>::const_iterator it = m_ItemList.begin();
    while (it != m_ItemList.end())
    {
        pStream->Write(&(*it), sizeof(SIItem));
        it++;
    }

    if (m_Leafs.pLeaf1)
    {
        long flag = 1;
        pStream->Write(flag);

        m_Leafs.pLeaf1->SaveToStream(pStream);
        m_Leafs.pLeaf2->SaveToStream(pStream);
        m_Leafs.pLeaf3->SaveToStream(pStream);
        m_Leafs.pLeaf4->SaveToStream(pStream);
    }
    else
    {
        long flag = 0;//没有子节点
        pStream->Write(flag);
    }

    return pStream->GetPos() - oldpos;
}

dword CCellQuadTree::CQuadNode::LoadFromStream(CStreamPtr pStream)
{
    dword oldpos = pStream->GetPos();

    pStream->Read(&m_Extent, sizeof(WKSRect));

    //还原本节点中的itemid
    dword itemcount;
    pStream->Read(itemcount);
    for (dword i = 0; i < itemcount; i++)
    {
        SIItem item;
        pStream->Read(&item, sizeof(SIItem));
        m_ItemList.push_back(item);
    }

    long flag;
    pStream->Read(flag);
    if (flag)
    {
        QuadSplitEnvelope(0, m_Extent, m_Leafs.Extent1);
        QuadSplitEnvelope(1, m_Extent, m_Leafs.Extent2);
        QuadSplitEnvelope(2, m_Extent, m_Leafs.Extent3);
        QuadSplitEnvelope(3, m_Extent, m_Leafs.Extent4);

        //还原4个子节点
        m_Leafs.pLeaf1 = new CQuadNode(pStream);
        m_Leafs.pLeaf2 = new CQuadNode(pStream);
        m_Leafs.pLeaf3 = new CQuadNode(pStream);
        m_Leafs.pLeaf4 = new CQuadNode(pStream);
    }
    else
    {
        //don't forget these suckers
        m_Leafs.pLeaf1 = NULL;
        m_Leafs.pLeaf4 = NULL;
        m_Leafs.pLeaf3 = NULL;
        m_Leafs.pLeaf2 = NULL;
    }

    return pStream->GetPos() - oldpos;
}

//-----------------------------------------------------------------------
//  以下是CSingleQuadTree的定义
//-----------------------------------------------------------------------

CSingleQuadTree::CSingleQuadTree()
{
    INIT_REFCOUNT

    m_pRoot = NULL;
    m_Parameters.treelevel = 0;
}

CSingleQuadTree::CSingleQuadTree(const CQTParams& paramerters)
{
    INIT_REFCOUNT

    m_pRoot = NULL; //don't forget!
    m_Parameters.treelevel = 0;
    this->Initial(paramerters);
}

CSingleQuadTree::~CSingleQuadTree()
{
    this->Reset();
}

void CSingleQuadTree::Reset()
{
    if (m_pRoot)
    {
        delete m_pRoot;//会自动循环删除各个子节点
        m_pRoot = NULL;
    }
}

void CSingleQuadTree::Search(const WKSRect* pEnvelope,
    CCellQuadTree::CQTreeSearchResultPtr pSearchResult) const
{
    if (pEnvelope && !EnvelopesTouched(*pEnvelope, this->m_MaxExtent))
    {
        return;
    }

    m_pRoot->Search(pEnvelope, pSearchResult);
    pSearchResult->m_Pos = pSearchResult->m_ItemIDs.begin();
}

bool CSingleQuadTree::Initial(const CQTParams& paramerters)
{
    if (0 >= paramerters.treelevel)
    {
        return false;
    }

    m_Parameters = paramerters;
    if (0.0001 > m_Parameters.BoundSize)
    {
        m_Parameters.BoundSize = 0.0001;
    }

    if (m_pRoot)
    {
        delete m_pRoot;
    }

    double halfsize = m_Parameters.BoundSize / 2;
    m_MaxExtent.bottom = m_Parameters.centerpoint.y - halfsize;
    m_MaxExtent.left = m_Parameters.centerpoint.x - halfsize;
    m_MaxExtent.right = m_Parameters.centerpoint.x + halfsize;
    m_MaxExtent.top = m_Parameters.centerpoint.y + halfsize;

    //约定根节点级数为1
    m_pRoot = new CQuadNode(m_MaxExtent);

    //循环拆分四叉树节点，构建整个四叉树
    m_pRoot->SplitNode(m_Parameters.treelevel);

    return true;
}

bool CSingleQuadTree::GetParameter(const dword index, double& param)
{
    bool r = true;

    switch(index)
    {
    case 0:
        {
            param = m_Parameters.centerpoint.x;
            break;
        }
    case 1:
        {
            param = m_Parameters.centerpoint.y;
            break;
        }
    case 2:
        {
            param = m_Parameters.BoundSize;
            break;
        }
    case 3:
        {
            param = m_Parameters.treelevel;
            break;
        }
    default:
        r = false;
    }

    return r;
}

bool CSingleQuadTree::AddItem(const SIItem& item, const WKSRect& mbr)
{
    //先判断范围是否超过，超过了就不干
    if (!EnvelopesTouched(m_MaxExtent, mbr))
    {
        return false;
    }

    m_pRoot->AddItem(item, mbr);
    return true;
}

bool CSingleQuadTree::DeleteItem(const SIItem& item, const WKSRect* const pMBR)
{
    return m_pRoot->DeleteItem(item, pMBR);
}

void CSingleQuadTree::ClearItems()
{
    this->Reset();
    this->Initial(m_Parameters);
}

bool CSingleQuadTree::Search(CSpatialIndex::CSISearchResultPtr& pSearchResult,
    const WKSRect* const pEnvelope) const
{
    CQTreeSearchResultPtr pQTreeSearchResult
        = new CQTreeSearchResult;

    //自动检索每一个子节点
    m_pRoot->Search(pEnvelope, pQTreeSearchResult);
    pQTreeSearchResult->m_Pos = pQTreeSearchResult->m_ItemIDs.begin();
    pSearchResult = pQTreeSearchResult._p();
    return true;
}

dword CSingleQuadTree::GetItemCount() const
{
    return m_pRoot->GetItemCount();
}

bool __stdcall CSingleQuadTree::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CSpatialIndex"))
        || (0 == strcmp(interfacename, "CCellQuadTree"))
        || (0 == strcmp(interfacename, "CSingleQuadTree"))
        )
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

dword __stdcall CSingleQuadTree::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    pStream->WriteData(&m_Parameters, sizeof(CQTParams));
    pStream->WriteData(&m_MaxExtent, sizeof(WKSRect));
    if (m_pRoot)
    {
        //保存节点们
        long flag = 1;
        pStream->WriteData(&flag, sizeof(long));
        CStreamPtr ps = (CStream*)pStream;
        m_pRoot->SaveToStream(ps);
    }
    else
    {
        long flag = 0;
        pStream->WriteData(&flag, sizeof(long));
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CSingleQuadTree::_LoadInstance(IStreamX* pStream, void* const assist)
{
    this->Reset();

    dword oldpos = pStream->GetPos();

    pStream->ReadData(&m_Parameters, sizeof(CQTParams));
    pStream->ReadData(&m_MaxExtent, sizeof(WKSRect));

    long flag;
    pStream->ReadData(&flag, sizeof(long));
    if (flag)
    {
        //从根节点开始恢复整棵树
        CStreamPtr ps = (CStream*)pStream;
        m_pRoot = new CQuadNode(ps);
    }

    return pStream->GetPos() - oldpos;
}

void CSingleQuadTree::GetParameters(CQTParams& paramerters) const
{
    paramerters = m_Parameters;
}


//-----------------------------------------------------------------------
//  以下是CMultiQuadTree的定义
//-----------------------------------------------------------------------

CMultiQuadTree::CMultiQuadTree()
{
    INIT_REFCOUNT

    m_Parameters.treelevel = 0;
}

CMultiQuadTree::CMultiQuadTree(const CQTParams& paramerters)
{
    INIT_REFCOUNT

    this->Initial(paramerters);
}

CMultiQuadTree::~CMultiQuadTree()
{
    this->Reset();
};

void CMultiQuadTree::Reset()
{
    SQTreeMap_it it = m_QuadTrees.begin();
    m_QuadTrees.clear();
    m_CrossBoundItems.clear();
}

bool CMultiQuadTree::CalcGridID(const WKSRect& mbr, long& gridid) const
{
    short multiplyX, multiplyY;
    double tmp = mbr.left - m_Origin.x;
    multiplyX = (short)(tmp / m_Parameters.BoundSize);
    if (tmp < 0) multiplyX += -1;
    tmp = mbr.bottom - m_Origin.y;
    multiplyY = (short)(tmp / m_Parameters.BoundSize);
    if (tmp < 0) multiplyY += -1;
    //multiplyY左移2字节 & multiplyX
    long tmpx = 0x0000FFFF & multiplyX;
    long tmpy = multiplyY << 16;
    gridid = tmpy | tmpx;

    tmp = mbr.right - m_Origin.x;
    multiplyX = (short)(tmp / m_Parameters.BoundSize);
    if (tmp < 0) multiplyX += -1;
    tmp = mbr.top - m_Origin.y;
    multiplyY = (short)(tmp / m_Parameters.BoundSize);
    if (tmp < 0) multiplyY += -1;
    //multiplyY左移2字节 & multiplyX
    tmpx = 0x0000FFFF & multiplyX;
    tmpy = multiplyY << 16;
    long gridid1 = tmpy | tmpx;

    //检查mbr的两个对角坐标是否在同一个格网内
    if (gridid == gridid1)
    {
        return true;
    }
    else
    {
        return false;
    }
}

CCellQuadTree::CQTParams CMultiQuadTree::GetParamsFromGridID(const long gridid) const
{
    CQTParams params = m_Parameters;
    //把gridid拆成高低各两个字节，分别计算centerpoint.x和centerpoint.y
    short multiplyX = (short)(gridid & 0x0000FFFF);
    short multiplyY = (short)((gridid >> 16));
    params.centerpoint.x = params.centerpoint.x
        + params.BoundSize*multiplyX;
    params.centerpoint.y = params.centerpoint.y
        + params.BoundSize*multiplyY;

    return params;
}

bool CMultiQuadTree::Initial(const CQTParams& paramerters)
{
    if (0 >= paramerters.treelevel)
    {
        return false;
    }

    //干掉m_CrossBoundItems和所有的四叉树
    m_QuadTrees.clear();
    m_CrossBoundItems.clear();

    //咩鱼
    m_Parameters = paramerters;
    if (0.0001 > m_Parameters.BoundSize)
    {
        m_Parameters.BoundSize = 0.0001;
    }
    double halfboundsize = m_Parameters.BoundSize / 2;
    m_Origin.x = m_Parameters.centerpoint.x - halfboundsize;
    m_Origin.y = m_Parameters.centerpoint.y - halfboundsize;

    //搞定
    return true;
}

bool CMultiQuadTree::GetParameter(const dword index, double& param)
{
    bool r = true;

    switch(index)
    {
    case 0:
        {
            param = m_Parameters.centerpoint.x;
            break;
        }
    case 1:
        {
            param = m_Parameters.centerpoint.y;
            break;
        }
    case 2:
        {
            param = m_Parameters.BoundSize;
            break;
        }
    case 3:
        {
            param = m_Parameters.treelevel;
            break;
        }
    default:
        r = false;
    }

    return r;
}

bool CMultiQuadTree::AddItem(const SIItem& item, const WKSRect& mbr)
{
    long gridid;
    if (this->CalcGridID(mbr, gridid))
    {
        SQTreePtr pSingleQuadTree;

        //检查这个飞机是否存在，不在就创建
        SQTreeMap_it it_tree = m_QuadTrees.find(gridid);
        if (it_tree == m_QuadTrees.end())
        {
            CQTParams params = this->GetParamsFromGridID(gridid);
            pSingleQuadTree = new CSingleQuadTree(params);
            m_QuadTrees[gridid] = pSingleQuadTree;
        }
        else
        {
            //四叉树已经存在
            pSingleQuadTree = it_tree->second;
        }
        return pSingleQuadTree->AddItem(item, mbr);
    }
    else
    {
        //挂在m_CrossBoundItems上
        m_CrossBoundItems[item] = mbr; 
        return true;
    }
}

bool CMultiQuadTree::DeleteItem(const SIItem& item, const WKSRect* const pMBR)
{
    //先检查每一个四叉树
    SQTreeMap_it it_tree = m_QuadTrees.begin();
    while (it_tree != m_QuadTrees.end())
    {
        SQTreePtr pSingleQuadTree = it_tree->second;
        if (pSingleQuadTree->DeleteItem(item, pMBR))
        {
            return true;
        }
        it_tree++;
    }

    //也可能在m_CrossBoundItems中
    ItemMbrMap_it it_cbmbr = m_CrossBoundItems.find(item);
    if (it_cbmbr != m_CrossBoundItems.end())
    {
        m_CrossBoundItems.erase(it_cbmbr);
        return true;
    }

    //到处都没有，
    return false;
}

void CMultiQuadTree::ClearItems()
{
    SQTreeMap_cit it_tree = m_QuadTrees.begin();
    while (it_tree != m_QuadTrees.end())
    {
        SQTreePtr pSingleQuadTree = it_tree->second;
        pSingleQuadTree->ClearItems();
        it_tree++;
    }
}

bool CMultiQuadTree::Search(CSpatialIndex::CSISearchResultPtr& pSearchResult,
    const WKSRect* const pEnvelope) const
{
    CQTreeSearchResultPtr pQTreeSearchResult
        = new CQTreeSearchResult;

    //这里应该优化一下，直接计算出需要search的四叉树ID，
    //而不应该遍历每一个四叉树
    //我是懒人谁管你
    SQTreeMap_cit it_tree = m_QuadTrees.begin();
    while (it_tree != m_QuadTrees.end())
    {
        SQTreePtr pSingleQuadTree = it_tree->second;
        pSingleQuadTree->Search(pEnvelope, pQTreeSearchResult);
        it_tree++;
    }

    //别忘了这些
    ItemMbrMap_cit it_mbr = m_CrossBoundItems.begin();
    while (it_mbr != m_CrossBoundItems.end())
    {
        if (!pEnvelope || EnvelopesTouched(*pEnvelope, it_mbr->second))
        {
            pQTreeSearchResult->m_ItemIDs.push_back(it_mbr->first);
        }
        it_mbr++;
    }

    pQTreeSearchResult->m_Pos = pQTreeSearchResult->m_ItemIDs.begin();
    pSearchResult = pQTreeSearchResult._p();
    return true;
}

dword CMultiQuadTree::GetItemCount() const
{
    dword itemcount = 0;
    SQTreeMap_cit it_tree = m_QuadTrees.begin();
    while (it_tree != m_QuadTrees.end())
    {
        SQTreePtr pSingleQuadTree = it_tree->second;
        itemcount += pSingleQuadTree->GetItemCount();
        it_tree++;
    }
    return itemcount + m_CrossBoundItems.size();
}

bool __stdcall CMultiQuadTree::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CSpatialIndex"))
        || (0 == strcmp(interfacename, "CCellQuadTree"))
        || (0 == strcmp(interfacename, "CMultiQuadTree"))
        )
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

dword __stdcall CMultiQuadTree::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    //保存原点坐标
    ps->Write(m_Origin.x);
    ps->Write(m_Origin.y);

    //包含的四叉树个数
    dword count = m_QuadTrees.size();
    ps->Write(count);

    //保存每一个gridid<-->四叉树pair
    SQTreeMap_cit it_tree = m_QuadTrees.begin();
    while (it_tree != m_QuadTrees.end())
    {
        long gridid = it_tree->first;
        ps->Write(gridid);
        SQTreePtr pSingleQuadTree = it_tree->second;
        pSingleQuadTree->_DumpTo(pStream, NULL);
        it_tree++;
    }

    //保存m_CrossBoundItems元素个数
    count = m_CrossBoundItems.size();
    ps->Write(count);

    //保存m_CrossBoundItems中的每一个id<-->mbr pair
    ItemMbrMap_cit it_mbr = m_CrossBoundItems.begin();
    while (it_mbr != m_CrossBoundItems.end())
    {
        ps->Write(&(it_mbr->first), sizeof(SIItem));
        ps->Write(&(it_mbr->second), sizeof(WKSRect));
        it_mbr++;
    }

    //保存基准参数
    ps->Write(&m_Parameters, sizeof(CQTParams));

    //收工
    return pStream->GetPos() - oldpos;
}

dword __stdcall CMultiQuadTree::_LoadInstance(IStreamX* pStream, void* const assist)
{
    this->Reset();

    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    //恢复原点坐标
    ps->Read(m_Origin.x);
    ps->Read(m_Origin.y);

    dword count, i;

    //读取四叉树个数
    ps->Read(count);

    //顺序读出每对id <--> 四叉树
    for (i = 0; i < count; i++)
    {
        long gridid;
        ps->Read(gridid);
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom(ps, pPersist, assist);
        SQTreePtr pSingleQuadTree = (CSingleQuadTree*)pPersist._p();
        m_QuadTrees[gridid] = pSingleQuadTree;
    }

    //读取m_CrossBoundItems元素个数
    ps->Read(count);

    //顺序读出每对id <--> mbr pair
    for (i = 0; i < count; i++)
    {
        SIItem item;
        ps->Read(&item, sizeof(SIItem));
        WKSRect mbr;
        ps->Read(&mbr, sizeof(WKSRect));
        m_CrossBoundItems[item] = mbr;
    }

    //读取基准参数
    ps->Read(&m_Parameters, sizeof(CQTParams));

    //完事
    return pStream->GetPos() - oldpos;
}

void CMultiQuadTree::GetParameters(CQTParams& paramerters) const
{
    paramerters = m_Parameters;
};

}
