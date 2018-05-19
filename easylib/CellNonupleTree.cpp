#include "CellNonupleTree.h"
#include "MathLib.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CCellNonupleTree)

//拆分envelope
inline void NonupleSplitEnvelope(const CCellNonupleTree::NodeLeafEnum subid,
    const WKSRect& originenvelope, WKSRect& subenvelope)
{
    double width = (originenvelope.right - originenvelope.left) / 3;
    double height = (originenvelope.top - originenvelope.bottom) / 3;

    switch(subid)
    {
    case CCellNonupleTree::NODELEAF_CENTER:
        {
            subenvelope.left = originenvelope.left + width;
            subenvelope.bottom = originenvelope.bottom + height;
        }
        break;
    case CCellNonupleTree::NODELEAF_UP:
        {
            subenvelope.left = originenvelope.left + width;
            subenvelope.bottom = originenvelope.top - height;
        }
        break;
    case CCellNonupleTree::NODELEAF_DOWN:
        {
            subenvelope.left = originenvelope.left + width;
            subenvelope.bottom = originenvelope.bottom;
        }
        break;
    case CCellNonupleTree::NODELEAF_LEFT:
        {
            subenvelope.left = originenvelope.left;
            subenvelope.bottom = originenvelope.bottom + height;
        }
        break;
    case CCellNonupleTree::NODELEAF_RIGHT:
        {
            subenvelope.left = originenvelope.right - width;
            subenvelope.bottom = originenvelope.bottom + height;
        }
        break;
    case CCellNonupleTree::NODELEAF_LEFTUP:
        {
            subenvelope.left = originenvelope.left;
            subenvelope.bottom = originenvelope.top - height;
        }
        break;
    case CCellNonupleTree::NODELEAF_RIGHTUP:
        {
            subenvelope.left = originenvelope.right - width;
            subenvelope.bottom = originenvelope.top - height;
        }
        break;
    case CCellNonupleTree::NODELEAF_LEFTDOWN:
        {
            subenvelope.left = originenvelope.left;
            subenvelope.bottom = originenvelope.bottom;
        }
        break;
    case CCellNonupleTree::NODELEAF_RIGHTDOWN:
        {
            subenvelope.left = originenvelope.right - width;
            subenvelope.bottom = originenvelope.bottom;
        }
        break;
    default:
        return;
    }

    subenvelope.right = subenvelope.left + width;
    subenvelope.top = subenvelope.bottom + height;

    return;
}

//-----------------------------------------------------------------------
//  以下是CNonupleNode的定义
//-----------------------------------------------------------------------

CCellNonupleTree::CNonupleNode::CNonupleNode(const long level,
    const WKSRect& extent, const WKSPoint& region_center)
{
    m_Level = level;
    m_Extent = extent;

    m_Leafs.pLeaf_Center    = NULL;
    m_Leafs.pLeaf_Up        = NULL;
    m_Leafs.pLeaf_Down      = NULL;
    m_Leafs.pLeaf_Left      = NULL;
    m_Leafs.pLeaf_Right     = NULL;
    m_Leafs.pLeaf_LeftUp    = NULL;
    m_Leafs.pLeaf_RightUp   = NULL;
    m_Leafs.pLeaf_LeftDown  = NULL;
    m_Leafs.pLeaf_RightDown = NULL;

    WKSPoint local_center;
    GetRectCenter(extent, local_center);
    m_Dist_to_center = mathlib::Distance(local_center, region_center);
}

#define releasenonuplenode(pNN) if (pNN) delete pNN;

CCellNonupleTree::CNonupleNode::~CNonupleNode()
{
    releasenonuplenode(m_Leafs.pLeaf_Center);
    releasenonuplenode(m_Leafs.pLeaf_Up);
    releasenonuplenode(m_Leafs.pLeaf_Down);
    releasenonuplenode(m_Leafs.pLeaf_Left);
    releasenonuplenode(m_Leafs.pLeaf_Right);
    releasenonuplenode(m_Leafs.pLeaf_LeftUp);
    releasenonuplenode(m_Leafs.pLeaf_RightUp);
    releasenonuplenode(m_Leafs.pLeaf_LeftDown);
    releasenonuplenode(m_Leafs.pLeaf_RightDown);
};

void CCellNonupleTree::CNonupleNode::SplitNode(const long sublevel, const WKSPoint& region_center)
{
    if (m_Leafs.pLeaf_Center || (1 >= sublevel))
    {
        return;//非叶节点，拆分失败
    }

    WKSRect subextent;

    //center
    NonupleSplitEnvelope(NODELEAF_CENTER, m_Extent, subextent);
    m_Leafs.pLeaf_Center = new CNonupleNode(m_Level+1, subextent, region_center);
    m_Leafs.pLeaf_Center->SplitNode(sublevel - 1, region_center);
    m_Leafs.Extent_Center = subextent;

    //up
    NonupleSplitEnvelope(NODELEAF_UP, m_Extent, subextent);
    m_Leafs.pLeaf_Up = new CNonupleNode(m_Level+1, subextent, region_center);
    m_Leafs.pLeaf_Up->SplitNode(sublevel - 1, region_center);
    m_Leafs.Extent_Up = subextent;

    //down
    NonupleSplitEnvelope(NODELEAF_DOWN, m_Extent, subextent);
    m_Leafs.pLeaf_Down = new CNonupleNode(m_Level+1, subextent, region_center);
    m_Leafs.pLeaf_Down->SplitNode(sublevel - 1, region_center);
    m_Leafs.Extent_Down = subextent;

    //left
    NonupleSplitEnvelope(NODELEAF_LEFT, m_Extent, subextent);
    m_Leafs.pLeaf_Left = new CNonupleNode(m_Level+1, subextent, region_center);
    m_Leafs.pLeaf_Left->SplitNode(sublevel - 1, region_center);
    m_Leafs.Extent_Left = subextent;

    //right
    NonupleSplitEnvelope(NODELEAF_RIGHTDOWN, m_Extent, subextent);
    m_Leafs.pLeaf_Right = new CNonupleNode(m_Level+1, subextent, region_center);
    m_Leafs.pLeaf_Right->SplitNode(sublevel - 1, region_center);
    m_Leafs.Extent_Right = subextent;

    //leftup
    NonupleSplitEnvelope(NODELEAF_LEFTUP, m_Extent, subextent);
    m_Leafs.pLeaf_LeftUp = new CNonupleNode(m_Level+1, subextent, region_center);
    m_Leafs.pLeaf_LeftUp->SplitNode(sublevel - 1, region_center);
    m_Leafs.Extent_LeftUp = subextent;

    //rightup
    NonupleSplitEnvelope(NODELEAF_RIGHTUP, m_Extent, subextent);
    m_Leafs.pLeaf_RightUp = new CNonupleNode(m_Level+1, subextent, region_center);
    m_Leafs.pLeaf_RightUp->SplitNode(sublevel - 1, region_center);
    m_Leafs.Extent_RightUp = subextent;

    //leftdown
    NonupleSplitEnvelope(NODELEAF_LEFTDOWN, m_Extent, subextent);
    m_Leafs.pLeaf_LeftDown = new CNonupleNode(m_Level+1, subextent, region_center);
    m_Leafs.pLeaf_LeftDown->SplitNode(sublevel - 1, region_center);
    m_Leafs.Extent_LeftDown = subextent;

    //rightdown
    NonupleSplitEnvelope(NODELEAF_RIGHTDOWN, m_Extent, subextent);
    m_Leafs.pLeaf_RightDown = new CNonupleNode(m_Level+1, subextent, region_center);
    m_Leafs.pLeaf_RightDown->SplitNode(sublevel - 1, region_center);
    m_Leafs.Extent_RightDown = subextent;
}

void CCellNonupleTree::CNonupleNode::Search(const CCellNonupleFilterPtr pFilter,
    CCellNonupleResultPtr& pResult, const bool dosearch) const
{
    pResult.Clear();

    if (!pFilter->CheckExtent(m_Extent, m_Level))
    {
        //不在本节点中，需要搜索同级的节点
        return;
    }

    if (dosearch)
    {
        //查询本节点的中心点
        WKSPoint centerpnt;
        centerpnt.x = (m_Extent.left + m_Extent.right)/2;
        centerpnt.y = (m_Extent.top + m_Extent.bottom)/2;
        pFilter->Search(centerpnt.x, centerpnt.y, m_Level, pResult);
    }

    //如果本节点中心点没有，检索9个子节点
    if (!pResult.Assigned() && m_Leafs.pLeaf_Center)
    {
        //按各个子节点距离整个区域中心点的距离顺序检索
        //尽量靠近中心
        vector<double> dist_sorted;
        dist_sorted.push_back(m_Leafs.pLeaf_Center->m_Dist_to_center);
        dist_sorted.push_back(m_Leafs.pLeaf_Up->m_Dist_to_center);
        dist_sorted.push_back(m_Leafs.pLeaf_Down->m_Dist_to_center);
        dist_sorted.push_back(m_Leafs.pLeaf_Left->m_Dist_to_center);
        dist_sorted.push_back(m_Leafs.pLeaf_Right->m_Dist_to_center);
        dist_sorted.push_back(m_Leafs.pLeaf_LeftUp->m_Dist_to_center);
        dist_sorted.push_back(m_Leafs.pLeaf_RightUp->m_Dist_to_center);
        dist_sorted.push_back(m_Leafs.pLeaf_LeftDown->m_Dist_to_center);
        dist_sorted.push_back(m_Leafs.pLeaf_RightDown->m_Dist_to_center);
        std::sort(dist_sorted.begin(), dist_sorted.end(), greater<double>());

        //  注意如果某个子节点pResult.Assigned()，则不需要继续遍历其它子节点
        WKSRect subextent;
        while (dist_sorted.size() > 0)
        {
            if (m_Leafs.pLeaf_Center->m_Dist_to_center < dist_sorted[dist_sorted.size() - 1] + 0.000001)
            {
                NonupleSplitEnvelope(NODELEAF_CENTER, m_Extent, subextent);
                //  注意中心子节点不必查询，只需判断extent
                m_Leafs.pLeaf_Center->Search(pFilter, pResult, false);
                if (pResult.Assigned())
                {
                    return;
                }

                dist_sorted.pop_back();
                if (dist_sorted.size() == 0)
                {
                    break;
                }
            }

            if (m_Leafs.pLeaf_Up->m_Dist_to_center < dist_sorted[dist_sorted.size() - 1] + 0.000001)
            {
                NonupleSplitEnvelope(NODELEAF_UP, m_Extent, subextent);
                m_Leafs.pLeaf_Up->Search(pFilter, pResult, true);
                if (pResult.Assigned())
                {
                    return;
                }

                dist_sorted.pop_back();
                if (dist_sorted.size() == 0)
                {
                    break;
                }
            }

            if (m_Leafs.pLeaf_Down->m_Dist_to_center < dist_sorted[dist_sorted.size() - 1] + 0.000001)
            {
                NonupleSplitEnvelope(NODELEAF_DOWN, m_Extent, subextent);
                m_Leafs.pLeaf_Down->Search(pFilter, pResult, true);
                if (pResult.Assigned())
                {
                    return;
                }

                dist_sorted.pop_back();
                if (dist_sorted.size() == 0)
                {
                    break;
                }
            }

            if (m_Leafs.pLeaf_Left->m_Dist_to_center < dist_sorted[dist_sorted.size() - 1] + 0.000001)
            {
                NonupleSplitEnvelope(NODELEAF_LEFT, m_Extent, subextent);
                m_Leafs.pLeaf_Left->Search(pFilter, pResult, true);
                if (pResult.Assigned())
                {
                    return;
                }

                dist_sorted.pop_back();
                if (dist_sorted.size() == 0)
                {
                    break;
                }
            }

            if (m_Leafs.pLeaf_Right->m_Dist_to_center < dist_sorted[dist_sorted.size() - 1] + 0.000001)
            {
                NonupleSplitEnvelope(NODELEAF_RIGHT, m_Extent, subextent);
                m_Leafs.pLeaf_Right->Search(pFilter, pResult, true);
                if (pResult.Assigned())
                {
                    return;
                }

                dist_sorted.pop_back();
                if (dist_sorted.size() == 0)
                {
                    break;
                }
            }

            if (m_Leafs.pLeaf_LeftUp->m_Dist_to_center < dist_sorted[dist_sorted.size() - 1] + 0.000001)
            {
                NonupleSplitEnvelope(NODELEAF_LEFTUP, m_Extent, subextent);
                m_Leafs.pLeaf_LeftUp->Search(pFilter, pResult, true);
                if (pResult.Assigned())
                {
                    return;
                }

                dist_sorted.pop_back();
                if (dist_sorted.size() == 0)
                {
                    break;
                }
            }

            if (m_Leafs.pLeaf_RightUp->m_Dist_to_center < dist_sorted[dist_sorted.size() - 1] + 0.000001)
            {
                NonupleSplitEnvelope(NODELEAF_RIGHTUP, m_Extent, subextent);
                m_Leafs.pLeaf_RightUp->Search(pFilter, pResult, true);
                if (pResult.Assigned())
                {
                    return;
                }
                
                dist_sorted.pop_back();
                if (dist_sorted.size() == 0)
                {
                    break;
                }
            }

            if (m_Leafs.pLeaf_LeftDown->m_Dist_to_center < dist_sorted[dist_sorted.size() - 1] + 0.000001)
            {
                NonupleSplitEnvelope(NODELEAF_LEFTDOWN, m_Extent, subextent);
                m_Leafs.pLeaf_LeftDown->Search(pFilter, pResult, true);
                if (pResult.Assigned())
                {
                    return;
                }
                
                dist_sorted.pop_back();
                if (dist_sorted.size() == 0)
                {
                    break;
                }
            }

            if (m_Leafs.pLeaf_RightDown->m_Dist_to_center < dist_sorted[dist_sorted.size() - 1] + 0.000001)
            {
                NonupleSplitEnvelope(NODELEAF_RIGHTDOWN, m_Extent, subextent);
                m_Leafs.pLeaf_RightDown->Search(pFilter, pResult, true);
                if (pResult.Assigned())
                {
                    return;
                }
                
                dist_sorted.pop_back();
                if (dist_sorted.size() == 0)
                {
                    break;
                }
            }
        }
    }

    //pResult无效，搜索失败
    return;
}

CCellNonupleTree::CCellNonupleTree()
{
    INIT_REFCOUNT

    m_pRoot = NULL; //don't forget!
    m_Parameters.level = 0;
}

CCellNonupleTree::CCellNonupleTree(const CNTParams& paramerters)
{
    INIT_REFCOUNT

    m_pRoot = NULL; //don't forget!
    m_Parameters.level = 0;
    this->Initial(paramerters);
}

CCellNonupleTree::~CCellNonupleTree()
{
    this->Reset();
};

void CCellNonupleTree::Reset()
{
    if (m_pRoot)
    {
        delete m_pRoot;//会自动循环删除各个子节点
        m_pRoot = NULL;
    }

    m_pFilter.Clear();
}

bool CCellNonupleTree::Initial(const CNTParams& paramerters)
{
    if (0 >= paramerters.level)
    {
        return false;
    }

    m_Parameters = paramerters;

    if (m_pRoot)
    {
        delete m_pRoot;
    }

    WKSPoint region_center;
    GetRectCenter(m_Parameters.extent, region_center);
    //约定根节点级数为1
    m_pRoot = new CNonupleNode(1, m_Parameters.extent, region_center);

    //循环拆分九叉树节点，构建整个九叉树
    m_pRoot->SplitNode(m_Parameters.level, region_center);

    return true;
}

bool __stdcall CCellNonupleTree::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CCellNonupleTree"))
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

bool __stdcall CCellNonupleTree::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    IObjPtr po;
    this->Clone(po);
    *ppObj = po._p();
    (*ppObj)->_AddRef();
    return true;
}

bool CCellNonupleTree::Clone(IObjPtr& pObj) const
{
    CCellNonupleTreePtr pCNT = new CCellNonupleTree(m_Parameters);

    IObjPtr pObjTmp;
    CLONE_PTR(m_pFilter, pObjTmp)
    CAST_PTR(pObjTmp, pCNT->m_pFilter, CCellNonupleFilter)

    CAST_PTR(pCNT, pObj, IObj)
    return true;
}

dword __stdcall CCellNonupleTree::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    pStream->WriteData(&m_Parameters, sizeof(CNTParams));

    long flag = 1;
    if (m_pFilter.Assigned())
    {
        //保存过滤器
        pStream->WriteData(&flag, sizeof(long));
        m_pFilter->_DumpTo(pStream, assist);
    }
    else
    {
        flag = 0;
        pStream->WriteData(&flag, sizeof(long));
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CCellNonupleTree::_LoadInstance(IStreamX* pStream, void* const assist)
{
    this->Reset();

    dword oldpos = pStream->GetPos();

    pStream->ReadData(&m_Parameters, sizeof(CNTParams));

    this->Initial(m_Parameters);

    long flag;
    pStream->ReadData(&flag, sizeof(long));
    if (flag)
    {
        //恢复过滤器
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom((CStream*)pStream, pPersist, assist);
        CAST_PTR(pPersist, m_pFilter, CCellNonupleFilter)
    }

    return pStream->GetPos() - oldpos;
}

void CCellNonupleTree::GetParameters(CNTParams& paramerters) const
{
    paramerters = m_Parameters;
}

void CCellNonupleTree::SetFilter(const CCellNonupleFilterPtr pFilter)
{
    m_pFilter = pFilter;
}

void CCellNonupleTree::Search(CCellNonupleResultPtr& pResult) const
{
    pResult.Clear();
    m_pRoot->Search(m_pFilter, pResult, true);
}

}