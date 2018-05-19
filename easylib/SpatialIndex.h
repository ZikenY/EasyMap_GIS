#if !defined(SPATIALINDEX_INCLUDED_)
#define SPATIALINDEX_INCLUDED_

#include "Persist.h"

namespace easymap
{

class CSpatialIndex;
typedef TSmartPtr<CSpatialIndex> CSpatialIndexPtr;

//================================================================================
//  空间索引鸡肋
//================================================================================
typedef dword SIItem;
class CSpatialIndex : public IPersist
{
public:
    //---------  保存索引的遍历结果，item列表  ---------
    class CSISearchResult : public IObj
    {
    protected:
        virtual ~CSISearchResult(){};

    public:
        virtual bool Next(SIItem& item) = 0;
        virtual void First() = 0;
        virtual dword GetCount() const = 0;
        virtual bool IsEof() const = 0;
    };
    typedef TSmartPtr<CSISearchResult> CSISearchResultPtr;
    //--------------------------------------------------


protected:
    virtual ~CSpatialIndex(){};

public:
    //  得到索引的各项参数
    virtual bool GetParameter(const dword index, double& param) = 0;

    //  将新的item挂在索引结构中
    virtual bool AddItem(const SIItem& item, const WKSRect& mbr) = 0;

    //  删除一个item，可以给出这个item的mbr，以加速删除过程中的检索速度
    virtual bool DeleteItem(const SIItem& item, const WKSRect* const pMBR = NULL) = 0;

    //  XXXX You
    virtual void ClearItems() = 0;

    //  查询索引，返回满足条件的item列表
    virtual bool Search(CSISearchResultPtr& pSISearchResult,
        const WKSRect* const pEnvelope) const = 0;

    //  取得item个数
    virtual dword GetItemCount() const = 0;
};
//================================================================================

}

#endif
