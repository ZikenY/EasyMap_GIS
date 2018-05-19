#if !defined(NETTOPOBASE_INCLUDED_)
#define NETTOPOBASE_INCLUDED_

//网络拓扑结构及算法

#pragma warning(disable: 4786)
#include <vector>
#include <set>
#include <map>
#include <algorithm>
using namespace std;

#include "Stream.h"
#include "..\\include\\WKSInclude.h"

namespace easymap
{

namespace net
{

//-------------------------------------------------
//注意对于所有的顶点id、0代表无效
//-------------------------------------------------

//顶点
struct Vertex : public WKSPoint
{
    inline bool operator==(const Vertex& rhs) const
    {
        return (this->vid == rhs.vid) ? true : false;
    };
    inline bool operator>(const Vertex& rhs) const
    {
        return (this->vid > rhs.vid) ? true : false;
    };
    inline bool operator<(const Vertex& rhs) const
    {
        return (this->vid < rhs.vid) ? true : false;
    };

    inline Vertex()
    {
        vid = 0;
    };

    inline Vertex(const Vertex& v)
    {
        x = v.x;
        y = v.y;
        vid = v.vid;
    };

    inline Vertex& operator=(const Vertex& rhs)
    {
        if (this == &rhs) return *this;
        x = rhs.x;
        y = rhs.y;
        vid = rhs.vid;
        return *this;
    };

    dword vid;
};


//边，注意是单向边
struct Edge
{
    Edge()
        {from = to = 0; eid = 0; weight = 0;};
    dword from;                 //起点vertex id
    dword to;                   //终点vertex id
    double weight;              //边权
    long eid;                  //边id，算法中没用，调用者有用
};


//转向点，即边2边
struct TurnPoint
{
    inline TurnPoint()
    {
        turnpoint.vid = from.vid = to.vid = 0;
        blocked = 0;
        weight = 0;
        tid = 0;
    };
    Vertex turnpoint;           //转向点
    Vertex from;                //第一条边起点
    Vertex to;                  //第二条边终点
    long blocked;               //在该方向上：1 - 阻塞，0 - 通行
    double weight;              //转向点权值
    dword tid;                  //转向点id
};

//内部使用，保存某一顶点到v的权值，以及边id，用在VertexNeighbors中
struct Vertex_to
{
    inline Vertex_to()
        {v.vid = 0; to_weight = 0; eid = 0;};
    inline Vertex_to(const Vertex& vertex, const double w, const long edge_id)
        {v = vertex; to_weight = w; eid = edge_id;};
    Vertex v;
    double to_weight;   //这个用于在VertexNeighbors中保存顶点到neighbor点的权值
    long eid;  //从顶点到v的边的eid，算法中没用，调用者有用
};

//内部使用，顶点的相邻点（with weight）列表
struct VertexNeighbors
{
    VertexNeighbors()
        {vid = 0;};
    dword vid;                              //顶点id
    map<dword, Vertex_to> vertexes;         //相邻顶点列表，保存每个相邻顶点到该顶点的权值，key为vid
};

//内部使用，权值和id，支持weight的比较运算
struct WeightID
{
    inline WeightID()
    {
        weight = 0; vid = 0; eid = 0;
    };

    inline WeightID(const double w, const dword w_vid, const dword w_eid)
    {
        weight = w; vid = w_vid; eid = w_eid;
    };

    inline bool operator==(const WeightID& rhs) const
    {
        return (this->weight == rhs.weight) ? true : false;
    };

    inline bool operator>(const WeightID& rhs) const
    {
        return (this->weight > rhs.weight) ? true : false;
    };

    inline bool operator<(const WeightID& rhs) const
    {
        return (this->weight < rhs.weight) ? true : false;
    };

    double weight;
    dword vid;
    long eid;
};


//=======================================================================
//  整个路网的拓扑结构，以及一个简化的最短路径算法
//=======================================================================
class NetTopo
{
private:
    double _tolerance;                      //容差

    //-------------------------------------------------------------------
    //  顶点列表
    //-------------------------------------------------------------------
    dword _max_vid;
    map<dword, Vertex> _vertexes;           //顶点列表
                                            //key为顶点vid

    dword _max_tid;
    map<dword, TurnPoint> _turnpoints;      //转向点列表
                                            //key为转向点tid

    //-------------------------------------------------------------------
    //  网络的静态拓扑结构
    //-------------------------------------------------------------------
    map<dword, VertexNeighbors*> _topo_map; //顶点 <==> 相邻点映射
                                            //key为该顶点vid
                                            //value为以该顶点为起点的单向边终点
    //-------------------------------------------------------------------


    //-------------------------------------------------------------------
    //  顺序路径点、障碍点及障碍边，key为_vertexes中的key
    //-------------------------------------------------------------------
    vector<dword>   _routes;
    set<dword>      _barriers;
    vector<Edge>    _blockedge;
    //-------------------------------------------------------------------

    //-------------------------------------------------------------------
    //  计算最短路径时的临时结构，key为顶点vid
    //-------------------------------------------------------------------
    set<dword> _vs_untag;                   //未标记节点列表
    set<dword> _vs_temptag;                 //临时标记节点列表
    set<dword> _vs_permtag;                 //永久标记节点列表
    //-------------------------------------------------------------------


    //  通过坐标找顶点
    dword _findvertex(const WKSPoint& point) const;

    //  在增加路径点及障碍点之前判断是否重复
    bool _checkrepeat(const dword vid) const;

    //  运算之前预处理静态结构
    void _before();

    //  运算结束后恢复原始静态结构
    void _after();


    //-------------------------------------------------------------------
    //  简化的快速最短路径算法，不用权，速度比较快，但功能很弱
    //-------------------------------------------------------------------
    //  两点间最短路径，简化算法
    //  path为路径的顶点key列表，注意from不会加到path中
    //  返回值为false则搜索失败（没找到终点）
    bool _fastshortest_2vertexes(const dword from, const dword to, vector<dword>& path,
        vector<long>& edge_ids);

    //  递归函数，last为已经找到的上一个顶点，to为终点
    //  返回true代表找到终点，搜索成功
    bool _fastshortest_step(const dword last, const Vertex& to, vector<dword>& path,
        vector<long>& edge_ids);

    //  顺序经过多个顶点的最佳路径搜索，path为路径的顺序顶点列表，edge_ids为经过的边id列表
    //  返回值为false则搜索失败
    bool _fastshortest(vector<dword>& path, vector<long>& edge_ids);
    //-------------------------------------------------------------------

public:
    NetTopo();
    ~NetTopo();

    void SetTolerance(const double tolerance);
    double GetTolerance() const;

    //  清除所有的静态结构、临时结构、路径点、障碍点、转向点
    void ClearAll();

    bool equal(const double& a, const double& b) const;
    bool equal(const WKSPoint& a, const WKSPoint& b) const;

    //  向网络中增加一条边
    //  注意edge_id为调用者自定义的边id，算法对此不作要求
    void add_edge(const WKSPoint& from, const WKSPoint& to, const double& weight,
        const long edge_id, bool bidirectional = true);

    //  增加、删除路径点
    bool add_route(const WKSPoint& route);
    Vertex get_route(const WKSPoint& route) const;
    Vertex get_route(const dword i) const;
    bool remove_route(const WKSPoint& route);
    dword get_routecount() const;
    void clear_routes();

    //  增加、删除障碍点
    bool add_barrier_vertex(const WKSPoint& barrier);
    Vertex get_barrier_vertex(const WKSPoint& barrier) const;
    Vertex get_barrier_vertex(const dword i) const;
    bool remove_barrier_vertex(const WKSPoint& barrier);
    dword get_barrier_vertexcount() const;
    void clear_barrier_vertexes();

    //  增加、删除障碍边
    //  注意这里的边edge_id为用户定义，算法不做要求
    //  一个edge_id可以对应多个单向边
    bool add_blocked_edge(const WKSPoint& from, const WKSPoint& to,
        Edge& blocked_edge);
    bool get_blocked_edge(const WKSPoint& from, const WKSPoint& to,
        Edge& blocked_edge) const;
    bool remove_blocked_edge(const WKSPoint& from, const WKSPoint& to);
    dword add_blocked_edges(const long blocked_edgeid);
    dword remove_blocked_edges(const long blocked_edgeid);
    dword add_blocked_lineedges(const WKSPoint& point_on_edge, vector<Edge>& blocked_edges);
    dword get_blocked_lineedges(const WKSPoint& point_on_edge, vector<Edge>& blocked_edges) const;
    dword get_blocked_edgecount() const;
    bool get_blocked_edge_byindex(const dword i, long& blocked_edgeid,
        WKSPoint& from, WKSPoint& to) const;
    void clear_blocked_edges();


    //  简化的快速最短路径算法，返回值：true - 成功找到终点
    //  输出：vs是路径顺序顶点列表，edge_ids为路径边id列表
    bool fastshortest(vector<Vertex>& vs, vector<long>& edge_ids);

friend dword NetTopo2Stream(const NetTopo& nettopo, CStreamPtr pStream);
friend dword Stream2NetTopo(CStreamPtr pStream, NetTopo& nettopo);
};


//  nettopo <-> stream
//  将网络的静态结构、路径点及障碍点序列化
dword NetTopo2Stream(const NetTopo& nettopo, CStreamPtr pStream);
dword Stream2NetTopo(CStreamPtr pStream, NetTopo& nettopo);

}

}

#endif
