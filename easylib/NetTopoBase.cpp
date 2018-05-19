#include "NetTopoBase.h"
#include "MathLib.h"
#include <math.h>

namespace easymap
{

namespace net
{

NetTopo::NetTopo()
{
    _max_vid = 0;
    _max_tid = 0;
    _tolerance = 0.1;
}

NetTopo::~NetTopo()
{
    this->ClearAll();
}

void NetTopo::SetTolerance(const double tolerance)
{
    _tolerance = tolerance;
}

double NetTopo::GetTolerance() const
{
    return _tolerance;
}

void NetTopo::ClearAll()
{
    map<dword, VertexNeighbors*>::iterator it = _topo_map.begin();
    while (it != _topo_map.end())
    {
        delete it->second;
        it++;
    }

    _vertexes.clear();
    _turnpoints.clear();
    _topo_map.clear();
    _routes.clear();
    _barriers.clear();
    _blockedge.clear();
    _vs_untag.clear();
    _vs_temptag.clear();
    _vs_permtag.clear();
}

bool NetTopo::equal(const double& a, const double& b) const
{
    if (fabs(a - b) < _tolerance)
    {
        return true;
    }

    return false;
}

bool NetTopo::equal(const WKSPoint& a, const WKSPoint& b) const
{
    if ((fabs(a.x - b.x) < _tolerance) && (fabs(a.y - b.y) < _tolerance))
    {
        return true;
    }

    return false;
}

void NetTopo::add_edge(const WKSPoint& from, const WKSPoint& to, const double& weight,
    const long edge_id, bool bidirectional)
{
    dword vid_from = 0;
    dword vid_to = 0; 
    dword eid = 0;

    Vertex vertex_from;
    Vertex vertex_to;
    //首先检查顶点是否已经在列表中
    map<dword, Vertex>::const_iterator it_vertexes = _vertexes.begin();
    while (it_vertexes != _vertexes.end())
    {
        if (this->equal(it_vertexes->second, from))
        {
            //起点已经存在
            vid_from = it_vertexes->first;
            vertex_from = it_vertexes->second;
        }

        if (this->equal(it_vertexes->second, to))
        {
            //终点已经存在
            vid_to = it_vertexes->first;
            vertex_to = it_vertexes->second;
        }

        if ((vid_from > 0) && (vid_to > 0))
        {
            break;
        }

        it_vertexes++;
    }

    if (vid_from == 0)
    {
        //起点是新顶点
        vertex_from.vid = vid_from = ++_max_vid;
        vertex_from.x = from.x;
        vertex_from.y = from.y;
        _vertexes[vid_from] = vertex_from;
    }

    if (vid_to == 0)
    {
        //终点是新顶点
        vertex_to.vid = vid_to = ++_max_vid;
        vertex_to.x = to.x;
        vertex_to.y = to.y;
        _vertexes[vid_to] = vertex_to;
    }

    //－－－－－－－－ 用起点更新网络拓扑 －－－－－－－－－－
    VertexNeighbors* pVertexNeighbors = NULL;
    map<dword, VertexNeighbors*>::const_iterator it_topo_map = _topo_map.find(vid_from);
    if (it_topo_map != _topo_map.end())
    {
        pVertexNeighbors = it_topo_map->second;
    }
    else
    {
        pVertexNeighbors = new VertexNeighbors;
        _topo_map[vid_from] = pVertexNeighbors;
    }

    //终点与起点相邻
    Vertex_to vt_to(vertex_to, weight, edge_id);
    pVertexNeighbors->vertexes[vid_to] = vt_to;
    //－－－－－－－－－－－－－－－－－－－－－－－－－－－－


    //－－－－－－－－ 用终点更新网络拓扑 －－－－－－－－－－
    pVertexNeighbors = NULL;
    it_topo_map = _topo_map.find(vid_to);
    if (it_topo_map != _topo_map.end())
    {
        pVertexNeighbors = it_topo_map->second;
    }
    else
    {
        pVertexNeighbors = new VertexNeighbors;
        _topo_map[vid_to] = pVertexNeighbors;
    }

    if (bidirectional)
    {
        //双向边，起点与终点相邻
        Vertex_to vt_from(vertex_from, weight, edge_id);
        pVertexNeighbors->vertexes[vid_from] = vt_from;
    }
    //－－－－－－－－－－－－－－－－－－－－－－－－－－－－
}

dword NetTopo::_findvertex(const WKSPoint& point) const
{
    map<dword, Vertex>::const_iterator it = _vertexes.begin();
    while (it != _vertexes.end())
    {
        const WKSPoint& pnt = it->second;
        if (equal(pnt, point))
        {
            return it->first;
        }
        it++;
    }

    return 0;
}

bool NetTopo::_checkrepeat(const dword vid) const
{
    vector<dword>::const_iterator it_routes = _routes.begin();
    while (it_routes != _routes.end())
    {
        if (vid == *it_routes)
        {
            return true;
        }

        it_routes++;
    }

    set<dword>::const_iterator it_barriers = _barriers.find(vid);
    if (it_barriers != _barriers.end())
    {
        return true;
    }

    return false;
}

void NetTopo::_before()
{
    //在静态拓扑结构中打断每条障碍边
    for (long i = 0; i < _blockedge.size(); i++)
    {
        //这条障碍边的起点
        VertexNeighbors* pVN = _topo_map[_blockedge[i].from];
        //干掉终点
        pVN->vertexes.erase(_blockedge[i].to);
    }
}

void NetTopo::_after()
{
    //恢复静态拓扑结构，原先每条障碍边的数据
    for (long i = 0; i < _blockedge.size(); i++)
    {
        //这条障碍边的起点
        VertexNeighbors* pVN = _topo_map[_blockedge[i].from];
        //得到障碍边的终点信息
        Vertex v_to = _vertexes[_blockedge[i].to];
        Vertex_to to(v_to, _blockedge[i].weight, _blockedge[i].eid);
        //加上终点
        pVN->vertexes[_blockedge[i].to] = to;
    }
}

bool NetTopo::add_route(const WKSPoint& route)
{
    dword vid = _findvertex(route);
    if (vid == 0)
    {
        return false;
    }

    if (this->_checkrepeat(vid))
    {
        return false;
    }

    _routes.push_back(vid);
    return true;
}

bool NetTopo::add_barrier_vertex(const WKSPoint& barrier)
{
    dword vid = _findvertex(barrier);
    if (vid == 0)
    {
        return false;
    }

    if (this->_checkrepeat(vid))
    {
        return false;
    }

    _barriers.insert(vid);
    return true;
}

Vertex NetTopo::get_route(const WKSPoint& route) const
{
    Vertex r;
    for (long i = 0; i < _routes.size(); i++)
    {
        dword vid = _routes[i];
        map<dword, Vertex>::const_iterator it_vertexes = _vertexes.find(vid);
        if (it_vertexes != _vertexes.end())
        {
            Vertex v = it_vertexes->second;
            if (equal(v, route))
            {
                r = v;
                break;
            }
        }
    }

    return r;
}

Vertex NetTopo::get_route(const dword i) const
{
    Vertex v_null;

    if (i > _routes.size())
    {
        return v_null;
    }

    dword vid = _routes[i];
    map<dword, Vertex>::const_iterator it_vertexes = _vertexes.find(vid);
    if (it_vertexes != _vertexes.end())
    {
        return it_vertexes->second;
    }

    return v_null;
}

Vertex NetTopo::get_barrier_vertex(const WKSPoint& barrier) const
{
    Vertex r;
    set<dword>::const_iterator it_barriers = _barriers.begin();
    while (it_barriers != _barriers.end())
    {
        dword vid = *it_barriers;
        map<dword, Vertex>::const_iterator it_vertexes = _vertexes.find(vid);
        if (it_vertexes != _vertexes.end())
        {
            Vertex v = it_vertexes->second;
            if (equal(v, barrier))
            {
                r = v;
                break;
            }
        }

        it_barriers++;
    }

    return r;
}

Vertex NetTopo::get_barrier_vertex(const dword i) const
{
    Vertex v_null;

    if (i > _barriers.size())
    {
        return v_null;
    }

    set<dword>::const_iterator it_barriers = _barriers.begin();
    std::advance(it_barriers, i);
    map<dword, Vertex>::const_iterator it_vertexes = _vertexes.find(*it_barriers);
    if (it_vertexes != _vertexes.end())
    {
        return it_vertexes->second;
    }

    return v_null;
}

bool NetTopo::remove_route(const WKSPoint& route)
{
    vector<dword>::iterator it_routes = _routes.begin();
    while (it_routes != _routes.end())
    {
        dword vid = *it_routes;
        map<dword, Vertex>::const_iterator it_vertexes = _vertexes.find(vid);
        if (it_vertexes != _vertexes.end())
        {
            Vertex v = it_vertexes->second;
            if (equal(v, route))
            {
                _routes.erase(it_routes);
                return true;
            }
        }

        it_routes++;
    }

    return false;
}

bool NetTopo::remove_barrier_vertex(const WKSPoint& barrier)
{
    set<dword>::iterator it_barriers = _barriers.begin();
    while (it_barriers != _barriers.end())
    {
        dword vid = *it_barriers;
        map<dword, Vertex>::const_iterator it_vertexes = _vertexes.find(vid);
        if (it_vertexes != _vertexes.end())
        {
            Vertex v = it_vertexes->second;
            if (equal(v, barrier))
            {
                _barriers.erase(it_barriers);
                return true;
            }
        }

        it_barriers++;
    }

    return false;
}

dword NetTopo::get_routecount() const
{
    return _routes.size();
}

dword NetTopo::get_barrier_vertexcount() const
{
    return _barriers.size();
}

void NetTopo::clear_routes()
{
    _routes.clear();
}

void NetTopo::clear_barrier_vertexes()
{
    _barriers.clear();
}

bool NetTopo::add_blocked_edge(const WKSPoint& from, const WKSPoint& to,
    Edge& blocked_edge)
{
    map<dword, VertexNeighbors*>::const_iterator it_topo = _topo_map.begin();
    while (it_topo != _topo_map.end())
    {
        VertexNeighbors* pVN = it_topo->second;
        map<dword, Vertex_to>::const_iterator it_vertexes = pVN->vertexes.begin();
        while (it_vertexes != pVN->vertexes.end())
        {
            map<dword, Vertex>::const_iterator it_vertexes_from = _vertexes.find(it_topo->first);
            map<dword, Vertex>::const_iterator it_vertexes_to = _vertexes.find(it_vertexes->first);
            Vertex from_topo = it_vertexes_from->second;
            Vertex to_topo = it_vertexes_to->second;
            if (equal(from, from_topo) && equal(to, to_topo))
            {
                blocked_edge.from = it_topo->first;
                blocked_edge.to = it_vertexes->first;
                blocked_edge.weight = it_vertexes->second.to_weight;
                blocked_edge.eid = it_vertexes->second.eid;

                vector<Edge>::const_iterator it_blockedge = _blockedge.begin();
                while (it_blockedge != _blockedge.end())
                {
                    Edge e_tmp = *it_blockedge;
                    if ((e_tmp.from == blocked_edge.from)
                        && (e_tmp.to == blocked_edge.to))
                    {
                        //该障碍边已经存在，不能重复添加
                        return false;
                    }
                    it_blockedge++;
                }

                _blockedge.push_back(blocked_edge);
                return true;
            }

            it_vertexes++;
        }

        it_topo++;
    }

    return false;
}

bool NetTopo::get_blocked_edge(const WKSPoint& from, const WKSPoint& to,
    Edge& blocked_edge) const
{
    map<dword, VertexNeighbors*>::const_iterator it_topo = _topo_map.begin();
    while (it_topo != _topo_map.end())
    {
        VertexNeighbors* pVN = it_topo->second;
        map<dword, Vertex_to>::const_iterator it_vertexes = pVN->vertexes.begin();
        while (it_vertexes != pVN->vertexes.end())
        {
            map<dword, Vertex>::const_iterator it_vertexes_from = _vertexes.find(it_topo->first);
            map<dword, Vertex>::const_iterator it_vertexes_to = _vertexes.find(it_vertexes->first);
            Vertex from_topo = it_vertexes_from->second;
            Vertex to_topo = it_vertexes_to->second;
            if (equal(from, from_topo) && equal(to, to_topo))
            {
                blocked_edge.from = it_topo->first;
                blocked_edge.to = it_vertexes->first;
                blocked_edge.weight = it_vertexes->second.to_weight;
                blocked_edge.eid = it_vertexes->second.eid;

                for (long i = 0; i < _blockedge.size(); i++)
                {
                    if ((_blockedge[i].from == blocked_edge.from)
                        && (_blockedge[i].to == blocked_edge.to))
                    {
                        return true;
                    }
                }

                return false;
            }

            it_vertexes++;
        }

        it_topo++;
    }

    return false;
}

bool NetTopo::remove_blocked_edge(const WKSPoint& from, const WKSPoint& to)
{
    Edge blocked_edge;
    map<dword, VertexNeighbors*>::const_iterator it_topo = _topo_map.begin();
    while (it_topo != _topo_map.end())
    {
        VertexNeighbors* pVN = it_topo->second;
        map<dword, Vertex_to>::const_iterator it_vertexes = pVN->vertexes.begin();
        while (it_vertexes != pVN->vertexes.end())
        {
            map<dword, Vertex>::const_iterator it_vertexes_from = _vertexes.find(it_topo->first);
            map<dword, Vertex>::const_iterator it_vertexes_to = _vertexes.find(it_vertexes->first);
            Vertex from_topo = it_vertexes_from->second;
            Vertex to_topo = it_vertexes_to->second;
            if (equal(from, from_topo) && equal(to, to_topo))
            {
                blocked_edge.from = it_topo->first;
                blocked_edge.to = it_vertexes->first;
                blocked_edge.weight = it_vertexes->second.to_weight;
                blocked_edge.eid = it_vertexes->second.eid;
                vector<Edge>::iterator it_blockedge = _blockedge.begin();
                while (it_blockedge != _blockedge.end())
                {
                    Edge e_tmp = *it_blockedge;
                    if ((e_tmp.from == blocked_edge.from)
                        && (e_tmp.to == blocked_edge.to))
                    {
                        _blockedge.erase(it_blockedge);
                        return true;
                    }

                    it_blockedge++;
                }

                return false;
            }

            it_vertexes++;
        }

        it_topo++;
    }

    return false;
}

dword NetTopo::add_blocked_edges(const long blocked_edgeid)
{
    dword r = 0;
    map<dword, VertexNeighbors*>::const_iterator it_topo = _topo_map.begin();
    while (it_topo != _topo_map.end())
    {
        VertexNeighbors* pVN = it_topo->second;
        map<dword, Vertex_to>::const_iterator it_vertexes = pVN->vertexes.begin();
        while (it_vertexes != pVN->vertexes.end())
        {
            Vertex_to vto = it_vertexes->second;
            if (vto.eid == blocked_edgeid)
            {
                Edge edge;
                edge.from = it_topo->first;
                edge.to = it_vertexes->first;
                edge.weight = vto.to_weight;
                edge.eid = vto.eid;
                _blockedge.push_back(edge);
                r++;
            }

            it_vertexes++;
        }

        it_topo++;
    }

    return r;
}

dword NetTopo::remove_blocked_edges(const long blocked_edgeid)
{
    dword r = 0;
    vector<Edge>::iterator it = _blockedge.begin();
    while (it != _blockedge.end())
    {
        if ((*it).eid == blocked_edgeid)
        {
            _blockedge.erase(it);
            it = _blockedge.begin();
            r++;
            continue;
        }

        it++;
    }

    return r;
}


dword NetTopo::add_blocked_lineedges(const WKSPoint& point_on_edge, vector<Edge>& blocked_edges)
{
    dword r = 0;
    map<dword, VertexNeighbors*>::const_iterator it_topo = _topo_map.begin();
    while (it_topo != _topo_map.end())
    {
        VertexNeighbors* pVN = it_topo->second;
        map<dword, Vertex_to>::const_iterator it_vertexes = pVN->vertexes.begin();
        while (it_vertexes != pVN->vertexes.end())
        {
            map<dword, Vertex>::const_iterator it_vertexes_from = _vertexes.find(it_topo->first);
            map<dword, Vertex>::const_iterator it_vertexes_to = _vertexes.find(it_vertexes->first);
            Vertex from = it_vertexes_from->second;
            Vertex to = it_vertexes_to->second;
            double d = mathlib::PointToLineSegment(from, to, point_on_edge);
            if (d < _tolerance)
            {
                Edge edge;
                edge.from = it_topo->first;
                edge.to = it_vertexes->first;
                edge.weight = it_vertexes->second.to_weight;
                edge.eid = it_vertexes->second.eid;
                _blockedge.push_back(edge);
                blocked_edges.push_back(edge);
                r++;
            }

            it_vertexes++;
        }

        it_topo++;
    }

    return r;
}

dword NetTopo::get_blocked_lineedges(const WKSPoint& point_on_edge, vector<Edge>& blocked_edges) const
{
    dword r = 0;
    map<dword, VertexNeighbors*>::const_iterator it_topo = _topo_map.begin();
    while (it_topo != _topo_map.end())
    {
        VertexNeighbors* pVN = it_topo->second;
        map<dword, Vertex_to>::const_iterator it_vertexes = pVN->vertexes.begin();
        while (it_vertexes != pVN->vertexes.end())
        {
            map<dword, Vertex>::const_iterator it_vertexes_from = _vertexes.find(it_topo->first);
            map<dword, Vertex>::const_iterator it_vertexes_to = _vertexes.find(it_vertexes->first);
            Vertex from = it_vertexes_from->second;
            Vertex to = it_vertexes_to->second;
            double d = mathlib::PointToLineSegment(from, to, point_on_edge);
            if (d < _tolerance)
            {
                for (long i = 0; i < _blockedge.size(); i++)
                {
                    if (it_vertexes->second.eid == _blockedge[i].eid)
                    {
                        blocked_edges.push_back(_blockedge[i]);
                        r++;
                    }
                }
            }

            it_vertexes++;
        }

        it_topo++;
    }

    return r;
}

dword NetTopo::get_blocked_edgecount() const
{
    return _blockedge.size();
}

bool NetTopo::get_blocked_edge_byindex(const dword i, long& blocked_edgeid,
    WKSPoint& from, WKSPoint& to) const
{
    if (i >= _blockedge.size())
    {
        return false;
    }

    blocked_edgeid = _blockedge[i].eid;
    map<dword, Vertex>::const_iterator it = _vertexes.find(_blockedge[i].from);
    from = it->second;
    it = _vertexes.find(_blockedge[i].to);
    to = it->second;
    return true;
}

void NetTopo::clear_blocked_edges()
{
    _blockedge.clear();
}

bool NetTopo::_fastshortest_2vertexes(const dword from, const dword to, vector<dword>& path,
    vector<long>& edge_ids)
{
    _vs_untag.clear();
    _vs_temptag.clear();
    _vs_permtag.clear();

    //首先，所有的顶点都是未标记的，除了from
    map<dword, Vertex>::const_iterator it_vertexes = _vertexes.begin();
    while (it_vertexes != _vertexes.end())
    {
        _vs_untag.insert(it_vertexes->first);
        it_vertexes++;
    }

    //起点from
    set<dword>::iterator it_vs_untag = _vs_untag.find(from);
    if (it_vs_untag == _vs_untag.end())
    {
        return false;
    }

    //from是永久标记顶点
    _vs_untag.erase(it_vs_untag);
    _vs_permtag.insert(from);

    //开始递归
    map<dword, Vertex>::const_iterator it_vertex_to = _vertexes.find(to);
    Vertex to_vertex = it_vertex_to->second;
    return this->_fastshortest_step(from, to_vertex, path, edge_ids);
}

bool NetTopo::_fastshortest_step(const dword last, const Vertex& to,
    vector<dword>& path, vector<long>& edge_ids)
{
    vector<vector<dword> > subpathes;
    vector<vector<dword> > subedges;

    //找到与last相连的所有顶点，进行下一级递归
    map<dword, VertexNeighbors*>::const_iterator it_topo = _topo_map.find(last);
    VertexNeighbors* pVNs = it_topo->second;
    vector<WeightID> dist_to_wids;
    map<dword, Vertex_to>::const_iterator it_vns_v = pVNs->vertexes.begin();
    while (it_vns_v != pVNs->vertexes.end())
    {
        //判断一下是否为终点
        if (it_vns_v->first == to.vid)
        {
            //搜索成功 :D
            path.push_back(to.vid);
            edge_ids.push_back(it_vns_v->second.eid);

            _vs_permtag.insert(it_vns_v->first);
            _vs_temptag.erase(it_vns_v->first);
            _vs_untag.erase(it_vns_v->first);

            return true;
        }

        //排除障碍点
        if (_barriers.size() > 0)
        {
            set<dword>::const_iterator it_barriers = _barriers.find(it_vns_v->first);
            if (it_barriers != _barriers.end())
            {
                it_vns_v++;
                continue;
            }
        }

        //排除永久标记顶点，避免找到上一个顶点
        if (_vs_permtag.find(it_vns_v->first) != _vs_permtag.end())
        {
            it_vns_v++;
            continue;
        }

        //排除临时标记顶点  ??
//        if (_vs_temptag.find(it_vns_v->first) != _vs_temptag.end())
//        {
//            it_vns_v++;
//            continue;
//        }

        //记录每个待选顶点到终点的长度
        WeightID dist_to_wid(mathlib::Distance(it_vns_v->second.v, to),
          it_vns_v->first, it_vns_v->second.eid);
        dist_to_wids.push_back(dist_to_wid);

        //先暂时都扔到临时标记顶点列表中
        _vs_temptag.insert(it_vns_v->first);
        _vs_untag.erase(it_vns_v->first);
        it_vns_v++;
    }

    if (dist_to_wids.size() == 0)
    {
        //搜索到头，毛都没找到
        return false;
    }

    //按dist_to从小到大设置永久标记，并依次作为下一个路径点，继续递归
    std::sort(dist_to_wids.begin(), dist_to_wids.end());
    vector<WeightID>::const_iterator it_dist_to = dist_to_wids.begin();
    while (it_dist_to != dist_to_wids.end())
    {
        //将这个临时标记变成永久标记
        _vs_temptag.erase((*it_dist_to).vid);
        _vs_permtag.insert((*it_dist_to).vid);
        path.push_back((*it_dist_to).vid);
        edge_ids.push_back((*it_dist_to).eid);
        //递归下一级
        if (this->_fastshortest_step((*it_dist_to).vid, to, path, edge_ids))
        {
            //快速最短路径搜索，直接搞定
            return true;
        }
        else
        {
            //死路，继续下一个顶点
            path.pop_back();
            edge_ids.pop_back();
        }

        it_dist_to++;
    }

    //快速最短路径搜索失败
    return false;
}

bool NetTopo::_fastshortest(vector<dword>& path, vector<long>& edge_ids)
{
    if (_routes.size() < 2)
    {
        return false;
    }

    //先把起点添加到列表中
    path.push_back(_routes[0]);

    //顺序搜索每一段
    vector<dword>::const_iterator it_routes = _routes.begin();
    dword vid_1 = *it_routes;
    it_routes++;
    while (it_routes != _routes.end())
    {
        dword vid_2 = *it_routes;
        if (!this->_fastshortest_2vertexes(vid_1, vid_2, path, edge_ids))
        {
            return false;
        }
        vid_1 = vid_2;
        it_routes++;
    }

    return true;
}

bool NetTopo::fastshortest(vector<Vertex>& vs, vector<long>& edge_ids)
{
    this->_before();

    vector<dword> vids;
    bool r = this->_fastshortest(vids, edge_ids);
    for (long i = 0; i < vids.size(); i++)
    {
        map<dword, Vertex>::const_iterator it_vs = _vertexes.find(vids[i]);
        vs.push_back(it_vs->second);
    }

    this->_after();

    std::unique(edge_ids.begin(), edge_ids.end());
    return r;
}




dword NetTopo2Stream(const NetTopo& nettopo, CStreamPtr pStream)
{
    dword oldpos = pStream->GetPos();

    //保存容差
    pStream->Write(nettopo._tolerance);

    //保存顶点列表
    pStream->Write(nettopo._max_vid);
    long count = nettopo._vertexes.size();
    pStream->Write(count);
    map<dword, Vertex>::const_iterator it_vertexes = nettopo._vertexes.begin();
    while (it_vertexes != nettopo._vertexes.end())
    {
        dword vid = it_vertexes->first;
        pStream->Write(vid);
        Vertex v = it_vertexes->second;
        pStream->Write(&v, sizeof(Vertex));
        it_vertexes++;
    }

    //保存转向点列表
    pStream->Write(nettopo._max_tid);
    count = nettopo._turnpoints.size();
    pStream->Write(count);
    map<dword, TurnPoint>::const_iterator it_turnpoints = nettopo._turnpoints.begin();
    while (it_turnpoints != nettopo._turnpoints.end())
    {
        dword tid = it_turnpoints->first;
        pStream->Write(tid);
        TurnPoint t = it_turnpoints->second;
        pStream->Write(&t, sizeof(TurnPoint));
        it_turnpoints++;
    }

    //保存顶点 <==> 相邻点、边映射
    count = nettopo._topo_map.size();
    pStream->Write(count);
    map<dword, VertexNeighbors*>::const_iterator it_topo = nettopo._topo_map.begin();
    while (it_topo != nettopo._topo_map.end())
    {
        dword vid = it_topo->first;
        pStream->Write(vid);
        VertexNeighbors& vn = *it_topo->second;

        //相邻顶点列表
        count = vn.vertexes.size();
        pStream->Write(count);
        map<dword, Vertex_to>::const_iterator it_vertexes = vn.vertexes.begin();
        while (it_vertexes != vn.vertexes.end())
        {
            pStream->Write(it_vertexes->first);
            pStream->Write(&it_vertexes->second, sizeof(Vertex_to));
            it_vertexes++;
        }

        it_topo++;
    }

    //顺序路径点
    count = nettopo._routes.size();
    pStream->Write(count);
    long i;
    for (i = 0; i < nettopo._routes.size(); i++)
    {
        pStream->Write(nettopo._routes[i]);
    }

    //障碍点
    count = nettopo._barriers.size();
    pStream->Write(count);
    set<dword>::const_iterator it_barriers = nettopo._barriers.begin();
    while (it_barriers != nettopo._barriers.end())
    {
        pStream->Write(*it_barriers);
        it_barriers++;
    }

    //障碍边
    count = nettopo._blockedge.size();
    pStream->Write(count);
    for (i = 0; i < nettopo._blockedge.size(); i++)
    {
        Edge e = nettopo._blockedge[i];
        pStream->Write(&e, sizeof(Edge));
    }

    return pStream->GetPos() - oldpos;
}

dword Stream2NetTopo(CStreamPtr pStream, NetTopo& nettopo)
{
    nettopo.ClearAll();
    dword oldpos = pStream->GetPos();

    //恢复容差
    pStream->Read(nettopo._tolerance);

    //恢复顶点列表
    pStream->Read(nettopo._max_vid);
    long i, count;
    pStream->Read(count);
    for (i = 0; i < count; i++)
    {
        dword vid;
        pStream->Read(vid);
        Vertex v;
        pStream->Read(&v, sizeof(Vertex));
        nettopo._vertexes[vid] = v;
    }

    //恢复转向点列表
    pStream->Read(nettopo._max_tid);
    pStream->Read(count);
    for (i = 0; i < count; i++)
    {
        dword tid;
        pStream->Read(tid);
        TurnPoint tp;
        pStream->Read(&tp, sizeof(TurnPoint));
        nettopo._turnpoints[tid] = tp;
    }

    //恢复顶点 <==> 相邻点、边映射
    pStream->Read(count);
    for (i = 0; i < count; i++)
    {
        dword vid;
        pStream->Read(vid);
        VertexNeighbors* pVN = new VertexNeighbors;
        pVN->vid = vid;
        //相邻顶点列表
        dword n_count;
        pStream->Read(n_count);
        for (long i_nv = 0; i_nv < n_count; i_nv++)
        {
            dword nvid;
            pStream->Read(nvid);
            Vertex_to nvt;
            pStream->Read(&nvt, sizeof(Vertex_to));
            pVN->vertexes[nvid] = nvt;
        }

        nettopo._topo_map[vid] = pVN;
    }

    //顺序路径点
    pStream->Read(count);
    for (i = 0; i < count; i++)
    {
        dword ri;
        pStream->Read(ri);
        nettopo._routes.push_back(ri);
    }

    //障碍点
    pStream->Read(count);
    for (i = 0; i < count; i++)
    {
        dword bi;
        pStream->Read(bi);
        nettopo._barriers.insert(bi);
    }

    //障碍边
    pStream->Read(count);
    for (i = 0; i < count; i++)
    {
        Edge e;
        pStream->Read(&e, sizeof(Edge));
        nettopo._blockedge.push_back(e);
    }

    return pStream->GetPos() - oldpos;
}

}

}