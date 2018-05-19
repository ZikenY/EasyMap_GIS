#include "CommonInclude.h"
#include "ShapeAux.h"
#include "StringFuncs.h"

namespace easymap
{

//将ShapeLib几何类型转换为图层类型
ShapeType ShpType2ShapeType(const int shptype)
{
    switch (shptype)
    {
    case SHPT_POINT:
    case SHPT_POINTZ:
    case SHPT_POINTM:
        return SHAPETYPE_POINT;

    case SHPT_MULTIPOINT:
    case SHPT_MULTIPOINTZ:
    case SHPT_MULTIPOINTM:
        return SHAPETYPE_MULTIPOINT;

    case SHPT_ARC:
    case SHPT_ARCZ:
    case SHPT_ARCM:
        return SHAPETYPE_POLYLINE;

    case SHPT_POLYGON:
    case SHPT_POLYGONZ:
    case SHPT_POLYGONM:
    case SHPT_MULTIPATCH:
        return SHAPETYPE_POLYGON;

    default:
        return SHAPETYPE_UNKNOWN;
    }
}

int ShapeType2ShpType(const ShapeType shapetype)
{
    switch (shapetype)
    {
    case SHAPETYPE_POINT:
        return SHPT_POINTZ;

    case SHAPETYPE_MULTIPOINT:
        return SHPT_MULTIPOINTZ;

    case SHAPETYPE_POLYLINE:
        return SHPT_ARCZ;

    case SHAPETYPE_POLYGON:
        return SHPT_MULTIPATCH;

    default:
        return SHPT_NULL;
    }
}

int GeoType2ShapeType(const GeometryType geotype)
{
    switch (geotype)
    {
    case GEOMETRYTYPE_POINT:
        return SHPT_POINTZ;

    case GEOMETRYTYPE_MULTIPOINT:
        return SHPT_MULTIPOINTZ;

    case GEOMETRYTYPE_POLYLINE:
        return SHPT_ARCZ;

    case GEOMETRYTYPE_POLYGON:
        return SHPT_MULTIPATCH;

    default:
        return SHPT_NULL;
    }
}

//读取Shape文件内的地物并添加到slimlayer中
long AddShapes2SlimLayer(const SHPHandle pShp, const DBFHandle pDbf,
    CSlimLayerPtr pSlimLayer, const long annofield)
{
    if (!pShp || !pSlimLayer.Assigned())
    {
        return -1;
    }

//    pSlimLayer->SetAttribTag(0);
    int entitiecount;
    int shapetype;
    ::SHPGetInfo(pShp, &entitiecount, &shapetype, NULL, NULL);

    static DBFFieldType fieldtypearray[200];
    int fieldcount = -1;
    if (pDbf)
    {
        int dbfrecordcount = ::DBFGetRecordCount(pDbf);
        if (dbfrecordcount == entitiecount)
        {
            fieldcount = ::DBFGetFieldCount(pDbf);

            for (long i = 0; i < fieldcount; i++)
            {
                int fieldwidth, fielddecimals;
                char fieldname[50];
                DBFFieldType fieldtype = ::DBFGetFieldInfo(pDbf, i,
                    fieldname, &fieldwidth, &fielddecimals);
                fieldtypearray[i] = fieldtype;
            }
        }
    }

    string attrib;

    for (long i = 0; i < entitiecount; i++)
    {
        SHPObject* pSHPObject = ::SHPReadObject(pShp, i);
        if (pSHPObject)
        {
            IGeometryPtr pGeometry;
            switch (pSHPObject->nSHPType)
            {
            case SHPT_POINT:
            case SHPT_POINTZ:
            case SHPT_POINTM:
                {
                    WKSPointZ point;
                    point.x = pSHPObject->padfX[0];
                    point.y = pSHPObject->padfY[0];
                    point.z = pSHPObject->padfZ[0];

                    pGeometry = (IGeometry*)(new CPoint(point));
                }
                break;

            case SHPT_MULTIPOINT:
            case SHPT_MULTIPOINTZ:
            case SHPT_MULTIPOINTM:
                {
                    CMultiPointPtr pMultiPoint = new CMultiPoint;
                    for (long j = 0; j < pSHPObject->nVertices; j++)
                    {
                        WKSPointZ point;
                        point.x = pSHPObject->padfX[j];
                        point.y = pSHPObject->padfY[j];
                        point.z = pSHPObject->padfZ[j];
                        pMultiPoint->AddPoint(point);
                    }
                    pGeometry = (IGeometry*)(pMultiPoint._p());
                }
                break;

            case SHPT_ARC:
            case SHPT_ARCZ:
            case SHPT_ARCM:
                {
                    CPolylinePtr pPolyline = new CPolyline;
                    for (long part = 0; part < pSHPObject->nParts; part++)
                    {
                        long pointcount;
                        if (pSHPObject->nParts - 1 == part)
                        {
                            pointcount = pSHPObject->nVertices - pSHPObject->panPartStart[part];
                        }
                        else
                        {
                            pointcount = pSHPObject->panPartStart[part + 1] - pSHPObject->panPartStart[part];
                        }

                        CPathPtr pPath = new CPath;
                        for (int j = pSHPObject->panPartStart[part]; j < pSHPObject->panPartStart[part] + pointcount; j++)
                        {
                            WKSPointZ point;
                            point.x = pSHPObject->padfX[j];
                            point.y = pSHPObject->padfY[j];
                            point.z = pSHPObject->padfZ[j];
                            pPath->AddPoint(point);
                        }
                        pPolyline->AddPathRef(pPath);
                    }
                    pGeometry = (IGeometry*)pPolyline._p();
                }
                break;

            case SHPT_POLYGON:
            case SHPT_POLYGONM:
            case SHPT_MULTIPATCH:
                {
                    CPolygonPtr pPolygon = new CPolygon;
                    for (long part = 0; part < pSHPObject->nParts; part++)
                    {
                        long pointcount;
                        if (pSHPObject->nParts - 1 == part)
                        {
                            pointcount = pSHPObject->nVertices - pSHPObject->panPartStart[part];
                        }
                        else
                        {
                            pointcount = pSHPObject->panPartStart[part + 1] - pSHPObject->panPartStart[part];
                        }

                        CRingPtr pRing = new CRing;
                        for (int j = pSHPObject->panPartStart[part]; j < pSHPObject->panPartStart[part] + pointcount; j++)
                        {
                            WKSPointZ point;
                            point.x = pSHPObject->padfX[j];
                            point.y = pSHPObject->padfY[j];
                            point.z = pSHPObject->padfZ[j];
                            pRing->AddPoint(point);
                        }
                        pPolygon->AddRingRef(pRing);
                    }
                    pGeometry = (IGeometry*)pPolygon._p();
                }
                break;

            default:
                continue;
            }

            if (0 >= fieldcount)
            {
                attrib = "狐狸";
            }
            else
            {
                attrib = "";
            }

            for (long j = 0; j < fieldcount; j++)
            {
                switch(fieldtypearray[j])
                {
                case FTString:
                    {
                        string attribvalue = ::DBFReadStringAttribute(pDbf, i, j);
                        NeatenFeatureStringValue(attribvalue, attribvalue);
                        attrib = attrib + attribvalue + "\n";
                        break;
                    }
                case FTInteger:
                    attrib = attrib + IntToStr(::DBFReadIntegerAttribute(pDbf, i, j)) + "\n";
                    break;
                case FTDouble:
                    attrib = attrib + FloatToStr(::DBFReadDoubleAttribute(pDbf, i, j)) + "\n";
                    break;
                default:
                    attrib += "猪头\n";
                }
            }

            string annotation;
            if ((0 <= annofield) && (annofield < fieldcount))
            {
                //属性转注记
                switch(fieldtypearray[annofield])
                {
                case FTString:
                    annotation = ::DBFReadStringAttribute(pDbf, i, annofield);
                    break;
                case FTInteger:
                    annotation = IntToStr(::DBFReadIntegerAttribute(pDbf, i, annofield)) + "\n";
                    break;
                case FTDouble:
                    annotation = FloatToStr(::DBFReadDoubleAttribute(pDbf, i, annofield)) + "\n";
                    break;
                default:
                    annotation += "猪头\n";
                }
            }

            pSlimLayer->AddFeature(pGeometry, attrib, annotation);

            //干掉SHPReadObject()得到的对象
            SHPDestroyObject(pSHPObject);
        }
    }
    return entitiecount;
}

bool ImportShapeFile(const string& shapefile, const MapUnits mapunit,
    const double& basescale, const double& precision, const long indexlevel,
    const long annofield, const string& slimdir, CSlimLayerPtr& pSlimLayer)
{
    pSlimLayer.Clear();

    string noext = RemoveExtNamePart(shapefile);
    string shpname = noext + string(".shp");
    string dbfname = noext + string(".dbf");
    string prjname = noext + string(".prj");
    string slimname = RemoveDirectoryPart(noext);
    string slimfile = slimdir + "\\" + slimname;
    bool anno = false;
    if (0 <= annofield)
    {
        anno = true;
        string annotail = slimfile.substr(slimfile.size() - 2, slimfile.size());
        if (annotail != "_a")
        {
            slimfile = slimfile + "_a";
        }
    }
    slimfile = slimfile + ".esd";

    SHPHandle pShp = ::SHPOpen(shpname.c_str(), "rb");
    if (!pShp)
    {
        return false;
    }
    
    DBFHandle pDbf = ::DBFOpen(dbfname.c_str(), "rb");

    int shptype;
    double minbounds[4];
    double maxbounds[4];
    ::SHPGetInfo(pShp, NULL, &shptype, minbounds, maxbounds);

    WKSRect extent;
    extent.left = minbounds[0];
    extent.right = maxbounds[0];
    extent.top = maxbounds[1];
    extent.bottom = minbounds[1];

    ShapeType shapetype = ShpType2ShapeType(shptype);

    bool mustattach = false;

    if (slimdir == "")
    {
        //内存数据源
        CFieldsPtr pFieldsDummy;
        pSlimLayer = new CSlimLayer(shapetype, mapunit, basescale,
            precision, extent, indexlevel, pFieldsDummy, anno);
        pSlimLayer->SetName(RemoveDirectoryPart(noext).c_str());
        pSlimLayer->SetAlias(RemoveDirectoryPart(noext));
    }
    else
    {
        //文件数据源，先判断是否已存在
        pSlimLayer = new CSlimLayer(slimfile, false);
        if (pSlimLayer->Valid())
        {
            //要判断是否兼容，投篮
        }
        else
        {
            //不存在或不正确，新建
            CFieldsPtr pFieldsDummy;
            pSlimLayer = new CSlimLayer(shapetype, mapunit, basescale,
                precision, extent, indexlevel, pFieldsDummy, anno);
            pSlimLayer->SetName(RemoveDirectoryPart(noext).c_str());
            pSlimLayer->SetAlias(RemoveDirectoryPart(noext));
            mustattach = true;
        }
    }

    bool r = true;
    if (mustattach)
    {
        r = pSlimLayer->AttachToFile(slimfile);
    }

    if (r)
    {
        string attribinfo;
        int fieldcount = 0;
        if (pDbf) fieldcount = ::DBFGetFieldCount(pDbf);
        int fieldwidth, fielddecimals;
        char fieldname[20];

        for (long i = 0; i < fieldcount; i++)
        {
            DBFFieldType fieldtype = ::DBFGetFieldInfo(pDbf, i, fieldname,
                &fieldwidth, &fielddecimals);
            string sft;
            switch (fieldtype)
            {
                case FTInteger:
                    sft = "##i_";
                    break;

                case FTDouble:
                    sft = "##f_";
                    break;

                default:
                    sft = "##s_";
            }
            attribinfo = attribinfo + sft + fieldname + "\n";
        }

        pSlimLayer->SetFields(attribinfo);

        AddShapes2SlimLayer(pShp, pDbf, pSlimLayer, annofield);
        pSlimLayer->SaveData();

        string sr;
        //从prj文件中读取垃圾sr
        if (!File2String(prjname, sr))
        {
            sr = "Unknown coordinates system";
        }
        pSlimLayer->SetSR(sr);
    }

    ::SHPClose(pShp);
    if (pDbf) ::DBFClose(pDbf);

    return r;
}

const dword ALLPOINTCOUNT_BUFF_LEN = 10000;
const dword PARTCOUNT_BUFF_LEN = 300;
void Slim2SHPObject(const IGeometryPtr pGeo, SHPObject*& pSO)
{
    pSO = NULL;
    WKSPointZ pnt;
    dword i, j, partcount, partpnt, allpnt, allcount;
    double *px, *py, *pz;
    int *pPartStart;

    static double buf_x[ALLPOINTCOUNT_BUFF_LEN];
    static double buf_y[ALLPOINTCOUNT_BUFF_LEN];
    static double buf_z[ALLPOINTCOUNT_BUFF_LEN];
    static int buf_part[PARTCOUNT_BUFF_LEN];

    int nSHPType = GeoType2ShapeType(pGeo->GetGeometryType());

    switch (nSHPType)
    {
    case SHPT_POINTZ:
        {
            CPointPtr pPoint;
            CAST_PTR(pGeo, pPoint, CPoint)
            pPoint->GetCoordinates(pnt);
            pSO = SHPCreateSimpleObject(SHPT_POINTZ, 1, &pnt.x, &pnt.y, &pnt.z);//?
        }
        break;

    case SHPT_MULTIPOINTZ:
        {
            CMultiPointPtr pMP;
            CAST_PTR(pGeo, pMP, CMultiPoint)
            allcount = pMP->GetPointCount();
            if (ALLPOINTCOUNT_BUFF_LEN < allcount)
            {
                px = new double[allcount];
                py = new double[allcount];
                pz = new double[allcount];
            }
            else
            {
                px = buf_x;
                py = buf_y;
                pz = buf_z;
            }

            for (i = 0; i < allcount; i++)
            {
                pMP->GetPoint(pnt, i);
                px[i] = pnt.x;
                py[i] = pnt.y;
                pz[i] = pnt.z;
            }
            pSO = SHPCreateObject(SHPT_MULTIPOINTZ, -1, allcount, NULL, NULL,
                allcount, px, py, pz, NULL);

            if (ALLPOINTCOUNT_BUFF_LEN < allcount)
            {
                delete[] px;
                delete[] py;
                delete[] pz;
            }
        }
        break;

    case SHPT_ARCZ:
        {
            CPathPtr pPath;
            CPolylinePtr pPolyline;
            CAST_PTR(pGeo, pPolyline, CPolyline)
            partcount = pPolyline->GetPathCount();

            if (PARTCOUNT_BUFF_LEN < partcount)
            {
                pPartStart = new int[partcount];
            }
            else
            {
                pPartStart = buf_part;
            }

            j = allcount = 0;
            for (i = 0; i < partcount; i++)
            {
                pPartStart[j++] = allcount;
                pPolyline->GetPathRef(pPath, i);
                allcount += pPath->GetPointCount();
            }

            if (ALLPOINTCOUNT_BUFF_LEN < allcount)
            {
                px = new double[allcount];
                py = new double[allcount];
                pz = new double[allcount];
            }
            else
            {
                px = buf_x;
                py = buf_y;
                pz = buf_z;
            }

            allpnt = 0;
            for (i = 0; i < partcount; i++)
            {
                pPolyline->GetPathRef(pPath, i);
                partpnt = pPath->GetPointCount();
                for (j = 0; j < partpnt; j++)
                {
                    pPath->GetPoint1(j, pnt);
                    px[allpnt] = pnt.x;
                    py[allpnt] = pnt.y;
                    pz[allpnt++] = pnt.z;
                }
            }

            pSO = SHPCreateObject(SHPT_ARCZ, -1, partcount, pPartStart, NULL,
                allpnt, px, py, pz, NULL);

            if (ALLPOINTCOUNT_BUFF_LEN < allcount)
            {
                delete[] px;
                delete[] py;
                delete[] pz;
            }

            if (PARTCOUNT_BUFF_LEN < partcount)
            {
                delete pPartStart;
            }

        }
        break;

    case SHPT_MULTIPATCH:
        {
            CRingPtr pRing;
            CPolygonPtr pPolygon;
            CAST_PTR(pGeo, pPolygon, CPolygon)
            partcount = pPolygon->GetRingCount();

            if (PARTCOUNT_BUFF_LEN < partcount)
            {
                pPartStart = new int[partcount];
            }
            else
            {
                pPartStart = buf_part;
            }

            j = allcount = 0;
            for (i = 0; i < partcount; i++)
            {
                pPartStart[j++] = allcount;
                pPolygon->GetRingRef(pRing, i);
                allcount += pRing->GetPointCount();
            }

            if (ALLPOINTCOUNT_BUFF_LEN < allcount)
            {
                px = new double[allcount];
                py = new double[allcount];
                pz = new double[allcount];
            }
            else
            {
                px = buf_x;
                py = buf_y;
                pz = buf_z;
            }

            allpnt = 0;
            for (i = 0; i < partcount; i++)
            {
                pPolygon->GetRingRef(pRing, i);
                partpnt = pRing->GetPointCount();
                for (j = 0; j < partpnt; j++)
                {
                    pRing->GetPoint1(j, pnt);
                    px[allpnt] = pnt.x;
                    py[allpnt] = pnt.y;
                    pz[allpnt++] = pnt.z;
                }
            }

            pSO = SHPCreateObject(SHPT_MULTIPATCH, -1, partcount, pPartStart, NULL,
                allpnt, px, py, pz, NULL);

            if (ALLPOINTCOUNT_BUFF_LEN < allcount)
            {
                delete[] px;
                delete[] py;
                delete[] pz;
            }

            if (PARTCOUNT_BUFF_LEN < partcount)
            {
                delete pPartStart;
            }
        }
        break;

    default:
        {
            return;
        }
    }

    SHPComputeExtents(pSO);
}

void Slim2Shape(const CSlimLayerPtr pSL, const SHPHandle pSH, const DBFHandle pDH)
{
    vector<dword> fids;
    pSL->GetFids(fids);

    Strings strings;
    int fieldcount = 0;
    if (_valid(pDH))
    {
        fieldcount = DBFGetFieldCount(pDH);
    }

    vector<DBFFieldType> fieldtypes;
    long i;
    for (i = 0; i < fieldcount; i++)
    {
        int fieldwidth, fielddecimals;
        char fieldname[50];
        DBFFieldType fieldtype = ::DBFGetFieldInfo(pDH, i, fieldname, &fieldwidth, &fielddecimals);
        fieldtypes.push_back(fieldtype);
    }

    vector<dword>::const_iterator it = fids.begin();
    while (it != fids.end())
    {
        dword fid = *(it++);
        IGeometryPtr pGeo;
        string sa, anno;
        pSL->GetFeature(fid, pGeo, sa, anno);
        SHPObject* pSO = NULL;
        Slim2SHPObject(pGeo, pSO);
        if (_invalid(pSO))
        {
            continue;
        }

        int shapeid = SHPWriteObject(pSH, -1, pSO);
        SHPDestroyObject(pSO);

        //还要写入dbf
        if (_valid(pDH))
        {
            strings.SetText(sa);
            for (i = 0; i < fieldcount; i++)
            {
                string fv;
                strings.GetLine(i, fv);
                switch (fieldtypes[i])
                {
                case FTInteger:
                    DBFWriteIntegerAttribute(pDH, shapeid, i, easymap::StrToInt(fv));
                    break;
                case FTDouble:
                    DBFWriteDoubleAttribute(pDH, shapeid, i, easymap::StrToFloat(fv));
                    break;
                default:
                    {
                        RestoreFeatureStringValue(fv, fv);
                        DBFWriteStringAttribute(pDH, shapeid, i, fv.c_str());
                    }
                }
            }
        }
    }
}

bool ExportShapeFile(const CSlimLayerPtr pSL, const string& fn)
{
    string shpname = RemoveExtNamePart(fn) + ".shp";
    string dbfname = RemoveExtNamePart(fn) + ".dbf";
    string prjname = RemoveExtNamePart(fn) + ".prj";

    GeometryColumnInfo colinfo;
    pSL->GetGeometryColumnInfo(colinfo);
    int shptype = ShapeType2ShpType(colinfo.ShpType);
    if (SHPT_NULL == shptype) {return false;}

    SHPHandle pSH = SHPCreate(shpname.c_str(), shptype);
    if (_invalid(pSH)) {return false;}
       
    DBFHandle pDH = NULL;
    string sa;
    pSL->GetFields(sa);
    sa = Trim(sa);
    if (sa.size() > 0)
    {
        pDH = DBFCreate(dbfname.c_str());
    }

    if (_valid(pDH))
    {
        //增加属性字段
        Strings strings;
        strings.SetText(sa);
        for (long i = 0; i < (long)strings.GetLineCount(); i++)
        {
            int nDecimals = 0;
            string fieldname;
            strings.GetLine(i, fieldname);
            string s1 = fieldname.substr(0, 4);
            DBFFieldType fieldtype = FTString;
            if (s1 == "##i_")
            {
                fieldtype = FTInteger;
            }
            else if (s1 == "##f_")
            {
                fieldtype = FTDouble;
                nDecimals = 5;
            }

            //去掉前缀
            if ((s1 == "##i_") || (s1 == "##f_") || (s1 == "##s_"))
            {
                fieldname = fieldname.substr(4, fieldname.size() - 2);
            }

            if ( 0 > DBFAddField(pDH, fieldname.c_str(), fieldtype, 10, nDecimals))
            {
                //有问题
                DBFClose(pDH);
                pDH = NULL;
                break;
            }
        }
    }

    Slim2Shape(pSL, pSH, pDH);

    SHPClose(pSH);
    if (_valid(pDH))
    {
        DBFClose(pDH);
    }

    string sr = Trim(pSL->GetSpatialReference());
    if (sr.size() > 0)
    {
        //写入prj文件
        String2File(sr, prjname.c_str());
    }

    return true;
}

}