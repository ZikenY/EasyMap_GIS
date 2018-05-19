#ifndef __FROM_E00_
#define __FROM_E00_

#include <afxwin.h>         // MFC core and standard components
#include <afxtempl.h>

#define ERR_SUCCESS                    1
#define ERR_NO_ERROR                -1
#define ERR_UNEXPECTED                -2

#define ERR_FILE_NAME            -1000
#define ERR_FILE_NOT_EXIST        -1001
#define ERR_FILE_OPEN            -1002
#define ERR_FILE_FORMAT            -1003
#define ERR_FILE_READ            -1004  
#define ERR_FILE_WRITE            -1005   
#define ERR_READ                -1006      
#define ERR_WRITE                -1007         
#define ERR_NO_FILE                -1008   
#define ERR_NO_DATA                -1009      
#define ERR_TEMP_FILE_WRITE        -1010       

typedef    CArray<long,long>    CLongArray;
typedef struct tagENVELOPE
{    
public:
        double  minX;
        double  minY;
        double  maxX;
         double  maxY;
}ENVELOPE;

typedef struct tagE00Arc{
    long Oid;
    long Uid;
    long fNode;
    long tNode;
    long lPolygon;
    long rPolygon;
    long PointNums;
    double* CoordData;
}E00Arc;

typedef CMap<long,long, E00Arc *, E00Arc *> CMapOidToLine;

typedef struct tagE00LabPoint{
    long Oid;
    long Uid;
    double x;
    double y;
}E00LabPoint;

typedef CMap<long,long, E00LabPoint *, E00LabPoint *> CMapOidToLabPoint;

typedef struct tagE00Polygon{
    long Oid;
    long ArcNums;
    ENVELOPE env;
    CLongArray ArcOidArray;
}E00Polygon;

typedef CMap<long,long, E00Polygon *, E00Polygon *> CMapOidToPolygon;

typedef struct tagE00Region{
    CString RegionName;
    long Oid;
    CLongArray PolygonOidArray;
}E00Region;
typedef CMap<long,long, E00Region *, E00Region *> CMapOidToRegion;

typedef CMap< CString, LPCSTR, CMapOidToRegion*, CMapOidToRegion*> CMapNameToRegionMap;

typedef CMap< CString, LPCSTR, CMapOidToPolygon*, CMapOidToPolygon*> CMapNameToRegionPolygonMap;

typedef struct tagE00Text{
    CString LayerName;
    long Oid;
    long PointNums;
    long length;
    double size;
    double x0;
    double y0;
    double * CoordData;
    CString text;
}E00Text;

typedef CMap<long,long, E00Text *, E00Text *> CMapOidToText;

typedef struct tagE00SpatialReference{
    long Scale;
    CString ProjectionName;
    CString Zunits;
    CString XYUnits;
    CString SpheroidName;
    double Xshift;
    double Yshift;
    struct tagParameters{
        double factor;
        double longitute;
        double latitute;
        double FalseEast;
        double FalseNorth;
    }Parameters;
}E00SpatialReference;

typedef struct tagE00Field{
    CString name;
    int type;
    int storeWidth;
    int outputWidth;
    int decimal;
}E00Field;

typedef CArray< E00Field*, E00Field*> E00Fields;
typedef CMap<long,long,CStringArray*, CStringArray*> IDToStringArrayMap;

typedef struct tagE00Table{
    CString TableName;
    E00Fields Fields;
    IDToStringArrayMap RecordArrayMap;
}E00Table;

typedef CMap< CString,LPCSTR,E00Table*, E00Table*> E00Info;

class CFromE00
{
public:
    virtual ~CFromE00();
    CFromE00();
private:

    CStringArray m_FileNameArray;
    CString m_strCurrentFileName;
public:

    CMapOidToLabPoint*            m_pPointMap;               
    CMapOidToLine*                m_pLineMap;                
    CMapOidToPolygon*            m_pPolygonMap;              
    CMapNameToRegionMap*        m_pRegionMap;                
    CMapNameToRegionPolygonMap* m_pNameToRegionPolygonMap;   
    CMapOidToPolygon*            m_pRouteMap;                
    CMapOidToText*                m_pTextMap;                

    E00Info *                    m_pE00Info;

    E00SpatialReference m_E00SpatialReference;

    BOOL m_bNode;
    CMapOidToLabPoint*            m_pNodePointMap;             
private:
    BOOL m_bAatAttrByUid;
public:

    BOOL BeginReadData();

    BOOL FreeidMap();

    long GetObjectCount();
private:

    BOOL ReadAndWrite(CString filename);
    BOOL transArc( FILE *fp, int precID = 2 );
    BOOL transLab( FILE *fp, int precID = 2 );
    BOOL transPal( FILE *fp, int precID = 2 );
    BOOL transTx7( FILE *fp, int precID = 2 );
    BOOL transRXP( FILE *fp, int precID = 2 );
    BOOL transRPL( FILE *fp, int precID = 2 );
    BOOL transInfo( FILE *fp, int precID = 2 );
    void ReMap();

private:
    int m_nErrorID;

public:
    BOOL ClearFileNames();
    BOOL AddFileName(CString filename);

    long GetPointCount();
    long GetLineCount();
    long GetPolygonCount();
    long GetTextCount();
};

#endif