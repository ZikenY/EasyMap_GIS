#include "FromE00.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


CFromE00 :: ~CFromE00()
{
    FreeidMap();
    m_FileNameArray.RemoveAll();
    m_strCurrentFileName.Empty();

    m_nErrorID = ERR_NO_ERROR;
}

CFromE00 :: CFromE00()
{
    m_FileNameArray.RemoveAll();
    m_strCurrentFileName.Empty();
    m_pPointMap = NULL;           
    m_pLineMap = NULL;                  
    m_pPolygonMap = NULL;               
    m_pRegionMap = NULL;                
    m_pNameToRegionPolygonMap = NULL;   
    m_pRouteMap = NULL;                 
    m_pTextMap = NULL;                  
    m_pE00Info = NULL;                  

    m_bAatAttrByUid = FALSE;

    m_bNode = FALSE;
    m_pNodePointMap = NULL;             

}


BOOL CFromE00 :: BeginReadData()
{
    int n = m_FileNameArray.GetSize();
    if(n == 0){
        m_nErrorID = ERR_NO_FILE;
        return FALSE;
    }
    for(int i = 0;i < n;i++)
    {
        FreeidMap();
        CString filename = m_FileNameArray.GetAt(i);
        char drive[5], dir[80], fname[20], ext[5];
        _splitpath( filename, drive, dir, fname, ext );
        m_strCurrentFileName = (CString)fname;
        m_strCurrentFileName.MakeUpper();
        if(!ReadAndWrite(filename))
        {
            return FALSE;
        }
    }

    return TRUE;
}

BOOL CFromE00 :: ReadAndWrite(CString filename)
{
    char strFileName[256];
    memset(strFileName,0,sizeof(strFileName));
    sprintf(strFileName,"%s",filename);
    FILE *fp = fopen( strFileName, "rb" );
    if ( fp == NULL ){
        m_nErrorID = ERR_FILE_OPEN;
        return FALSE;
    }
    char buffer[128];
    fgets( buffer, sizeof(buffer), fp );
    if ( strncmp( "EXP", buffer, 3 ) )
    {
        fclose(fp);
        m_nErrorID = ERR_FILE_FORMAT;
        return FALSE;
    }

    int compressType;
    sscanf( buffer + 3, "%d", &compressType );

    switch ( compressType )
    {
    case 0:
        {
            do
            {
                fgets( buffer, sizeof(buffer), fp );
                CString str = buffer;
                str.TrimLeft();
                str.TrimRight();
                int flag = atoi(str.Right(1));
                if(flag != 2 && flag != 3) continue;
                if ( 0 == strncmp( "ARC", buffer, 3 ) ||
                    0 == strncmp( "ARC", buffer, 3 ))
                {
                    if(!transArc( fp, atoi(buffer + 3))){
                        fclose(fp);
                        return FALSE;
                    }
                }
                if ( 0 == strncmp( "PAL", buffer, 3 ) ||
                      0 == strncmp( "PAL", buffer, 3 ))
                {
                    if(!transPal( fp, atoi(buffer + 3)))
                    {
                        fclose(fp);
                        return FALSE;
                    }
                }
                if ( 0 == strncmp( "LAB", buffer, 3 ) ||
                     0 == strncmp( "LAB", buffer, 3 ))
                {
                    if(!transLab( fp, atoi(buffer + 3))){
                        fclose(fp);
                        return FALSE;
                    }
                }
                if ( 0 == strncmp( "TX6", buffer, 3 ) || 
                     0 == strncmp( "TX7", buffer, 3 ) ||
                     0 == strncmp( "TX6", buffer, 3 ) ||
                     0 == strncmp( "TX7", buffer, 3 ))
                {
                    if(!transTx7(fp, atoi(buffer + 3) )){
                        fclose(fp);
                        return FALSE;
                    }
                }
                if ( 0 == strncmp( "RXP", buffer, 3 ) ||
                     0 == strncmp( "RXP", buffer, 3 ))
                {
                    if(!transRXP( fp, atoi(buffer + 3)))
                    {
                        fclose(fp);
                        return FALSE;
                    }
                }
                if ( 0 == strncmp( "RPL", buffer, 3 ) ||
                     0 == strncmp( "RPL", buffer, 3 ))
                {
                    if(!transRPL( fp, atoi(buffer + 3))){
                        fclose(fp);
                        return FALSE;
                    }
                }
                if ( 0 == strncmp( "IFO", buffer, 3 )|| 
                    0 == strncmp( "IFO", buffer, 3 ))
                {
                    m_bAatAttrByUid = FALSE;
                    if(!transInfo( fp, atoi(buffer + 3)))
                    {
                        fclose(fp);
                        return FALSE;
                    }
                }
            }
            while ( strncmp( "EOS", buffer, 3 ) && !feof(fp));
            break;
        }
    case 1:
        break;
    case 2:
        break;
    default:
        break;
    }
    fclose(fp);
    ReMap();
    return TRUE;
}
BOOL CFromE00 :: transTx7( FILE *fp, int precID )
{
    long TextCount = 0;
    long id[8];
    char buffer[100];
    char seps[]   = " \t\n";
    char *token;
    memset(buffer,'\0',sizeof(buffer));
    fgets(buffer,sizeof(buffer),fp);
    while(strnicmp(buffer,"JABBERWOCKY",11) != 0)
    {
        fgets(buffer,sizeof(buffer),fp);
        token = strtok( buffer, seps );
        if(NULL == token) return FALSE;
        int i = 0;
        while( token != NULL )
        {
            id[i] = atol(token);
            token = strtok( NULL, seps );
            ++i;
         }
        ASSERT(i <= 8);
        
        while ( -1 != id[0] && !feof(fp))
        {
            TextCount ++;
            short AlignmentStyle = 10;
            for ( i = 0; i < 40; ++i )
            {
                short t;
                fscanf( fp, "%d", &t );
                if( i == 0)
                    AlignmentStyle = t;
            }
            float gip;
            fscanf( fp, "%f", &gip );
            double size,size1,size2;
            fscanf( fp, "%lf%lf%lf\n", &size, &size1, &size2 );
            E00Text* pText = new E00Text;
            pText->PointNums = id[2];
            pText->Oid = TextCount;
            pText->size = size;
            pText->x0 = size1;
            pText->y0 = size2;
            pText->CoordData = new double[id[2] * 2];
            for ( i = 0; i < id[2] ; ++i ){
                double x,y;
                fscanf( fp, "%lf%lf\n", &x,&y);
                pText->CoordData[2 * i] = x;
                pText->CoordData[2 * i+1] = y;

            }

            char *text = new char[id[6] + 1];
            char buf[80];
            memset(buf,'\0',sizeof(buf));
            fgets(buf,sizeof(buf),fp);
            strncpy(text,buf, id[6]);
            text[id[6]] = '\0';
            
            
            if(strlen(text) == 0)
            {
                delete []text;
                text = NULL;
                delete []pText->CoordData;
                pText->CoordData = NULL;
                delete pText;
                pText = NULL;
            }
            else
            {
                pText->text = text;
                if(NULL != m_pTextMap)
                    m_pTextMap->SetAt(TextCount,pText);
                else
                {
                    m_pTextMap = new CMapOidToText;
                    m_pTextMap->SetAt(TextCount,pText);
                }
                delete text;
                text = NULL;
            }
            memset(buffer,'\0',sizeof(buffer));
            do
            {
                fgets(buffer,sizeof(buffer),fp);
            }while(buffer[0] == '\x00');
            token = strtok( buffer, seps );
            if(NULL == token) return FALSE;
            int ik = 0;
            while( token != NULL )
            {
                id[ik] = atol(token);
                token = strtok( NULL, seps );
                ++ik;
             }
            ASSERT(ik <= 8);
            
        }
        fgets(buffer,sizeof(buffer),fp);
    }
    return TRUE;
}

BOOL CFromE00 :: transInfo( FILE *fp, int precID )
{
    m_pE00Info = new E00Info;
    char buffer[128];
    memset( buffer, 0, sizeof(buffer) );
    fgets( buffer, sizeof(buffer), fp );
    while ( strncmp( "EOI", buffer, 3 ) && !feof(fp) )
    {
        E00Table* pTable = new E00Table;
        char title[32];
        memset( title, 0, sizeof(title) );
        strncpy( title, buffer, 32 );
        CString strTableName;
        strTableName = strtok(title," ");
        strTableName.TrimLeft();
        strTableName.TrimRight();
        strTableName.MakeUpper();
        
        pTable->TableName = strTableName;
        CString LeftStr,RightStr;
        CString strType = strTableName;
        int n = strType.Find('.');
        if(-1 == n){
            delete pTable; pTable = NULL;
            fgets( buffer, sizeof(buffer), fp );
        }
        CString strPrefix = strType.Left(n);
        strType = strType.Right(strType.GetLength() - n - 1);
        LeftStr = strType.Left(3);
        RightStr = strType.Right(strType.GetLength() - 3);

        int nLocation_ID = -1;
        int nLocation_UID = -1;
        CString strIDFieldName = "";
        CString strUIDFieldName = "";
        if(RightStr.IsEmpty())
        {
            strIDFieldName = strPrefix + "#";
            strUIDFieldName = strPrefix + "-ID";
        }
        else
        {
            strIDFieldName = RightStr + "#";
            strUIDFieldName = RightStr + "-ID";
        }
        
        int i = 0;
        int numField = atoi( strtok( buffer + 34, " " ) );
        int secnumField =  atoi(strtok( NULL, " " ));
        strtok( NULL, " " );
        long numObj = atoi( strtok( NULL, " " ) );
        E00Fields fieldArray;
        fieldArray.RemoveAll();
        E00Fields TempFieldArray;
        TempFieldArray.RemoveAll();
        for (i = 0; i < numField; i++ )
        {
            fgets( buffer, sizeof(buffer), fp );
            char FName[17];
            strncpy( FName, buffer, 16 );
            FName[16] = '\0';
            CString FieldName(FName);
            FieldName.TrimLeft();
            FieldName.TrimRight();
            char buffer1[5];
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 16, 3 );
            int len = atoi(buffer1);
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 19, 2 );
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 21, 4 );
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 25, 1 );
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 26, 2 );
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 28, 4 );
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 32, 2 );
            int dec = atoi(buffer1);
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 34, 2 );
            int type = atoi(buffer1);
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 36, 1 );
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 37, 2 );
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 39, 4 );
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 43, 4 );
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 47, 2 );
            memset( buffer1, 0, sizeof( buffer1 ) );
            strncpy( buffer1, buffer + 65, 4 );
            E00Field* pTempField = new E00Field;
            pTempField->decimal = dec;
            pTempField->name = FieldName;
            pTempField->storeWidth = len;
            pTempField->type = type;
            TempFieldArray.Add(pTempField);
            
            if(FieldName.CompareNoCase(strIDFieldName) == 0)
            {
                nLocation_ID = i;
            }
            if(FieldName.CompareNoCase(strUIDFieldName) == 0)
            {
                nLocation_UID = i;
            }

            E00Field* pField = new E00Field;
            pField->decimal = dec;
            pField->name = FieldName;
            pField->storeWidth = len;
            pField->type = type;
            fieldArray.Add(pField);

            
        }

        pTable->Fields.InsertAt(0,&fieldArray);

        for(int se = 0; se < (secnumField - numField); se++)
        {
            fgets( buffer, sizeof(buffer), fp );
        }
            
        int sum = 0;
        for( i = 0;i < numField; ++i )
        {
            int FieldType = TempFieldArray.GetAt(i)->type;
            int FieldLength = TempFieldArray.GetAt(i)->storeWidth;
            switch ( FieldType )
            {
            case 1:sum += 8;
                break;
            case 2:sum += FieldLength;
                break;
            case 3:sum += FieldLength;
                break;
            case 4:sum += 14;
                break;
            case 5:sum += 11;
                break;
            case 6:
                {
                    if ( 8 == FieldLength )
                        sum += 24;
                    else
                        sum += 14;
                    break;
                }
            }
        }
        int row = sum / 80, vol = sum - row * 80;

        for ( int j = 0; j < numObj; ++j )
        {
            CString temp;
            for( i = 0; i < row; ++i)
            {
                fgets(buffer,sizeof(buffer),fp);
                
                CString string = buffer;
                string.TrimRight();
                int vn = string.GetLength();
                if(vn < 80)
                {
                    for(int h = 0; h < 80 - vn;h++)
                        string += (_T(" "));
                }
                if(vn > 80){
                    string = string.Left(79);
                    string += "-";
                }
                temp += string;
                
            }
            if(vol != 0)
            {
                fgets(buffer,sizeof(buffer),fp);
                CString aString = buffer;
                aString.TrimRight();
                int bn = aString.GetLength();
                if(bn < vol)
                {
                    for(int h = 0; h < vol - bn;h++)
                        aString += (_T(" "));
                }
                temp += aString;
            }

            char s[1024];
            CString result;
            int num = 0;
            CStringArray * pstrArray = new CStringArray;
            for ( i = 0; i < numField; ++i )
            {
                CString strFieldName  = TempFieldArray.GetAt(i)->name;
                int FieldType = TempFieldArray.GetAt(i)->type;
                int FieldLength = TempFieldArray.GetAt(i)->storeWidth;
                switch ( FieldType )
                {
                    case 1:
                    {
                        memset( s, 0, sizeof(s) );
                        for ( int k = 0; k < 8; ++k )
                            s[k] = temp[num + k];
                        num += 8;

                        result = s;
                        result.TrimLeft();
                        result.TrimRight();
                        break;
                    }
                case 2:
                    {
                        int nHanzi = 0;
                        memset( s, 0, sizeof(s) );
                        for ( int k = 0; k < FieldLength; ++k ){
                            s[k] = temp[num + k];
                            if(s[k] < 0){
                                nHanzi ++;
                            }
                        }
                        num += FieldLength;
                        result = s;
                        result.TrimLeft();
                        result.TrimRight();
                        int nb = result.GetLength();
                        if(nb == FieldLength && result.GetAt(nb -1) < 0 && nHanzi % 2 == 1){
                            result = result.Left(nb-1);
                            result += "-";
                        }
                        char *ss = new char[result.GetLength() * 2 + 1];
                        int jidx = 0;
                        for ( int i = 0; i < result.GetLength(); ++i,++jidx )
                        {
                            if ( result[i] == '\'' )
                            {
                                ss[jidx] = '\'';
                                ++jidx;
                            }
                            ss[jidx] = result[i];
                        }
                        ss[jidx] = 0;
                        result = ss;
                        delete [] ss;
                        break;
                    }
                case 3:
                    {
                        memset( s, 0, sizeof(s) );
                        for ( int k = 0; k < FieldLength; ++k )
                            s[k] = temp[num + k];
                        num += FieldLength;

                        char format[16];
                        sprintf( format, "%%%dld", FieldLength );
                        
                        long l;
                        sscanf( s, format, &l );
                        memset( s, 0, sizeof(s) );
                        sprintf( s, format, l );
                        result = s;
                        break;
                    }
                case 4:
                    {
                        memset( s, 0, sizeof(s) );
                        for ( int k = 0; k < 14; ++k )
                            s[k] = temp[num + k];
                        num += 14;
                        result = s;
                        double dEle;
                        if(result.Find('E') == -1){
                            dEle = atof(result);
                            dEle = dEle / 10;
                            result.Format("%lf",dEle);
                        }
                        break;
                    }
                case 5:
                    {
                        memset( s, 0, sizeof(s) );
                        for ( int k = 0; k < 11; ++k )
                            s[k] = temp[num + k];
                        num += 11;
                        result = s;
                        break;
                    }
                case 6:
                    {
                        memset( s, 0, sizeof(s) );
                        if ( 8 == FieldLength )
                        {
                            for ( int k = 0; k < 24; ++k )
                                s[k] = temp[num + k];
                            num += 24;

                        }
                        else
                        {
                            for ( int k = 0; k < 14; ++k )
                                s[k] = temp[num + k];
                            num += 14;
                        }
                        result = s;
                        break;
                    }
                }
                pstrArray->Add(result);
            }
            if(nLocation_ID != -1 && nLocation_UID != -1)
            {
                CStringArray * pStrA = NULL;
                char strVal[20];
                char strUIDVal[20];
                sprintf(strVal,"%s",pstrArray->GetAt(nLocation_ID));
                sprintf(strUIDVal,"%s",pstrArray->GetAt(nLocation_UID));
                long nValue = atol(strVal);
                long nUIDValue = atol(strUIDVal);
                if(nValue <= 0)
                {
                    if(nUIDValue > 0)
                        nValue = nUIDValue;

                    if(LeftStr == "AAT" && RightStr.IsEmpty())
                    {
                        m_bAatAttrByUid = TRUE;
                    }
                }

                if(pTable->RecordArrayMap.Lookup(nValue,pStrA))
                {
                }
                pTable->RecordArrayMap.SetAt(nValue,pstrArray);
            }
            else if(LeftStr.CompareNoCase("TRN") == 0 && RightStr.IsEmpty())
            {
                pTable->RecordArrayMap.SetAt(j,pstrArray);
            }
            else
            {
                pstrArray->RemoveAll();
                delete pstrArray ;
                pstrArray = NULL;
            }
        }

        m_pE00Info->SetAt(strTableName,pTable);

        fgets( buffer, sizeof(buffer), fp );
        while ( buffer[0] == 10 || 13 == buffer[0])
        {
            fgets( buffer, sizeof(buffer), fp );
        }

        for(i = 0; i < TempFieldArray.GetSize();i++){
            E00Field* pE00Field = TempFieldArray.GetAt(i);
            if(pE00Field){
                delete pE00Field;
                pE00Field = NULL;
            }
        }
        TempFieldArray.RemoveAll();

    }
    return TRUE;
}
BOOL CFromE00 :: transLab( FILE *fp, int precID )
{
    long id = -1;
    fscanf( fp, "%ld", &id );
    while ( -1 != id && !feof(fp))
    {
        long id1 = 0;
        fscanf( fp, "%ld", &id1 );
        for ( int i = 0; i < 3; ++i )
        {
            double x,y;
            fscanf( fp, "%lf%lf", &x, &y );
            if(-1 == id && 0 == id1 && x == 0.0 && y == 0.0)
            {
                return TRUE;
            }
            if(0 == i)
            {
                E00LabPoint* pp = new E00LabPoint;
                pp->Oid = id;
                pp->Uid = id1;
                pp->x = x;pp->y = y;
                if(m_pPointMap != NULL)
                {
                    E00LabPoint* pTempPoint = NULL;
                    if(m_pPointMap->Lookup(id,pTempPoint))
                    {
                        delete pp; pp = NULL;
                    }
                    else
                        m_pPointMap->SetAt(id,pp);
                }
                else
                {
                    m_pPointMap = new CMapOidToLabPoint;
                    m_pPointMap->SetAt(id,pp);
                }
            }
        }
        fscanf( fp, "%d", &id );
    }
    return TRUE;
}

BOOL CFromE00::transRXP(FILE *fp, int precID)
{
    char buffer[81];
    fgets( buffer, sizeof(buffer), fp );
    strtok( buffer, "\n" );
    strtok( buffer, "\r" );
    m_pRegionMap = new CMapNameToRegionMap;
    while ( 0 != strncmp( "JABBERWOCKY", buffer, 11 ) && !feof(fp))
    {
        CString rName = "";
        rName = buffer;
        rName.TrimRight();
        rName.TrimRight();
        
        long lRegionId,lPolygonId;
        fscanf( fp, "%ld%ld\n", &lRegionId,&lPolygonId );
        CMapOidToRegion * ptempMapRegion = NULL;
        ptempMapRegion = new CMapOidToRegion;
        while ( -1 != lRegionId && !feof(fp))
        {
            E00Region* pRegion = NULL;
            if(ptempMapRegion->Lookup(lRegionId,pRegion))
            {
                pRegion->PolygonOidArray.Add(lPolygonId);
            }
            else
            {
                pRegion = new E00Region;
                pRegion->Oid = lRegionId;
                pRegion->RegionName = rName;
                pRegion->PolygonOidArray.Add(lPolygonId);
                ptempMapRegion->SetAt(lRegionId,pRegion);
            }
            fscanf( fp, "%ld%ld\n", &lRegionId,&lPolygonId );
        }
        m_pRegionMap->SetAt(rName,ptempMapRegion);
        fgets( buffer, sizeof(buffer), fp );
        strtok( buffer, "\n" );
        strtok( buffer, "\r" );
    }
    return TRUE;
}

BOOL CFromE00::transRPL(FILE *fp, int precID)
{
    char buffer[81];
    fgets( buffer, sizeof(buffer), fp );
    strtok( buffer, "\n" );
    strtok( buffer, "\r" );
    m_pNameToRegionPolygonMap = new CMapNameToRegionPolygonMap;
    while ( 0 != strncmp( "JABBERWOCKY", buffer, 11 ) && !feof(fp))
    {
        CString rName = "";
        rName = buffer;
        rName.TrimRight();
        rName.TrimRight();
        long index = 0;
        long numArc;
        CMapOidToPolygon * tempMapPolygonToid = NULL;
        tempMapPolygonToid = new CMapOidToPolygon;
        fscanf( fp, "%ld", &numArc );
        while ( -1 != numArc && !feof(fp))
        {
            double minx,miny,maxx,maxy;
            fscanf( fp, "%lf%lf", &minx, &miny );
            fscanf( fp, "%lf%lf", &maxx, &maxy );
            CLongArray oidArray;
            oidArray.RemoveAll();
            if(0 == numArc) numArc = 1;
            for ( int i = 0; i < numArc; ++i )
            {
                long id1, id2, id3;
                fscanf( fp, "%ld%ld%ld", &id1, &id2, &id3 );
                oidArray.Add(id1);
            }
            index++;
            E00Polygon* pPoly = new E00Polygon;
            pPoly->Oid = index;
            pPoly->ArcNums = numArc;
            pPoly->env.minX = minx;
            pPoly->env.minY = miny;
            pPoly->env.maxX = maxx;
            pPoly->env.maxY = maxy;
            pPoly->ArcOidArray.InsertAt(0,&oidArray);

            tempMapPolygonToid->SetAt(index,pPoly);
                
            fscanf( fp, "%d", &numArc );
        }
        m_pNameToRegionPolygonMap->SetAt(rName,tempMapPolygonToid);
        
        fgets( buffer, sizeof(buffer), fp );
        if(precID == 3)
            fgets( buffer, sizeof(buffer), fp );

        fgets( buffer, sizeof(buffer), fp );
        strtok( buffer, "\n" );
        strtok( buffer, "\r" );
    }
    return TRUE;
}

BOOL CFromE00::FreeidMap()
{
    if(NULL != m_pPointMap)
    {
        E00LabPoint* pp = NULL;
        long tmpid;
        POSITION pos = m_pPointMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pPointMap->GetNextAssoc(pos,tmpid,pp);
            if(pp != NULL)
            {
                delete pp;pp = NULL;
            }
        }
        m_pPointMap->RemoveAll();
        delete m_pPointMap;
        m_pPointMap = NULL;
    }
    if(NULL != m_pLineMap)
    {
        E00Arc* pArc = NULL;
        long tmpid;
        POSITION pos = m_pLineMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pLineMap->GetNextAssoc(pos,tmpid,pArc);
            if((pArc != NULL) && (AfxIsValidAddress(pArc,sizeof(E00Arc))))
            {
                if(NULL != pArc->CoordData)
                {
                    delete [] pArc->CoordData;
                    pArc->CoordData = NULL;
                }
                delete pArc;pArc = NULL;
            }
        }
        m_pLineMap->RemoveAll();
        delete m_pLineMap;
        m_pLineMap = NULL;
    }
    if(NULL != m_pPolygonMap)
    {
        E00Polygon * pPoly = NULL;
        long tmpid;
        POSITION pos = m_pPolygonMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pPolygonMap->GetNextAssoc(pos,tmpid,pPoly);
            if((pPoly != NULL) && (AfxIsValidAddress(pPoly,sizeof(E00Polygon))))
            {
                pPoly->ArcOidArray.RemoveAll();
                delete pPoly;pPoly = NULL;
            }
        }
        m_pPolygonMap->RemoveAll();
        delete m_pPolygonMap;
        m_pPolygonMap = NULL;
    }
    if(NULL != m_pRegionMap)
    {
        CMapOidToRegion* pMapOidToRegion = NULL;
        CString strRegionName = "";
        POSITION position = m_pRegionMap->GetStartPosition();
        while(NULL != position)
        {
            m_pRegionMap->GetNextAssoc(position,strRegionName,pMapOidToRegion);
            if(NULL != pMapOidToRegion)
            {
                E00Region * pRegion = NULL;
                long tmpid;
                POSITION tmppos = pMapOidToRegion->GetStartPosition();
                while(NULL != tmppos)
                {
                    pMapOidToRegion->GetNextAssoc(tmppos,tmpid,pRegion);

                    if(NULL != pRegion)
                    {
                        pRegion->PolygonOidArray.RemoveAll();
                        delete pRegion;pRegion = NULL;
                    }
                }
                pMapOidToRegion->RemoveAll();
                delete pMapOidToRegion; pMapOidToRegion = NULL;
            }
        }
        m_pRegionMap->RemoveAll();
        delete m_pRegionMap;
        m_pRegionMap = NULL;
    }
    if(NULL != m_pNameToRegionPolygonMap)
    {
        CMapOidToPolygon * pPolygonMap = NULL;
        CString strRegionName = "";
        POSITION firstPos = m_pNameToRegionPolygonMap->GetStartPosition();
        while(NULL != firstPos)
        {
            m_pNameToRegionPolygonMap->GetNextAssoc(firstPos,strRegionName,pPolygonMap);
            if((pPolygonMap != NULL) && (AfxIsValidAddress(pPolygonMap,sizeof(CMapOidToPolygon))))
            {
                long tempid;
                E00Polygon* pPoly = NULL;
                POSITION pos = pPolygonMap->GetStartPosition();
                while(NULL != pos)
                {
                    pPolygonMap->GetNextAssoc(pos,tempid,pPoly);
                    if(NULL != pPoly)
                    {
                        pPoly->ArcOidArray.RemoveAll();
                    }
                    delete pPoly;pPoly = NULL;
                }
                pPolygonMap->RemoveAll();
                delete pPolygonMap;pPolygonMap = NULL;
            }
        }
        m_pNameToRegionPolygonMap->RemoveAll();
        delete m_pNameToRegionPolygonMap;
        m_pNameToRegionPolygonMap = NULL;
    }
    if(NULL != m_pRouteMap)
    {
        E00Polygon * pPoly = NULL;
        long tmpid;
        POSITION pos = m_pRouteMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pRouteMap->GetNextAssoc(pos,tmpid,pPoly);
            if((pPoly != NULL) && (AfxIsValidAddress(pPoly,sizeof(E00Polygon))))
            {
                pPoly->ArcOidArray.RemoveAll();
                delete pPoly;pPoly = NULL;
            }
        }
        m_pRouteMap->RemoveAll();
        delete m_pRouteMap;
        m_pRouteMap = NULL;
    }
    if(NULL != m_pTextMap)
    {
        E00Text * pText = NULL;
        long tmpid;
        POSITION pos = m_pTextMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pTextMap->GetNextAssoc(pos,tmpid,pText);
            if((pText != NULL) && (AfxIsValidAddress(pText,sizeof(E00Text))))
            {
                if(NULL != pText->CoordData)
                {
                    delete [] pText->CoordData;
                    pText->CoordData = NULL;
                }
                delete pText;pText = NULL;
            }
        }
        m_pTextMap->RemoveAll();
        delete m_pTextMap;
        m_pTextMap = NULL;
    }
    if(NULL != m_pE00Info)
    {
        E00Table * pInfo = NULL;
        CString strTableName ;
        POSITION pos = m_pE00Info->GetStartPosition();
        while(NULL != pos)
        {
            m_pE00Info->GetNextAssoc(pos,strTableName,pInfo);
            if((pInfo != NULL) && (AfxIsValidAddress(pInfo,sizeof(E00Table))))
            {
                long count = pInfo->Fields.GetSize();
                for(long i = 0 ; i < count; i++)
                {
                    E00Field* pField = pInfo->Fields.GetAt(i);
                    if(NULL != pField)
                    {
                        delete pField;
                        pField = NULL;
                    }
                }
                pInfo->Fields.RemoveAll();
                POSITION tempos = pInfo->RecordArrayMap.GetStartPosition();
                while(NULL != tempos)
                {
                    long nID;
                    CStringArray * pArray = NULL;
                    pInfo->RecordArrayMap.GetNextAssoc(tempos,nID,pArray);
                    pArray->RemoveAll();
                    delete pArray; pArray = NULL;
                }
                pInfo->RecordArrayMap.RemoveAll();
                delete pInfo;pInfo = NULL;
            }
        }
        m_pE00Info->RemoveAll();
        delete m_pE00Info;
        m_pE00Info = NULL;
    }

    if(NULL != m_pNodePointMap)
    {
        E00LabPoint* pp = NULL;
        long tmpid;
        POSITION pos = m_pNodePointMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pNodePointMap->GetNextAssoc(pos,tmpid,pp);
            if(pp != NULL)
            {
                delete pp;pp = NULL;
            }
        }
        m_pNodePointMap->RemoveAll();
        delete m_pNodePointMap;
        m_pNodePointMap = NULL;
    }

    return TRUE;
}

long CFromE00::GetObjectCount()
{
    long nCount = 0;
    if(NULL != m_pPointMap)
    {
        E00LabPoint* pp = NULL;
        long tmpid;
        POSITION pos = m_pPointMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pPointMap->GetNextAssoc(pos,tmpid,pp);
            nCount ++;
        }
    }
    if(NULL != m_pLineMap)
    {
        E00Arc* pArc = NULL;
        long tmpid;
        POSITION pos = m_pLineMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pLineMap->GetNextAssoc(pos,tmpid,pArc);
            nCount ++;
        }
    }
    if(NULL != m_pPolygonMap)
    {
        E00Polygon * pPoly = NULL;
        long tmpid;
        POSITION pos = m_pPolygonMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pPolygonMap->GetNextAssoc(pos,tmpid,pPoly);
            nCount ++;
        }
    }
    if(NULL != m_pNameToRegionPolygonMap)
    {
        CMapOidToPolygon * pPolygonMap = NULL;
        CString strRegionName = "";
        POSITION firstPos = m_pNameToRegionPolygonMap->GetStartPosition();
        while(NULL != firstPos)
        {
            m_pNameToRegionPolygonMap->GetNextAssoc(firstPos,strRegionName,pPolygonMap);
            if((pPolygonMap != NULL) && (AfxIsValidAddress(pPolygonMap,sizeof(CMapOidToPolygon))))
            {
                long tempid;
                E00Polygon* pPoly = NULL;
                POSITION pos = pPolygonMap->GetStartPosition();
                while(NULL != pos)
                {
                    pPolygonMap->GetNextAssoc(pos,tempid,pPoly);
                    nCount ++;
                }
            }
        }
    }
    if(NULL != m_pRouteMap)
    {
        E00Polygon * pPoly = NULL;
        long tmpid;
        POSITION pos = m_pRouteMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pRouteMap->GetNextAssoc(pos,tmpid,pPoly);
            nCount ++;
        }
    }
    if(NULL != m_pTextMap)
    {
        E00Text * pText = NULL;
        long tmpid;
        POSITION pos = m_pTextMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pTextMap->GetNextAssoc(pos,tmpid,pText);
            nCount ++;
        }
    }

    if(NULL != m_pNodePointMap)
    {
        E00LabPoint* pp = NULL;
        long tmpid;
        POSITION pos = m_pNodePointMap->GetStartPosition();
        while(NULL != pos)
        {
            m_pNodePointMap->GetNextAssoc(pos,tmpid,pp);
            nCount ++;
        }
    }
    return nCount;
}

BOOL CFromE00::transPal(FILE *fp, int precID)
{
    int numParcel = 1;
    long numArc = -1;
    fscanf( fp, "%ld", &numArc );

    while ( -1 != numArc && !feof(fp))
    {
        double minx,miny,maxx,maxy;
        fscanf( fp, "%lf%lf", &minx, &miny );
        fscanf( fp, "%lf%lf", &maxx, &maxy );

        CLongArray tempOidArray;
        tempOidArray.RemoveAll();
        if(0 == numArc) numArc = 1;
        for ( int i = 0; i < numArc; ++i )
        {
            long id1, id2, id3;
            fscanf( fp, "%ld%ld%ld", &id1, &id2, &id3 );
            tempOidArray.Add(id1);
        }
        
        if(numParcel > 1)
        {
            E00Polygon* pPolygon = new E00Polygon;
            pPolygon->Oid = numParcel;
            pPolygon->ArcNums = numArc;
            pPolygon->env.minX = minx;
            pPolygon->env.minY = miny;
            pPolygon->env.maxX = maxx;
            pPolygon->env.maxY = maxy;

            pPolygon->ArcOidArray.RemoveAll();
            pPolygon->ArcOidArray.InsertAt(0,&tempOidArray);
            if(m_pPolygonMap != NULL)
            {
                m_pPolygonMap->SetAt(numParcel,pPolygon);
            }
            else
            {
                m_pPolygonMap = new CMapOidToPolygon;
                m_pPolygonMap->SetAt(numParcel,pPolygon);
            }
        }

        ++numParcel;
        fscanf( fp, "%d", &numArc );
        tempOidArray.RemoveAll();
    }
    return TRUE;
}

BOOL CFromE00 :: transArc( FILE *fp, int precID )
{
    long id;
    fscanf( fp, "%ld", &id );
    while ( -1 != id && !feof(fp))
    {
        long id1, id2, id3, id4, id5, numPoint;
        fscanf( fp, "%ld%ld%ld%ld%ld%ld", &id1, &id2, &id3, &id4, &id5, &numPoint );

        E00Arc* strLine = new E00Arc;
        strLine->Oid = id;
        strLine->Uid = id1;
        strLine->PointNums = numPoint;
        strLine->fNode = id2;
        strLine->tNode = id3;
        strLine->lPolygon = id4;
        strLine->rPolygon = id5;
        strLine->CoordData = NULL;
        strLine->CoordData = new double[numPoint * 2];
        for ( int i = 0; i < numPoint; ++i )
        {
            double x,y;
            fscanf( fp, "%lf%lf", &x, &y );
            *(strLine->CoordData + i * 2) = x;
            *(strLine->CoordData + i * 2 + 1) = y;

            if(m_bNode){
                if(0 == i && id2 > 0){
                    E00LabPoint * pStartNode = NULL;
                    pStartNode = new E00LabPoint;
                    pStartNode->Oid = id2;
                    pStartNode->Uid = id1;
                    pStartNode->x = x;
                    pStartNode->y = y;
                    E00LabPoint* strTmpNode = NULL;
                    if(m_pNodePointMap != NULL)
                    {
                        if(!m_pNodePointMap->Lookup(id2,strTmpNode))
                        {
                            m_pNodePointMap->SetAt(id2,pStartNode);
                        }
                    }
                    else
                    {
                        m_pNodePointMap = new CMapOidToLabPoint;
                        m_pNodePointMap->SetAt(id2,pStartNode);
                    }
                }
                if((numPoint - 1) == i && id3 > 0){
                    E00LabPoint * pEndNode = NULL;
                    pEndNode = new E00LabPoint;
                    pEndNode->Oid = id3;
                    pEndNode->Uid = id1;
                    pEndNode->x = x;
                    pEndNode->y = y;
                    E00LabPoint* strTmpNode = NULL;
                    if(m_pNodePointMap != NULL)
                    {
                        if(!m_pNodePointMap->Lookup(id3,strTmpNode))
                        {
                            m_pNodePointMap->SetAt(id3,pEndNode);
                        }
                    }
                    else
                    {
                        m_pNodePointMap = new CMapOidToLabPoint;
                        m_pNodePointMap->SetAt(id3,pEndNode);
                    }
                }
            }
        }

        E00Arc* strTmpLine = NULL;
        if(m_pLineMap != NULL)
        {
            if(!m_pLineMap->Lookup(id,strTmpLine))
            {
                m_pLineMap->SetAt(id,strLine);
            }
        }
        else
        {
            m_pLineMap = new CMapOidToLine;
            m_pLineMap->SetAt(id,strLine);
        }
        fscanf( fp, "%d", &id );
    }
    
    return TRUE;
}

void CFromE00 :: ReMap()
{    
    if(m_bAatAttrByUid)
    {
        if(NULL != m_pLineMap)
        {
            CMapOidToLine TempMap;
            TempMap.RemoveAll();

            E00Arc* pArc = NULL;
            long tmpid;
            POSITION pos = m_pLineMap->GetStartPosition();
            while(NULL != pos)
            {
                m_pLineMap->GetNextAssoc(pos,tmpid,pArc);

                E00Arc* strLine = new E00Arc;
                strLine->PointNums = pArc->PointNums;
                strLine->fNode = pArc->fNode;
                strLine->tNode = pArc->tNode;
                strLine->lPolygon = pArc->lPolygon;
                strLine->rPolygon = pArc->rPolygon;
                strLine->Uid = pArc->Oid;
                strLine->Oid = pArc->Uid;
                
                strLine->CoordData = new double[pArc->PointNums * 2]; 
                memcpy(strLine->CoordData,pArc->CoordData,sizeof(double) * (pArc->PointNums * 2));
                
                TempMap.SetAt(strLine->Oid,strLine);
            }

            pos = m_pLineMap->GetStartPosition();
            while(NULL != pos)
            {
                m_pLineMap->GetNextAssoc(pos,tmpid,pArc);
                if((pArc != NULL) && (AfxIsValidAddress(pArc,sizeof(E00Arc))))
                {
                    if(NULL != pArc->CoordData)
                    {
                        delete [] pArc->CoordData;
                        pArc->CoordData = NULL;
                    }
                    delete pArc;pArc = NULL;
                }
            }
            m_pLineMap->RemoveAll();
            pos = TempMap.GetStartPosition();
            while(pos){
                TempMap.GetNextAssoc(pos,tmpid,pArc);
                m_pLineMap->SetAt(tmpid,pArc);
            }
        }
    }    
    return;
}

BOOL CFromE00::ClearFileNames()
{
    m_FileNameArray.RemoveAll();
    return TRUE;
}

BOOL CFromE00::AddFileName(CString filename)
{
    m_FileNameArray.Add(filename);
    return TRUE;
}

long CFromE00::GetPointCount()
{
    return this->m_pPointMap->GetCount();
}

long CFromE00::GetLineCount()
{
    return this->m_pLineMap->GetCount();
}

long CFromE00::GetPolygonCount()
{
    return this->m_pPolygonMap->GetCount();
}

long CFromE00::GetTextCount()
{
    return this->m_pTextMap->GetCount();
}
