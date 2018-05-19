#include "CommonInclude.h"
#include "Fields.h"
#include "SupportClasses.h"
#include "StringFuncs.h"

namespace easymap
{

CLASS_FACTORY_INSTANCE(CFieldValue)
CLASS_FACTORY_INSTANCE(CFieldValues)
CLASS_FACTORY_INSTANCE(CField)
CLASS_FACTORY_INSTANCE(CFields)

//------------------------------------------------------------------------------
//  CFieldValue::CFieldValue
//------------------------------------------------------------------------------
CFieldValue::CFieldValue()
{
    INIT_REFCOUNT

    m_FT = FIELDTYPE_UNKNOWN;
}

CFieldValue::CFieldValue(const CFieldValue& fieldvalue)
{
    INIT_REFCOUNT

    FieldType ft = fieldvalue.m_FT;
    this->m_FT = ft;
    ::memcpy(&this->m_FV, &fieldvalue.m_FV, sizeof(FieldValue));
    if ((FIELDTYPE_STRING == ft) || (FIELDTYPE_BLOB == ft))
    {
        if (_valid(fieldvalue.m_FV.ptr))
        {
            dword size = (dword)(*(fieldvalue.m_FV.ptr));
            this->m_FV.ptr = new char[sizeof(dword) + size];
            memcpy(this->m_FV.ptr, fieldvalue.m_FV.ptr, size+sizeof(dword));
        }
    }
}

CFieldValue::~CFieldValue()
{
    SetFieldType(FIELDTYPE_UNKNOWN);
}

bool __stdcall CFieldValue::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CFieldValue(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool CFieldValue::Clone(IObjPtr& pObj) const
{
    CFieldValuePtr pValue = new CFieldValue(*this);
    CAST_PTR(pValue, pObj, IObj)
    return true;
}

bool __stdcall CFieldValue::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CFieldValue"))
        || (0 == strcmp(interfacename, "IFieldValue")))
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

dword __stdcall CFieldValue::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    pStream->WriteData(&m_FT, sizeof(FieldType));
    pStream->WriteData(&m_FV, sizeof(FieldValue));
    if ((FIELDTYPE_STRING == m_FT) || (FIELDTYPE_BLOB == m_FT))
    {
        dword size = 0;
        if (_invalid(m_FV.ptr))
        {
            pStream->WriteData(&size, sizeof(dword));
        }
        else
        {
            ::memcpy(&size, m_FV.ptr, sizeof(dword));
            pStream->WriteData(&size, sizeof(dword));
            pStream->WriteData(sizeof(dword) + m_FV.ptr, size);
        }
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CFieldValue::_LoadInstance(IStreamX* pStream, void* const assist)
{
    this->SetFieldType(FIELDTYPE_UNKNOWN);

    dword oldpos = pStream->GetPos();

    pStream->ReadData(&m_FT, sizeof(FieldType));
    pStream->ReadData(&m_FV, sizeof(FieldValue));
    if ((FIELDTYPE_STRING == m_FT) || (FIELDTYPE_BLOB == m_FT))
    {
        dword size;
        pStream->ReadData(&size, sizeof(dword));
        if (0 == size)
        {
            m_FV.ptr = NULL;
        }
        else
        {
            m_FV.ptr = new char[sizeof(dword) + size];
            ::memcpy(m_FV.ptr, &size, sizeof(dword));
            pStream->ReadData(m_FV.ptr + sizeof(dword), size);
        }
    }

    return pStream->GetPos() - oldpos;
}

void __stdcall CFieldValue::SetFieldType(const FieldType fieldtype)
{
    if (fieldtype == m_FT) {return;}
    if (((FIELDTYPE_STRING == m_FT) || (FIELDTYPE_BLOB == m_FT))
        && (_valid(m_FV.ptr)))
    {
        delete[] m_FV.ptr;
    }

    m_FT = fieldtype;
    if ((FIELDTYPE_STRING == m_FT) || (FIELDTYPE_BLOB == m_FT))
        m_FV.ptr = NULL;
}

FieldType __stdcall CFieldValue::GetFieldType() const
{
    return m_FT;
}

bool __stdcall CFieldValue::SetInteger(const long a)
{
    if (FIELDTYPE_SHORT == m_FT) {m_FV.sh = (short)a;}
    else if (FIELDTYPE_LONG == m_FT) {m_FV.ln = a;}
    else {return false;}

    return true;
}

bool __stdcall CFieldValue::GetInteger(long& a) const
{
    if (FIELDTYPE_SHORT == m_FT) {a = (short)m_FV.sh;}
    else if (FIELDTYPE_LONG == m_FT) {a = m_FV.ln;}
    else {return false; }
    
    return true;
}

bool __stdcall CFieldValue::SetFloat(const double a)
{
    if (FIELDTYPE_SINGLE == m_FT) {m_FV.sg = (float)a;}
    else if (FIELDTYPE_DOUBLE == m_FT) {m_FV.db = a;}
    else {return false;}
    
    return true;
}

bool __stdcall CFieldValue::GetFloat(double& a) const
{
    if (FIELDTYPE_SINGLE == m_FT) {a = (float)m_FV.sg;}
    else if (FIELDTYPE_DOUBLE == m_FT) {a = m_FV.db;}
    else {return false;}

    return true;
}

bool __stdcall CFieldValue::SetText(const char* const text)
{
    return this->SetString(text);
}

const char* __stdcall CFieldValue::GetText() const
{
    if (FIELDTYPE_STRING != m_FT)
        return NULL;

    if (NULL != m_FV.ptr)
    {
        return m_FV.ptr + sizeof(dword);
    }

    return NULL;
}

void __stdcall CFieldValue::ToString(IAnsiString** ppString) const
{
    if (_invalid(ppString)) return;
    assert(!*ppString);
    *ppString = new CAnsiString();
    (*ppString)->_AddRef();

    string s = this->ToString();
    (*ppString)->SetText(s.c_str());
}

bool CFieldValue::SetString(const string& s)
{
    if (FIELDTYPE_STRING != m_FT) {return false;}
    if (_valid(m_FV.ptr)) {delete[] m_FV.ptr;}

    dword len = s.size() + 1;
    m_FV.ptr = new char[len+sizeof(dword)];
    ::memcpy(m_FV.ptr, &len, sizeof(dword));
    ::memcpy(m_FV.ptr+sizeof(dword), s.c_str(), len);

    return true;
}

bool CFieldValue::GetString(string& s) const
{
    if (FIELDTYPE_STRING != m_FT)
        return false;

    if (NULL != m_FV.ptr)
    {
        s = m_FV.ptr + sizeof(dword);
    }
    else
    {
        s = "";
    }
    return true;
}

dword CFieldValue::SetBlob(CStreamPtr pStream, const dword size)
{
    if (FIELDTYPE_BLOB != m_FT) return 0;
    if (_valid(m_FV.ptr)) delete[] m_FV.ptr;
    m_FV.ptr = new char[sizeof(dword) + size];
    memcpy(m_FV.ptr, &size, sizeof(dword));
    return sizeof(dword) + pStream->Read(sizeof(dword) + m_FV.ptr, size);
}

dword CFieldValue::GetBlob(CStreamPtr pStream) const
{
    if (FIELDTYPE_BLOB != m_FT) return 0;
    if (_invalid(m_FV.ptr)) return 0;
    dword size;
    memcpy(&size, m_FV.ptr, sizeof(dword));
    pStream->Write(size);
    return sizeof(dword) + pStream->Write(sizeof(dword) + m_FV.ptr, size);
}

string CFieldValue::ToString() const
{
    switch (m_FT)
    {
    case FIELDTYPE_SHORT:
    case FIELDTYPE_LONG:
        long integervalue;
        this->GetInteger(integervalue);
        return IntToStr(integervalue);

    case FIELDTYPE_SINGLE:
    case FIELDTYPE_DOUBLE:
        double floatvalue;
        this->GetFloat(floatvalue);
        return FloatToStr(floatvalue);

    default:
        return this->GetText();
    }
}


void NeatenFeatureStringValue(const string from, string& to)
{
    to = "";
    dword size = from.size();
    to.reserve(size);
    for (dword i = 0; i < size; i++)
    {
        if ('\n' == from[i])
        {
            to = to + "\\n";
        }
        else if('\r' == from[i])
        {
            to = to + "\\r";
        }
        else if('\\' == from[i])
        {
            to = to + "\\\\";
        }
        else
        {
            to = to + from[i];
        }
    }
}

bool RestoreFeatureStringValue(const string from, string& to)
{
    to = "";
    dword size = from.size();
    to.reserve(size);
    for (dword i = 0; i < size; i++)
    {
        char c = from[i];
        if ('\\' == c)
        {
            c = from[++i];
            if ('\\' == c)
            {
                to = to + c;
            }
            else if ('n' == c)
            {
                to = to + '\n';
            }
            else if ('r' == c)
            {
                to = to + '\r';
            }
            else
            {
                return false;
            }
        }
        else
        {
            to = to + c;
        }
    }
    return true;
}

CFieldValues::CFieldValues()
{
    INIT_REFCOUNT

}

CFieldValues::CFieldValues(const CFieldsPtr pFields)
{
    INIT_REFCOUNT

    if (!pFields.Assigned()) return;
    dword count = pFields->GetFieldCount();
    for (dword i = 0; i < count; i++)
    {
        CFieldPtr pField;
        pFields->GetField(i, pField);
        FieldType fieldtype = pField->GetFieldType();
        CFieldValuePtr pFieldValue = new CFieldValue;
        pFieldValue->SetFieldType(fieldtype);
        m_fieldvalues.push_back(pFieldValue);
    }
}

CFieldValues::CFieldValues(const CFieldValues& fieldvalues)
{
    INIT_REFCOUNT

    dword fieldcount = fieldvalues.m_fieldvalues.size();
    for (dword i = 0; i < fieldcount; i++)
    {
        IObjPtr pObj;
        CLONE_PTR(fieldvalues.m_fieldvalues[i], pObj)
        CFieldValuePtr pFeatureValue;
        CAST_PTR(pObj, pFeatureValue, CFieldValue)
        m_fieldvalues.push_back(pFeatureValue);
    }
}

CFieldValues::~CFieldValues()
{
}

string CFieldValues::SaveToString() const
{
    long fv_int;
    double fv_float;
    string fv_text, attribstring;

    dword fieldcount = m_fieldvalues.size();
    for (dword i = 0; i < fieldcount; i++)
    {
        FieldType fieldtype;
        this->GetFieldType(i, fieldtype);

        switch (fieldtype)
        {
        case FIELDTYPE_SHORT:
        case FIELDTYPE_LONG:
            this->GetInteger(i, fv_int);
            attribstring = attribstring + IntToStr(fv_int) + "\n";
            break;

        case FIELDTYPE_SINGLE:
        case FIELDTYPE_DOUBLE:
            this->GetFloat(i, fv_float);
            attribstring = attribstring + FloatToStr(fv_float) + "\n";
            break;

        default:
            const char* pc = this->GetText(i);
            if (_valid(pc))
            {
                fv_text = pc;
            }

            NeatenFeatureStringValue(fv_text, fv_text);
            attribstring = attribstring + fv_text + "\n";
        }
    }

    return attribstring;
}

void CFieldValues::LoadFromString(const string& fieldvalues)
{
    Strings strings;
    strings.SetText(fieldvalues);

    dword fieldcount = this->GetFieldCount();
    for (dword i = 0; i < fieldcount; i++)
    {
        string fv;
        strings.GetLine(i, fv);

        FieldType fieldtype = FIELDTYPE_UNKNOWN;
        this->GetFieldType(i, fieldtype);
        switch (fieldtype)
        {
        case FIELDTYPE_SHORT:
        case FIELDTYPE_LONG:
            this->SetInteger(i, StrToInt(fv));
            break;

        case FIELDTYPE_SINGLE:
        case FIELDTYPE_DOUBLE:
            this->SetFloat(i, StrToFloat(fv));
            break;

        default:
            RestoreFeatureStringValue(fv, fv);
            this->SetText(i, fv.c_str());
        }
    }
}

bool __stdcall CFieldValues::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);
    *ppObj = new CFieldValues(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool CFieldValues::Clone(IObjPtr& pObj) const
{
    CFieldValuesPtr pFieldValues = new CFieldValues(*this);
    CAST_PTR(pFieldValues, pObj, IObj)
    return true;
}

bool __stdcall CFieldValues::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CFieldValues"))
        || (0 == strcmp(interfacename, "IFieldValues")))
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

dword __stdcall CFieldValues::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    dword count = m_fieldvalues.size();
    pStream->WriteData(&count, sizeof(dword));

    for (dword i = 0; i < count; i++)
    {
        CFieldValuePtr pFieldValue = m_fieldvalues[i];
        pFieldValue->_DumpTo(pStream, NULL);
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CFieldValues::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_fieldvalues.clear();
    dword count;
    ps->Read(count);

    for (dword i = 0; i < count; i++)
    {
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom(ps, pPersist, assist);
        CFieldValuePtr pFieldValue;
        CAST_PTR(pPersist, pFieldValue, CFieldValue)
        m_fieldvalues.push_back(pFieldValue);
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CFieldValues::GetFieldValue(const dword index, IFieldValue** ppFieldValue) const
{
    if (index >= m_fieldvalues.size()) return false;
    if (_invalid(ppFieldValue)) return false;
    assert(!*ppFieldValue);
    *ppFieldValue = NULL;

    CFieldValuePtr pFieldValue = m_fieldvalues[index];
    IObjPtr pNewObj;
    CLONE_PTR(pFieldValue, pNewObj)
    *ppFieldValue = (IFieldValue*)pNewObj._p();
    (*ppFieldValue)->_AddRef();
    return true;

}

bool __stdcall CFieldValues::GetFieldType(const dword index, FieldType& fieldtype) const
{
    if (index >= m_fieldvalues.size()) return false;
    fieldtype = m_fieldvalues[index]->GetFieldType();
    return true;
}

bool __stdcall CFieldValues::SetInteger(const dword index, const long a)
{
    if (index >= m_fieldvalues.size()) return false;
    return m_fieldvalues[index]->SetInteger(a);
}

bool __stdcall CFieldValues::GetInteger(const dword index, long& a) const
{
    if (index >= m_fieldvalues.size()) return false;
    return m_fieldvalues[index]->GetInteger(a);
}

bool __stdcall CFieldValues::SetFloat(const dword index, const double a)
{
    if (index >= m_fieldvalues.size()) return false;
    return m_fieldvalues[index]->SetFloat(a);
}

bool __stdcall CFieldValues::GetFloat(const dword index, double& a) const
{
    if (index >= m_fieldvalues.size()) return false;
    return m_fieldvalues[index]->GetFloat(a);
}

bool __stdcall CFieldValues::SetText(const dword index, const char* const text)
{
    if (index >= m_fieldvalues.size()) return false;
    return m_fieldvalues[index]->SetText(text);
}

const char* __stdcall CFieldValues::GetText(const dword index) const
{
    if (index >= m_fieldvalues.size()) return NULL;
    return m_fieldvalues[index]->GetText();
}

dword __stdcall CFieldValues::GetFieldCount() const
{
    return m_fieldvalues.size();
}

string CFieldValues::FieldValueToString(const dword index) const
{
    if (index >= m_fieldvalues.size()) return "";

    CFieldValuePtr pFieldValue = m_fieldvalues[index];
    FieldType fieldtype = pFieldValue->GetFieldType();
    switch (fieldtype)
    {
    case FIELDTYPE_SHORT:
    case FIELDTYPE_LONG:
        long integervalue;
        pFieldValue->GetInteger(integervalue);
        return IntToStr(integervalue);

    case FIELDTYPE_SINGLE:
    case FIELDTYPE_DOUBLE:
        double floatvalue;
        pFieldValue->GetFloat(floatvalue);
        return FloatToStr(floatvalue);

    default:
        return pFieldValue->GetText();
    }
}


//------------------------------------------------------------------------------
//  CField
//------------------------------------------------------------------------------
CField::CField(const string& fieldname, const FieldType fieldtype,
    const FieldIndexType indextype, const bool nullable)
{
    INIT_REFCOUNT

    m_pDefaultValue = new CFieldValue;
    string fname = Trim(fieldname);
    if (string("") == fname)
    {
        m_pDefaultValue->SetFieldType(FIELDTYPE_UNKNOWN);
        return;
    }

    m_FieldName = fname;
    m_pDefaultValue->SetFieldType(fieldtype);
    m_FieldIndex = indextype;
    m_Nullable = nullable;
}

CField::CField(const CField& field)
{
    INIT_REFCOUNT

    this->m_FieldName = field.m_FieldName;
    this->m_FieldIndex = field.m_FieldIndex;
    this->m_Nullable = field.m_Nullable;
    IObjPtr pDVObj;
    field.m_pDefaultValue->Clone(pDVObj);
    CAST_PTR(pDVObj, this->m_pDefaultValue, CFieldValue)
}

CField::CField()
{
    INIT_REFCOUNT

    m_pDefaultValue = new CFieldValue;
    m_pDefaultValue->SetFieldType(FIELDTYPE_UNKNOWN);
}


CField::~CField()
{
}

bool __stdcall CField::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
//    if (_valid(*pp)) ((IObj*)(*pp))->_Release();
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CField"))
        || (0 == strcmp(interfacename, "IField")))
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

dword __stdcall CField::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Write(m_FieldName);
    ps->Write(&m_FieldIndex, sizeof(FieldIndexType));
    ps->WriteBool(m_Nullable);
    m_pDefaultValue->_DumpTo(pStream, NULL);

    return pStream->GetPos() - oldpos;
}

dword __stdcall CField::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    ps->Read(m_FieldName);
    ps->Read(&m_FieldIndex, sizeof(FieldIndexType));
    ps->ReadBool(m_Nullable);
    CPersistPtr pPersist;
    CPersist::_InstantiateFrom(ps, pPersist, assist);
    CAST_PTR(pPersist, m_pDefaultValue, CFieldValue);

    return pStream->GetPos() - oldpos;
}


bool __stdcall CField::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CField(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool CField::Clone(IObjPtr& pObj) const
{
    CFieldPtr pField = new CField(*this);
    CAST_PTR(pField, pObj, IObj)
    return true;
}

const char* __stdcall CField::GetFieldName() const
{
    return m_FieldName.c_str();
}

string CField::GetName() const
{
    return m_FieldName;
}

FieldType __stdcall CField::GetFieldType() const
{
    return m_pDefaultValue->GetFieldType();
}

FieldIndexType __stdcall CField::GetIndexType() const
{
    return m_FieldIndex;
}

bool __stdcall CField::GetNullable() const
{
    return m_Nullable;
}


//------------------------------------------------------------------------------
//  CFields
//------------------------------------------------------------------------------
CFields::CFields()
{
    INIT_REFCOUNT
}

CFields::CFields(const CFields& fields)
{
    INIT_REFCOUNT

    dword size = fields.GetFieldCount();
    for (dword i = 0; i < size; i++)
    {
        CFieldPtr pField;
        fields.GetField(i, pField);
        CFieldPtr pFieldNew = new CField(*pField);
        m_Fields.push_back(pFieldNew);
    }
}

CFields::~CFields()
{
}

bool __stdcall CFields::Clone(IObj** ppObj) const
{
    if (_invalid(ppObj)) return false;
    assert(!*ppObj);

    *ppObj = new CFields(*this);
    (*ppObj)->_AddRef();
    return true;
}

bool CFields::Clone(IObjPtr& pObj) const
{
    CFieldsPtr pFields = new CFields(*this);
    CAST_PTR(pFields, pObj, IObj)
    return true;
}

bool __stdcall CFields::GotoInterface(const char* const interfacename, void** pp)
{
    if (_invalid(pp)) return false;
    assert(!*pp);

    if ((0 == strcmp(interfacename, "IObj"))
        || (0 == strcmp(interfacename, "IPersist"))
        || (0 == strcmp(interfacename, "CPersist"))
        || (0 == strcmp(interfacename, "CFields"))
        || (0 == strcmp(interfacename, "IFields")))
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

dword __stdcall CFields::_SaveInstance(IStreamX* pStream, void* const assist) const
{
    dword oldpos = pStream->GetPos();

    dword fieldcount = m_Fields.size();
    pStream->WriteData(&fieldcount, sizeof(dword));

    for (dword i = 0; i < fieldcount; i++)
    {
        CFieldPtr pField;
        this->GetField(i, pField);
        pField->_DumpTo(pStream, NULL);
    }

    return pStream->GetPos() - oldpos;
}

dword __stdcall CFields::_LoadInstance(IStreamX* pStream, void* const assist)
{
    dword oldpos = pStream->GetPos();
    CStreamPtr ps = (CStream*)pStream;

    m_Fields.clear();
    dword fieldcount;
    ps->Read(fieldcount);

    for (dword i = 0; i < fieldcount; i++)
    {
        CPersistPtr pPersist;
        CPersist::_InstantiateFrom(ps, pPersist, assist);
        CFieldPtr pField;
        CAST_PTR(pPersist, pField, CField)
        this->AddField(pField);
    }

    return pStream->GetPos() - oldpos;
}

bool __stdcall CFields::AddField(const IField* pField)
{
    if (_invalid(pField)) return false;

    CFieldPtr pF = (CField*)pField;
    return this->AddField(pF);
}

bool CFields::AddField(const CFieldPtr pField)
{
    string newname = UpperString(pField->GetName());
    FieldType fieldtype = pField->GetFieldType();
    if (("" == newname) || (FIELDTYPE_UNKNOWN == fieldtype)) return false;

    vector<CFieldPtr>::const_iterator it = m_Fields.begin();
    while (it != m_Fields.end())
    {
        string fname = (*it)->GetName().c_str();
        if (0 == strcmp(fname.c_str(), newname.c_str())) return false;

        it++;
    }

    IObjPtr pObj;
    CLONE_PTR(pField, pObj)
    CFieldPtr pF;
    CAST_PTR(pObj, pF, CField)
    m_Fields.push_back(pF);
    return true;
}

bool __stdcall CFields::AddField2(const char* const name, const FieldType fieldtype,
    FieldIndexType indextype, bool nullable)
{
    string fieldname = Trim(name);
    CFieldPtr pField = new CField(fieldname, fieldtype, indextype, nullable);
    return this->AddField(pField);
}

bool __stdcall CFields::DeleteField(const dword index)
{
    if ((index < 0) || (index >= m_Fields.size())) return false;

    vector<CFieldPtr>::iterator it = m_Fields.begin();
    std::advance(it, index);
    m_Fields.erase(it);
    return true;
}

bool __stdcall CFields::MoveField(const dword from, const dword to)
{
    return false;
}

bool __stdcall CFields::GetField(const dword index, IField** ppField) const
{
    if (_invalid(ppField)) return false;
    assert(!*ppField);
    *ppField = NULL;

    CFieldPtr pF;
    bool r = this->GetField(index, pF);
    if (pF.Assigned())
    {
        *ppField = (IField*)pF._p();
        (*ppField)->_AddRef();
    }

    return r;
}

bool CFields::GetField(const dword index, CFieldPtr& pField) const
{
    if ((index < 0) || (index >= m_Fields.size())) return false;

    IObjPtr pObj;
    CLONE_PTR(m_Fields[index], pObj)
    CFieldPtr pF;
    CAST_PTR(pObj, pF, CField)
    pField = pF;
    return true;
}

dword __stdcall CFields::GetFieldCount() const
{
    return m_Fields.size();
}

string FieldValue2String(const IFieldValuePtr pFieldValue)
{
    switch (pFieldValue->GetFieldType())
    {
    case FIELDTYPE_SHORT:
    case FIELDTYPE_LONG:
        {
            long nvalue;
            pFieldValue->GetInteger(nvalue);
            return IntToStr(nvalue);
        }
        break;

    case FIELDTYPE_SINGLE:
    case FIELDTYPE_DOUBLE:
        {
            double fvalue;
            pFieldValue->GetFloat(fvalue);
            return FloatToStr(fvalue);
        }
        break;

    case FIELDTYPE_STRING:
        {
            return pFieldValue->GetText();
        }
        break;

    default:
        {}
    }

    return "";
}

}
