#if !defined(FIELDS_INCLUDED_)
#define FIELDS_INCLUDED_

#include "CommonInclude.h"
#include "..\\include\\InterfaceFields.h"

namespace easymap
{

class CFieldValue;
typedef TSmartPtr<CFieldValue> CFieldValuePtr;
class CFieldValues;
typedef TSmartPtr<CFieldValues> CFieldValuesPtr;
class CField;
typedef TSmartPtr<CField> CFieldPtr;
class CFields;
typedef TSmartPtr<CFields> CFieldsPtr;


//================================================================================
//  ×Ö¶ÎÖµ
//================================================================================
class CFieldValue : public IFieldValue
{
CLASS_NAME(CFieldValue)
PERSIST_DUMP(CFieldValue)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CFieldValue();
    CFieldValue(const CFieldValue& fieldvalue);
private:
    ~CFieldValue();

private:
    union FieldValue
    {
        short   sh;
        long    ln;
        float   sg;
        double  db;
        char*   ptr;
    };

    FieldType m_FT;
    FieldValue m_FV;

public:
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

private:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    void __stdcall SetFieldType(const FieldType fieldtype);
    FieldType __stdcall GetFieldType() const;
    bool __stdcall SetInteger(const long a);
    bool __stdcall GetInteger(long& a) const;
    bool __stdcall SetFloat(const double a);
    bool __stdcall GetFloat(double& a) const;
    bool __stdcall SetText(const char* const text);
    const char* __stdcall GetText() const;
    void __stdcall ToString(IAnsiString** ppString) const;
    bool SetString(const string& s);
    bool GetString(string& s) const;
    dword SetBlob(CStreamPtr pStream, const dword size);
    dword GetBlob(CStreamPtr pStream) const;
    string ToString() const;
};
//================================================================================


//================================================================================
//  ×Ö¶ÎÖµ
//================================================================================
class CFieldValues : public IFieldValues
{
CLASS_NAME(CFieldValues)
PERSIST_DUMP(CFieldValues)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CFieldValues(const CFieldValues& fieldvalues);
    CFieldValues(const CFieldsPtr pFields);
private:
    CFieldValues();
    ~CFieldValues();

private:
    vector<CFieldValuePtr> m_fieldvalues;

public:
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

private:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    bool __stdcall GetFieldValue(const dword index, IFieldValue** ppFieldValue) const;
    bool __stdcall GetFieldType(const dword index, FieldType& fieldtype) const;
    bool __stdcall SetInteger(const dword index, const long a);
    bool __stdcall GetInteger(const dword index, long& a) const;
    bool __stdcall SetFloat(const dword index, const double a);
    bool __stdcall GetFloat(const dword index, double& a) const;
    bool __stdcall SetText(const dword index, const char* const text);
    const char* __stdcall GetText(const dword index) const;
    dword __stdcall GetFieldCount() const;
    string FieldValueToString(const dword index) const;
    string SaveToString() const;
    void LoadFromString(const string& fieldvalues);
};
//================================================================================


//================================================================================
//  ×Ö¶Î
//================================================================================
class CField : public IField
{
CLASS_NAME(CField)
PERSIST_DUMP(CField)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CField(
        const string& fieldname,
        const FieldType fieldtype,
        const FieldIndexType indextype = FIELDINDEX_NOINDEX,
        const bool nullable = true
        );
    CField(const CField& field);
private:
    ~CField();

private:
    string          m_FieldName;
    FieldIndexType  m_FieldIndex;
    bool            m_Nullable;
    CFieldValuePtr  m_pDefaultValue;

private:
    CField();

public:
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

private:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

public:
    const char* __stdcall GetFieldName() const;
    string GetName() const;
    FieldType __stdcall GetFieldType() const;
    FieldIndexType __stdcall GetIndexType() const;
    bool __stdcall GetNullable() const;
};
//================================================================================


//================================================================================
//  ×Ö¶Î¼¯ºÏ
//================================================================================
class CFields : public IFields
{
CLASS_NAME(CFields)
PERSIST_DUMP(CFields)
NO_EVENTS_DISPATCHER
NO_EVENTS_LISTENER

public:
    CFields();
    CFields(const CFields& fields);
private:
    ~CFields();

public:
    bool __stdcall Clone(IObj** ppObj) const;
    bool Clone(IObjPtr& pObj) const;

public:
    bool __stdcall GotoInterface(const char* const interfacename, void** pp);
    dword __stdcall _SaveInstance(IStreamX* pStream, void* const assist) const;
    dword __stdcall _LoadInstance(IStreamX* pStream, void* const assist);

private:
    vector<CFieldPtr> m_Fields;

public:
    bool __stdcall AddField(const IField* pField);
    bool AddField(const CFieldPtr pField);
    bool __stdcall AddField2(const char* const name, const FieldType fieldtype,
        FieldIndexType indextype, bool nullable);
    bool __stdcall DeleteField(const dword index);
    bool __stdcall MoveField(const dword from, const dword to);
    bool __stdcall GetField(const dword index, IField** ppField) const;
    bool GetField(const dword index, CFieldPtr& pField) const;
    dword __stdcall GetFieldCount() const;
};
//================================================================================


CLASS_FACTORY(CFieldValue)
CLASS_FACTORY(CFieldValues)
CLASS_FACTORY(CField)
CLASS_FACTORY(CFields)

void NeatenFeatureStringValue(const string from, string& to);
bool RestoreFeatureStringValue(const string from, string& to);

string FieldValue2String(const IFieldValuePtr pFieldValue);
}

#endif