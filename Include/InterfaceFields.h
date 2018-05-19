#if !defined(INTERFACEFIELDS_INCLUDED_)
#define INTERFACEFIELDS_INCLUDED_

#include "InterfaceSupport.h"

namespace easymap
{

class IFieldValue;
class IFieldValues;
class IField;
class IFields;
typedef TSmartPtr<IFieldValue> IFieldValuePtr;
typedef TSmartPtr<IFieldValues> IFieldValuesPtr;
typedef TSmartPtr<IField> IFieldPtr;
typedef TSmartPtr<IFields> IFieldsPtr;

typedef long FieldType;
const FieldType FIELDTYPE_UNKNOWN       = 0;
const FieldType FIELDTYPE_SHORT         = 1;
const FieldType FIELDTYPE_LONG          = 2;
const FieldType FIELDTYPE_SINGLE        = 3;
const FieldType FIELDTYPE_DOUBLE        = 4;
const FieldType FIELDTYPE_STRING        = 5;
const FieldType FIELDTYPE_BLOB          = 6;

typedef long FieldIndexType;
const FieldIndexType FIELDINDEX_NOINDEX = 0;
const FieldIndexType FIELDINDEX_NODUP   = 1;
const FieldIndexType FIELDINDEX_DUP     = 2;


//================================================================================
//  字段值
//================================================================================
class IFieldValue : public IPersist
{
public:
    virtual void __stdcall SetFieldType(const FieldType fieldtype) = 0;
    virtual FieldType __stdcall GetFieldType() const = 0;
    virtual bool __stdcall SetInteger(const long a) = 0;
    virtual bool __stdcall GetInteger(long& a) const = 0;
    virtual bool __stdcall SetFloat(const double a) = 0;
    virtual bool __stdcall GetFloat(double& a) const = 0;
    virtual bool __stdcall SetText(const char* const text) = 0;
    virtual const char* __stdcall GetText() const = 0;
    virtual void __stdcall ToString(IAnsiString** ppString) const = 0;
};


//================================================================================
//  字段值集合
//================================================================================
class IFieldValues : public IPersist
{
public:
    virtual bool __stdcall GetFieldValue(const dword index, IFieldValue** ppFieldValue) const = 0;
    virtual bool __stdcall GetFieldType(const dword index, FieldType& fieldtype) const = 0;
    virtual bool __stdcall SetInteger(const dword index, const long a) = 0;
    virtual bool __stdcall GetInteger(const dword index, long& a) const = 0;
    virtual bool __stdcall SetFloat(const dword index, const double a) = 0;
    virtual bool __stdcall GetFloat(const dword index, double& a) const = 0;
    virtual bool __stdcall SetText(const dword index, const char* const text) = 0;
    virtual const char* __stdcall GetText(const dword index) const = 0;
    virtual dword __stdcall GetFieldCount() const = 0;
};


//================================================================================
//  字段
//================================================================================
class IField : public IPersist
{
public:
    virtual const char* __stdcall GetFieldName() const = 0;
    virtual FieldType __stdcall GetFieldType() const = 0;
    virtual FieldIndexType __stdcall GetIndexType() const = 0;
    virtual bool __stdcall GetNullable() const = 0;
};


//================================================================================
//  字段集合
//================================================================================
class IFields : public IPersist
{
public:
    virtual bool __stdcall AddField(const IField* pField) = 0;
    virtual bool __stdcall AddField2(const char* const name, const FieldType fieldtype,
        FieldIndexType indextype, bool nullable) = 0;
    virtual bool __stdcall DeleteField(const dword index) = 0;
    virtual bool __stdcall MoveField(const dword from, const dword to) = 0;
    virtual bool __stdcall GetField(const dword index, IField** ppField) const = 0;
    virtual dword __stdcall GetFieldCount() const = 0;
};

}

#endif