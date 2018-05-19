unit InterfaceFields;

interface

uses
    InterfaceObj;

type TFieldType = Longint;
const FIELDTYPE_UNKNOWN =   TFieldType(0);
const FIELDTYPE_SHORT   =   TFieldType(1);
const FIELDTYPE_LONG    =   TFieldType(2);
const FIELDTYPE_SINGLE  =   TFieldType(3);
const FIELDTYPE_DOUBLE  =   TFieldType(4);
const FIELDTYPE_STRING  =   TFieldType(5);
const FIELDTYPE_BLOB    =   TFieldType(6);

type TFieldIndexType = Longint;
const FIELDINDEX_NOINDEX =   TFieldIndexType(0);
const FIELDINDEX_NODUP  =   TFieldIndexType(1);
const FIELDINDEX_DUP    =   TFieldIndexType(2);

type IFieldValue = interface(IPersist)
    procedure SetFieldType(const fieldtype: TFieldType); stdcall;
    function GetFieldType(): TFieldType; stdcall;
    function SetInteger(const a: Longint): Boolean; stdcall;
    function GetInteger(out a: Longint): Boolean; stdcall;
    function SetFloat(const a: double): Boolean; stdcall;
    function GetFloat(out a: double): Boolean; stdcall;
    function SetText(const text: PChar): Boolean; stdcall;
    function GetText(): PChar; stdcall;
    procedure ToString(out ppString: IAnsiString); stdcall;
end;

type IFieldValues = interface(IPersist)
    function GetFieldValue(const index: Longword; out ppFieldValue: IFieldValue): Boolean; stdcall;
    function GetFieldType(const index: Longword; out fieldtype: TFieldType): Boolean; stdcall;
    function SetInteger(const index: Longword; const a: Longint): Boolean; stdcall;
    function GetInteger(const index: Longword; out a: Longint): Boolean; stdcall;
    function SetFloat(const index: Longword; const a: Double): Boolean; stdcall;
    function GetFloat(const index: Longword; out a: Double): Boolean; stdcall;
    function SetText(const index: Longword; const text: PChar): Boolean; stdcall;
    function GetText(const index: Longword): PChar; stdcall;
    function GetFieldCount(): Longword; stdcall;
end;

type IField = interface(IPersist)
    function GetFieldName(): PChar; stdcall;
    function GetFieldType(): TFieldType; stdcall;
    function GetIndexType(): TFieldIndexType; stdcall;
    function GetNullable(): Boolean; stdcall;
end;

type IFields = interface(IPersist)
    function AddField(const pField: IField): Boolean; stdcall;
    function AddField2(const name: PChar; const fieldtype: TFieldType;
        indextype: TFieldIndexType; nullable: Boolean): Boolean; stdcall;
    function DeleteField(const index: Longword): Boolean; stdcall;
    function MoveField(const from: Longword; const _to: Longword): Boolean; stdcall;
    function GetField(const index: Longword; out ppField: IField): Boolean; stdcall;
    function GetFieldCount(): Longword; stdcall;
end;

implementation

end.
