unit InterfaceObj;

interface

{$IFNDEF VER150}
type IInterface = IUnknown;
{$ENDIF}

type TEvent_Identity = Longint;
const EVENT_IDENTITY_NULL = TEvent_Identity(0);

type
IObjArray = interface;
IObj = interface(IInterface)
    function _Debug(): Longword; stdcall;
    function _Interface(const interfacename: PChar; var p: Pointer): Boolean; stdcall;
    function GetClassName(): PChar; stdcall;

    function GetEventsCount(): Longword; stdcall;
    function AddEvent(const ei: TEvent_Identity): Boolean; stdcall;
    function RemoveEvent(const ei: TEvent_Identity): Boolean; stdcall;
    function GetEvent(const index: Longword): TEvent_Identity; stdcall;
    function _RegisterListener(const ei: TEvent_Identity; const pListener: IObj): Boolean; stdcall;
    function _UnregisterListener(const ei: TEvent_Identity; const pListener: IObj): Boolean; stdcall;
    function _GetListenersByEvent(const ei: TEvent_Identity; out ppListeners: IObjArray): Boolean; stdcall;
    function _GetEventCountByListener(const pListener: IObj): Longword; stdcall;
    function _KickAssListener(const pListener: IObj): Boolean; stdcall;

    function RegisterToDispatcher(const ei: TEvent_Identity; const pDispatcher: IObj): Boolean; stdcall;
    function UnregisterFromDispatcher(const ei: TEvent_Identity; const pDispatcher: IObj): Boolean; stdcall;
    function UnregisterAllFromDispatcher(const pDispatcher: IObj): Boolean; stdcall;
    function DispatchMessage(const ei: TEvent_Identity; const pMessage: IObj; var tag: Double;
        const message_description: PChar): Boolean; stdcall;

    function Clone(var obj: IObj): Boolean; stdcall;
end;

IObjArray = interface(IObj)
    function Add(const pObj: IObj): Boolean; stdcall;
    function SetAt(const index: Longword; const pObj: IObj): Boolean; stdcall;
    function GetAt(const index: Longword; out ppObj: IObj): Boolean; stdcall;
    procedure Clear(); stdcall;
    function Resize(const newsize: Longword): Boolean; stdcall;
    function GetSize(): Longword; stdcall;
end;

type TStreamOffsetOrigin    = Longint;
const SOFROMBEGINNING       = TStreamOffsetOrigin(0);
const SOFROMCURRENT         = TStreamOffsetOrigin(1);
const SOFROMEND             = TStreamOffsetOrigin(2);

type IStreamX = interface(IObj)
    function ReadOnly(): Boolean; stdcall;
    function MovePos(const delta: Longint; const origin: TStreamOffsetOrigin): Longword; stdcall;
    function GetPos(): Longword; stdcall;
    function Eof(): Boolean; stdcall;
    function Bof(): Boolean; stdcall;
    function ReadData(p: Pointer; const count: Longword): Longword; stdcall;
    function WriteData(const p: Pointer; const count: Longword): Longword; stdcall;
    function SetSize(const size: Longword): Boolean; stdcall;
    function GetSize(): Longword; stdcall;
    function SaveToFile(const filename: PChar): Boolean; stdcall;
    function LoadFromFile(const filename: PChar): Boolean; stdcall;
end;

type IPersist = interface(IObj)
    function _SaveInstance(pStream: IStreamX; reserved: Pointer): Longword; stdcall;
    function _LoadInstance(pStream: IStreamX; reserved: Pointer): Longword; stdcall;
    function _DumpTo(pStream: IStreamX; reserved: Pointer): Longword; stdcall;
    function Dump(pStream: IStreamX): Longword; stdcall;
end;
    
type IIntArray = interface(IPersist)
    function Add(const value: Longint): Boolean; stdcall;
    function SetAt(const index: Longword; const value: Longint): Boolean; stdcall;
    function GetAt(const index: Longword; out value: Longint): Boolean; stdcall;
    procedure Clear(); stdcall;
    function Resize(const newsize: Longword): Boolean; stdcall;
    function GetSize(): Longword; stdcall;
end;

type IDoubleArray = interface(IPersist)
    function Add(const value: Double): Boolean; stdcall;
    function SetAt(const index: Longword; const value: Double): Boolean; stdcall;
    function GetAt(const index: Longword; out value: Double): Boolean; stdcall;
    procedure Clear(); stdcall;
    function Resize(const newsize: Longword): Boolean; stdcall;
    function GetSize(): Longword; stdcall;
end;

type IAnsiString = interface(IPersist)
    procedure SetText(const text: PChar); stdcall;
    function GetText(): PChar; stdcall;
    function GetSize(): Longword; stdcall;
end;

type IStringArray = interface(IPersist)
    function Add(const text: PChar): Boolean; stdcall;
    function SetAt(const index: Longword; const text: PChar): Boolean; stdcall;
    function GetAt(const index: Longword; out ppString: IAnsiString): Boolean; stdcall;
    procedure Clear(); stdcall;
    function Resize(const newsize: Longword): Boolean; stdcall;
    function GetSize(): Longword; stdcall;
end;

type IStringMapObj = interface(IObj)
    function _Set(const key: PChar; const pValue: IObj): Boolean; stdcall;
    function _Get(const key: PChar; out ppValue: IObj): Boolean; stdcall;
    function GetAt(const index: Longword; out ppKey: IAnsiString; out ppValue: IObj): Boolean; stdcall;
    procedure GetKeys(out ppKeys: IStringArray); stdcall;
    procedure GetValues(out ppValues: IObjArray); stdcall;
    procedure Clear(); stdcall;
    function GetSize(): Longword; stdcall;
end;

function GotoInterface(const source: IObj; const interfacename: PChar): Pointer;

implementation

function GotoInterface(const source: IObj; const interfacename: PChar): Pointer;
var
    p: Pointer;
begin
    p := nil;
    source._Interface(interfacename, p);
    if (Assigned(p)) then source._Release();
    Result := p;
end;

end.
