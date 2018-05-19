unit InterfaceTracker;

interface

uses
    InterfaceObj, InterfaceGeometry, InterfaceDisplay;

type IGeometryTracker = interface(IObj)
    function SetDisplay(const pDisplay: IDisplay): Boolean; stdcall;
    function GetDisplay(out ppDisplay: IDisplay): Boolean; stdcall;
    function ClearDisplay(): Boolean; stdcall;
    function SetSymbol(const pSymbol: ISymbol): Boolean; stdcall;
    function GetSymbol(const symtype: TSymbolType; out ppSymbol: ISymbol): Boolean; stdcall;
    function Start(const X: Longint; const Y: Longint): Boolean; stdcall;
    function Finish(): Boolean; stdcall;
    function Started(): Boolean; stdcall;
    function MouseDown(const X: Longint; const Y: Longint): Boolean; stdcall;
    function MouseMove(const X: Longint; const Y: Longint): Boolean; stdcall;
    function Resize(): Boolean; stdcall;
    function Refresh(): Boolean; stdcall;
end;

type IMoveTracker = interface(IGeometryTracker)
    function AddGeometryRef(const pGeo: IGeometry): Boolean; stdcall;
    function NextGeometryRef(out ppGeo: IGeometry): Boolean; stdcall;
    procedure ResetIterator(); stdcall;
    function GetGeometryCount(): Longword; stdcall;
    procedure ClearGeometry() stdcall;
end;

implementation


end.
