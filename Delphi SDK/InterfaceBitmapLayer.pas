unit InterfaceBitmapLayer;

interface

uses
    InterfaceDisplayTransformation, InterfaceLayer;

type TDOMInfo = record
    mapunit: TMapUnits;
    Xr: Double;
    Yc: Longword;
    Dr: Longword;
    Dc: Double;
end;

type IBitmapLayer = interface(ILayer)
    function LoadBmpFile(const filename: PChar; const loaddom: Boolean): Boolean; stdcall;
    function GetBmpFileName(): PChar; stdcall;
    procedure SetDomInfo(var dominfo: TDOMInfo); stdcall;
    procedure GetDomInfo(out dominfo: TDOMInfo); stdcall;
end; 

implementation

end.
