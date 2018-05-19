unit InterfaceSymbol;

interface

uses
    WKSStructs, InterfaceObj, InterfaceDisplay, Windows;

type TPointFillStyle = Longint;
const POINTFILLSTYLE_REGULAR    =  TPointFillStyle(1);
const POINTFILLSTYLE_LABELPOINT =  TPointFillStyle(2);

type ISimplePointSymbol = interface(IPointSymbol)
    function SetDiameter(const diameter: Double): Boolean; stdcall;
    function GetDiameter(out diameter: Double): Boolean; stdcall;
    function SetLineWidth(const width: Double): Boolean; stdcall;
    function GetLineWidth(out width: Double): Boolean; stdcall;
    procedure SetSolid(const solid: Boolean); stdcall;
    procedure GetSolid(out solid: Boolean); stdcall;
end;

type IEnvelopePointSymbol = interface(IPointSymbol)
    function SetWidth(const width: Double): Boolean; stdcall;
    function GetWidth(out width: Double): Boolean; stdcall;
    function SetHeight(const height: Double): Boolean; stdcall;
    function GetHeight(out height: Double): Boolean; stdcall;
    function SetLineWidth(const width: Double): Boolean; stdcall;
    function GetLineWidth(out width: Double): Boolean; stdcall;
    procedure SetSolid(const solid: Boolean); stdcall;
    procedure GetSolid(out solid: Boolean); stdcall;
end;

type IPolyPointSymbol = interface(IPointSymbol)
    function AddNode(var node: WKSPoint): Boolean; stdcall;
    function MoveNode(var node: WKSPoint; const index: Longword): Boolean; stdcall;
    function DeleteNode(const index: Longword): Boolean; stdcall;
    function GetNode(const index: Longword; out node: WKSPoint): Boolean; stdcall;
    function SetNode(const index: Longword; var node: WKSPoint): Boolean; stdcall;
    function SetNodeOrder(const index: Longword; const neworder: Longword): Boolean; stdcall;
    function GetNodeCount(): Longword; stdcall;
    procedure ClearNodes(); stdcall;
    function SetLineWidth(const width: Double): Boolean; stdcall;
    function GetLineWidth(out width: Double): Boolean; stdcall;
    procedure SetSolid(const solid: Boolean); stdcall;
    procedure GetSolid(out solid: Boolean); stdcall;
end;

type ILineSimpleTemplate = interface(IPersist)
    function SetFirstMark(const mark: Boolean): Boolean; stdcall;
    function GetFirstMark(out mark: Boolean): Boolean; stdcall;
    function AddSector(const sector: Longword): Boolean; stdcall;
    function GetSector(const index: Longword; out sector: Longword): Boolean; stdcall;
    function GetCount(out count: Longword): dword; stdcall;
    function Clear(): Boolean; stdcall;
    function SetFactor(const factor: Double): Boolean; stdcall;
    function GetFactor(out factor: Double): Boolean; stdcall;
end;

type ISimpleLineSymbol = interface(ILineSymbol)
    function SetWidth(const width: Double): Boolean; stdcall;
    function GetWidth(out width: Double): Boolean; stdcall;
    function SetTemplate(const pTemplate: ILineSimpleTemplate): Boolean; stdcall;
    function GetTemplate(out ppTemplate: ILineSimpleTemplate): Boolean; stdcall;
end;

type IPointLineSymbol = interface(ILineSymbol)
    function SetPointSymbol(const pPointSymbol: IPointSymbol): Boolean; stdcall;
    function GetPointSymbol(out ppPointSymbol: IPointSymbol): Boolean; stdcall;
    function SetTemplate(const pTemplate: ILineSimpleTemplate): Boolean; stdcall;
    function GetTemplate(out ppTemplate: ILineSimpleTemplate): Boolean; stdcall;
end;

type ISimpleFillSymbol = interface(IFillSymbol)
    function SetFillStyle(const style: Longint): Boolean; stdcall;
    function GetFillStyle(out style: Longint): Boolean; stdcall;
    function SetFillHatch(const hatch: Longint): Boolean; stdcall;
    function GetFillHatch(out hatch: Longint): Boolean; stdcall;
    function SetBorderColor(const color: COLORREF): Boolean; stdcall;
    function GetBorderColor(out color: COLORREF): Boolean; stdcall;
    function SetBorderWidth(width: Double): Boolean; stdcall;
    function GetBorderWidth(out width: Double): Boolean; stdcall;
end;

type IPointFillSymbol = interface(IFillSymbol)
    function SetPointSymbol(const pPointSymbol: IPointSymbol): Boolean; stdcall;
    function GetPointSymbol(out ppPointSymbol: IPointSymbol): Boolean; stdcall;

    function SetPointsSpace(const space_x: Double; const space_y: Double): Boolean; stdcall;
    function GetPointsSpace(out space_x: Double; out space_y: Double): Boolean; stdcall;

    function SetPointsOffset(const offset_x: Double; const offset_y: Double): Boolean; stdcall;
    function GetPointsOffset(out offset_x: Double; out offset_y: Double): Boolean; stdcall;

    function SetFillStyle(const fillstyle: TPointFillStyle): Boolean; stdcall;
    function GetFillStyle(out fillstyle: TPointFillStyle): Boolean; stdcall;

    function SetBorderSymbol(const pLineSymbol: ILineSymbol): Boolean; stdcall;
    function GetBorderSymbol(out ppLineSymbol: ILineSymbol): Boolean; stdcall;
end;

type ISimpleTextSymbol = interface(ITextSymbol)
end;

type IMultiPointSymbol = interface(IPointSymbol)
    function AddSymbol(const pSymbol: IPointSymbol): Boolean; stdcall;
    function AddSimpleSymbol(const color: COLORREF; const diameter: Double): Boolean; stdcall;
    function SetSymbolRef(const pSymbol: IPointSymbol; const index: Longword): Boolean; stdcall;
    function RemoveSymbol(const index: Longword): Boolean; stdcall;
    function GetSymbolRef(out ppSymbol: IPointSymbol; const index: Longword): Boolean; stdcall;
    function SetSymbolOrder(const pSymbol: IPointSymbol; const neworder: Longword): Boolean; stdcall;
    function GetSymbolCount(): Longword; stdcall;
    procedure ClearSymbols(); stdcall;
    function SetSize(const size: Double): Boolean; stdcall;
    function GetSize(out size: Double): Boolean; stdcall;
end;

type IMultiLineSymbol = interface(ILineSymbol)
    function AddSymbol(const pSymbol: ILineSymbol): Boolean; stdcall;
    function AddSimpleSymbol(const color: COLORREF; const linewidth: Double): Boolean; stdcall;
    function SetSymbolRef(const pSymbol: ILineSymbol; const index: Longword): Boolean; stdcall;
    function RemoveSymbol(const index: Longword): Boolean; stdcall;
    function GetSymbolRef(out ppSymbol: ILineSymbol; const index: Longword): Boolean; stdcall;
    function SetSymbolOrder(const pSymbol: ILineSymbol; const neworder: Longword): Boolean; stdcall;
    function GetSymbolCount(): Longword; stdcall;
    procedure ClearSymbols(); stdcall;
    function SetSize(const size: Double): Boolean; stdcall;
    function GetSize(out size: Double): Boolean; stdcall;
end;

type IMultiFillSymbol = interface(IFillSymbol)
    function AddSymbol(const pSymbol: IFillSymbol): Boolean; stdcall;
    function AddSimpleSymbol(const color: COLORREF): Boolean; stdcall;
    function SetSymbolRef(const pSymbol: IFillSymbol; const index: Longword): Boolean; stdcall;
    function RemoveSymbol(const index: Longword): Boolean; stdcall;
    function GetSymbolRef(out ppSymbol: IFillSymbol; const index: Longword): Boolean; stdcall;
    function SetSymbolOrder(const pSymbol: IFillSymbol; const neworder: Longword): Boolean; stdcall;
    function GetSymbolCount(): Longword; stdcall;
    procedure ClearSymbols(); stdcall;
end;

type ISymbolLib = interface(IPersist)
    function AddSymbol(const pSymbol: ISymbol): Boolean; stdcall;
    function SetSymbolRef(const pSymbol: ISymbol; const index: Longword): Boolean; stdcall;
    function GetSymbolRef(out ppSymbol: ISymbol; const index: Longword): Boolean; stdcall;
    function SetSymbolOrder(const pSymbol: ISymbol; const neworder: Longword): Boolean; stdcall;
    function RemoveSymbol(const index: Longword): Boolean; stdcall;
    function ClearSymbols(): Boolean; stdcall;
    function GetSymbolCount(): Longword; stdcall;
    function SetDesc(const desc: PChar): Boolean; stdcall;
    function GetDesc(): PChar; stdcall;
    function SaveToFile(const filename: PChar): Boolean; stdcall;
    function LoadFromFile(const filename: PChar): Boolean; stdcall;
end;

implementation

end.
