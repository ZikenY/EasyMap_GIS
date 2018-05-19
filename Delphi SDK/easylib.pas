unit easylib;

interface

uses
    Windows, InterfaceObj, InterfaceGeometry, InterfaceDisplayTransformation, InterfaceDisplay,
    InterfaceSymbol, InterfaceLayer, InterfaceFields, WKSStructs;

type
    //反序列化
    TInstantiate = function(pStream: IStreamX; out pPersist: IPersist): Longword; stdcall;

    //创建对象
    TCreateObj = procedure(classname: PChar; out obj: IObj); stdcall;

    //-------------------------------------------------------------------------------------
    //  为了方便
    //-------------------------------------------------------------------------------------
    TCreateGeometry = function(const geometrytype: TGeometryType;
        out ppGeometry: IGeometry): Boolean; stdcall;
    TCreateSimplePointSymbol = function(const diameter: Double; const color: COLORREF;
        out ppSimplePointSymbol: ISimplePointSymbol): Boolean; stdcall;
    TCreateEnvelopePointSymbol = function(const width: Double; const height: Double; const color: COLORREF;
        out ppEnvelopePointSymbol: IEnvelopePointSymbol): Boolean; stdcall;
    TCreatePolyPointSymbol = function(const solid: Boolean; const linewidth: Double;
        const color: COLORREF; out ppPolyPointSymbol: IPolyPointSymbol): Boolean; stdcall;
    TCreateSimpleLineSymbol = function(const width: Double; const color: COLORREF;
        out ppSimpleLineSymbol: ISimpleLineSymbol): Boolean; stdcall;
    TCreatePointLineSymbol = function(out ppPointLineSymbol: IPointLineSymbol): Boolean; stdcall;
    TCreateSimpleFillSymbol = function(const color: COLORREF; const borderwidth: Double;
        const bordercolor: COLORREF; out ppSimpleFillSymbol: ISimpleFillSymbol): Boolean; stdcall;
    TCreatePointFillSymbol = function(out ppPointFillSymbol: IPointFillSymbol;
        const color: COLORREF; const spacex: Double; const spacey: Double): Boolean; stdcall;
    TCreateSimpleTextSymbol = function(const width: Double; const height: Double; text: PChar;
        const textcolor: COLORREF; out ppSimpleTextSymbol: ISimpleTextSymbol): Boolean; stdcall;
    TCreateMultiPointSymbol = function(out ppMultiPointSymbol: IMultiPointSymbol): Boolean; stdcall;
    TCreateMultiLineSymbol = function(out ppMultiLineSymbol: IMultiLineSymbol): Boolean; stdcall;
    TCreateMultiFillSymbol = function(out ppMultiFillSymbol: IMultiFillSymbol): Boolean; stdcall;

    TCreateSlimData = function(const filename: PChar; out pLayer: ILayer; const mapunit: TMapUnits;
        const basescale: Double; precision: Double; indexlevel: Longint; const shapetype: TShapeType;
        const extent: WKSRect; const pFields: IFields; const anno: Boolean; const filemap: Boolean): Boolean; stdcall;
    TLoadSlimData = function(const filename: PChar; out pLayer: ILayer; readonly: Boolean;
        filemap: Boolean): Boolean; stdcall;
    TLoadShapeFile = function(const filename: PChar; out pLayer: ILayer; const mapunit: TMapUnits;
        const basescale: Double; precision: Double; indexlevel: Longint; readonly: Boolean): Boolean; stdcall;
    //-------------------------------------------------------------------------------------

function InitialEasyLib(const easyfile: PChar): Boolean;
function FreeEasyLib(): Boolean;
function LoadSymbolLib(const filename: PChar; pSymbolLib: ISymbolLib): Boolean; overload;
function LoadSymbolLib(const filename: PChar; pSymbolLib: ISymbolLib;
    const symboltype: TSymbolType): Boolean; overload;

var
    easylibmodule: HMODULE;

    Instantiate: TInstantiate;
    CreateObj: TCreateObj;
    CreateGeometry: TCreateGeometry;
    CreateSimplePointSymbol: TCreateSimplePointSymbol;
    CreateEnvelopePointSymbol: TCreateEnvelopePointSymbol;
    CreatePolyPointSymbol: TCreatePolyPointSymbol;
    CreateSimpleLineSymbol: TCreateSimpleLineSymbol;
    CreatePointLineSymbol: TCreatePointLineSymbol;
    CreateSimpleFillSymbol: TCreateSimpleFillSymbol;
    CreatePointFillSymbol: TCreatePointFillSymbol;
    CreateSimpleTextSymbol: TCreateSimpleTextSymbol;
    CreateMultiPointSymbol: TCreateMultiPointSymbol;
    CreateMultiLineSymbol: TCreateMultiLineSymbol;
    CreateMultiFillSymbol: TCreateMultiFillSymbol;
    CreateSlimData: TCreateSlimData;
    LoadSlimData: TLoadSlimData;
    LoadShapeFile: TLoadShapeFile;

implementation

function InitialEasyLib(const easyfile: PChar): Boolean;
begin
    Result := False;
    easylibmodule := LoadLibrary(easyfile);
    if (0 = easylibmodule) then
    begin
        Exit;
    end;

    Instantiate                 := TInstantiate(GetProcAddress(easylibmodule, 'Instantiate'));
    CreateObj                   := TCreateObj(GetProcAddress(easylibmodule, 'CreateObj'));
    CreateGeometry              := TCreateGeometry(GetProcAddress(easylibmodule, 'CreateGeometry'));
    CreateSimplePointSymbol     := TCreateSimplePointSymbol(GetProcAddress(easylibmodule, 'CreateSimplePointSymbol'));
    CreateEnvelopePointSymbol   := TCreateEnvelopePointSymbol(GetProcAddress(easylibmodule, 'CreateEnvelopePointSymbol'));
    CreatePolyPointSymbol       := TCreatePolyPointSymbol(GetProcAddress(easylibmodule, 'CreatePolyPointSymbol'));
    CreateSimpleLineSymbol      := TCreateSimpleLineSymbol(GetProcAddress(easylibmodule, 'CreateSimpleLineSymbol'));
    CreatePointLineSymbol       := TCreatePointLineSymbol(GetProcAddress(easylibmodule, 'CreatePointLineSymbol'));
    CreateSimpleFillSymbol      := TCreateSimpleFillSymbol(GetProcAddress(easylibmodule, 'CreateSimpleFillSymbol'));
    CreatePointFillSymbol       := TCreatePointFillSymbol(GetProcAddress(easylibmodule, 'CreatePointFillSymbol'));
    CreateSimpleTextSymbol      := TCreateSimpleTextSymbol(GetProcAddress(easylibmodule, 'CreateSimpleTextSymbol'));
    CreateMultiPointSymbol      := TCreateMultiPointSymbol(GetProcAddress(easylibmodule, 'CreateMultiPointSymbol'));
    CreateMultiLineSymbol       := TCreateMultiLineSymbol(GetProcAddress(easylibmodule, 'CreateMultiLineSymbol'));
    CreateMultiFillSymbol       := TCreateMultiFillSymbol(GetProcAddress(easylibmodule, 'CreateMultiFillSymbol'));
    LoadSlimData                := TLoadSlimData(GetProcAddress(easylibmodule, 'LoadSlimData'));
    LoadShapeFile               := TLoadShapeFile(GetProcAddress(easylibmodule, 'LoadShapeFile'));

    Result := True;
end;

function FreeEasyLib(): Boolean;
begin
    Result := False;
    if (0 = easylibmodule) then begin
        Exit;
    end;
    
    FreeLibrary(easylibmodule);
    Result := True;
end;

function LoadSymbolLib(const filename: PChar; pSymbolLib: ISymbolLib): Boolean;
var
    FindFileData: WIN32_FIND_DATA;
    pObj: IObj;
    pSL: ISymbolLib;
    pSymbol: ISymbol;
    i, n: Longint;
    desc: AnsiString;
begin
    Result := False;

    if (not Assigned(pSymbolLib)) then begin
        Exit;
    end;

    if (FindFirstFile(filename, FindFileData) = INVALID_HANDLE_VALUE) then begin
        Exit;
    end;

    CreateObj('CSymbolLib', pObj);
    pSL := ISymbolLib(GotoInterface(pObj, 'ISymbolLib'));
    pSL.LoadFromFile(filename);

    pSymbolLib.ClearSymbols();
    desc := pSL.GetDesc();
    pSymbolLib.SetDesc(PChar(desc));
    n := pSL.GetSymbolCount();
    for i := 0 to n - 1 do begin
        pSL.GetSymbolRef(pSymbol, i);
        pSymbolLib.AddSymbol(pSymbol);
    end;

    Result := True;
end;

function LoadSymbolLib(const filename: PChar; pSymbolLib: ISymbolLib;
    const symboltype: TSymbolType): Boolean;
var
    pObj: IObj;
    pSL: ISymbolLib;
    pSymbol: ISymbol;
    i, n: Longint;
begin
    CreateObj('CSymbolLib', pObj);
    pSL := ISymbolLib(GotoInterface(pObj, 'ISymbolLib'));
    Result := LoadSymbolLib(filename, pSL);
    if (not Result) then Exit;

    pSymbolLib.ClearSymbols();
    n := pSL.GetSymbolCount();
    for i := 0 to n - 1 do begin
        pSL.GetSymbolRef(pSymbol, i);
        if (pSymbol.GetSymbolType() <> symboltype) then begin
            Continue;
        end;

        pSymbolLib.AddSymbol(pSymbol);
    end;

    Result := True;
end;

end.
