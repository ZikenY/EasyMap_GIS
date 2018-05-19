unit InterfaceActiveView;

interface

uses
    InterfaceObj, InterfaceLayer, InterfaceDisplay, InterfaceTrackCancel,
    WKSStructs, windows;

type IActiveView = interface(IPersist)
    function SetDisplay(const pDisplay: IDisplay): Boolean; stdcall;
    function GetDisplay(out ppDisplay: IDisplay): Boolean; stdcall;
    function GainFocus(const dc: HDC; var rect: TRect): Boolean; stdcall;
    function LostFocus(): Boolean; stdcall;
    function IsFocused(): Boolean; stdcall;
    function DrawData(pDisplay: IDisplay; const pEnvelope: PWKSRect;
        const pTrackCancel: ITrackCancel): TDrawResult; stdcall;
    function DrawSelection(const pEnvelope: PWKSRect; const pTrackCancel: ITrackCancel
        ): TDrawResult; stdcall; overload;
    function DrawSelectionEx(pDisplay: IDisplay; const cacheid: Longint;
        const pEnvelope: PWKSRect; const pTrackCancel: ITrackCancel): TDrawResult; stdcall; overload;
    procedure RefreshWindow(); stdcall;
    function EraseView(): Boolean; stdcall;
    function EraseSelectionView(): Boolean; stdcall;
    function UpdateData(const pTrackCancel: ITrackCancel): TDrawResult; stdcall;
    function UpdateSelection(const pTrackCancel: ITrackCancel): TDrawResult; stdcall;
    procedure DrawingHint(const visible: Boolean); stdcall;
end;

implementation

end.
