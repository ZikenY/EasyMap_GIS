unit InterfaceTrackCancel;

interface

uses InterfaceObj;

type ITrackCancel = interface(IPersist)
    function CheckCancel(): Boolean; stdcall;
    procedure PostProgress(const cacheid: Longint); stdcall;
    procedure ShowHint(const cacheid: Longint); stdcall;
end;

implementation

end.
