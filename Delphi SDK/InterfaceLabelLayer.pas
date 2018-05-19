unit InterfaceLabelLayer;

interface

uses
    InterfaceObj, InterfaceDisplayTransformation, InterfaceDisplay, InterfaceLayer,
    InterfaceTrackCancel, WKSStructs, Windows;

type ILabelLayer = interface(ILayer)
    //  必须是SlimLayer或者ShapeLayer
    function SetVectorLayer(const pLayer: ILayer): Boolean; stdcall;
    function GetVectorLayer(out ppLayer: ILayer): Boolean; stdcall;

    //  标注字段设置
    function SetFieldIndex(const fieldindex: Longint): Boolean; stdcall;
    function GetFieldIndex(): Longint; stdcall;

    //  注记符号
    function SetTextSymbol(const pTextSymbol: ITextSymbol): Boolean; stdcall;
    function GetTextSymbol(out ppTextSymbol: ITextSymbol): Boolean; stdcall;

    //  参考比例尺
    function SetRefScale(const scale: Double): Boolean; stdcall;
    function GetRefScale(): Double; stdcall;

    //  取出可视范围内的标注内容
    function GetLabelText(out ppTextPositions: IDoubleArray; out ppLabelTexts: IStringArray;
        const pDT: IDisplayTransformation; const pTrackCancel: ITrackCancel;
        var visibleextent: WKSRect): TDrawResult; stdcall;

    //  清空缓存内容，用于和数据图层同步
    procedure ClearCache(); stdcall;
end;

type ILabelLayerManager = interface(IPersist)
    //  管理一组LabelLayer
    function AddLabelLayer(const pLabelLayer: ILabelLayer): Boolean; stdcall;
    function RemoveLabelLayer(const index: Longword): Boolean; stdcall;
    function RemoveLabelLayerEx(pLabelLayer: ILabelLayer): Boolean; stdcall;
    function GetLabelLayer(out ppLabelLayer: ILabelLayer; const index: Longword): Boolean; stdcall;
    function SetLabelLayerOrder(const pLabelLayer: ILabelLayer; const neworder: Longword): Boolean; stdcall;
    procedure ClearLabelLayers(); stdcall;
    function GetLabelLayerCount(): Longword; stdcall;

    //  是否避让
    procedure SetTextAvoidable(const avoidable: Boolean); stdcall;
    function GetTextAvoidable(): Boolean; stdcall;

    //  绘制标注
    function DrawLabels(const pDisplay: IDisplay; const pEnvelope: PWKSRect;
        const pTrackCancel: ITrackCancel): TDrawResult; stdcall;

    //  是否显示自动标注
    procedure EnableLabelDraw(Enable: Boolean); stdcall;
    function LabelDrawEnabled(): Boolean; stdcall;
end;

implementation

end.
