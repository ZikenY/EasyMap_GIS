program NavigationDemo;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {FormMain},
  easylib in '..\Delphi SDK\easylib.pas',
  InterfaceActiveView in '..\Delphi SDK\InterfaceActiveView.pas',
  InterfaceBitmapLayer in '..\Delphi SDK\InterfaceBitmapLayer.pas',
  InterfaceDisplay in '..\Delphi SDK\InterfaceDisplay.pas',
  InterfaceDisplayTransformation in '..\Delphi SDK\InterfaceDisplayTransformation.pas',
  InterfaceFields in '..\Delphi SDK\InterfaceFields.pas',
  InterfaceGeometry in '..\Delphi SDK\InterfaceGeometry.pas',
  InterfaceLabelLayer in '..\Delphi SDK\InterfaceLabelLayer.pas',
  InterfaceLayer in '..\Delphi SDK\InterfaceLayer.pas',
  InterfaceLayerAgent in '..\Delphi SDK\InterfaceLayerAgent.pas',
  InterfaceMap in '..\Delphi SDK\InterfaceMap.pas',
  InterfaceObj in '..\Delphi SDK\InterfaceObj.pas',
  InterfaceSymbol in '..\Delphi SDK\InterfaceSymbol.pas',
  InterfaceTrackCancel in '..\Delphi SDK\InterfaceTrackCancel.pas',
  InterfaceTracker in '..\Delphi SDK\InterfaceTracker.pas',
  WKSStructs in '..\Delphi SDK\WKSStructs.pas',
  UnitNavigating in 'UnitNavigating.pas' {FormNavigating};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
