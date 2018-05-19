program shp2esd;

uses
  Forms,
  Unitshapefile2slimdata in 'Unitshapefile2slimdata.pas' {Form1},
  InterfaceFields in '..\Delphi SDK\InterfaceFields.pas',
  InterfaceGeometry in '..\Delphi SDK\InterfaceGeometry.pas',
  InterfaceLayer in '..\Delphi SDK\InterfaceLayer.pas',
  InterfaceLayerAgent in '..\Delphi SDK\InterfaceLayerAgent.pas',
  InterfaceObj in '..\Delphi SDK\InterfaceObj.pas',
  easylib in '..\Delphi SDK\easylib.pas',
  InterfaceActiveView in '..\Delphi SDK\InterfaceActiveView.pas',
  InterfaceBitmapLayer in '..\Delphi SDK\InterfaceBitmapLayer.pas',
  InterfaceDisplay in '..\Delphi SDK\InterfaceDisplay.pas',
  InterfaceMap in '..\Delphi SDK\InterfaceMap.pas',
  InterfaceSymbol in '..\Delphi SDK\InterfaceSymbol.pas',
  InterfaceTrackCancel in '..\Delphi SDK\InterfaceTrackCancel.pas',
  InterfaceTracker in '..\Delphi SDK\InterfaceTracker.pas',
  InterfaceDisplayTransformation in '..\Delphi SDK\InterfaceDisplayTransformation.pas',
  WKSStructs in '..\Delphi SDK\WKSStructs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
