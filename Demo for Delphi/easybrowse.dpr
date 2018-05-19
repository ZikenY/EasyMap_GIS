program easybrowse;

uses
  Forms,
  mainform in 'mainform.pas' {FormMain},
  easylib in '..\Delphi SDK\easylib.pas',
  InterfaceBitmapLayer in '..\Delphi SDK\InterfaceBitmapLayer.pas',
  InterfaceDisplay in '..\Delphi SDK\InterfaceDisplay.pas',
  InterfaceFields in '..\Delphi SDK\InterfaceFields.pas',
  InterfaceGeometry in '..\Delphi SDK\InterfaceGeometry.pas',
  InterfaceLayer in '..\Delphi SDK\InterfaceLayer.pas',
  InterfaceLayerAgent in '..\Delphi SDK\InterfaceLayerAgent.pas',
  InterfaceObj in '..\Delphi SDK\InterfaceObj.pas',
  InterfaceSymbol in '..\Delphi SDK\InterfaceSymbol.pas',
  InterfaceTrackCancel in '..\Delphi SDK\InterfaceTrackCancel.pas',
  InterfaceMap in '..\Delphi SDK\InterfaceMap.pas',
  InterfaceActiveView in '..\Delphi SDK\InterfaceActiveView.pas',
  InterfaceTracker in '..\Delphi SDK\InterfaceTracker.pas',
  InterfaceDisplayTransformation in '..\Delphi SDK\InterfaceDisplayTransformation.pas',
  InterfaceLabelLayer in '..\Delphi SDK\InterfaceLabelLayer.pas',
  WKSStructs in '..\Delphi SDK\WKSStructs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
