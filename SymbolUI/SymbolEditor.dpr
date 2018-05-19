program SymbolEditor;

uses
  Forms,
  MainForm in 'MainForm.pas' {FormMain},
  MathLib in 'MathLib.pas',
  SubSymbolAttribForm in 'SubSymbolAttribForm.pas' {SubSymbolAttrib},
  NewSymbolForm in 'NewSymbolForm.pas' {NewSymbol},
  LineTemplate in 'LineTemplate.pas' {FormLineTemplate},
  SymbolSelector in 'SymbolSelector.pas' {FormSymbolSelector},
  easylib in '..\Delphi SDK\easylib.pas',
  InterfaceDisplay in '..\Delphi SDK\InterfaceDisplay.pas',
  InterfaceFields in '..\Delphi SDK\InterfaceFields.pas',
  InterfaceGeometry in '..\Delphi SDK\InterfaceGeometry.pas',
  InterfaceLayer in '..\Delphi SDK\InterfaceLayer.pas',
  InterfaceLayerAgent in '..\Delphi SDK\InterfaceLayerAgent.pas',
  InterfaceObj in '..\Delphi SDK\InterfaceObj.pas',
  InterfaceSymbol in '..\Delphi SDK\InterfaceSymbol.pas',
  InterfaceTrackCancel in '..\Delphi SDK\InterfaceTrackCancel.pas',
  yyspace in 'yyspace.pas',
  InterfaceDisplayTransformation in '..\Delphi SDK\InterfaceDisplayTransformation.pas',
  WKSStructs in '..\Delphi SDK\WKSStructs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
