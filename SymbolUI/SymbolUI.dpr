library SymbolUI;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
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
  MainForm in 'MainForm.pas' {FormMain},
  InterfaceDisplayTransformation in '..\Delphi SDK\InterfaceDisplayTransformation.pas',
  WKSStructs in '..\Delphi SDK\WKSStructs.pas';

{$R *.RES}

exports SetMainWnd;
exports SelectSymbol;

begin
end.
