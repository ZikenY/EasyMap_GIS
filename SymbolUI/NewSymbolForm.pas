unit NewSymbolForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TNewSymbol = class(TForm)
    BitBtn4: TBitBtn;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    select: Longint;
  end;

implementation

{$R *.DFM}

procedure TNewSymbol.FormShow(Sender: TObject);
begin
    select := 0;
end;

procedure TNewSymbol.BitBtn4Click(Sender: TObject);
begin
    Self.Close();
end;

procedure TNewSymbol.BitBtn1Click(Sender: TObject);
begin
    select := 1;
    Self.Close();
end;

procedure TNewSymbol.BitBtn2Click(Sender: TObject);
begin
    select := 2;
    Self.Close();
end;

procedure TNewSymbol.BitBtn3Click(Sender: TObject);
begin
    select := 3;
    Self.Close();
end;

end.
