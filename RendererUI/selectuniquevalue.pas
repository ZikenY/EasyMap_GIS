unit selectuniquevalue;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TFormSelectUniqueValue = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    ListView1: TListView;
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }

    m_OK: Boolean;
  end;

implementation

{$R *.DFM}

procedure TFormSelectUniqueValue.FormShow(Sender: TObject);
begin
    m_OK := False;
end;

procedure TFormSelectUniqueValue.Button2Click(Sender: TObject);
begin
    m_OK := True;
    Self.Close();
end;

procedure TFormSelectUniqueValue.Button1Click(Sender: TObject);
begin
    Self.Close();
end;

procedure TFormSelectUniqueValue.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin                
    Action := caHide;
end;

end.
