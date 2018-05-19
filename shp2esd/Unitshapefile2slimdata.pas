unit Unitshapefile2slimdata;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  easylib, InterfaceObj, InterfaceGeometry, InterfaceLayer, InterfaceLayerAgent,
  InterfaceDisplayTransformation, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    EditOutDir: TEdit;
    Button3: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    EditScale: TEdit;
    Label2: TLabel;
    EditPrecision: TEdit;
    Label3: TLabel;
    CBUnits: TComboBox;
    Label4: TLabel;
    CBLevel: TComboBox;
    Label5: TLabel;
    OpenDialog1: TOpenDialog;
    BtnDo: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BtnDoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
    CBUnits.ItemIndex := 0;
    CBLevel.ItemIndex := 5;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
    i, j: Longint;
    flag: Boolean;
begin
    if (not OpenDialog1.Execute()) then begin
        Exit;
    end;

    if (OpenDialog1.Files.Count <= 0) then begin
        Exit;
    end;

    flag := false;
    for i := 0 to OpenDialog1.Files.Count - 1 do begin
        for j := 0 to ListBox1.Items.Count - 1 do begin
            if (Trim(ListBox1.Items[j]) = Trim(OpenDialog1.Files[i])) then begin
                flag := True;
                break;
            end;
        end;

        if (flag) then begin
            flag := False;
        end
        else begin
            ListBox1.Items.Add(OpenDialog1.Files[i]);
        end;
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    ListBox1.Clear();
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
    ShowMessage('请手工输入吧～～～～');
end;

procedure TForm1.BtnDoClick(Sender: TObject);
var
    i, j, itemsleft: Longint;
    shpfilename: AnsiString;
    pLayer: ILayer;
    mapunit: TMapUnits;
    scale, precision: Double;
    spatialindexlevel: Longint;
    pObj: IObj;
    pLA: IVectorLayerAgent;
    outdir, esd_nodir, esd: AnsiString;
    filelist: TStringList;
begin
//
    if (ListBox1.Items.Count <= 0) then begin
        ShowMessage('选择文件先');
        Exit;
    end;

    outdir := Trim(EditOutDir.Text) + '\';

    if (CBUnits.ItemIndex = 0) then begin
        mapunit := UNIT_M;
    end
    else begin
        mapunit := UNIT_DEGREE;
    end;

    scale := StrToFloat(EditScale.Text);
    precision := StrToFloat(EditPrecision.Text);
    spatialindexlevel := CBLevel.ItemIndex;

    filelist := TStringList.Create();
    filelist.Text := ListBox1.Items.Text;

    Self.Caption := '转换中，请等待。。。';
    Self.Refresh();

    for i := 0 to filelist.Count - 1 do begin
        shpfilename := filelist[i];
        pLayer := nil;
        LoadShapeFile(PChar(shpfilename), pLayer, mapunit, scale,
            precision, spatialindexlevel, True);

        if (not Assigned(pLayer)) then begin
            Continue;
        end;

        esd_nodir := pLayer.GetName();

        CreateObj('CVectorLayerAgent', pObj);
        pLA := IVectorLayerAgent(GotoInterface(pObj, 'IVectorLayerAgent'));
        pLA.SetLayer(pLayer);

        esd := outdir + esd_nodir + '.esd';
        if (pLA.Shapefile2ESD(PChar(esd))) then begin
            itemsleft := ListBox1.Items.Count;
            for j := 0 to itemsleft - 1 do begin
                if (shpfilename = ListBox1.Items[j]) then begin
                    ListBox1.Items.Delete(j);
                    Break;
                end;
            end;
        end;

        Self.Refresh();
    end;

    Self.Caption := 'shp2esd';
    Self.Refresh();

    if (ListBox1.Items.Count > 0) then begin
        ShowMessage('有文件转换失败，请检查。');
    end
    else begin
        ShowMessage('搞定。');
    end;
end;

initialization
    if (not InitialEasyLib('easylib.dll')) then begin
        MessageBox(0, 'no easylib.dll', '', MB_OK);
        Application.Terminate();
    end;

finalization
    if (0 <> easylibmodule) then
        FreeLibrary(easylibmodule);

    FreeEasyLib();

end.
