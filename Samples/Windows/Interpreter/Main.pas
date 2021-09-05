unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, NovusInterpreter;

type
  TMainForm = class(TForm)
    Memo: TMemo;
    ParseButton: TButton;
    LexStringGrid: TStringGrid;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure ResetUI;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation


procedure TMainForm.ResetUI;
begin
  LexStringGrid.Cells[0,0] := 'Value';
  LexStringGrid.Cells[1,0] := 'Line Number';
  LexStringGrid.Cells[2,0] := 'Column Number';
end;

{$R *.dfm}

procedure TMainForm.FormShow(Sender: TObject);
begin
  ResetUI;
end;

end.
