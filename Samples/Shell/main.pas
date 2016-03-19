unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, NovusShell;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
Var
  loShell: TNovusShell;
begin
  Try
    Try
      loShell := TNovusShell.Create;

      if not loShell.RunCommand('notepad.exe') then
         ShowMessage('Error')
      else
        Showmessage('finish app');
    Except




    End;
  Finally

    loShell.Free;
  End;







end;

procedure TForm1.Button2Click(Sender: TObject);
Var
  loShell: TNovusShell;
begin
  Try
    Try
      loShell := TNovusShell.Create;

      if not loShell.RunCommand('cmd.exe') then
         ShowMessage('Error')
      else
        Showmessage('finish app');
    Except




    End;
  Finally

    loShell.Free;
  End;
end;

end.
