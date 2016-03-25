unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, NovusShell;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo: TMemo;
    btnCmdDir: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);

    procedure btnCmdDirClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnCmdDirClick(Sender: TObject);
const
  CR = #13#10;
Var
  loShell: TNovusShell;
  lsOutput: String;
  lsError: String;
begin
  Try
    Try
      loShell := TNovusShell.Create;

      if not loShell.RunRedirectCommand('Cmd.exe', 'Dir'+CR+'Exit'+CR, lsOutput,lsError ) then
        begin
          Memo.Lines.Add(lsError);
          ShowMessage('Error')
        end
      else
        begin
          Memo.Lines.Add(lsOutput);
          Memo.Lines.Add(lsError);

          Showmessage('finish app');
        end;
    Except




    End;
  Finally

    loShell.Free;
  End;



end;

procedure TForm1.Button1Click(Sender: TObject);
Var
  loShell: TNovusShell;
begin
  Try
    Try
      loShell := TNovusShell.Create;

      if not loShell.RunCommand('notepad.exe', '', '') then
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

      if not loShell.RunCommand('cmd.exe', '', '') then
         ShowMessage('Error')
      else
        Showmessage('finish app');
    Except




    End;
  Finally

    loShell.Free;
  End;
end;

procedure TForm1.Button3Click(Sender: TObject);
Var
  loShell: TNovusShell;
begin
  Try
    Try
      loShell := TNovusShell.Create;

      if not loShell.RunCommand('powershell.exe', '', '') then
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
