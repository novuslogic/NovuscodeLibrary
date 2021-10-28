unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, NovusShell, NovusUtilities;

type
  TForm1 = class(TForm)
    btnRunCommand: TButton;
    btnRunCommandSilent: TButton;
    Memo: TMemo;
    btnRunCommandCapture: TButton;
    procedure btnRunCommandClick(Sender: TObject);
    procedure btnRunCommandSilentClick(Sender: TObject);

    procedure btnRunCommandCaptureClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.btnRunCommandCaptureClick(Sender: TObject);
Var
  loShell: TNovusShell;
  lsOutput: string;
begin
  Memo.Lines.Clear;
  Try
    Try
      loShell := TNovusShell.Create;

      if loShell.RunCommandCapture('Cmd.exe /C Dir '+CR+'Exit'+CR, lsOutput ) <> 0 then
        ShowMessage(loShell.GetLastSysErrorMess)
      else
        begin

          Memo.Lines.Add(lsOutput);
          Showmessage('finish app Captured to memo');
        end;
    Except

    End;
  Finally
    loShell.Free;
  End;
end;

procedure TForm1.btnRunCommandClick(Sender: TObject);
Var
  loShell: TNovusShell;
begin

  Try
    Try
      loShell := TNovusShell.Create;

      if loShell.RunCommand('Notepad.exe', '', '') <> 0 then
        ShowMessage(loShell.GetLastSysErrorMess)
      else
        Showmessage('finish app');
    Except

    End;
  Finally

    loShell.Free;
  End;
end;

procedure TForm1.btnRunCommandSilentClick(Sender: TObject);
Var
  loShell: TNovusShell;
begin
  Memo.Lines.Clear;

  Try
    Try
      loShell := TNovusShell.Create;

      if loShell.RunCommandSilent('Cmd.exe', '', '/C Dir > Tmp.Txt '+CR+'Exit'+CR ) <> 0 then
        ShowMessage(loShell.GetLastSysErrorMess)
      else
        begin
          Memo.Lines.LoadFromFile('tmp.txt');
          Showmessage('finish app copied to memo');
        end;
    Except

    End;
  Finally

    loShell.Free;
  End;
end;






end.
