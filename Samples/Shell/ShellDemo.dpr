program ShellDemo;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  NovusShell in '..\..\Source\Core\Utilities\NovusShell.pas',
  NovusUtilities in '..\..\Source\Core\Utilities\NovusUtilities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
