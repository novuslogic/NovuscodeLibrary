program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  NovusPlugin in '..\..\Source\Core\Plugin\NovusPlugin.pas',
  PluginClasses in 'PluginClasses.pas',
  NovusUtilities in '..\..\Source\Core\Utilities\NovusUtilities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
