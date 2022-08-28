program InterpreterTest1;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  NovusInterpreter in '..\..\..\Source\Core\Parser\NovusInterpreter.pas',
  Interpreter in 'Interpreter.pas',
  NovusParser in '..\..\..\Source\Core\Parser\NovusParser.pas',
  NovusList in '..\..\..\Source\Core\Object\NovusList.pas',
  NovusObject in '..\..\..\Source\Core\Object\NovusObject.pas',
  NovusBO in '..\..\..\Source\Core\Object\NovusBO.pas',
  NovusCommentsParserCell in '..\..\..\Source\Core\Parser\ParserCells\NovusCommentsParserCell.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
