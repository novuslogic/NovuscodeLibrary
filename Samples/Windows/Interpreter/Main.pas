unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Interpreter,
  NovusInterpreter, NovusList;

type
  TMainForm = class(TForm)
    Memo: TMemo;
    ParseButton: TButton;
    LexStringGrid: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure ParseButtonClick(Sender: TObject);
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
  LexStringGrid.Cells[3,0] := 'Token Type';

end;

{$R *.dfm}

procedure TMainForm.FormShow(Sender: TObject);
begin
  ResetUI;
end;

procedure TMainForm.ParseButtonClick(Sender: TObject);
Var
  LInterpreter: tInterpreter;
  loToken: tToken;
  I, liRow: Integer;
begin
  Try
    LInterpreter:= tInterpreter.Create;

    LInterpreter.LoadFromString(Memo.text);

    LInterpreter.Execute;


    liRow := 1;
    for I := 0 to LInterpreter.oTokenList.Count - 1 do
      begin
        loToken := tToken(LInterpreter.oTokenList.Items[i]);

        LexStringGrid.Cells[0,liRow] := loToken.RawToken;
        LexStringGrid.Cells[1,liRow] := IntToStr(loToken.StartSourceLineNo);
        LexStringGrid.Cells[2,liRow] := IntToStr(loToken.StartColumnPos);


        inc(liRow);
      end;





    (*
    Try
      loNovusListEnumerator := LInterpreter.oTokenList.GetEnumerator;

      for loToken in loNovusListEnumerator do
        begin
         // do something
        end;
    Finally
      loNovusListEnumerator.free;
    End;
    *)

    (*
    for loToken in LInterpreter.oTokenList.GetEnumerator do
    begin
      // do something
    end;
    *)


    (*
    while sc.token <> tEndOfStream do
          begin
          StringGrid.Cells[0,rowCount] := sc.TokenToString;
          StringGrid.Cells[1,rowCount] := inttostr (sc.tokenRecord.lineNumber);
          StringGrid.Cells[2,rowCount] := inttostr (sc.tokenRecord.columnNumber);
          sc.nextToken;
          inc (rowCount);
          end;*)


  Finally
    LInterpreter.Free;
  End;
end;

end.
