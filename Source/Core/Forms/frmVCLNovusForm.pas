unit frmVCLNovusForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NovusUtilities, StdCtrls;

type
  ThiVCLNovusForm = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fParentWinControl: TWinControl;
    fbIgnoreInitWindow: Boolean;
    fbUseFormKeyPress: Boolean;
  public
    { Public declarations }
    procedure Handle_WM_CLOSEWINDOW(var msg: TMessage); message WM_CLOSEWINDOW;

    function InitWindow: Boolean; virtual;

    procedure SetupWindow; virtual;

    property ParentWinControl: TWinControl read fParentWinControl
      write fParentWinControl;

    property IgnoreInitWindow: Boolean read fbIgnoreInitWindow
      write fbIgnoreInitWindow;

    property UseFormKeyPress: Boolean read fbUseFormKeyPress
      write fbUseFormKeyPress;

  end;

  ThiVCLNovusFormClass = class of ThiVCLNovusForm;

var
  hiVCLNovusForm: ThiVCLNovusForm;

implementation

procedure ThiVCLNovusForm.FormCreate(Sender: TObject);
begin
  IgnoreInitWindow := false;
end;

procedure ThiVCLNovusForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if UseFormKeyPress = false then
    Exit;

  if Sender Is TCustomMemo then
    Exit;

  If Key = #13 Then
  begin
    SelectNext(ActiveControl as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure ThiVCLNovusForm.FormShow(Sender: TObject);
begin
  if not fbIgnoreInitWindow then
    InitWindow;
end;

function ThiVCLNovusForm.InitWindow: Boolean;
begin
  Result := True;
end;

procedure ThiVCLNovusForm.SetupWindow;
begin
  //
end;

procedure ThiVCLNovusForm.Handle_WM_CLOSEWINDOW(var msg: TMessage);
begin
  { }
end;

{$R *.dfm}

end.
