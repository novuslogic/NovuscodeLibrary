unit NovusDialogs;

interface


uses NovusUtilities, dialogs, forms;

Type
  TNovusVCLUtils = class(TNovusUtilities)
  public
    class function MessageDlgEx(const Title: String; const Msg: string; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
  end;

implementation

class function TNovusVCLUtils.MessageDlgEx(const Title: String; const Msg: string; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  with CreateMessageDialog(Msg, mtCustom, Buttons) do
  try
    Caption:=Title;
    HelpContext:=HelpCtx;
    HelpFile:='';
    Position:=poScreenCenter;
    result:=ShowModal;
  finally
    Free;
  end;
end;

end.
