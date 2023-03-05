unit NovusOTAUtils;

interface

Uses Classes, ToolsAPI, vcl.menus, vcl.dialogs, NovusVCLControlsUtils, SysUtils,
  ActnList;

type
  tNovusOTAUtils = class
  private
  protected
  public
    class function GetIDEMainMenu: TMainMenu;
    class function CreateMenuItem(aName, aCaption: String; aMenuItem: tMenuItem;
      aNearMenuName: String = ''; aInsertAfter: Boolean = True;
      aInsertAsChild: Boolean = False; aAction: tAction = nil;
      aOnClick: TNotifyEvent = NIL): tMenuItem;

    class procedure OutputMessage(aMessage: String);



  end;

implementation

class function tNovusOTAUtils.GetIDEMainMenu: TMainMenu;
begin
  Result := (BorlandIDEServices as INTAServices).GetMainMenu;
end;

class function tNovusOTAUtils.CreateMenuItem(aName, aCaption: String;
  aMenuItem: tMenuItem; aNearMenuName: String = '';
  aInsertAfter: Boolean = True; aInsertAsChild: Boolean = False;
  aAction: tAction = nil; aOnClick: TNotifyEvent = NIL): tMenuItem;
Var
  lINTAServices: INTAServices;
  MyAction : TAction;
begin
  Result := nil;

  lINTAServices := (BorlandIDEServices as INTAServices);

  Result := tNovusVCLControlsUtils.FindMenuItembyCaption([aCaption], aMenuItem);

  if Result = nil then
  begin
    if supports(BorlandIDEServices, INTAServices, lINTAServices) then
    begin
      if Assigned(aAction) then
        if aAction.Caption = aAction.name then aAction.Caption := aCaption;

      Result := tMenuItem.create(nil);
      Result.caption := aCaption;

      Result.Name := aName;
      Result.OnClick := aOnClick;

      Result.Action := aAction;

      if aNearMenuName = '' then
        aNearMenuName := 'toolsmenu';


      lINTAServices.addactionmenu(aNearMenuName, aAction, Result, aInsertAfter,
         aInsertAsChild);

      {$ifdef debug}
      outputmessage('insertmenu: ' + acaption);
      {$endif}
    end;
  end;
end;

class procedure tNovusOTAUtils.OutputMessage(aMessage: String);
Begin
  (BorlandIDEServices As IOTAMessageServices).AddTitleMessage(aMessage);
End;

end.
