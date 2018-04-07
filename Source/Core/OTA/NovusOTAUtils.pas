unit NovusOTAUtils;

interface

Uses Classes, ToolsAPI, vcl.menus, vcl.dialogs, NovusVCLControlsUtils, SysUtils;

type


   TMenuBreadcrumbs = array of String;

   tNovusOTAUtils  = class
   private
   protected
   public
     class function GetIDEMainMenu: TMainMenu;
     class function CreateMenuItem(aName, aCaption : String; aMenuItem: tMenuItem; aNearMenuName: String = '';aInsertAfter: Boolean = True;
      aInsertAsChild: Boolean = False) : TMenuItem;

     class procedure OutputMessage(aMessage : String);

   end;

implementation

class function tNovusOTAUtils.GetIDEMainMenu: TMainMenu;
begin
  Result :=(BorlandIDEServices as INTAServices).GetMainMenu;
end;



class function  tNovusOTAUtils.CreateMenuItem(aName, aCaption : String; aMenuItem: tMenuItem; aNearMenuName: String = '';aInsertAfter: Boolean = True;
      aInsertAsChild: Boolean = False) : TMenuItem;
Var
  lINTAServices: INTAServices;
begin
  Result := nil;

  LINTAServices  := (BorlandIDEServices as INTAServices);

  Result := tNovusVCLControlsUtils.FindMenuItembyCaption([aCaption], aMenuItem);

  if Result = nil then
    begin
      if supports(borlandideservices, intaservices, Lintaservices) then
          begin
            Result := tmenuitem.create(nil);
            Result.caption := aCaption;
            Result.Name := aName;

            if aNearMenuName = ''  then
              aNearMenuName := 'toolsmenu';

            Lintaservices.addactionmenu(aNearMenuName, nil, Result, aInsertAfter,
                       aInsertAsChild);

            {$IFDEF DEBUG}
            OutputMessage('InsertMenu: ' + aCaption);
            {$ENDIF}
          end;
    end;
end;

class procedure tNovusOTAUtils.OutputMessage(aMessage : String);
Begin
  (BorlandIDEServices As IOTAMessageServices).AddTitleMessage(aMessage);
End;







end.
