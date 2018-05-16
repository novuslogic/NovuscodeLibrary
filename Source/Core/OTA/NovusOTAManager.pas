unit NovusOTAManager;

interface

Uses Classes, VCL.Menus, ToolsAPI, SysUtils, NovusOTAUtils, ActnList,
  VCL.dialogs, NovusVCLControlsUtils;

Type
  TNovusOTAManager = Class(Tobject)
  protected
  private
    FSvcs: IOTAServices;
    FIDEMainMenu: TMainMenu;
    function GetIDEMainMenu: TMainMenu;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure InitManager; virtual;

    function CurrentProject: IOTAProject;
    function CurrentProjectName: string;

    function GetIDEFile: tMenuItem;

    function CurrentProjectGroup: IOTAProjectGroup;

    function ModuleServices: IOTAModuleServices;

    property IDEMainMenu: TMainMenu read GetIDEMainMenu;

    property Svcs: IOTAServices read FSvcs write FSvcs;

    procedure OutputMessage(aMessage: String);
    procedure DebugOutputMessage(aMessage: String);

    function CreateMenuItem(aName, aCaption: String; aMenuItem: tMenuItem;
      aNearMenuName: String = ''; aInsertAfter: Boolean = True;
      aInsertAsChild: Boolean = False; aAction: tAction = nil;
      aOnClick: TNotifyEvent = NIL): tMenuItem;

  End;

implementation

constructor TNovusOTAManager.Create;
begin
  FIDEMainMenu := nil;

  FSvcs := BorlandIDEServices As IOTAServices;

  InitManager;
end;

destructor TNovusOTAManager.Destroy;
begin
  DebugOutputMessage('TNovusOTAManager.Destroy');
end;

procedure TNovusOTAManager.InitManager;
begin
end;

function TNovusOTAManager.ModuleServices: IOTAModuleServices;
begin
  result := (BorlandIDEServices as IOTAModuleServices);
end;

function TNovusOTAManager.GetIDEMainMenu: TMainMenu;
begin
  if Not Assigned(FIDEMainMenu) then
    FIDEMainMenu := tNovusOTAUtils.GetIDEMainMenu;

  result := FIDEMainMenu;
end;

function TNovusOTAManager.CurrentProject: IOTAProject;
var
  FModuleServices: IOTAModuleServices;
  module: IOTAModule;
  project: IOTAProject;
  projectgroup: IOTAProjectGroup;
  multipleprojects: Boolean;
  i: Integer;
begin
  result := nil;

  multipleprojects := False;
  FModuleServices := ModuleServices;

  if (FModuleServices = nil) then
    Exit;

  for i := 0 to (FModuleServices.ModuleCount - 1) do
  begin
    module := FModuleServices.Modules[i];
    if (module.QueryInterface(IOTAProjectGroup, projectgroup) = S_OK) then
    begin
      result := projectgroup.ActiveProject;

      Exit;
    end

    else if module.QueryInterface(IOTAProject, project) = S_OK then
    begin
      if (result = nil) then
        result := project // Found the first project, so save it
      else
        multipleprojects := True;
      // It doesn't look good, but keep searching for a project group
    end;
  end;

  if multipleprojects then
    result := nil;
end;

function TNovusOTAManager.CurrentProjectGroup: IOTAProjectGroup;
var
  services: IOTAModuleServices;
  i: Integer;
begin
  result := nil;
  services := ModuleServices;
  for i := 0 to ModuleServices.ModuleCount - 1 do
  begin
    if Supports(ModuleServices.Modules[i], IOTAProjectGroup, result) then
    begin
      Break;
    end;
  end;
end;

function TNovusOTAManager.CurrentProjectName: string;
var
  lProjectName: string;
begin
  if Assigned(CurrentProject()) then
  begin
    lProjectName := (CurrentProject as IOTAModule).FileName;
    lProjectName := ChangeFileExt(ExtractFileName(lProjectName), '');
  end
  else
  begin
    lProjectName := '';
  end;
  result := lProjectName;
end;

procedure TNovusOTAManager.OutputMessage(aMessage: String);
begin
  tNovusOTAUtils.OutputMessage(aMessage);
end;

procedure TNovusOTAManager.DebugOutputMessage(aMessage: String);
begin
{$IFDEF DEBUG}
  tNovusOTAUtils.OutputMessage(aMessage);
{$ENDIF}
end;

function TNovusOTAManager.CreateMenuItem(aName, aCaption: String;
  aMenuItem: tMenuItem; aNearMenuName: String = '';
  aInsertAfter: Boolean = True; aInsertAsChild: Boolean = False;
  aAction: tAction = nil; aOnClick: TNotifyEvent = NIL): tMenuItem;
begin
  result := tNovusOTAUtils.CreateMenuItem(aName, aCaption, aMenuItem,
    aNearMenuName, aInsertAfter, aInsertAsChild, NIL, aOnClick);
end;

function TNovusOTAManager.GetIDEFile: tMenuItem;
begin
  Result := nil;

  if Not Assigned(IDEMainMenu) then Exit;

  Result := tNovusVCLControlsUtils.FindMenuItembyCaption(['File'],
    IDEMainMenu.Items);
end;

end.
