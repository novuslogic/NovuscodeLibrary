unit NovusPlugin;

interface

Uses Windows, SysUtils, Classes, NovusUtilities, NovusList;

const
  func_GetPluginObject = 'GetPluginObject';

type
  TNovusPlugin = class(tobject)
  private
  protected
  public
    function GetPluginName: string; virtual; safecall;

    procedure Initialize; virtual; safecall;
    procedure Finalize; virtual; safecall;

    property PluginName: string read GetPluginName;
  end;

  TGetPluginObject = function: TNovusPlugin; stdcall;

  TPluginInfo = class(TObject)
  private
  protected
    fsFileName: String;
    fHandle: Thandle;
    fsPluginName: String;
    fPlugin: TNovusPlugin;
    fGetPluginObjectFunc: TGetPluginObject;
  public
    destructor Destroy; override;

    property FileName: string
      read fsFilename write fsFilename;

    property PluginName: String
      read fsPluginName write fsPluginName;

    property Handle: Thandle
      read fHandle write fHandle;

    property Plugin: TNovusPlugin
       read fPlugin write fPlugin;

    property GetPluginObjectFunc: TGetPluginObject
      read fGetPluginObjectFunc write fGetPluginObjectFunc;
  end;

  TNovusPlugins = class(Tobject)
  private
  protected
    fPluginList: TNovusList;
    function GetPlugins(Index: integer): TNovusPlugin;
    function GetPluginCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearPluginList;
    function GetPluginlist(aIndex: Integer): tPluginInfo;

    function FindPlugin(const aPluginName: string;
      out aPlugin: TNovusPlugin): boolean; overload;

    function FindPlugin(const aPluginName: string): boolean; overload;

    function LoadPlugin(const aFilename: String): boolean;
    procedure UnloadPlugin(aIndex: integer);
    procedure UnloadAllPlugins;
    property Plugins[Index: integer]: TNovusPlugin read GetPlugins; default;
    property PluginCount: integer read GetPluginCount;
  end;

implementation

uses System.Generics.Defaults;

constructor TNovusPlugins.Create;
begin
  fPluginList := TNovusList.Create(TPluginInfo);
end;

destructor TNovusPlugins.Destroy;
begin
  if (fPluginList <> nil) then
  begin
    UnloadAllPlugins;
    fPluginList.Free;
  end;
end;

procedure TNovusPlugins.UnloadAllPlugins;
var
  I: integer;
begin
  for I := PluginCount - 1 downto 0 do
    UnloadPlugin(I);
end;

procedure TNovusPlugins.UnloadPlugin(aIndex: integer);
var
  FPluginInfo: tPluginInfo;
  lHandle: Thandle;
begin
  if (aIndex = fPluginList.Count) or (aIndex < 0) then Exit;

  FPluginInfo := fPluginList.Items[aIndex] as tPluginInfo;
  if (FPluginInfo.Handle = 0) then
    Exit;

  try
    Try
      FPluginInfo.Plugin.Finalize;

      if FPluginInfo.Handle <> 0 then
        FreeLibrary(FPluginInfo.Handle);
    Except
      raise Exception.Create('Unloaded error:' + TNovusUtilities.GetExceptMess);
    End;

  finally
    fPluginList.Delete(FPluginInfo, true);
  end;
end;

function TNovusPlugins.LoadPlugin(const aFilename: String): boolean;
var
  lHandle: Thandle;
  FPluginInfo: tPluginInfo;
begin
  Result := False;

  if not FileExists(aFilename) then
    Exit;

  Try
    FPluginInfo := tPluginInfo.Create;

    FPluginInfo.FileName := aFilename;

    FPluginInfo.Handle := SafeLoadLibrary(aFilename);

    FPluginInfo.GetPluginObjectFunc := GetProcAddress(FPluginInfo.Handle, func_GetPluginObject);

    FPluginInfo.Plugin := FPluginInfo.GetPluginObjectFunc();
    FPluginInfo.PluginName := FPluginInfo.Plugin.PluginName;

    if FindPlugin(FPluginInfo.Plugin.PluginName) then
      raise Exception.Create('Plugin Loaded already');

    FPluginInfo.Plugin.Initialize;

    fPluginList.Add(FPluginInfo);

    Result := True;
  Except
    if Assigned(FPluginInfo) then FPluginInfo.Free;

    raise;
  End;
end;

function TNovusPlugins.FindPlugin(const aPluginName: string;
  out aPlugin: TNovusPlugin): boolean;
var
  I: integer;
begin
  Result := False;
  aPlugin := nil;

  for I := 0 to (PluginCount - 1) do
    if SameText(Plugins[I].PluginName, aPluginName) then
    begin
      aPlugin := Plugins[I];
      Result := True;

      Exit;
    end;
end;


function TNovusPlugins.FindPlugin(const aPluginName: string): boolean;
var
  I: integer;
begin
  Result := False;

  for I := 0 to (PluginCount - 1) do
    if SameText(Plugins[I].PluginName, aPluginName) then
    begin
      Result := True;

      Exit;
    end;
end;

function TNovusPlugins.GetPluginlist(aIndex: Integer): tPluginInfo;
begin
  Result := NIl;

  if (aIndex = fPluginList.Count) or (aIndex < 0) then Exit;

  Result := fPluginList.items[aIndex] as tPluginInfo;
end;

function TNovusPlugins.GetPlugins(Index: integer): TNovusPlugin;
Var
  FPluginInfo: tPluginInfo;
begin
  Result := NIL;

  if (Index = fPluginList.Count) or (Index < 0) then Exit;

  FPluginInfo := fPluginList.Items[index] as tPluginInfo;

  Result := FPluginInfo.Plugin;
end;


function TNovusPlugins.GetPluginCount: integer;
begin
  Result := fPluginList.Count;
end;

procedure TNovusPlugins.ClearPluginList;
begin
  fPluginList.Clear;
end;

// TPluginInfo
destructor TPluginInfo.Destroy;
begin
  if Assigned(fPlugin) then fPlugin := NIL;

  if (FHandle <> 0) then FreeLibrary(FHandle);
end;


// TNovusPlugin
function TNovusPlugin.GetPluginName: string;
begin
  result := '';
end;

procedure TNovusPlugin.Initialize;
begin
end;

procedure TNovusPlugin.Finalize;
begin
end;

end.

