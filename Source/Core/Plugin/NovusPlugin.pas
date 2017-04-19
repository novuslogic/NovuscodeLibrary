unit NovusPlugin;

interface

Uses Windows, SysUtils, Classes;

const
  func_GetPluginObject = 'GetPluginObject';

type
  INovusPlugin = interface
    ['{7AAD37E6-7B50-4266-8268-E8955FC6209E}']
    function GetPluginName: string; safecall;

    procedure Initialize; safecall;
    procedure Finalize; safecall;

    property PluginName: string read GetPluginName;
  end;

  TGetPluginObject = function: INovusPlugin; stdcall;

  PPluginInfo = ^TPluginInfo;

  TPluginInfo = record
    FileName: string;
    Handle: Thandle;
    Plugin: INovusPlugin;
    GetPluginObjectFunc: TGetPluginObject;
  end;

  TNovusPlugins = class(Tobject)
  private
  protected
    fPlugins: TList;
    function GetPlugins(Index: integer): INovusPlugin;
    function GetPluginCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function FindPlugin(const aPluginName: string;
      out aPlugin: INovusPlugin): boolean;
    function LoadPlugin(const aFilename: String): boolean;
    procedure UnloadPlugin(aIndex: integer);
    procedure UnloadAllPlugins;
    property Plugins[Index: integer]: INovusPlugin read GetPlugins; default;
    property PluginCount: integer read GetPluginCount;
  end;

implementation

uses System.Generics.Defaults;

constructor TNovusPlugins.Create;
begin
  fPlugins := TList.Create;
end;

destructor TNovusPlugins.Destroy;
begin
  if (fPlugins <> nil) then
  begin
    UnloadAllPlugins;
    fPlugins.Free;
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
  FPluginInfo: PPluginInfo;
  lHandle: Thandle;
begin
  if PluginCount = 0 then
    Exit;

  FPluginInfo := fPlugins[aIndex];
  if (FPluginInfo^.Handle = 0) then
    Exit;

  try
    FPluginInfo^.Plugin.Finalize;

    lHandle := FPluginInfo^.Handle;

    FPluginInfo^.Plugin := nil;

    if (lHandle <> 0) then
      FreeLibrary(lHandle);

  finally
    Dispose(FPluginInfo);

    fPlugins.Delete(aIndex);
  end;
end;

function TNovusPlugins.LoadPlugin(const aFilename: String): boolean;
var
  lHandle: Thandle;
  FPluginInfo: PPluginInfo;
  ptr: pointer;
  FPlugin: INovusPlugin;
begin
  Result := False;

  if not FileExists(aFilename) then
    Exit;

  Try
    New(FPluginInfo);

    FPluginInfo^.FileName := aFilename;

    FPluginInfo^.Handle := 0;
    FPluginInfo^.Handle := LoadLibrary(Pchar(aFilename));

    ptr := GetProcAddress(FPluginInfo^.Handle, func_GetPluginObject);

    @FPluginInfo^.GetPluginObjectFunc := ptr;
    FPlugin := FPluginInfo.GetPluginObjectFunc();
    FPluginInfo^.Plugin := FPlugin;
    FPlugin := nil;

    if FindPlugin(FPluginInfo^.Plugin.PluginName, FPlugin) then
      raise Exception.Create('Plugin Loaded already');

    FPlugin := nil;

    FPluginInfo^.Plugin.Initialize;

    fPlugins.Add(FPluginInfo);

    Result := True;
  Except
    lHandle := FPluginInfo^.Handle;
    FPluginInfo^.Plugin := nil;
    Dispose(FPluginInfo);
    if (lHandle <> 0) then
      FreeLibrary(lHandle);
    raise;
  End;
end;

function TNovusPlugins.FindPlugin(const aPluginName: string;
  out aPlugin: INovusPlugin): boolean;
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

function TNovusPlugins.GetPlugins(Index: integer): INovusPlugin;
begin
  Result := PPluginInfo(fPlugins[Index])^.Plugin;
end;

function TNovusPlugins.GetPluginCount: integer;
begin
  Result := fPlugins.Count;
end;

end.
