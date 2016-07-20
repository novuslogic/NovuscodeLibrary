unit NovusPlugin;

interface

Uses Windows, SysUtils, Classes ;


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
    function GetPluginCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function FindPlugin(const aPluginName: string; out aPlugin: INovusPlugin): boolean;
    function LoadPlugin(const aFilename: String): Boolean;
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
      fplugins.Free;
  end;
end;

procedure TNovusPlugins.UnloadAllPlugins;
var
  I: integer;
begin
  for i := PluginCount - 1 downto 0 do
    UnloadPlugin(I);
end;

procedure TNovusPlugins.UnloadPlugin(aIndex: integer);
var
  FPluginInfo: PPluginInfo;
begin
  if PluginCount = 0 then Exit;

  FPluginInfo := fPlugins[aIndex];
  if (FPluginInfo^.Handle = 0) then Exit;

  try
    fPlugins.Delete(aIndex);

    FPluginInfo^.Plugin.Finalize;

    TSingletonImplementation(FPluginInfo^.Plugin).Free;
    FPluginInfo^.Plugin := NIL;

    if (FPluginInfo^.Handle <> 0) then FreeLibrary(FPluginInfo^.Handle);

  finally
    Dispose(FPluginInfo);
  end;
end;


function TNovusPlugins.LoadPlugin(const aFilename: String): Boolean;
var
  lHandle: THandle;
  FPluginInfo: PPluginInfo;
  ptr: pointer;
  FPlugin: INovusPlugin;
begin
  Result := False;

  if not FileExists(aFileName) then Exit;

  Try
    New(FPluginInfo);

    FPluginInfo^.FileName := aFileName;

    FPluginInfo^.Handle := 0;
    FPluginInfo^.Handle := LoadLibrary(Pchar(aFileName));

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
    if (lHandle <> 0) then FreeLibrary(lHandle);
    raise;
  End;
end;

function TNovusPlugins.FindPlugin(const aPluginName: string; out aPlugin: INovusPlugin): boolean;
var
  i: integer;
begin
  Result := false;
  aPlugin := nil;

  for i := 0 to (PluginCount - 1) do
    if SameText(Plugins[i].PluginName, aPluginName) then
      begin
        aPlugin := Plugins[i];

        result := true;
        Exit;
    end;
end;

function TNovusPlugins.GetPlugins(Index: integer): INovusPlugin;
begin
  result := PPluginInfo(fPlugins[Index])^.Plugin;
end;

function TNovusPlugins.GetPluginCount: integer;
begin
  result := fPlugins.Count;
end;

end.
