unit Plugin;

interface

Uses NovusPlugin, SysUtils, Classes, PluginClasses ;



function GetPluginObject: INovusPlugin; stdcall;

implementation

var
  _TestPlugin: TTestPlugin = nil;


function GetPluginObject: INovusPlugin;
begin
  if (_TestPlugin = nil) then _TestPlugin := TTestPlugin.Create;
  result := _TestPlugin;
end;

exports
  GetPluginObject name func_GetPluginObject;

initialization
  _TestPlugin := nil;

finalization
  FreeAndNIL(_TestPlugin);

end.
