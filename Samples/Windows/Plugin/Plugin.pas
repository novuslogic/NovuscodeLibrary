unit Plugin;

interface

Uses NovusPlugin, SysUtils, Classes, PluginClasses ;



function GetPluginObject: TNovusPlugin; stdcall;

implementation

var
  _TestPlugin: TTestPlugin = nil;


function GetPluginObject: TNovusPlugin;
begin
  if (_TestPlugin = nil) then _TestPlugin := TTestPlugin.Create;
  result := _TestPlugin;
end;

exports
  GetPluginObject name func_GetPluginObject;

initialization
  _TestPlugin := nil;

finalization
  FreeandNIL(_TestPlugin);

end.
