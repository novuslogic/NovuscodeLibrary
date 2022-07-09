unit PluginClasses;

interface

Uses NovusPlugin, Classes, System.Generics.Defaults;

Type
   TTestPlugin = class(TNovusPlugin)
   private
   protected
   public
     function GetPluginName: string; override; safecall;

     procedure Initialize; override; safecall;
     procedure Finalize; override; safecall;

     function GetTest: string;  safecall;
   end;

implementation

function TTestPlugin.GetTest: string;
begin
  Result := 'Test';
end;

function TTestPlugin.GetPluginName: string;
begin
  Result := 'TestPlugin';
end;

procedure TTestPlugin.Initialize;
begin
//
end;

procedure TTestPlugin.Finalize;
begin
//
end;


end.
