unit PluginClasses;

interface

Uses NovusPlugin, Classes, System.Generics.Defaults;

Type
   ITestPlugin = interface(INovusPlugin)
    ['{838468EA-1750-4CB5-B6B3-E7078F59A46A}']
    function GetTest: string; safecall;
  end;

   TTestPlugin = class( TSingletonImplementation, INovusPlugin, ITestPlugin)
   private
   protected
   public
     function GetPluginName: string; safecall;

     procedure Initialize; safecall;
     procedure Finalize; safecall;

     function GetTest: string;  safecall;

     property PluginName: string read GetPluginName;
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
  ReportMemoryLeaksOnShutdown := true;
end;


end.
