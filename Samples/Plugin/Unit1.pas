unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NovusPlugin, NovusUtilities,
  Vcl.StdCtrls, PluginClasses;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FNovusPlugins: TNovusPlugins;
    FTestPlugin: INovusPlugin;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Try
    if Assigned(FTestPlugin) then
     Showmessage(TTestPlugin(FTestPlugin).GetTest);
  Except
    Showmessage(TNovusUtilities.GetExceptMess);
  End;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  System.ReportMemoryLeaksOnShutdown := true;

  FNovusPlugins := TNovusPlugins.Create;
  FNovusPlugins.LoadPlugin('TestPlugin.DLL');
  FNovusPlugins.FindPlugin('TestPlugin', FTestPlugin);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FNovusPlugins.UnloadPlugin(0);
  FNovusPlugins.Free;

  ReportMemoryLeaksOnShutdown := true;
end;

end.
