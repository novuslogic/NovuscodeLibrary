unit NovusCommandLine;

interface

uses
  NovusConsole, SysUtils, Classes, NovusList;

type
  INovusCommandLineResult = interface
  ['{4E2886DD-4120-4376-B778-0D803E621850}']
    function GetErrors: Boolean;
    procedure SetErrors(Value: boolean);

    property Errors : boolean read GetErrors write SetErrors;

  end;

   TNovusCommandLineResult = class(TInterfacedObject, INovusCommandLineResult)
   protected
   private
     function GetErrors: Boolean;
     procedure SetErrors(Value: boolean);
   public
     property Errors : boolean read GetErrors write SetErrors;
   end;

  INovusCommandLineParam = interface
  ['{0CC48B45-F78D-42A5-9C49-C36C3BEE853E}']
    function GetCommandName: string;
    procedure SetCommandName(Value: string);

    property CommandName : string read GetCommandName write SetCommandName;

  end;

  tNovusCommandLineParam = class(TInterfacedObject, INovusCommandLineParam)
  protected
    fsCommandName: String;
  private
    function GetCommandName: string;
    procedure SetCommandName(Value: string);
  public
    property CommandName : string read GetCommandName write SetCommandName;
    function Execute: boolean; virtual;
  end;

  tNovusCommandLine = class
  protected
    class var
      FCommandList: TNovusList;
      FParamStrList: tStringlist;
  private
    class function ParamStrToStringList: TStringlist;
  public
    class constructor Create;
    class destructor Destroy;

    class function Parse: TNovusCommandLineResult;

    class function RegisterCommand(const aCommandName: string;
            aCommandLineParam: tNovusCommandLineParam): tNovusCommandLineParam;

  end;

implementation


class constructor tNovusCommandLine.Create;
begin
  FCommandList := TNovusList.Create;
end;

class destructor tNovusCommandLine.Destroy;
begin
  if Assigned(FParamStrList) then FParamStrList.Free;

  FCommandList.Free;
end;


class function tNovusCommandLine.Parse: TNovusCommandLineResult;
begin
  Result := NIL;

  FParamStrList := tNovusCommandLine.ParamStrToStringList;


end;

class function tNovusCommandLine.ParamStrToStringList: TStringlist;
Var
  i: integer;
begin
  Result := TStringlist.Create;

  if ParamCount > 0 then
  begin
    for i := 1 to ParamCount do
      Result.Add(ParamStr(i));
  end;
end;


//  tNovusCommandLine

class function tNovusCommandLine.RegisterCommand(const aCommandName: string;
            aCommandLineParam: tNovusCommandLineParam): tNovusCommandLineParam;
begin

  if not Assigned(aCommandLineParam) then aCommandLineParam := tNovusCommandLineParam.Create;


  aCommandLineParam.CommandName := aCommandName;


  FCommandList.Add(FCommandList);
end;



//  tNovusCommandLineParam

function tNovusCommandLineParam.GetCommandName: string;
begin
  result := fsCommandName;
end;

procedure tNovusCommandLineParam.SetCommandName(Value: string);
begin
  fsCommandName := Value;
end;

function tNovusCommandLineParam.Execute: boolean;
begin

end;



// tNovusCommandlineResult

function TNovusCommandLineResult.GetErrors: Boolean;
begin

end;

procedure TNovusCommandLineResult.SetErrors(Value: boolean);
begin

end;

end.
