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

    class function Parse: INovusCommandLineResult;

    class function RegisterCommand(const CommandName: string;
                                   aClass: TClass): INovusCommandLineParam; overload;
  end;

implementation


class constructor tNovusCommandLine.Create;
begin
  FCommandList := TNovusList.Create(tNovusCommandLineParam);
end;

class destructor tNovusCommandLine.Destroy;
begin
  if Assigned(FParamStrList) then FParamStrList.Free;

  FCommandList.Free;
end;


class function tNovusCommandLine.Parse: INovusCommandLineResult;
begin
  Result := NIL;

  FParamStrList := tNovusCommandLine.ParamStrToStringList;

  //Result.ParamStrToStringList;
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

class function tNovusCommandLine.RegisterCommand(const CommandName: string;
                                   aClass: TClass): INovusCommandLineParam;
begin

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


end.
