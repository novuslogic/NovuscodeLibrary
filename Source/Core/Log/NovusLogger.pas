unit NovusLogger;

interface

Uses NovusObject, NovusLogger.Provider, System.SysUtils, NovusUtilities,
     System.Threading, System.Classes, System.Generics.Collections, System.Types;

type
  TNovusLogTaskQueue = class
  private
    FTaskQueue: TQueue<ITask>;
    FLock: TObject;
    FIsRunning: Boolean;
    procedure ExecuteNextTask;
  public
    constructor Create;
    destructor Destroy;
    procedure AddTask(const ATaskProc: TProc);
  end;

  TNovusLogger = class(tNovusObject)
  private
  protected
    FLoggerTaskQueue: TNovusLogTaskQueue;
    FProviders: array of TNovusLogger_Provider;
  public
    procedure PushLogMessage(aLogMessage: string; aLogDateTime: tDateTime; aSeverityType: TSeverityType; aProvider: TNovusLogger_Provider); virtual;

    procedure AddLogSuccess(aMessage: string); virtual;
    procedure AddLogInformation(aMessage : string); virtual;
    procedure AddLogError(aMessage: string); overload; virtual;
    procedure AddLogError(aException: Exception); overload; virtual;
    procedure AddLogWarning(aMessage: string); virtual;
    procedure AddLogDebug(aMessage: string); virtual;
    procedure AddLogSystem(aMessage: string); virtual;
    function AddLogException: String; overload; virtual;
    procedure AddLogException(aMessage: String); overload; virtual;
    procedure AddLogException(aException: Exception); overload; virtual;

    procedure FlushLogs; virtual;

    function OpenLog: Boolean; virtual;
    function CloseLog: Boolean;  virtual;

    constructor Create(const aProviders: array of tNovuslogger_Provider); virtual;
    destructor Destroy; override;
  end;

implementation


// TNovusLogger
constructor TNovusLogger.Create(const aProviders: array of tNovuslogger_Provider);
begin
  FLoggerTaskQueue:= TNovusLogTaskQueue.Create;

  SetLength(FProviders, Length(aProviders));
  for var I := Low(aProviders) to High(aProviders) do
    begin
      FProviders[I] := aProviders[I];
      FProviders[I].Logger := Self;
    end;
end;

destructor TNovusLogger.Destroy;
begin
  FlushLogs;

  FLoggerTaskQueue.Free;

  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      begin
        FProviders[I].Free;
      end;

  inherited;
end;


function TNovusLogger.OpenLog: Boolean;
begin
  Result := true;

  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      begin
        Result := FProviders[I].OpenLog;
        if Not Result then Break;
        
      end;
end;

function TNovusLogger.CloseLog: Boolean;
begin
  result := True;

   for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      begin
        Result := FProviders[I].CloseLog;
        if Not Result then Break;
      end;
end;

procedure TNovusLogger.PushLogMessage(aLogMessage: string; aLogDateTime: tDateTime; aSeverityType: TSeverityType; aProvider: TNovusLogger_Provider);
begin
   aProvider.SendLogMessage(aLogMessage, aLogDateTime, aSeverityType);


   (*
    FLoggerTaskQueue.AddTask(procedure
    begin
      aProvider.SendLogMessage(aLogMessage, aLogDateTime, aSeverityType);
    end);
    *)
end;

procedure TNovusLogger.AddLogSuccess(aMessage: string);
begin
  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      FProviders[I].AddLogSuccess(aMessage);
end;

procedure TNovusLogger.AddLogInformation(aMessage : string);
begin
  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
       FProviders[I].AddLogInformation(aMessage);
end;

procedure TNovusLogger.AddLogError(aMessage: string);
begin
  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      FProviders[I].AddLogError(aMessage);
end;

procedure TNovusLogger.AddLogError(aException: Exception);
begin
  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      FProviders[I].AddLogError(aException);
end;

procedure TNovusLogger.AddLogWarning(aMessage: string);
begin
  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
       FProviders[I].AddLogWarning(aMessage);

end;

function TNovusLogger.AddLogException: String;
begin
  var lsMessage := TNovusUtilities.GetExceptMess;
  Result := lsMessage;

  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      FProviders[I].AddLogException(lsMessage);
end;

procedure TNovusLogger.AddLogDebug(aMessage: string);
begin
  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      FProviders[I].AddLogDebug(aMessage);
end;

procedure TNovusLogger.AddLogSystem(aMessage: string);
begin
  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      FProviders[I].AddLogSystem(aMessage);
end;



procedure TNovusLogger.AddLogException(aMessage: String);
begin
  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      FProviders[I].AddLogException(aMessage);
end;


procedure TNovusLogger.AddLogException(aException: Exception);
begin
  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      FProviders[I].AddLogException(aException);
end;

procedure TNovusLogger.FlushLogs;
begin
  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      FProviders[I].FlushLog;
end;



// TNovusLogTaskQueue
constructor TNovusLogTaskQueue.Create;
begin
  inherited Create;
  FTaskQueue := TQueue<ITask>.Create;
  FLock := TObject.Create;
  FIsRunning := False;
end;

destructor TNovusLogTaskQueue.Destroy;
begin
  FTaskQueue.Free;
  FLock.Free;
end;

procedure TNovusLogTaskQueue.AddTask(const ATaskProc: TProc);
var
  NewTask: ITask;
begin
  NewTask := TTask.Create(procedure
    begin
      ATaskProc();
      ExecuteNextTask;
    end);

  TMonitor.Enter(FLock);
  try
    FTaskQueue.Enqueue(NewTask);
    if not FIsRunning then
      ExecuteNextTask;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TNovusLogTaskQueue.ExecuteNextTask;
var
  NextTask: ITask;
begin
  TMonitor.Enter(FLock);
  try
    if FTaskQueue.Count > 0 then
    begin
      FIsRunning := True;
      NextTask := FTaskQueue.Dequeue;
    end
    else
    begin
      FIsRunning := False;
      Exit;
    end;
  finally
    TMonitor.Exit(FLock);
  end;

  NextTask.Start;
end;




end.
