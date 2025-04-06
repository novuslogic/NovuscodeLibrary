unit NovusLogger;

interface

Uses NovusObject, NovusLogger.Provider, System.SysUtils, NovusUtilities,
  System.Threading, System.Classes, System.Generics.Collections, System.Types;

type
  TNovusLogger_ProviderClass = class of TNovusLogger_Provider;

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
    procedure PushLogMessage(aLogMessage: string; aLogDateTime: tDateTime;
      aSeverityType: TSeverityType; aProvider: TNovusLogger_Provider); virtual;

    function FormatLogOutput(aLogMessage: string; aDateTime: tDateTime; aSeverityType: TSeverityType;
        aProviderClass: TNovusLogger_ProviderClass = nil): string;

    function FindProvider(aProviderClass: TNovusLogger_ProviderClass = nil): TNovusLogger_Provider;

    function AddLog(aLogMessage: string; aLogDateTime: tDateTime; aSeverityType: TSeverityType; aProviderClass: TNovusLogger_ProviderClass = nil): string; virtual;

    procedure AddLogSuccess(aLogMessage: string;
      aProviderClass: TNovusLogger_ProviderClass = nil); virtual;
    procedure AddLogInformation(aLogMessage: string;
      aProviderClass: TNovusLogger_ProviderClass = nil); virtual;
    procedure AddLogError(aLogMessage: string;
      aProviderClass: TNovusLogger_ProviderClass = nil); overload; virtual;
    procedure AddLogError(aException: Exception;
      aProviderClass: TNovusLogger_ProviderClass = nil); overload; virtual;
    procedure AddLogWarning(aLogMessage: string;
      aProviderClass: TNovusLogger_ProviderClass = nil); virtual;
    procedure AddLogDebug(aLogMessage: string;
      aProviderClass: TNovusLogger_ProviderClass = nil); virtual;
    procedure AddLogSystem(aLogMessage: string;
      aProviderClass: TNovusLogger_ProviderClass = nil); virtual;
    function AddLogException(aProviderClass: TNovusLogger_ProviderClass = nil): string; overload; virtual;
    function AddLogException(aLogMessage: string;
      aProviderClass: TNovusLogger_ProviderClass = nil): string; overload; virtual;
    procedure AddLogException(aException: Exception;
      aProviderClass: TNovusLogger_ProviderClass = nil); overload; virtual;

    procedure FlushLogs; virtual;

    function OpenLog: Boolean; virtual;
    function CloseLog: Boolean; virtual;

    constructor Create(const aProviders
      : array of TNovusLogger_Provider); virtual;
    destructor Destroy; override;
  end;

implementation

// TNovusLogger
constructor TNovusLogger.Create(const aProviders
  : array of TNovusLogger_Provider);
begin
  FLoggerTaskQueue := TNovusLogTaskQueue.Create;

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
      if Not Result then
        Break;

    end;
end;

function TNovusLogger.CloseLog: Boolean;
begin
  Result := true;

  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
    begin
      Result := FProviders[I].CloseLog;
      if Not Result then
        Break;
    end;
end;

procedure TNovusLogger.PushLogMessage(aLogMessage: string;
  aLogDateTime: tDateTime; aSeverityType: TSeverityType;
  aProvider: TNovusLogger_Provider);
begin
  aProvider.SendLogMessage(aLogMessage, aLogDateTime, aSeverityType);
end;


function TNovusLogger.FormatLogOutput(aLogMessage: string; aDateTime: tDateTime; aSeverityType: TSeverityType; aProviderClass: TNovusLogger_ProviderClass = nil): string;
begin
  if aProviderClass = nil then
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) then
        Result := FProviders[I].FormatLogOutput(aLogMessage,aDateTime,aSeverityType)
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        Result := FProviders[I].FormatLogOutput(aLogMessage,aDateTime,aSeverityType);
  end;
end;

function TNovusLogger.FindProvider(aProviderClass: TNovusLogger_ProviderClass = nil): TNovusLogger_Provider;
begin
  Result := nil;

  for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          Result := FProviders[I];
          break;
        end;
end;


function TNovusLogger.AddLog(aLogMessage: string; aLogDateTime: tDateTime; aSeverityType: TSeverityType; aProviderClass: TNovusLogger_ProviderClass = nil): string;
begin
  Result := '';

  if aProviderClass = nil then
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
            Result := FProviders[I].AddLog(aLogMessage,aLogDateTime,aSeverityType);
        end;
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
            Result := FProviders[I].AddLog(aLogMessage,aLogDateTime,aSeverityType);
        end;
  end;
end;

procedure TNovusLogger.AddLogSuccess(aLogMessage: string;
  aProviderClass: TNovusLogger_ProviderClass = nil);
begin
  if aProviderClass = nil then
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogSuccess(aLogMessage);
        end;
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogSuccess(aLogMessage);
        end;
  end;
end;

procedure TNovusLogger.AddLogInformation(aLogMessage: string;
  aProviderClass: TNovusLogger_ProviderClass = nil);
begin
  If aProviderClass = nil then
  begin
    for var I := High(FProviders) downto Low(FProviders) do
      If Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogInformation(aLogMessage);
        end;
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogInformation(aLogMessage);
        end;
  end;
end;

procedure TNovusLogger.AddLogError(aLogMessage: string;
  aProviderClass: TNovusLogger_ProviderClass = nil);
begin
  If aProviderClass = nil then
  begin
    for var I := High(FProviders) downto Low(FProviders) do
      If Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogError(aLogMessage);
        end;
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogError(aLogMessage);
        end;
  end;
end;

procedure TNovusLogger.AddLogError(aException: Exception;
  aProviderClass: TNovusLogger_ProviderClass = nil);
begin
  If aProviderClass = nil then
  begin
    for var I := High(FProviders) downto Low(FProviders) do
      If Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogError(aException);
        end;
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogError(aException);
        end;
  end;
end;

procedure TNovusLogger.AddLogWarning(aLogMessage: string;
  aProviderClass: TNovusLogger_ProviderClass = nil);
begin
  If aProviderClass = nil then
  begin
    for var I := High(FProviders) downto Low(FProviders) do
      If Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogWarning(aLogMessage);
        end;
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
             FProviders[I].AddLogWarning(aLogMessage);
        end;
  end;
end;

function TNovusLogger.AddLogException(aProviderClass
  : TNovusLogger_ProviderClass = nil): String;
begin
  var
  lsMessage := TNovusUtilities.GetExceptMess;
  Result := lsMessage;

  If aProviderClass = nil then
  begin
    for var I := High(FProviders) downto Low(FProviders) do
      If Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogException(lsMessage);
        end;
  end
 else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogException(lsMessage);
        end;
  end;
end;

procedure TNovusLogger.AddLogDebug(aLogMessage: string;
  aProviderClass: TNovusLogger_ProviderClass = nil);
begin
  If aProviderClass = nil then
  begin
    for var I := High(FProviders) downto Low(FProviders) do
      If Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogDebug(aLogMessage);
        end;
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogDebug(aLogMessage);
        end;
  end;
end;

procedure TNovusLogger.AddLogSystem(aLogMessage: string;
  aProviderClass: TNovusLogger_ProviderClass = nil);
begin
  If aProviderClass = nil then
  begin
    for var I := High(FProviders) downto Low(FProviders) do
      If Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
           FProviders[I].AddLogSystem(aLogMessage);
        end;
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogSystem(aLogMessage);
        end;
  end;
end;

function TNovusLogger.AddLogException(aLogMessage: string;
  aProviderClass: TNovusLogger_ProviderClass = nil): string;
begin
  If aProviderClass = nil then
  begin
    for var I := High(FProviders) downto Low(FProviders) do
      If Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
            Result := FProviders[I].AddLogException(aLogMessage);
        end;
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
            Result := FProviders[I].AddLogException(aLogMessage);
        end;
  end;
end;

procedure TNovusLogger.AddLogException(aException: Exception;
  aProviderClass: TNovusLogger_ProviderClass = nil);
begin
  If aProviderClass = nil then
  begin
    for var I := High(FProviders) downto Low(FProviders) do
      If Assigned(FProviders[I]) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogException(aException);
        end;
  end
  else
  begin
    for var I := Low(FProviders) to High(FProviders) do
      if Assigned(FProviders[I]) and (FProviders[I] is aProviderClass) then
        begin
          if FProviders[I].Active then
            FProviders[I].AddLogException(aException);
        end;
  end;
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
  NewTask := TTask.Create(
    procedure
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
      FIsRunning := true;
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
