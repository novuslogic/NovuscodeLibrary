unit NovusLogger;

interface

Uses NovusObject, NovusLogger.Provider, System.SysUtils, NovusUtilities,
     System.Threading, System.Classes, System.Generics.Collections, System.Types;

type
  TNovusLoggerData = record
  private
    FProvider: TNovusLogger_Provider;
    fsLogMessage: String;
  public
    property Provider: TNovusLogger_Provider
      read FProvider
      write FProvider;

    property LogMessage: String
      read fsLogMessage
      write fsLogMessage;
  end;

  TLoggerThread = class(TThread)
  private
    FLogQueue: TThreadedQueue<TNovusLoggerData>;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; LogQueue: TThreadedQueue<TNovusLoggerData>);
  end;


  TNovusLogger = class(tNovusObject)
  private
  protected
    FProviders: array of TNovusLogger_Provider;
    FLogThreadedQueue: TThreadedQueue<TNovusLoggerData>;
  public
    procedure PushLogMessage(aMessage: string; aProvider: TNovusLogger_Provider); virtual;

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
  FLogThreadedQueue := TThreadedQueue<TNovusLoggerData>.Create(TThread.ProcessorCount, 1000, 100);

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

  for var I := High(FProviders) downto Low(FProviders) do
    If Assigned(FProviders[I]) then
      begin
        FProviders[I].Free;
      end;

  FLogThreadedQueue.Free;

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

procedure TNovusLogger.PushLogMessage(aMessage: string; aProvider: TNovusLogger_Provider);
Var
  LoggerThread: TLoggerThread;
  LogData: TNovusLoggerData;
begin
   try
    LoggerThread := TLoggerThread.Create(True, FLogThreadedQueue);
    LoggerThread.Start;

    LogData.Provider := aProvider;

    FLogThreadedQueue.PushItem(LogData);
  finally
  end;
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

//TNovusThreadLog
constructor TLoggerThread.Create(CreateSuspended: Boolean; LogQueue: TThreadedQueue<TNovusLoggerData>);
begin
  inherited Create(CreateSuspended);
  FLogQueue := LogQueue;

  FreeOnTerminate := true;
end;

procedure TLoggerThread.Execute;
var
  LogData: TNovusLoggerData;
  QueueResult: TWaitResult;
begin
  while not Terminated do
  begin
    QueueResult := FLogQueue.PopItem(LogData);
    if QueueResult = TWaitResult.wrSignaled then
    begin
      LogData.Provider.SendLogMessage(Logdata.LogMessage);


      LogData.Provider := NIL;



    end
    else if QueueResult = TWaitResult.wrTimeout then
    begin
      // Handle timeout if needed
    end;
  end;
end;


end.
