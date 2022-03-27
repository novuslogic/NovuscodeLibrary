unit NovusLogger;

interface

Uses NovusObject, NovusLog;

type
  TNovusLogger_Target = class(TNovusObject)
  private
  protected
     function GetLogTargetName: String; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property LogTargetName: string
      read GetLogTargetName;
  end;

  TNovusLogger = class(tNovusObject)
  private
  protected
  public
    constructor Create(aTarget: array of TNovusLogger_Target); virtual;
    destructor Destroy; override;
  end;

implementation


// TNovusLogger
constructor TNovusLogger.Create(aTarget: array of TNovusLogger_Target);
begin
end;

destructor TNovusLogger.Destroy;
begin
end;

//TNovusLogger_Target
constructor TNovusLogger_Target.Create;
begin
end;

destructor TNovusLogger_Target.Destroy;
begin
end;

function TNovusLogger_Target.GetLogTargetName: String;
begin
  Result := '';
end;


end.
