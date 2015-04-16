unit NovusBOField;

interface

Uses NovusInfrastructre, DB, NovusStringUtils, SysUtils, Variants, Classes, math;


Type
  TNovusBOFieldType  = (nftUnknown,
    nftString,
    nftInteger,
    nftBoolean,
    nftFloat,
    nftCurrency,
    nftDateTime,
    nftLargeInt,
    nftBlob     ,
    nftSmallInt
    );

  TNovusBOField = class(tNovusInfrastructre)
  private
  protected
    fbToJSON: Boolean;
    fsFieldName: String;
    FValue: Variant;
    fiSize: Integer;
    fFieldType: TNovusBOFieldType;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function IsClear: Boolean;

    property FieldName: String
       read fsFieldName
       write fsFieldName;

    property Value: Variant
      read FValue
      write FValue;

    property ToJSON: Boolean
      read fbToJSON
      write fbToJSON;

    property Size: Integer
      read fiSize
      write fiSize;

    property FieldType: TNovusBOFieldType
      read FFieldType
      write FFieldType;
  end;

  TNovusBOIntegerField = class(TNovusBOField)
  private
  protected
    function GetAsString: String;
    function GetAsInteger: Integer;
    procedure SetAsInteger(AValue: Integer);
  public
    constructor Create; override;

    property AsInteger: Integer
      read GetAsInteger
      write SetAsInteger;

    property AsString: String
      read GetAsString;
  end;

  TNovusBOFloatField = class(TNovusBOField)
  private
  protected
    function GetAsFloat: extended;
    procedure SetAsFloat(AValue: extended);
  public
    constructor Create; override;

    property AsFloat: extended
      read GetAsFloat
      write SetAsFloat;
  end;

  TNovusBONumericField = class(TNovusBOFloatField)
  private
  protected
    function GetAsNumeric: Double;
    procedure SetAsNumeric(AValue: Double);
    function GetAsDouble: Double;
    procedure SetAsDouble(AValue: Double);
    function GetAsCurrency: Currency;
    procedure SetAsCurrency(AValue: Currency);
  public
    constructor Create; override;

    property AsNumeric: extended
      read GetAsNumeric
      write SetAsNumeric;

    property AsDouble: extended
      read GetAsDouble
      write SetAsDouble;

    property AsDouble: extended
      read GetAsDouble
      write SetAsDouble;
  end;

  TNovusBOStringField = class(TNovusBOField)
  private
  protected
    function GetAsAnsiString: AnsiString;
    procedure SetAsAnsiString(aValue: AnsiString);

    function GetAsString: String;
    procedure SetAsString(AValue: String);
  public
    constructor Create; override;

    property AsString: String
      read GetAsString
      write SetAsString;

    property AsAnsiString: AnsiString
      read GetAsAnsiString
      write SetAsAnsiString;
  end;

  TNovusBOBlobField = class(TNovusBOField)
  private
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    property AsBlob: variant
      read FValue
      write FValue;
  end;

  TNovusBOSmallIntField = class(TNovusBOField)
  private
  protected
    fbUseAsBoolean: Boolean;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(AValue: Boolean);

    function GetAsSmallInt: SmallInt;
    procedure SetAsSmallInt(AValue: SmallInt);
  public
    constructor Create; override;

    property AsBoolean: Boolean
      read GetAsBoolean
      write SetAsBoolean;

    property AsSmallInt: SmallInt
      read GetAsSmallInt
      write SetAsSmallInt;

    property UseAsBoolean: Boolean
      read fbUseAsBoolean
      write fbUseAsBoolean;

  end;

  TNovusBOBooleanField = class(TNovusBOField)
  private
  protected
    FValue: Boolean;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(AValue: Boolean);
  public
    constructor Create; override;

    property AsBoolean: Boolean
      read GetAsBoolean
      write SetAsBoolean;

  end;

  TNovusBODateTimeField = class(TNovusBOField)
  private
  protected
    fsDateFormat: String;
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(AValue: TDateTime);
    function GetAsString: String;
  public
    constructor Create; override;

    property AsString: String
      read GetAsString;

    property DateFormat: String
      read fsDateFormat
      write fsDateFormat;

    property AsDateTime: TDateTime
      read GetAsDateTime
      write SetAsDateTime;
  end;

implementation

// NovusBOField

constructor TNovusBOField.create;
begin
  inherited create;

  fbToJSON := True;
end;

destructor TNovusBOField.destroy;
begin
  inherited destroy;
end;

function TNovusBOField.IsClear: Boolean;
begin
  Result := VarIsClear(FValue);
end;


//NovusBOIntegerFieldType

constructor TNovusBOIntegerField.create;
begin
  inherited create;

  FieldType := TNovusBOFieldType.nftInteger;
end;

function TNovusBOIntegerField.GetAsInteger: Integer;
begin
  Result := FValue;
end;

function TNovusBOIntegerField.GetAsString: String;
begin
  Result := IntToStr(FValue);
end;

procedure TNovusBOIntegerField.SetAsInteger(AValue: Integer);
begin
  FValue := AValue;
end;

//NovusBOStringField

constructor TNovusBOStringField.create;
begin
  inherited create;

  FieldType := TNovusBOFieldType.nftString;
end;

function TNovusBOStringField.GetAsString: String;
begin
  Result := FValue;
end;

procedure TNovusBOStringField.SetAsString(AValue: String);
begin
  FValue := AValue;
end;

function TNovusBOStringField.GetAsAnsiString: AnsiString;
begin
  Result := FValue;
end;

procedure TNovusBOStringField.SetAsAnsiString(aValue: AnsiString);
begin
  FValue := aValue;
end;

// NovusBOBooleanField
constructor TNovusBOBooleanField.create;
begin
  inherited create;

  FieldType := TNovusBOFieldType.nftBoolean;
end;

function TNovusBOBooleanField.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

procedure TNovusBOBooleanField.SetAsBoolean(AValue: Boolean);
begin
  FValue := AValue;
end;




//NovusBOSmallIntField
constructor TNovusBOSmallIntField.create;
begin
  inherited create;

  FieldType := TNovusBOFieldType.nftSmallInt;
end;


function TNovusBOSmallIntField.GetAsSmallInt: SmallInt;
begin
  Result := FValue;
end;

procedure TNovusBOSmallIntField.SetAsSmallInt(AValue: SmallInt);
begin
  FValue := AValue;
end;

function TNovusBOSmallIntField.GetAsBoolean: Boolean;
begin
  Result := (FValue = 1);
end;

procedure TNovusBOSmallIntField.SetAsBoolean(AValue: Boolean);
begin
  if AValue then FValue := 1
  else FValue := 0;
end;

//NovusBODateTimeField
constructor TNovusBODateTimeField.create;
begin
  inherited create;

  FieldType := TNovusBOFieldType.nftDateTime;
end;


function TNovusBODateTimeField.GetAsDateTime: TDateTime;
begin
  Result := FValue;
end;

function TNovusBODateTimeField.GetAsString;
begin
  Result := '';

  if Fvalue = 0 then Exit;

  DateTimeToString(Result, fsDateFormat, FValue);
end;

procedure TNovusBODateTimeField.SetAsDateTime(AValue: TDateTime);
begin
  FValue := AValue;
end;

//TNovusBlobField
constructor TNovusBOBlobField.create;
begin
  inherited create;

  FieldType := TNovusBOFieldType.nftBlob;
end;

destructor TNovusBOBlobField.destroy;
begin
  inherited destroy;
end;


// NovusBOFloatField
constructor TNovusBOFloatField.create;
begin
  inherited create;

  FieldType := TNovusBOFieldType.nftFloat;
end;

function TNovusBOFloatField.GetAsFloat: extended;
begin
  Result := FValue;
end;

procedure TNovusBOFloatField.SetAsFloat(AValue: extended);
begin
  FValue := AValue;
end;


// TNovusBONumericField

function TNovusBOnumericField.GetAsNumeric: Double;
begin
  Result := FValue;
end;

procedure TNovusBOnumericField.SetAsNumeric(AValue: Double);
begin
  FValue := AValue;
end

function TNovusBOnumericField.GetAsDouble: Double;
begin
  Result := FValue;
end;

procedure TNovusBOnumericField.SetAsDouble(AValue: Double);
begin
  FValue := AValue;
end

function TNovusBOnumericField.GetAsCurrency: Double;
begin
  Result := FValue;
end;

procedure TNovusBOnumericField.SetAsCurrency(AValue: Double);
begin
  FValue := AValue;
end



end.
