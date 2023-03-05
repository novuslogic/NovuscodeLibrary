{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusBO;

interface

Uses NovusObject, (*Activex, ComObj,*) Classes, SysUtils,
  NovusBOField, NovusBOMap, NovusUtilities, DBXJson, DB, NovusDateUtils,
{$IFDEF DELPHIXE6_UP}
  JSON, System.Variants,
{$ENDIF}
  NovusStringUtils, NovusDateStringUtils, NovusGUID;

Type

  TNovusBO = Class(TNovusObject)
  private
  protected
    fsLastExceptionMessage: String;
    FoNovusBOMap: TNovusBOMap;

    fGUID: tGUID;
    foChildList: tList;
    foFormObject: tObject;
    fsGUIDString: String;
    fbIsNewRec: Boolean;
    foParentObject: tObject;

    function GetoChildList: tList; virtual;
    function GetImageIndex: Integer; virtual;
    function GetIsNewRec: Boolean; virtual;
    procedure SetIsNewRec(Value: Boolean);
    function GetGUIDID: String;
    procedure SetGUIDID(Value: String);

    function GetEOF: Boolean; virtual;
    function GetBOF: Boolean; virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure InitObjects; virtual;

    function GetText: String; virtual;
    function GetPrimary_ID: Integer; virtual;
    function GetIsMandtoryRecord: Boolean; virtual;

    procedure CopyFromJSONObject(aJSONObject: TJSONObject;
      aFieldName: string = ''); virtual;
    function ToJSONObject: TJSONObject; virtual;
    function ToJSON: String;
    function ToJSONNoBrackets: String;

    procedure PropertiesToFields; virtual;
    procedure FieldsToProperties; overload; virtual;
    procedure FieldsToProperties(aDataSet: TDataSet); overload; virtual;

    procedure SetupFields; virtual;

    procedure First; virtual;
    procedure Next; virtual;

    procedure Open; virtual;
    procedure Close; virtual;

    procedure New; virtual;
    procedure Clear; virtual;

    function Retrieve: Boolean; virtual;

    function Post: Boolean; virtual;
    function Delete: Boolean; virtual;

    function NewBO: TNovusBO; virtual;
    function CloneBO: TNovusBO; virtual;
    procedure CopyFrom(AFromNovusBO: TNovusBO); virtual;

    procedure CopyBOFields(AFromNovusBO: TNovusBO);

    procedure Sort; virtual;

    Property oParentObject: tObject read foParentObject write foParentObject;

    property BOF: Boolean read GetBOF;

    property EOF: Boolean read GetEOF;

    Property IsMandtoryRecord: Boolean read GetIsMandtoryRecord;

    Property IsNewRec: Boolean read GetIsNewRec write SetIsNewRec;

    Property GUIDString: String read fsGUIDString;

    Property oFormObject: tObject read foFormObject write foFormObject;

    property Text: String read GetText;

    property ImageIndex: Integer read GetImageIndex;

    Property oChildList: tList read GetoChildList;

    property GUIDID: String read GetGUIDID write SetGUIDID;

    property Primary_ID: Integer Read GetPrimary_ID;

    property oBOMap: TNovusBOMap read FoNovusBOMap write FoNovusBOMap;

    property LastExceptionMessage: String read fsLastExceptionMessage
      write fsLastExceptionMessage;
  end;

  TNovusBOClass = class of TNovusBO;

implementation

// uses NovusStringUtils;

constructor TNovusBO.Create;
begin
  FoNovusBOMap := TNovusBOMap.Create;

  inherited Create;

//  CoCreateGuid(fGUID);

  fGUID := TNovusGuid.NewGuid;

  fsGUIDString := GUIDToString(fGUID);

  fbIsNewRec := False;

  foParentObject := NIL;

  foChildList := NIL;

  SetupFields;
end;

destructor TNovusBO.Destroy;
begin
  inherited;

  FreeandNIL(FoNovusBOMap);

  If Assigned(foChildList) then
    FreeandNIL(foChildList);
end;

procedure TNovusBO.SetupFields;
begin
end;

function TNovusBO.GetImageIndex: Integer;
begin
  Result := -1;
end;

function TNovusBO.Retrieve: Boolean;
begin
  Result := False;
end;

function TNovusBO.Post: Boolean;
begin
  Result := False;

  fbIsNewRec := False;
end;

function TNovusBO.Delete: Boolean;
begin
  Result := False;

  fbIsNewRec := False;
end;

procedure TNovusBO.New;
begin
  fbIsNewRec := True;
  Clear;
end;

procedure TNovusBO.Clear;
begin
end;

function TNovusBO.GetText: String;
begin
  Result := '';
end;

function TNovusBO.GetoChildList;
begin
  If Not Assigned(foChildList) then
    foChildList := tList.Create;

  Result := foChildList;
end;

function TNovusBO.GetIsNewRec: Boolean;
begin
  Result := fbIsNewRec;
end;

procedure TNovusBO.SetIsNewRec(Value: Boolean);
begin
  fbIsNewRec := Value;
end;

procedure TNovusBO.CopyFrom(AFromNovusBO: TNovusBO);
begin
end;

procedure TNovusBO.CopyBOFields(AFromNovusBO: TNovusBO);
Var
  I: Integer;
  loNovusBOField1: tNovusBOField;
  loNovusBOField2: tNovusBOField;
begin
  if Not Assigned(AFromNovusBO) then
    Exit;

  if Assigned(AFromNovusBO.oBOMap) then
  begin
    For I := 0 to AFromNovusBO.oBOMap.oFieldList.Count - 1 do
    begin
      loNovusBOField1 := tNovusBOField(AFromNovusBO.oBOMap.oFieldList[I]);

      loNovusBOField2 := oBOMap.FieldByName(loNovusBOField1.FieldName);
      if Assigned(loNovusBOField2) then
        loNovusBOField2.Value := loNovusBOField1.Value;
    end;
  end;
end;

function TNovusBO.CloneBO: TNovusBO;
begin
  Result := NIL;
end;

function TNovusBO.NewBO: TNovusBO;
begin
  Result := NIL;
end;

function TNovusBO.GetGUIDID: String;
begin
  Result := fsGUIDString;
end;

procedure TNovusBO.SetGUIDID(Value: String);
begin
  fsGUIDString := Value;

  fGUID := StringToGUID(fsGUIDString);
end;

function TNovusBO.GetIsMandtoryRecord: Boolean;
begin
  Result := False;
end;

function TNovusBO.GetPrimary_ID: Integer;
begin
  Result := 0;
end;

procedure TNovusBO.First;
begin
end;

procedure TNovusBO.Next;
begin
end;

function TNovusBO.GetEOF: Boolean;
begin
  Result := True;

end;

function TNovusBO.GetBOF: Boolean;
begin
  Result := False;
end;

procedure TNovusBO.Open;
begin
end;

procedure TNovusBO.Close;
begin
end;

procedure TNovusBO.Sort;
begin
end;

procedure TNovusBO.PropertiesToFields;
begin
end;

procedure TNovusBO.FieldsToProperties;
begin
end;

procedure TNovusBO.FieldsToProperties(aDataSet: TDataSet);
begin
end;

procedure TNovusBO.InitObjects;
begin
end;

procedure TNovusBO.CopyFromJSONObject(aJSONObject: TJSONObject;
  aFieldName: string = '');
var
  loBOField: tNovusBOField;
  I: Integer;
  LJPair: TJSONPair;

  procedure PassField(var aBOField: tNovusBOField; aLJPair: TJSONPair);

  begin
    if aBOField is TnovusBODateTimeField then
    begin
      aBOField.Value := TNovusDateUtils.UnixTimeToDateTime
        (TNovusDateStringUtils.JSONDateStr2UnixTime
        (TJSONString(aLJPair.JsonValue).Value));
    end
    else if aBOField is TNovusBOIntegerField then
      aBOField.Value := TJSONNumber(aLJPair.JsonValue).AsInt
    else if aBOField is TNovusBOStringField then
      aBOField.Value := TJSONString(aLJPair.JsonValue).Value
    else if aBOField is TNovusBOSmallintField then
    begin
      if (tNovusStringUtils.IsBoolean(aLJPair.JsonValue.ToString)) or
        TNovusBOSmallintField(loBOField).UseAsBoolean then
      begin
        aBOField.Value :=
          Smallint(tNovusStringUtils.StrToBoolean(aLJPair.JsonValue.ToString));
      end
      else
        aBOField.Value := Smallint(TJSONNumber(aLJPair.JsonValue).AsInt);
    end;
  end;

begin
  if Not Assigned(aJSONObject) then
    Exit;

  for I := 0 to aJSONObject.Size - 1 do
  begin
    LJPair := aJSONObject.Get(I);

    loBOField := oBOMap.FieldByName(TJSONPair(LJPair).JsonString.Value);
    if Assigned(loBOField) then
    begin
      if aFieldName = '' then
      begin
        PassField(loBOField, LJPair);
      end
      else if aFieldName = loBOField.FieldName then
      begin
        PassField(loBOField, LJPair);

        break;
      end;
    end;
  end;
end;

function TNovusBO.ToJSONObject: TJSONObject;
var
  loJSONObject: TJSONObject;
  I: Integer;
  loBOField: tNovusBOField;
begin
  loJSONObject := TJSONObject.Create;

  for I := 0 to oBOMap.oFieldList.Count - 1 do
  begin
    loBOField := tNovusBOField(oBOMap.oFieldList.Items[I]);

    if loBOField.ToJSON then
    begin
      if loBOField is TnovusBODateTimeField then
      begin
        if loBOField.Value <> 0 then
          loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName,
            TJSONString.Create(TNovusDateUtils.DateTimeToISO8601
            (loBOField.Value))));
      end
      else if loBOField is TNovusBOStringField then
        loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName,
          TJSONString.Create(loBOField.Value)))
      else
      if loBOField is TNovusBOIntegerField then
      {$IFDEF DELPHI11_UP}
         loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONNumber.Create(VarToStr(loBOField.Value))))
      {$ELSE}
         loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONNumber.Create(loBOField.Value)))
      {$ENDIF}
      else if loBOField is TNovusBOFloatField then
      {$IFDEF DELPHI11_UP}
         loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONNumber.Create(VarToStr(loBOField.Value))))
      {$ELSE}
         loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONNumber.Create(loBOField.Value)))
      {$ENDIF}

      else if loBOField is TNovusBOBooleanField then
        loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName,
          TJSONString.Create(tNovusStringUtils.BooleanToStr(TNovusBOBooleanField
          (loBOField).AsBoolean))))
      else if loBOField is TNovusBOSmallintField then
      begin
        if Not TNovusBOSmallintField(loBOField).UseAsBoolean then
        {$IFDEF DELPHI11_UP}
           loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONNumber.Create(VarToStr(loBOField.Value))))
        {$ELSE}
           loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONNumber.Create(loBOField.Value)))
        {$ENDIF}
        else
          loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName,
            TJSONString.Create(tNovusStringUtils.BooleanToStr
            (TNovusBOSmallintField(loBOField).AsBoolean))));
      end;
    end;
  end;

  Result := loJSONObject;
end;

function TNovusBO.ToJSON: String;
var
  loJSONObject: TJSONObject;
begin
  loJSONObject := ToJSONObject;

  Result := loJSONObject.ToString;
  loJSONObject.Free;
end;

function TNovusBO.ToJSONNoBrackets: String;
begin
  Result := tNovusStringUtils.StripChar(ToJSON, '{');

  Result := tNovusStringUtils.StripChar(Result, '}');
end;

end.
