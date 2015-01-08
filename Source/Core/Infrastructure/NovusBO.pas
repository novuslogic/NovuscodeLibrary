unit NovusBO;

interface

Uses NovusInfrastructre, Activex, ComObj, Classes, SysUtils,
     NovusBOField,  NovusBOMap, NovusUtilities, DBXJson, DB;

Type

  TNovusBO = Class(TNovusInfrastructre)
  private
  protected
    fsLastExceptionMessage: String;
    FoNovusBOMap: TNovusBOMap;

    fGUID: tGUID;
    foChildList: tList;
    foFormObject: tObject;
    fsGUIDString: String;
    fbIsNewRec: Boolean;
    foParentObject: TObject;

    function GetoChildList: TList; virtual;
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

    procedure CopyFromJSONObject(aJSONObject: TJSONObject; aFieldName: string = ''); virtual;
    function  ToJSONObject: TJSONobject; virtual;
    function  ToJSON: String;
    function  ToJSONNoBrackets: String;

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

    function NewBO: tNovusBO; virtual;
    function CloneBO: tNovusBO; virtual;
    procedure CopyFrom(AFromNovusBO: tNovusBO); virtual;

    procedure CopyBOFields(AFromNovusBO: tNovusBO);

    procedure Sort; virtual;

    Property oParentObject: TObject
      read foParentObject
      write  foParentObject;

    property BOF: Boolean
       read GetBOF;

    property EOF: Boolean
       read GetEOF;

    Property IsMandtoryRecord: Boolean
      read GetIsMandtoryRecord;

    Property IsNewRec: Boolean
      read GetIsNewRec
      write SetIsNewRec;

    Property GUIDString: String
      read fsGUIDString;

    Property oFormObject: tObject
      read foFormObject
      write foFormObject;

    property Text: String
      read GetText;

    property ImageIndex: Integer
      read GetImageIndex;

    Property oChildList: tList
       read GetoChildList;

    property GUIDID: String
      read GetGUIDID
      write SetGUIDID;

    property Primary_ID: Integer
      Read GetPrimary_ID;

    property oBOMap: tNovusBOMap
      read FoNovusBOMap
      write FoNovusBOMap;

    property LastExceptionMessage: String
      read fsLastExceptionMessage
      write fsLastExceptionMessage;
  end;

  TNovusBOClass = class of TNovusBO;

implementation

uses NovusStringUtils;

constructor TNovusBO.Create;
begin
  FoNovusBOMap := TNovusBOMap.create;

  inherited Create;

  CoCreateGuid(fGUID);

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

procedure TNovusBO.CopyFrom(AFromNovusBO: tNovusBO);
begin
end;

procedure TNovusBO.CopyBOFields(AFromNovusBO: tNovusBO);
Var
  I: Integer;
  loNovusBOField1: tNovusBOField;
  loNovusBOField2: tNovusBOField;
begin
  if Not Assigned(AFromNovusBO) then Exit;

  if Assigned(AFromNovusBO.oBOMap) then
    begin
      For I := 0 to AFromNovusBO.oBOMap.oFieldList.Count -1 do
        begin
          loNovusBOField1 := tNovusBOField(AFromNovusBO.oBOMap.oFieldList[i]);

          loNovusBOField2 := oBOMap.FieldByName(loNovusBOField1.FieldName);
          if Assigned(loNovusBOField2) then
            loNovusBOField2.Value := loNovusBOField1.Value;
        end;
    end;
end;

function TNovusBO.CloneBO: tNovusBO;
begin
  Result := NIL;
end;

function TNovusBO.NewBO: tNovusBO;
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


procedure TNovusBO.CopyFromJSONObject(aJSONObject: TJSONObject; aFieldName: string = '');
var
  loBOField : TNovusBOField;
  I: Integer;
  LJPair    : TJSONPair;

  procedure PassField(var aBOField : TNovusBOField; aLJPair: TJSONPair);
  begin
    if aBOField is TNovusBOIntegerField then
       aBOField.value := TJSONNumber(aLJPair.JsonValue).AsInt
    else
    if aBOField is TNovusBOStringField then
       aBOField.value := TJSONString(aLJPair.JsonValue).Value
    else
    if aBOField is TNovusBOSmallintField then
      begin
        if (tNovusStringUtils.IsBoolean(aLJPair.JsonValue.ToString)) or TNovusBOSmallIntField(loBOField).UseAsBoolean then
          begin
           aBOField.value := Smallint(tNovusStringUtils.StrToBoolean(aLJPair.JsonValue.ToString));
          end
       else
          aBOField.value := Smallint(TJSONNumber(aLJPair.JsonValue).AsInt);
      end;
  end;



begin
  if Not Assigned(aJSONObject) then Exit;

   for I := 0 to aJSONObject.Size -1 do
     begin
       LJPair  :=  aJSONObject.Get(i);

       loBOField := oBOMap.FieldByName(TJSONPair(LJPair).JsonString.Value);
       if Assigned(loBOField) then
         begin
           if aFieldName = '' then
             begin
               PassField(loBOField,LJPair);
             end
           else
           if aFieldName = loBOField.FieldName then
             begin
               PassField(loBOField,LJPair);

               break;
             end;
         end;
     end;
end;

function TNovusBO.ToJSONObject: TJSONobject;
var
   loJSONObject: TJSONObject;
   I: Integer;
   LJValue    : TJSONValue;
   loBOField: TNovusBOField;
begin
  loJSONObject:= TJSONObject.Create;

  for I := 0 to oBOMap.oFieldList.Count -1 do
    begin
      loBOField := TNovusBOField(oBOMap.oFieldList.Items[i]);

      if loBOField.ToJSON then
        begin
          if loBOField is  TNovusBOStringField then
            loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONString.Create(loBOField.Value)))
          else
          if loBOField is TNovusBOIntegerField then
            loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONNumber.Create(loBOField.Value)))
          else
          if loBOField is TNovusBOFloatField then
            loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONNumber.Create(loBOField.Value)))
          else
          if loBOField is  TNovusBOBooleanField then
            loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONString.Create(TNovusStringUtils.BooleanToStr(TNovusBOBooleanField(loBOField).AsBoolean))))
          else
          if loBOField is  TNovusBOSmallIntField then
            begin
              if Not TNovusBOSmallIntField(loBOField).UseAsBoolean then
                loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONNumber.Create(loBOField.Value)))
              else
                loJSONObject.AddPair(TJSONPair.Create(loBOField.FieldName, TJSONString.Create(TNovusStringUtils.BooleanToStr(TNovusBOSmallIntField(loBOField).AsBoolean))));
            end;
        end;
    end;

  result := loJSONObject;
end;

function TNovusBO.ToJSON: String;
var
  loJSONObject: TJSONObject;
begin
  loJSONObject := ToJSONObject;

  result := loJSONObject.ToString;
  loJSONObject.Free;
end;

function TNovusBO.ToJSONNoBrackets: String;
begin
  Result := TNovusStringUtils.StripChar(ToJSON, '{');

  Result := TNovusStringUtils.StripChar(Result, '}');
end;

end.




