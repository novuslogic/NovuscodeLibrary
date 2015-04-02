unit NovusXMLBO;

interface

Uses NovusBO, JvSimpleXML, SysUtils, NovusUtilities, NovusSimpleXML,
     NovusStringUtils, Classes;

Type
  TNovusXMLBO = Class(TNovusBO)
  private
  protected
    fbFreeObject: Boolean;
    fNodeNames: tStringlist;
    fsXMLFileName: String;
    foXMLDocument: TJvSimpleXML;
    function  GetoXMLDocument : TJvSimpleXML; virtual;
    procedure SetoXMLDocument(Value : TJvSimpleXML); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SaveXML;
    procedure LoadXML;

    function FindNode(aNodeList: TJvSimpleXmlElem; NodeName: String; Var Index: Integer): TJvSimpleXmlElem;

    function AddFieldAsString(aNodeList: TJvSimpleXmlElem;FieldName: String;PropertyName: String;Value: String): TJvSimpleXmlElem;
    function AddFieldAsBoolean(aNodeList: TJvSimpleXmlElem;FieldName: String;PropertyName: String;Value: Boolean): TJvSimpleXmlElem;
    procedure SetFieldAsBoolean(aNodeList: TJvSimpleXmlElem;FieldName: String; Value: Boolean);
    procedure SetFieldAsinteger(aNodeList: TJvSimpleXmlElem;FieldName: String; Value: integer);
    procedure SetFieldAsString(aNodeList: TJvSimpleXmlElem;FieldName: String; Value: String);
    function GetFieldAsinteger(aNodeList: TJvSimpleXmlElem;FieldName: String): integer;
    function GetFieldAsBoolean(aNodeList: TJvSimpleXmlElem;FieldName: String): Boolean;
    function GetFieldAsString(aNodeList: TJvSimpleXmlElem; FieldName: String): String;

    Property oXMLDocument: TJvSimpleXML
      read GetoXMLDocument
      write SetoXMLDocument;

    Property XMLFileName: string
      read fsXMLFileName
      write fsXMLFileName;

    property NodeNames: tStringList
      read fNodeNames
      write fNodeNames;

    procedure New; override;
    function Retrieve: Boolean; override;
    function Post: Boolean; override;
    procedure DeleteXML(aNodeList: TJvSimpleXmlElem); virtual;

    property FreeObject: Boolean
      read fbFreeObject
      write fbFreeObject;
  end;

implementation

constructor TNovusXMLBO.Create;
begin
  fsXMLFileName := '';

  inherited Create;

  fNodeNames := tStringlist.Create;

  foXMLDocument := TJvSimpleXML.Create(NIL);

  fbFreeObject := True;
end;

destructor TNovusXMLBO.Destroy;
begin
  fNodeNames.Free;

  if FreeObject then
    begin
      foXMLDocument.Free
    end;

  inherited;
end;

procedure TNovusXMLBO.New;
begin
  inherited New;
end;

function TNovusXMLBO.Retrieve: Boolean;
begin
  Result := inherited Retrieve;

  Result := False;

  If Assigned(oXMLDocument) then
   If FileExists(XMLFilename) then
     begin
       LoadXML;

       TNovusSimpleXML.ListNodeNames(oXMLDocument.Root, fNodeNames);

       fbIsNewRec := False;

       Result := True;
     end;
end;

procedure TNovusXMLBO.LoadXML;
begin
  oXMLDocument.LoadFromFile(XMLFilename);
end;


function TNovusXMLBO.Post: Boolean;
begin
  Result := inherited Retrieve;

  Result := False;

  If Assigned(oXMLDocument) then
   If FileExists(XMLFilename) then
     begin
       SaveXML;

       fbIsNewRec := False;

       Result := True;
     end;
end;

procedure TNovusXMLBO.SaveXML;
begin
  oXMLDocument.SaveToFile(XMLFilename);
end;

function  TNovusXMLBO.GetoXMLDocument :TJvSimpleXML;
begin
  Result := foXMLDocument;
end;

procedure TNovusXMLBO.SetoXMLDocument(Value : TJvSimpleXML);
begin
  TNovusUtilities.FreeObject(foXMLDocument);
  if (Value <> nil) then
    foXMLDocument := value;
end;

function TNovusXMLBO.GetFieldAsBoolean(aNodeList: TJvSimpleXmlElem;FieldName: String): Boolean;
begin
  result := false;
  If uppercase(GetFieldAsString(aNodeList,FieldName)) = 'TRUE' then
    Result := True;
end;

function TNovusXMLBO.GetFieldAsInteger(aNodeList: TJvSimpleXmlElem;FieldName: String): Integer;
begin
  Result := TNovusStringUtils.StrToInt(GetFieldAsString(aNodeList,FieldName));
end;

function TNovusXMLBO.AddFieldAsString(aNodeList: TJvSimpleXmlElem;FieldName: String;PropertyName: String;Value: String): TJvSimpleXmlElem;
begin
  Result := NIL;

  If Trim(PropertyName) <> '' then
    begin
      Result := aNodeList.Items.Add(FieldName);
      Result.Properties.Add(PropertyName, Value)
    end
  else
    Result := aNodeList.Items.Add(FieldName, Value);
end;


function TNovusXMLBO.AddFieldAsBoolean(aNodeList: TJvSimpleXmlElem;FieldName: String;PropertyName: String;Value: Boolean): TJvSimpleXmlElem;
begin
  Result := AddFieldAsString(aNodeList,FieldName,PropertyName,TNovusStringUtils.BooleanToStr(Value));
end;

procedure TNovusXMLBO.SetFieldAsString(aNodeList: TJvSimpleXmlElem;FieldName: String; Value: String);
Var
  Index: Integer;
  fJvSimpleXmlElem: TJvSimpleXmlElem;
begin
  Index := 0;
  fJvSimpleXmlElem := TNovusSimpleXML.FindNode(aNodeList, FieldName,Index);

  If Assigned(fJvSimpleXmlElem) then
    fJvSimpleXmlElem.Value := Value;
end;

procedure TNovusXMLBO.SetFieldAsBoolean(aNodeList: TJvSimpleXmlElem;FieldName: String; Value: Boolean);
begin
  If Value = True then SetFieldAsString(aNodeList,FieldName, 'True')
  else
    SetFieldAsString(aNodeList,FieldName, 'False');
end;

procedure TNovusXMLBO.SetFieldAsInteger(aNodeList: TJvSimpleXmlElem;FieldName: String; Value: Integer);
begin
  SetFieldAsString(aNodeList,FieldName, InttoStr(Value))
end;

function TNovusXMLBO.GetFieldAsString(aNodeList: TJvSimpleXmlElem; FieldName: String): String;
Var
  Index: Integer;
  fJvSimpleXmlElem: TJvSimpleXmlElem;
begin
  Result := '';

  If Not Assigned(aNodeList) then Exit;

  Index := 0;
  fJvSimpleXmlElem := TNovusSimpleXML.FindNode(aNodeList, FieldName,Index);

  If Assigned(fJvSimpleXmlElem) then
    Result := fJvSimpleXmlElem.Value;
end;

procedure TNovusXMLBO.DeleteXML(aNodeList: TJvSimpleXmlElem);
begin
  If Not Assigned(aNodeList) then Exit;

  foXMLDocument.Root.Items.Delete(aNodeList.Name);

  Post;
end;


function TNovusXMLBO.FindNode(aNodeList: TJvSimpleXmlElem; NodeName: String; Var Index: Integer): TJvSimpleXmlElem;
begin
  Result := NIL;

  if Assigned(aNodeList) then
    Result := TNovusSimpleXML.FindNode(aNodeList, NodeName, Index);
end;


end.




