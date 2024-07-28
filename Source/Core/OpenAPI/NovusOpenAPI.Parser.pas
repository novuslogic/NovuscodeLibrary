unit NovusOpenAPI.Parser;

interface

uses
  NovusObject, System.IOUtils, System.JSON, System.SysUtils,
  System.Generics.Collections,
  System.Classes, NovusStringUtils;

type
  TOpenAPI3SecurityRequirement = class(TObject)
  private
    fRequirements: TObjectDictionary<string, TArray<string>>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseFromJSON(AJSONArray: TJSONArray);
    property Requirements: TObjectDictionary < string, TArray < string >>
      read fRequirements write fRequirements;
  end;

  TOpenAPI3RequestBody = class(TObject)
  private
    fDescription: string;
    fContent: TJSONObject; // Content can be a complex object
    fRequired: Boolean;
  public
    property Description: string read fDescription write fDescription;
    property Content: TJSONObject read fContent write fContent;
    property Required: Boolean read fRequired write fRequired;
  end;

  TOpenAPI3Callback = class(TObject)
  private
    fCallback: TJSONObject; // Placeholder for the actual callback structure
  public
    property Callback: TJSONObject read fCallback write fCallback;
  end;

  TOpenAPI3Callbacks = class(TObject)
  private
    fCallbacks: TObjectDictionary<string, TObject>;
    // Can be TOpenAPI3Callback or TOpenAPI3Reference
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseFromJSON(AJSONObject: TJSONObject);
    property Callbacks: TObjectDictionary<string, TObject> read fCallbacks
      write fCallbacks;
  end;

  TOpenAPI3Response = class(TObject)
  private
    fDescription: string;
    fContent: TJSONObject; // Content can be a complex object
  public
    property Description: string read fDescription write fDescription;
    property Content: TJSONObject read fContent write fContent;
  end;

  TOpenAPI3Responses = class(TObject)
  private
    fResponses: TObjectDictionary<string, TOpenAPI3Response>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseFromJSON(AJSONObject: TJSONObject);
    property Responses: TObjectDictionary<string, TOpenAPI3Response>
      read fResponses write fResponses;
  end;

  TOpenAPI3ExternalDocs = class(TObject)
  private
    fDescription: string;
    fUrl: string;
  public
    property Description: string read fDescription write fDescription;
    property Url: string read fUrl write fUrl;
  end;

  TOpenAPI3Parameter = class(TObject)
  private
    fName: string;
    fIn: string;
    fDescription: string;
    fRequired: Boolean;
    fDeprecated: Boolean;
    fAllowEmptyValue: Boolean;
  public
    property Name: string read fName write fName;
    property InLocation: string read fIn write fIn;
    // 'In' is a reserved word, use InLocation instead
    property Description: string read fDescription write fDescription;
    property Required: Boolean read fRequired write fRequired;
    property Deprecated: Boolean read fDeprecated write fDeprecated;
    property AllowEmptyValue: Boolean read fAllowEmptyValue
      write fAllowEmptyValue;
  end;

  TOpenAPI3Reference = class(TObject)
  private
    fRef: string;
  public
    property Ref: string read fRef write fRef;
  end;

  TOpenAPI3Contact = class(TObject)
  private
    fName: string;
    fUrl: string;
    fEmail: string;
  public
    property Name: string read fName write fName;
    property Url: string read fUrl write fUrl;
    property Email: string read fEmail write fEmail;
  end;

  TOpenAPI3License = class(TObject)
  private
    fName: string;
    fUrl: string;
  public
    property Name: string read fName write fName;
    property Url: string read fUrl write fUrl;
  end;

  TOpenAPI3ServerVariable = class(TObject)
  private
    fDefault: string;
    fEnum: TArray<string>;
    fDescription: string;
  public
    property Default: string read fDefault write fDefault;
    property Enum: TArray<string> read fEnum write fEnum;
    property Description: string read fDescription write fDescription;
  end;

  TOpenAPI3Server = class(TObject)
  private
    fUrl: string;
    fDescription: string;
    fVariables: TObjectDictionary<string, TOpenAPI3ServerVariable>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseVariables(AJSONObject: TJSONObject);
    property Url: string read fUrl write fUrl;
    property Description: string read fDescription write fDescription;
    property Variables: TObjectDictionary<string, TOpenAPI3ServerVariable>
      read fVariables write fVariables;
  end;

  TOpenAPI3Operation = class(TObject)
  private
    fSummary: string;
    fDescription: string;
    fTags: TArray<string>;
    fExternalDocs: TOpenAPI3ExternalDocs;
    fOperationId: string;
    fParameters: TObjectList<TObject>;
    fResponses: TOpenAPI3Responses;
    fCallbacks: TOpenAPI3Callbacks;
    fDeprecated: Boolean;
    fRequestBody: TOpenAPI3RequestBody;
    fSecurity: TObjectList<TOpenAPI3SecurityRequirement>;
    fServers: TObjectList<TOpenAPI3Server>; // New property for servers
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseServers(AJSONArray: TJSONArray);

    property Summary: string read fSummary write fSummary;
    property Description: string read fDescription write fDescription;
    property Tags: TArray<string> read fTags write fTags;
    property ExternalDocs: TOpenAPI3ExternalDocs read fExternalDocs
      write fExternalDocs;
    property OperationId: string read fOperationId write fOperationId;
    property Parameters: TObjectList<TObject> read fParameters
      write fParameters;
    property Responses: TOpenAPI3Responses read fResponses write fResponses;
    property Callbacks: TOpenAPI3Callbacks read fCallbacks write fCallbacks;
    property _Deprecated: Boolean read fDeprecated write fDeprecated;
    property RequestBody: TOpenAPI3RequestBody read fRequestBody
      write fRequestBody;
    property Security: TObjectList<TOpenAPI3SecurityRequirement> read fSecurity
      write fSecurity;
    property Servers: TObjectList<TOpenAPI3Server> read fServers write fServers;
  end;

  TOpenAPI3Path = class(TObject)
  private
    fSummary: string;
    fDescription: string;
    fOperations: TObjectDictionary<string, TOpenAPI3Operation>;
    fRef: string;
    fFullPath: String;
    fPathList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function FindOperation(aOperation: string): TOpenAPI3Operation;
    procedure ParseOperations(AJSONObject: TJSONObject);

    property Operations: TObjectDictionary<string, TOpenAPI3Operation>
      read fOperations write fOperations;
    property Summary: string read fSummary write fSummary;
    property Description: string read fDescription write fDescription;
    property Ref: string read fRef write fRef;

    property FullPath: String read fFullPath write fFullPath;
    property PathList: TStringList read fPathList write fPathList;
  end;

   // Class representing OpenAPI 3 Components
  TOpenAPI3Components = class(TObject)
  private
    fSchemas: TObjectDictionary<string, TOpenAPI3Schema>;
    fResponsesObj: TJSONObject;
    fParametersObj: TJSONObject;
    fExamplesObj: TJSONObject;
    fRequestBodiesObj: TJSONObject;
    fHeadersObj: TJSONObject;
    fSecuritySchemesObj: TJSONObject;
    fLinksObj: TJSONObject;
    fCallbacksObj: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseFromJSON(AJSONObject: TJSONObject);
    property Schemas: TObjectDictionary<string, TOpenAPI3Schema> read fSchemas write fSchemas;
    property Responses: TJSONObject read fResponsesObj write fResponsesObj;
    property Parameters: TJSONObject read fParametersObj write fParametersObj;
    property Examples: TJSONObject read fExamplesObj write fExamplesObj;
    property RequestBodies: TJSONObject read fRequestBodiesObj write fRequestBodiesObj;
    property Headers: TJSONObject read fHeadersObj write fHeadersObj;
    property SecuritySchemes: TJSONObject read fSecuritySchemesObj write fSecuritySchemesObj;
    property Links: TJSONObject read fLinksObj write fLinksObj;
    property Callbacks: TJSONObject read fCallbacksObj write fCallbacksObj;
  end;

  TOpenAPI3Tag = class(TObject)
  private
    fName: string;
    fDescription: string;
  public
    property Name: string read fName write fName;
    property Description: string read fDescription write fDescription;
  end;

  TOpenAPI3Info = class(TObject)
  private
    fTitle: string;
    fDescription: string;
    fTermsOfService: string;
    fVersion: string;
    fContact: TOpenAPI3Contact;
    fLicense: TOpenAPI3License;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseFromJSON(AJSONObject: TJSONObject);
    property Title: string read fTitle write fTitle;
    property Description: string read fDescription write fDescription;
    property TermsOfService: string read fTermsOfService write fTermsOfService;
    property Version: string read fVersion write fVersion;
    property Contact: TOpenAPI3Contact read fContact write fContact;
    property License: TOpenAPI3License read fLicense write fLicense;
  end;

  TOpenAPI3Schema = class(TObject)
  private
    fOpenapi: string;
    fJsonSchemaDialect: string;
    fInfo: TOpenAPI3Info;
    fInfoObj: TJSONObject;
    fPaths: TObjectDictionary<string, TOpenAPI3Path>;
    fComponents: TOpenAPI3Components;
    fServers: TObjectList<TOpenAPI3Server>;
    fWebhooks: TObjectDictionary<string, TObject>;
    fSecurity: TObjectList<TOpenAPI3SecurityRequirement>;
    fTags: TObjectList<TOpenAPI3Tag>;

    procedure ParsePaths(AJSONObject: TJSONObject);
    procedure ParseServers(AJSONArray: TJSONArray);
    procedure ParseWebhooks(AJSONObject: TJSONObject);
    procedure ParseSecurity(AJSONArray: TJSONArray);
    procedure ParseTags(AJSONArray: TJSONArray);
  public
    constructor Create;
    destructor Destroy; override;

    function FindPath(aPath: String): TOpenAPI3Path;

    property Openapi: string read fOpenapi write fOpenapi;
    property JsonSchemaDialect: string read fJsonSchemaDialect write fJsonSchemaDialect;
    property Info: TOpenAPI3Info read fInfo write fInfo;
    property InfoObj: TJSONObject read fInfoObj write fInfoObj;
    property Paths: TObjectDictionary<string, TOpenAPI3Path> read fPaths write fPaths;
    property Components: TOpenAPI3Components read fComponents write fComponents;
    property Servers: TObjectList<TOpenAPI3Server> read fServers write fServers;
    property Webhooks: TObjectDictionary<string, TObject> read fWebhooks write fWebhooks;
    property Security: TObjectList<TOpenAPI3SecurityRequirement> read fSecurity write fSecurity;
    property Tags: TObjectList<TOpenAPI3Tag> read fTags write fTags;
  end;


  TNovusOpenAPIParser = class(TNovusObject)
  private
    FSchema: TOpenAPI3Schema;
    fJSON: TJSONObject;

    function Parse: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute: Boolean;

    function LoadFromFile(aFilename: String): Boolean;

    property Schema: TOpenAPI3Schema read FSchema write FSchema;
  end;

implementation

uses
  NovusJSONUtils;

// TOpenAPI3Info

constructor TOpenAPI3Info.Create;
begin
  inherited Create;
  fContact := TOpenAPI3Contact.Create;
  fLicense := TOpenAPI3License.Create;
end;

destructor TOpenAPI3Info.Destroy;
begin
  fContact.Free;
  fLicense.Free;
  inherited Destroy;
end;

procedure TOpenAPI3Info.ParseFromJSON(AJSONObject: TJSONObject);
var
  ContactObj, LicenseObj: TJSONObject;
begin
  if Assigned(AJSONObject) then
  begin
    fTitle := tNovusJSONUtils.GetJSONStringValue(AJSONObject, 'title');
    fDescription := tNovusJSONUtils.GetJSONStringValue(AJSONObject,
      'description');
    fTermsOfService := tNovusJSONUtils.GetJSONStringValue(AJSONObject,
      'termsOfService');
    fVersion := tNovusJSONUtils.GetJSONStringValue(AJSONObject, 'version');

    // Parse contact information
    ContactObj := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'contact');
    if Assigned(ContactObj) then
    begin
      fContact.Name := tNovusJSONUtils.GetJSONStringValue(ContactObj, 'name');
      fContact.Url := tNovusJSONUtils.GetJSONStringValue(ContactObj, 'url');
      fContact.Email := tNovusJSONUtils.GetJSONStringValue(ContactObj, 'email');
    end;

    // Parse license information
    LicenseObj := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'license');
    if Assigned(LicenseObj) then
    begin
      fLicense.Name := tNovusJSONUtils.GetJSONStringValue(LicenseObj, 'name');
      fLicense.Url := tNovusJSONUtils.GetJSONStringValue(LicenseObj, 'url');
    end;
  end;
end;

// TOpenAPI3Server

constructor TOpenAPI3Server.Create;
begin
  inherited Create;
  fVariables := TObjectDictionary<string, TOpenAPI3ServerVariable>.Create
    ([doOwnsValues]);
end;

destructor TOpenAPI3Server.Destroy;
begin
  fVariables.Free;
  inherited Destroy;
end;

procedure TOpenAPI3Server.ParseVariables(AJSONObject: TJSONObject);
var
  VariablePair: TJSONPair;
  VariableObj: TOpenAPI3ServerVariable;
  EnumArray: TJSONArray;
  EnumList: TList<string>;
begin
  if Assigned(AJSONObject) then
  begin
    for VariablePair in AJSONObject do
    begin
      VariableObj := TOpenAPI3ServerVariable.Create;
      VariableObj.Default := tNovusJSONUtils.GetJSONStringValue
        (VariablePair.JsonValue as TJSONObject, 'default');
      VariableObj.Description := tNovusJSONUtils.GetJSONStringValue
        (VariablePair.JsonValue as TJSONObject, 'description');
      EnumArray := tNovusJSONUtils.GetJSONArrayValue
        (VariablePair.JsonValue as TJSONObject, 'enum');
      if Assigned(EnumArray) then
      begin
        EnumList := TList<string>.Create;
        try
          for var i := 0 to EnumArray.Count - 1 do
            EnumList.Add(EnumArray.Items[i].Value);
          VariableObj.Enum := EnumList.ToArray;
        finally
          EnumList.Free;
        end;
      end;
      fVariables.Add(VariablePair.JsonString.Value, VariableObj);
    end;
  end;
end;

// TOpenAPI3Path

constructor TOpenAPI3Path.Create;
begin
  inherited Create;
  fOperations := TObjectDictionary<string, TOpenAPI3Operation>.Create
    ([doOwnsValues]);

  fPathList := TStringList.Create;
end;

destructor TOpenAPI3Path.Destroy;
begin
  fPathList.Free;
  fOperations.Free;
  inherited Destroy;
end;

function TOpenAPI3Path.FindOperation(aOperation: string): TOpenAPI3Operation;
begin
  if not fOperations.TryGetValue(aOperation, Result) then
    Result := nil;
end;

procedure TOpenAPI3Path.ParseOperations(AJSONObject: TJSONObject);
var
  OperationPair: TJSONPair;
  Operation: TOpenAPI3Operation;
  TagsArray: TJSONArray;
  ParametersArray: TJSONArray;
  ParameterObj: TJSONObject;
  ExternalDocsObj: TJSONObject;
  Parameter: TOpenAPI3Parameter;
  Reference: TOpenAPI3Reference;
  ResponsesObj: TJSONObject;
  CallbacksObj: TJSONObject;
  RequestBodyObj: TJSONObject;
  SecurityArray: TJSONArray;
  SecurityRequirement: TOpenAPI3SecurityRequirement;
  ServersArray: TJSONArray; // New variable for servers
begin
  if Assigned(AJSONObject) then
  begin
    // Parse FullPath to List
    TNovusStringUtils.SplitStringToList(fFullPath, '/', fPathList);

    // Parse $ref
    fRef := tNovusJSONUtils.GetJSONStringValue(AJSONObject, '$ref');
    if fRef <> '' then
      Exit;

    // Parse summary
    fSummary := tNovusJSONUtils.GetJSONStringValue(AJSONObject, 'summary');

    // Parse description
    fDescription := tNovusJSONUtils.GetJSONStringValue(AJSONObject,
      'description');

    for OperationPair in AJSONObject do
    begin
      // Skip the summary and description keys
      if (OperationPair.JsonString.Value = 'summary') or
        (OperationPair.JsonString.Value = 'description') then
        Continue;

      Operation := TOpenAPI3Operation.Create;
      Operation.Summary := tNovusJSONUtils.GetJSONStringValue
        (OperationPair.JsonValue as TJSONObject, 'summary');
      Operation.Description := tNovusJSONUtils.GetJSONStringValue
        (OperationPair.JsonValue as TJSONObject, 'description');
      Operation.OperationId := tNovusJSONUtils.GetJSONStringValue
        (OperationPair.JsonValue as TJSONObject, 'operationId');
      Operation._Deprecated := tNovusJSONUtils.GetJSONBooleanValue
        (OperationPair.JsonValue as TJSONObject, 'deprecated');

      // Parse tags
      TagsArray := tNovusJSONUtils.GetJSONArrayValue
        (OperationPair.JsonValue as TJSONObject, 'tags');
      if Assigned(TagsArray) then
      begin
        SetLength(Operation.fTags, TagsArray.Count);
        for var i := 0 to TagsArray.Count - 1 do
        begin
          Operation.fTags[i] := TagsArray.Items[i].Value;
        end;
      end;

      // Parse externalDocs
      ExternalDocsObj := tNovusJSONUtils.GetJSONObjectValue
        (OperationPair.JsonValue as TJSONObject, 'externalDocs');
      if Assigned(ExternalDocsObj) then
      begin
        Operation.ExternalDocs.Description := tNovusJSONUtils.GetJSONStringValue
          (ExternalDocsObj, 'description');
        Operation.ExternalDocs.Url := tNovusJSONUtils.GetJSONStringValue
          (ExternalDocsObj, 'url');
      end;

      // Parse parameters
      ParametersArray := tNovusJSONUtils.GetJSONArrayValue
        (OperationPair.JsonValue as TJSONObject, 'parameters');
      if Assigned(ParametersArray) then
      begin
        for var ParamItem in ParametersArray do
        begin
          if ParamItem is TJSONObject then
          begin
            ParameterObj := TJSONObject(ParamItem);
            if ParameterObj.GetValue('$ref') <> nil then
            begin
              Reference := TOpenAPI3Reference.Create;
              Reference.Ref := tNovusJSONUtils.GetJSONStringValue
                (ParameterObj, '$ref');
              Operation.Parameters.Add(Reference);
            end
            else
            begin
              Parameter := TOpenAPI3Parameter.Create;
              Parameter.Name := tNovusJSONUtils.GetJSONStringValue
                (ParameterObj, 'name');
              Parameter.InLocation := tNovusJSONUtils.GetJSONStringValue
                (ParameterObj, 'in');
              Parameter.Description := tNovusJSONUtils.GetJSONStringValue
                (ParameterObj, 'description');
              Parameter.Required := tNovusJSONUtils.GetJSONBooleanValue
                (ParameterObj, 'required');
              Parameter.Deprecated := tNovusJSONUtils.GetJSONBooleanValue
                (ParameterObj, 'deprecated');
              Parameter.AllowEmptyValue := tNovusJSONUtils.GetJSONBooleanValue
                (ParameterObj, 'allowEmptyValue');
              Operation.Parameters.Add(Parameter);
            end;
          end;
        end;
      end;

      // Parse responses
      ResponsesObj := tNovusJSONUtils.GetJSONObjectValue
        (OperationPair.JsonValue as TJSONObject, 'responses');
      if Assigned(ResponsesObj) then
      begin
        Operation.Responses.ParseFromJSON(ResponsesObj);
      end;

      // Parse callbacks
      CallbacksObj := tNovusJSONUtils.GetJSONObjectValue
        (OperationPair.JsonValue as TJSONObject, 'callbacks');
      if Assigned(CallbacksObj) then
      begin
        Operation.Callbacks.ParseFromJSON(CallbacksObj);
      end;

      // Parse requestBody
      RequestBodyObj := tNovusJSONUtils.GetJSONObjectValue
        (OperationPair.JsonValue as TJSONObject, 'requestBody');
      if Assigned(RequestBodyObj) then
      begin
        Operation.RequestBody.Description := tNovusJSONUtils.GetJSONStringValue
          (RequestBodyObj, 'description');
        Operation.RequestBody.Content := tNovusJSONUtils.GetJSONObjectValue
          (RequestBodyObj, 'content');
        Operation.RequestBody.Required := tNovusJSONUtils.GetJSONBooleanValue
          (RequestBodyObj, 'required');
      end;

      // Parse security
      SecurityArray := tNovusJSONUtils.GetJSONArrayValue
        (OperationPair.JsonValue as TJSONObject, 'security');
      if Assigned(SecurityArray) then
      begin
        for var i := 0 to SecurityArray.Count - 1 do
        begin
          SecurityRequirement := TOpenAPI3SecurityRequirement.Create;
          SecurityRequirement.ParseFromJSON
            (SecurityArray.Items[i] as TJSONArray);
          Operation.Security.Add(SecurityRequirement);
        end;
      end;

      // Parse servers
      ServersArray := tNovusJSONUtils.GetJSONArrayValue
        (OperationPair.JsonValue as TJSONObject, 'servers');
      if Assigned(ServersArray) then
      begin
        Operation.ParseServers(ServersArray);
      end;

      fOperations.Add(OperationPair.JsonString.Value, Operation);
    end;
  end;
end;

// TOpenAPI3Components implementation

constructor TOpenAPI3Components.Create;
begin
  inherited Create;
  fSchemas := TObjectDictionary<string, TOpenAPI3Schema>.Create([doOwnsValues]);
  fResponsesObj := TJSONObject.Create;
  fParametersObj := TJSONObject.Create;
  fExamplesObj := TJSONObject.Create;
  fRequestBodiesObj := TJSONObject.Create;
  fHeadersObj := TJSONObject.Create;
  fSecuritySchemesObj := TJSONObject.Create;
  fLinksObj := TJSONObject.Create;
  fCallbacksObj := TJSONObject.Create;
end;

destructor TOpenAPI3Components.Destroy;
begin
  fSchemas.Free;
  fResponsesObj := nil;
  fParametersObj := nil;
  fExamplesObj := nil;
  fRequestBodiesObj := nil;
  fHeadersObj := nil;
  fSecuritySchemesObj := nil;
  fLinksObj := nil;
  fCallbacksObj := nil;
  inherited Destroy;
end;

procedure TOpenAPI3Components.ParseFromJSON(AJSONObject: TJSONObject);
var
  SchemaPair: TJSONPair;
  FSchema: TOpenAPI3Schema;
begin
  if Assigned(AJSONObject) then
  begin
    // Parse schemas
    for SchemaPair in tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'schemas') do
    begin
      FSchema := TOpenAPI3Schema.Create;
      FSchema.ParseFromJSON(SchemaPair.JsonValue as TJSONObject);
      fSchema.Add(SchemaPair.JsonString.Value, FSchema);
    end;

    fResponsesObj := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'responses');
    fParametersObj := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'parameters');
    fExamplesObj := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'examples');
    fRequestBodiesObj := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'requestBodies');
    fHeadersObj := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'headers');
    fSecuritySchemesObj := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'securitySchemes');
    fLinksObj := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'links');
    fCallbacksObj := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'callbacks');
  end;
end;
// TOpenAPI3SecurityRequirement

constructor TOpenAPI3SecurityRequirement.Create;
begin
  inherited Create;
  fRequirements := TObjectDictionary < string, TArray < string >>.Create;
end;

destructor TOpenAPI3SecurityRequirement.Destroy;
begin
  fRequirements.Free;
  inherited Destroy;
end;

procedure TOpenAPI3SecurityRequirement.ParseFromJSON(AJSONArray: TJSONArray);
var
  RequirementPair: TJSONPair;
  ScopesArray: TJSONArray;
  ScopesList: TList<string>;
  RequirementObj: TJSONObject;
begin
  if Assigned(AJSONArray) then
  begin
    for var i := 0 to AJSONArray.Count - 1 do
    begin
      RequirementObj := AJSONArray.Items[i] as TJSONObject;
      for RequirementPair in RequirementObj do
      begin
        ScopesArray := RequirementPair.JsonValue as TJSONArray;
        ScopesList := TList<string>.Create;
        try
          for var J := 0 to ScopesArray.Count - 1 do
            ScopesList.Add(ScopesArray.Items[J].Value);
          fRequirements.Add(RequirementPair.JsonString.Value,
            ScopesList.ToArray);
        finally
          ScopesList.Free;
        end;
      end;
    end;
  end;
end;

// TOpenAPI3Schema

constructor TOpenAPI3Schema.Create;
begin
  inherited Create;
  fInfo := TOpenAPI3Info.Create;
  fInfoObj := TJSONObject.Create;
  fPaths := TObjectDictionary<string, TOpenAPI3Path>.Create([doOwnsValues]);
  fComponents := TOpenAPI3Components.Create;
  fServers := TObjectList<TOpenAPI3Server>.Create;
  fWebhooks := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  fSecurity := TObjectList<TOpenAPI3SecurityRequirement>.Create;
  fTags := TObjectList<TOpenAPI3Tag>.Create;
end;

destructor TOpenAPI3Schema.Destroy;
begin
  fInfo.Free;
  fInfoObj := NIl;
  fPaths.Free;
  fComponents.Free;
  fServers.Free;
  fWebhooks.Free;
  fSecurity.Free;
  fTags.Free;
  inherited Destroy;
end;

procedure TOpenAPI3Schema.ParsePaths(AJSONObject: TJSONObject);
var
  PathPair: TJSONPair;
  Path: TOpenAPI3Path;
begin
  if Assigned(AJSONObject) then
  begin
    for PathPair in AJSONObject do
    begin
      Path := TOpenAPI3Path.Create;

      Path.FullPath := PathPair.JsonString.Value;

      Path.ParseOperations(PathPair.JsonValue as TJSONObject);
      fPaths.Add(PathPair.JsonString.Value, Path);
    end;
  end;
end;

procedure TOpenAPI3Schema.ParseServers(AJSONArray: TJSONArray);
var
  i: Integer;
  ServerObj: TJSONObject;
  Server: TOpenAPI3Server;
begin
  if Assigned(AJSONArray) then
  begin
    for i := 0 to AJSONArray.Count - 1 do
    begin
      ServerObj := AJSONArray.Items[i] as TJSONObject;
      Server := TOpenAPI3Server.Create;
      Server.Url := tNovusJSONUtils.GetJSONStringValue(ServerObj, 'url');
      Server.Description := tNovusJSONUtils.GetJSONStringValue(ServerObj,
        'description');
      Server.ParseVariables(tNovusJSONUtils.GetJSONObjectValue(ServerObj,
        'variables'));
      fServers.Add(Server);
    end;
  end;
end;

procedure TOpenAPI3Schema.ParseWebhooks(AJSONObject: TJSONObject);
var
  WebhookPair: TJSONPair;
  PathItemObj: TOpenAPI3Path;
  RefObj: TOpenAPI3Reference;
begin
  if Assigned(AJSONObject) then
  begin
    for WebhookPair in AJSONObject do
    begin
      if WebhookPair.JsonValue is TJSONObject then
      begin
        if (WebhookPair.JsonValue as TJSONObject).GetValue<string>('$ref') <> ''
        then
        begin
          RefObj := TOpenAPI3Reference.Create;
          RefObj.Ref := tNovusJSONUtils.GetJSONStringValue
            (WebhookPair.JsonValue as TJSONObject, '$ref');
          fWebhooks.Add(WebhookPair.JsonString.Value, RefObj);
        end
        else
        begin
          PathItemObj := TOpenAPI3Path.Create;
          PathItemObj.ParseOperations(WebhookPair.JsonValue as TJSONObject);
          fWebhooks.Add(WebhookPair.JsonString.Value, PathItemObj);
        end;
      end;
    end;
  end;
end;

procedure TOpenAPI3Schema.ParseSecurity(AJSONArray: TJSONArray);
var
  SecurityRequirement: TOpenAPI3SecurityRequirement;
begin
  if Assigned(AJSONArray) then
  begin
    for var i := 0 to AJSONArray.Count - 1 do
    begin
      SecurityRequirement := TOpenAPI3SecurityRequirement.Create;
      SecurityRequirement.ParseFromJSON(AJSONArray.Items[i] as TJSONArray);
      fSecurity.Add(SecurityRequirement);
    end;
  end;
end;

procedure TOpenAPI3Schema.ParseTags(AJSONArray: TJSONArray);
var
  TagObj: TJSONObject;
  Tag: TOpenAPI3Tag;
begin
  if Assigned(AJSONArray) then
  begin
    for var i := 0 to AJSONArray.Count - 1 do
    begin
      TagObj := AJSONArray.Items[i] as TJSONObject;
      Tag := TOpenAPI3Tag.Create;
      Tag.Name := tNovusJSONUtils.GetJSONStringValue(TagObj, 'name');
      Tag.Description := tNovusJSONUtils.GetJSONStringValue(TagObj,
        'description');
      fTags.Add(Tag);
    end;
  end;
end;

function TOpenAPI3Schema.FindPath(aPath: String): TOpenAPI3Path;
begin
  if not fPaths.TryGetValue(aPath, Result) then
    Result := nil;
end;

// TNovusOpenAPIParser

constructor TNovusOpenAPIParser.Create;
begin
  inherited Create;
  FSchema := TOpenAPI3Schema.Create;
  fJSON := TJSONObject.Create;
end;

destructor TNovusOpenAPIParser.Destroy;
begin
  fJSON.Free;

  FSchema.Free;

  inherited Destroy;
end;

function TNovusOpenAPIParser.LoadFromFile(aFilename: String): Boolean;
begin
  Result := False;
  if not FileExists(aFilename) then
    Exit;

  try
    fJSON := TJSONObject.ParseJSONValue(TFile.ReadAllText(aFilename))
      as TJSONObject;
    Result := Assigned(fJSON);
  except
    on E: Exception do
      WriteLn('Error loading JSON file: ' + E.Message);
  end;
end;

function TNovusOpenAPIParser.Execute: Boolean;
begin
  Result := Parse;
end;

function TNovusOpenAPIParser.Parse: Boolean;
var
  PathsObj, WebhooksObj, ComponentsObj: TJSONObject;
  ServersArray, SecurityArray, TagsArray: TJSONArray;
begin
  Result := False;
  if not Assigned(fJSON) then
    Exit;

  try
    FSchema.Openapi := tNovusJSONUtils.GetJSONStringValue(fJSON, 'openapi');
    FSchema.JsonSchemaDialect := tNovusJSONUtils.GetJSONStringValue(fJSON,
      'jsonSchemaDialect');
    FSchema.InfoObj := tNovusJSONUtils.GetJSONObjectValue(fJSON, 'info');
    if Assigned(FSchema.InfoObj) then
    begin
      FSchema.Info.ParseFromJSON(FSchema.InfoObj);
    end;

    PathsObj := tNovusJSONUtils.GetJSONObjectValue(fJSON, 'paths');
    if Assigned(PathsObj) then
    begin
      FSchema.ParsePaths(PathsObj);
    end;

    ServersArray := tNovusJSONUtils.GetJSONArrayValue(fJSON as TJSONObject,
      'servers');
    if Assigned(ServersArray) then
    begin
      FSchema.ParseServers(ServersArray);
    end;

    WebhooksObj := tNovusJSONUtils.GetJSONObjectValue(fJSON, 'webhooks');
    if Assigned(WebhooksObj) then
    begin
      FSchema.ParseWebhooks(WebhooksObj);
    end;

    ComponentsObj := tNovusJSONUtils.GetJSONObjectValue(fJSON, 'components');
    if Assigned(ComponentsObj) then
    begin
      FSchema.Components.ParseFromJSON(ComponentsObj);
    end;

    SecurityArray := tNovusJSONUtils.GetJSONArrayValue(fJSON as TJSONObject,
      'security');
    if Assigned(SecurityArray) then
    begin
      FSchema.ParseSecurity(SecurityArray);
    end;

    TagsArray := tNovusJSONUtils.GetJSONArrayValue
      (fJSON as TJSONObject, 'tags');
    if Assigned(TagsArray) then
    begin
      FSchema.ParseTags(TagsArray);
    end;

    (*
      // Example: Output the title and version from the info object
      if Assigned(FSchema.Info) then
      begin
      WriteLn('Title: ' + FSchema.Info.Title);
      WriteLn('Version: ' + FSchema.Info.Version);
      if Assigned(FSchema.Info.Contact) then
      begin
      WriteLn('Contact Name: ' + FSchema.Info.Contact.Name);
      WriteLn('Contact URL: ' + FSchema.Info.Contact.Url);
      WriteLn('Contact Email: ' + FSchema.Info.Contact.Email);
      end;
      if Assigned(FSchema.Info.License) then
      begin
      WriteLn('License Name: ' + FSchema.Info.License.Name);
      WriteLn('License URL: ' + FSchema.Info.License.Url);
      end;
      end;

      // Example: Output jsonSchemaDialect
      WriteLn('jsonSchemaDialect: ' + FSchema.JsonSchemaDialect);

      // Example: Output paths
      for var PathPair in FSchema.Paths do
      begin
      WriteLn('Path: ' + PathPair.Key);
      for var OperationPair in PathPair.Value.Operations do
      begin
      WriteLn('  Operation: ' + OperationPair.Key);
      WriteLn('    Summary: ' + OperationPair.Value.Summary);
      WriteLn('    Description: ' + OperationPair.Value.Description);
      end;
      end;

      // Example: Output servers
      for var Server in FSchema.Servers do
      begin
      WriteLn('Server URL: ' + Server.Url);
      WriteLn('Server Description: ' + Server.Description);
      for var Variable in Server.Variables do
      begin
      WriteLn('Variable Name: ' + Variable.Key);
      WriteLn('Variable Default: ' + Variable.Value.Default);
      WriteLn('Variable Description: ' + Variable.Value.Description);
      if Length(Variable.Value.Enum) > 0 then
      begin
      WriteLn('Variable Enum: ' + string.Join(', ', Variable.Value.Enum));
      end;
      end;
      end;

      // Example: Output webhooks
      for var WebhookPair in FSchema.Webhooks do
      begin
      WriteLn('Webhook: ' + WebhookPair.Key);
      if WebhookPair.Value is TOpenAPI3Reference then
      begin
      WriteLn('  Reference: ' + TOpenAPI3Reference(WebhookPair.Value).Ref);
      end
      else if WebhookPair.Value is TOpenAPI3Path then
      begin
      WriteLn('  Path Item:');
      for var OperationPair in TOpenAPI3Path(WebhookPair.Value).Operations do
      begin
      WriteLn('    Operation: ' + OperationPair.Key);
      WriteLn('      Summary: ' + OperationPair.Value.Summary);
      WriteLn('      Description: ' + OperationPair.Value.Description);
      end;
      end;
      end;

      // Example: Output components
      WriteLn('Components:');
      if Assigned(FSchema.Components.Schemas) then
      WriteLn('  Schemas: ' + FSchema.Components.Schemas.ToString);
      if Assigned(FSchema.Components.Responses) then
      WriteLn('  Responses: ' + FSchema.Components.Responses.ToString);
      if Assigned(FSchema.Components.Parameters) then
      WriteLn('  Parameters: ' + FSchema.Components.Parameters.ToString);
      if Assigned(FSchema.Components.Examples) then
      WriteLn('  Examples: ' + FSchema.Components.Examples.ToString);
      if Assigned(FSchema.Components.RequestBodies) then
      WriteLn('  Request Bodies: ' + FSchema.Components.RequestBodies.ToString);
      if Assigned(FSchema.Components.Headers) then
      WriteLn('  Headers: ' + FSchema.Components.Headers.ToString);
      if Assigned(FSchema.Components.SecuritySchemes) then
      WriteLn('  Security Schemes: ' + FSchema.Components.SecuritySchemes.ToString);
      if Assigned(FSchema.Components.Links) then
      WriteLn('  Links: ' + FSchema.Components.Links.ToString);
      if Assigned(FSchema.Components.Callbacks) then
      WriteLn('  Callbacks: ' + FSchema.Components.Callbacks.ToString);

      // Example: Output security
      for var SecurityRequirement in FSchema.Security do
      begin
      WriteLn('Security Requirement:');
      for var RequirementPair in SecurityRequirement.Requirements do
      begin
      WriteLn('  Scheme: ' + RequirementPair.Key);
      WriteLn('  Scopes: ' + string.Join(', ', RequirementPair.Value));
      end;
      end;

      // Example: Output tags
      for var Tag in FSchema.Tags do
      begin
      WriteLn('Tag: ' + Tag.Name);
      WriteLn('  Description: ' + Tag.Description);
      end;
    *)
    Result := True;
  except
    on E: Exception do
      WriteLn('Error parsing JSON data: ' + E.Message);
  end;
end;

// TOpenAPI3Operation

constructor TOpenAPI3Operation.Create;
begin
  inherited Create;
  fExternalDocs := TOpenAPI3ExternalDocs.Create;
  fParameters := TObjectList<TObject>.Create(True); // Owns objects
  fResponses := TOpenAPI3Responses.Create;
  fCallbacks := TOpenAPI3Callbacks.Create;
  fRequestBody := TOpenAPI3RequestBody.Create;
  fSecurity := TObjectList<TOpenAPI3SecurityRequirement>.Create(True);
  fServers := TObjectList<TOpenAPI3Server>.Create; // Initialize servers
  fDeprecated := False;
end;

destructor TOpenAPI3Operation.Destroy;
begin
  fExternalDocs.Free;
  fParameters.Free;
  fResponses.Free;
  fCallbacks.Free;
  fRequestBody.Free;
  fSecurity.Free;
  fServers.Free; // Free servers
  inherited Destroy;
end;

procedure TOpenAPI3Operation.ParseServers(AJSONArray: TJSONArray);
var
  i: Integer;
  ServerObj: TJSONObject;
  Server: TOpenAPI3Server;
begin
  if Assigned(AJSONArray) then
  begin
    for i := 0 to AJSONArray.Count - 1 do
    begin
      ServerObj := AJSONArray.Items[i] as TJSONObject;
      Server := TOpenAPI3Server.Create;
      Server.Url := tNovusJSONUtils.GetJSONStringValue(ServerObj, 'url');
      Server.Description := tNovusJSONUtils.GetJSONStringValue(ServerObj,
        'description');
      Server.ParseVariables(tNovusJSONUtils.GetJSONObjectValue(ServerObj,
        'variables'));
      fServers.Add(Server);
    end;
  end;
end;

// TOpenAPI3Responses

procedure TOpenAPI3Responses.ParseFromJSON(AJSONObject: TJSONObject);
var
  ResponsePair: TJSONPair;
  Response: TOpenAPI3Response;
begin
  if Assigned(AJSONObject) then
  begin
    for ResponsePair in AJSONObject do
    begin
      Response := TOpenAPI3Response.Create;
      Response.Description := tNovusJSONUtils.GetJSONStringValue
        (ResponsePair.JsonValue as TJSONObject, 'description');
      Response.Content := tNovusJSONUtils.GetJSONObjectValue
        (ResponsePair.JsonValue as TJSONObject, 'content');
      fResponses.Add(ResponsePair.JsonString.Value, Response);
    end;
  end;
end;

constructor TOpenAPI3Responses.Create;
begin
  inherited Create;
  fResponses := TObjectDictionary<string, TOpenAPI3Response>.Create
    ([doOwnsValues]);
end;

destructor TOpenAPI3Responses.Destroy;
begin
  fResponses.Free;
  inherited Destroy;
end;

// TOpenAPI3Callbacks

constructor TOpenAPI3Callbacks.Create;
begin
  inherited Create;
  fCallbacks := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
end;

destructor TOpenAPI3Callbacks.Destroy;
begin
  fCallbacks.Free;
  inherited Destroy;
end;

procedure TOpenAPI3Callbacks.ParseFromJSON(AJSONObject: TJSONObject);
var
  CallbackPair: TJSONPair;
  Callback: TOpenAPI3Callback;
  Reference: TOpenAPI3Reference;
  RefValue: TJSONValue;
begin
  if Assigned(AJSONObject) then
  begin
    for CallbackPair in AJSONObject do
    begin
      if CallbackPair.JsonValue is TJSONObject then
      begin
        RefValue := (CallbackPair.JsonValue as TJSONObject).GetValue('$ref');
        if Assigned(RefValue) then
        begin
          Reference := TOpenAPI3Reference.Create;
          Reference.Ref := tNovusJSONUtils.GetJSONStringValue
            (CallbackPair.JsonValue as TJSONObject, '$ref');
          fCallbacks.Add(CallbackPair.JsonString.Value, Reference);
        end
        else
        begin
          Callback := TOpenAPI3Callback.Create;
          Callback.Callback := CallbackPair.JsonValue as TJSONObject;
          fCallbacks.Add(CallbackPair.JsonString.Value, Callback);
        end;
      end;
    end;
  end;
end;

// TOpenAPI3SchemaObject implementation

procedure TOpenAPI3SchemaObject.ParseFromJSON(AJSONObject: TJSONObject);
begin
  if Assigned(AJSONObject) then
  begin
    fTitle := tNovusJSONUtils.GetJSONStringValue(AJSONObject, 'title');
    fType := tNovusJSONUtils.GetJSONStringValue(AJSONObject, 'type');
    // Parse other properties as needed
  end;
end;

end.
