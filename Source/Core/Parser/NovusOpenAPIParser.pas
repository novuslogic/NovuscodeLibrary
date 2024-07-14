unit NovusOpenAPIParser;

interface

uses
  NovusObject, System.IOUtils, System.JSON, System.SysUtils, System.Generics.Collections;

type
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
    property Variables: TObjectDictionary<string, TOpenAPI3ServerVariable> read fVariables write fVariables;
  end;

  TOpenAPI3Operation = class(TObject)
  private
    fSummary: string;
    fDescription: string;
  public
    property Summary: string read fSummary write fSummary;
    property Description: string read fDescription write fDescription;
  end;

  TOpenAPI3Path = class(TObject)
  private
    fOperations: TObjectDictionary<string, TOpenAPI3Operation>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseOperations(AJSONObject: TJSONObject);
    property Operations: TObjectDictionary<string, TOpenAPI3Operation> read fOperations write fOperations;
  end;

  TOpenAPI3Reference = class(TObject)
  private
    fRef: string;
  public
    property Ref: string read fRef write fRef;
  end;

  TOpenAPI3Components = class(TObject)
  private
    fSchemas: TJSONObject;
    fResponses: TJSONObject;
    fParameters: TJSONObject;
    fExamples: TJSONObject;
    fRequestBodies: TJSONObject;
    fHeaders: TJSONObject;
    fSecuritySchemes: TJSONObject;
    fLinks: TJSONObject;
    fCallbacks: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseFromJSON(AJSONObject: TJSONObject);
    property Schemas: TJSONObject read fSchemas write fSchemas;
    property Responses: TJSONObject read fResponses write fResponses;
    property Parameters: TJSONObject read fParameters write fParameters;
    property Examples: TJSONObject read fExamples write fExamples;
    property RequestBodies: TJSONObject read fRequestBodies write fRequestBodies;
    property Headers: TJSONObject read fHeaders write fHeaders;
    property SecuritySchemes: TJSONObject read fSecuritySchemes write fSecuritySchemes;
    property Links: TJSONObject read fLinks write fLinks;
    property Callbacks: TJSONObject read fCallbacks write fCallbacks;
  end;

  TOpenAPI3SecurityRequirement = class(TObject)
  private
    fRequirements: TObjectDictionary<string, TArray<string>>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseFromJSON(AJSONArray: TJSONArray);
    property Requirements: TObjectDictionary<string, TArray<string>> read fRequirements write fRequirements;
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
    fsOpenapi: string;
    fJsonSchemaDialect: string;
    fInfo: TOpenAPI3Info;
    fInfoObj: TJSONObject;
    fPaths: TObjectDictionary<string, TOpenAPI3Path>;
    fComponents: TOpenAPI3Components;
    fServers: TObjectList<TOpenAPI3Server>;
    fWebhooks: TObjectDictionary<string, TObject>;
    fSecurity: TObjectList<TOpenAPI3SecurityRequirement>;
    fTags: TObjectList<TOpenAPI3Tag>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParsePaths(AJSONObject: TJSONObject);
    procedure ParseServers(AJSONArray: TJSONArray);
    procedure ParseWebhooks(AJSONObject: TJSONObject);
    procedure ParseSecurity(AJSONArray: TJSONArray);
    procedure ParseTags(AJSONArray: TJSONArray);
    property Openapi: string read fsOpenapi write fsOpenapi;
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
    fDescription := tNovusJSONUtils.GetJSONStringValue(AJSONObject, 'description');
    fTermsOfService := tNovusJSONUtils.GetJSONStringValue(AJSONObject, 'termsOfService');
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
  fVariables := TObjectDictionary<string, TOpenAPI3ServerVariable>.Create([doOwnsValues]);
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
      VariableObj.Default := tNovusJSONUtils.GetJSONStringValue(VariablePair.JsonValue as TJSONObject, 'default');
      VariableObj.Description := tNovusJSONUtils.GetJSONStringValue(VariablePair.JsonValue as TJSONObject, 'description');
      EnumArray := tNovusJSONUtils.GetJSONArrayValue(VariablePair.JsonValue as TJSONArray, 'enum');
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
  fOperations := TObjectDictionary<string, TOpenAPI3Operation>.Create([doOwnsValues]);
end;

destructor TOpenAPI3Path.Destroy;
begin
  fOperations.Free;
  inherited Destroy;
end;

procedure TOpenAPI3Path.ParseOperations(AJSONObject: TJSONObject);
var
  OperationPair: TJSONPair;
  Operation: TOpenAPI3Operation;
begin
  if Assigned(AJSONObject) then
  begin
    for OperationPair in AJSONObject do
    begin
      Operation := TOpenAPI3Operation.Create;
      Operation.Summary := tNovusJSONUtils.GetJSONStringValue(OperationPair.JsonValue as TJSONObject, 'summary');
      Operation.Description := tNovusJSONUtils.GetJSONStringValue(OperationPair.JsonValue as TJSONObject, 'description');
      fOperations.Add(OperationPair.JsonString.Value, Operation);
    end;
  end;
end;

// TOpenAPI3Components

constructor TOpenAPI3Components.Create;
begin
  inherited Create;
  fSchemas := TJSONObject.Create;
  fResponses := TJSONObject.Create;
  fParameters := TJSONObject.Create;
  fExamples := TJSONObject.Create;
  fRequestBodies := TJSONObject.Create;
  fHeaders := TJSONObject.Create;
  fSecuritySchemes := TJSONObject.Create;
  fLinks := TJSONObject.Create;
  fCallbacks := TJSONObject.Create;
end;

destructor TOpenAPI3Components.Destroy;
begin
  fSchemas.Free;
  fResponses.Free;
  fParameters.Free;
  fExamples.Free;
  fRequestBodies.Free;
  fHeaders.Free;
  fSecuritySchemes.Free;
  fLinks.Free;
  fCallbacks.Free;
  inherited Destroy;
end;

procedure TOpenAPI3Components.ParseFromJSON(AJSONObject: TJSONObject);
begin
  if Assigned(AJSONObject) then
  begin
    fSchemas := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'schemas');
    fResponses := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'responses');
    fParameters := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'parameters');
    fExamples := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'examples');
    fRequestBodies := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'requestBodies');
    fHeaders := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'headers');
    fSecuritySchemes := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'securitySchemes');
    fLinks := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'links');
    fCallbacks := tNovusJSONUtils.GetJSONObjectValue(AJSONObject, 'callbacks');
  end;
end;

// TOpenAPI3SecurityRequirement

constructor TOpenAPI3SecurityRequirement.Create;
begin
  inherited Create;
  fRequirements := TObjectDictionary<string, TArray<string>>.Create;
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
    for var I := 0 to AJSONArray.Count - 1 do
    begin
      RequirementObj := AJSONArray.Items[I] as TJSONObject;
      for RequirementPair in RequirementObj do
      begin
        ScopesArray := RequirementPair.JsonValue as TJSONArray;
        ScopesList := TList<string>.Create;
        try
          for var J := 0 to ScopesArray.Count - 1 do
            ScopesList.Add(ScopesArray.Items[J].Value);
          fRequirements.Add(RequirementPair.JsonString.Value, ScopesList.ToArray);
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
  fInfoObj.Free;
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
      Path.ParseOperations(PathPair.JsonValue as TJSONObject);
      fPaths.Add(PathPair.JsonString.Value, Path);
    end;
  end;
end;

procedure TOpenAPI3Schema.ParseServers(AJSONArray: TJSONArray);
var
  I: Integer;
  ServerObj: TJSONObject;
  Server: TOpenAPI3Server;
begin
  if Assigned(AJSONArray) then
  begin
    for I := 0 to AJSONArray.Count - 1 do
    begin
      ServerObj := AJSONArray.Items[I] as TJSONObject;
      Server := TOpenAPI3Server.Create;
      Server.Url := tNovusJSONUtils.GetJSONStringValue(ServerObj, 'url');
      Server.Description := tNovusJSONUtils.GetJSONStringValue(ServerObj, 'description');
      Server.ParseVariables(tNovusJSONUtils.GetJSONObjectValue(ServerObj, 'variables'));
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
        if (WebhookPair.JsonValue as TJSONObject).GetValue<string>('$ref') <> '' then
        begin
          RefObj := TOpenAPI3Reference.Create;
          RefObj.Ref := tNovusJSONUtils.GetJSONStringValue(WebhookPair.JsonValue as TJSONObject, '$ref');
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
    for var I := 0 to AJSONArray.Count - 1 do
    begin
      SecurityRequirement := TOpenAPI3SecurityRequirement.Create;
      SecurityRequirement.ParseFromJSON(AJSONArray.Items[I] as TJSONArray);
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
    for var I := 0 to AJSONArray.Count - 1 do
    begin
      TagObj := AJSONArray.Items[I] as TJSONObject;
      Tag := TOpenAPI3Tag.Create;
      Tag.Name := tNovusJSONUtils.GetJSONStringValue(TagObj, 'name');
      Tag.Description := tNovusJSONUtils.GetJSONStringValue(TagObj, 'description');
      fTags.Add(Tag);
    end;
  end;
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
  FSchema.Free;
  fJSON.Free;
  inherited Destroy;
end;

function TNovusOpenAPIParser.LoadFromFile(aFilename: String): Boolean;
begin
  Result := False;
  if not FileExists(aFilename) then
    Exit;

  try
    fJSON := TJSONObject.ParseJSONValue(TFile.ReadAllText(aFilename)) as TJSONObject;
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
    FSchema.JsonSchemaDialect := tNovusJSONUtils.GetJSONStringValue(fJSON, 'jsonSchemaDialect');
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

    ServersArray := tNovusJSONUtils.GetJSONArrayValue(TJSONArray(fJSON), 'servers');
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

    SecurityArray := tNovusJSONUtils.GetJSONArrayValue(tJSONArray(fJSON), 'security');
    if Assigned(SecurityArray) then
    begin
      FSchema.ParseSecurity(SecurityArray);
    end;

    TagsArray := tNovusJSONUtils.GetJSONArrayValue(tJSONArray(fJSON), 'tags');
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

end.

