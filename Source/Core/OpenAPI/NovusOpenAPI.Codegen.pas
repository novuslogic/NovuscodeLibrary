unit NovusOpenAPI.Codegen;

interface

uses
  NovusStringBuilder, NovusOpenAPI.Parser, NovusObject, System.SysUtils,
  System.Generics.Collections, System.JSON, System.IOUtils, NovusFileUtils,
  NovusList;

type
  TNovusOpenAPIClassBuilderList = class(TNovusList);


  TNovusOpenAPIClassBuilder = class(TNovusObject)
  protected
  private
    fClassName: String;
  public
    property ClassName_: String read fClassname write fClassName;
  end;



  TNovusOpenAPICodegen = class(TNovusObject)
  private
    FTitleClassname: String;
    FUseFilenameSeparator: Boolean;
    FParser: TNovusOpenAPIParser;
    FOutputDir: string;

    function GetFirstPartAfterSlash(const InputStr: string): string;
    function RemoveSlashes(const Input: string): string;
    function ConvertToDotNotation(const Method: string): string;
    function GetNameSpaceFileName(Key: String): String;
    function GetNameSpace(Key: String): String;
    procedure GenerateModelClasses;
    procedure GenerateAPIClasses;
    procedure WriteToFile(const AFileName, AContent: string);
    function MapOpenAPITypeToDelphiType(const OpenAPIType, OpenAPIFormat: string): string;
  public
    constructor Create(aParser: TNovusOpenAPIParser; aOutputDir: string);
    destructor Destroy; override;
    procedure GenerateCode;
    property OutputDir: string read FOutputDir write FOutputDir;
    property TitleClassName: string read FTitleClassname write FTitleClassname;
    property UseFilenameSeparator: boolean read FUseFilenameSeparator write FUseFilenameSeparator;
  end;

implementation

{ TNovusOpenAPICodegen }

constructor TNovusOpenAPICodegen.Create(aParser: TNovusOpenAPIParser; aOutputDir: string);
begin
  inherited Create;
  FParser := aParser;
  FOutputDir := aOutputDir;
  if not DirectoryExists(FOutputDir) then
    ForceDirectories(FOutputDir);
end;

destructor TNovusOpenAPICodegen.Destroy;
begin
  // FParser is owned by the caller, do not free it here
  inherited Destroy;
end;

procedure TNovusOpenAPICodegen.GenerateCode;
begin
  try
    GenerateModelClasses;
    GenerateAPIClasses;
  except
    on E: Exception do
      WriteLn('Error generating code: ' + E.Message);
  end;
end;

procedure TNovusOpenAPICodegen.GenerateModelClasses;
var
  Schema: TOpenAPI3Schema;
  ModelBuilder: TNovusStringBuilder;
  SchemaName: string;
  PropertyPair: TPair<string, TOpenAPI3Property>;
  PropertyName, PropertyType: string;
begin
  Schema := FParser.Schema;
  ModelBuilder := TNovusStringBuilder.Create;
  try
    // Iterate over the schemas and generate model classes
    for var Pair in Schema.Components.Schemas do
    begin
      SchemaName := Pair.Key;
      ModelBuilder.Clear;
      ModelBuilder.AppendLine('unit ' + GetNameSpace(SchemaName) + ';');
      ModelBuilder.AppendLine('');
      ModelBuilder.AppendLine('interface');
      ModelBuilder.AppendLine('');
      ModelBuilder.AppendLine('type');
      ModelBuilder.AppendLine('  T' + GetNameSpace(SchemaName) + ' = class');
      ModelBuilder.AppendLine('  private');

      // Iterate over properties and generate fields
      for PropertyPair in Pair.Value.Properties do
      begin
        PropertyName := PropertyPair.Value.Name;
        PropertyType := MapOpenAPITypeToDelphiType(PropertyPair.Value.Type_, PropertyPair.Value.Format);
        ModelBuilder.AppendLine('    F' + PropertyName + ': ' + PropertyType + ';');
      end;

      ModelBuilder.AppendLine('  public');

      // Generate getters and setters
      for PropertyPair in Pair.Value.Properties do
      begin
        PropertyName := PropertyPair.Value.Name;
        PropertyType := MapOpenAPITypeToDelphiType(PropertyPair.Value.Type_, PropertyPair.Value.Format);
        ModelBuilder.AppendLine('    function Get' + PropertyName + ': ' + PropertyType + ';');
        ModelBuilder.AppendLine('    procedure Set' + PropertyName + '(const Value: ' + PropertyType + ');');
        ModelBuilder.AppendLine('    property ' + PropertyName + ': ' + PropertyType + ' read Get' + PropertyName + ' write Set' + PropertyName + ';');
      end;

      ModelBuilder.AppendLine('  end;');
      ModelBuilder.AppendLine('');
      ModelBuilder.AppendLine('implementation');
      ModelBuilder.AppendLine('');

      // Implement getters and setters
      for PropertyPair in Pair.Value.Properties do
      begin
        PropertyName := PropertyPair.Value.Name;
        PropertyType := MapOpenAPITypeToDelphiType(PropertyPair.Value.Type_, PropertyPair.Value.Format);
        ModelBuilder.AppendLine('function T' + GetNameSpace(SchemaName) + '.Get' + PropertyName + ': ' + PropertyType + ';');
        ModelBuilder.AppendLine('begin');
        ModelBuilder.AppendLine('  Result := F' + PropertyName + ';');
        ModelBuilder.AppendLine('end;');
        ModelBuilder.AppendLine('');
        ModelBuilder.AppendLine('procedure T' + GetNameSpace(SchemaName) + '.Set' + PropertyName + '(const Value: ' + PropertyType + ');');
        ModelBuilder.AppendLine('begin');
        ModelBuilder.AppendLine('  F' + PropertyName + ' := Value;');
        ModelBuilder.AppendLine('end;');
        ModelBuilder.AppendLine('');
      end;

      ModelBuilder.AppendLine('end.');

      WriteToFile(TNovusFileUtils.TrailingBackSlash(FOutputDir) + 'Model\'+ GetNameSpaceFilename(SchemaName), ModelBuilder.ToString);
    end;
  finally
    ModelBuilder.Free;
  end;
end;

function TNovusOpenAPICodegen.GetFirstPartAfterSlash(const InputStr: string): string;
var
  SlashPos: Integer;
  TempStr: string;
begin
  // Find the position of the first '/'
  SlashPos := Pos('/', InputStr);
  // If no '/' is found, return an empty string
  if SlashPos = 0 then
    Exit('');
  // Remove the part of the string before and including the first '/'
  TempStr := Copy(InputStr, SlashPos + 1, Length(InputStr) - SlashPos);
  // Find the position of the next '/'
  SlashPos := Pos('/', TempStr);
  // If another '/' is found, return the part before it; otherwise, return the whole TempStr
  if SlashPos > 0 then
    Result := Copy(TempStr, 1, SlashPos - 1)
  else
    Result := TempStr;
end;

procedure TNovusOpenAPICodegen.GenerateAPIClasses;
var
  Paths: TObjectDictionary<string, TOpenAPI3Path>;
  APIBuilder: TNovusStringBuilder;
  lsEndPoint: string;
  lsClassName: String;
  Path: TOpenAPI3Path;
  OpenAPIClassBuilderList: tNovusOpenAPIClassBuilderList;
  OpenAPIClassBuilder: tNovusOpenAPIClassBuilder;
  Method: string;
  FunctionName: String;
  (*

  OperationPair: TPair<string, TOpenAPI3Operation>;
  Parameter: TObject;
  PathName, ClassName, MethodName, MethodParams, ModelParam: string;
  ModelTypeName: string;
  *)



begin
  Paths := FParser.Schema.Paths;
  APIBuilder := TNovusStringBuilder.Create;

  APIBuilder.Clear;
  try
    OpenAPIClassBuilderList:= tNovusOpenAPIClassBuilderList.Create(tNovusOpenAPIClassBuilder);

    // Iterate over the paths and generate API classes
    for var PathPair in Paths do
    begin
      lsEndPoint := PathPair.Key;
      lsClassName := GetFirstPartAfterSlash(lsEndPoint);

      APIBuilder.AppendLine('ClassName: ' + lsClassname + ' Endpoint: ' + lsEndPoint);

      OpenAPIClassBuilder :=  OpenAPIClassBuilderList.FindItem(lsClassName) As TNovusOpenAPICLassBuilder;
      if Not Assigned(OpenAPIClassBuilder) then
         begin
            OpenAPIClassBuilder := TNovusOpenAPICLassBuilder.Create();
            OpenAPIClassBuilder.ClassName_ := lsClassName;

            OpenAPIClassBuilderList.Add(lsClassName,OpenAPIClassBuilder);
          end;

      Path := PathPair.Value;

      for var Operation in Path.Operations do
      begin
        Method := Operation.Key;
        FunctionName :=  Operation.Value.OperationId;


        for var Parameter in Operation.Value.Parameters do
          begin




          end;




        //Operation.Value.Parameters


      end;


    //  OpenAPIClassBuilder.AddFunction


  {
      Path := PathPair.Value;
      PathName := PathPair.Key;
      ClassName := 'T' + GetNameSpace(RemoveSlashes(PathName));

      APIBuilder.Clear;
      APIBuilder.AppendLine('unit ' + GetNameSpace(ConvertToDotNotation(PathName)) + ';');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('interface');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('uses');
      APIBuilder.AppendLine('  System.SysUtils, System.Classes, ' + TitleClassName + '.Models;');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('type');
      APIBuilder.AppendLine('  ' + ClassName + ' = class');
      APIBuilder.AppendLine('  public');

      // Add methods based on the operations in the path
      for OperationPair in Path.Operations do
      begin
        MethodName := OperationPair.Key;
        MethodParams := '';

        // Extract parameters and build the method signature

        for Parameter in OperationPair.Value.Parameters do
        begin
          if Parameter is TOpenAPI3Parameter then
          begin
            var ParamName := TOpenAPI3Parameter(Parameter).Name;
            var ParamSchema := TOpenAPI3Parameter(Parameter).Schema;
            var ParamType := 'string'; // Default to string if schema is not available
            (*
            // If the parameter has a schema, map it to a Delphi class
            if Assigned(ParamSchema) then
            begin
              if ParamSchema.Type_ = 'object' then
                ParamType := 'T' + GetNameSpace(ParamName) // Assuming schema is named after the parameter
              else
                ParamType := MapOpenAPITypeToDelphiType(ParamSchema.Type_, ParamSchema.Format);
            end;

            if MethodParams <> '' then
              MethodParams := MethodParams + '; ';
            MethodParams := MethodParams + 'const ' + ParamName + ': ' + ParamType;
            *)
          end;

        end;
        (*
        // Determine the model type based on the request body schema if applicable
        if Assigned(OperationPair.Value.RequestBody) then
        begin
          for var ContentPair in OperationPair.Value.RequestBody.Content do
          begin
            ModelTypeName := 'T' + GetNameSpace(ContentPair.Key); // Assume the model class is named after the schema
            if MethodParams <> '' then
              MethodParams := MethodParams + '; ';
            MethodParams := MethodParams + 'const Model: ' + ModelTypeName;
          end;
        end;
        *)



        if MethodParams = '' then
          MethodParams := '()'
        else
          MethodParams := '(' + MethodParams + ')';

        APIBuilder.AppendLine('    function ' + MethodName + MethodParams + ': string;');  // Function signature with parameters
      end;

      APIBuilder.AppendLine('  end;');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('implementation');
      APIBuilder.AppendLine('');

      // Implement methods
      for OperationPair in Path.Operations do
      begin
        MethodName := OperationPair.Key;

        APIBuilder.AppendLine('function ' + ClassName + '.' + MethodName + ': string;');
        APIBuilder.AppendLine('begin');
        APIBuilder.AppendLine('  // Implement API call here');
        APIBuilder.AppendLine('end;');
        APIBuilder.AppendLine('');
      end;

      APIBuilder.AppendLine('end.');

      WriteToFile(TNovusFileUtils.TrailingBackSlash(FOutputDir) + 'API\' + GetNameSpaceFileName(ConvertToDotNotation(PathName)), APIBuilder.ToString);
      }
    end;
  finally
    WriteToFile(TNovusFileUtils.TrailingBackSlash(FOutputDir) + 'API\PathList.pas', APIBuilder.ToString);

    APIBuilder.Free;
    OpenAPIClassBuilderList.Free;
  end;
end;

procedure TNovusOpenAPICodegen.WriteToFile(const AFileName, AContent: string);
begin
  try
    TFile.WriteAllText(AFileName, AContent);
  except
    on E: Exception do
      WriteLn('Error writing to file ' + AFileName + ': ' + E.Message);
  end;
end;

function TNovusOpenAPICodegen.GetNameSpace(Key: String): String;
begin
  Result := TitleClassName + Key;
end;

function TNovusOpenAPICodegen.GetNameSpaceFilename(Key: String): String;
begin
  if FUseFilenameSeparator then
     Result := Format('%s.%s.pas', [TitleClassName, Key])
  else
    Result := Format('%s.%s.pas', [TitleClassName, Key]);
end;

function TNovusOpenAPICodegen.MapOpenAPITypeToDelphiType(const OpenAPIType, OpenAPIFormat: string): string;
begin
  if OpenAPIType = 'string' then
    Result := 'string'
  else if OpenAPIType = 'integer' then
    Result := 'Integer'
  else if OpenAPIType = 'boolean' then
    Result := 'Boolean'
  else if (OpenAPIType = 'number') and (OpenAPIFormat = 'float') then
    Result := 'Single'
  else if (OpenAPIType = 'number') and (OpenAPIFormat = 'double') then
    Result := 'Double'
  else
    Result := 'string'; // Default to string if type is unknown
end;

function TNovusOpenAPICodegen.ConvertToDotNotation(const Method: string): string;
var
  CleanMethod: string;
begin
  // Remove leading and trailing slashes
  CleanMethod := Method;
  if (Length(CleanMethod) > 0) and (CleanMethod[1] = '/') then
    Delete(CleanMethod, 1, 1);
  if (Length(CleanMethod) > 0) and (CleanMethod[Length(CleanMethod)] = '/') then
    Delete(CleanMethod, Length(CleanMethod), 1);
  // Replace remaining slashes with dots
  CleanMethod := StringReplace(CleanMethod, '/', '.', [rfReplaceAll]);
  Result := CleanMethod;
end;

function TNovusOpenAPICodegen.RemoveSlashes(const Input: string): string;
begin
  // Replace all slashes with an empty string
  Result := StringReplace(Input, '/', '', [rfReplaceAll]);
end;

end.

