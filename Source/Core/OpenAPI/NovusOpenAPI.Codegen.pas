unit NovusOpenAPI.Codegen;

interface

uses
  NovusStringBuilder, NovusOpenAPI.Parser, NovusObject, System.SysUtils,
  System.Generics.Collections, System.JSON, System.IOUtils, NovusFileUtils;

type
  TNovusOpenAPICodegen = class(TNovusObject)
  private
    FTitleClassname: String;
    FUseFilenameSeparator: Boolean;
    FParser: TNovusOpenAPIParser;
    FOutputDir: string;

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

procedure TNovusOpenAPICodegen.GenerateAPIClasses;
var
  Paths: TObjectDictionary<string, TOpenAPI3Path>;
  APIBuilder: TNovusStringBuilder;
  Path: TOpenAPI3Path;
  OperationPair: TPair<string, TOpenAPI3Operation>;
  PathName, ClassName, MethodName: string;
begin
  Paths := FParser.Schema.Paths;
  APIBuilder := TNovusStringBuilder.Create;
  try
    // Iterate over the paths and generate API classes
    for var PathPair in Paths do
    begin
      Path := PathPair.Value;
      PathName := PathPair.Key;
      ClassName := 'T' + GetNameSpace(RemoveSlashes(PathName));

      APIBuilder.Clear;
      APIBuilder.AppendLine('unit ' + GetNameSpace(ConvertToDotNotation(PathName)) +';');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('interface');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('uses');
      APIBuilder.AppendLine('  System.SysUtils, System.Classes;');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('type');
      APIBuilder.AppendLine('  ' + ClassName + ' = class');
      APIBuilder.AppendLine('  public');

      // Add methods based on the operations in the path
      for OperationPair in Path.Operations do
      begin
        MethodName := OperationPair.Key;
        APIBuilder.AppendLine('    function ' + MethodName + ': string;');  // Only function names are generated
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

      WriteToFile(TNovusFileUtils.TrailingBackSlash(FOutputDir) +'API\'+ GetNameSpaceFileName(ConvertToDotNotation(PathName)),  APIBuilder.ToString);
    end;
  finally
    APIBuilder.Free;
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

