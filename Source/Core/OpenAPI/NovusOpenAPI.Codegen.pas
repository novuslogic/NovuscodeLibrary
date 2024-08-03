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


    function GetNameSpaceFileName(Key: String): String;
    function GetNameSpace(Key: String): String;
    procedure GenerateModelClasses;
    procedure GenerateAPIClasses;
    procedure WriteToFile(const AFileName, AContent: string);
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
    //GenerateAPIClasses;
  except
    on E: Exception do
      WriteLn('Error generating code: ' + E.Message);
  end;
end;

procedure TNovusOpenAPICodegen.GenerateModelClasses;
var
  Schema: TOpenAPI3Schema;
  ModelBuilder: TNovusStringBuilder;
  PropertyPair: TJSONPair;
  SchemaName, PropertyName, PropertyType: string;
  PropertiesObject: TJSONObject;
begin
  Schema := FParser.Schema;
  ModelBuilder := TNovusStringBuilder.Create;
  try
    // Iterate over the schemas and generate model classes
    for var Pair in Schema.Components.Schemas do
    begin
      SchemaName := Pair.Key;
      PropertiesObject := Pair.Value.Properties; // Accessing the new Properties property
      
      ModelBuilder.Clear;
      ModelBuilder.AppendLine('unit ' + GetNameSpace(SchemaName) + ';');
      ModelBuilder.AppendLine('');
      ModelBuilder.AppendLine('interface');
      ModelBuilder.AppendLine('');
      ModelBuilder.AppendLine('type');
      ModelBuilder.AppendLine('  T' + GetNameSpace(SchemaName) + ' = class');
      ModelBuilder.AppendLine('  private');

      // Iterate over properties and generate fields
      if Assigned(PropertiesObject) then
      begin
        for PropertyPair in PropertiesObject do
        begin
          PropertyName := PropertyPair.JsonString.Value;
          PropertyType := 'string'; // Default to string, adjust based on actual type
          ModelBuilder.AppendLine('    F' + PropertyName + ': ' + PropertyType + ';');
        end;
      end;

      ModelBuilder.AppendLine('  public');

      // Generate getters and setters
      if Assigned(PropertiesObject) then
      begin
        for PropertyPair in PropertiesObject do
        begin
          PropertyName := PropertyPair.JsonString.Value;
          PropertyType := 'string'; // Default to string, adjust based on actual type
          ModelBuilder.AppendLine('    function Get' + PropertyName + ': ' + PropertyType + ';');
          ModelBuilder.AppendLine('    procedure Set' + PropertyName + '(const Value: ' + PropertyType + ');');
          ModelBuilder.AppendLine('    property ' + PropertyName + ': ' + PropertyType + ' read Get' + PropertyName + ' write Set' + PropertyName + ';');
        end;
      end;

      ModelBuilder.AppendLine('  end;');
      ModelBuilder.AppendLine('');
      ModelBuilder.AppendLine('implementation');
      ModelBuilder.AppendLine('');

      // Implement getters and setters
      if Assigned(PropertiesObject) then
      begin
        for PropertyPair in PropertiesObject do
        begin
          PropertyName := PropertyPair.JsonString.Value;
          PropertyType := 'string'; // Default to string, adjust based on actual type
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
      end;

      ModelBuilder.AppendLine('end.');

      WriteToFile(TNovusFileUtils.TrailingBackSlash(FOutputDir) + GetNameSpaceFilename(SchemaName), ModelBuilder.ToString);
    end;
  finally
    ModelBuilder.Free;
  end;
end;

procedure TNovusOpenAPICodegen.GenerateAPIClasses;
var
  Paths: TObjectDictionary<string, TOpenAPI3Path>;
  APIBuilder: TNovusStringBuilder;
begin
  Paths := FParser.Schema.Paths;
  APIBuilder := TNovusStringBuilder.Create;
  try
    // Iterate over the paths and generate API classes
    for var Pair in Paths do
    begin
      APIBuilder.Clear;
      APIBuilder.AppendLine('unit ' + Pair.Key + 'API;');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('interface');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('type');
      APIBuilder.AppendLine('  T' + Pair.Key + 'API = class');
      APIBuilder.AppendLine('  private');
      // Add fields based on the path properties
      APIBuilder.AppendLine('  public');
      // Add methods based on the operations in the path
      APIBuilder.AppendLine('  end;');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('implementation');
      APIBuilder.AppendLine('');
      APIBuilder.AppendLine('end.');

      WriteToFile(FOutputDir + Pair.Key + 'API.pas', APIBuilder.ToString);
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

end.

