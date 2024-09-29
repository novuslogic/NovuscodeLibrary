unit NovusOpenAPI.Codegen;

interface

uses
  NovusStringBuilder, NovusOpenAPI.Parser, NovusObject, System.SysUtils,
  System.Generics.Collections, System.JSON, System.IOUtils, NovusFileUtils,
  NovusList, NovusStringUtils, NovusOpenAPI.Utils;

type
  TNovusOpenAPIClassBuilderList = class(TNovusList);
  TNovusOpenAPIClassFunctionList = class(TNovusList);
  TNovusOpenAPIClassParameterList = class(TNovusList);


  TNovusOpenAPIClassParameter = class(TNovusObject)
  protected
  private
    fParamName: string;
    fParamDescription: string;
    fParamtype: string;
    fParamRequired: Boolean;
    fParamIn: string;
    FParamFormat: string;
  public
    constructor Create(aParamName: string;
    aParamDescription: string;
    aParamtype: string;
    aParamRequired: Boolean;
    aParamIn: string;
    aParamFormat: string);

    destructor Destroy; override;

    property ParamName: string read fParamName write fParamName;
    property ParamDescription: string read fParamDescription write fParamDescription  ;
    property Paramtype: string read fParamtype write fParamtype;
    property ParamRequired: Boolean read fParamRequired write fParamRequired;
    property ParamIn: string read fParamIn write fParamIn;
    property ParamFormat: string read fParamFormat write fParamFormat;
  end;

  TNovusOpenAPIClassFunction  = class(TNovusObject)
  protected
  private
    fParameterList: TNovusOpenAPIClassParameterList;
    fEndPoint:String;
    fMethod: string;
    fFunctionName: string;
  public
    constructor Create(aFunctionName: string; aMethod: string; aEndPoint:String);
    destructor Destroy; override;

    function BuildFullFunction(aWithProc: Boolean): string;

    procedure AddParameter(aParamName: string; aParamDescription: string;
      aParamtype: string; aParamRequired: Boolean; aParamIn: String;
      aParamFormat: string);

    property FunctionName: String  read fFunctionName write fFunctionName;

    property ParameterList: TNovusOpenAPIClassParameterList read fParameterList write fParameterList;
  end;


  TNovusOpenAPIClassBuilder = class(TNovusObject)
  protected
  private
    fPath: TOpenAPI3Path;
    fClassName: String;
    FFunctionList: TNovusOpenAPIClassFunctionList;
  public
    constructor Create;
    destructor Destroy; override;

    function AddFunction(aFunctionName: string; aMethod: string; aEndPoint:String): TNovusOpenAPIClassFunction;

    property ClassName_: String read fClassname write fClassName;

    property FunctionList: TNovusOpenAPIClassFunctionList read FFunctionList write FFunctionList;

    property Path: TOpenAPI3Path read fPath write fPath;
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
        PropertyType := TNovusOpenAPIUtils.OpenAPITypeToDelphiType(PropertyPair.Value.Type_, PropertyPair.Value.Format);
        ModelBuilder.AppendLine('    F' + PropertyName + ': ' + PropertyType + ';');
      end;

      ModelBuilder.AppendLine('  public');

      // Generate getters and setters
      for PropertyPair in Pair.Value.Properties do
      begin
        PropertyName := PropertyPair.Value.Name;
        PropertyType := TNovusOpenAPIUtils.OpenAPITypeToDelphiType(PropertyPair.Value.Type_, PropertyPair.Value.Format);
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
        PropertyType := TNovusOpenAPIUtils.OpenAPITypeToDelphiType(PropertyPair.Value.Type_, PropertyPair.Value.Format);
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
  OpenAPIClassFunction:TNovusOpenAPIClassFunction;
  OpenAPIClassBuilder: tNovusOpenAPIClassBuilder;
  Method: string;
  FunctionName: String;
  Paramtype: string;
  ParamFormat: string;
  ParamName: String;
  ParamDescription: string;
  ParamRequired: Boolean;
  ParamIn: String;
  FullFunction: String;
  ResponseDescription: string;
  ResponseCode: string;
begin
  Paths := FParser.Schema.Paths;

  try
    OpenAPIClassBuilderList := TNovusOpenAPIClassBuilderList.Create
      (TNovusOpenAPIClassBuilder);

    // Iterate over the paths and generate API classes
    for var PathPair in Paths do
    begin
      lsEndPoint := PathPair.Key;
      lsClassName := GetFirstPartAfterSlash(lsEndPoint);
      Path := PathPair.Value;

      OpenAPIClassBuilder := OpenAPIClassBuilderList.FindItem(lsClassName)
        As TNovusOpenAPIClassBuilder;
      if Not Assigned(OpenAPIClassBuilder) then
      begin
        OpenAPIClassBuilder := TNovusOpenAPIClassBuilder.Create();
        OpenAPIClassBuilder.ClassName_ := lsClassName;
        OpenAPIClassBuilder.Path := Path;

        OpenAPIClassBuilderList.Add(lsClassName, OpenAPIClassBuilder);
      end;



      for var Operation in Path.Operations do
      begin
        Method := Operation.Key;
        FunctionName := Operation.Value.OperationId;

        OpenAPIClassFunction := OpenAPIClassBuilder.AddFunction(FunctionName,
          Method, lsEndPoint);

        If Assigned(OpenAPIClassFunction) then
        begin
          for var Parameter in Operation.Value.Parameters do
          begin
            ParamName := Parameter.Name;
            ParamDescription := Parameter.Description;
            Paramtype := Parameter.Schema.Type_;
            ParamRequired := Parameter.Required;
            ParamIn := Parameter.In_;
            ParamFormat := Parameter.Schema.Format_;

            OpenAPIClassFunction.AddParameter(ParamName, ParamDescription,
              Paramtype, ParamRequired, ParamIn, ParamFormat);

          end;

         for var Response in Operation.Value.Responses.Responses do
           begin
             ResponseCode := Response.Key;
             ResponseDescription :=  Response.Value.Description ;

              for var MediaType in Response.Value.Content do
                begin
                  Item



                end;

           end;
        end;
      end;
    end;

    for OpenAPIClassBuilder in OpenAPIClassBuilderList.List do
      begin
        lsClassName := 'T' + TitleClassName + TNovusStringUtils.UpLower(OpenAPIClassBuilder.ClassName_, true);

        Try
          APIBuilder := TNovusStringBuilder.Create;


          APIBuilder.AppendLine('unit ' + TitleClassName + '.' + TNovusStringUtils.UpLower(OpenAPIClassBuilder.ClassName_, true) + ';');
          APIBuilder.AppendLine('');
          APIBuilder.AppendLine('interface');
          APIBuilder.AppendLine('');
          APIBuilder.AppendLine('uses');
          APIBuilder.AppendLine('  System.SysUtils, System.Classes;');
          APIBuilder.AppendLine('');
          APIBuilder.AppendLine('type');
          APIBuilder.AppendLine('  ' + lsClassName + ' = class');
          APIBuilder.AppendLine('  public');


          For OpenAPIClassFunction in OpenAPIClassBuilder.FunctionList do
          begin
            FullFunction := OpenAPIClassFunction.BuildFullFunction(True);
            APIBuilder.AppendLine('     ' + FullFunction);
          end;
          APIBuilder.AppendLine('  end;');
          APIBuilder.AppendLine('');

          APIBuilder.AppendLine('implementation');

          APIBuilder.AppendLine('');

          For OpenAPIClassFunction in OpenAPIClassBuilder.FunctionList do
          begin
           APIBuilder.AppendLine('procedure ' + TitleClassName + TNovusStringUtils.UpLower(OpenAPIClassBuilder.ClassName_, true)  + '.' + OpenAPIClassFunction.BuildFullFunction(false));
           APIBuilder.AppendLine('begin');
           APIBuilder.AppendLine('');
           APIBuilder.AppendLine('end;');
           APIBuilder.AppendLine('');
          end;

          APIBuilder.AppendLine('');

          APIBuilder.AppendLine('end.');

        Finally
          WriteToFile(TNovusFileUtils.TrailingBackSlash(FOutputDir) + 'API\' + TitleClassName + '.' + TNovusStringUtils.UpLower(OpenAPIClassBuilder.ClassName_, true) + '.pas', APIBuilder.ToString);

          APIBuilder.Free;
        End;

      end;






    //





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






  finally
//    WriteToFile(TNovusFileUtils.TrailingBackSlash(FOutputDir) + 'API\PathList.pas', APIBuilder.ToString);

//    APIBuilder.Free;
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

// TNovusOpenAPIClassBuilder
constructor TNovusOpenAPIClassBuilder.Create;
begin
  FFunctionList:= TNovusOpenAPIClassFunctionList.Create(TNovusOpenAPIClassFunction);

  inherited Create;
end;

destructor TNovusOpenAPIClassBuilder.Destroy;
begin
  FFunctionList.Free;

  inherited Destroy;
end;

function TNovusOpenAPIClassBuilder.AddFunction(aFunctionName: string; aMethod: string; aEndPoint:String): TNovusOpenAPIClassFunction;
begin
  Var OpenAPIClassFunction := TNovusOpenAPIClassFunction.Create(aFunctionName, aMethod, aEndPoint);

  FFunctionList.Add(OpenAPIClassFunction);

  Result := OpenAPIClassFunction;
end;


// TNovusOpenAPIClassFunction
constructor TNovusOpenAPIClassFunction.Create(aFunctionName: string; aMethod: string; aEndPoint:String);
begin
  inherited Create;

  fFunctionName := aFunctionName;
  fMethod := aMethod;
  fEndPoint := aEndPoint;


  fParameterList:= TNovusOpenAPIClassParameterList.Create(TNovusOpenAPIClassParameter);
end;

destructor TNovusOpenAPIClassFunction.Destroy;
begin
  fParameterList.Free;

  inherited Destroy;
end;

procedure TNovusOpenAPIClassFunction.AddParameter(aParamName: string;
                                                  aParamDescription: string;
                                                  aParamtype: string;
                                                  aParamRequired: Boolean;
                                                  aParamIn: String;
                                                  aParamFormat: String);

begin
  Var NovusOpenAPIClassParameter := TNovusOpenAPIClassParameter.Create(aParamName,
                                                  aParamDescription,
                                                  aParamtype,
                                                  aParamRequired,
                                                  aParamIn,
                                                  aParamFormat);

  fParameterList.Add(NovusOpenAPIClassParameter);
end;

function TNovusOpenAPIClassFunction.BuildFullFunction(aWithProc: Boolean): string;
var
  lParameter: TNovusOpenAPIClassParameter;
  Params: String;
  ParamCount: integer;
  ParamName: String;
begin
  Result := '';

  Params := '(';
  ParamCount := 0;
  For lParameter in ParameterList do
    begin
      ParamName := TNovusOpenAPIUtils.OpenAPIParamToDelphiFunctionType(lParameter.ParamName);
      Params := Params + ParamName + ': ' +
            TNovusOpenAPIUtils.OpenAPITypeToDelphiType(lParameter.ParamType, lParameter.ParamFormat);

     Inc(ParamCount);
     if ParamCount < ParameterList.Count then
         Params := Params + '; ';
    end;

  if Params = '' then Params := '()'
    else  Params := Params + ')';

  if aWithProc then
    Result := 'procedure ' + FunctionName +  Params + ';'
  else
     Result := FunctionName +  Params + ';';
end;

// TNovusOpenAPIClassParameter
constructor TNovusOpenAPIClassParameter.Create(aParamName: string;
    aParamDescription: string;
    aParamtype: string;
    aParamRequired: Boolean;
    aParamIn: string;
    aParamFormat: string);
begin
  inherited Create;

  fParamName := aParamName;
  fParamDescription := aParamDescription;
  fParamtype := aParamtype;
  fParamRequired := aParamRequired;
  fParamIn := aParamIn;
  fParamFormat := aParamFormat;
end;

destructor TNovusOpenAPIClassParameter.Destroy;
begin
  inherited Destroy;
end;




end.
