unit NovusSimpleJSON;

interface

uses NovusUtilities, System.JSON, System.SysUtils, System.Classes, NovusJSONUtils;


type
  tNovusSimpleJSON = class(tNovusUtilities)
  protected
  private
    FJSONString: string;
    FJSONData: TJSONObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function LoadfromJSON(const aFileName: string): boolean;
  end;

implementation

constructor tNovusSimpleJSON.Create;
begin
  FJSONString := '';
  FJSONData := nil;
end;

destructor tNovusSimpleJSON.Destroy;
begin
  If Assigned(FJSONData) then
    begin
      FJSONData.Free;
      FJSONData := nil;
    end;
end;

function tNovusSimpleJSON.LoadfromJSON(const aFileName: string): boolean;
var
  FileStream: TFileStream;
begin
  Result := False;
  If FileExists(aFilename) then Exit;

  // Load the JSON file into a string
  FileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(FJSONString, FileStream.Size);
    FileStream.ReadBuffer(Pointer(FJSONString)^, FileStream.Size);
  finally
    FileStream.Free;
  end;

  if Trim(FJSONString) = '' then Exit;

  // Parse the JSON string
  FJSONData := TJSONObject.ParseJSONValue(FJSONString) as TJSONObject;
end;


end.
