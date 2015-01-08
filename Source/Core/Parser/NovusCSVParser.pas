unit NovusCSVParser;

interface

uses
  Classes, NovusList, NovusParser;

type
  TNovusCSVData = Class(TObject)
  private
    FFields: TStringList;
    FLine: String;
  public
     constructor Create;
     destructor Destroy; override;

     Property Fields: TStringList
       read FFields
       write FFields;

     property Line: String
       read FLine
       write FLine;
  End;


  TValueSeparatorType = (vsComma, vsTab);

  TNovusCsvParser = class(TNovusParser)
  private
    FCSVDataList: TNovusList;
    FIsFirstLine: boolean;
    FIgnoreFirstLine: boolean;
    FIsFileOpen: boolean;
    FFields: TStrings;
    FFile: TextFile;
    FLine: string;
    FValueSeparatorType: TValueSeparatorType;
    FValueSeparator: char;

    procedure SetValueSeparatorType(const Value: TValueSeparatorType);

  public
    constructor Create;
    destructor Destroy; override;

    property IgnoreFirstLine: boolean read FIgnoreFirstLine write FIgnoreFirstLine;
    property Fields: TStrings read FFields;
    property Line: string read FLine;
    property ValueSeparatorType: TValueSeparatorType read FValueSeparatorType write SetValueSeparatorType;
    property ValueSeparator: char read FValueSeparator write FValueSeparator;

    procedure Close;

    procedure SaveToCSVFile(Const Filename: String);
    procedure LoadFromFile(const FileName: string; DoParseCSV: Boolean = true);

    function  ReadLn: boolean;
    procedure ParseCSVFile;
    procedure ParseCSVLine(ALine: string; AFields: TStrings; ValueSeparator: char = ',');

    property CSVDataList: tNovusList
      read FCSVDataList
      write FCSVDataList;

  end;



implementation

uses SysUtils;

procedure TNovusCSVParser.ParseCSVLine(ALine: string; AFields: TStrings; ValueSeparator: char);
var
iState:  cardinal;
i:       cardinal;
iLength: cardinal;
sField:  string;
begin
  iLength := Length(ALine);

  if iLength = 0 then
    Exit;

  iState := 0;
  sField := '';


  for i := 1 to iLength do
  begin
    case iState of

      0:
      begin
        sField := '';
        if ALine[i] = '"' then
        begin
          iState := 2;
        end
        else if ALine[i] = ValueSeparator then
        begin
          AFields.Add(sField);
          if (i = iLength) then // EOL //
            AFields.Add('');
        end
        else
        begin
          sField := ALine[i];
          iState := 1;
          if (i = iLength) then // EOL //
            AFields.Add(sField);
        end;
      end;

//----------------------------------------------------------------------//

      1: // continuation of regular field //
      begin
        if ALine[i] = ValueSeparator then  // end of regular field //
        begin
          AFields.Add(sField);
          // if end of input, then we know there remains a "null" field //
          if (i = iLength) then
          begin
            AFields.Add('');
          end // (i = iLength) //
          else
          begin
            iState := 0;
          end;
        end
        else  // concatenate current char //
        begin
          sField := sField + ALine[i];
          if (i = iLength) then // EOL //
            AFields.Add(sField);
        end; // if ALine[i] = //
      end; // 1 //

//----------------------------------------------------------------------//

      2: // continuation of embedded quotes or commas //
      begin
        case ALine[i] of
          '"': // end of embedded comma field or beginning of embedded quote
//
          begin
            if (i < iLength) then // NotTheEndPos //
            begin
              if (ALine[i+1] = ValueSeparator) then
              begin // end of embedded comma field //
                iState := 1
              end
              else
              begin
                iState := 3;
              end;
            end
            else
            begin // end of field since end of line //
              AFields.Add(sField);
            end;
          end
          else // concatenate current char //
          begin
            sField := sField + ALine[i];
          end;
        end;
      end;
      3:
      begin
        case ALine[i] of
          '"':
          begin
            sField := sField + ALine[i];
            iState := 2;
          end;
        end;
      end;
    end;
  end;
end;


procedure TNovusCSVParser.Close;
begin
  if FIsFileOpen = true then
  begin
    CloseFile(FFile);
    FIsFileOpen := false;
  end;
end;


constructor TNovusCSVParser.Create;
begin
  inherited;
  FIgnoreFirstLine := false;
  FFields := TStringList.Create;
  SetValueSeparatorType(vsComma);

  FCSVDataList := TNovusList.Create(TNovusCSVData);
end;


destructor TNovusCSVParser.Destroy;
begin
  Close;

  FCSVDataList.Free;
  FFields.Free;
  inherited;
end;


procedure TNovusCSVParser.LoadFromFile(const FileName: string;DoParseCSV: Boolean = true);
begin
  FIsFileOpen := false;
  AssignFile(FFile, FileName);
  {$IFDEF WIN32}
  FileMode := fmOpenRead;
  {$ENDIF}
  Reset(FFile);
  FIsFirstLine := true;
  FIsFileOpen := true;

  If DoParseCSV then ParseCSVFile;
end;

procedure TNovusCSVParser.ParseCSVFile;
Var
  lCSVData: TNovusCSVData;
  I: Integer;
begin
  while ReadLn do
    begin
      lCSVData := TNovusCSVData.Create;

      lCSVData.Line := Line;

      for I := 0 to Fields.Count - 1 do
        lCSVData.Fields.Add(Fields[I]);

      CSVDataList.Add(lCSVData);
    end;
end;

function TNovusCSVParser.ReadLn: boolean;
var
  isOkay: boolean;
begin
  isOkay := false;
  FFields.Clear;

  // Skip the first line if requested.
  if (FIsFirstLine = true) and (FIgnoreFirstLine = true) then
  begin
    if Eof(FFile) = false then
    begin
      {$IFDEF CLR}
      Borland.Delphi.System.Readln(FFile,FLine);
      {$ELSE}
      System.Readln(FFile,FLine);
      {$ENDIF}
      FIsFirstLine := false;
    end;
  end;

  if Eof(FFile) = false then
  begin
    {$IFDEF CLR}
    Borland.Delphi.System.Readln(FFile,FLine);
    {$ELSE}
    System.Readln(FFile,FLine);
    {$ENDIF}
    ParseCSVLine(FLine, FFields, FValueSeparator);

    isOkay := true;
  end;
  result := isOkay;
end;

procedure TNovusCSVParser.SetValueSeparatorType(
  const Value: TValueSeparatorType);
begin
  FValueSeparatorType := value;

  // Set the value separator based on the specified type.
  case FValueSeparatorType of
    vsComma: FValueSeparator := ',';
    vsTab: FValueSeparator := #09;
  end;
end;

procedure TNovusCSVParser.SaveToCSVFile(Const Filename: String);
Var
  FCSVData: tCSVData;
  FLine: String;
  I, X: Integer;
begin
  ReWrite(FOutFile, FileName);

  If Not IgnoreFirstLine then
    begin
      FLine := '';
      For I := 0 to Fields.Count - 1 do
        begin
          Fline := Fline + Fields.Strings[I];

          If I < Fields.Count -1 then
            Fline := Fline + FValueSeparator;
        end;

      {$IFNDEF CLR}
      System.Writeln(FoutFile,FLine);
      {$ENDIF}
     end;
     
  For I := 0 to CSVDataList.Count -1 do
    begin
      FCSVData := tCSVData(CSVDataList.Items[I]);

      FLine := '';
      For x := 0 to FCSVData.Fields.Count -1 do
        begin
          Fline := Fline + FCSVData.Fields.Strings[x];

          If X < FCSVData.Fields.Count -1 then
            Fline := Fline + FValueSeparator;
        end;

      {$IFNDEF CLR}
      System.Writeln(FoutFile,FLine);
      {$ENDIF}
    end;

  CloseFile(FOutFile);
end;

constructor TNovusCSVData.Create;
begin
  inherited;

  FFields := TStringList.Create;
end;

destructor TNovusCSVData.Destroy;
begin
  FFields.Free;

  inherited;
end;


end.
