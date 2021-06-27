{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusWebUtils;

interface

Uses Classes, NovusUtilities, SysUtils, SHDocVw, VCL.forms, NovusShell,
     ActiveX, NovusNumUtils, UrlMon, Windows, System.Net.URLClient;


Type
  TMIMEType = record
    Extension: String;
    Kindofdocument: string;
    MIMEType: String;
  end;



Const
  BINDF_GETNEWESTVERSION = 16;

  MIMETypes: array[0..63] of TMIMEType =
  (
   (Extension : 'aac';Kindofdocument: 'AAC audio file'; MimeType : 'audio/aac')  ,
   (Extension : 'abw';Kindofdocument: 'AbiWord document'; MimeType : 'application/x-abiword'),
   (Extension : 'arc';Kindofdocument: 'Archive document (multiple files embedded)'; MimeType : 'application/octet-stream'),
   (Extension : 'avi';Kindofdocument: 'AVI: Audio Video Interleave'; MimeType : 'video/x-msvideo'),
   (Extension : 'azw';Kindofdocument: 'Amazon Kindle eBook format'; MimeType : 'application/vnd.amazon.ebook'),
   (Extension : 'bin';Kindofdocument: 'Any kind of binary data'; MimeType : 'application/octet-stream '),
   (Extension : 'bz';Kindofdocument: 'BZip archive'; MimeType : 'application/x-bzip'),
   (Extension : 'bz2';Kindofdocument: 'BZip2 archive'; MimeType : 'application/x-bzip2'),
   (Extension : 'csh';Kindofdocument: 'C-Shell script'; MimeType : 'application/x-csh'),
   (Extension : 'css';Kindofdocument: 'Cascading Style Sheets (CSS)'; MimeType : 'text/css'),
   (Extension : 'csv';Kindofdocument: 'Comma-separated values (CSV)'; MimeType : 'text/csv '),
   (Extension : 'doc';Kindofdocument: 'Microsoft Word'; MimeType : 'application/msword'),
   (Extension : 'eot';Kindofdocument: 'MS Embedded OpenType fonts'; MimeType : 'application/vnd.ms-fontobject'),
   (Extension : 'epub';Kindofdocument: 'Electronic publication (EPUB)'; MimeType : 'application/epub+zip'),
   (Extension : 'gif';Kindofdocument: 'Graphics Interchange Format (GIF)'; MimeType : 'image/gif'),
   (Extension : 'htm';Kindofdocument: 'HyperText Markup Language (HTML)'; MimeType : 'text/html'),
   (Extension : 'html';Kindofdocument: 'HyperText Markup Language (HTML)'; MimeType : 'text/html'),
   (Extension : 'ico';Kindofdocument: 'Icon format'; MimeType : 'image/x-icon'),
   (Extension : 'ics';Kindofdocument: 'iCalendar format'; MimeType : 'text/calendar'),
   (Extension : 'jar';Kindofdocument: 'Java Archive (JAR)'; MimeType : 'application/java-archive'),
   (Extension : 'jpeg';Kindofdocument: 'JPEG images'; MimeType : 'image/jpeg'),
   (Extension : 'jpg';Kindofdocument: 'JPEG images'; MimeType : 'image/jpeg'),
   (Extension : 'js';Kindofdocument: 'JavaScript (ECMAScript)'; MimeType : 'application/javascript'),
   (Extension : 'json';Kindofdocument: 'JSON format'; MimeType : 'application/json'),
   (Extension : 'mid';Kindofdocument: 'Musical Instrument Digital Interface (MIDI)'; MimeType : 'audio/midi'),
   (Extension : 'midi';Kindofdocument: 'Musical Instrument Digital Interface (MIDI)'; MimeType : 'audio/midi'),
   (Extension : 'mpeg';Kindofdocument: 'MPEG Video'; MimeType : 'video/mpeg'),
   (Extension : 'mpkg';Kindofdocument: 'Apple Installer Package'; MimeType : 'application/vnd.apple.installer+xml'),
   (Extension : 'odp';Kindofdocument: 'OpenDocuemnt presentation document'; MimeType : 'application/vnd.oasis.opendocument.presentation'),
   (Extension : 'ods';Kindofdocument: 'OpenDocuemnt spreadsheet document'; MimeType : 'application/vnd.oasis.opendocument.spreadsheet'),
   (Extension : 'odt';Kindofdocument: 'OpenDocument text document'; MimeType : 'application/vnd.oasis.opendocument.text'),
   (Extension : 'oga';Kindofdocument: 'OGG audio'; MimeType : 'audio/ogg'),
   (Extension : 'ogv';Kindofdocument: 'OGG video'; MimeType : 'video/ogg'),
   (Extension : 'ogx';Kindofdocument: 'OGG'; MimeType : 'application/ogg'),
   (Extension : 'otf';Kindofdocument: 'OpenType font'; MimeType : 'font/otf'),
   (Extension : 'png';Kindofdocument: 'Portable Network Graphics'; MimeType : 'image/png'),
   (Extension : 'pdf';Kindofdocument: 'Adobe Portable Document Format (PDF)'; MimeType : 'application/pdf'),
   (Extension : 'ppt';Kindofdocument: 'Microsoft PowerPoint'; MimeType : 'application/vnd.ms-powerpoint'),
   (Extension : 'rar';Kindofdocument: 'RAR archive'; MimeType : 'application/x-rar-compressed'),
   (Extension : 'rtf';Kindofdocument: 'Rich Text Format (RTF)'; MimeType : 'application/rtf'),
   (Extension : 'sh';Kindofdocument: 'Bourne shell script'; MimeType : 'application/x-sh'),
   (Extension : 'svg';Kindofdocument: 'Scalable Vector Graphics (SVG)'; MimeType : 'image/svg+xml'),
   (Extension : 'swf';Kindofdocument: 'Small web format (SWF) or Adobe Flash document'; MimeType : 'application/x-shockwave-flash'),
   (Extension : 'tar';Kindofdocument: 'Tape Archive (TAR)'; MimeType : 'application/x-tar'),
   (Extension : 'tif';Kindofdocument: 'Tagged Image File Format (TIFF)'; MimeType : 'image/tiff'),
   (Extension : 'tiff';Kindofdocument: 'Tagged Image File Format (TIFF)'; MimeType : 'image/tiff'),
   (Extension : 'ts';Kindofdocument: 'Typescript file'; MimeType : 'application/typescript'),
   (Extension : 'ttf';Kindofdocument: 'TrueType Font'; MimeType : 'font/ttf'),
   (Extension : 'vsd';Kindofdocument: 'Microsoft Visio'; MimeType : 'application/vnd.visio'),
   (Extension : 'wav';Kindofdocument: 'Waveform Audio Format'; MimeType : 'audio/x-wav'),
   (Extension : 'weba';Kindofdocument: 'WEBM audio'; MimeType : 'audio/webm'),
   (Extension : 'webm';Kindofdocument: 'WEBM video'; MimeType : 'video/webm'),
   (Extension : 'webp';Kindofdocument: 'WEBP image'; MimeType : 'image/webp'),
   (Extension : 'woff';Kindofdocument: 'Web Open Font Format (WOFF)'; MimeType : 'font/woff'),
   (Extension : 'woff2';Kindofdocument: 'Web Open Font Format (WOFF)'; MimeType : 'font/woff2'),
   (Extension : 'xhtml';Kindofdocument: 'XHTML'; MimeType : 'application/xhtml+xml'),
   (Extension : 'xls';Kindofdocument: 'Microsoft Excel'; MimeType : 'application/vnd.ms-excel'),
   (Extension : 'xlsx';Kindofdocument: 'Microsoft Excel'; MimeType : 'application/vnd.ms-excel'),
   (Extension : 'xml';Kindofdocument: 'XML'; MimeType : 'application/xml'),
   (Extension : 'xul';Kindofdocument: 'XUL'; MimeType : 'application/vnd.mozilla.xul+xml'),
   (Extension : 'zip';Kindofdocument: 'ZIP archive'; MimeType : 'application/zip'),
   (Extension : '3gp';Kindofdocument: '3GPP audio/video container'; MimeType : 'video/3gpp'),
   (Extension : '3g2';Kindofdocument: '3GPP2 audio/video container'; MimeType : 'video/3gpp2'),
   (Extension : '7z';Kindofdocument: '7-zip archive'; MimeType : 'application/x-7z-compressed')
  );

Type
  TNovusWebUtils = class(TNovusUtilities)
  protected
  private
  public
    {$IFDEF MSWINDOWS}
    /// <summary>
    ///   Return MIME from Windows API using FindMimeFromData
    /// </summary>
    /// <remarks>
    ///   Only for the Windows Platform
    /// </remarks>
    class function GetMimeFromData(aURL: String): string;
    {$ENDIF}
    /// <summary>
    ///   Return MIME Type from URL
    /// </summary>
    /// <remarks>
    ///   https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Complete_list_of_MIME_types
    /// </remarks>
    class function GetMIMEType(aURL: String): string;
    class function OpenDefaultWebBrowser(const aURL: string): Integer;
    class function UrlEncode(const aDecodedStr: String; aPluses: Boolean  = false): String;
    class function UrlDecode(const aEncodedStr: String): String;
    class procedure WebBrowserLoadFromHTML(aWebBrowser: TWebBrowser; aHTML: tStringlist);
     /// <summary>
    ///   Return Filename from URL path
    /// </summary>
    class function GetURLFilename(const aURLPath:String;Const Delimiter:String='/'):String;


  end;


implementation

{$IFDEF MSWINDOWS}
class function TNovusWebUtils.GetMimeFromData(aURL: String): string;
var
  fBuffer : LPWSTR;
begin
  fBuffer := nil;
  FindMimeFromData(nil, PWideChar(aURL), nil, 0, nil, 0, fBuffer, 0);
  if fBuffer <> nil then
  begin
    Result := fBuffer;
    CoTaskMemFree(fBuffer);
  end
  else
    Result := fBuffer;
end;
{$ENDIF}


class function TNovusWebUtils.GetMIMEType(aURL: String): string;
Var
  fsExtension: string;
  I: Integer;
begin
  Result := '';
  if Trim(aURL) = '' then Exit;
  {$IFDEF MSWINDOWS}
  Result := TNovusWebUtils.GetMimeFromData(aURL);
  if Result <> ''  then Exit;
  {$ENDIF}

  fsExtension := ExtractFileExt(aURL);
  if fsExtension <> '' then
    begin
      for i := Low(MimeTypes) to High(MimeTypes) do
        begin
          if SameText('.' + MimeTypes[i].Extension, fsExtension) then
          begin
            Result := MimeTypes[i].MIMEType;
            exit;
          end;
        end;
    end;
end;


class function TNovusWebUtils.OpenDefaultWebBrowser(const aURL: string): Integer;
var
  loNovusShell: tNovusShell;
begin
  Try
    loNovusShell:= tNovusShell.Create;

    result := loNovusShell.RunCommand(aURL, '', '');
  Finally
    loNovusShell.Free;
  End;
end;


class function TNovusWebUtils.UrlEncode(const aDecodedStr: String; aPluses: Boolean): String;
begin
  Result := System.Net.URLClient.TURI.UrlEncode(aDecodedStr, aPluses);
end;

class function TNovusWebUtils.UrlDecode(const aEncodedStr: String): String;
begin
  Result := System.Net.URLClient.TURI.URLDecode(aEncodedStr);
end;


class procedure TNovusWebUtils.WebBrowserLoadFromHTML(AWebBrowser: TWebBrowser; AHTML: tStringlist);
var
   sl: TStringList;
   ms: TMemoryStream;
begin
   AWebBrowser.Navigate('about:blank') ;
   while AWebBrowser.ReadyState < READYSTATE_INTERACTIVE do
    Application.ProcessMessages;

   if Assigned(AWebBrowser.Document) then
   begin
     sl := TStringList.Create;
     try
       ms := TMemoryStream.Create;
       try
         sl.Text := AHTML.Text;
         sl.SaveToStream(ms) ;
         ms.Seek(0, 0) ;
         (AWebBrowser.Document as IPersistStreamInit).Load(TStreamAdapter.Create(ms)) ;
       finally
         ms.Free;
       end;
     finally
       sl.Free;
     end;
   end;
end;

class function TNovusWebUtils.GetURLFilename(const aURLPath:String;Const Delimiter:String='/'):String;
var
  I: Integer;
begin
  I := LastDelimiter(Delimiter, aURLPath);
  Result := Copy(aURLPath, I + 1, MaxInt);
  Result := TNovusWebUtils.UrlDecode(Result);
end;



end.
