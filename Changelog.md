9/07.20201

* Fixed function TNovusDateUtils.JSONDateToDatetime for Z char in seconds it get striped


27/06/2021

* New function TNovusIndyUtils.URLDownloadToFile(aURL: String;
  aDownloadPath: String): TDownloadResponse;

25/06/2021

* Updated all version of Packages 
* Added NovusCodeLibrary_Env Package  to  NovusCodeLibrary_Indy Package.

19/05/2021

* Removed validation code from TNovusFileUtils.IsValidFolder 
* New property in TNovusTemplate class Lastmessage 
* Changed function TNovusTemplate.ParseTemplate: boolean; from procedure

28/03/2021

* Rename TNovusGuidEx to TNovusGuid and unit name.
* New function class function TNovusFileUtils.ExtractName(aFullFileName: String): String
* Rename class from TGuidExUtils to TNovusGuidEx
* New function class function TNovusNumUtils.ReverseBytesOrder(aBytes: TBytes): tBytes
* New function class function TNovusNumUtils.HexStrToBytes(aHexStr: String): tBytes
* New function class function TNovusNumUtils.CreateWord(const aHiByte, aLoByte: byte): word

09/03/2021

* New option IgnoreCase in function TNovusUtilities.RegExMatchEx default on
* New function class function TNovusNumUtils.StrToUInt64(aStr: String): UInt64

26/01/2021

* New function class function TNovusNumUtils.BinToUInt64(BinStr: string): UInt64
* New function class function HexToUint8(HexStr: String): UInt8
* New function class function HexToUInt64(HexStr: String): UInt64

26/01/2021

* Renamed TNovusStringUtils.IsAlphaStr to TNovusStringUtils.IsAlpha
* Renamed TNovusStringUtils.IsAlpha to TNovusStringUtils.IsAlphaChar
* Renamed TNovusStringUtils.IsNumber to TNovusStringUtils.IsNumericChar
* Updated function TNovusStringUtils.StrToUInt64(const s: String): UInt64; internally to StrToUint 
* New function TNovusUtilities.RegExMatchEx(aInput: string; aPattern: string;
      aInversed: Boolean; aMatchValue: Boolean): String;
* New function TNovusUtilities.RegExMatch(aInput: string; aPattern: string; aInversed: boolean= false): String;
* New procedure TNovusStringUtils.LeftTrim(aStr: String): String;

08/01/2021

* New procedure tNovusPlugin.ClearPluginList;
* New function tNovusPlugin.GetPluginlist(aIndex: Integer): PPluginInfo;

* tNovusPlugin Class internal fPlugins: Tlist refactored to fPluginList

05/01/2021

* Updated to Delphi 10.4 
* Updated DelphiVersions.inc with Delphi 10.4 support
* [Rename Infrastructure folder to BusinessObject and move to it own package from](https://github.com/novuslogic/NovuscodeLibrary/issues/2)
* Fixed memory bug in tnovuslist.clear
* Removed  function IsOptionsExists: Boolean 

06/06/2020

* New NovusVariants class procedure TNovusVariants.DisposeVarRec(aVarRec: TVarRec);
* An upgrade to class function TNovusVariants.VarToVarRec(aValue: variant): TVarRec;


02/04/2020

* frmVCLNovusForm added to package NovusCodeLibaryVCLUtils
* New NovuscodeLibraryProjectGroup.groupproj for Delphi 10.3

18/03/2020

* NovusInfrastructure Package has been renamed to NovusObject Package

16/03/2020

* Split Utilities units from package NovusCodeLibrary_Core into NovusCodeLibrary_Utils
* New unit NovusVariants
* New function class function TNovusVariants.VarToVarRec(aValue: variant): TVarRec;
* Upgraded BuildPackages

25/02/2020

* Rebuild of NovusCommandLine class
  
22/02/2020

* Removed NovusBasicStrParser from NovusCodeLibrary_Parser Package
* Removed NovusBasicStrParser unit

22/01/2020

* Fixed blank line issue ErrorMessage in TNovusCommandLineResult

15/01/2020

* rename RunCaptureCommand to RunCommandCapture to keep name consistent with in shell class.
  

14/01/2020

* Updated BuildpPackages to latest CodeImatic.codegen
* Renamed frmNovusForm to frmVCLNovusForm allowing for Firemonkey forms to be used.

14/10/2019

* Fixed InsensitiveKey in function TNovusList.Add(aKey: string; AItem: TObject): Integer; where key was not Insensitive


20/08/2019

* TNovuslist - New property InsensitiveKey allowing for Insensitive Keys in Dictionary
* Updated BuildPackages with lastest CodeImatic.build and CodeImatc.codegen
* Fix all packages Lib Suffix cause by issue in CodeImatc.codegen VarCmdLine.LIBSUFFIX variable being lowercase and being blank


14/072019

* Enlarged capture buffer in TNovusShell.WindowsCaptureExecute


03/07/2019

* TNovusLogFile - Replaced Text pointer with tStreamWriter
* TNovusLogFile - Removed FilePonter property
* TNovusLogFile - Removed IsFileOpen property
* TNovusLogFile - Removed IsFileOpen property 
* TNovusLogFile - Removed function ReadLine: String;
* TnovusLogFile - Removed procedure ReadAll;


27/04/2019

* Updated BuildPackages to current pre release CodeImatic 
* Now support Delphi 10.3 and packages
* New package NovusCodeLibrary_WebUtils.dpk - Web functions library
* New package NovusCodeLibrary_cURL.dpk - cURL function library. Requires https://github.com/Mercury13/curl4delphi


13/02/2019

* Rename HTTPMessage to HTTPResponse in class tNovuscURLUtils
* Add New property HTTPResponseCode in class tNovuscURLUtils


12/02/2019 

* Renamed property ErrorMessage to HTTPMessage in class tNovuscURLUtils


10/02/2019

* New class tNovuscURLUtils based on curl4delphi by Mikhail Merkuryev 
* New function TNovusWebUtils.GetURLFilename
* Replaced TNovusWebUtils.UrlEncode and TNovusWebUtils.UrlDecode with System.Net.URLClient.TURI equivalent functions.
* Support for Delphi 10.3  / C++Builder Rio


22/10/2018

* Add NovusJOSNUtils unit to NovusLibrary_core Package Delphi10_2


04/09/2018

Fixed class function tNovusJSONUtils.InJSONArrayToInt64(const aElement: string;
  const aJSONArray: TJSONArray): Int64; NULL Crashing 


09/08/2018

* New function  tNovusCommandLineCommand.FindRegisterObject(aClassName: String): TObject;
* New procedure tNovusCommandLineCommand.RegisterObject(aObject: TObject);

06/08/2018 

* Fixed class procedure SetupSDDatabase(Const ADatabase: tsddatabase;
      AServerName: String; ARemoteDatabase: String; AAliasName: String;
      AServerType: TSDServerType; AUserName, APassword: string;
      AParams: tStringList; ASQLLibrary: string; ALoginPrompt: Boolean = False;
      APort: Integer = 0); pass Port to Firebird and Interbase TSDServerType

05/08/2018

* Updated CreatePackages to latest of CodeImatic.build and CodeImatic.codegen
* New Package NovusCodeLibrary_Console Console and CLI Library.


14/06/2018

* TNovusCommandLine support updated TNovuslist
* TNovuslist fixed memory leak.
* TNovuslist now supports class pass through in constructor


2/06/2018

* Move NovusConsoleUtils class to NovusConsole Class

16/05/2018

* New function class function TNovusSimpleXML.HasProperties(aNode: TJvSimpleXmlElem; aName: string): string;
* Adjusted to handle root class function FindNode(aNode: TJvSimpleXmlElem; NodeName: String; Var Index: Integer): TJvSimpleXmlElem;


16/04/2018

* Class TNovusVersionUtils updated to Filename pass when valid version 


10/04/2018

* Updated TNovusOTAUtils.CreateMenuItem supports TAction


7/04/2018

* New classes for OpenTools class TNovusOTAManager and TNovusOTAUtils
* New class for VCL Controls TNovusVCLControlsUtils 


22/02/2018

* Fix nil pointer issue within NovusVersionUtil due to Win64 compiler
* New class TNovusFileUtils.IsOnlyFolder(aFolder: string): boolean
* New class procedure TNovusStringUtils.ClearStringlist(aStringlist: tStringlist)

21/1/2018

* Fixes class function IsValidFolder(aFolder: String): Boolean; in class NovusFileUtils including extractfilename 
* Fixed InsertAllTagValues if no tags pass TemplateDoc to OutputDoc in NovusTemplate


27/10/2017
 
 * Updated tNovusEnvironment.ParseGetEnvironmentVar

02/10/2017

* New function TNovusFileUtils.FilePathToURL(const aFilePath: string): string; 

01/10/2017

* TNovusFileUtils.IsTextFile now using TEncoding.GetBufferEncoding to return correct Encoding

29/09/2017

* New function TNovusWindows.IsStringUniCode(aString: String): boolean;
* Fix TNovusFileUtils.IsTextFile Incorrect check for encoding of Unicode

26/09/2017

* Updated class function ExtractFileExtA(aFileExt: String): String;

21/09/2017

* New function TNovusFileUtils.ExtractFileExtA(aFileExt: String): String;

9/08/2017

* TNovusparser class was crashing if input file was blank.

31/08/2017

* New function IsAvailableKeyEx(aHandle: THandle): TKeyEvent;

23/08/2017

* New function TNovusWebUtils.GetMimeFromData
* New function TNovusWebUtils.GetMIMEType
* New function TNovusWebUtils.OpenDefaultWebBrowser
* New function TNovusWebUtils.WebBrowserLoadFromHTML
* Moved TNovusIndyUtils.UrlEncode and TNovusIndyUtils.UrlDecode to New TNovusWebUtils Class
* New TNovusWebUtils Class

15/05/2017

*  New procedure TNovusList.InitClass moved initial of class for list


30/04/2017

* New unit NovusStreamUtils.pas
* [TNovusVersionUtils.GetFileInfoString is not passing an pascal string but an ansistring](https://github.com/novuslogic/NovuscodeLibrary/issues/3)
* New property IgnoreBlankValue in class TNovusTemplate - If TagValue is blank the TagName remains in OutputDoc

19/04/2017
* [Reformatted all units with Delphi internal source code formater](https://github.com/novuslogic/NovuscodeLibrary/issues/1)

18/04/2017

* Public release.
* Fixed NovusTemplate class second token order issue and missing tags
* Change this file to Markdown

28/11/2016

* New unit NovusEnvironment
* New unit NovusDialogs

* New Delphi XE8 packages NovusCodeLibrary_VCLUtils
* New Delphi XE8 packages NovusCodeLibrary_Env

27/10/2015

* Fixed pathing issue in Delphi XE8 Packages

26/10/2016

* Fix Plugin Sample with Sharemem


19/09/2016

* New packages for Delphi XE8 NovusCodeLibrary_Plugin, NovusCodeLibrary_Parser


18/09/2016

* Added NovusCodeLibrary.inc and DelphiVersion.Inc
* Added support for $LIBSUFFIX in packages
* Added support Delphi XE to Delphi 10.1
* Changed naming conventions to use package version
* Added support for $(Platform)\$(Config) in packages


11/09/2016

* Start of the Changelog
* Changed Licence to Apache License Version 2.0
* New class NovusJSONUTils
* Removed StrToInt from class NovusStringUtils - to keep compatible with Delphi RTL
* Add Str2Int64 to class NovusStringUtils





