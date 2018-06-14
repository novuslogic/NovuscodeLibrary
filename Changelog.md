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





