 unit NovusBasicStrParser;

 interface

 Uses NovusUtilities, NovusStringUtils, NovusParser, Classes,
     SysUtils, StrUtils;


 type
   tNovusBasicStrParser = Class(tNovusParser)
     private
       FItems: TStringList;
       stText         : String;    // The string passed to the constructor
       stWordCount    : Integer;   // Internal count of words in the string
       stFindString   : String;    // The substring used by FindFirst, FindNext
       stFindPosition : Integer;   // FindFirst/FindNext current position

       procedure GetWordCount;                  // Calculates the word count
       procedure SetText(const Value: String);  // Changes the text string

     // These methods and properties are all usable by instances of the class
     published
       // Called when creating an instance (object) from this class
       // The passed string is the one that is operated on by the methods below
       constructor Create(AText : String);
       destructor Destroy; override;


       // Utility to replace all occurences of a substring in the string
       // The number of replacements is returned
       // This utility is CASE SENSITIVE
       function Replace(fromStr, toStr : String) : Integer;

       // Utility to find the first occurence of a substring in the string
       // The returned value is the found string position (strings start at 1)
       // if not found, -1 is returned
       // This utility is CASE SENSITIVE
       function FindFirst(search : String) : Integer;

       // Utility to find the next occurence of the FindFirst substring
       // if not found, -1 is returned
       // if no FindFirst performed before this call, -2 is returned
       // This utility is CASE SENSITIVE
       function FindNext : Integer;

       // The string itself - allow it to be read and overwritten
       property Text : String
           read stText
          write SetText;    // We call a method to do this

       // The number of words in the document. Words are groups of characters
       // separated by blanks, tabs, carriage returns and line feeds
       property WordCount : Integer
           read stWordCount;

       property Items: TStringList
         read FItems
         write FItems;
   end;

 implementation


 // Constructor : Create an instance of the class. Takes a string as argument.
 // -----------------------------------------------------------------------------
 // The passed string is stored, and the number of words it contains is counted
 // The FindFirst function string and position are reset
 constructor tNovusBasicStrParser.Create(AText: String);
 begin
   FItems := TStringList.Create;

   stText         := AText;         // Save the passed string
   stFindPosition := 1;            // Start a search at the string start
   stFindString   := '';           // No find string provided yet
   GetWordCount;                   // Call a subroutine to get the word count
 end;


 destructor tNovusBasicStrParser.destroy;
begin
  inherited destroy;
end;


 // SetText : Routine to change the text string
 // -----------------------------------------------------------------------------
 // It is important that we call a routine to change the text because we must
 // recalculate the word length, and reposition the find function at the start
 procedure tNovusBasicStrParser.SetText(const Value: String);
 begin
   stText         := Value;        // Save the passed string
   stFindPosition := 1;            // Reposition the find mechanism to the start
   GetWordCount;                   // Recalculate the word count
 end;
 

 // FindFirst : Finds the first position of a substring in the string
 // -----------------------------------------------------------------------------
 // The passed substring is saved, and the string scanned for an occurence
 // The found string index is returned. If not found, -1 is returned
 function tNovusBasicStrParser.FindFirst(search: String): Integer;
 begin
   // Here we sort of cheat - we save the search string and just call
   // FindNext after setting the initial string start conditions
   stFindString   := search;
   stFindPosition := 1;
 
   Result := FindNext;
 end;
 
 
 // FindNext : Finds the next occurence of the substring in the string
 // -----------------------------------------------------------------------------
 // If FindFirst had not been called, -2 is returned
 // If FindFirst had been called, string scanning is resumed where the left off
 // If found, the index position is returned. Of not found, -1 is returned.
 function tNovusBasicStrParser.FindNext: Integer;
 var
   index    : Integer;
   findSize : Integer;
 begin
   /// Only scan if we have a valid scan string
   if Length(stFindString) = 0
   then Result := -2
   else
   begin
     // Set the search string size
     findSize := Length(stFindString);
 
     // Set the result to the 'not found' value
     Result := -1;
 
     // Start the search from where we last left off
     index  := stFindPosition;
 
     // Scan the string :
     // We check for a match with the first character of the fromStr as we step
     // along the string. Only when the first character matches do we compare
     // the whole string. This is more efficient.
     // We abort the loop if the string is found.
     while (index <= Length(stText)) and (Result < 0) do
     begin
       // Check the first character of the search string
       if stText[index] = stFindString[1] then
       begin
         // Now check the whole string - setting up a loop exit condition if
         // the string matches
         if AnsiMidStr(stText, index, findSize) = stFindString
         then Result := index;
       end;
 
       // Move along the string
       Inc(index);
     end;
 
     // Position the next search from where the above leaves off
     // Notice that index gets incremented even with a successful match
     stFindPosition := index
   end;
 
   // This subroutine will now exit with the established Result value
 end;
 
 
 // Replace : Replaces all occurences of a substring in the string
 // -----------------------------------------------------------------------------
 // The string is scanned for occurences of the string, replacing where found
 // The number of replacements is returned, or 0 if none performed.
 function tNovusBasicStrParser.Replace(fromStr, toStr: String): Integer;
 var
   fromSize, count, index  : Integer;
   newText : String;
   matched : Boolean;
 begin
   // Get the size of the from string
   fromSize := Length(fromStr);
 
   // Start with 0 replacements
   count := 0;
 
   // We will build the target string in the newText variable
   newText := '';
   index := 1;
 
   // Scan the string :
   // We check for a match with the first character of the fromStr as we step
   // along the string. Only when the first character matches do we compare
   // the whole string. This is more efficient.
   while index <= Length(stText) do
   begin
     // Indicate no match for this character
     matched := false;
 
     // Check the first character of the fromStr
     if stText[index] = fromStr[1] then
     begin
       if AnsiMidStr(stText, index, fromSize) = fromStr then
       begin
         // Increment the replace count
         Inc(count);

         // Store the toStr in the target string
         newText := newText + toStr;

         // Move the index past the from string we just matched
         Inc(index, fromSize);

         // Indicate that we have a match
         matched := true;
       end;
     end;

     // if no character match :
     if not matched then
     begin
       // Store the current character in the target string, and
       // then skip to the next source string character
       newText := newText + stText[index];
       Inc(index);
     end;
   end;

   // Copy the newly built string back to stText - as long as we made changes
   if count > 0 then stText := newText;
 
   // Return the number of replacements made
   Result := count;
 end;

 
 // GetWordCount : Subroutine used to calculate the string word count
 // -----------------------------------------------------------------------------
 // The string is scanned for character groups (separated by blanks, tabs etc)
 // The number found is stored in the csWordCount private global variable
 procedure tNovusBasicStrParser.GetWordCount;
 const
   // Define word separator types that we will recognise
   LF    = #10;
   TAB   = #9;
   CR    = #13;
   BLANK = #32;
   SEMICOL = ';';
   EQUALS = '=';
 var
   lsWord: String;
   WordSeparatorSet : Set of Char;  // We will set on only the above characters
   index  : Integer;     // Used to scan along the string
   inWord : Boolean;     // Indicates whether we are in the middle of a word
 begin
   // Turn on the TAB, CR, LF and BLANK characters in our word separator set
   WordSeparatorSet := [LF, TAB, CR, BLANK, SEMICOL];

   // Start with 0 words
   stWordCount := 0;

   // Scan the string character by character looking for word separators
   inWord := false;

   lsWord := '';

   for index := 1 to Length(stText) do
   begin
     // Have we found a separator character?
     if stText[index] In WordSeparatorSet then
     begin
       // Separator found - have we moved from a word?
       if inWord then
         begin
           FItems.Add(lsWord);

           Inc(stWordCount);    // Yes - we have ended another word

           lsWord := '';
         end;

       // Indicate that we are not in a word anymore
       inWord := false;
     end
     else
       // Separator not found - we are in a word
       inWord := true;

      if InWord then
        lsWord := lsWord + stText[index];
   end;

   // Finally, were we still in a word at the end of the string?
   // if so, we must add one to the word count since we did not meet a separator
   if inWord then
     begin
       FItems.Add(lsWord);

       Inc(stWordCount);
     end;
 end;

 end.
