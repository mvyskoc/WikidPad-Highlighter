WikidPad-Highlighter
==================== 

Syntax highlighting plugin for the wikidPad application 
  http://wikidpad.sourceforge.net/

Plugin enable to show you source code from the file in the wikidPad document.
Currently only mssql, python, plain text is supported.

Usage:

In the WikiDpad write:
   [:highlighting: "my_file.sql"; mssql; cp1250; showlines]

or you can directly insert your source code:
   [:highlighting:///
        Your source code    
    ///;python; showlines]

where:
   my_file   - If relative path is given,  the base directory is 
               directory of currently opened wiki.
   mssql     - Type of source code and currently it can also be:
                 python, text
   cp1250    - Encoding of the source file. If it is not given utf8 is assumed.
               For Windows binary of the WikiDpad can be necessary copy
               appropriate source python files into the librabry.zip\encodings
   showlines - If this word is stated, line numbers will be shown.

Installation:
  Put the source file into the directory: WikidPad/user_extensions
