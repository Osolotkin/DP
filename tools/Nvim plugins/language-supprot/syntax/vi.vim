" Vim syntax file

" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim



syn match       viCommentLn     "//.*"
syn region      viCommentEx     start="\/{" end="\/}"
syn match       viString        /"[^"]*"/
syn match       viCharacter     /'[^']*'/
syn match       viNumber        /\v\d+/
syn keyword     viBoolean       true false 
syn match       viFloat         /\v\d+\.\d+/

syn match       viIdentifier    /\v[a-zA-Z_][a-zA-Z0-9_]*/
syn match       viFunctionCall  /\v[a-zA-Z_][a-zA-Z0-9_]*\s*\(/

syn keyword     viStatement     break continue return
syn keyword     viConditional   if else switch
syn keyword     viRepeat        while for loop
syn keyword     viLabel         case
"syn match       viOperator      /[+\-*\/=<>!&|^%]/
syn keyword     viKeyword       const embed       
"syn keyword     viException     

syn match       viPreProc       /#\w\+/
syn match       viInclude       /#import/ 

syn keyword     viType          int i32 i64 f32 f64
"syn keyword     viStorageClass  const embed
syn keyword     viStructure     def enum fcn



hi def link     viCommentLn     Comment
hi def link     viCommentEx     Comment
hi def link     viString        String
hi def link     viCharacter     Character
hi def link     viNumber        Number
hi def link     viBoolean       Boolean
hi def link     viFloat         Float

hi def link     viIdentifier    Identifier
hi def link     viFunctionCall  Function

hi def link     viStatement     Statement
hi def link     viConditional   Conditional
hi def link     viRepeat        Repeat
hi def link     viLabel         Label
"hi def link     viOperator      Operator 
hi def link     viKeyword       Keyword

hi def link     viPreProc       PreProc
hi def link     viInclude       Include

hi def link     viType          Type
"hi def link     viStorageClass  StorageClass
hi def link     viStructure     Structure
