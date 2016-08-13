" Vim syntax file
" Language:	Graciela
" Maintainer: Carlos Spaggiari (carlos.25896@gmail.com)
" Last Change:	2016 Jul. 14
" Version: 0.1
"


" Keywords
syn keyword	gFunction	begin end proc func program
syn keyword	gFunction	abstract type implements if 
syn keyword	gFunction	do od fi var const array of
syn keyword	gFunction	int bool char float
syn keyword	gFunction	forall exist min max sigma pi
syn keyword	gFunction	set multiset rel seq tuple
syn keyword	gFunction	write writeln read random

syn keyword	gAssert 	inv pre post repinv coupinv bound 

syn match gAssert       "{a"
syn match gAssert       "a}" 
syn match String        "\".*\""
syn match Comment       "//.*$"
syn match Comment       "/\*.*\*/"

" Operators
syn match Operator	"\(<<\|>>\|[-+*/\^|:<>!=]\)="
syn match Operator	"<<\|>>\|++\|--\|->"
syn match Operator	"[.!~*<>^\\:|=,+-]"
syn match Operator	"/[^/*=]"me=e-1
syn match Operator	"/$"
syn match Operator	"[][]"



syn keyword gArgType in out ref inout

" Preprocs
syn keyword cDefined defined contained containedin=cDefine
hi def link cDefined cDefine

" Functions
syn match cUserFunction "\<\h\w*\>\(\s\|\n\)*("me=e-1 contains=cType,gDelimiter,cDefine
syn match cUserFunctionPointer "(\s*\*\s*\h\w*\s*)\(\s\|\n\)*(" contains=gDelimiter,Operator

hi def link cUserFunction gFunction
hi def link cUserFunctionPointer gFunction

" Delimiters
syn match gDelimiter    "[();]"
" foldmethod=syntax fix, courtesy of Ivan Freitas
syn match gBraces display "[{%}]"


" Booleans
syn keyword gBoolean true false 


" Links
hi def link gFunction 	Function
hi def link gIdentifier Function
hi def link gDelimiter 	Delimiter
hi def link gArgType 	Delimiter
hi def link gAssert 	Delimiter
" foldmethod=syntax fix, courtesy of Ivan Freitas
hi def link gBraces Delimiter
hi def link gBoolean Boolean

