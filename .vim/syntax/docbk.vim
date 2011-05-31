" Vim syntax file
" Language:	DockBook
" Maintainer:	Xin Wang <dram.wang@gmail.com>

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
    finish
endif

syntax clear

syn sync minlines=100

syn keyword docbkStructure preface chapter section
syn keyword docbkStructure appendix dedication
syn keyword docbkStructure sect1 sect2 sect3 sect4 sect5

syn match docbkIgnore
	\ "</\?\(para\|code\|literal\|command\|title\|filename\)>"

syn match docbkIgnore
	\ "</\?\(term\|listitem\)>"

syn match docbkIgnore "</link>"

syn region docbkLink matchgroup=docbkIgnore
	\ start="<link xlink:href=\""
	\ end="\"/\?>"

"syn match docbkIgnore "\"/>"

syn match docbkTag "</\?[0-9a-zA-Z]\+/\?>"
	\ display
	\ contains=docbkIgnore,docbkStructure

syn region   docbkRegion
	\ start="<\z(preface\|chapter\|section\|sect1\|sect2\|sect3\|sect4\|sect5\|appendix\|dedication\|info\|index\)[\ >]"
	\ end=+</\z1>+
	\ fold
	\ contains=docbkTag,docbkRegion,docbkIgnore,docbkLink
	\ keepend
	\ extend

hi def link docbkTag Define
hi def link docbkStructure Keyword
hi def link docbkIgnore Comment

let b:current_syntax = "docbk"
