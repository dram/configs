" Vim syntax file
" Language:	DockBook
" Maintainer:	Xin Wang <dram.wang@gmail.com>

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
    finish
endif

syntax clear

syn sync minlines=100

syn match docbkTag "</\?[0-9a-zA-Z]\+/\?>" display

syn region   docbkRegion
	\ start="<\z(preface\|chapter\|section\|appendix\|dedication\|info\|index\)[\ >]"
	\ end=+</\z1>+
	\ fold
	\ contains=docbkTag,docbkRegion
	\ keepend
	\ extend

hi def link docbkTag Identifier

let b:current_syntax = "docbk"
