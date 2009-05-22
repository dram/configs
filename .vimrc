" Vim Inital file
set nocompatible
filetype plugin on
filetype indent on
set backspace=indent,eol,start
syntax on
set nobackup
set autoindent
set bg=dark
set term=xterm-256color
color xoria256
set vb t_vb=
set ic smartcase
set winminheight=0
set laststatus=2
set hidden
set confirm
set nonumber
set wildignore=*.o
set ttymouse=xterm2

let g:c_no_comment_fold=1
let g:is_posix=1

set csprg=gtags-cscope
cs add GTAGS

let mapleader = ','
nmap <silent> <leader>m		:MRU<CR>
nmap <silent> <leader>t		:GtagsCursor<CR>
nmap <silent> <leader>n		:cn<CR>
nmap <silent> <leader>p		:cp<CR>

" vim: sw=4 ts=4 fdm=marker
