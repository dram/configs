" Vim Inital file
set nocompatible
filetype plugin on
filetype indent on
set backspace=indent,eol,start
set fileencodings=utf-8,gbk,latin1
syntax on
set nobackup
set autoindent
set bg=dark
color ir_black
set vb t_vb=
set ic smartcase
set winminheight=0
set laststatus=2
set hidden
set confirm
set nonumber
set ruler
set wildignore=*.o

runtime ftplugin/man.vim

let g:c_no_comment_fold=1
let g:is_posix=1
let MRU_Max_Entries=30

autocmd BufEnter,FileType scheme :syntax sync minlines=50
autocmd BufNewFile,BufRead *.{md,mkd} set filetype=markdown
au BufNewFile,BufRead *.fr :setf forth

" :h last-position-jump
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

let mapleader = ','
nmap <silent> <leader>m		:MRU<CR>
nmap <silent> <leader>t		:GtagsCursor<CR>
nmap <silent> <leader>n		:cn<CR>
nmap <silent> <leader>p		:cp<CR>

" vim: sw=4 ts=4 fdm=marker
