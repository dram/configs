" Vim Inital file
set nocompatible
filetype plugin on
filetype indent on
set backspace=indent,eol,start
set fileencodings=utf-8,gbk,latin1
set hlsearch
syntax on
set nobackup
set autoindent
set bg=dark
color desert
set vb t_vb=
set ic smartcase
set winminheight=0
set laststatus=2
set hidden
set confirm
set nonumber
set ruler
set wildignore=*.o
set shellpipe=2>&1\|\ tee
set colorcolumn=81
hi ColorColumn ctermbg=darkgrey

runtime ftplugin/man.vim

let g:c_no_comment_fold=1
let g:is_posix=1
let MRU_Max_Entries=30
let g:leave_my_textwidth_alone=1

set guioptions-=T
set guifont=Liberation\ Mono\ 10

au guienter *		color herald
au guienter *		set novisualbell
au guienter *		set guifont=Liberation\ Mono\ 10
au guienter *		set lines=30

autocmd BufEnter,FileType scheme :syntax sync minlines=50
autocmd BufNewFile,BufRead *.{md,mkd} set filetype=markdown
au BufNewFile,BufRead *.fr :setf forth
au BufWritePost .c,*.h silent! !ctags -R &

if filereadable('/tmp/uim-vi-mode')
	function TurnOnUIMIfNecessary()
		let l:text = readfile('/tmp/uim-vi-mode', 'b')

		if len(l:text) >= 1 && l:text[0] == ' T'
			call writefile(['tT'], '/tmp/uim-vi-mode')
		endif
	endfunction

	au! InsertEnter * call TurnOnUIMIfNecessary()
endif

map! <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>

" :h last-position-jump
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

let mapleader = ','
nmap <silent> <leader>m		:MRU<CR>
nmap <silent> <leader>t		:GtagsCursor<CR>
nmap <silent> <leader>n		:cn<CR>
nmap <silent> <leader>p		:cp<CR>
nmap <silent> <leader>x		:call GSTToggle()<cr>

nmap <silent> ( :call search('\n\\|。\\|！\\|？\\|\.\s\\|!\s\\|?\s', "bw")<cr>
nmap <silent> ) :call search('\n\\|。\\|！\\|？\\|\.\s\\|!\s\\|?\s', "w")<cr>

cmap %/ <C-R>=expand("%:p:h").'/'<CR>

" vim: sw=4 ts=4 fdm=marker
