let s:cur_dir = "/media/sf_music/"
let s:jump_list = []

function! GSTExec(lst)
	call writefile(a:lst, '/tmp/vim-gst.pipe')
endfunction

function! GSTBrowserKeymap()
	" Key mappings
	nnoremap <buffer> <silent> <tab>	:wincmd w<cr>
	nnoremap <buffer> <silent> <space>	:call GSTPlay()<cr>
	nnoremap <buffer> <silent> <cr>		:call GSTEnter()<cr>
	nnoremap <buffer> <silent> <bs>		:call GSTBack()<cr>
	nnoremap <buffer> <silent> =		:call GSTIncreaseVolume()<cr>
	nnoremap <buffer> <silent> -		:call GSTDecreaseVolume()<cr>
	nnoremap <buffer> <silent> a		:call GSTAdd()<cr>
	nnoremap <buffer> <silent> c		:call GSTClear()<cr>
	nnoremap <buffer> <silent> p		:call GSTPause()<cr>
	nnoremap <buffer> <silent> s		:call GSTStop()<cr>
	nnoremap <buffer> <silent> n		:call GSTNext()<cr>
	nnoremap <buffer> <silent> N		:call GSTPrevious()<cr>
endfunction

function! GSTGlob(path)
	let l:tmp = &wildignore
	silent! set wildignore=*.jpg,*.db,*.JPG,*.m3u,*.M3U,*.txt,*.TXT
	silent! set wildignore+=*.lrc,*.LRC
	let l:text = glob(a:path)
	exec 'silent! set wildignore=' . l:tmp
	return l:text
endfunction

function! GSTListDir(path)
	let l:all = split(GSTGlob(a:path), "\n")
	return l:all
endfunction

function! GSTOrderListDir(path)
	let l:gbk_text = iconv(GSTGlob(a:path), 'utf-8', 'gbk')
	let l:gbk_sorted = join(sort(split(l:gbk_text, "\n")), "\n")
	let l:utf8_text = iconv(l:gbk_sorted, 'gbk', 'utf-8')
	return split(l:utf8_text, "\n")
endfunction

function! GSTPlaylistKeymap()
	" Key mappings
	nnoremap <buffer> <silent> <tab>	:wincmd w<cr>
	nnoremap <buffer> <silent> n		:call GSTNext()<cr>
	nnoremap <buffer> <silent> N		:call GSTPrevious()<cr>
	nnoremap <buffer> <silent> <space>	:call GSTPlaylistPlay()<cr>
	nnoremap <buffer> <silent> <cr>		:call GSTPlaylistPlay()<cr>
endfunction

function! GSTBack()
	if expand('%') == '__GST_BROWSER__'
		lcd ..
		let s:cur_dir = getcwd() . '/'
		call GSTRefresh()
		if len(s:jump_list) != 0
			exec "normal " . s:jump_list[-1] . "G"
			call remove(s:jump_list, -1)
		endif
	endif
endfunction

function! GSTStop()
	call GSTExec(["STOP"])
	call GSTRefreshPlaylist()
endfunction

function! GSTClear()
	call GSTExec(["CLEAR"])
	sleep 500m
	call GSTRefreshPlaylist()
endfunction

function! GSTNext()
	call GSTExec(["NEXT"])
	sleep 500m
	call GSTRefreshPlaylist()
endfunction

function! GSTPrevious()
	call GSTExec(["PREVIOUS"])
	sleep 500m
	call GSTRefreshPlaylist()
endfunction

function! GSTPause()
	call GSTExec(["PAUSE"])
	sleep 500m
	call GSTRefreshPlaylist()
endfunction

function! GSTEnter()
	if expand('%') == '__GST_BROWSER__'
		if isdirectory(s:cur_dir . getline('.'))
			let s:cur_dir = s:cur_dir . getline('.') . '/'
			call add(s:jump_list, line('.'))
			call GSTRefresh()
		else
			sleep 500m
			call GSTExec(["CLEAR", 'ADD ' . s:cur_dir . getline('.')])
		endif
	endif
endfunction

function! GSTSeek(ln)
	call GSTExec(["SEEK " . a:ln])
	sleep 500m
	call GSTRefreshPlaylist()
endfunction

function! GSTPlaylistPlay()
	if expand('%') == '__GST_PLAYLIST__'
		call GSTSeek(line('.') - 1)
	endif
endfunction

function! GSTPlay()
	let l:path = s:cur_dir . getline('.')
	if isdirectory(l:path)
		let l:items = GSTListDir(getline('.') . '/*')
		call map(l:items, '"ADD " . s:cur_dir . v:val')
		call GSTExec(extend(['CLEAR'], l:items))
	else
		call GSTExec(['CLEAR', 'ADD ' . l:path])
	endif
	sleep 500m
	call GSTRefreshPlaylist()
endfunction

function! GSTAdd()
	let l:path = s:cur_dir . getline('.')
	if isdirectory(l:path)
		let l:items = GSTListDir(getline('.') . '/*')
		call map(l:items, '"ADD " . s:cur_dir . v:val')
		call GSTExec(l:items)
	else
		call GSTExec(['ADD ' . l:path])
	endif
	sleep 500m
	call GSTRefreshPlaylist()
endfunction


function! GSTCreateWindow()
        silent! new __GST_PLAYLIST__
	silent! only
        silent! setlocal buftype=nofile noswapfile nobuflisted nowrap
        silent! setlocal nofoldenable cursorline nospell nolist

        syntax match VIMMPCurrent "^-.*"
        hi link VIMMPCurrent Folded

	call GSTPlaylistKeymap()

        silent! vertical leftabove split __GST_BROWSER__
        silent! setlocal buftype=nofile noswapfile nobuflisted nowrap
        silent! setlocal nofoldenable cursorline nospell nolist
	silent! setlocal fileencoding=gbk

	call GSTBrowserKeymap()
endfunction

function! GSTRefreshPlaylist()
	let l:winnum = bufwinnr('__GST_PLAYLIST__')
	if l:winnum != -1
		let l:current = winnr()
                exec l:winnum . 'wincmd w'
		if expand('%') == '__GST_PLAYLIST__'
			0,$delete
			if filereadable('/tmp/vim-gst-playlist.txt')
				call append(0, readfile('/tmp/vim-gst-playlist.txt'))
			endif
			normal gg
		endif
		exec l:current . 'wincmd w'
	endif
endfunction

function! GSTRefresh()
	if expand('%') == '__GST_BROWSER__'
		exec 'lcd ' . escape(s:cur_dir, ' ')
		let l:items = GSTOrderListDir('*')

		0,$delete
		call append(0, l:items)
		normal gg
	endif
	call GSTRefreshPlaylist()
endfunction

function! GSTToggle()
	let l:browser_win = bufwinnr('__GST_BROWSER__')
	let l:playlist_win = bufwinnr('__GST_PLAYLIST__')

	exec 'silent! bdelete ' . bufnr('__GST_BROWSER__')
	exec 'silent! bdelete ' . bufnr('__GST_PLAYLIST__')

	if l:browser_win == '-1' || l:playlist_win == '-1'
		call GSTCreateWindow()
		call GSTRefresh()

		au WinEnter __GST_PLAYLIST__	:call GSTRefreshPlaylist()
		"au WinEnter __GST_BROWSER__	:call GSTRefreshPlaylist()
		au CursorHold __GST_PLAYLIST__	:call GSTRefreshPlaylist()
	endif
endfunction

