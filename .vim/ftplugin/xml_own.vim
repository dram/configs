
function! s:getcol()
	return col('.')
endfunction

function! s:setcol(col)
	let l:pos = getpos('.')
	let l:pos[2] = a:col
	call setpos('.', l:pos)
endfunction

function! s:make_element()
	let l:before = strpart(getline('.'), 0, col('.') - 1)
	let l:after = strpart(getline('.'), col('.') - 1)
	let l:tag_pos = match(l:before, '\w\+$')
	if l:tag_pos == -1
		return
	endif
	let l:tag_name = strpart(l:before, l:tag_pos)
	let l:tag = printf("<%s></%s>", l:tag_name, l:tag_name)
	call setline('.', strpart(l:before, 0, l:tag_pos) . l:tag . l:after)
	call s:setcol(col('.') + 2)
endfunction

inoremap <silent><buffer> ;; <C-\><C-O>:call <SID>make_element()<CR>
