" diffchar.vim - Highlight the differences, based on characters and words
"
" This script has been developed in order to make diff mode more useful.
" DiffText does not show the exact difference, but this script will
" highlight its difference, character by character - so called DiffChar.
"
" Use this script just after diff command. DiffText area will be narrowed
" down to show the DiffChar. You can use this script in non-diff'ed usual
" mode as well.
"
" For example, diff command shows: (<|DiffText area|>)
"
"      (file A) The <|swift brown fox jumped over the lazy|> dog.
"      (file B) The <|lazy fox jumped over the swift brown|> dog.
"
" then this script will narrow down the DiffText area:
"
"      (file A) The <|swift brown|> fox jumped over the <|lazy|> dog.
"      (file B) The <|lazy|> fox jumped over the <|swift brown|> dog.
"
" Default commands:
" :[range]SDChar (Highlight DiffChar for [range])
" :[range]RDChar (Reset DiffChar for [range])
"
" Default <Plug> mappings:
" <Plug>(DiffChar_ToggleAllLines)    (toggle DiffChar for all lines)
" <Plug>(DiffChar_ToggleCurrentLine) (toggle DiffChar for the current line)
" <Plug>(DiffChar_PrevStart)         (jump to the start position of the previous DiffChar unit)
" <Plug>(DiffChar_NextStart)         (jump to the start position of the next DiffChar unit)
" <Plug>(DiffChar_NextEnd)           (jump to the end position of the previous DiffChar unit)
" <Plug>(DiffChar_NextEnd)           (jump to the end position of the next DiffChar unit)
"
" This script has been always positively supporting mulltibyte characters.
"
" Update : 4.1
" * Implemented to echo a matching difference unit with its color when jumping
"   cursor by "[b"/"]b" or "[e"/"]e".
" * Fixed defects: not using the new uniq() function introduced in vim 7.4.
"
" Update : 4.0
" * Enhanced to easily find a corresponding pair of each difference unit.
"   - each unit pair will be shown in individual same color on both windows.
"     A g:DiffColors (and t:DiffColors) variable is a type of matching colors,
"     0 (default) for always 1 color as before, 1/2/3 for 4/8/16 colors in
"     fixed order, and 100 for all available colors in dynamic random order.
"   - when jumping cursor by "[b"/"]b" or "[e"/"]e" in either window,
"     the start or end position of a matching unit will be highlighted with
"     a cursor-type color in another window.
"
" Update : 3.6
" * Added two g:DiffUnit (and t:DiffUnit) types. "Word3" will split at the
"   \< or \> boundaries, which can separate based on the character class like
"   CJK, Hiragana, Katakana, Hangul, full width symbols and so on. And "CSV(c)"
"   will split the units by a specified character "c". For example, "CSV(,)"
"   and "CSV(\t)" can be used for comma and tab separated text.
"
" Update : 3.5
" * Fixed defects: DiffChar highlighting units do not override/hide hlsearch.
"
" Update : 3.4
" * Enhanced to support individual DiffChar handling on each tab page.
"   Difference unit and algorithm can also be set page by page using
"   tab page local variables, t:DiffUnit and t:DiffAlgorithm.
"
" Update : 3.3
" * Enhanced to jump cursor to the DiffChar highlighting units. Sample keymaps
"   "]b" and "[b" will move cursor forwards to the next and backwards to the
"   previous start positions. And "]e" and "[e" will move to the end positions.
"
" Update : 3.2
" * Enhanced to follow diff mode without any limitations. Compare between
"   the corresponding DiffChange lines on both windows and properly handle
"   DiffAdd and DiffDelete lines.
"
" Update : 3.1
" * Enhanced to show/reset/toggle DiffChar highlightings on individual line
"   by line.
" * Implemented the window layout handling.
"   - the DiffChar'ed windows will remain the highlightings even if the
"     window position is rotated/replaced/moved and another new window opens.
"   - if either DiffChar'ed window is closed, reset all the DiffChar
"     highlightings on another window.
" * Removed limitations:
"   - when more than 2 windows exist, current and next (wincmd w) windows
"     will be selected.
"   - if the specified numbers of lines are different in both windows, ignore
"     the redundant lines and continue to compare the text on the same lines.
"   - RDChar sample command has a range attribute (e.g. %RDChar).
" * Fixed defects:
"   - reset just DiffChar highlightings only and remain others.
"
" Update : 3.0
" * Implemented word by word differences. A g:DiffUnit variable is a type of
"   a difference unit. Its default is "Char", which will trace character
"   by character as before. "Word1" will split into \w\+ words and
"   any \W single characters. And "Word2" will separate the units at the
"   \s\+ space boundaries.
" * Improved the performance around 10%.
"
" Update : 2.1
" * Coding changes in the O(NP) function for readability.
"
" Update : 2.0
" * Implemented the O(NP) and O(ND) Difference algorithms to improve the
"   performance. This update uses the O(NP) by default, and can be changed
"   to the O(ND) if necessary, or to the basic algorithm implemented in
"   the initial version.
"
" Author: Rick Howe
" Last Change: 2014/06/18
" Created:
" Requires:
" Version: 4.1

if exists("g:loaded_diffchar")
	finish
endif
let g:loaded_diffchar = 4.1

let s:save_cpo = &cpo
set cpo&vim

" Default commands
command! -range SDChar call s:ShowDiffChar(<line1>, <line2>)
command! -range RDChar call s:ResetDiffChar(<line1>, <line2>)

" Default mappings
nnoremap <silent> <Plug>(DiffChar_ToggleAllLines)    :call <SID>ToggleDiffChar(1, line('$'))<CR>
nnoremap <silent> <Plug>(DiffChar_ToggleCurrentLine) :call <SID>ToggleDiffChar(line('.'))<CR>
nnoremap <silent> <Plug>(DiffChar_PrevStart)         :call <SID>JumpDiffChar(0, 1)<CR>
nnoremap <silent> <Plug>(DiffChar_NextStart)         :call <SID>JumpDiffChar(1, 1)<CR>
nnoremap <silent> <Plug>(DiffChar_NextEnd)           :call <SID>JumpDiffChar(0, 0)<CR>
nnoremap <silent> <Plug>(DiffChar_NextEnd)           :call <SID>JumpDiffChar(1, 0)<CR>

" Set a difference unit type
let g:DiffUnit = "Char"		" any single character
" let g:DiffUnit = "Word1"	" \w\+ word and any \W single character
" let g:DiffUnit = "Word2"	" non-space and space words
" let g:DiffUnit = "Word3"	" \< or \> character class boundaries
" let g:DiffUnit = "CSV(,)"	" character separated by (,;\t)

" Set a difference unit matching colors
let g:DiffColors = 0		" always 1 color
" let g:DiffColors = 1		" 4 colors in fixed order
" let g:DiffColors = 2		" 8 colors in fixed order
" let g:DiffColors = 3		" 16 colors in fixed order
" let g:DiffColors = 100	" all available colors in dynamic random order

" Set a difference algorithm
let g:DiffAlgorithm = "ONP"
" let g:DiffAlgorithm = "OND"
" let g:DiffAlgorithm = "Basic"

function! s:InitializeDiffChar()
	if min(tabpagebuflist()) == max(tabpagebuflist())
		echo "Need more buffers on this page!"
		return -1
	endif

	" define a DiffChar dictionary on this tab page
	let t:DChar = {}

	" select current and next (wincmd w) window's buffers
	let t:DChar.buf = {}
	let cwin = winnr()
	let t:DChar.buf[1] = winbufnr(cwin)
	let w = 0
	while 1		" find different buf than buf[1]
		let t:DChar.buf[2] = winbufnr((cwin + w) % winnr('$') + 1)
		if t:DChar.buf[1] != t:DChar.buf[2] | break | endif
		let w += 1
	endwhile

	" find corresponding DiffChange/DiffText lines on diff mode buffers
	let t:DChar.vdl = {}
	let dc = hlID("DiffChange")
	let dt = hlID("DiffText")
	for k in [1, 2]
		exec bufwinnr(t:DChar.buf[k]) . "wincmd w"
		if &diff
			let t:DChar.vdl[k] = []
			for l in range(1, line('$'))
				let id = diff_hlID(l, 1)
				if id == dc || id == dt
					let t:DChar.vdl[k] += [l]
				endif
			endfor
		else
			unlet t:DChar.vdl
			break
		endif
	endfor

	" set line and its highlight id record
	let t:DChar.mid = {}
	let t:DChar.mid[1] = {}
	let t:DChar.mid[2] = {}

	" set highlighted lines and columns record
	let t:DChar.hlc = {}
	let t:DChar.hlc[1] = {}
	let t:DChar.hlc[2] = {}

	" set a difference unit type on this tab page and set a split pattern
	if !exists("t:DiffUnit")
		let t:DiffUnit = g:DiffUnit
	endif
	if t:DiffUnit == "Char"		" any single character
		let t:DChar.spt = '\zs'
	elseif t:DiffUnit == "Word1"	" \w\+ word and any \W character
		let t:DChar.spt = '\(\w\+\|\W\)\zs'
	elseif t:DiffUnit == "Word2"	" non-space and space words
		let t:DChar.spt = '\(\s\+\|\S\+\)\zs'
	elseif t:DiffUnit == "Word3"	" \< or \> boundaries
		let t:DChar.spt = '\<\|\>'
	elseif t:DiffUnit =~ '^CSV(.\+)$'	" character separated
		let s = substitute(t:DiffUnit, '^CSV(\(.\+\))$', '\1', '')
		let t:DChar.spt = '\(\([^\'. s . ']\+\)\|\' . s . '\)\zs'
	else
		let t:DChar.spt = '\zs'
		echo 'Not a valid difference unit type. Use "Char" instead.'
	endif

	" set a difference algorithm on this tab page
	if !exists("t:DiffAlgorithm")
		let t:DiffAlgorithm = g:DiffAlgorithm
	endif

	" set a matching pair cursor id on this tab page
	let t:DChar.mpc = {}

	" set a difference matching colors on this tab page
	if !exists("t:DiffColors")
		let t:DiffColors = g:DiffColors
	endif

	" set types of difference matching colors from available highlights
	let t:DChar.dmc = ["DiffText"]
	if t:DiffColors == 1
		let t:DChar.dmc += ["DiffDelete", "WildMenu", "VisualNOS"]
	elseif t:DiffColors == 2
		let t:DChar.dmc += ["DiffDelete", "WildMenu", "TabLine",
			\"ErrorMsg", "DiffAdd", "VisualNOS", "Conceal"]
	elseif t:DiffColors == 3
		let t:DChar.dmc += ["ModeMsg", "DiffDelete", "Title",
			\"WildMenu", "CursorLineNr", "TabLine", "NonText",
			\"ErrorMsg", "StatusLine", "DiffAdd", "WarningMsg",
			\"VisualNOS", "MoreMsg", "Conceal", "SpecialKey"]
	elseif t:DiffColors == 100
		redir => hl | silent highlight | redir END
		let h = map(filter(split(hl, '\n'),
			\'v:val =~ "^\\S" && v:val =~ "="'), 'split(v:val)[0]')
		unlet h[index(h, "DiffChange")]
		unlet h[index(h, "DiffText")]
		unlet h[index(h, hlexists("Cursor") ? "Cursor" : "MatchParen")]
		while !empty(h)
			let r = localtime() % len(h)
			let t:DChar.dmc += [h[r]] | unlet h[r]
		endwhile
	endif

	exec cwin . "wincmd w"
endfunction

function! s:ToggleDiffChar(...)
	if a:0 == 1 | let sline = a:1 | let eline = a:1
	elseif a:0 == 2 | let sline = a:1 | let eline = a:2
	else | return | endif

	if exists("t:DChar")
		for l in range(sline, eline)
			if has_key(t:DChar.mid[1], l) ||
						\has_key(t:DChar.mid[2], l)
				call s:ResetDiffChar(sline, eline)
				return
			endif
		endfor
	endif
	call s:ShowDiffChar(sline, eline)
endfunction

function! s:ResetDiffChar(sline, eline)
	if !exists("t:DChar") | return | endif

	let cwin = winnr()
	let cbuf = winbufnr(cwin)
	if cbuf == t:DChar.buf[1] | let k = 1
	elseif cbuf == t:DChar.buf[2] | let k = 2
	else | return | endif

	" create a DiffChar line list between sline/eline
	if exists("t:DChar.vdl")	" diff mode
		let [d1, d2] = s:GetDiffModeLines(k, a:sline, a:eline)
	else				" non-diff mode
		let d1 = range(a:sline, a:eline)
		let d2 = range(a:sline, a:eline)
	endif

	for k in [1, 2]
		" a split buf may have more than one windows, try all
		for w in range(1, winnr('$'))
			if winbufnr(w) == t:DChar.buf[k]
				exec w . "wincmd w"
				call s:ClearDiffChar(k, d{k})
			endif
		endfor
		if empty(t:DChar.mid[k])
			exec "au! BufWinLeave <buffer=" . t:DChar.buf[k] . ">"
			call s:ResetDiffCharPair(k)
		endif
	endfor

	" unlet t:DChar when no DiffChar highlightings in either buffer
	if empty(t:DChar.mid[1]) || empty(t:DChar.mid[2])
		unlet t:DChar
	endif

	exec cwin . "wincmd w"
endfunction

function! s:ShowDiffChar(sline, eline)
	" initialize when t:DChar is not defined
	if !exists("t:DChar") && s:InitializeDiffChar() == -1
		return
	endif

	let cwin = winnr()
	let cbuf = winbufnr(cwin)
	if cbuf == t:DChar.buf[1] | let k = 1
	elseif cbuf == t:DChar.buf[2] | let k = 2
	else | return | endif

	" create a DiffChar line list between sline/eline and get those lines
	if exists("t:DChar.vdl")	" diff mode
		let [d1, d2] = s:GetDiffModeLines(k, a:sline, a:eline)
		for k in [1, 2]
			let t{k} = []
			for d in d{k}
				let t{k} += getbufline(t:DChar.buf[k], d)
			endfor
			let n{k} = len(t{k})
		endfor
	else				" non-diff mode
		for k in [1, 2]
			let t{k} = getbufline(t:DChar.buf[k], a:sline, a:eline)
			let n{k} = len(t{k})
			let d{k} = range(a:sline, a:sline + n{k} - 1)
		endfor
	endif

	" remove redundant lines in either window
	if n1 == 0 || n2 == 0 | return | endif
	if n1 > n2
		unlet t1[n2 - n1 :]
		unlet d1[n2 - n1 :]
		let n1 = n2
	elseif n1 < n2
		unlet t2[n1 - n2 :]
		unlet d2[n1 - n2 :]
		let n2 = n1
	endif

	" a list of different lines and columns
	let lc1 = {}
	let lc2 = {}

	" compare each line and trace difference units
	for n in range(n1)
		if t1[n] ==# t2[n] | continue | endif

		" split each line to the difference units
		let u1 = split(t1[n], t:DChar.spt)
		let u2 = split(t2[n], t:DChar.spt)

		" find first/last same units and get them out to trace
		let ns = min([len(u1), len(u2)])
		let fu = 0
		while fu < ns && u1[fu] ==# u2[fu]
			let fu += 1
		endwhile
		let ns -= fu
		let lu = -1
		while lu >= -ns && u1[lu] ==# u2[lu]
			let lu -= 1
		endwhile
		let fsu = (fu == 0) ? [] : u1[:fu - 1]	" 1st same units
		let u1d = u1[fu : lu]	" actual difference units in u1
		let u2d = u2[fu : lu]	" actual difference units in u2

		" trace the actual diffference units
		let c1 = [] | let h1 = [] | let l1 = len(join(fsu, ''))
		let c2 = [] | let h2 = [] | let l2 = len(join(fsu, ''))
		for [edit, unit] in s:TraceDiffChar{t:DiffAlgorithm}(u1d, u2d)
								\+ [['=', '']]
			let m = len(unit)
			if edit == '='
				if !empty(h1) || !empty(h2)
					let c1 += [h1] | let h1 = []
					let c2 += [h2] | let h2 = []
				endif
				let l1 += m
				let l2 += m
			elseif edit == '-'
				let h1 += range(l1 + 1, l1 + m)
				let l1 += m
			elseif edit == '+'
				let h2 += range(l2 + 1, l2 + m)
				let l2 += m
			endif
		endfor

		" add different lines and columns to the list
		let lc1[d1[n]] = c1
		let lc2[d2[n]] = c2
	endfor

	" highlight lines and columns and add it to the mid record
	for k in [1, 2]
		" a split buf may have more than one windows, try all
		for w in range(1, winnr('$'))
			if winbufnr(w) == t:DChar.buf[k]
				exec w . "wincmd w"
				call s:ClearDiffChar(k, keys(lc{k}))
			endif
		endfor
		exec bufwinnr(t:DChar.buf[k]) . "wincmd w"
		call s:HighlightDiffChar(k, lc{k})
		if !empty(t:DChar.mid[k]) &&
			\!exists("#BufWinLeave#<buffer=" . t:DChar.buf[k] . ">")
			exec "au BufWinLeave <buffer=" . t:DChar.buf[k] .
					\"> call s:ResetDiffChar(1, line('$'))"
		endif
	endfor

	" reset t:DChar when no DiffChar highlightings in either buffer
	if empty(t:DChar.mid[1]) || empty(t:DChar.mid[2])
		unlet t:DChar
	endif

	exec cwin . "wincmd w"
endfunction

function! s:GetDiffModeLines(key, sline, eline)
	" in diff mode, need to compare the different line between buffers
	" if current buffer is t:DChar.buf[1], narrow sline <= t:DChar.vdl[1]
	" <= eline and get the corresponding lines from t:DChar.vdl[2]
	let [i, j] = (a:key == 1) ? [1, 2] : [2, 1]
	let d1 = copy(t:DChar.vdl[1])
	let d2 = copy(t:DChar.vdl[2])
	for l in range(len(d{i}))
		if d{i}[l] < a:sline || a:eline < d{i}[l]
			let d{i}[l] = -1
			let d{j}[l] = -1
		endif
	endfor
	call filter(d1, 'v:val != -1')
	call filter(d2, 'v:val != -1')
	return [d1, d2]
endfunction

function! s:ClearDiffChar(key, lines)
	" if actual mids does not include a recorded mid,
	" maybe this is another split window for the buf
	let amid = map(getmatches(), 'v:val.id')
	if empty(values(t:DChar.mid[a:key])) ||
			\index(amid, values(t:DChar.mid[a:key])[0][0]) == -1
		return
	endif

	for l in a:lines
		if has_key(t:DChar.mid[a:key], l)
			for id in t:DChar.mid[a:key][l]
				call matchdelete(id)
			endfor
			unlet t:DChar.mid[a:key][l]
			unlet t:DChar.hlc[a:key][l]
		endif
	endfor
endfunction

function! s:HighlightDiffChar(key, lncol)
	let nc = len(t:DChar.dmc)
	for [line, col] in items(a:lncol)
		let dl = '\%' . line . 'l'
		let mid = [matchadd("DiffChange", dl . '.', 0)]
		for i in range(len(col))
			let c = col[i]
			if empty(c) | continue | endif
			let dc = '\%>' . (c[0] - 1) . 'c\%<' . (c[-1] + 1) . 'c'
			let mid += [matchadd(t:DChar.dmc[i % nc], dl . dc, 0)]
		endfor
		let t:DChar.mid[a:key][line] = mid
		let t:DChar.hlc[a:key][line] = col
	endfor
endfunction

function! s:JumpDiffChar(dir, pos)
	" dir : 1 = forward, else = backward
	" pos : 1 = start, else = end
	if !exists("t:DChar") | return | endif

	let cwin = winnr()
	let cbuf = winbufnr(cwin)
	if cbuf == t:DChar.buf[1] | let k = 1
	elseif cbuf == t:DChar.buf[2] | let k = 2
	else | return | endif

	let found = 0
	let l = line('.')
	while !found && 1 <= l && l <= line('$')
		if has_key(t:DChar.hlc[k], l)
			if l == line('.')
				let c = col('.')
				if !a:pos
					" end pos workaround for multibyte char
					let c += len(matchstr(getline(l), '.',
								\c - 1)) - 1
				endif
			else
				let c = a:dir ? 0 : 99999
			endif
			let hc = map(copy(t:DChar.hlc[k][l]),
				\'empty(v:val) ? "" : v:val[a:pos ? 0 : -1]')
			if !a:dir
				let c = - c
				call map(reverse(hc),
						\'empty(v:val) ? "" : - v:val')
			endif
			for n in range(len(hc))
				if !empty(hc[n]) && c < hc[n]
					let c = hc[n]
					if !a:dir
						let c = - c
						let n = len(hc) - n - 1
					endif
					call cursor(l, c)
					call s:ShowDiffCharPair(k, l, n, a:pos)
					let found = 1
					break
				endif
			endfor
		endif
		let l = a:dir ? l + 1 : l - 1
	endwhile

	exec cwin . "wincmd w"
endfunction

function! s:ShowDiffCharPair(key, line, icol, pos)
	let m = (a:key == 1) ? 2 : 1
	if exists("t:DChar.vdl")	" diff mode
		let line = t:DChar.vdl[m][index(t:DChar.vdl[a:key], a:line)]
	else				" non-diff mode
		let line = a:line
	endif

	exec bufwinnr(t:DChar.buf[m]) . "wincmd w"
	call s:ResetDiffCharPair(m)
	let col = t:DChar.hlc[m][line][a:icol]
	if empty(col)
		echo "No matching unit exists!"
	else
		let cu = getline(line)[col[0] - 1 : col[-1] - 1]
		" end pos workaround for multibyte char
		let c = a:pos ? col[0] : col[- len(split(cu, '\zs')[-1])]
		let t:DChar.mpc[m] =
			\matchadd(hlexists("Cursor") ? "Cursor" : "MatchParen",
					\'\%' . line . 'l\%' . c . 'c', 0)
		if !exists("#WinEnter<buffer=" . t:DChar.buf[m] . ">")
			exec "au WinEnter <buffer=" . t:DChar.buf[m] .
				\"> call s:ResetDiffCharPair(" . m . ")"
		endif
		" echo the matching unit with its color
		exec "echohl " . t:DChar.dmc[a:icol % len(t:DChar.dmc)]
		echo cu
		echohl None
	endif
	exec bufwinnr(t:DChar.buf[a:key]) . "wincmd w"
endfunction

function! s:ResetDiffCharPair(key)
	if exists("t:DChar.mpc[a:key]")
		let amid = map(getmatches(), 'v:val.id')
		if index(amid, t:DChar.mpc[a:key]) == -1 | return | endif
		call matchdelete(t:DChar.mpc[a:key])
		unlet t:DChar.mpc[a:key]
		exec "au! WinEnter <buffer=" . t:DChar.buf[a:key] . ">"
		echo
	endif
endfunction

" O(NP) Difference algorithm
function! s:TraceDiffCharONP(u1, u2)
	let n1 = len(a:u1)
	let n2 = len(a:u2)
	if n1 == 0 && n2 == 0 | return [] | endif

	" reverse to be N <= M, u2 <= u1
	if n1 >= n2
		let reverse = 0
		let M = n1 | let N = n2
		let u1 = a:u1 | let u2 = a:u2
	else
		let reverse = 1
		let M = n2 | let N = n1
		let u1 = a:u2 | let u2 = a:u1
	endif

	let fp = repeat([-1], M + N + 3)
	let offset = N + 1
	let delta = M - N
	let etree = []		" [next edit, previous p, previous k]

	let p = -1
	while fp[delta + offset] != M
		let p += 1
		let etree += [repeat([['', 0, 0]], p * 2 + delta + 1)]
		for [k, r] in map(range(-p, delta - 1, 1), '[v:val, "A"]')
			\+ map(range(delta + p, delta + 1, -1), '[v:val, "C"]')
			\+ [[delta, "B"]]
			if fp[k - 1 + offset] < fp[k + 1 + offset]
				let x = fp[k + 1 + offset]
				let pp = (r == 'A') ? p - 1 : p
				let pk = k + 1
				let ed = '+'
			else
				let x = fp[k - 1 + offset] + 1
				let pp = (r == 'C') ? p - 1 : p
				let pk = k - 1
				let ed = '-'
			endif
			let y = x - k
			while x < M && y < N && u1[x] ==# u2[y]
				let x += 1
				let y += 1
				let ed .= '='
			endwhile
			let fp[k + offset] = x
			" add [ed, pp, pk] for current p and k
			let etree[p][p + k] = [ed, pp, pk]
		endfor
	endwhile

	" create an edit sequence back from last p and k
	let eseq = ''
	while p >= 0 && p + k >= 0
		let eseq = etree[p][p + k][0] . eseq
		let [p, k] = etree[p][p + k][1:2]
	endwhile
	let eseq = eseq[1:]		" delete the first entry

	" trace the edit sequence starting from [0, 0] and
	" create a shortest edit script (SES)
	let ses = repeat([['', '']], len(eseq))	" ['+/-/=', unit]
	let i = 0
	let j = 0
	for n in range(len(eseq))
		let edit = eseq[n]
		if edit == '='
			let unit = u1[i]
			let i += 1
			let j += 1
		elseif edit == '-'
			let unit = u1[i]
			let i += 1
		elseif edit == '+'
			let unit = u2[j]
			let j += 1
		endif
		let ses[n] = [edit, unit]
	endfor
	if reverse		" reverse the edit
		call map(ses, '[tr(v:val[0], "+-", "-+"), v:val[1]]')
	endif

	return ses
endfunction

" O(ND) Difference algorithm
function! s:TraceDiffCharOND(u1, u2)	
	let n1 = len(a:u1)
	let n2 = len(a:u2)
	if n1 == 0 && n2 == 0 | return [] | endif

	let offset = n1 + n2
	let V = repeat([0], offset * 2 + 1)
	let etree = []		" [next edit, previous K's position]
	let found = 0

	for D in range(offset + 1)
		let etree += [[]]	" add a list for each D
		for k in range(-D, D, 2)
			if k == -D || k != D &&
					\V[k - 1 + offset] < V[k + 1 + offset]
				let x = V[k + 1 + offset]
				let pk = k + 1
				let ed = '+'
			else
				let x = V[k - 1 + offset] + 1
				let pk = k - 1
				let ed = '-'
			endif
			let y = x - k
			while x < n1 && y < n2 && a:u1[x] ==# a:u2[y]
				let x += 1
				let y += 1
				let ed .= '='
			endwhile
			let V[k + offset] = x
			" add [ed, pk] of k for [-D], [-D+2], ..., [D-2], [D]
			let etree[D] += [[ed, (pk + D - 1)/2]]
			" find the goal?
			if x >= n1 && y >= n2 | let found = 1 | break | endif
		endfor
		if found | break | endif	" break loop
	endfor

	" create an edit sequence back from last D
	let eseq = ''	
	let pk = -1
	for d in range(D, 0, -1)
		let eseq = etree[d][pk][0] . eseq
		let pk = etree[d][pk][1]
	endfor
	let eseq = eseq[1:]			" delete the first entry

	" trace the edit sequence starting from [0, 0] and
	" create a shortest edit script (SES)
	let ses = repeat([['', '']], len(eseq))	" ['+/-/=', unit]
	let i = 0
	let j = 0
	for n in range(len(eseq))
		let edit = eseq[n]
		if edit == '='
			let unit = a:u1[i]
			let i += 1
			let j += 1
		elseif edit == '-'
			let unit = a:u1[i]
			let i += 1
		elseif edit == '+'
			let unit = a:u2[j]
			let j += 1
		endif
		let ses[n] = [edit, unit]
	endfor

	return ses
endfunction

" Basic Difference algorithm
function! s:TraceDiffCharBasic(u1, u2)
	let n1 = len(a:u1)
	let n2 = len(a:u2)
	if n1 == 0 && n2 == 0 | return [] | endif

	" initialize an edit graph [next edit, # of steps to goal]
	let egraph = []
	for i in range(n1 + 1)
		let egraph += [repeat([['', 0]], n2 + 1)]
	endfor

	" assign values in egraph[] based on u1 and u2
	let egraph[n1][n2] = ['*', 0]		" last point = goal
	for i in range(n1)			" last column's points
		let egraph[i][n2] = ['-', n1 - i]
	endfor
	for j in range(n2)			" last row's points
		let egraph[n1][j] = ['+', n2 - j]
	endfor
	for i in range(n1 - 1, 0, -1)		" other points from goal
		for j in range(n2 - 1, 0, -1)
			if a:u1[i] ==# a:u2[j]
				let egraph[i][j] =
						\['=', egraph[i + 1][j + 1][1]]
			elseif egraph[i + 1][j][1] < egraph[i][j + 1][1]
				let egraph[i][j] =
						\['-', egraph[i + 1][j][1] + 1]
			else
				let egraph[i][j] =
						\['+', egraph[i][j + 1][1] + 1]
			endif
		endfor
	endfor

	" trace the next edit starting from [0, 0] and
	" create a shortest edit script (SES)
	let ses = []			" ['+/-/=', unit]
	let i = 0
	let j = 0
	while 1
		let edit = egraph[i][j][0]
		if edit == '='
			let unit = a:u1[i]
			let i += 1
			let j += 1
		elseif edit == '-'
			let unit = a:u1[i]
			let i += 1
		elseif edit == '+'
			let unit = a:u2[j]
			let j += 1
		elseif edit == '*'
			break
		endif
		let ses += [[edit, unit]]
	endwhile

	return ses
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
