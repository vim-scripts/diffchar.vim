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
" Sample commands:
" :[range]SDChar (Highlight DiffChar for [range])
" :[range]RDChar (Reset DiffChar for [range])
"
" Sample keymaps:
" <F7> (for all lines and toggle the DiffChar highlight)
" <F8> (for current line and toggle the DiffChar highlight)
" [b (jump to the start position of the previous DiffChar unit)
" ]b (jump to the start position of the next DiffChar unit)
" [e (jump to the end position of the previous DiffChar unit)
" ]e (jump to the end position of the next DiffChar unit)
"
" This script has been always positively supporting mulltibyte characters.
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
" Last Change: 2014/6/01
" Created:
" Requires:
" Version: 3.5

if exists("g:loaded_diffchar")
	finish
endif
let g:loaded_diffchar = 1

let s:save_cpo = &cpo
set cpo&vim

" Sample commands
command! -range SDChar :call s:ShowDiffChar(<line1>, <line2>)
command! -range RDChar :call s:ResetDiffChar(<line1>, <line2>)

" Sample keymaps
nmap <F7> :call <SID>ToggleDiffChar(1, line('$'))<CR>
nmap <F8> :call <SID>ToggleDiffChar(line('.'))<CR>
nmap [b :call <SID>MoveCursorDiffChar(0, 1)<CR>
nmap ]b :call <SID>MoveCursorDiffChar(1, 1)<CR>
nmap [e :call <SID>MoveCursorDiffChar(0, 0)<CR>
nmap ]e :call <SID>MoveCursorDiffChar(1, 0)<CR>

" Set a difference unit type
let g:DiffUnit = "Char"		" any single character
" let g:DiffUnit = "Word1"	" \w\+ word and any \W single character
" let g:DiffUnit = "Word2"	" non-space and space words

" Set a difference algorithm
let g:DiffAlgorithm = "ONP"
" let g:DiffAlgorithm = "OND"
" let g:DiffAlgorithm = "Basic"

function! s:InitializeDiffChar()
	if len(uniq(sort(tabpagebuflist()))) < 2
		echo "Need more buffers on this page!"
		return -1
	endif

	" define a DiffChar dictionary on this tab page
	let t:dchar = {}

	" select current and next (wincmd w) window's buffers
	let t:dchar.buf = {}
	let cwin = winnr()
	let t:dchar.buf[1] = winbufnr(cwin)
	let i = 0
	while 1		" find different buf than buf[1]
		let t:dchar.buf[2] = winbufnr((cwin + i) % winnr('$') + 1)
		if t:dchar.buf[1] != t:dchar.buf[2] | break | endif
		let i += 1
	endwhile

	" find corresponding DiffChange/DiffText highlight lines on both
	" diff mode buffers and set vim diff mode for selected buffers
	let t:dchar.vdm = 1
	let dcID = hlID("DiffChange")
	let dtID = hlID("DiffText")
	let t:dchar.dcl = {}
	for n in [1, 2]
		exec bufwinnr(t:dchar.buf[n]) . "wincmd w"
		let t:dchar.dcl[n] = []
		if &diff
			for l in range(1, line('$'))
				let id = diff_hlID(l, 1)
				if id == dcID || id == dtID
					let t:dchar.dcl[n] += [l]
				endif
			endfor
		endif
		let t:dchar.vdm = and(t:dchar.vdm, &diff)
	endfor

	" set line and its highlight id record
	let t:dchar.mid = {}
	let t:dchar.mid[1] = {}
	let t:dchar.mid[2] = {}

	" set highlighted lines and columns record
	let t:dchar.hlc = {}
	let t:dchar.hlc[1] = {}
	let t:dchar.hlc[2] = {}

	" set a difference unit type on this tab page and set a split pattern
	if !exists("t:DiffUnit")
		let t:DiffUnit = g:DiffUnit
	endif
	if t:DiffUnit == "Char"		" any single character
		let t:dchar.spt = '\zs'
	elseif t:DiffUnit == "Word1"	" \w\+ word and any \W character
		let t:dchar.spt = '\(\w\+\|\W\)\zs'
	elseif t:DiffUnit == "Word2"	" non-space and space words
		let t:dchar.spt = '\(\s\+\|\S\+\)\zs'
	else
		let t:dchar.spt = '\zs'
		echo 'Not a valid difference unit type. Use "Char" instead.'
	endif

	" set a difference algorithm on this tab page
	if !exists("t:DiffAlgorithm")
		let t:DiffAlgorithm = g:DiffAlgorithm
	endif

	exec cwin . "wincmd w"
endfunction

function! s:ToggleDiffChar(...)
	if a:0 == 1 | let sline = a:1 | let eline = a:1
	elseif a:0 == 2 | let sline = a:1 | let eline = a:2
	else | return | endif

	if exists("t:dchar")
		for l in range(sline, eline)
			if has_key(t:dchar.mid[1], l) || has_key(t:dchar.mid[2], l)
				call s:ResetDiffChar(sline, eline)
				return
			endif
		endfor
	endif
	call s:ShowDiffChar(sline, eline)
endfunction

function! s:ResetDiffChar(sline, eline)
	if !exists("t:dchar") | return | endif

	let cwin = winnr()
	let cbuf = winbufnr(cwin)
	if cbuf != t:dchar.buf[1] && cbuf != t:dchar.buf[2] | return | endif

	" create a DiffChar line list between sline/eline
	if t:dchar.vdm		" diff mode
		let [d1, d2] = s:GetDiffModeLines(a:sline, a:eline)
	else			" non-diff mode
		for n in [1, 2]
			let d{n} = range(a:sline, a:eline)
		endfor
	endif

	for n in [1, 2]
		" a split buf has more than 1 windows
		for w in range(1, winnr('$'))
			if winbufnr(w) == t:dchar.buf[n]
				exec w . "wincmd w"
				call s:ClearDiffChar(n, d{n})
			endif
		endfor
		if empty(t:dchar.mid[n])
			exec "au! BufWinLeave <buffer=" . t:dchar.buf[n] . ">"
		endif
	endfor

	" unlet t:dchar when no DiffChar highlightings in either buffer
	if empty(t:dchar.mid[1]) || empty(t:dchar.mid[2])
		unlet t:dchar
	endif

	exec cwin . "wincmd w"
endfunction

function! s:ShowDiffChar(sline, eline)
	" initialize when t:dchar is not defined
	if !exists("t:dchar") && s:InitializeDiffChar() == -1
		return
	endif

	let cwin = winnr()
	let cbuf = winbufnr(cwin)
	if cbuf != t:dchar.buf[1] && cbuf != t:dchar.buf[2] | return | endif

	" create a DiffChar line list between sline/eline and get those lines
	if t:dchar.vdm		" diff mode
		let [d1, d2] = s:GetDiffModeLines(a:sline, a:eline)
		for n in [1, 2]
			let t{n} = []
			for d in d{n}
				let t{n} += getbufline(t:dchar.buf[n], d)
			endfor
			let n{n} = len(t{n})
		endfor
	else			" non-diff mode
		for n in [1, 2]
			let t{n} = getbufline(t:dchar.buf[n], a:sline, a:eline)
			let n{n} = len(t{n})
			let d{n} = range(a:sline, a:sline + n{n} - 1)
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
		let u1 = split(t1[n], t:dchar.spt)
		let u2 = split(t2[n], t:dchar.spt)

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
		let c1 = []
		let c2 = []
		let l1 = len(join(fsu, ''))
		let l2 = l1
		for [edit, unit] in s:TraceDiffChar{t:DiffAlgorithm}(u1d, u2d)
			let m = len(unit)
			if edit == '='
				let l1 += m
				let l2 += m
			elseif edit == '-'
				let c1 += range(l1 + 1, l1 + m)
				let l1 += m
			elseif edit == '+'
				let c2 += range(l2 + 1, l2 + m)
				let l2 += m
			endif
		endfor

		" add different lines and columns to the list
		let lc1[d1[n]] = c1
		let lc2[d2[n]] = c2
	endfor

	" highlight lines and columns and add it to the mid record
	for n in [1, 2]
		" a split buf has more than 1 windows
		for w in range(1, winnr('$'))
			if winbufnr(w) == t:dchar.buf[n]
				exec w . "wincmd w"
				call s:ClearDiffChar(n, keys(lc{n}))
			endif
		endfor
		exec bufwinnr(t:dchar.buf[n]) . "wincmd w"
		call s:HighlightDiffChar(n, lc{n})
		if !empty(t:dchar.mid[n]) &&
			\!exists("#BufWinLeave#<buffer=" . t:dchar.buf[n] . ">")
			exec "au BufWinLeave <buffer=" . t:dchar.buf[n] . "> call s:ResetDiffChar(1, line('$'))"
		endif
	endfor

	" unlet t:dchar when no DiffChar highlightings in either buffer
	if empty(t:dchar.mid[1]) || empty(t:dchar.mid[2])
		unlet t:dchar
	endif

	exec cwin . "wincmd w"
endfunction

function! s:GetDiffModeLines(sline, eline)
	" in diff mode, need to compare the different line between buffers
	" if current buffer is t:dchar.buf[1], narrow sline <= t:dchar.dcl[1]
	" <= eline and get the corresponding lines from t:dchar.dcl[2]
	let d1 = copy(t:dchar.dcl[1])
	let d2 = copy(t:dchar.dcl[2])
	let cbuf = winbufnr(0)
	if cbuf == t:dchar.buf[1] | let i = 1 | let j = 2
	elseif cbuf == t:dchar.buf[2] | let i = 2 | let j = 1 | endif
	for n in range(len(d{i}))
		if d{i}[n] < a:sline || a:eline < d{i}[n]
			let d{i}[n] = -1
			let d{j}[n] = -1
		endif
	endfor
	for n in [1, 2]
		call filter(d{n}, 'v:val != -1')
	endfor
	return [d1, d2]
endfunction

function! s:ClearDiffChar(n, lines)
	" if recorded mids are not included in actual mids, no need to clean
	let amid = sort(map(getmatches(),'v:val.id'))
	let rmid = []
	for m in values(t:dchar.mid[a:n])
		let rmid += m
	endfor
	if uniq(sort(amid + rmid)) != amid | return | endif

	for l in a:lines
		if has_key(t:dchar.mid[a:n], l)
			for id in t:dchar.mid[a:n][l]
				call matchdelete(id)
			endfor
			unlet t:dchar.mid[a:n][l]
			unlet t:dchar.hlc[a:n][l]
		endif
	endfor
endfunction

function! s:HighlightDiffChar(n, lncol)
	for [line, col] in items(a:lncol)
		let dl = '\%' . line . 'l'
		let mid = [matchadd("DiffChange", dl . '.', 0)]
		let hlc = []
		if !empty(col)
			let dc = col[0]
			for c in range(1, len(col) - 1)
				let dc .= (col[c - 1] + 1 == col[c] ?  '-' : '#') . col[c]
			endfor
			for d in split(substitute(dc, '\d\+-\zs\%(\d\+-\)\+\ze\d\+', '', 'g'), '#')
				let hlc += map(d =~ '-' ? [split(d, '-')] : [[d, d]], '[eval(v:val[0]), eval(v:val[1])]') 
			endfor
			let dc = ''
			for [s, e] in hlc
				if s == e
					let dc .= '\%' . s . 'c'
				elseif s + 1 == e
					let dc .= '\%' . s . 'c\|\%' . e . 'c'
				else
					let dc .= '\%>' . (s - 1) . 'c\%<' . (e + 1) . 'c'
				endif
				let dc .= '\|'
			endfor
			let mid += [matchadd("DiffText", dl . '\%(' . dc[: -3] . '\)', 0)]
		endif
		let t:dchar.mid[a:n][line] = mid
		let t:dchar.hlc[a:n][line] = hlc
	endfor
endfunction

function! s:MoveCursorDiffChar(dir, pos)
	" dir : 1 = forward, else = backward
	" pos : 1 = start, else = end
	if !exists("t:dchar") | return | endif
	let cbuf = winbufnr(0)
	if cbuf != t:dchar.buf[1] && cbuf != t:dchar.buf[2] | return | endif
	let n = (cbuf == t:dchar.buf[1]) ? 1 : 2

	let l = line('.')
	if has_key(t:dchar.hlc[n], l) && !empty(t:dchar.hlc[n][l])
		let hlc = map(copy(t:dchar.hlc[n][l]), 'v:val[a:pos ? 0 : 1]')
		if !a:dir
			call map(reverse(hlc), '- v:val')
		endif
		let c = a:dir ? col('.') : - col('.')
		if !a:pos	" end position workaround for multibyte char
			let m = len(matchstr(getline('.'), '.', col('.') - 1))
			let c = a:dir ? c + (m - 1) : c - (m - 1)
		endif
		call filter(hlc, 'c < v:val')
		if !empty(hlc)
			call cursor(l, a:dir ? hlc[0] : - hlc[0])
			return
		endif
	endif
	let l = a:dir ? l + 1 : l - 1
	while 1 <= l && l <= line('$')
		if has_key(t:dchar.hlc[n], l) && !empty(t:dchar.hlc[n][l])
			let hlc = map(copy(t:dchar.hlc[n][l]), 'v:val[a:pos ? 0 : 1]')
			call cursor(l, hlc[a:dir ? 0 : -1])
			return
		endif
		let l = a:dir ? l + 1 : l - 1
	endwhile
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
	if reverse == 1		" reverse the edit
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
			if k == -D || k != D && V[k - 1 + offset] < V[k + 1 + offset]
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
		if found == 1 | break | endif	" break loop
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
				let egraph[i][j] = ['=', egraph[i + 1][j + 1][1]]
			elseif egraph[i + 1][j][1] < egraph[i][j + 1][1]
				let egraph[i][j] = ['-', egraph[i + 1][j][1] + 1]
			else
				let egraph[i][j] = ['+', egraph[i][j + 1][1] + 1]
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
