" diffchar.vim - Highlight the differences, based on characters and words
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
"   a difference unit. Its default is 'Char', which will trace character
"   by character as before. 'Word1' will split into \w\+ words and
"   any \W single characters. And 'Word2' will separate the units at the
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
"
" Note that this script tries to compare the text on the same line in both
" windows. If DiffAdd and/or DiffDelete lines exist, it might work incorrectly.
"
" This script has been always positively supporting mulltibyte characters.
"
" Author: Rick Howe
" Last Change: 2014/5/19
" Created:
" Requires:
" Version: 3.1

" Sample commands
command! -range SDChar :call s:ShowDiffChar(<line1>, <line2>)
command! -range RDChar :call s:ResetDiffChar(<line1>, <line2>)

" Sample keymaps
map <F7> :call <SID>ToggleDiffChar(1, line('$'))<CR>
map <F8> :call <SID>ToggleDiffChar(line('.'))<CR>

" Set a difference unit type
let g:DiffUnit = 'Char'		" any single character
" let g:DiffUnit = 'Word1'	" \w\+ word and any \W single character
" let g:DiffUnit = 'Word2'	" non-space and space words

" Set a difference algorithm
let g:DiffAlgorithm = 'ONP'
" let g:DiffAlgorithm = 'OND'
" let g:DiffAlgorithm = 'Basic'

" Line and its match ids record, {'line#': [mid1, mid2, ...], ...}
let s:mid1 = {}
let s:mid2 = {}
let s:buf1 = 0
let s:buf2 = 0

function! s:ToggleDiffChar(...)
	if a:0 == 1 | let sline = a:1 | let eline = a:1
	elseif a:0 == 2 | let sline = a:1 | let eline = a:2
	else | return
	endif
	for l in range(sline, eline)
		if has_key(s:mid1, l) || has_key(s:mid2, l)
			call s:ResetDiffChar(sline, eline)
			return
		endif
	endfor
	call s:ShowDiffChar(sline, eline)
endfunction

function! s:ResetDiffChar(sline, eline)
	let cbuf = winbufnr(0)
	if cbuf != s:buf1 && cbuf != s:buf2 | return | endif
	for i in (cbuf == s:buf1) ? [2, 1] : [1, 2]
		exec bufwinnr(s:buf{i}) . 'wincmd w'
		for l in range(a:sline, a:eline)
			if has_key(s:mid{i}, l)
				for id in s:mid{i}[l]
					call matchdelete(id)
				endfor
				unlet s:mid{i}[l]
			endif
		endfor
		if empty(s:mid{i})
			exec 'au! BufWinLeave <buffer=' . s:buf{i} . '>'
		endif
	endfor
endfunction

function! s:ShowDiffChar(sline, eline)
	" get buffer id only when there is no mid in both windows
	if empty(s:mid1) && empty(s:mid2)
		let wnum = winnr('$')
		if wnum == 1
			echo "Need another window!" | return
		else
			" select current and next (wincmd w) window's buffers
			let s:buf1 = winbufnr(0)
			let s:buf2 = winbufnr(winnr() % wnum + 1)
			" set event for new buffers
			exec 'au BufWinLeave <buffer=' . s:buf1 . '> call s:ResetDiffChar(1, line("$"))'
			exec 'au BufWinLeave <buffer=' . s:buf2 . '> call s:ResetDiffChar(1, line("$"))'
		endif
	endif

	" get lines (sline, eline) of both windows
	let cbuf = winbufnr(0)
	if cbuf != s:buf1 && cbuf != s:buf2 | return | endif
	for i in (cbuf == s:buf1) ? [2, 1] : [1, 2]
		exec bufwinnr(s:buf{i}) . 'wincmd w'
		let t{i} = getline(a:sline, a:eline)
		let n{i} = len(t{i})
	endfor

	" remove redundant lines in either window
	if n1 == 0 || n2 == 0 | return | endif
	if n1 > n2
		unlet t1[n2 - n1 :] | let n1 = n2
	elseif n1 < n2
		unlet t2[n1 - n2 :] | let n2 = n1
	endif

	" set a split pattern according to the difference unit type
	if g:DiffUnit == 'Char'		" any single character
		let sptn = '\zs'
	elseif g:DiffUnit == 'Word1'	" \w\+ word and any \W single character
		let sptn = '\(\w\+\|\W\)\zs'
	elseif g:DiffUnit == 'Word2'	" non-space and space words
		let sptn = '\(\s\+\|\S\+\)\zs'
	else
		echo "Not a valid difference unit type!" | return
	endif

	let lc1 = {}	" a list of different lines and columns for t1
	let lc2 = {}	" a list of different lines and columns for t2

	" compare each line and trace difference units
	for n in range(n1)
		if t1[n] ==# t2[n]
			\|| has_key(s:mid1, a:sline + n)
			\|| has_key(s:mid2, a:sline + n)
			continue
		endif

		" split each line to the difference units
		let u1 = split(t1[n], sptn)
		let u2 = split(t2[n], sptn)

		" find first/last same units and get them out to trace
		let ns = (len(u1) < len(u2)) ? len(u1) : len(u2)
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
		for [edit, unit] in s:TraceDiffChar{g:DiffAlgorithm}(u1d, u2d)
			let m = len(unit)
			if edit == '-'
				let c1 += range(l1 + 1, l1 + m)
				let l1 += m
			elseif edit == '+'
				let c2 += range(l2 + 1, l2 + m)
				let l2 += m
			elseif edit == '='
				let l1 += m
				let l2 += m
			endif
		endfor

		" add different lines and columns to the list
		let lc1[a:sline + n] = c1
		let lc2[a:sline + n] = c2
	endfor

	" highlight lines and columns and add to the mid record
	for i in (cbuf == s:buf1) ? [2, 1] : [1, 2]
		exec bufwinnr(s:buf{i}) . 'wincmd w'
		call extend(s:mid{i}, s:HighlightDiffChar(lc{i}))
	endfor
endfunction

function! s:HighlightDiffChar(lncol)
	let mid = {}
	for [line, col] in items(a:lncol)
		let dl = '\%' . line . 'l'
		let id = [matchadd('DiffChange', dl . '.')]
		if !empty(col)
			let dc = string(col[0])
			for i in range(1, len(col) - 1)
				let dc .= (col[i - 1] + 1 == col[i] ?  '-' : '#') . string(col[i])
			endfor
			let dc = substitute(dc, '\%(^\|#\)\zs\(\d\+\)-\%(\d\+-\)\+\(\d\+\)\ze\%(#\|$\)', '\\%>\1c\\%<\2c', 'g')
			let dc = substitute(dc, '>\zs\d\+\zec', '\=submatch(0) - 1', 'g')
			let dc = substitute(dc, '<\zs\d\+\zec', '\=submatch(0) + 1', 'g')
			let dc = tr(dc, '-', '#')
			let dc = substitute(dc, '\%(^\|#\)\zs\d\+\ze\%(#\|$\)', '\\%&c', 'g')
			let id += [matchadd('DiffText', dl . '\%(' . substitute(dc, '#', '\\|', 'g') . '\)')]
		endif
		let mid[line] = id
	endfor
	return mid
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
		call add(etree, repeat([['', 0, 0]], p * 2 + delta + 1))
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
		if edit == '-'
			let unit = u1[i]
			let i += 1
		elseif edit == '+'
			let unit = u2[j]
			let j += 1
		elseif edit == '='
			let unit = u1[i]
			let i += 1
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
		call add(etree, [])	" add a list for each D
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
			call add(etree[D], [ed, (pk + D - 1)/2])
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
		if edit == '-'
			let unit = a:u1[i]
			let i += 1
		elseif edit == '+'
			let unit = a:u2[j]
			let j += 1
		elseif edit == '='
			let unit = a:u1[i]
			let i += 1
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
		call add(egraph, repeat([['', 0]], n2 + 1))
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
		if edit == '-'
			let unit = a:u1[i]
			let i += 1
		elseif edit == '+'
			let unit = a:u2[j]
			let j += 1
		elseif edit == '='
			let unit = a:u1[i]
			let i += 1
			let j += 1
		elseif edit == '*'
			break
		endif
		call add(ses, [edit, unit])
	endwhile

	return ses
endfunction
