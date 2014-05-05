" diffchar.vim - Highlight the difference, character by character
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
"      (file B) The <|lazy|> fox jumped over the<| swift|> <|brown|> dog.
"
" Sample commands:
" :[range]SDChar (Highlight DiffChar for [range])
" :RDChar (Reset DiffChar)
"
" Sample keymaps:
" <F7> (for all lines and toggle the DiffChar highlight)
" <F8> (for current line and toggle the DiffChar highlight)
"
" Note that this script assumes that there have been only 2 windows and
" both windows have same number of lines displayed. And try to compare
" the text on the same line in both windows. If DiffAdd and/or DiffDelete
" lines exist, it might work incorrectly.
"
" Author: Rick Howe
" Last Change: 2014/5/5
" Created:
" Requires:
" Version: 2.0

" sample commands
command! -range SDChar :call s:ShowDiffChar(<line1>, <line2>)
command! RDChar :call s:ResetDiffChar()

" sample keymaps
map <F7> :call <SID>ToggleDiffChar(1, line('$'))<CR>
map <F8> :call <SID>ToggleDiffChar(line('.'))<CR>

" Set a difference algorithm function, ONP, OND, or Basic
let s:DiffAlgorithm = "ONP"

let s:diffchar = 0
function! s:ToggleDiffChar(...)
	if a:0 == 1 | let sln = a:1 | let eln = a:1
	elseif a:0 == 2 | let sln = a:1 | let eln = a:2
	else | return
	endif
	if s:diffchar == 0
		if s:ShowDiffChar(sln, eln) != -1
			let s:diffchar = 1
		endif
	else
		call s:ResetDiffChar()
		let s:diffchar = 0
	endif
endfunction

function! s:ResetDiffChar()
	let w = winnr()
	windo call clearmatches()
	exec w .'wincmd w'
endfunction

function! s:ShowDiffChar(sln, eln)
	if a:sln > a:eln | return -1 | endif
	if winnr('$') != 2 | echo "More/less than 2 Windows!" | return -1 | endif

	" get lines (sln, eln) of both windows
	let tln1 = getline(a:sln, a:eln)
	let nbl1 = len(tln1)
	wincmd w
	let tln2 = getline(a:sln, a:eln)
	let nbl2 = len(tln2)
	wincmd w
	if nbl1 != nbl2 | echo "Not same # of lines!" | return -1 | endif

	" compare each line and check different chars
	let dln = []		" a list of diff lines
	let dc1 = []		" a list of diff columns for each line in w1
	let dc2 = []		" a list of diff columns for each line in w2
	for n in range(nbl1)
		let s1 = tln1[n]
		let s2 = tln2[n]
		if s1 !=# s2
			let [fs, s1d, s2d, ls] = s:FindSameDiffStr(s1, s2)
			let nfs = len(fs)
			let c1 = []
			let c2 = []
			let s1 = ''
			let s2 = ''
			for [d, c] in s:TraceDiffChar{s:DiffAlgorithm}(s1d, s2d)
				if d == '-'
					call add(c1, nfs + len(s1) + 1)
					let s1 .= c
				elseif d == '+'
					call add(c2, nfs + len(s2) + 1)
					let s2 .= c
				elseif d == '='
					let s1 .= c
					let s2 .= c
				endif
			endfor
			call add(dln, a:sln + n)
			call add(dc1, c1)
			call add(dc2, c2)
		endif
	endfor

	" highlight different lines and columns
	call s:HighlightDiffChar(dln, dc1)
	wincmd w
	call s:HighlightDiffChar(dln, dc2)
	wincmd w
endfunction

function! s:HighlightDiffChar(ll, lc)
	for n in range(len(a:ll))
		let dl = '\%' . a:ll[n] . 'l'
		call matchadd("DiffChange", dl . '.')
		if !empty(a:lc[n])
			call matchadd("DiffText", dl . '\%(' . join(map(a:lc[n], '"\\%" . v:val . "c"'), '\|') . '\)')
		endif
	endfor
endfunction

function! s:FindSameDiffStr(s1, s2)
	let s1 = split(a:s1, '\zs')
	let s2 = split(a:s2, '\zs')
	let n1 = len(s1)
	let n2 = len(s2)
	let ns = (n1 < n2) ? n1 : n2
	let fs = ''
	let ls = ''

	" find the fist same string starting from 0
	for i in range(ns)
		if s1[i] !=# s2[i] | break | endif
		let fs = fs . s1[i]
		let s1[i] = ''
		let s2[i] = ''
	endfor
	" find the last same string ending to $
	for i in range(-1, -ns, -1)
		if s1[i] !=# s2[i] | break | endif
		let ls = s1[i] . ls
		let s1[i] = ''
		let s2[i] = ''
	endfor

	" also return different strings between the first and last same strings
	return [fs, join(s1, ''), join(s2, ''), ls]
endfunction

" O(NP) Difference algorithm
function! s:TraceDiffCharONP(s1, s2)
	let s:s1 = split(a:s1, '\zs')
	let s:s2 = split(a:s2, '\zs')
	let n1 = len(s:s1)
	let n2 = len(s:s2)
	if n1 == 0 && n2 == 0 | return [] | endif
	
	" reverse to be N <= M, s2 <= s1
	if n1 > n2
		let rev = 0
		let s:M = n1 | let s:N = n2
	else
		let rev = 1
		let s:M = n2 | let s:N = n1
		let sx = s:s1 | let s:s1 = s:s2 | let s:s2 = sx
	endif

	let s:fp = repeat([-1], s:M + s:N + 3)
	let s:offset = s:N + 1
	let delta = s:M - s:N
	let s:dtree = []	" [next direction, previous p, previous k]

	let s:p = 0
	while 1
		call add(s:dtree, repeat([['', 0, 0]], len(range(-s:p, delta + s:p))))
		for k in range(-s:p, delta - 1, 1)
			let s:fp[k + s:offset] = s:Snake(k, 'A')
		endfor
		for k in range(delta + s:p, delta + 1, -1)
			let s:fp[k + s:offset] = s:Snake(k, 'C')
		endfor
		let k = delta | let s:fp[k + s:offset] = s:Snake(k, 'B')
		" find the goal?
		if s:fp[delta + s:offset] == s:M
			break
		endif
		let s:p += 1
	endwhile

	" create a sequence of direction back from last p and k
	let ds = ''
	while s:p >= 0 && s:p + k >= 0
		let ds = s:dtree[s:p][s:p + k][0] . ds
		let [s:p, k] = s:dtree[s:p][s:p + k][1:2]
	endwhile

	" trace the direction starting from [0, 0]
	let d_c = []		" ['+/-/=', char]
	let i = 0
	let j = 0
	for dir in split(ds[1:], '\zs')
		if dir == '-'
			let char = s:s1[i]
			let i += 1
		elseif dir == '+'
			let char = s:s2[j]
			let j += 1
		elseif dir == '='
			let char = s:s1[i]
			let i += 1
			let j += 1
		endif
		if rev == 1	" reverse the operation
			let dir = tr(dir, "+-", "-+")
		endif
		call add(d_c, [dir, char])
	endfor

	return d_c
endfunction

function! s:Snake(k, range)
	if s:fp[a:k - 1 + s:offset] + 1 <= s:fp[a:k + 1 + s:offset]
		let x = s:fp[a:k + 1 + s:offset]
		let pp = (a:range == 'A') ? s:p - 1 : s:p
		let pk = a:k + 1
		let dir = '+'
	else
		let x = s:fp[a:k - 1 + s:offset] + 1
		let pp = (a:range == 'C') ? s:p - 1 : s:p
		let pk = a:k - 1
		let dir = '-'
	endif
	let y = x - a:k
	while x < s:M && y < s:N && s:s1[x] ==# s:s2[y]
		let x += 1
		let y += 1
		let dir .= '='
	endwhile
	" add [dir, pp, pk] for p and k
	let s:dtree[s:p][s:p + a:k] = [dir, pp, pk]
	return x
endfunction

" O(ND) Difference algorithm
function! s:TraceDiffCharOND(s1, s2)	
	let s1 = split(a:s1, '\zs')
	let s2 = split(a:s2, '\zs')
	let n1 = len(s1)
	let n2 = len(s2)
	if n1 == 0 && n2 == 0 | return [] | endif

	let offset = n1 + n2
	let V = repeat([0], offset * 2 + 1)
	let dtree = []		" [next direction, previous K's position]
	let found = 0

	for D in range(offset + 1)
		call add(dtree, [])	" add a list for each D
		for k in range(-D, D, 2)
			if k == -D || k != D && V[k - 1 + offset] < V[k + 1 + offset]
				let x = V[k + 1 + offset]
				let pk = k + 1
				let dir = '+'
			else
				let x = V[k - 1 + offset] + 1
				let pk = k - 1
				let dir = '-'
			endif
			let y = x - k
			while x < n1 && y < n2 && s1[x] ==# s2[y]
				let x += 1
				let y += 1
				let dir .= '='
			endwhile
			let V[k + offset] = x
			" add [dir, pk] of k for [-D], [-D+2], ..., [D-2], [D]
			call add(dtree[D], [dir, (pk + D - 1)/2])
			" find the goal?
			if(x >= n1 && y >= n2) | let found = 1 | break | endif
		endfor
		if found == 1 | break | endif	" break loop
	endfor

	" create a sequence of direction back from last D
	let ds = ''	
	let pk = -1
	for d in range(D, 0, -1)
		let ds = dtree[d][pk][0] . ds
		let pk = dtree[d][pk][1]
	endfor

	" trace the direction starting from [0, 0]
	let d_c = []		" ['+/-/=', char]
	let i = 0
	let j = 0
	for dir in split(ds[1:], '\zs')
		if dir == '-'
			let char = s1[i]
			let i += 1
		elseif dir == '+'
			let char = s2[j]
			let j += 1
		elseif dir == '='
			let char = s1[i]
			let i += 1
			let j += 1
		endif
		call add(d_c, [dir, char])
	endfor

	return d_c
endfunction

" Basic Difference algorithm
function! s:TraceDiffCharBasic(s1, s2)
	let s1 = split(a:s1, '\zs')
	let s2 = split(a:s2, '\zs')
	let n1 = len(s1)
	let n2 = len(s2)
	if n1 == 0 && n2 == 0 | return [] | endif

	" initialize d_s[next direction, # of steps to goal] graph
	let d_s = []
	for i in range(n1 + 1)
		call add(d_s, repeat([['', 0]], n2 + 1))
	endfor

	" assign values in d_s[] based on s1 and s2
	let d_s[n1][n2] = ['*', 0]		" last point = goal
	for i in range(n1)			" last column's points
		let d_s[i][n2] = ['-', n1 - i]
	endfor
	for j in range(n2)			" last row's points
		let d_s[n1][j] = ['+', n2 - j]
	endfor
	for i in range(n1 - 1, 0, -1)		" other points from goal
		for j in range(n2 - 1, 0, -1)
			if d_s[i + 1][j][1] < d_s[i][j + 1][1]
				let step = d_s[i + 1][j][1]
				let dir = '-'	" go down, vertical
			else
				let step = d_s[i][j + 1][1]
				let dir = '+'	" go right, horizontal
			endif
			" on the same character points, go diagonal
			if s1[i] ==# s2[j] && d_s[i + 1][j + 1][1] < step
				let step = d_s[i + 1][j + 1][1]
				let dir = '='	" go down and right, diagonal
			endif
			let d_s[i][j] = [dir, step + 1]
		endfor
	endfor

	" trace each points based on the next direction starting from [0, 0]
	let d_c = []			" ['+/-/=', char]
	let i = 0
	let j = 0
	while 1
		let dir = d_s[i][j][0]
		if dir == '-'
			let char = s1[i]
			let i += 1
		elseif dir == '+'
			let char = s2[j]
			let j += 1
		elseif dir == '='
			let char = s1[i]
			let i += 1
			let j += 1
		elseif dir == '*'
			break
		endif
		call add(d_c, [dir, char])
	endwhile

	return d_c
endfunction
