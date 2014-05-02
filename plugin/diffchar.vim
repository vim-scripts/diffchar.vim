" diffchar.vim - trace and highlight the difference, character by character
"
" This script has been developed in order to make diff mode more useful.
" DiffText does not show the exact difference, but this script will
" highlight its difference, character by character - so called DiffChar.
"
" Use this script just after diff command. DiffText area will be narrowed
" down to show the DiffChar. You can use this script in non-diff'ed usual
" mode as well.
"
" For example, diff command shows: (=== : DiffText area)
"
" The swift brown fox jumped over the fazy dogs. (file A)
"     ====================================
" The fazy fox jumped over the swift brown dogs. (file B)
"     ====================================
"
" then this script will narrow down the DiffText area:
"
" The swift brown fox jumped over the fazy dogs. (file A)
"     ===========                     ====
" The fazy fox jumped over the swift brown dogs. (file B)
"     ====                    ============
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
" The character comparing algorism in this scrip is a very basic one.
" So it might be necessary to improve the performance with other
" enhanced algorisms.
"
" Author: Rick Howe
" Last Change: 2014/5/2
" Created:     
" Requires: 
" Version:  1.0

" sample commands
command! -range SDChar :call s:ShowDiffChar(<line1>, <line2>)
command! RDChar :call s:ResetDiffChar()

" sample keymaps
map <F7> :call <SID>ToggleDiffChar(1, line('$'))<CR>
map <F8> :call <SID>ToggleDiffChar(line('.'))<CR>

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
			for [d, c] in s:TraceDiffChar(s1d, s2d)
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

function! s:TraceDiffChar(s1, s2)
	let s1 = split(a:s1, '\zs')
	let s2 = split(a:s2, '\zs')
	let n1 = len(s1)
	let n2 = len(s2)

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

	return(d_c)
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
