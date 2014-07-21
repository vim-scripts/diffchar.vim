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
" Update : 4.4
" * Enhanced to follow diffopt's icase and iwhite options for both diff and
"   non-diff modes (ignorecase option is not used). Previously, it has been
"   always case and space/tab sensitive.
" * Implemented to highlight the difference units using a new matchaddpos()
"   function, introduced in 7.4.330, when available to draw faster.
"
" Update : 4.3
"  Enhanced to differently show added/deleted/changed difference units
"  with original diff highlightings.
" - added units will be always highlighted with DiffAdd.
" - changed units will be highlighted based on the g:DiffColors (and
"   t:DiffColors) variable, but DiffText is always used for the first
"   changed unit.
" - when jumping cursor by "[b"/"]b" or "[e"/"]e" on the added unit, it
"   highlights around the corresponding deleted units with a cursor-type color
"   in another window, and echoes a diff-delete filler with DiffDelete,
"   along with common characters on both sides (e.g. a-----b).
"
" Update : 4.2
" * Enhanced to update the highlighted DiffChar units while editing.
"   A g:DiffUpdate (and t:DiffUpdate) variable enables and disables (default)
"   this update behavior. If a text line was added/deleted, reset all the
"   highlightings. This feature is available on vim 7.4.
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
" Last Change: 2014/07/21
" Created:
" Requires:
" Version: 4.4

if exists("g:loaded_diffchar")
	finish
endif
let g:loaded_diffchar = 4.4

let s:save_cpo = &cpo
set cpo&vim

" Sample commands
command! -range SDChar call s:ShowDiffChar(<line1>, <line2>)
command! -range RDChar call s:ResetDiffChar(<line1>, <line2>)

" Sample keymaps
nnoremap <silent> <F7> :call <SID>ToggleDiffChar(1, line('$'))<CR>
nnoremap <silent> <F8> :call <SID>ToggleDiffChar(line('.'))<CR>
nnoremap <silent> [b :call <SID>JumpDiffChar(0, 1)<CR>
nnoremap <silent> ]b :call <SID>JumpDiffChar(1, 1)<CR>
nnoremap <silent> [e :call <SID>JumpDiffChar(0, 0)<CR>
nnoremap <silent> ]e :call <SID>JumpDiffChar(1, 0)<CR>

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

" Set a difference unit updating while editing
if exists("##TextChanged") && exists("##TextChangedI")
let g:DiffUpdate = 0		" disable
" let g:DiffUpdate = 1		" enable
endif

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

	" set ignorecase and ignorespace flags
	let t:DChar.igc = (&diffopt =~ "icase")	
	let t:DChar.igs = (&diffopt =~ "iwhite")

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
		let t:DChar.spt = t:DChar.igs ? '\(\s\+\|.\)\zs' : '\zs'
	elseif t:DiffUnit == "Word1"	" \w\+ word and any \W character
		let t:DChar.spt = t:DChar.igs ? '\(\s\+\|\w\+\|\W\)\zs' :
							\'\(\w\+\|\W\)\zs'
	elseif t:DiffUnit == "Word2"	" non-space and space words
		let t:DChar.spt = '\(\s\+\|\S\+\)\zs'
	elseif t:DiffUnit == "Word3"	" \< or \> boundaries
		let t:DChar.spt = '\<\|\>'
	elseif t:DiffUnit =~ '^CSV(.\+)$'	" character separated
		let s = substitute(t:DiffUnit, '^CSV(\(.\+\))$', '\1', '')
		let t:DChar.spt = '\(\([^\'. s . ']\+\)\|\' . s . '\)\zs'
	else
		let t:DChar.spt = t:DChar.igs ? '\(\s\+\|.\)\zs' : '\zs'
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
		let t:DChar.dmc += ["NonText", "Search", "VisualNOS"]
	elseif t:DiffColors == 2
		let t:DChar.dmc += ["NonText", "Search", "VisualNOS",
			\"ErrorMsg", "MoreMsg", "TabLine", "Title"]
	elseif t:DiffColors == 3
		let t:DChar.dmc += ["NonText", "Search", "VisualNOS",
			\"ErrorMsg", "MoreMsg", "TabLine", "Title",
			\"StatusLine", "WarningMsg", "Conceal", "SpecialKey",
			\"ColorColumn", "ModeMsg", "SignColumn", "CursorLineNr"]
	elseif t:DiffColors == 100
		redir => hl | silent highlight | redir END
		let h = map(filter(split(hl, '\n'),
			\'v:val =~ "^\\S" && v:val =~ "="'), 'split(v:val)[0]')
		unlet h[index(h, "DiffAdd")]
		unlet h[index(h, "DiffDelete")]
		unlet h[index(h, "DiffChange")]
		unlet h[index(h, "DiffText")]
		unlet h[index(h, hlexists("Cursor") ? "Cursor" : "MatchParen")]
		while !empty(h)
			let r = localtime() % len(h)
			let t:DChar.dmc += [h[r]] | unlet h[r]
		endwhile
	endif

	" set a difference unit updating on this tab page
	" and a record of line string values
	if exists("##TextChanged") && exists("##TextChangedI")
		if !exists("t:DiffUpdate")
			let t:DiffUpdate = g:DiffUpdate
		endif
		if t:DiffUpdate
			let t:DChar.lsv = {}
		endif
	endif

	exec cwin . "wincmd w"
endfunction

function! s:ToggleDiffChar(...)
	if a:0 == 1 | let sline = a:1 | let eline = a:1
	elseif a:0 == 2 | let sline = a:1 | let eline = a:2
	else | return | endif

	if exists("t:DChar")
		for l in range(sline, eline)
			if has_key(t:DChar.hlc[1], l) ||
						\has_key(t:DChar.hlc[2], l)
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

	" set a possible DiffChar line list between sline and eline
	if exists("t:DChar.vdl")	" diff mode
		let [d1, d2] = s:GetDiffModeLines(k, a:sline, a:eline)
	else				" non-diff mode
		let d1 = range(a:sline, a:eline)
		let d2 = range(a:sline, a:eline)
	endif

	for k in [1, 2]
		" remove not highlighted lines
		let hl = map(keys(t:DChar.hlc[k]), 'eval(v:val)')
		for n in range(len(d{k}))
			if index(hl, d{k}[n]) == -1
				let d{k}[n] = -1
			endif
		endfor
		call filter(d{k}, 'v:val != -1')

		let buf = t:DChar.buf[k]

		" a split buf may have more than one windows, try all
		for w in range(1, winnr('$'))
			if winbufnr(w) == buf
				exec w . "wincmd w"
				call s:ClearDiffChar(k, d{k})
			endif
		endfor

		" when no highlight exists, reset events
		if empty(t:DChar.hlc[k])
			exec "au! BufWinLeave <buffer=" . buf . ">"
			if exists("t:DChar.lsv")
				exec "au! TextChanged <buffer=" . buf . ">"
				exec "au! TextChangedI <buffer=" . buf . ">"
				unlet t:DChar.lsv[k]
			endif
			call s:ResetDiffCharPair(k)
		endif
	endfor

	" reset t:DChar when no DiffChar highlightings in either buffer
	if empty(t:DChar.hlc[1]) || empty(t:DChar.hlc[2])
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

	" set a possible DiffChar line list between sline and eline
	if exists("t:DChar.vdl")	" diff mode
		let [d1, d2] = s:GetDiffModeLines(k, a:sline, a:eline)
	else				" non-diff mode
		let d1 = range(a:sline, a:eline)
		let d2 = range(a:sline, a:eline)
	endif

	" remove already highlighted lines and get those text
	for k in [1, 2]
		let hl = map(keys(t:DChar.hlc[k]), 'eval(v:val)')
		for n in range(len(d{k}))
			if index(hl, d{k}[n]) != -1
				let d{k}[n] = -1
			endif
		endfor
		call filter(d{k}, 'v:val != -1')

		let t{k} = []
		for d in d{k}
			let t{k} += getbufline(t:DChar.buf[k], d)
		endfor
		let n{k} = len(t{k})
	endfor

	" remove redundant lines in either window
	if n1 > n2
		unlet t1[n2 - n1 :]
		unlet d1[n2 - n1 :]
		let n1 = n2
	elseif n1 < n2
		unlet t2[n1 - n2 :]
		unlet d2[n1 - n2 :]
		let n2 = n1
	endif

	" set ignorecase flag
	let save_igc = &ignorecase
	let &ignorecase = t:DChar.igc

	" a list of different lines and columns
	let lc1 = {}
	let lc2 = {}

	" compare each line and trace difference units
	for n in range(n1)
		" split each line to the difference units
		let u1 = split(t1[n], t:DChar.spt)
		let u2 = split(t2[n], t:DChar.spt)

		" set unit lists for tracing
		let u1t = copy(u1)
		let u2t = copy(u2)
		if t:DChar.igs
			" convert \s\+ to a single space on ignorespace mode
			call map(u1t, 'substitute(v:val, "\\s\\+", " ", "g")')
			call map(u2t, 'substitute(v:val, "\\s\\+", " ", "g")')
		endif
		if u1t == u2t | continue | endif

		" find first/last same units and get them out to trace
		let ns = min([len(u1t), len(u2t)])
		let fu = 0
		while fu < ns && u1t[fu] == u2t[fu]
			let fu += 1
		endwhile
		let ns -= fu
		let lu = -1
		while lu >= -ns && u1t[lu] == u2t[lu]
			let lu -= 1
		endwhile
		let u1t = u1t[fu : lu]
		let u2t = u2t[fu : lu]

		" trace the actual diffference units
		let l1 = (fu == 0) ? 0 : len(join(u1[: fu - 1], ''))
		let l2 = (fu == 0) ? 0 : len(join(u2[: fu - 1], ''))
		let c1 = [] | let h1 = [] | let p1 = fu
		let c2 = [] | let h2 = [] | let p2 = fu
		for [ed, ut] in s:TraceDiffChar{t:DiffAlgorithm}(u1t, u2t)
								\+ [['*', '']]
			if ed == '=' || ed == '*'
				if !empty(h1)
					if !empty(h2)
						let c1 += [['c', h1]]
						let c2 += [['c', h2]]
					else
						let c1 += [['a', h1]]
						let c2 += [['d', [l2, l2 + 1]]]
					endif
				else
					if !empty(h2)
						let c1 += [['d', [l1, l1 + 1]]]
						let c2 += [['a', h2]]
					endif
				endif
				if ed == '='
					let h1 = []
					let h2 = []
					let l1 += len(u1[p1])
					let l2 += len(u2[p2])
					let p1 += 1
					let p2 += 1
				endif
			elseif ed == '-'
				let ul = len(u1[p1])
				let h1 += range(l1 + 1, l1 + ul)
				let l1 += ul
				let p1 += 1
			elseif ed == '+'
				let ul = len(u2[p2])
				let h2 += range(l2 + 1, l2 + ul)
				let l2 += ul
				let p2 += 1
			endif
		endfor

		" add different lines and columns to the list
		if !empty(c1) || !empty(c2)
			let lc1[d1[n]] = c1
			let lc2[d2[n]] = c2
		endif
	endfor

	" restore ignorecase flag
	let &ignorecase = save_igc

	" highlight lines and columns and set events
	for k in [1, 2]
		let buf = t:DChar.buf[k]
		exec bufwinnr(buf) . "wincmd w"
		call s:HighlightDiffChar(k, lc{k})
		if !empty(t:DChar.hlc[k])
			if !exists("#BufWinLeave#<buffer=" . buf . ">")
				exec "au BufWinLeave <buffer=" . buf .
					\"> call s:ResetDiffChar(1, line('$'))"
			endif
			if exists("t:DChar.lsv")
				if !exists("#TextChanged#<buffer=" . buf . ">")
					exec "au TextChanged <buffer=" . buf .
						\"> call s:UpdateDiffChar("
								\. k . ")"
				endif
				if !exists("#TextChangedI#<buffer=" . buf . ">")
					exec "au TextChangedI <buffer=" . buf .
						\"> call s:UpdateDiffChar("
								\. k . ")"
				endif
				let t:DChar.lsv[k] = s:GetLineStrValues(k)
			endif
		endif
	endfor

	" reset t:DChar when no DiffChar highlightings in either buffer
	if empty(t:DChar.hlc[1]) || empty(t:DChar.hlc[2])
		unlet t:DChar
	endif

	exec cwin . "wincmd w"
endfunction

function! s:GetLineStrValues(key)
	let hl = map(keys(t:DChar.hlc[a:key]), 'eval(v:val)')
	let lsv = []
	for l in range(1, line('$'))
		if index(hl, l) != -1
			let str = getbufline(t:DChar.buf[a:key], l)[0]
			let val = 0
			for n in range(len(str))
				let val += char2nr(str[n]) * (n + 1)
			endfor
			let lsv += [val]
		else
			let lsv += [-1]
		endif
	endfor
	return lsv
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

function! s:UpdateDiffChar(key)
	" if number of lines was changed, reset all
	if len(t:DChar.lsv[a:key]) != line('$')
		call s:ResetDiffChar(1, line('$'))
		return
	endif

	" save the current t:DChar settings except highlightings
	let sdc = deepcopy(t:DChar)
	let sdc.mid[1] = {} | let sdc.mid[2] = {}
	let sdc.hlc[1] = {} | let sdc.hlc[2] = {}

	" update only highlighted and current changed lines
	let lsv = s:GetLineStrValues(a:key)
	for l in map(keys(t:DChar.hlc[a:key]), 'eval(v:val)')
		if lsv[l - 1] != t:DChar.lsv[a:key][l - 1]
			call s:ResetDiffChar(l, l)
			if !exists("t:DChar") | let t:DChar = sdc | endif
			call s:ShowDiffChar(l, l)
		endif
	endfor
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

function! s:HighlightDiffChar(key, lnecol)
	for [l, ec] in items(a:lnecol)
		if !has_key(t:DChar.mid[a:key], l)
			if exists("*matchaddpos")
				let mid = [matchaddpos("DiffChange", [[l]], 0)]
			else
				let dl = '\%' . l . 'l'
				let mid = [matchadd("DiffChange", dl . '.', 0)]
			endif
			for i in range(len(ec))
				let [e, c] = ec[i]
				if e == 'd' | continue | endif
				let hl = (e == 'a') ? "DiffAdd" :
					\t:DChar.dmc[i % len(t:DChar.dmc)]
				if exists("*matchaddpos")
					let mid += [matchaddpos(hl,
						\[[l, c[0], len(c)]], 0)]
				else
					let dc = '\%>' . (c[0] - 1) . 'c\%<' .
						\(c[-1] + 1) . 'c'
					let mid += [matchadd(hl, dl . dc, 0)]
				endif
			endfor
			let t:DChar.mid[a:key][l] = mid
			let t:DChar.hlc[a:key][l] = ec
		endif
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
					let c += len(matchstr(getbufline(
						\cbuf, l)[0], '.', c - 1)) - 1
				endif
			else
				let c = a:dir ? 0 : 99999
			endif
			let hc = map(copy(t:DChar.hlc[k][l]),
						\'(v:val[0] == "d") ?
						\"" : v:val[1][a:pos ? 0 : -1]')
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

	let ln = getbufline(t:DChar.buf[m], line)[0]
	if t:DChar.hlc[m][line][a:icol][0] == 'd'
		" deleted unit
		let cp = t:DChar.hlc[m][line][a:icol][1][0]
		let pc = (0 < cp) ? split(ln[ : cp - 1], '\zs')[-1] : ""
		let nc = (cp < len(ln)) ? split(ln[cp : ], '\zs')[0] : ""
		" echo a-----b with DiffChange/DiffDelete
		echohl DiffChange
		echon pc
		echohl DiffDelete
		let col = t:DChar.hlc[a:key][a:line][a:icol][1]
		echon repeat('-', strwidth(getbufline(t:DChar.buf[a:key],
					\a:line)[0][col[0] - 1 : col[-1] - 1]))
		echohl DiffChange
		echon nc
		echohl None
		" set position/length for both side of deleted unit
		let clen = len(pc . nc)
		let cpos = cp - len(pc) + 1
	else
		" changed unit
		let col = t:DChar.hlc[m][line][a:icol][1]
		let dc = ln[col[0] - 1 : col[-1] - 1]
		" echo the matching unit with its color
		exec "echohl " . t:DChar.dmc[a:icol % len(t:DChar.dmc)]
		echon dc
		echohl None
		" set position/length for matching unit
		let clen = len(split(dc, '\zs')[a:pos ? 0 : -1])
		let cpos = col[a:pos ? 0 : - clen]
	endif

	" show cursor on deleted unit or matching unit on another window
	exec bufwinnr(t:DChar.buf[m]) . "wincmd w"
	call s:ResetDiffCharPair(m)
	if exists("*matchaddpos")
		let t:DChar.mpc[m] = matchaddpos(
			\hlexists("Cursor") ? "Cursor" : "MatchParen",
			\[[line, cpos, clen]], 0)
	else
		let t:DChar.mpc[m] = matchadd(
			\hlexists("Cursor") ? "Cursor" : "MatchParen",
			\'\%' . line . 'l\%>' . (cpos - 1) . 'c\%<' .
			\(cpos + clen) . 'c', 0)
	endif
	if !exists("#WinEnter<buffer=" . t:DChar.buf[m] . ">")
		exec "au WinEnter <buffer=" . t:DChar.buf[m] .
			\"> call s:ResetDiffCharPair(" . m . ")"
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
		echon ""
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
			while x < M && y < N && u1[x] == u2[y]
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
			while x < n1 && y < n2 && a:u1[x] == a:u2[y]
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
	let egph = []
	for i in range(n1 + 1)
		let egph += [repeat([['', 0]], n2 + 1)]
	endfor

	" assign values in egph[] based on u1 and u2
	let egph[n1][n2] = ['*', 0]		" last point = goal
	for i in range(n1)			" last column's points
		let egph[i][n2] = ['-', n1 - i]
	endfor
	for j in range(n2)			" last row's points
		let egph[n1][j] = ['+', n2 - j]
	endfor
	for i in range(n1 - 1, 0, -1)		" other points from goal
		for j in range(n2 - 1, 0, -1)
			if a:u1[i] == a:u2[j]
				let egph[i][j] = ['=', egph[i + 1][j + 1][1]]
			elseif egph[i + 1][j][1] < egph[i][j + 1][1]
				let egph[i][j] = ['-', egph[i + 1][j][1] + 1]
			else
				let egph[i][j] = ['+', egph[i][j + 1][1] + 1]
			endif
		endfor
	endfor

	" trace the next edit starting from [0, 0] and
	" create a shortest edit script (SES)
	let ses = []			" ['+/-/=', unit]
	let i = 0
	let j = 0
	while 1
		let edit = egph[i][j][0]
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
