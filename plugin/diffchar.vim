" diffchar.vim - Highlight the exact differences, based on characters and words
"
" This plugin has been developed in order to make diff mode more useful.
" DiffText does not show the exact difference, but this plugin will
" highlight its difference, character by character - so called DiffChar.
"
" Use this plugin just after diff command. DiffText area will be narrowed
" down to show the DiffChar. You can use this plugin in non-diff usual
" mode as well.
"
" For example, diff command shows: (<|DiffText area|>)
"
"      (file A) The <|swift brown fox jumped over the lazy|> dog.
"      (file B) The <|lazy fox jumped over the swift brown|> dog.
"
" then this plugin will narrow down the DiffText area:
"
"      (file A) The <|swift brown|> fox jumped over the <|lazy|> dog.
"      (file B) The <|lazy|> fox jumped over the <|swift brown|> dog.
"
" Since update 4.7, vim is able to automatically show the exact differences
" on diff mode.
"
" This plugin has been always positively supporting mulltibyte characters.
"
" Sample commands
" :[range]SDChar - Highlight difference units for [range]
" :[range]RDChar - Reset the highlight of difference units for [range]
"
" Configurable sample keymaps
" <Plug>ToggleDiffCharAllLines (default: <F7>)
"     toggle the highlight/reset of difference units for all lines
" <Plug>ToggleDiffCharCurrentLine (default: <F8>)
"     toggle the highlight/reset of difference units for current line
" <Plug>JumpDiffCharPrevStart (default: [b)
"     jump cursor to the start position of the previous difference unit
" <Plug>JumpDiffCharNextStart (default: ]b)
"     jump cursor to the start position of the next difference unit
" <Plug>JumpDiffCharPrevEnd (default: [e)
"     jump cursor to the end position of the previous difference unit
" <Plug>JumpDiffCharNextEnd (default: ]e)
"     jump cursor to the end position of the next difference unit
"
" Global variables (and tab local variables when using t:)
" g:DiffUnit - type of difference unit
"     "Char"   : any single character (default)
"     "Word1"  : \w\+ word and any \W single character
"     "Word2"  : non-space and space words
"     "Word3"  : \< or \> character class boundaries
"     "CSV(,)" : separated by a character such as ',', ';', and '\t'
" g:DiffColors - matching colors for changed unit pairs
"     0   : always DiffText (default)
"     1   : 4 colors in fixed order
"     2   : 8 colors in fixed order
"     3   : 16 colors in fixed order
"     100 : all available colors in dynamic random order
"         (notes : always DiffAdd for added units)
" g:DiffUpdate - interactively updating of highlightings while editing
"     0 : disable (default)
"     1 : enable
"       (notes : available on vim 7.4)
" g:DiffAlgorithm - difference algorithm
"     "ONP"   : S.Wu, U.Manber, G.Myers and W.Miller,
"               "An O(NP) Sequence Comparison Algorithm" (default)
"     "OND"   : E.W.Myers, "An O(ND) Difference Algorithm and Its Variations"
"     "Basic" : basic algorithm using edit graph and shortest edit distance
"
" DiffCharExpr(iet, exd) function for the diffexpr option
"     iet: 1 = internal algorithm, 0 = external diff command,
"          else = threshold value of differences to apply either
"     exd: 1 = initially show exact differences, 0 = vim original ones
"
" Update : 4.8
" * Enhanced to set the threshold value on DiffCharExpr() to check how many
"   differences and then apply either of internal algorithm or external diff
"   command. The default for diffexpr option using DiffCharExpr() is changed
"   to use this threshold, 200 - apply internal if less than 200 differences,
"   apply external if more.
" * Changed the way to select windows when more than 2 windows in the page.
"   - automatically select the diff mode's next (wincmd w) window, if any,
"     in addition to the current window
"   - can select any of splitted windows as vim can do for diff
"
" Update : 4.7
" * Enhanced to set DiffCharExpr() to the diffexpr option, if it is empty.
"   When diff mode begins, vim calls this function which finds differences by
"   this plugin's internal diff algorithm (default) and then initially shows
"   the exact differences (default). You can also explicitly set this function
"   to the option with different arguments.
" * Enhanced to make the key mappings configurable.
"   For example, the default <F7> can be modified by:
"        nmap <silent> "your favorite key" <Plug>ToggleDiffCharAllLines
" * Fixed to correctly adjust the position of difference units when diffopt's
"   iwhite option is enabled.
"
" Update : 4.6
" * Fixed to correctly show the colors of changed units in one-by-one defined
"   order of g:DiffColors. Since an added unit was improperly counted as
"   changed one, some colors were skipped and not shown. The first changed
"   unit is now always highlighted with DiffText in any color mode.
"
" Update : 4.5
" * Fixed to trace the differences until the end of the units. Previously
"   the last same units were skipped, so last added units were sometimes shown
"   as changed ones (eg: the last "swift brown" on above were shown as changed
"   units but now shows "brown" as added ones).
" * Enhanced to use your global variables if defined in vimrc.
"
" Update : 4.4
" * Enhanced to follow diffopt's icase and iwhite options for both diff and
"   non-diff modes (ignorecase option is not used). Previously, it has been
"   always case and space/tab sensitive.
" * Implemented to highlight the difference units using a new matchaddpos()
"   function, introduced in 7.4.330, when available to draw faster.
"
" Update : 4.3
" * Enhanced to differently show added/deleted/changed difference units
"   with original diff highlightings.
"   - added units will be always highlighted with DiffAdd.
"   - changed units will be highlighted based on the g:DiffColors (and
"     t:DiffColors) variable, but DiffText is always used for the first
"     changed unit.
"   - when jumping cursor by "[b"/"]b" or "[e"/"]e" on the added unit, it
"     highlights around the corresponding deleted units with a cursor-type color
"     in another window, and echoes a diff-delete filler with DiffDelete,
"     along with common characters on both sides (e.g. a-----b).
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
" Last Change: 2015/01/12
" Created:
" Requires:
" Version: 4.8

if exists("g:loaded_diffchar")
	finish
endif
let g:loaded_diffchar = 4.8

let s:save_cpo = &cpo
set cpo&vim

" Sample commands
command! -range SDChar call s:ShowDiffChar(<line1>, <line2>)
command! -range RDChar call s:ResetDiffChar(<line1>, <line2>)

" Sample keymaps
nnoremap <silent> <Plug>ToggleDiffCharAllLines
				\ :call <SID>ToggleDiffChar(1, line('$'))<CR>
nnoremap <silent> <Plug>ToggleDiffCharCurrentLine
				\ :call <SID>ToggleDiffChar(line('.'))<CR>
nnoremap <silent> <Plug>JumpDiffCharPrevStart
				\ :call <SID>JumpDiffChar(0, 1)<CR>
nnoremap <silent> <Plug>JumpDiffCharNextStart
				\ :call <SID>JumpDiffChar(1, 1)<CR>
nnoremap <silent> <Plug>JumpDiffCharPrevEnd
				\ :call <SID>JumpDiffChar(0, 0)<CR>
nnoremap <silent> <Plug>JumpDiffCharNextEnd
				\ :call <SID>JumpDiffChar(1, 0)<CR>
if !hasmapto('<Plug>ToggleDiffCharAllLines', 'n')
	nmap <silent> <F7> <Plug>ToggleDiffCharAllLines
endif
if !hasmapto('<Plug>ToggleDiffCharCurrentLine', 'n')
	nmap <silent> <F8> <Plug>ToggleDiffCharCurrentLine
endif
if !hasmapto('<Plug>JumpDiffCharPrevStart', 'n')
	nmap <silent> [b <Plug>JumpDiffCharPrevStart
endif
if !hasmapto('<Plug>JumpDiffCharNextStart', 'n')
	nmap <silent> ]b <Plug>JumpDiffCharNextStart
endif
if !hasmapto('<Plug>JumpDiffCharPrevEnd', 'n')
	nmap <silent> [e <Plug>JumpDiffCharPrevEnd
endif
if !hasmapto('<Plug>JumpDiffCharNextEnd', 'n')
	nmap <silent> ]e <Plug>JumpDiffCharNextEnd
endif

" Set a difference unit type
if !exists("g:DiffUnit")
let g:DiffUnit = "Char"		" any single character
" let g:DiffUnit = "Word1"	" \w\+ word and any \W single character
" let g:DiffUnit = "Word2"	" non-space and space words
" let g:DiffUnit = "Word3"	" \< or \> character class boundaries
" let g:DiffUnit = "CSV(,)"	" character separated by (,;\t)
endif

" Set a difference unit matching colors
if !exists("g:DiffColors")
let g:DiffColors = 0		" always 1 color
" let g:DiffColors = 1		" 4 colors in fixed order
" let g:DiffColors = 2		" 8 colors in fixed order
" let g:DiffColors = 3		" 16 colors in fixed order
" let g:DiffColors = 100	" all available colors in dynamic random order
endif

" Set a difference unit updating while editing
if !exists("g:DiffUpdate")
if exists("##TextChanged") && exists("##TextChangedI")
let g:DiffUpdate = 0		" disable
" let g:DiffUpdate = 1		" enable
endif
endif

" Set a difference algorithm
if !exists("g:DiffAlgorithm")
let g:DiffAlgorithm = "ONP"
" let g:DiffAlgorithm = "OND"
" let g:DiffAlgorithm = "Basic"
endif

" Set a diff expression
if !exists("g:DiffExpr") || g:DiffExpr
if empty(&diffexpr)
let &diffexpr = "DiffCharExpr(200, 1)"	" set threshold and show exact diffs
" let &diffexpr = "DiffCharExpr(1, 1)"	" apply int algo and show exact diffs
" let &diffexpr = "DiffCharExpr(1, 0)"	" apply int algo and show vim original
" let &diffexpr = "DiffCharExpr(0, 1)"	" apply ext cmd and show exact diffs
" let &diffexpr = "DiffCharExpr(0, 0)"	" apply ext cmd and show vim original
endif
endif

function! DiffCharExpr(iet, exd)
	" read both files to be diff traced
	let [f1, f2] = [readfile(v:fname_in), readfile(v:fname_new)]

	" find the fist diff trial call and return here
	if [f1, f2] == [["line1"], ["line2"]]
		call writefile(["1c1"], v:fname_out)
		return
	endif

	" check how many differences between f1 and f2
	if a:iet > 1
		for k in [1, 2]
			let d{k} = {}
			for s in f{k}
				if !empty(s) | let d{k}[s] = 0 | endif
			endfor
		endfor
		" compare with a:iet and apply either
		let iet = (- len(d1) - len(d2) + len(extend(d1, d2)) * 2)
							\< a:iet ? 1 : 0
	else
		let iet = a:iet
	endif
	" get diff commands from internal or external
	let dfcmd = iet ?
		\s:ApplyIntDiffAlgorithm(f1, f2) : s:ApplyExtDiffCommand()

	" write a list of diff commands
	call writefile(dfcmd, v:fname_out)

	" return if no need to show exact differences
	if exists("t:DChar") || !a:exd | return | endif

	" find 'c' command and extract the line to be replaced
	let [r1, r2] = [[], []]
	let cpt = '^\(\d\+\)\%(,\(\d\+\)\)\=c\(\d\+\)\%(,\(\d\+\)\)\=$'
	let rep = '\1 \2 \3 \4'
	for ct in filter(dfcmd, 'v:val =~ "c"')
		let cl = split(substitute(ct, cpt, rep, ''), ' ', 1)
		let nr = min([empty(cl[1]) ? 0 : cl[1] - cl[0],
					\empty(cl[3]) ? 0 : cl[3] - cl[2]])
		let [r1, r2] += [range(cl[0], cl[0] + nr),
						\range(cl[2], cl[2] + nr)]
	endfor

	" return if no replaced lines
	if empty(r1) | return | endif

	" initialize diffchar
	if s:InitializeDiffChar() == -1 | return | endif

	" check which window is v:fname_in/v:fname_new and
	" set those window ids and replaced lines
	let t:DChar.win = {}
	let t:DChar.vdl = {}
	for w in range(1, winnr('$'))
		if !getwinvar(w, "&diff") | continue | endif
		if getbufline(winbufnr(w), r1[0]) ==# [f1[r1[0] - 1]]
			let [t:DChar.win[1], t:DChar.vdl[1]] = [w, r1]
		elseif getbufline(winbufnr(w), r2[0]) ==# [f2[r2[0] - 1]]
			let [t:DChar.win[2], t:DChar.vdl[2]] = [w, r2]
		endif
	endfor

	" highlight the exact differences on the replaced lines
	if exists("t:DChar.win[1]") && exists("t:DChar.win[2]")
		call s:MarkDiffCharID(1)
		call s:ShowDiffChar(1, line('$'))
	else
		unlet t:DChar
	endif
endfunction

function! s:ApplyIntDiffAlgorithm(f1, f2)
	" handle icase and iwhite diff options
	let save_igc = &ignorecase
	let &ignorecase = (&diffopt =~ "icase")
	if &diffopt =~ "iwhite"
		for k in [1, 2]
			let f{k} = copy(a:f{k})
			call map(f{k}, 'substitute(v:val, "\\s\\+", " ", "g")')
			call map(f{k}, 'substitute(v:val, "\\s\\+$", "", "")')
		endfor
	else
		let [f1, f2] = [a:f1, a:f2]
	endif

	" trace the diff lines between f1/f2
	let dfcmd = []
	let [d1, d2, l1, l2] = [[], [], 1, 1]
	let [ex, nu] = ['', 0]
	for [ed, ut] in s:TraceDiffChar{g:DiffAlgorithm}(f1, f2)
						\+ [['=', ''], ['', '']]
		if ex == ed | let nu += 1 | continue | endif
		if ex == '='
			let ee = [empty(d1), empty(d2)]
			if ee == [0, 0]
				let dfcmd += [((d1[0] == d1[-1]) ?
					\d1[0] : d1[0] . ',' . d1[-1]) . 'c' .
					\((d2[0] == d2[-1]) ?
					\d2[0] : d2[0] . ',' . d2[-1])]
			elseif ee == [0, 1]
				let dfcmd += [((d1[0] == d1[-1]) ?
					\d1[0] : d1[0] . ',' . d1[-1])
					\. 'd' . (l2 - 1)]
			elseif ee == [1, 0]
				let dfcmd += [(l1 - 1) . 'a' .
					\((d2[0] == d2[-1]) ?
					\d2[0] : d2[0] . ',' . d2[-1])]
			endif
			let [d1, d2] = [[], []]
			let [l1, l2] += [nu + 1, nu + 1]
		elseif ex == '-'
			let [d1, l1] += [[l1, l1 + nu], nu + 1]
		elseif ex == '+'
			let [d2, l2] += [[l2, l2 + nu], nu + 1]
		endif
		let [ex, nu] = [ed, 0]
	endfor

	" restore ignorecase flag
	let &ignorecase = save_igc

	return dfcmd
endfunction

function! s:ApplyExtDiffCommand()
	" execute a diff command
	let opt = "-a --binary "
	if &diffopt =~ "icase" | let opt .= "-i " | endif
	if &diffopt =~ "iwhite" | let opt .= "-b " | endif
	let dfout = system("diff " . opt . v:fname_in . " " . v:fname_new)

	" return diff commands only
	return filter(split(dfout, '\n'), 'v:val =~ "^\\d"')
endfunction

function! s:InitializeDiffChar()
	if winnr('$') < 2
		echo "Need more windows on this tab page!"
		return -1
	endif

	" define a DiffChar dictionary on this tab page
	let t:DChar = {}

	" select current window and next diff mode window if possible
	let t:DChar.win = {}
	let cwin = winnr()
	let w = 0
	while 1
		let nwin = (cwin + w) % winnr('$') + 1
		if nwin == cwin
			let nwin = cwin % winnr('$') + 1
			break
		endif
		if getwinvar(nwin, "&diff") | break | endif
		let w += 1
	endwhile
	let [t:DChar.win[1], t:DChar.win[2]] = [cwin, nwin]
	call s:MarkDiffCharID(1)

	" find corresponding DiffChange/DiffText lines on diff mode windows
	let t:DChar.vdl = {}
	let dh = [hlID("DiffChange"), hlID("DiffText")]
	for k in [1, 2]
		if getwinvar(t:DChar.win[k], "&diff")
			exec t:DChar.win[k] . "wincmd w"
			let t:DChar.vdl[k] = []
			for l in range(1, line('$'))
				if index(dh, diff_hlID(l, 1)) != -1
					let t:DChar.vdl[k] += [l]
				endif
			endfor
		else
			unlet t:DChar.vdl
			break
		endif
	endfor
	exec cwin . "wincmd w"

	" set ignorecase and ignorespace flags
	let t:DChar.igc = (&diffopt =~ "icase")	
	let t:DChar.igs = (&diffopt =~ "iwhite")

	" set line and its highlight id record
	let t:DChar.mid = {}
	let [t:DChar.mid[1], t:DChar.mid[2]] = [{}, {}]

	" set highlighted lines and columns record
	let t:DChar.hlc = {}
	let [t:DChar.hlc[1], t:DChar.hlc[2]] = [{}, {}]

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
endfunction

function! s:ShowDiffChar(sl, el)
	" initialize when t:DChar is not defined
	if !exists("t:DChar") && s:InitializeDiffChar() == -1 | return | endif

	call s:RefreshDiffCharWin()

	let cwin = winnr()
	if cwin == t:DChar.win[1] | let k = 1
	elseif cwin == t:DChar.win[2] | let k = 2
	else | return | endif

	" set a possible DiffChar line list between sl and el
	if exists("t:DChar.vdl")	" diff mode
		let [d1, d2] = s:GetDiffModeLines(k, a:sl, a:el)
	else				" non-diff mode
		let [d1, d2] = [range(a:sl, a:el), range(a:sl, a:el)]
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
			let t{k} += getbufline(winbufnr(t:DChar.win[k]), d)
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
	let [lc1, lc2] = [{}, {}]

	" compare each line and trace difference units
	for n in range(n1)
		" split each line to the difference units
		let [u1, u2] =
			\[split(t1[n], t:DChar.spt), split(t2[n], t:DChar.spt)]

		" set unit lists for tracing
		let [u1t, u2t] = [copy(u1), copy(u2)]
		if t:DChar.igs
			" convert \s\+ to a single space and
			" remove/unlet the last \s\+$ on ignorespace mode
			for k in [1, 2]
				if !empty(u{k}t)
					call map(u{k}t, 'substitute
						\(v:val, "\\s\\+", " ", "g")')
					let u{k}t[-1] = substitute
						\(u{k}t[-1], '\s\+$', '', '')
					if empty(u{k}t[-1])
						unlet u{k}t[-1]
					endif
				endif
			endfor
		endif
		if u1t == u2t | continue | endif

		" get first same units out to trace
		let ns = min([len(u1t), len(u2t)])
		let fu = 0
		while fu < ns && u1t[fu] == u2t[fu]
			let fu += 1
		endwhile
		let [u1t, u2t] = [u1t[fu :], u2t[fu :]]

		" trace the actual diffference units
		let [c1, c2, h1, h2, p1, p2] = [[], [], [], [], fu, fu]
		let [l1, l2] = (fu == 0) ? [0, 0] :
						\[len(join(u1[: fu - 1], '')),
						\len(join(u2[: fu - 1], ''))]
		let [nc, na] = [0, 0]		" changed and added counts
		let [ex, nu] = ['', 0]
		for [ed, ut] in s:TraceDiffChar{t:DiffAlgorithm}(u1t, u2t)
						\+ [['=', ''], ['', '']]
			if ex == ed | let nu += 1 | continue | endif
			if ex == '='
				let ee = [empty(h1), empty(h2)]
				if ee == [0, 0]
					let [c1, c2] += [
						\[['c', nc, [h1[0], h1[-1]]]],
						\[['c', nc, [h2[0], h2[-1]]]]]
					let nc += 1
				elseif ee == [0, 1]
					let [c1, c2] += [
						\[['a', na, [h1[0], h1[-1]]]],
						\[['d', na, [l2, l2 + 1]]]]
					let na += 1
				elseif ee == [1, 0]
					let [c1, c2] += [
						\[['d', na, [l1, l1 + 1]]],
						\[['a', na, [h2[0], h2[-1]]]]]
					let na += 1
				endif
				let [h1, h2] = [[], []]
				let [l1, l2, p1, p2] += [
					\len(join(u1[p1 : p1 + nu], '')),
					\len(join(u2[p2 : p2 + nu], '')),
					\nu + 1, nu + 1]
			elseif ex == '-'
				let ul = len(join(u1[p1 : p1 + nu], ''))
				let [h1, l1, p1] +=
					\[[l1 + 1, l1 + ul], ul, nu + 1]
			elseif ex == '+'
				let ul = len(join(u2[p2 : p2 + nu], ''))
				let [h2, l2, p2] +=
					\[[l2 + 1, l2 + ul], ul, nu + 1]
			endif
			let [ex, nu] = [ed, 0]
		endfor

		" add different lines and columns to the list
		if !empty(c1) || !empty(c2)
			let [lc1[d1[n]], lc2[d2[n]]] = [c1, c2]
		endif
	endfor

	" restore ignorecase flag
	let &ignorecase = save_igc

	" highlight lines and columns and set events
	for k in [1, 2]
		exec t:DChar.win[k] . "wincmd w"
		call s:HighlightDiffChar(k, lc{k})
		if empty(t:DChar.hlc[k]) | continue | endif
		let buf = winbufnr(t:DChar.win[k])
		if !exists("#BufWinLeave#<buffer=" . buf . ">")
			exec "au BufWinLeave <buffer=" . buf .
				\"> call s:ResetDiffChar(1, line('$'))"
		endif
		if exists("t:DChar.lsv")
			if !exists("#TextChanged#<buffer=" . buf . ">")
				exec "au TextChanged <buffer=" . buf .
					\"> call s:UpdateDiffChar(" . k . ")"
			endif
			if !exists("#TextChangedI#<buffer=" . buf . ">")
				exec "au TextChangedI <buffer=" . buf .
					\"> call s:UpdateDiffChar(" . k . ")"
			endif
			let t:DChar.lsv[k] = s:GetLineStrValues(k)
		endif
	endfor
	exec cwin . "wincmd w"

	" reset t:DChar when no DiffChar highlightings in either buffer
	if empty(t:DChar.hlc[1]) || empty(t:DChar.hlc[2])
		call s:MarkDiffCharID(0)
		unlet t:DChar
	endif
endfunction

function! s:ResetDiffChar(sl, el)
	if !exists("t:DChar") | return | endif

	call s:RefreshDiffCharWin()

	let cwin = winnr()
	if cwin == t:DChar.win[1] | let k = 1
	elseif cwin == t:DChar.win[2] | let k = 2
	else | return | endif

	" set a possible DiffChar line list between sl and el
	if exists("t:DChar.vdl")	" diff mode
		let [d1, d2] = s:GetDiffModeLines(k, a:sl, a:el)
	else				" non-diff mode
		let [d1, d2] = [range(a:sl, a:el), range(a:sl, a:el)]
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

		exec t:DChar.win[k] . "wincmd w"
		call s:ClearDiffChar(k, d{k})

		" when no highlight exists, reset events
		if empty(t:DChar.hlc[k])
			let buf = winbufnr(t:DChar.win[k])
			exec "au! BufWinLeave <buffer=" . buf . ">"
			if exists("t:DChar.lsv")
				exec "au! TextChanged <buffer=" . buf . ">"
				exec "au! TextChangedI <buffer=" . buf . ">"
				unlet t:DChar.lsv[k]
			endif
			call s:ResetDiffCharPair(k)
		endif
	endfor
	exec cwin . "wincmd w"

	" reset t:DChar when no DiffChar highlightings in either buffer
	if empty(t:DChar.hlc[1]) || empty(t:DChar.hlc[2])
		call s:MarkDiffCharID(0)
		unlet t:DChar
	endif
endfunction

function! s:ToggleDiffChar(...)
	if a:0 == 1 | let sl = a:1 | let el = a:1
	elseif a:0 == 2 | let sl = a:1 | let el = a:2
	else | return | endif

	if exists("t:DChar")
		for l in range(sl, el)
			if has_key(t:DChar.hlc[1], l) ||
						\has_key(t:DChar.hlc[2], l)
				call s:ResetDiffChar(sl, el)
				return
			endif
		endfor
	endif
	call s:ShowDiffChar(sl, el)
endfunction

function! s:HighlightDiffChar(key, lnecol)
	for [l, enc] in items(a:lnecol)
		if has_key(t:DChar.mid[a:key], l) | continue | endif
		if exists("*matchaddpos")
			let mid = [matchaddpos("DiffChange", [[l]], 0)]
		else
			let dl = '\%' . l . 'l'
			let mid = [matchadd("DiffChange", dl . '.', 0)]
		endif
		for [e, n, c] in enc
			if e == 'd' | continue | endif
			let hl = (e == 'a') ?
				\"DiffAdd" : t:DChar.dmc[n % len(t:DChar.dmc)]
			if exists("*matchaddpos")
				let mid += [matchaddpos(hl,
					\[[l, c[0], c[-1] - c[0] + 1]], 0)]
			else
				let dc = '\%>' . (c[0] - 1) . 'c\%<' .
							\(c[-1] + 1) . 'c'
				let mid += [matchadd(hl, dl . dc, 0)]
			endif
		endfor
		let [t:DChar.mid[a:key][l], t:DChar.hlc[a:key][l]] = [mid, enc]
	endfor
endfunction

function! s:ClearDiffChar(key, lines)
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

function! s:UpdateDiffChar(key)
	" if number of lines was changed, reset all
	if len(t:DChar.lsv[a:key]) != line('$')
		call s:ResetDiffChar(1, line('$'))
		return
	endif

	" save the current t:DChar settings except highlightings
	let sdc = deepcopy(t:DChar)
	let [sdc.mid[1], sdc.mid[2]] = [{}, {}]
	let [sdc.hlc[1], sdc.hlc[2]] = [{}, {}]

	" update only highlighted and current changed lines
	let lsv = s:GetLineStrValues(a:key)
	for l in map(keys(t:DChar.hlc[a:key]), 'eval(v:val)')
		if lsv[l - 1] != t:DChar.lsv[a:key][l - 1]
			call s:ResetDiffChar(l, l)
			if !exists("t:DChar") | let t:DChar = sdc | endif
			call s:MarkDiffCharID(1)
			call s:ShowDiffChar(l, l)
		endif
	endfor
endfunction

function! s:GetDiffModeLines(key, sl, el)
	" in diff mode, need to compare the different line between windows
	" if current window is t:DChar.win[1], narrow sl <= t:DChar.vdl[1]
	" <= el and get the corresponding lines from t:DChar.vdl[2]
	let [i, j] = (a:key == 1) ? [1, 2] : [2, 1]
	let [d1, d2] = [copy(t:DChar.vdl[1]), copy(t:DChar.vdl[2])]
	for l in range(len(d{i}))
		if d{i}[l] < a:sl || a:el < d{i}[l]
			let [d{i}[l], d{j}[l]] = [-1, -1]
		endif
	endfor
	call filter(d1, 'v:val != -1')
	call filter(d2, 'v:val != -1')
	return [d1, d2]
endfunction

function! s:GetLineStrValues(key)
	let hl = map(keys(t:DChar.hlc[a:key]), 'eval(v:val)')
	let lsv = []
	for l in range(1, line('$'))
		if index(hl, l) != -1
			let str = getbufline(winbufnr(t:DChar.win[a:key]), l)[0]
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

function! s:JumpDiffChar(dir, pos)
	" dir : 1 = forward, else = backward
	" pos : 1 = start, else = end
	if !exists("t:DChar") | return | endif

	call s:RefreshDiffCharWin()

	let cwin = winnr()
	if cwin == t:DChar.win[1] | let k = 1
	elseif cwin == t:DChar.win[2] | let k = 2
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
						\winbufnr(cwin), l)[0],
						\'.', c - 1)) - 1
				endif
			else
				let c = a:dir ? 0 : 99999
			endif
			let hc = map(copy(t:DChar.hlc[k][l]),
						\'(v:val[0] == "d") ?
						\"" : v:val[2][a:pos ? 0 : -1]')
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
endfunction

function! s:ShowDiffCharPair(key, line, icol, pos)
	let m = (a:key == 1) ? 2 : 1
	if exists("t:DChar.vdl")	" diff mode
		let line = t:DChar.vdl[m][index(t:DChar.vdl[a:key], a:line)]
	else				" non-diff mode
		let line = a:line
	endif
	let bl = getbufline(winbufnr(t:DChar.win[m]), line)[0]

	let [e, n, c] = t:DChar.hlc[m][line][a:icol]
	if e == 'd'
		" deleted unit
		let pc = (0 < c[0]) ? split(bl[ : c[0] - 1], '\zs')[-1] : ""
		let nc = (c[0] < len(bl)) ? split(bl[c[0] : ], '\zs')[0] : ""
		" echo a-----b with DiffChange/DiffDelete
		echohl DiffChange
		echon pc
		echohl DiffDelete
		let col = t:DChar.hlc[a:key][a:line][a:icol][2]
		echon repeat('-', strwidth(
			\getbufline(winbufnr(t:DChar.win[a:key]), a:line)[0]
						\[col[0] - 1 : col[-1] - 1]))
		echohl DiffChange
		echon nc
		echohl None
		" set position/length for both side of deleted unit
		let clen = len(pc . nc)
		let cpos = c[0] - len(pc) + 1
	else
		" changed unit
		let dc = bl[c[0] - 1 : c[-1] - 1]
		" echo the matching unit with its color
		exec "echohl " . t:DChar.dmc[n % len(t:DChar.dmc)]
		echon dc
		echohl None
		" set position/length for matching unit
		let clen = len(split(dc, '\zs')[a:pos ? 0 : -1])
		let cpos = a:pos ? c[0] : c[-1] - clen + 1
	endif

	" show cursor on deleted unit or matching unit on another window
	exec t:DChar.win[m] . "wincmd w"
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
	if !exists("#WinEnter#<buffer=" . winbufnr(t:DChar.win[m]) . ">")
		exec "au WinEnter <buffer=" . winbufnr(t:DChar.win[m]) .
			\"> call s:ResetDiffCharPair(" . m . ")"
	endif
	exec t:DChar.win[a:key] . "wincmd w"
endfunction

function! s:ResetDiffCharPair(key)
	if exists("t:DChar.mpc[a:key]")
		call matchdelete(t:DChar.mpc[a:key])
		unlet t:DChar.mpc[a:key]
		exec "au! WinEnter <buffer=" .
					\winbufnr(t:DChar.win[a:key]) . ">"
		echon ""
	endif
endfunction

function! s:MarkDiffCharID(on)
	" mark w:DCharID (0/1/2) on all windows, on : 1 = set, 0 = clear
	for win in range(1, winnr('$'))
		call setwinvar(win, "DCharID",
			\(win == t:DChar.win[1]) ? a:on * 1 :
			\(win == t:DChar.win[2]) ? a:on * 2 : 0)
	endfor
endfunction

function! s:RefreshDiffCharWin()
	" find diffchar windows and set their winnr to t:DChar.win again
	let t:DChar.win = {}
	for win in range(1, winnr('$'))
		let id = getwinvar(win, "DCharID", 0)
		if id | let t:DChar.win[id] = win | endif
	endfor
endfunction

" O(NP) Difference algorithm
function! s:TraceDiffCharONP(u1, u2)
	let [n1, n2] = [len(a:u1), len(a:u2)]
	if n1 == 0 && n2 == 0 | return [] | endif

	" reverse to be N <= M, u2 <= u1
	let rev = (n1 > n2) ? 0 : (n1 < n2) ? 1 :
				\(join(a:u1, '') >= join(a:u2, '')) ? 0 : 1
	let [u1, u2, M, N] = rev ? [a:u2, a:u1, n2, n1] : [a:u1, a:u2, n1, n2]

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
			let [x, ed, pp, pk] =
				\(fp[k - 1 + offset] < fp[k + 1 + offset]) ?
				\[fp[k + 1 + offset],
					\'+', (r == 'A') ? p - 1 : p, k + 1] :
				\[fp[k - 1 + offset] + 1,
					\'-', (r == 'C') ? p - 1 : p, k - 1]
			let y = x - k
			while x < M && y < N && u1[x] == u2[y]
				let ed .= '='
				let [x, y] += [1, 1]
			endwhile
			let fp[k + offset] = x
			" add [ed, pp, pk] for current p and k
			let etree[p][p + k] = [ed, pp, pk]
		endfor
	endwhile

	" create an edit sequence back from last p and k
	let eseq = ''
	while p >= 0 && p + k >= 0
		let [e, p, k] = etree[p][p + k]
		let eseq = e . eseq
	endwhile
	let eseq = eseq[1:]		" delete the first entry

	" trace the edit sequence starting from [0, 0] and
	" create a shortest edit script (SES)
	let ses = []			" ['+/-/=', unit]
	let [i1, i2] = [0, 0]
	for n in range(len(eseq))
		let ed = eseq[n]
		if ed == '='
			let ses += [[ed, u1[i1]]]
			let [i1, i2] += [1, 1]
		elseif ed == '-'
			let ses += [[ed, u1[i1]]]
			let i1 += 1
		elseif ed == '+'
			let ses += [[ed, u2[i2]]]
			let i2 += 1
		endif
	endfor
	if rev				" reverse the edit
		call map(ses, '[tr(v:val[0], "+-", "-+"), v:val[1]]')
	endif

	return ses
endfunction

" O(ND) Difference algorithm
function! s:TraceDiffCharOND(u1, u2)	
	let [n1, n2] = [len(a:u1), len(a:u2)]
	if n1 == 0 && n2 == 0 | return [] | endif

	" reverse to be u2 <= u1
	let rev = (n1 > n2) ? 0 : (n1 < n2) ? 1 :
				\(join(a:u1, '') >= join(a:u2, '')) ? 0 : 1
	let [u1, u2, M, N] = rev ? [a:u2, a:u1, n2, n1] : [a:u1, a:u2, n1, n2]

	let offset = M + N
	let V = repeat([0], offset * 2 + 1)
	let etree = []		" [next edit, previous K's position]
	let found = 0

	for D in range(offset + 1)
		let etree += [[]]	" add a list for each D
		for k in range(-D, D, 2)
			let [x, ed, pk] = (k == -D || k != D &&
				\V[k - 1 + offset] < V[k + 1 + offset]) ?
				\[V[k + 1 + offset], '+', k + 1] :
				\[V[k - 1 + offset] + 1, '-', k - 1]
			let y = x - k
			while x < M && y < N && u1[x] == u2[y]
				let ed .= '='
				let [x, y] += [1, 1]
			endwhile
			let V[k + offset] = x
			" add [ed, pk] of k for [-D], [-D+2], ..., [D-2], [D]
			let etree[D] += [[ed, (pk + D - 1)/2]]
			" find the goal?
			if x >= M && y >= N | let found = 1 | break | endif
		endfor
		if found | break | endif	" break loop
	endfor

	" create an edit sequence back from last D
	let eseq = ''	
	let pk = -1
	for d in range(D, 0, -1)
		let [ed, pk] = etree[d][pk]
		let eseq = ed . eseq
	endfor
	let eseq = eseq[1:]		" delete the first entry

	" trace the edit sequence starting from [0, 0] and
	" create a shortest edit script (SES)
	let ses = []			" ['+/-/=', unit]
	let [i1, i2] = [0, 0]
	for n in range(len(eseq))
		let ed = eseq[n]
		if ed == '='
			let ses += [[ed, u1[i1]]]
			let [i1, i2] += [1, 1]
		elseif ed == '-'
			let ses += [[ed, u1[i1]]]
			let i1 += 1
		elseif ed == '+'
			let ses += [[ed, u2[i2]]]
			let i2 += 1
		endif
	endfor
	if rev				" reverse the edit
		call map(ses, '[tr(v:val[0], "+-", "-+"), v:val[1]]')
	endif

	return ses
endfunction

" Basic Difference algorithm
function! s:TraceDiffCharBasic(u1, u2)
	let [n1, n2] = [len(a:u1), len(a:u2)]
	if n1 == 0 && n2 == 0 | return [] | endif

	" reverse to be u2 <= u1
	let rev = (n1 > n2) ? 0 : (n1 < n2) ? 1 :
				\(join(a:u1, '') >= join(a:u2, '')) ? 0 : 1
	let [u1, u2, n1, n2] = rev ? [a:u2, a:u1, n2, n1] : [a:u1, a:u2, n1, n2]

	" initialize an edit graph [next edit, # of steps to goal]
	let egph = []
	for i1 in range(n1 + 1)
		let egph += [repeat([['', 0]], n2 + 1)]
	endfor

	" assign values in egph[] based on u1 and u2
	let egph[n1][n2] = ['*', 0]		" last point = goal
	for i1 in range(n1)			" last column's points
		let egph[i1][n2] = ['-', n1 - i1]
	endfor
	for i2 in range(n2)			" last row's points
		let egph[n1][i2] = ['+', n2 - i2]
	endfor
	for i1 in range(n1 - 1, 0, -1)		" other points from goal
		for i2 in range(n2 - 1, 0, -1)
			if u1[i1] == u2[i2]
				let egph[i1][i2] =
					\['=', egph[i1 + 1][i2 + 1][1]]
			else
				let [i1p, i2p] =
					\[egph[i1 + 1][i2], egph[i1][i2 + 1]]
				let egph[i1][i2] =
					\(i1p[1] < i2p[1]) ? ['-', i1p[1] + 1] :
					\(i1p[1] > i2p[1]) ? ['+', i2p[1] + 1] :
					\(i1p[0] == '=') ? ['-', i1p[1] + 1] :
					\(i2p[0] == '=') ? ['+', i2p[1] + 1] :
					\(i1p[0] == '-' && i2p[0] == '-') ?
					\['+', i2p[1] + 1] : ['-', i1p[1] + 1]
			endif
		endfor
	endfor

	" trace the next edit starting from [0, 0] and
	" create a shortest edit script (SES)
	let ses = []			" ['+/-/=', unit]
	let [i1, i2] = [0, 0]
	while 1
		let ed = egph[i1][i2][0]
		if ed == '='
			let ses += [[ed, u1[i1]]]
			let [i1, i2] += [1, 1]
		elseif ed == '-'
			let ses += [[ed, u1[i1]]]
			let i1 += 1
		elseif ed == '+'
			let ses += [[ed, u2[i2]]]
			let i2 += 1
		elseif ed == '*'
			break
		endif
	endwhile
	if rev				" reverse the edit
		call map(ses, '[tr(v:val[0], "+-", "-+"), v:val[1]]')
	endif

	return ses
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
