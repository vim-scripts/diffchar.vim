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
" This plugin has been using "An O(NP) Sequence Comparison Algorithm"
" developed by S.Wu, et al., which always finds an optimum sequence quickly.
" But for longer lines and less-similar files, it takes time to complete the
" diff tracing. At update 5.2, this plugin splits the tracing with the diff
" command. Firstly applies the internal O(NP) algorithm. If not completed
" within the time specified by a g:DiffSplitTime global (and tabpage)
" variable, continuously switches to the diff command at that point, and
" then joins both results. This approach provides a stable performance and
" reasonable accuracy, because the diff command effectively optimizes
" between them. The default of its variable is 500 ms, which would be useful
" for smaller files. If prefer to always apply the internal algorithm for
" accuracy (or the diff command for performance) only, set some large value
" (or 0) to the variable.
"
" Since update 4.7, this plugin has set the DiffCharExpr() to the diffexpr
" option, if it is empty. This function would be useful for smaller files.
" If the total number of lines on both diff windows <= 200, by default,
" it applies this plugin's internal algorithm to make the diff faster.
" And, by default, it initially shows the exact differences for all lines
" whenever diff mode begins. You can change these arguments of this function
" like "set diffexpr=DiffCharExpr(100, 0)", but if prefer not to use this
" enhancement, set 0 to g:DiffExpr.
"
" This plugin has been always positively supporting mulltibyte characters.
"
" Commands
" :[range]SDChar - Highlight difference units for [range]
" :[range]RDChar - Reset the highlight of difference units for [range]
"
" Configurable Keymaps
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
" Global Variables (and Tabpage variables when using t:)
" g:DiffUnit - type of difference unit
"     "Char"   : any single character (default)
"     "Word1"  : \w\+ word and any \W single character
"     "Word2"  : non-space and space words
"     "Word3"  : \< or \> character class boundaries
"     "CSV(,)" : separated by characters such as ',', ';', and '\t'
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
" g:DiffSplitTime - a time length (ms) to apply the internal algorithm first
"     0 ~ : (500 as default)
"
" DiffCharExpr(mxi, exd) function for the diffexpr option
"     mxi: the maximum number of total lines of both windows to apply internal
"          algorithm, apply diff command when more lines
"     exd: 1 = initially show exact differences, 0 = vim original ones
"
" Update : 5.2
" * Enhanced to provide a stable performance even for less-similar long files.
"   The new approach applies this plugin's algorithm first, and if not
"   completed within the specified time, continuously splits the tracing with
"   the diff command, and join both results.
" * Fixed: when diff command does not choose minimal algorithm and it shows
"   the equivalent lines as "changed", this plugin sometimes makes an error.
" * Fixed: if file encoding is not same as buffer encoding, a difference may
"   not be correctly detected in DiffCharExpr().
"
" Update : 5.1
" * Since vim 7.4.682, it has become impossible to overwrite the vim's diff
"   highlights with this plugin. Then, for example, DiffText bold typeface
"   will be left in all the diff highlighted lines (for more info, see
"   https://groups.google.com/forum/?hl=en_US#!topic/vim_use/1jQnbTva2fY).
"   This update provides a workaround to reduce its effect and to show the
"   differences mostly same as before.
"
" Update : 5.0
" * Significantly improved the way to trace and show the differences and
"   make them 1.5 ~ 2.0 times faster.
" * Introduced g:DiffMaxRatio (and t:DiffMaxRatio), a maximum difference
"   ratio to trace (100% as default). Once exceeds, the diff tracing is
"   recursively split and helps to keep performance instead of diff accuracy.
" * Discontinued other difference algorithms (OND and Basic) than the ONP,
"   then g:DiffAlgorithm no longer supported.
" * Improved to allow to specify one or more characters for "CSV(c)" in
"   g:DiffUnit (and t:DiffUnit). For example, "CSV(,:\t)" will split the
"   units by a comma, colon, and tab. Use '\\' for a backslash.
"
" Update : 4.9
" * Fixed DiffCharExpr() to check the number of total lines, not different
"   lines only, of both windows and apply either internal algorithm or
"   external diff command, in order to keep the appropriate performance
"   for large files.
"
" Update : 4.81
" * Enhanced to make DiffCharExpr() a bit faster by using uniq() or so.
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
" Last Change: 2015/08/15
" Created:
" Requires:
" Version: 5.2

if exists("g:loaded_diffchar")
	finish
endif
let g:loaded_diffchar = 5.2

let s:save_cpo = &cpo
set cpo&vim

" Commands
command! -range SDChar call s:ShowDiffChar(<line1>, <line2>)
command! -range RDChar call s:ResetDiffChar(<line1>, <line2>)

" Configurable Keymaps
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
" let g:DiffUnit = "CSV(,)"	" split characters
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

" Set a time length (ms) to apply this plugin's internal algorithm first
if !exists("g:DiffSplitTime")
let g:DiffSplitTime = 500	" if timeout, split to diff command
endif

" Set a diff expression if empty
if !exists("g:DiffExpr") || g:DiffExpr
if empty(&diffexpr)
let &diffexpr = "DiffCharExpr(200, 1)"	" set # of lines and show exact diffs
" let &diffexpr = "DiffCharExpr(0, 0)"	" apply ext cmd and show vim original
endif
endif

function! DiffCharExpr(mxi, exd)
	" read both files to be diff traced
	let [f1, f2] = [readfile(v:fname_in), readfile(v:fname_new)]

	" find the fist diff trial call and return here
	if [f1, f2] == [["line1"], ["line2"]]
		call writefile(["1c1"], v:fname_out)
		return
	endif

	" get a list of diff commands
	let dfcmd = (len(f1 + f2) <= a:mxi) ?
		\s:ApplyInternalAlgorithm(f1, f2) : s:ApplyDiffCommand()

	" write to output file
	call writefile(dfcmd, v:fname_out)

	" return if no need to show exact differences
	if exists("t:DChar") || !a:exd | return | endif

	" find 'c' command and extract the line to be changed
	let [c1, c2] = [[], []]
	for ct in filter(dfcmd, 'v:val =~ "c"')
		let cl = split(substitute(ct,
			\'^\(\d\+\)\%(,\(\d\+\)\)\=c\(\d\+\)\%(,\(\d\+\)\)\=$',
			\'\1 \2 \3 \4', ''), ' ', 1)
		let cn = min([empty(cl[1]) ? 0 : cl[1] - cl[0],
					\empty(cl[3]) ? 0 : cl[3] - cl[2]])
		let [c1, c2] += [range(cl[0], cl[0] + cn),
						\range(cl[2], cl[2] + cn)]
	endfor

	" return if no changed lines
	if empty(c1) | return | endif

	" initialize diffchar
	if s:InitializeDiffChar() == -1 | return | endif

	" try to find which window is for v:fname_in/v:fname_new
	let w = filter(range(1, winnr('$')), 'getwinvar(v:val, "&diff")')
	for w1 in w
		for w2 in w
			if w1 == w2 | continue | endif
			let [b1, b2] =
				\[getbufline(winbufnr(w1), c1[-1]),
				\getbufline(winbufnr(w2), c2[-1])]
			if b1 !=# b2 &&
				\b1 ==# [iconv(f1[c1[-1] - 1],
					\getwinvar(w1, "&fileencoding"),
					\getwinvar(w1, "&encoding"))] &&
				\b2 ==# [iconv(f2[c2[-1] - 1],
					\getwinvar(w2, "&fileencoding"),
					\getwinvar(w2, "&encoding"))]
				" found and set winnr and changed lines
				let [t:DChar.win[1], t:DChar.win[2]] = [w1, w2]
				let [t:DChar.vdl[1], t:DChar.vdl[2]] = [c1, c2]
				" highlight the changed lines
				call s:MarkDiffCharID(1)
				call s:ShowDiffChar(1, line('$'))
				return
			endif
		endfor
	endfor
	" not found and clear
	unlet t:DChar
endfunction

function! s:ApplyInternalAlgorithm(f1, f2)
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
	let [l1, l2] = [1, 1]

	for ed in split(s:TraceDiffChar(f1, f2), '\%(=\+\|[+-]\+\)\zs')
		let qn = len(ed)
		if ed[0] == '='		" one or more '='
			let [l1, l2] += [qn, qn]
		else			" one or more '[+-]'
			let q1 = len(escape(ed, '-')) - qn
			let q2 = qn - q1
			let dfcmd += [
				\((q1 == 0) ? (l1 - 1) : (q1 == 1) ?
					\l1 : l1 . ',' . (l1 + q1 - 1)) .
				\((q1 == 0) ? 'a' : (q2 == 0) ? 'd' : 'c') .
				\((q2 == 0) ? (l2 - 1) : (q2 == 1) ?
					\l2 : l2 . ',' . (l2 + q2 - 1))]
			let [l1, l2] += [q1, q2]
		endif
	endfor

	" restore ignorecase flag
	let &ignorecase = save_igc

	return dfcmd
endfunction

function! s:ApplyDiffCommand()
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
			let t:DChar.vdl[k] = filter(range(1, line('$')),
				\'index(dh, diff_hlID(v:val, 1)) != -1')
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
		let t:DChar.spt = t:DChar.igs ? '\%(\s\+\|.\)\zs' : '\zs'
	elseif t:DiffUnit == "Word1"	" \w\+ word and any \W character
		let t:DChar.spt = t:DChar.igs ? '\%(\s\+\|\w\+\|\W\)\zs' :
							\'\%(\w\+\|\W\)\zs'
	elseif t:DiffUnit == "Word2"	" non-space and space words
		let t:DChar.spt = '\%(\s\+\|\S\+\)\zs'
	elseif t:DiffUnit == "Word3"	" \< or \> boundaries
		let t:DChar.spt = '\<\|\>'
	elseif t:DiffUnit =~ '^CSV(.\+)$'	" split characters
		let s = escape(t:DiffUnit[4 : -2], '^-]')
		let t:DChar.spt = '\%([^'. s . ']\+\|[' . s . ']\)\zs'
	elseif t:DiffUnit =~ '^SRE(.\+)$'	" split regular expression
		let t:DChar.spt = t:DiffUnit[4 : -2]
	else
		let t:DChar.spt = t:DChar.igs ? '\%(\s\+\|.\)\zs' : '\zs'
		echo 'Not a valid difference unit type. Use "Char" instead.'
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
		for c in ["DiffAdd", "DiffDelete", "DiffChange", "DiffText",
				\hlexists("Cursor") ? "Cursor" : "MatchParen"]
			unlet h[index(h, c)]
		endfor
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

	" Set a time length (ms) to apply the internal algorithm first
	if !exists("t:DiffSplitTime")
		let t:DiffSplitTime = g:DiffSplitTime
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
		call filter(d{k}, 'index(hl, v:val) == -1')
		let u{k} = []
		for d in d{k}
			let u{k} += getbufline(winbufnr(t:DChar.win[k]), d)
		endfor
		let n{k} = len(u{k})
	endfor

	" remove redundant lines in either window
	if n1 > n2
		unlet u1[n2 - n1 :]
		unlet d1[n2 - n1 :]
		let n1 = n2
	elseif n1 < n2
		unlet u2[n1 - n2 :]
		unlet d2[n1 - n2 :]
		let n2 = n1
	endif

	" set ignorecase flag
	let save_igc = &ignorecase
	let &ignorecase = t:DChar.igc

	" remove equivalent lines
	for n in range(n1 - 1, 0, -1)
		if u1[n] == u2[n]
			unlet u1[n] | unlet d1[n]
			unlet u2[n] | unlet d2[n]
			let [n1, n2] -= [1, 1]
		endif
	endfor
	if n1 == 0
		let &ignorecase = save_igc
		unlet t:DChar
		return
	endif

	" a list of actual difference units for tracing
	call map(u1, 'split(v:val, t:DChar.spt)')
	call map(u2, 'split(v:val, t:DChar.spt)')

	" trace with this plugin's algorithm first,
	" if timeout, split to the diff command
	let cbx = s:TraceWithInternalAlgorithm(u1, u2)
	let cmp = empty(cbx) ? 0 : max(keys(cbx)) + 1
	if cmp < n1
		for [ln, cx] in
			\ items(s:TraceWithDiffCommand(u1[cmp :], u2[cmp :]))
			let cbx[ln + cmp] = cx
		endfor
	endif

	" a list of different lines and columns
	let [lc1, lc2] = [{}, {}]
	for [ln, cx] in items(cbx)
		let [lc1[d1[ln]], lc2[d2[ln]]] = [cx[0], cx[1]]
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
	elseif has("patch-7.4.682")
		call s:ToggleDiffTextHL(1)
	endif
endfunction

function! s:TraceWithInternalAlgorithm(u1, u2)
	" a list of commands with byte index per line
	let cbx = {}

	" start timer
	let st = reltime()

	" compare each line and trace difference units
	for ln in range(len(a:u1))
		" if timeout, break here
		if str2float(reltimestr(reltime(st))) >
						\t:DiffSplitTime / 1000.0
			break
		endif

		" set unit lists for tracing
		let [u1, u2] = [a:u1[ln], a:u2[ln]]
		let [u1t, u2t] = [u1, u2]

		" handle ignorespace option
		if t:DChar.igs
			for k in [1, 2]
				if !empty(u{k}t)
					" convert \s\+ to a single space
					call map(u{k}t, 'substitute
						\(v:val, "\\s\\+", " ", "g")')
					" remove/unlet the last \s\+$
					let u{k}t[-1] = substitute
						\(u{k}t[-1], '\s\+$', '', '')
					if empty(u{k}t[-1])
						unlet u{k}t[-1]
					endif
				endif
			endfor
		endif

		" skip diff tracing if no diff exists
		if u1t == u2t | continue | endif

		" start diff tracing
		let [c1, c2, p1, p2, l1, l2] = [[], [], 0, 0, 1, 1]
		for ed in split(s:TraceDiffChar(u1t, u2t),
						\'\%(=\+\|[+-]\+\)\zs')
			let qn = len(ed)
			if ed[0] == '='		" one or more '='
				let [l1, l2, p1, p2] += [
					\len(join(u1[p1 : p1 + qn - 1], '')),
					\len(join(u2[p2 : p2 + qn - 1], '')),
					\qn, qn]
			else			" one or more '[+-]'
				let q1 = len(escape(ed, '-')) - qn
				let q2 = qn - q1
				for k in [1, 2]
					if q{k} > 0
						let r = len(join(u{k}[
							\p{k} : p{k} + q{k} - 1
							\], ''))
						let h{k} = [l{k}, l{k} + r - 1]
						let [l{k}, p{k}] += [r, q{k}]
					else
						let h{k} = [l{k} - 1, l{k}]
					endif
				endfor
				let [e1, e2] = (q1 == 0) ? ['d', 'a'] :
					\(q2 == 0) ? ['a', 'd'] : ['c', 'c']
				let [c1, c2] += [[[e1, h1]], [[e2, h2]]]
			endif
		endfor

		if !empty(c1) || !empty(c2)
			let cbx[ln] = [c1, c2]
		endif
	endfor

	return cbx
endfunction

function! s:TraceWithDiffCommand(u1, u2)
	" prepare 2 input files for diff
	let [utd, lnb, lne, lns] = [':', '{', '}', '#####']
	for k in [1, 2]
		" insert its number + unit delimiter per unit, and
		" enclose the line with its number + begin/end symbols + id,
		" and append a line separator
		let v{k} = []
		for n in range(len(a:u{k}))
			let l = n + 1
			let v{k} += [l . lnb . k] +
				\map(copy(a:u{k}[n]), 'l . utd . v:val') +
						\[l . lne . k] + [lns]
		endfor
		let f{k} = tempname()
		call writefile(v{k}[:-2], f{k})
	endfor

	" call diff and get output as a list
	let opt = "-a --binary "
	if exists("g:DiffOptions") | let opt .= g:DiffOptions . " " | endif
	if t:DChar.igc | let opt .= "-i " | endif
	if t:DChar.igs | let opt .= "-b " | endif
	let dfo = split(system("diff " . opt . f1 . " " . f2), '\n')
	call delete(f1) | call delete(f2)

	" trace diff output and generate a list of diff's commands per line
	let dlc = {}

	let hml = 0		" one hunk takes multiple lines or not
	let [bkd, lnd] = [nr2char(0x1e), nr2char(0x1f)]
	call map(dfo, '((v:val =~ "^\\d\\+") ? bkd . lnd : lnd) . v:val')
	for db in split(join(dfo, ''), bkd)
		let dl = split(db, lnd)
		let dc = dl[0]
		let z1 = filter(copy(dl), 'v:val[0] == "<"')
		let z2 = filter(copy(dl), 'v:val[0] == ">"')

		" get an operation and line numbers from diff's command line
		let [s1, e1, cd, s2, e2] = split(substitute(dc,
			\'^\(\d\+\)\%(,\(\d\+\)\)\=\(.\)
				\\(\d\+\)\%(,\(\d\+\)\)\=$',
					\'\1 \2 \3 \4 \5', ''), ' ', 1)
		if empty(e1) | let e1 = s1 | endif
		if empty(e2) | let e2 = s2 | endif
		let [s1, e1, s2, e2] = [eval(s1), eval(e1), eval(s2), eval(e2)]
		let dx = [cd, [s1, e1], [s2, e2]]

		" check if one hunk takes signle line or not
		let ll = map(filter(z1 + z2, 'v:val !~ "^. " . lns'),
			\'substitute(v:val, "^. \\(\\d\\+\\).*$", "\\1", "")')

		" if single line, just copy
		if min(ll) == max(ll)
			let dlc[ll[0]] = get(dlc, ll[0], []) + [dx]
			continue
		endif

		" if multiple lines, separate by lines
		let hml += 1
		let lh = []
		for lx in sort(map(ll, 'printf("%8d", v:val)'))
			let lx = eval(lx)
			if index(lh, lx) == -1
				for k in [1, 2]
					let h{k} = len(filter(copy(z{k}),
						\'v:val =~ "^. " . lx . "\\D"'))
					let e{k} = s{k} + h{k} - 1
				endfor
				if h1 > 0 && h2 > 0
					let dx = ['c', [s1, e1], [s2, e2]]
					let [s1, s2] += [h1 + 1, h2 + 1]
				elseif h1 > 0 && h2 == 0
					let dx = ['d', [s1, e1], [s2, s2]]
					let s1 += h1 + 1
				elseif h1 == 0 && h2 > 0
					let dx = ['a', [s1, s1], [s2, e2]]
					let s2 += h2 + 1
				endif
				let dlc[lx] = get(dlc, lx, []) + [dx]
				let lh += [lx]
			endif
		endfor
	endfor

	" when a hunk takes multiple lines in this diff output,
	" find and merge continuous 'a+d' and 'd+a' to one 'c'
	if hml > 0
		for [ln, ds] in items(dlc)
			let cc = join(map(copy(ds), 'v:val[0]'), '')
			if stridx(cc, 'ad') == -1 && stridx(cc, 'da') == -1
				continue
			endif
			let dn = len(ds)
			for n in range(dn - 1)
				let [cdi, x1i, x2i] = ds[n]
				let [cdj, x1j, x2j] = ds[n + 1]
				let [d_a, a_d] = [[cdi, cdj] == ['d', 'a'],
						\[cdi, cdj] == ['a', 'd']]
				if d_a || a_d
					if x1i[1] >= x1j[0] &&
							\x1i[0] <= x1j[1] ||
						\x2i[1] >= x2j[0] &&
							\x2i[0] <= x2j[1]
						let ds[n] = d_a ?
							\['c', x1i, x2j] :
							\['c', x1j, x2i]
						unlet ds[n + 1]
					endif
				endif
			endfor
			if dn != len(ds) | let dlc[ln] = ds | endif
		endfor
	endif

	" generate a list of commands with byte index per line
	let cbx = {}

	for [ln, ds] in items(dlc)
		let ln -= 1
		let [c1, c2] = [[], []]

		let dn = len(ds)
		for n in range(dn)
			let [cd, x1, x2] = ds[n]
			let [s1, e1] = x1
			let [s2, e2] = x2

			" adjust the unit number and diff's command
			" (remove a count of line begin and end symbols)
			if n == 0
				let [bs1, bs2] = [s1 - 1, s2 - 1]
			endif
			let [s1, s2] -= (n == 0) ?
					\[bs1, bs2] : [bs1 + 1, bs2 + 1]
			let [e1, e2] -= (n == dn - 1) ?
					\[bs1 + 2, bs2 + 2] : [bs1 + 1, bs2 + 1]
			if cd == 'c'
				let [w1, w2] = [s1 > e1, s2 > e2]
				if [w1, w2] == [1, 1]
					continue
				elseif [w1, w2] == [1, 0]
					let cd = 'a'
					let s1 = e1
				elseif [w1, w2] == [0, 1]
					let cd = 'd'
					let s2 = e2
				endif
			endif
			let [r1, r2] = (cd == 'a') ? ['d', 'a'] :
					\(cd == 'd') ? ['a', 'd'] : ['c', 'c']

			" caluculate byte index from unit number
			for k in [1, 2]
				let [s{k}, e{k}] -= [1, 1]
				if r{k} == 'd'
					let s{k} = (s{k} < 0) ? 0 : len(join(
						\a:u{k}[ln][:s{k}], ''))
					let e{k} = s{k} + 1
				else
					let e{k} = len(join(
						\a:u{k}[ln][s{k} : e{k}], ''))
					let s{k} = (s{k} == 0) ? 1 : len(join(
						\a:u{k}[ln][:s{k} - 1], '')) + 1
					let e{k} += s{k} - 1
				endif
			endfor

			let [c1, c2] += [[[r1, [s1, e1]]], [[r2, [s2, e2]]]]
		endfor

		if !empty(c1) || !empty(c2)
			let cbx[ln] = [c1, c2]
		endif
	endfor

	return cbx
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
		call filter(d{k}, 'index(hl, v:val) != -1')

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
		if has("patch-7.4.682")
			call s:ToggleDiffTextHL(0)
		endif
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

function! s:HighlightDiffChar(key, lec)
	for [l, ec] in items(a:lec)
		if has_key(t:DChar.mid[a:key], l) | continue | endif
		if exists("*matchaddpos")
			let mid = [matchaddpos("DiffChange", [[l]], 0)]
		else
			let dl = '\%' . l . 'l'
			let mid = [matchadd("DiffChange", dl . '.', 0)]
		endif
		let n = 0
		for [e, c] in ec
			if e == 'c'
				let hl = t:DChar.dmc[n % len(t:DChar.dmc)]
				let n += 1
			elseif e == 'a' | let hl = "DiffAdd"
			else | continue
			endif
			if exists("*matchaddpos")
				let mid += [matchaddpos(hl,
					\[[l, c[0], c[1] - c[0] + 1]], 0)]
			else
				let dc = '\%>' . (c[0] - 1) . 'c\%<' .
							\(c[1] + 1) . 'c'
				let mid += [matchadd(hl, dl . dc, 0)]
			endif
		endfor
		let [t:DChar.mid[a:key][l], t:DChar.hlc[a:key][l]] = [mid, ec]
	endfor
endfunction

function! s:ClearDiffChar(key, lines)
	for l in a:lines
		if has_key(t:DChar.mid[a:key], l)
			call map(t:DChar.mid[a:key][l], 'matchdelete(v:val)')
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
						\'(v:val[0] == "d") ? "" :
						\v:val[1][a:pos ? 0 : 1]')
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

function! s:ShowDiffCharPair(key, line, col, pos)
	let m = (a:key == 1) ? 2 : 1
	if exists("t:DChar.vdl")	" diff mode
		let line = t:DChar.vdl[m][index(t:DChar.vdl[a:key], a:line)]
	else				" non-diff mode
		let line = a:line
	endif
	let bl = getbufline(winbufnr(t:DChar.win[m]), line)[0]

	let [e, c] = t:DChar.hlc[m][line][a:col]
	if e == 'd'
		" deleted unit
		let pc = (0 < c[0]) ? split(bl[ : c[0] - 1], '\zs')[-1] : ""
		let nc = (c[0] < len(bl)) ? split(bl[c[0] : ], '\zs')[0] : ""
		" echo a-----b with DiffChange/DiffDelete
		echohl DiffChange
		echon pc
		echohl DiffDelete
		let col = t:DChar.hlc[a:key][a:line][a:col][1]
		echon repeat('-', strwidth(
			\getbufline(winbufnr(t:DChar.win[a:key]), a:line)[0]
						\[col[0] - 1 : col[1] - 1]))
		echohl DiffChange
		echon nc
		echohl None
		" set position/length for both side of deleted unit
		let clen = len(pc . nc)
		let cpos = c[0] - len(pc) + 1
	else
		" changed unit
		let dc = bl[c[0] - 1 : c[1] - 1]
		" echo the matching unit with its color
		exec "echohl " . t:DChar.dmc[
			\(count(map(t:DChar.hlc[m][line][: a:col],
			\'v:val[0]'), 'c') - 1) % len(t:DChar.dmc)]
		echon dc
		echohl None
		" set position/length for matching unit
		let clen = len(split(dc, '\zs')[a:pos ? 0 : -1])
		let cpos = a:pos ? c[0] : c[1] - clen + 1
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

" "An O(NP) Sequence Comparison Algorithm"
" by S.Wu, U.Manber, G.Myers and W.Miller
function! s:TraceDiffChar(u1, u2)
	let [l1, l2] = [len(a:u1), len(a:u2)]
	if l1 == 0 && l2 == 0 | return ''
	elseif l1 == 0 | return repeat('+', l2)
	elseif l2 == 0 | return repeat('-', l1)
	endif

	" reverse to be M >= N
	let [M, N, u1, u2, e1, e2] = (l1 >= l2) ?
				\[l1, l2, a:u1, a:u2, '+', '-'] :
				\[l2, l1, a:u2, a:u1, '-', '+']

	let D = M - N
	let fp = repeat([-1], M + N + 3)
	let etree = []		" [next edit, previous p, previous k]

	let p = -1
	while fp[D] != M
		let p += 1
		let etree += [repeat([['', 0, 0]], p * 2 + D + 1)]
		for k in range(-p, D - 1, 1) + range(D + p, D + 1, -1) + [D]
			let [x, etree[p][k]] = (fp[k - 1] < fp[k + 1]) ?
				\[fp[k + 1],
					\[e1, k < D ? p - 1 : p, k + 1]] :
				\[fp[k - 1] + 1,
					\[e2, k > D ? p - 1 : p, k - 1]]
			let y = x - k
			while x < M && y < N && u1[x] == u2[y]
				let etree[p][k][0] .= '='
				let [x, y] += [1, 1]
			endwhile
			let fp[k] = x
		endfor
	endwhile

	" create a shortest edit script (SES) from last p and k
	let ses = ''
	while p != 0 || k != 0
		let [e, p, k] = etree[p][k]
		let ses = e . ses
	endwhile
	let ses = etree[p][k][0] . ses

	return ses[1:]		" remove the first entry
endfunction

if has("patch-7.4.682")
function! s:ToggleDiffTextHL(on)
	" no need in no-diff mode
	if !exists("t:DChar.vdl") | return | endif

	" number of tabpages where DiffText area has been overwritten
	let tn = len(filter(map(range(1, tabpagenr('$')),
			\'gettabvar(v:val, "DChar")'),
			\'!empty(v:val) && exists("v:val.dtm")'))

	if a:on
		call s:OverwriteDiffTextArea()
		if tn == 0
			" disable HL at the first ON among all tabpages
			let s:originalHL = &highlight
			let &highlight = join(filter(split(s:originalHL, ','),
				\'v:val[0] !~ "\\C[CT]"'), ',') . ",Cn,Tn"
		endif
	else
		call s:RestoreDiffTextArea()
		if tn == 1
			" restore HL at the last OFF among all tabpages
			let &highlight = s:originalHL
			unlet s:originalHL
		endif
	endif
endfunction

function! s:OverwriteDiffTextArea()
	" overwrite diff's DiffText area with DiffText match
	if exists("t:DChar.dtm") | return | endif
	let t:DChar.dtm = {}
	let dc = hlID("DiffChange") | let dt = hlID("DiffText")
	let cwin = winnr()
	for k in [1, 2]
		exec t:DChar.win[k] . "wincmd w"
		let t:DChar.dtm[k] = []
		for l in map(keys(t:DChar.hlc[k]), 'eval(v:val)')
			let h = t:DChar.hlc[k][l]
			let c1 = h[0][0] == 'd' ? h[0][1][1] : h[0][1][0]
			let c2 = h[-1][0] == 'd' ? h[-1][1][0] : h[-1][1][1]
			if c1 > c2 | continue | endif
			let t:DChar.dtm[k] += [matchaddpos("DiffChange",
								\[[l]], -1)]
			let t:DChar.dtm[k] += [matchaddpos("DiffText",
						\[[l, c1, c2 - c1 + 1]], -1)]
		endfor
	endfor
	exec cwin . "wincmd w"
endfunction

function! s:RestoreDiffTextArea()
	" delete all the overwritten DiffText matches
	if !exists("t:DChar.dtm") | return | endif
	let cwin = winnr()
	for k in [1, 2]
		exec t:DChar.win[k] . "wincmd w"
		call map(t:DChar.dtm[k], 'matchdelete(v:val)')
	endfor
	exec cwin . "wincmd w"
	unlet t:DChar.dtm
endfunction
endif

let &cpo = s:save_cpo
unlet s:save_cpo
