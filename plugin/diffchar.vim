" diffchar.vim - Highlight the exact differences, based on characters and words
"
" This plugin has been developed in order to make diff mode more useful. Vim
" highlights all the text in between the changed first and last characters
" on changed lines. But this plugin will find the exact differences between
" them, character by character - so called DiffChar.
"
" For example, in diff mode: ([DiffText], <DiffAdd>)
"
"    (file A) The [quick brown fox jumps over the lazy] dog.
"    (file B) The [lazy fox jumps over the quick brown] dog.
"
" this plugin will exactly highlight the changed and added units:
"
"    (file A) The [quick] <brown >fox jumps over the [lazy] dog.
"    (file B) The [lazy] fox jumps over the [quick] <brown >dog.
"
" This plugin can be triggered after diff mode starts by pressing <F7>/<F8>
" or using SDChar command. At update 5.5, a new global variable,
" g:DiffModeSync, is introduced. Its default is "enable" and synchronously
" show/reset the highlights of the exact differences as soon as the diff
" mode starts/ends. It also works on your custom diff tool (e.g. git-diff)
" when specified to the diffexpr option.
"
" In diff mode, this plugin compares the corresponding changed lines between
" two windows. In non-diff mode, it compares the same lines between them.
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
" between them. The default of its variable is 500 ms, which would be
" useful for smaller files. If prefer to always apply the internal algorithm
" for accuracy (or the diff command for performance) only, set some large
" value (or 0) to the variable.
"
" Since update 4.7, this plugin has set the DiffCharExpr() to the diffexpr
" option, if it is empty. this function would be rather useful for smaller
" files than the diff command. If the total number of lines on two windows
" <= 200, by default, this plugin's internal algorithm is used to make the
" diff faster. If prefer to leave the diffexpr option as empty, set 0 to
" g:DiffExpr.
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
"     1 : enable (default)
"     0 : disable
"       (notes : available on vim 7.4)
" g:DiffSplitTime - a time length (ms) to apply the internal algorithm first
"     0 ~ : (500 as default)
" g:DiffModeSync - synchronously show/reset with diff mode
"     1 : enable (default)
"     0 : disable
" g:DiffExpr - set DiffCharExpr() to the diffexpr potion
"     1 : enable (default)
"     0 : disable
"
" DiffCharExpr(mxi) function for the diffexpr option
"     mxi: the maximum number of total lines of both windows to apply internal
"          algorithm, apply diff command when more lines
"
" Update : 5.5
" * Introduced g:DiffModeSync to synchronously show/reset the highlights as
"   the diff mode starts/ends, which also works on your custom diff tool.
" * Changed to enable g:DiffUpdate as a default and then interactively update
"   the highlights while editing.
" * Enhanced to draw and delete the highlights faster by specifying as many
"   position parameters as possible in one matchaddpos() and matchadd() call.
" * Changed to select current window and next diff mode window (if present)
"   whose buffer is different at initialize.
" * Fixed:
"   - caused an error on getwinvar() in vim 7.3.
"   - in non-gVim, did not show a matching pair cursor when jumping the
"     cursor by using [e or ]b, depending on a color scheme.
"   - sometimes failed to toggle the highlights when using <F7> or <F8> in
"     diff mode windows.
"   - did not interactively update the highlight of all the lines when
"     multiple lines were changed at once if g:DiffUpdate = 1.
"
" Update : 5.4
" * Enhanced to show a position of a deleted unit with underline on its
"   previous and next characters. This position is where a unit is added
"   between those characters in another diffchar window.
" * Improved to be able to change this plugin's global variables anytime.
" * Changed to select current window and then the next (wincmd w) window
"   whose buffer is different.
"
" Update : 5.3
" * Performance improved for long lines and some defects fixed when the diff
"   command is used for the diff tracing.
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
"   - can select any of split windows as vim can do for diff
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
" Last Change: 2015/11/23
" Created:
" Requires:
" Version: 5.5

if exists("g:loaded_diffchar")
	finish
endif
let g:loaded_diffchar = 5.5

let s:save_cpo = &cpo
set cpo&vim

" Commands
command! -range SDChar call s:ShowDiffChar(range(<line1>, <line2>))
command! -range RDChar call s:ResetDiffChar(range(<line1>, <line2>))

" Configurable Keymaps
nnoremap <silent> <Plug>ToggleDiffCharAllLines
			\ :call <SID>ToggleDiffChar(range(1, line('$')))<CR>
nnoremap <silent> <Plug>ToggleDiffCharCurrentLine
			\ :call <SID>ToggleDiffChar([line('.')])<CR>
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
if exists("##TextChanged") && exists("##TextChangedI")
if !exists("g:DiffUpdate")
let g:DiffUpdate = 1		" enable
" let g:DiffUpdate = 0		" disable
endif
endif

" Set a time length (ms) to apply this plugin's internal algorithm first
if !exists("g:DiffSplitTime")
let g:DiffSplitTime = 500	" when timeout, split to diff command
" let g:DiffSplitTime = 0	" always apply diff command only
endif

" Set a diff mode synchronization to show/reset exact differences
if !exists("g:DiffModeSync")
let g:DiffModeSync = 1		" enable
" let g:DiffModeSync = 0	" disable
endif

" Set this plugin's DiffCharExpr() to the diffexpr option if empty
if !exists("g:DiffExpr")
let g:DiffExpr = 1		" enable
" let g:DiffExpr = 0		" disable
endif
if g:DiffExpr && empty(&diffexpr)
let &diffexpr = "DiffCharExpr(200)"	" use internal algo if <= 200 lines
" let &diffexpr = "DiffCharExpr(0)"	" use diff command only
endif

" Set an event group of this plugin
augroup dchar
au!
au! FilterWritePre * call s:SetDiffModeSync()
augroup END

function! DiffCharExpr(mxi)
	" read both files to be diff traced
	let [f1, f2] = [readfile(v:fname_in), readfile(v:fname_new)]

	" find the fist diff trial call and return here
	if [f1, f2] == [["line1"], ["line2"]]
		call writefile(["1c1"], v:fname_out)
		return
	endif

	" get a list of diff commands and write to output file
	call writefile(len(f1 + f2) > a:mxi ? s:ApplyDiffCommand() :
		\s:ApplyInternalAlgorithm(f1, f2), v:fname_out)
endfunction

function! s:SetDiffModeSync()
	if !g:DiffModeSync | return | endif

	" set current bufnr, actual diff winnr, actual diff bufnr
	let cbuf = bufnr('%')
	let dwin = filter(range(1, winnr('$')), 'getwinvar(v:val, "&diff")')
	let dbuf = map(copy(dwin), 'winbufnr(v:val)')

	" reset bufnr record
	if exists("s:save_dex")
		" diffexpr was not invoked in last prepare, meaning it was
		" the first event in one diff session, reset except last one
		if len(s:diffbuf) > 1 | unlet s:diffbuf[:-2] | endif
	endif
	if !exists("s:diffbuf") || index(s:diffbuf, cbuf) != -1 ||
		\!empty(filter(copy(s:diffbuf), 'index(dbuf, v:val) == -1'))
		" this is the first event in one diff session, reset all
		let s:diffbuf = []
	endif

	" append current bufnr where the event happens
	let s:diffbuf += [cbuf]

	" run each time when event happens on 2 buffers among one session
	if len(s:diffbuf) > 1
		" find winnr of v:fname_in(s:diffbuf[0]) and v:fname_new([-1])
		let win = {}
		let cwin = winnr()
		for k in [1, 2]
			let w = filter(copy(dwin), 'winbufnr(v:val) ==
						\s:diffbuf[k == 1 ? 0 : -1]')
			let win[k] = (len(w) > 1 &&
					\index(w, cwin) != -1) ? cwin : w[0]
		endfor

		" prepare diffexpr to be called soon
		if !exists("s:save_dex")
			let s:save_dex = &diffexpr
		endif
		let &diffexpr = "DiffModeSyncExpr(" . string(win) . ")"
	endif
endfunction

function! DiffModeSyncExpr(win)
	" call saved diffexpr or diff command if empty
	call eval(empty(s:save_dex) ? "DiffCharExpr(0)" : s:save_dex)

	" clear current diffchar highlights if present
	if exists("t:DChar")
		call s:RefreshDiffCharWID()
		let cwin = winnr()
		if index(values(t:DChar.win), cwin) != -1
			call s:ResetDiffChar(range(1, line('$')))
		else
			let save_ei = &eventignore | let &eventignore = "all"
			exec t:DChar.win[1] . "wincmd w"
			call s:ResetDiffChar(range(1, line('$')))
			exec cwin . "wincmd w"
			let &eventignore = save_ei
		endif
	endif

	" find 'c' command and extract the line to be changed
	let [c1, c2] = [[], []]
	for ct in filter(readfile(v:fname_out), 'v:val =~ "^\\d.*c"')
		let [p1, p2] = map(split(ct, 'c'), 'split(v:val, ",")')
		let cn = min([len(p1) == 1 ? 0 : p1[1] - p1[0],
					\len(p2) == 1 ? 0 : p2[1] - p2[0]])
		let [c1, c2] += [range(p1[0], p1[0] + cn),
						\range(p2[0], p2[0] + cn)]
	endfor

	" if there are changed lines and initialize successes
	if !empty(c1) && !empty(c2) && s:InitializeDiffChar() != -1
		" change window numbers and diff lines
		let t:DChar.win = a:win
		let t:DChar.vdl = {1: c1, 2: c2}

		" highlight the diff changed lines
		call s:MarkDiffCharWID(1)
		let cwin = winnr()
		if index(values(t:DChar.win), cwin) != -1
			call s:ShowDiffChar(range(1, line('$')))
		else
			let save_ei = &eventignore | let &eventignore = "all"
			exec t:DChar.win[1] . "wincmd w"
			call s:ShowDiffChar(range(1, line('$')))
			exec cwin . "wincmd w"
			let &eventignore = save_ei
		endif
	endif

	" resume back to the original diffexpr
	if exists("s:save_dex")
		let &diffexpr = s:save_dex
		unlet s:save_dex
	endif
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
	" return diff commands only
	return filter(split(
		\system("diff " . opt . v:fname_in . " " . v:fname_new),
						\'\n'), 'v:val =~ "^\\d"')
endfunction

function! s:InitializeDiffChar()
	if min(tabpagebuflist()) == max(tabpagebuflist())
		echo "Need more buffers displayed on this tab page!"
		return -1
	endif

	" define a DiffChar dictionary on this tab page
	let t:DChar = {}

	" select current window and next (diff mode if available) window
	" whose buffer is different
	let t:DChar.win = {}
	let cwin = winnr()
	let nwin = -1
	for w in range(cwin + 1, winnr('$')) + range(1, cwin - 1)
		if winbufnr(w) != winbufnr(cwin)
			if nwin == -1 | let nwin = w | endif
			if getwinvar(w, "&diff")
				let nwin = w
				break
			endif
		endif
	endfor
	let [t:DChar.win[1], t:DChar.win[2]] = [cwin, nwin]
	call s:MarkDiffCharWID(1)

	" set highlight groups used for diffchar on this tab page
	let t:DChar.dhl = {"A": "DiffAdd", "C": "DiffChange",
		\"D": "DiffDelete", "T": "DiffText", "Z": "_DiffDelPos",
		\"U": has("gui_running") ? "Cursor" : "CursorColumn"}

	" find corresponding DiffChange/DiffText lines on diff mode windows
	let t:DChar.vdl = {}
	let dh = [hlID(t:DChar.dhl.C), hlID(t:DChar.dhl.T)]
	let save_ei = &eventignore | let &eventignore = "all"
	for k in [1, 2]
		if getwinvar(t:DChar.win[k], "&diff")
			exec t:DChar.win[k] . "wincmd w"
			let t:DChar.vdl[k] = filter(range(1, line('$')),
				\'index(dh, diff_hlID(v:val, 1)) != -1')
			if empty(t:DChar.vdl[k])
				unlet t:DChar.vdl
				break
			endif
		else
			unlet t:DChar.vdl
			break
		endif
	endfor
	exec cwin . "wincmd w"
	let &eventignore = save_ei

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
	let du = exists("t:DiffUnit") ? t:DiffUnit : g:DiffUnit
	if du == "Char"		" any single character
		let t:DChar.usp = t:DChar.igs ? '\%(\s\+\|.\)\zs' : '\zs'
	elseif du == "Word1"	" \w\+ word and any \W character
		let t:DChar.usp = t:DChar.igs ? '\%(\s\+\|\w\+\|\W\)\zs' :
							\'\%(\w\+\|\W\)\zs'
	elseif du == "Word2"	" non-space and space words
		let t:DChar.usp = '\%(\s\+\|\S\+\)\zs'
	elseif du == "Word3"	" \< or \> boundaries
		let t:DChar.usp = '\<\|\>'
	elseif du =~ '^CSV(.\+)$'	" split characters
		let s = escape(du[4 : -2], '^-]')
		let t:DChar.usp = '\%([^'. s . ']\+\|[' . s . ']\)\zs'
	elseif du =~ '^SRE(.\+)$'	" split regular expression
		let t:DChar.usp = du[4 : -2]
	else
		let t:DChar.usp = t:DChar.igs ? '\%(\s\+\|.\)\zs' : '\zs'
		echo 'Not a valid difference unit type. Use "Char" instead.'
	endif

	" set a difference unit updating on this tab page
	" and a record of line values and number of total lines
	if exists("##TextChanged") && exists("##TextChangedI")
		if exists("t:DiffUpdate") ? t:DiffUpdate : g:DiffUpdate
			let t:DChar.lsv = {}
			let [t:DChar.lsv[1], t:DChar.lsv[2]] = [{}, {}]
		endif
	endif

	" Set a time length (ms) to apply the internal algorithm first
	let t:DChar.slt = exists("t:DiffSplitTime") ?
				\t:DiffSplitTime : g:DiffSplitTime

	" set a matching pair cursor id on this tab page
	let t:DChar.pci = {}

	" set a difference matching colors on this tab page
	let dc = exists("t:DiffColors") ? t:DiffColors : g:DiffColors
	let t:DChar.dmc = [t:DChar.dhl.T]
	if dc == 1
		let t:DChar.dmc += ["NonText", "Search", "VisualNOS"]
	elseif dc == 2
		let t:DChar.dmc += ["NonText", "Search", "VisualNOS",
			\"ErrorMsg", "MoreMsg", "TabLine", "Title"]
	elseif dc == 3
		let t:DChar.dmc += ["NonText", "Search", "VisualNOS",
			\"ErrorMsg", "MoreMsg", "TabLine", "Title",
			\"StatusLine", "WarningMsg", "Conceal", "SpecialKey",
			\"ColorColumn", "ModeMsg", "SignColumn", "Question"]
	elseif dc == 100
		redir => hl | silent highlight | redir END
		let h = map(filter(split(hl, '\n'),
			\'v:val =~ "^\\S" && v:val =~ "="'), 'split(v:val)[0]')
		for c in values(t:DChar.dhl)
			let i = index(h, c) | if i != -1 | unlet h[i] | endif
		endfor
		while !empty(h)
			let r = localtime() % len(h)
			let t:DChar.dmc += [h[r]] | unlet h[r]
		endwhile
	endif

	" define a specific highlight group to show a position
	" of a deleted unit, _DiffDelPos = DiffChange +/- underline
	exec "silent highlight clear " . t:DChar.dhl.Z
	" get current DiffChange
	redir => hl | exec "silent highlight " . t:DChar.dhl.C | redir END
	let ha = {}
	for [ky, ag] in map(filter(split(hl, '\%(\n\|\s\)\+'),
				\'v:val =~ "="'), 'split(v:val, "=")')
		let ha[ky] = ag
	endfor
	" add or delete a specific attribute (underline)
	let at = "underline"
	let hm = has("gui_running") ? "gui" : &t_Co > 1 ? "cterm" : "term"
	let ha[hm] = !exists("ha[hm]") ? at :
			\match(ha[hm], at) == -1 ? ha[hm] . ',' . at :
			\substitute(ha[hm], at . ',\=\|,\=' . at, '', '')
	" set as a highlight
	exec "silent highlight " . t:DChar.dhl.Z . " " .
			\join(values(map(filter(ha, '!empty(v:val)'),
						\'v:key . "=" . v:val')))
endfunction

function! s:ShowDiffChar(lines)
	" initialize when t:DChar is not defined
	if !exists("t:DChar") && s:InitializeDiffChar() == -1 | return | endif

	" refresh window number of diffchar windows
	call s:RefreshDiffCharWID()

	" return if current window is not either of diffchar windows
	let cwin = winnr()
	for k in [1, 2, 0]
		if k == 0 | return | endif
		if t:DChar.win[k] == cwin | break | endif
	endfor

	" set a possible DiffChar line list among a:lines
	let [d1, d2] = exists("t:DChar.vdl") ?
		\s:DiffModeLines(k, a:lines) : [copy(a:lines), copy(a:lines)]

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
		unlet u1[n2 - n1 :] | unlet d1[n2 - n1 :] | let n1 = n2
	elseif n1 < n2
		unlet u2[n1 - n2 :] | unlet d2[n1 - n2 :] | let n2 = n1
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
		if empty(t:DChar.hlc[1]) || empty(t:DChar.hlc[2])
			call s:MarkDiffCharWID(0)
			unlet t:DChar
		endif
		let &ignorecase = save_igc
		return
	endif

	" a list of actual difference units for tracing
	call map(u1, 'split(v:val, t:DChar.usp)')
	call map(u2, 'split(v:val, t:DChar.usp)')

	" a list of different lines and columns
	let [lc1, lc2] = [{}, {}]
	let cmp = 0
	for fn in ["TraceWithInternalAlgorithm", "TraceWithDiffCommand"]
		" trace with this plugin's algorithm first,
		" if timeout, split to the diff command
		for [ln, cx] in items(s:{fn}(u1[cmp :], u2[cmp :]))
			let [lc1[d1[cmp + ln]], lc2[d2[cmp + ln]]] =
							\[cx[0], cx[1]]
		endfor
		let cmp = len(lc1)
		if cmp >= n1 | break | endif
	endfor

	" restore ignorecase flag
	let &ignorecase = save_igc

	let buf = {}

	" highlight lines and columns
	let save_ei = &eventignore | let &eventignore = "all"
	for k in [1, 2]
		let buf[k] = winbufnr(t:DChar.win[k])
		exec t:DChar.win[k] . "wincmd w"
		call s:HighlightDiffChar(k, lc{k})

		if exists("t:DChar.lsv")
			call extend(t:DChar.lsv[k], s:LinesValues(k,
				\map(keys(lc{k}), 'eval(v:val)')))
			let t:DChar.lsv[k][0] = line('$')
		endif
	endfor
	exec cwin . "wincmd w"
	let &eventignore = save_ei

	if empty(t:DChar.hlc[1]) || empty(t:DChar.hlc[2])
		call s:MarkDiffCharWID(0)
		unlet t:DChar
		return
	endif

	" set events in each buffer
	for k in [1, 2]
		exec "au! dchar BufWinLeave <buffer=" . buf[k] .
				\"> call s:ResetDiffChar(range(1, line('$')))"
	endfor
	if exists("t:DChar.lsv")
		for k in [1, 2]
			exec "au! dchar TextChanged,TextChangedI <buffer=" .
				\buf[k] . "> call s:UpdateDiffChar(" . k . ")"
		endfor
	endif
	if g:DiffModeSync && exists("t:DChar.vdl")
		for k in [1, 2]
			exec "au! dchar CursorHold <buffer=" . buf[k] .
				\"> call s:ResetSwitchDiffModeSync(" . k . ")"
		endfor
		if !exists("s:save_ut") && len(filter(range(1, tabpagenr('$')),
				\'!empty(gettabvar(v:val, "DChar"))')) == 1
			let s:save_ut = &updatetime
			let &updatetime = 1
		endif
	endif

	if has("patch-7.4.682")
		call s:ToggleDiffHL(1)
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
		if str2float(reltimestr(reltime(st))) > t:DChar.slt / 1000.0
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
		let [c1, c2] = [[], []]
		let [l1, l2, p1, p2] = [1, 1, 0, 0]
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
				let [r1, r2] = (q1 == 0) ? ['d', 'a'] :
					\(q2 == 0) ? ['a', 'd'] : ['c', 'c']
				let [c1, c2] += [[[r1, h1]], [[r2, h2]]]
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
	if t:DChar.igc | let opt .= "-i " | endif
	if t:DChar.igs | let opt .= "-b " | endif
	let dfo = split(system("diff " . opt . f1 . " " . f2), '\n')
	call delete(f1) | call delete(f2)

	" trace diff output and generate a list of diff's commands per line
	let dlc = {}

	let hda = []	" a hunk takes multiple lines and includes 'd' or 'a'
	let [bkd, lnd] = [nr2char(0x1e), nr2char(0x1f)]
	call map(dfo, '((v:val =~ "^\\d\\+") ? bkd . lnd : lnd) . v:val')
	for db in split(join(dfo, ''), bkd)
		let dl = split(db, lnd)
		let dc = dl[0]
		let z1 = filter(copy(dl), 'v:val[0] == "<"')
		let z2 = filter(copy(dl), 'v:val[0] == ">"')

		" get an operation and line numbers from diff's command line
		let [se1, acd, se2] = map(split(substitute(
			\dc, '[acd]', '|&|', ''), '|'), 'split(v:val, ",")')
		let [s1, s2] = [eval(se1[0]), eval(se2[0])]
		let [e1, e2] = [len(se1) == 1 ? s1 : eval(se1[1]),
					\len(se2) == 1 ? s2 : eval(se2[1])]
		let dx = acd + [[s1, e1]] + [[s2, e2]]

		" check if one hunk takes signle line or not
		let ll = map(filter(z1 + z2, 'v:val !~ "^. " . lns'),
			\'substitute(v:val, "^. \\(\\d\\+\\).*$", "\\1", "")')

		" if single line, just copy
		if min(ll) == max(ll)
			let dlc[ll[0]] = get(dlc, ll[0], []) + [dx]
			continue
		endif

		" if multiple lines, separate by lines
		let lh = []
		for lx in sort(map(ll, 'printf("%8d", v:val)'))
			let lx = eval(lx)
			if index(lh, lx) == -1
				for k in [1, 2]
					let n{k} = len(filter(copy(z{k}),
						\'v:val =~ "^. " . lx . "\\D"'))
					let e{k} = s{k} + n{k} - 1
				endfor
				if n1 > 0 && n2 > 0
					let dx = ['c', [s1, e1], [s2, e2]]
					let [s1, s2] = [e1 + 2, e2 + 2]
				else
					if n1 > 0 && n2 == 0
						let dx = ['d', [s1, e1], []]
						let s1 = e1 + 2
					elseif n1 == 0 && n2 > 0
						let dx = ['a', [], [s2, e2]]
						let s2 = e2 + 2
					endif
					if index(hda, lx) == -1
						let hda += [lx]
					endif
				endif
				let dlc[lx] = get(dlc, lx, []) + [dx]
				let lh += [lx]
			endif
		endfor
	endfor

	" merge continuous 'a+d' and 'd+a' to one 'c'
	for ln in hda
		let ds = dlc[ln]
		let dn = len(ds)
		for n in range(dn - 1)
			if empty(ds[n][2]) && empty(ds[n + 1][1])
				let ds[n] = ['c', ds[n][1], ds[n + 1][2]]
				unlet ds[n + 1]
			elseif empty(ds[n][1]) && empty(ds[n + 1][2])
				let ds[n] = ['c', ds[n + 1][1], ds[n][2]]
				unlet ds[n + 1]
			endif
		endfor
		if dn != len(ds) | let dlc[ln] = ds | endif
	endfor

	" generate a list of commands with byte index per line
	let cbx = {}

	for [ln, ds] in items(dlc)
		let ln -= 1
		let [c1, c2] = [[], []]
		let [l1, l2, p1, p2] = [1, 1, 0, 0]

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
					if s{k} >= 0
						let l{k} += len(join(a:u{k}[ln]
							\[p{k} : s{k}], ''))
					endif
					let h{k} = [l{k} - 1, l{k}]
					let p{k} = e{k} + 1
				else
					if s{k} > 0
						let l{k} += len(join(a:u{k}[ln]
							\[p{k} : s{k} - 1], ''))
					endif
					let r = len(join(a:u{k}[ln]
							\[s{k} : e{k}], ''))
					let h{k} = [l{k}, l{k} + r - 1]
					let l{k} += r
					let p{k} = e{k} + 1
				endif
			endfor

			let [c1, c2] += [[[r1, h1]], [[r2, h2]]]
		endfor

		if !empty(c1) || !empty(c2)
			let cbx[ln] = [c1, c2]
		endif
	endfor

	return cbx
endfunction

function! s:ResetDiffChar(lines)
	if !exists("t:DChar") | return | endif

	" refresh window number of diffchar windows
	call s:RefreshDiffCharWID()

	" return if current window is not either of diffchar windows
	let cwin = winnr()
	for k in [1, 2, 0]
		if k == 0 | return | endif
		if t:DChar.win[k] == cwin | break | endif
	endfor

	" set a possible DiffChar line list among a:lines
	let [d1, d2] = exists("t:DChar.vdl") ?
		\s:DiffModeLines(k, a:lines) : [copy(a:lines), copy(a:lines)]

	let buf = {}

	" remove not highlighted lines
	let save_ei = &eventignore | let &eventignore = "all"
	for k in [1, 2]
		let hl = map(keys(t:DChar.hlc[k]), 'eval(v:val)')
		call filter(d{k}, 'index(hl, v:val) != -1')

		let buf[k] = winbufnr(t:DChar.win[k])
		exec t:DChar.win[k] . "wincmd w"
		call s:ClearDiffChar(k, d{k})
		call s:ResetDiffCharPair(k)

		if exists("t:DChar.lsv")
			call map(d{k}, 'remove(t:DChar.lsv[k], v:val)')
		endif
	endfor
	exec cwin . "wincmd w"
	let &eventignore = save_ei

	if !empty(t:DChar.hlc[1]) && !empty(t:DChar.hlc[2])
		return
	endif

	" reset events and all when no highlight exists
	for k in [1, 2]
		exec "au! dchar BufWinLeave <buffer=" . buf[k] . ">"
	endfor
	if exists("t:DChar.lsv")
		for k in [1, 2]
			exec "au! dchar TextChanged,TextChangedI <buffer=" .
								\buf[k] . ">"
		endfor
	endif
	if exists("t:DChar.vdl")
		for k in [1, 2]
			exec "au! dchar CursorHold <buffer=" . buf[k] . ">"
		endfor
		if exists("s:save_ut") && len(filter(range(1, tabpagenr('$')),
				\'!empty(gettabvar(v:val, "DChar"))')) == 1
			let &updatetime = s:save_ut
			unlet s:save_ut
		endif
	endif

	if has("patch-7.4.682")
		call s:ToggleDiffHL(0)
	endif
	call s:MarkDiffCharWID(0)
	unlet t:DChar
endfunction

function! s:ToggleDiffChar(lines)
	if exists("t:DChar")
		call s:RefreshDiffCharWID()
		for k in [1, 2, 0]
			if k == 0 | return | endif
			if t:DChar.win[k] == winnr() | break | endif
		endfor
		for hl in keys(t:DChar.hlc[k])
			if index(a:lines, eval(hl)) != -1
				call s:ResetDiffChar(a:lines)
				return
			endif
		endfor
	endif
	call s:ShowDiffChar(a:lines)
endfunction

function! s:HighlightDiffChar(key, lec)
	for [l, ec] in items(a:lec)
		if has_key(t:DChar.mid[a:key], l) | continue | endif
		let t:DChar.hlc[a:key][l] = ec

		" collect all the column positions per highlight group
		let ap = {}
		let cn = 0
		for [e, c] in ec
			if e == 'c'
				let hl = t:DChar.dmc[cn % len(t:DChar.dmc)]
				let cn += 1
			elseif e == 'a'
				let hl = t:DChar.dhl.A
			elseif e == 'd'
				let hl = t:DChar.dhl.Z
				let bl = getbufline(
					\winbufnr(t:DChar.win[a:key]), l)[0]
				let c = [c[0] - (0 < c[0] ? len(split(
					\bl[:c[0] - 1], '\zs')[-1]) : 0) + 1,
					\c[1] + (c[1] <= len(bl) ? len(split(
					\bl[c[1] - 1:], '\zs')[0]) : 0) - 1]
			endif
			let ap[hl] = get(ap, hl, []) + [c]
		endfor

		" do highlightings on all the lines and columns
		" with minimum matchaddpos() or one matchadd() call
		if exists("*matchaddpos")
			let t:DChar.mid[a:key][l] =
				\[matchaddpos(t:DChar.dhl.C, [[l]], 0)]
			for [hl, cp] in items(ap)
				call map(cp, '[l, v:val[0],
						\v:val[1] - v:val[0] + 1]')
				while !empty(cp)
					let t:DChar.mid[a:key][l] +=
						\[matchaddpos(hl, cp[:7], 0)]
					unlet cp[:7]
				endwhile
			endfor
		else
			let dl = '\%' . l . 'l'
			let t:DChar.mid[a:key][l] =
				\[matchadd(t:DChar.dhl.C, dl . '.', 0)]
			for [hl, cp] in items(ap)
				call map(cp, '"\\%>" . (v:val[0] - 1) .
					\"c\\%<" . (v:val[1] + 1) . "c"')
				let dc = len(cp) > 1 ?
					\'\%(' . join(cp, '\|') . '\)' : cp[0]
				let t:DChar.mid[a:key][l] +=
						\[matchadd(hl, dl . dc, 0)]
			endfor
		endif
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
	call s:RefreshDiffCharWID()
	let cwin = winnr()
	if cwin != t:DChar.win[a:key]
		let save_ei = &eventignore | let &eventignore = "all"
		exec t:DChar.win[a:key] . "wincmd w"
	endif

	" if number of lines was changed, reset all
	if t:DChar.lsv[a:key][0] != line('$')
		call s:ResetDiffChar(
			\range(1, max([t:DChar.lsv[a:key][0], line('$')])))
		return
	endif

	" save the current t:DChar settings except highlightings
	let sdc = deepcopy(t:DChar)
	let [sdc.mid[1], sdc.mid[2]] = [{}, {}]
	let [sdc.hlc[1], sdc.hlc[2]] = [{}, {}]
	if exists("sdc.dtm") | unlet sdc.dtm | endif
	if exists("sdc.lsv") | let [sdc.lsv[1], sdc.lsv[2]] = [{}, {}] | endif

	" update only highlighted and current changed lines
	let hl = map(keys(t:DChar.hlc[a:key]), 'eval(v:val)')
	let lsv = s:LinesValues(a:key, hl)
	let chl = filter(hl, 'lsv[v:val] != t:DChar.lsv[a:key][v:val]')

	call s:ResetDiffChar(chl)
	if !exists("t:DChar") | let t:DChar = sdc | endif
	call s:MarkDiffCharWID(1)

	call s:ShowDiffChar(chl)

	if exists("save_ei")
		exec cwin . "wincmd w"
		let &eventignore = save_ei
	endif
endfunction

function! s:ResetSwitchDiffModeSync(key)
	" when diff mode turns off on the current window, reset it
	if &diff | return | endif

	call s:RefreshDiffCharWID()

	let cwin = winnr()
	if cwin != t:DChar.win[a:key] | return | endif

	let [win, vdl] = [t:DChar.win, t:DChar.vdl]

	call s:ResetDiffChar(range(1, line('$')))

	" switch to another diff mode window of the same buffer if present
	let bwin = filter(range(1, winnr('$')),
		\'winbufnr(v:val) == bufnr("%") && getwinvar(v:val, "&diff")')
	if !empty(bwin) && s:InitializeDiffChar() != -1
		let t:DChar.win = map(win, 'v:key == a:key ? bwin[0] : v:val')
		let t:DChar.vdl = vdl

		call s:MarkDiffCharWID(1)

		let save_ei = &eventignore | let &eventignore = "all"
		exec t:DChar.win[1] . "wincmd w"
		call s:ShowDiffChar(range(1, line('$')))
		exec cwin . "wincmd w"
		let &eventignore = save_ei
	endif
endfunction

function! s:DiffModeLines(key, lines)
	" in diff mode, need to compare the different line between windows
	" if current window is t:DChar.win[1], narrow a:lines within vdl[1]
	" and get the corresponding lines from vdl[2]
	let [d1, d2] = [copy(t:DChar.vdl[1]), copy(t:DChar.vdl[2])]
	let [i, j] = (a:key == 1) ? [1, 2] : [2, 1]
	call map(d{i}, 'index(a:lines, v:val) == -1 ? -1 : v:val')
	call filter(d{j}, 'd{i}[v:key] != -1')
	call filter(d{i}, 'v:val != -1')
	return [d1, d2]
endfunction

function! s:LinesValues(key, lines)
	let lsv = {}
	for l in a:lines
		exec "let lsv[l] = " . substitute(sha256(getbufline(winbufnr(
			\t:DChar.win[a:key]), l)[0]), '.\{4}', '+0x&', 'g')
	endfor
	return lsv
endfunction

function! s:JumpDiffChar(dir, pos)
	" dir : 1 = forward, 0 = backward
	" pos : 1 = start, 0 = end
	if !exists("t:DChar") | return | endif

	" refresh window number of diffchar windows
	call s:RefreshDiffCharWID()

	" return if current window is not either of diffchar windows
	let cwin = winnr()
	for k in [1, 2, 0]
		if k == 0 | return | endif
		if t:DChar.win[k] == cwin | break | endif
	endfor

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
		let pc = 0 < c[0] ? split(bl[:c[0] - 1], '\zs')[-1] : ""
		let nc = c[1] <= len(bl) ? split(bl[c[1] - 1:], '\zs')[0] : ""
		" echo a-----b with DiffChange/DiffDelete
		exec "echohl " . t:DChar.dhl.C
		echon pc
		exec "echohl " . t:DChar.dhl.D
		let col = t:DChar.hlc[a:key][a:line][a:col][1]
		echon repeat('-', strwidth(
			\getbufline(winbufnr(t:DChar.win[a:key]), a:line)[0]
						\[col[0] - 1 : col[1] - 1]))
		exec "echohl " . t:DChar.dhl.C
		echon nc
		echohl None
		" set position/length for both side of deleted unit
		let clen = len(pc . nc)
		let cpos = c[0] - len(pc) + 1
	else
		" changed unit
		let dc = bl[c[0] - 1 : c[1] - 1]
		" echo the matching unit with its color
		exec "echohl " . t:DChar.dmc
				\[(count(map(t:DChar.hlc[m][line][: a:col],
				\'v:val[0]'), 'c') - 1) % len(t:DChar.dmc)]
		echon dc
		echohl None
		" set position/length for matching unit
		let clen = len(split(dc, '\zs')[a:pos ? 0 : -1])
		let cpos = a:pos ? c[0] : c[1] - clen + 1
	endif

	" show cursor on deleted unit or matching unit on another window
	let save_ei = &eventignore | let &eventignore = "all"
	exec t:DChar.win[m] . "wincmd w"
	call s:ResetDiffCharPair(m)
	if exists("*matchaddpos")
		let t:DChar.pci[m] = matchaddpos(t:DChar.dhl.U,
						\[[line, cpos, clen]], 0)
	else
		let t:DChar.pci[m] = matchadd(t:DChar.dhl.U, '\%' . line .
			\'l\%>' . (cpos - 1) . 'c\%<' . (cpos + clen) . 'c', 0)
	endif
	exec "au! dchar WinEnter <buffer=" . winbufnr(t:DChar.win[m]) .
				\"> call s:ResetDiffCharPair(" . m . ")"
	exec t:DChar.win[a:key] . "wincmd w"
	let &eventignore = save_ei
endfunction

function! s:ResetDiffCharPair(key)
	if exists("t:DChar.pci[a:key]")
		call matchdelete(t:DChar.pci[a:key])
		unlet t:DChar.pci[a:key]
		exec "au! dchar WinEnter <buffer=" .
					\winbufnr(t:DChar.win[a:key]) . ">"
		echon ""
	endif
endfunction

function! s:MarkDiffCharWID(on)
	" mark w:DCharWID (1/2) on diffchar windows or delete them
	for wvr in map(range(1, winnr('$')), 'getwinvar(v:val, "")')
		if has_key(wvr, "DCharWID") | unlet wvr["DCharWID"] | endif
	endfor
	if a:on
		call map([1, 2],
			\'setwinvar(t:DChar.win[v:val], "DCharWID", v:val)')
	endif
endfunction

function! s:RefreshDiffCharWID()
	" find diffchar windows and set their winnr to t:DChar.win again
	let t:DChar.win = {}
	for win in range(1, winnr('$'))
		let id = get(getwinvar(win, ''), "DCharWID", 0)
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
function! s:ToggleDiffHL(on)
	" no need in no-diff mode
	if !exists("t:DChar.vdl") | return | endif

	" number of tabpages where DiffChange/DiffText have been overwritten
	let tn = len(filter(map(range(1, tabpagenr('$')),
			\'gettabvar(v:val, "DChar")'),
			\'!empty(v:val) && exists("v:val.dtm")'))

	if a:on
		" globally disable DiffChange/DiffText at the first ON
		if tn == 0
			" if either of fg and attr is set, disable its HL
			let ct = ''
			for dh in ['C', 'T']
				for at in ["fg", "bold", "italic",
					\"underline", "undercurl", "reverse",
					\"inverse", "standout"]
					let vl = synIDattr(
						\hlID(t:DChar.dhl[dh]), at)
					if !empty(vl) && vl != -1
						let ct .= dh
						break
					endif
				endfor
				if ct == 'C' | let ct = 'CT' | break | endif
			endfor
			if !empty(ct)
				let s:save_hl = &highlight
				let &highlight = join(map(split(s:save_hl,
					\','), 'v:val[0] =~# "[" . ct . "]" ?
					\v:val[0] . "n" : v:val'), ',')
				let s:overwrite_ct = split(ct, '\zs')
			endif
		endif
		if exists("s:overwrite_ct")
			call s:OverwriteDiffHL(s:overwrite_ct)
		endif
	else
		" globally restore DiffChange/DiffText at the last OFF
		if exists("s:overwrite_ct")
			if tn == 1
				let &highlight = s:save_hl
				unlet s:save_hl
				unlet s:overwrite_ct
			endif
			call s:RestoreDiffHL()
		endif
	endif
endfunction

function! s:OverwriteDiffHL(ct)
	" overwrite disabled DiffChange/DiffText with its match
	if exists("t:DChar.dtm") | return | endif

	let t:DChar.dtm = {}

	let cwin = winnr()
	let save_ei = &eventignore | let &eventignore = "all"

	for k in [1, 2]
		exec t:DChar.win[k] . "wincmd w"

		if index(a:ct, 'C') != -1 | let cl = t:DChar.vdl[k] | endif

		let tl = []
		if !exists("s:save_dex")
			" normal case
			let dt = hlID(t:DChar.dhl.T)
			for l in t:DChar.vdl[k]
				let t = filter(range(1, col([l, '$']) - 1),
						\'diff_hlID(l, v:val) == dt')
				if empty(t) | continue | endif
				let [cs, ce] = [t[0], t[-1]]
				let tl += [[l, cs, ce - cs + 1]]
			endfor
		else
			" diffexpr exceptional case
			for l in t:DChar.vdl[k]
				let h = get(t:DChar.hlc[k], l, [])
				if empty(h) | continue | endif
				let cs = h[0][1][h[0][0] == 'd' ? 1 : 0]
				let ce = h[-1][1][h[-1][0] == 'd' ? 0 : 1]
				if cs > ce | continue | endif
				let tl += [[l, cs, ce - cs + 1]]
			endfor
		endif

		let t:DChar.dtm[k] = []
		for hl in a:ct
			let ll = (hl == 'C') ? cl : tl
			let p = 0
			while p < len(ll)
				let t:DChar.dtm[k] += [matchaddpos(
					\t:DChar.dhl[hl], ll[p : p + 7], -1)]
				let p += 8
			endwhile
		endfor
	endfor

	exec cwin . "wincmd w"
	let &eventignore = save_ei
endfunction

function! s:RestoreDiffHL()
	" delete all the overwritten DiffChange/DiffText matches
	if !exists("t:DChar.dtm") | return | endif

	let cwin = winnr()
	let save_ei = &eventignore | let &eventignore = "all"

	for k in [1, 2]
		exec t:DChar.win[k] . "wincmd w"
		call map(t:DChar.dtm[k], 'matchdelete(v:val)')
	endfor

	exec cwin . "wincmd w"
	let &eventignore = save_ei

	unlet t:DChar.dtm
endfunction
endif

let &cpo = s:save_cpo
unlet s:save_cpo
