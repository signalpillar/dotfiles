"        File: imaps.vim
"      Author: Srinath Avadhanula
"              ( srinath@fastmail.fm )
"         WWW: http://robotics.eecs.berkeley.edu/~srinath
" Description: insert mode template expander with cursor placement
"              while preserving filetype indentation.
" Last Change: Sun Mar 10 02:00 AM 2002 PST
" 
" this script provides a way to generate insert mode mappings which do not
" suffer from some of the problem of mappings and abbreviations while allowing
" cursor placement after the expansion. It can alternatively be thought of as
" a template expander. 
"
" Consider an example. If you do
"
" imap lhs something
"
" then a mapping is set up. However, there will be the following problems:
" 1. the 'ttimeout' option will generally limit how easily you can type the
"    lhs. if you type the left hand side too slowly, then the mapping will not
"    be activated.
" 2. if you mistype one of the letters of the lhs, then the mapping is
"    deactivated as soon as you backspace to correct the mistake.
"
" If, in order to take care of the above problems, you do instead
"
" iab lhs something
"
" then the timeout problem is solved and so is the problem of mistyping.
" however, abbreviations are only expanded after typing a non-word character.
" which causes problems of cursor placement after the expansion and invariably
" spurious spaces are inserted.
"
" this script attempts to solve all these problems by providing an emulation
" of imaps wchich does not suffer from its attendant problems. Because maps
" are activated without having to press additional characters, therefore
" cursor placement is possible. furthermore, file-type specific indentation is
" preserved, because the rhs is expanded as if the rhs is typed in literally
" by the user.
"  
" The script already provides some default mappings. each "mapping" is of the
" form:
"
" let s:<filetype>_<lhs> = "rhs"
"
" Consider a working example:
"
" let s:tex_bit  = "\\begin{itemize}\<cr>\\item ä\<cr>\\end{itemize}"
" 
" This effectively sets up the map
" 
" imap bit<leader>
"
" whenever you edit a latex file. i.e, if you type the leader character ('\'
" by default), after the word 'bit', then its expanded as follows:
"
" \begin{itemize}
" \item *
" \end{itemize}
"
" where * shows the cursor position. The special character ä (typed as
" CTRL-K + a + :) decides the cursor placement after the expansion. If there
" is no ä, then the cursor is left at the end.
"
" however, unlike in mappings, it is not necessary to enter the keys
" b,i,t,<leader> in quick succession. this works by just mapping the last
" character, which is chosen to be <leader> instead of all the characters. a
" check is then made to see if the characters entered before match the total
" LHS of the required mapping.
" 
" NOTE: There are other plugins out there which do the same thing. For
" example, Dr. Chip's Cabbr script at 
"    http://users.erols.com/astronaut/vim/index.html#CAbbrv
" However, this script is a bit more general in that mappings for various file
" types can be put in the same place and also, instead of having to extend a
" function by putting additional 'if ...' lines, you directly type in
" additional mappings as variables.
" 
"--------------------------------------%<--------------------------------------
" Bonus: This script also provides a command Snip which puts tearoff strings,
" '----%<----' above and below the visually selected range of lines. The
" length of the string is chosen to be equal to the longest line in the range.
"--------------------------------------%<--------------------------------------


" General purpose mappings {{{
let s:_date = "\<c-r>=strftime('%b %d %Y')\<cr>"
let s:_stamp = "Last Change: \<c-r>=strftime('%a %b %d %I:00 %p %Y PST')\<cr>"
let s:_winm = "http://robotics.eecs.berkeley.edu/~srinath/vim/winmanager-2.0.htm"
let s:_homep = "http://robotics.eecs.berkeley.edu/~srinath"
" End general purpose mappings }}}

" HTML mappings {{{
" HTML commands {{{2
let s:html_tab = "<table border=2 cellspacing=2 cellpadding=5>\<cr><tr>\<cr>\<tab><td>ä</td>\<cr>\<bs></tr>\<cr></table>"
let s:html_ref = "<a href=\"ä\"></a>"
let s:html_ol = "<ol>\<cr><li>ä</li>\<cr></ol>"
let s:html_ul = "<ul>\<cr><li>ä</li>\<cr></ul>"
let s:html_tr = "<tr>\<cr>\<tab><td>ä</td>\<cr>\<bs></tr>"
let s:html_td = "<td>ä</td>"
let s:html_bb = "<b>ä</b>"
let s:html_it = "<i>ä</i>"
" }}}
" HTML greek characters {{{2
let s:html_a = "\&alpha;"
let s:html_b = "\&beta;"
let s:html_c = "\&chi;"
let s:html_d = "\&delta;"
let s:html_e = "\&epsilon;"
let s:html_f = "\&phi;"
let s:html_g = "\&gamma;"
let s:html_h = "\&eta;"
let s:html_k = "\&kappa;"
let s:html_l = "\&lambda;"
let s:html_m = "\&mu;"
let s:html_n = "\&nu;"
let s:html_p = "\&pi;"
let s:html_q = "\&theta;"
let s:html_r = "\&rho;"
let s:html_s = "\&sigma;"
let s:html_t = "\&tau;"
let s:html_u = "\&upsilon;"
let s:html_v = "\&varsigma;"
let s:html_w = "\&omega;"
let s:html_x = "\&xi;"
let s:html_y = "\&psi;"
let s:html_z = "\&zeta;"
let s:html_A = "\&Alpha;"
let s:html_B = "\&Beta;"
let s:html_C = "\&Chi;"
let s:html_D = "\&Delta;"
let s:html_E = "\&Epsilon;"
let s:html_F = "\&Phi;"
let s:html_G = "\&Gamma;"
let s:html_H = "\&Eta;"
let s:html_K = "\&Kappa;"
let s:html_L = "\&Lambda;"
let s:html_M = "\&Mu;"
let s:html_N = "\&Nu;"
let s:html_P = "\&Pi;"
let s:html_Q = "\&Theta;"
let s:html_R = "\&Rho;"
let s:html_S = "\&Sigma;"
let s:html_T = "\&Tau;"
let s:html_U = "\&Upsilon;"
let s:html_V = "\&Varsigma;"
let s:html_W = "\&Omega;"
let s:html_X = "\&Xi;"
let s:html_Y = "\&Psi;"
let s:html_Z = "\&Zeta;"
" }}}
" end HTML mappings }}}

" Latex Mappings {{{1
" Latex command mappings {{{2
let s:tex_bar       = "\\left\<cr>\\begin{array}{ä}\<cr>\<cr>\\end{array}\<cr>\\right"
let s:tex_ben       = "\\begin{enumerate}\<cr>\\item ä\<cr>\\end{enumerate}"
let s:tex_bit       = "\\begin{itemize}\<cr>\\item ä\<cr>\\end{itemize}"
let s:tex_beq       = "\\begin{equation}\<cr>\<cr>ä\\end{equation}"
let s:tex_bqn       = "\\begin{eqnarray}\<cr>ä\<cr>\\end{eqnarray}"
let s:tex_bfg       = "\\begin{figure}[h]\<cr>\\centerline{\\psfig{figure=ä.eps}}\<cr>\\caption{}\<cr>\\label{fig:}\<cr>\\end{figure}"
let s:tex_bfe       = "\\begin{figure}\<cr>\\vspace{ä}\<cr>\\caption{}\<cr>\\end{figure}"
let s:tex_btb       = "\\begin{tabular}{ä}\<cr>\<cr>\\end{tabular}"
let s:tex_bta       = "\\begin{table}\<cr>\\centering\<cr>\\caption{tab:}\<cr>\\begin{tabular}{ä}\<cr>\<cr>\\end{tabular}\<cr>\\label{tab:}\<cr>\\end{table}"
let s:tex_pic       = "\\begin{picture}(4,4)\<cr>\\put(0.5,0){\\framebox(4,4){ä}}\<cr>\\end{picture}"
let s:tex_mat       = "\\left[\<cr>\\begin{array}{ä}\<cr>\\end{array}\<cr>\\right]"
let s:tex_verb      = "\\begin{verbatim}\<cr>\<cr>ä\\end{verbatim}"
let s:tex_frac      = "\\frac{ä}{}"
let s:tex_dot       = "\\dot{ä}"
let s:tex_ddot      = "\\ddot{ä}"
let s:tex_vb        = "\\verb|ä|"
let s:tex_bf        = "{\\bf ä}"
let s:tex_em        = "{\\em ä}"
let s:tex_it        = "{\\it ä}"
let s:tex_mb        = "\\mbox{ä}"
let s:tex_sq        = "\\sqrt{ä}"
let s:tex_eps       = "\\psfig{figure=ä.eps}"
let s:tex_minip     = "\\begin{minipage}[t]{äcm}\<cr>\\end{minipage}"
let s:tex_sec       = "\\section{ä}"
let s:tex_ssec      = "\\subsection{ä}"
let s:tex_sssec     = "\\subsubsection{ä}"
let s:tex_sum       = "\\sum{ä}{}"
let s:tex_suml      = "\\sum\\limits_{ä}^{}"
let s:tex_int       = "\\int_{ä}^{}"
let s:tex_intl      = "\\int\\limits_{ä}^{}"
let s:tex_bbr       = "\\left( ä \\right)"
let s:tex_bbc       = "\\left\\{ ä \\right\\}"
let s:tex_bbs       = "\\left[ ä \\right]"
let s:tex_rr        = "\\right"
let s:tex_ll        = "\\left"
let s:tex_part      = "\\partial"
" end latex command mappings }}}
" latex greek characters {{{2
let s:tex_a = "\\alpha"
let s:tex_b = "\\beta"
let s:tex_c = "\\chi"
let s:tex_d = "\\delta"
let s:tex_e = "\\epsilon"
let s:tex_f = "\\phi"
let s:tex_g = "\\gamma"
let s:tex_h = "\\eta"
let s:tex_k = "\\kappa"
let s:tex_l = "\\lambda"
let s:tex_m = "\\mu"
let s:tex_n = "\\nu"
let s:tex_p = "\\pi"
let s:tex_q = "\\theta"
let s:tex_r = "\\rho"
let s:tex_s = "\\sigma"
let s:tex_t = "\\tau"
let s:tex_u = "\\upsilon"
let s:tex_v = "\\varsigma"
let s:tex_w = "\\omega"
let s:tex_x = "\\xi"
let s:tex_y = "\\psi"
let s:tex_z = "\\zeta"
let s:tex_A = "\\Alpha"
let s:tex_B = "\\Beta"
let s:tex_C = "\\Chi"
let s:tex_D = "\\Delta"
let s:tex_E = "\\Epsilon"
let s:tex_F = "\\Phi"
let s:tex_G = "\\Gamma"
let s:tex_H = "\\Eta"
let s:tex_K = "\\Kappa"
let s:tex_L = "\\Lambda"
let s:tex_M = "\\Mu"
let s:tex_N = "\\Nu"
let s:tex_P = "\\Pi"
let s:tex_Q = "\\Theta"
let s:tex_R = "\\Rho"
let s:tex_S = "\\Sigma"
let s:tex_T = "\\Tau"
let s:tex_U = "\\Upsilon"
let s:tex_V = "\\Varsigma"
let s:tex_W = "\\Omega"
let s:tex_X = "\\Xi"
let s:tex_Y = "\\Psi"
let s:tex_Z = "\\Zeta"
" end latex greek chars }}}
" end latex macros }}}

" Vim Mappings {{{
let s:vim_while  = "let i = ä\<cr>while i <= \<cr>\<cr>\tlet i = i + 1\<cr>\<bs>endwhile"
let s:vim_fdesc   = "\"Description: "
" end vim mappings }}}

" Perl Mappings {{{
let s:perl_elif = "elsif (ä) {\<cr>\<tab>\<cr>\<bs>}"
let s:perl_for = "for ( ä; ; ) {\<cr>\<cr>}"
" end perl mappings }}}

function! <SID>ProcessImapLeader()
	" unless we are at the very end of the word, we need to go back in order
	" to find the last word typed.
	if virtcol('.') != virtcol('$')
		normal! h
		let word = expand('<cword>')
		normal! l
	else
		let word = expand('<cword>')
	end
	let rhs = ''
	if exists('s:'.&ft.'_'.word)
	" first find out whether this word is a mapping in the current file-type.
		exe 'let rhs = s:'.&ft.'_'.word
	elseif exists('s:_'.word)
	" also check for general purpose mappings.
		exe 'let rhs = s:_'.word
	end

	if rhs != ''
		" if this is a mapping, then erase the previous part of the map
		" by also returning a number of backspaces.
		let bkspc = substitute(word, '.', "\<bs>", "g")
		" if the RHS contains an ä, then go there. this enables easy cursor
		" placement. also put a call to the SAImaps_RemoveLastHistoryItem()
		" functions in order to clean up the search history.
		if rhs =~ 'ä'
			let movement = "\<esc>?ä\<cr>:call SAImaps_RemoveLastHistoryItem()\<cr>s"
		else
			let movement = ""
		end

		return bkspc.rhs.movement
	else
		" else put the mapleader.
		return exists("g:mapleader") ? g:mapleader : '\'
	end
endfunction

inoremap <Leader> <C-r>=<SID>ProcessImapLeader()<cr>

"-------------------------------------%<-------------------------------------
" this puts a the string "--------%<---------" above and below the visually
" selected block of lines. the length of the 'tearoff' string depends on the
" maximum string length in the selected range. this is an aesthetically more
" pleasing alternative instead of hardcoding a length.
"-------------------------------------%<-------------------------------------
function! <SID>Snip() range
	let i = a:firstline
	let maxlen = -2
	" find out the maximum virtual length of each line.
	while i <= a:lastline
		exe i
		let length = virtcol('$')
		let maxlen = (length > maxlen ? length : maxlen)
		let i = i + 1
	endwhile
	let maxlen = (maxlen > &tw && &tw != 0 ? &tw : maxlen)
	let half = maxlen/2
	exe a:lastline
	" put a string below
	exe "norm! o\<esc>".(half - 1)."a-\<esc>A%<\<esc>".(half - 1)."a-"
	" and above. its necessary to put the string below the block of lines
	" first because that way the first line number doesnt change...
	exe a:firstline
	exe "norm! O\<esc>".(half - 1)."a-\<esc>A%<\<esc>".(half - 1)."a-"
endfunction

com! -nargs=0 -range Snip :<line1>,<line2>call <SID>Snip()

" CleanUpHistory: This function needs to be globally visible because its
"                 called from outside the script during expansion.
function! SAImaps_RemoveLastHistoryItem()
  call histdel("/", -1)
  let @/ = histget("/", -1)
endfunction

" vim6:fdm=marker:nowrap
