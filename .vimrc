" disable compatibility mode with VI
" Use Vim settings, rather then Vi settings (much better!).
set nocompatible
filetype off

"{{{ General section

" Allow backgrounding buffers without writing them, and remember marks/undo
" for backgrounded buffers
set hidden
filetype plugin indent on

" Make tab completion for files/buffers act like bash
set wildmenu

" Keep more context when scrolling off the end of a buffer
set scrolloff=3

" do not beep
set t_vb=
set novisualbell

" Turn on the verboseness to see everything vim is doing.
" set verbose=9

" select when using the mouse
set selectmode=mouse

" set the commandheight
set cmdheight=2

" do not keep a backup files 
set nobackup
set nowritebackup

set noswapfile
"}}}

"====================================="
" Vundle configuration "
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle " required! 
Bundle 'gmarik/vundle'
" Plugins " {{{
" Git integration
Bundle "git.zip"
Bundle "tpope/vim-fugitive"
"Syntax highlight"
Bundle "cucumber.zip"
"Nerd tree
Bundle "The-NERD-tree"
map <F2> :NERDTreeToggle<CR>
"Nerd commenter
Bundle "The-NERD-Commenter"
"Color schemes
Bundle 'mayansmoke'
" Project plugin
Bundle 'project.tar.gz'
let loaded_project = 1

Bundle 'minibufexpl.vim'

Bundle 'Conque-Shell'

"====================================="

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" I like 4 spaces for indenting
set shiftwidth=4

" I like 4 stops
set tabstop=4

" Replace tabs with spaces
set expandtab


" *****************************************************
" //buffer
" if file changed from the outside - buffer refresh
set autoread
set number
set smarttab
" 4 spaces in tab
set softtabstop=4
"remove redundant spaces
set shiftround
" Search
set smartcase

" *****************************************************
" //indent
set smartindent
" Always  set auto indenting on
" inherit indent
set autoindent

" Set 52 lines for the display, 1 for the status line.
" and other display options
if has('gui_running')
    " i like about 80 character width lines

  set textwidth=78
  "  2 for the command line
  set lines=52
  " add columns for the Project plugin

  set columns=110
  " enable use of mouse
  set mouse=a
endif

" keep 1000 lines of command line history
set history=1000

" show the cursor position all the time
set ruler

" show incomplete command in status bar
set showcmd

" do incremental searches (annoying but handy);
set incsearch

" Show  tab characters. Visual Whitespace.
"set list
"set listchars=tab:>.
set listchars=tab:>-,trail:~,extends:>,precedes:<,eol:$

" Make searches case-sensitive only if they contain upper-case characters
set ignorecase
set smartcase


" smart search (override 'ic' when pattern has uppers)
set scs

" Set status line
:set statusline=%<%f%=\ [%1*%M%*%n%R%H]\ %-19(%3l,%02c%03V%)%O'%02b'
:hi User1 term=inverse,bold cterm=inverse,bold ctermfg=red

" GRB: clear the search buffer when hitting return
:nnoremap <CR> :nohlsearch<CR>/<BS>

" Always display a status line at the bottom of the window
set laststatus=2

" Insert two spaces after a period with every joining of lines.

" I like this as it makes reading texts easier (for me, at least).
set joinspaces

" showmatch: Show the matching bracket for the last ')'?
set showmatch

" wrapping
set nowrap

"wrap by word
set linebreak

" Java specific stuff
let java_highlight_all=1
let java_highlight_debug=1

let java_ignore_javadoc=1
let java_highlight_functions=1
let java_mark_braces_in_parens_as_errors=1

" Commands for :Explore
"let g:explVertical=1    " open vertical split winow
"let g:explSplitRight=1  " Put new window to the right of the explorer
"let g:explStartRight=0  " new windows go to right of explorer window

" for the TOhtml command
let html_use_css=1

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")

  syntax on
  set hlsearch
endif

" when capslock is pressed - standart commands works too
command! Q quit
command! W write

command -nargs=0 FormatXml :1,$!xmllint --format -<CR>

" *****************************************************
" C Y G W I N

"set shell=C:/cygwin/bin/bash

"set shellcmdflag=--login\ -c
"set shellxquote=\"

"
" *****************************************************
" E N C O D I N G S"

"

"set encoding=utf8
"set fileencodings=utf8,cp1251


" ************************************************************************
" C O M M A N D S
"


"switch to directory of current file
command! CD cd %:p:h

" ************************************************************************
" K E Y   M A P P I N G S
"

map <Leader>e :Explore<cr>
map <Leader>s :Sexplore<cr>

" pressing < or > will let you indent/unident selected lines

vnoremap < <gv
vnoremap > >gv

" Make p in Visual mode replace the selected text with the "" register.

vnoremap p <Esc>:let current_reg = @"<CR>gvs<C-R>=current_reg<CR><Esc>

" Make tab in v mode work like I think it should (keep highlighting):
vmap <tab> >gv
vmap <s-tab> <gv

" map ,L mz1G/Last modified:/e<Cr>CYDATETIME<Esc>`z

map ,L    :let @z=TimeStamp()<Cr>"zpa
map ,datetime :let @z=strftime("%d %b %Y %X")<Cr>"zpa

" Map <c-s> to write current buffer.

map <c-s> :w<cr>
imap <c-s> <c-o><c-s>
imap <c-s> <esc><c-s>
map ,date :let @z=strftime("%d %b %Y")<Cr>"zpa

" Buffer naviation
map <M-Left> :bprevious<CR>
map <M-Right> :bnext<CR>

" Select all.
map <c-a> ggVG

" Undo in insert mode.
imap <c-z> <c-o>u


" ************************************************************************
" B E G I N  A U T O C O M M A N D S
"
" Enable file type detection.
" Use the default filetype settings, so that mail gets 'tw' set to 72,
" 'cindent' is on in C files, etc.
" Also load indent files, to automatically do language-dependent indenting.


" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler


" Normally don't automatically format 'text' as it is typed, only do this

" with comments, at 79 characters.
au BufNewFile,BufEnter *.c,*.h,*.java,*.jsp set formatoptions-=t tw=79


" GUI ONLY type stuff.
if has("gui")

  :menu &MyVim.Current\ File.Convert\ Format.To\ Dos :set fileformat=dos<cr> :w<cr>

  :menu &MyVim.Current\ File.Convert\ Format.To\ Unix :set fileformat=unix<cr> :w<cr>

  :menu &MyVim.Current\ File.Remove\ Trailing\ Spaces\ and\ Tabs :%s/[  ]*$//g<cr>

  :menu &MyVim.Current\ File.Remove\ Ctrl-M :%s/^M//g<cr>
  :menu &MyVim.Current\ File.Remove\ All\ Tabs :retab<cr>

  :menu &MyVim.Current\ File.To\ HTML :runtime! syntax/2html.vim<cr>
  " these don't work for some reason

  ":amenu &MyVim.Insert.Date<Tab>,date <Esc><Esc>:,date<Cr>
  ":amenu &MyVim.Insert.Date\ &Time<Tab>,datetime <Esc><Esc>:let @z=YDATETIME<Cr>"zpa

  :amenu &MyVim.Insert.Last\ &Modified<Tab>,L <Esc><Esc>:let @z=TimeStamp()<CR>"zpa
  :amenu &MyVim.-SEP1- <nul>

  :amenu &MyVim.&Global\ Settings.Toggle\ Display\ Unprintables<Tab>:set\ list! :set list!<CR>

  :amenu &MyVim.-SEP2- <nul>
  :amenu &MyVim.&Project :Project<CR>

  " hide the mouse when characters are typed
  set mousehide

  " set the gui options to:
  "   g: grey inactive menu items
  "   m: display menu bar
  "   r: display scrollbar on right side of window
  "   b: display scrollbar at bottom of window

  "   t: enable tearoff menus on Win32
  "   T: enable toolbar on Win32
  set go=gmr
  colorscheme mayansmoke
  "set guifont=Lucida_Console:h9:cANSI
  "set guifont=Monospace\ 9
  set guifont=Monaco\ 10
else
  colorscheme slate
endif

" ************************************************************************
" A B B R E V I A T I O N S 

"
abbr #b /************************************************************************
abbr #e  ************************************************************************/

abbr hosts C:\WINNT\system32\drivers\etc\hosts

" abbreviation to manually enter a timestamp. Just type YTS in insert mode 

iab YTS <C-R>=TimeStamp()<CR>

" Date/Time stamps
" %a - Day of the week
" %b - Month

" %d - Day of the month
" %Y - Year
" %H - Hour
" %M - Minute
" %S - Seconds
" %Z - Time Zone

iab YDATETIME <c-r>=strftime(": %a %b %d, %Y %H:%M:%S %Z")<cr>


" ************************************************************************
"  F U N C T I O N S

"

" first add a function that returns a time stamp in the desired format 
if !exists("*TimeStamp")
    fun TimeStamp()

        return "Last-modified: " . strftime("%d %b %Y %X")
    endfun
endif



" do not redraw, when running macros..lazyredraw
set lz

 "F5 - view list of tabs
map <F5> <Esc>:buffers<cr>
map <F5> <esc>:buffers<cr>
map <F5> <esc><esc>:buffers<cr>

" F6 - go to previous tab
map <F6> :tabp<cr>
vmap <F6> <esc>:tabp<cr>i
imap <F6> <esc>:tabp<cr>i

" F7 - go to next tab
map <F7> :tabn<cr>
vmap <F7> <esc>:tabn<cr>i
imap <F7> <esc>:tabn<cr>i

 " F10 - delete(close) tab
map <F10> :tabclose<cr>
vmap <F10> <esc>:tabclose<cr>
imap <F10> <esc>:tabclose<cr>

  "F11 - create new tab
map <C-F11> :tabnew<cr>
vmap <C-F11> <esc>:tabnew<cr>
imap <C-F11> <esc>:tabnew<cr>


set complete=.,w,b,u,t,i



" Folding
"set foldmethod=indent
set foldcolumn=2
" open folding using space
nmap <Space> zo

" Syntax
syntax on


"""""""""""""""""""""""""""""""
" colors and fonts
"""""""""""""""""""""""""""""""
"function FoldTag(tagName)
    "if getline(v:lnum+1) =~ '<'.a:tagName.'>'
        "return '>1'
    "endif
    "if getline(v:lnum) =~ '</'.a:tagName.'>'
        "return '<1'
    "endif
    "return foldlevel(v:lnum)                                
"endfunction 

"function FoldStackTrace()
    "return FoldTag('stacktrace')
"endfunction

"function FoldClientProperties()
    "return FoldTag('ClientProperties')
"endfunction

function FoldStackTrace()
    if getline(v:lnum+1) =~ '<stacktrace>'
        return '>1'
    endif
    if getline(v:lnum) =~ '</stacktrace>'
        return '<1'
    endif
    return foldlevel(v:lnum)                                
endfunction 

autocmd BufEnter *.record call b:configureForRecordFiles()
autocmd BufEnter Message*txt call b:configureForRecordFiles()

function! b:configureForRecordFiles()
    set foldmethod=expr
    set foldexpr=FoldStackTrace() "stacktrace
    "set foldexpr=FoldClientProperties() "'ClientProperties')
    "set foldexpr=getline(v:lnum)=~'^\\s*<stacktrace>\\s*'?0:0
    "set foldexpr=getline(v:lnum)=~'^\\s*<\/stacktrace>\\s*'?'<1':0
    "syn
    "SynColor Error term=reverse cterm=NONE ctermfg=Black ctermbg=Black gui=NONE guifg=White guibg=Red

    "set foldmethod=syntax
    "syntax region stacktrace start="^\\s*<destination>\\s*" end="^\\s*</destination>\\s*"
    "
    syn match ConnectionFailed /<CONNECT.*RESULT=\"\"/
    syn match ConnectionOk /<CONNECT.*RESULT=\"success\"/
    hi def link ConnectionFailed Error
    hi def link ConnectionOk SpecialComment
endfunction


autocmd BufEnter *.log call b:configureForLog()

function! b:configureForLog()
    set nowrap
    set noswapfile              "do not save copy of file
    set bufhidden=unload        "save memory when other file is viewed
    " causes file silent state - without refreshing
    "set buftype=nowrite          "is read-only
    set undolevels=-1           "no undo
    
    "reread file content automatically
    set autoread
    " syntax
    "syn match ExceptionClass /\(\.\|\w\)\{-}Exception:\?\(\s.*\)\?/  contains=ExceptionMessage,package
    syn match ExceptionClass /\(\.\|\w\)\{-}Exception:\s.*/  contains=ExceptionMessage,package
    syn match ExceptionMessage /:\s.*/ contains=package,SessionFacade 
    syn match sessionfacade /\w*sessionfacade\w*/ 
    syn match package /com\.hp/
    syn keyword AtKeyword at
    hi def link ExceptionClass String
    hi def link ExceptionMessage Comment
    hi def link AtKeyword Keyword
    hi def link SessionFacade Macro
    hi def link package Typedef
endfunction

" GRB: use emacs-style tab completion when selecting files, etc
set wildmode=longest,list


" Python sections

" GRB: add pydoc command
:command! -nargs=+ Pydoc :call ShowPydoc("<args>")
function! ShowPydoc(module, ...)
    let fPath = "/tmp/pyHelp_" . a:module . ".pydoc"
    :execute ":!pydoc " . a:module . " > " . fPath
    :execute ":sp ".fPath
endfunction

" GRB: Always source python.vim for Python files
"au FileType python source ~/.vim/scripts/python.vim

" GRB: Use custom python.vim syntax file
"au! Syntax python source ~/.vim/syntax/python.vim
let python_highlight_all = 1
let python_slow_sync = 1

" support highlighting
let python_highlight_all = 1
autocmd FileType python set omnifunc=pythoncomplete#Complete
" Tab autocompletion
imap <S-Tab> <c-r>=InsertTabWrapper("backward")<cr>
imap <Tab> <c-r>=InsertTabWrapper("forward")<cr>

function! InsertTabWrapper(direction)
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    elseif "backward" == a:direction
        return "\<c-p>"
    else
        return "\<c-n>"
    endif
endfunction

set complete=""
set complete+=.
set complete+=k
set complete+=b
set complete+=t
"set completeopt-=preview
"set completeopt+=longest

autocmd BufRead *.py set smartindent
set cinwords=if,elif,else,for,while,try,except,finally,def,class

let g:Tlist_Ctags_Cmd="D:/opt/ctags58/ctags.exe"
let g:Tlist_Show_One_File=1
let g:Tlist_GainFocus_On_ToggleOpen = 1
let g:Tlist_Auto_Update = 1
let g:Tlist_Compact_Format = 1
let Tlist_Use_Right_Window = 1

map <F8> :TlistToggle<CR>


augr class
au!
au bufreadpost,filereadpost *.class %!d:\bin\jad.exe -noctor -ff -i -p %
au bufreadpost,filereadpost *.class set readonly
au bufreadpost,filereadpost *.class set ft=java
au bufreadpost,filereadpost *.class normal gg=G
au bufreadpost,filereadpost *.class set nomodified
augr END


" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>

vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>


