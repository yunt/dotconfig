" vim:expandtab shiftwidth=2 tabstop=8 textwidth=72

" Wu Yongwei's _vimrc for Vim 7
" Last Change: 2010-04-07 19:44:36

if v:version < 700
  echoerr 'This _vimrc requires Vim 7 or later.'
  quit
endif

if has('autocmd')
  " Remove ALL autocommands for the current group
  au!
endif

if has('gui_running')
  " Always show file types in menu
  let do_syntax_sel_menu=1
endif

if has('multi_byte')
  " Legacy encoding is the system default encoding
  let legacy_encoding=&encoding
endif

colors tango-desert
if has('gui_running') && has('multi_byte')
  " Set encoding (and possibly fileencodings)
  if $LANG !~ '\.' || $LANG =~? '\.UTF-8$'
    set encoding=utf-8
  else
    let &encoding=matchstr($LANG, '\.\zs.*')
    let &fileencodings='gbk,ucs-bom,utf-8,' . &encoding
    let legacy_encoding=&encoding
  endif
  set guioptions=egtm
  set lines=40
  set columns=120
  set nu
endif

let g:NeoComplCache_EnableAtStartup = 1
"let g:NeoComplCache_DictionaryFileTypeLists = {
      "\ 'default' : '',
      "\ 'vimshell' : $HOME.'/.vimshell_hist',
      "\ 'scheme' : $HOME.'/.gosh_completions',
      "\ 'scala' : $DOTVIM.'/dict/scala.dict',
      "\ 'ruby' : $DOTVIM.'/dict/ruby.dict'
      "\ }
let g:NeoComplCache_DictionaryFileTypeLists = {
      \ 'default' : '',
      \ 'php' : $HOME.'/.vim/dict/php.txt'
      \ }

set nocompatible
source $VIMRUNTIME/vimrc_example.vim
"if has('gui_running')
  "source $VIMRUNTIME/mswin.vim
  "unmap  <C-Y>|             " <C-Y> for Redo is kept in insert mode
  "iunmap <C-A>|             " <C-A> for Select-All is kept in normal mode
  "" Key mapping to switch windows quickly (<C-Tab> is already mapped)
  "nnoremap <C-S-Tab> <C-W>W
  "inoremap <C-S-Tab> <C-O><C-W>W
"endif

set autoindent
"set nobackup
set formatoptions+=mM
set fileencodings=ucs-bom,utf-8,default,latin1          " default value
"set grepprg=e:\gnuwin32\bin\grep\ -nH
"set statusline=%<%f\ %h%m%r%=%k[%{(&fenc==\"\")?&enc:&fenc}%{(&bomb?\",BOM\":\"\")}]\ %-14.(%l,%c%V%)\ %P
set statusline=%{GitBranch()}%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [%{(&fenc==\"\")?&enc:&fenc}%{(&bomb?\",BOM\":\"\")}]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
set cursorline
" set dictionary+=C:\Program\\\ Files\Vim\vimfiles\words
" set tags+=C:\Program\\\ Files\Vim\vimfiles\systags      " help ft-c-omni
set cst
set csto=1
setlocal tags+=/tags
set cspc=3
" set directory=~\Locals~1\Temp
set path=.,
        \,

filetype plugin on

" Personal setting for working with Windows NT/2000/XP (requires tee in path)
if &shell =~? 'cmd'
  "set shellxquote=\"
  set shellpipe=2>&1\|\ tee
endif

" Quote shell if it contains space and is not quoted
if &shell =~? '^[^"].* .*[^"]'
  let &shell='"' . &shell . '"'
endif

" Set British spelling convention for International English
if has('syntax')
  set spelllang=en
endif

if has('eval')
  " Function to find the absolute path of a runtime file
  function! FindRuntimeFile(filename, ...)
    if a:0 != 0 && a:1 =~ 'w'
      let require_writable=1
    else
      let require_writable=0
    endif
    let runtimepaths=&runtimepath . ','
    while strlen(runtimepaths) != 0
      let filepath=substitute(runtimepaths, ',.*', '', '') . '/' . a:filename
      if filereadable(filepath)
        if !require_writable || filewritable(filepath)
          return filepath
        endif
      endif
      let runtimepaths=substitute(runtimepaths, '[^,]*,', '', '')
    endwhile
    return ''
  endfunction

  " Function to switch the cursor position between the first column and the
  " first non-blank column
  function! GoToFirstNonBlankOrFirstColumn()
    let cur_col=col('.')
    normal ^
    if cur_col != 1 && cur_col == col('.')
      normal 0
    endif
  endfunction

  " Key mappings to make Home go to first non-blank column or first column
  nnoremap <silent> <Home>      :call GoToFirstNonBlankOrFirstColumn()<CR>
  inoremap <silent> <Home> <C-O>:call GoToFirstNonBlankOrFirstColumn()<CR>
endif

" Key mappings to ease browsing long lines
noremap  <C-J>         gj
noremap  <C-K>         gk
inoremap <M-Home> <C-O>g0
inoremap <M-End>  <C-O>g$

" Key mappings for quick arithmetic inside Vim (requires a calcu in path)
nnoremap <silent> <Leader>ma yypV:!calcu <C-R>"<CR>k$
vnoremap <silent> <Leader>ma yo<ESC>pV:!calcu <C-R>"<CR>k$
nnoremap <silent> <Leader>mr yyV:!calcu <C-R>"<CR>$
vnoremap <silent> <Leader>mr ygvmaomb:r !calcu <C-R>"<CR>"ay$dd`bv`a"ap

" Key mapping for confirmed exiting
nnoremap ZX :confirm qa<CR>

" Key mapping for opening the clipboard (Vim script #1014) to avoid
" conflict with the NERD Commenter (Vim script #1218)
nmap <unique> <silent> <Leader>co <Plug>ClipBrdOpen

" Key mapping to stop the search highlight
nmap <silent> <F2>      :nohlsearch<CR>
imap <silent> <F2> <C-O>:nohlsearch<CR>

" Key mappings to fold line according to syntax
nmap <silent> <F3> :set fdl=1 fdm=syntax<bar>syn sync fromstart<CR>
nmap <C-F3>   zv
nmap <M-F3>   zc

" Key mapping for the VimExplorer (Vim script #1950)
nmap <silent> <F4> :exec ':VE ' . expand('%:p:h')<CR>

" Key mapping to toggle the display of status line for the last window
nmap <silent> <F6> :if &laststatus == 1<bar>
                     \set laststatus=2<bar>
                     \echo<bar>
                   \else<bar>
                     \set laststatus=1<bar>
                   \endif<CR>

" Key mapping for the taglist.vim plug-in (Vim script #273)
nmap <F9>      :Tlist<CR>
imap <F9> <C-O>:Tlist<CR>

" Key mappings for quickfix commands, tags, and buffers
nmap <F11>   :cn<CR>
nmap <F12>   :cp<CR>
nmap <M-F11> :copen<CR>
nmap <M-F12> :cclose<CR>
nmap <C-F11> :tn<CR>
nmap <C-F12> :tp<CR>
nmap <S-F11> :n<CR>
nmap <S-F12> :prev<CR>

" Function to turn each paragraph to a line (to work with, say, MS Word)
function! ParagraphToLine()
  normal ma
  if &formatoptions =~ 'w'
    let reg_bak=@"
    normal G$vy
    if @" =~ '\s'
      normal o
    endif
    let @"=reg_bak
    silent! %s/\(\S\)$/\1\r/e
  else
    normal Go
  endif
  silent! g/\S/,/^\s*$/j
  silent! %s/\s\+$//e
  normal `a
endfunction

" Non-GUI setting
if !has('gui_running')
  " English messages only
  " language messages en

  " Do not increase the windows width in taglist
  let Tlist_Inc_Winwidth=0

  " Set text-mode menu
  if has('wildmenu')
    set wildmenu
    set cpoptions-=<
    set wildcharm=<C-Z>
    nmap <F10>      :emenu <C-Z>
    imap <F10> <C-O>:emenu <C-Z>
  endif

  " Change encoding according to the current console code page
  if &termencoding != '' && &termencoding != &encoding
    let &encoding=&termencoding
    let &fileencodings='ucs-bom,utf-8,' . &encoding
  endif
endif

" Display window width and height in GUI
if has('gui_running') && has('statusline')
  let &statusline=substitute(
                 \&statusline, '%=', '%=%{winwidth(0)}x%{winheight(0)}  ', '')
  set laststatus=2
endif

" Set up language and font in GUI
if has('gui_running') && has('multi_byte')
  function! UTF8_East()
    exec 'language messages ' . s:lang_east . '.UTF-8'
    set ambiwidth=double
    set encoding=utf-8
    let s:utf8_east_mode=1
  endfunction

  function! UTF8_West()
    exec 'language messages ' . s:lang_west . '.UTF-8'
    set ambiwidth=single
    set encoding=utf-8
    let s:utf8_east_mode=0
  endfunction

  function! UTF8_SwitchMode()
    if s:utf8_east_mode
      call UTF8_West()
      call UTF8_SetFont()
    else
      call UTF8_East()
      call UTF8_SetFont()
    endif
  endfunction

  function! UTF8_SetFont()
    if &encoding != 'utf-8'
      return
    endif
    if s:utf8_east_mode
      if &fileencoding == 'cp936' ||
            \&fileencoding == 'gbk' ||
            \&fileencoding == 'euc-cn'
        let s:font_east=s:font_schinese
      elseif &fileencoding == 'cp950' ||
            \&fileencoding == 'big5' ||
            \&fileencoding == 'euc-tw'
        let s:font_east=s:font_tchinese
      endif
      if &guifont != s:font_east
        exec 'set guifont=' . s:font_east
      endif
    else
      if &guifont != s:font_west
        exec 'set guifont=' . s:font_west
      endif
    endif
  endfunction

  function! UTF8_CheckAndSetFont()
    if (s:utf8_east_mode && &guifont == s:font_east) ||
          \(!s:utf8_east_mode && &guifont == s:font_west)
      call UTF8_SetFont()
    endif
  endfunction

  " Rebuild the menu to make the translations display correctly
  " --------------------------------------------------------------------
  " Uncomment the following code if all of the following conditions
  " hold:
  "   1) Unicode support is wanted (enabled by default for gVim in this
  "      _vimrc);
  "   2) The libintl.dll shipped with gVim for Windows is not updated
  "      with a new one that supports encoding conversion (see also
  "      <URL:http://tinyurl.com/2hnwaq> for issues with this approach);
  "   3) The environment variable LANG is not manually set to something
  "      like "zh_CN.UTF-8", and the default language is not ASCII-based
  "      (English).
  " The reason why the code is not enabled by default is because it can
  " interfere with the localization of menus created by plug-ins.
  " --------------------------------------------------------------------
  "
  "if $LANG !~ '\.' && v:lang !~? '^\(C\|en\)\(_\|\.\|$\)'
  "  runtime! delmenu.vim
  "endif

  " Fonts
  let s:font_schinese='DejaVu\ Sans\ YuanTi\ Mono:h10.5:cDEFAULT'
  let s:font_tchinese='MingLiU:h10.5:cDEFAULT'
  if legacy_encoding == 'cp936'
    let s:font_schinese='Monaco\ 10'              " Use the system default font
    " let s:font_schinese='Monaco:h9:cDEFAULT'              " Use the system default font
  elseif legacy_encoding == 'cp950'
    let s:font_tchinese='Monaco\ 10'              " Use the system default font
    " let s:font_tchinese='Monaco:h9:cDEFAULT'              " Use the system default font
  endif
  if legacy_encoding != 'cp950'
    let s:font_east=s:font_schinese
  else
    let s:font_east=s:font_tchinese
  endif
  let s:font_west='Monaco\ 10'
  " let s:font_west='Monaco:h9:cDEFAULT'

  " Extract the current east/west language settings
  if v:lang =~? '^\(zh\)\|\(ja\)\|\(ko\)'
    let s:lang_east=matchstr(v:lang, '^[a-zA-Z_]*\ze\(\.\|$\)')
    let s:lang_west='en'
    let s:utf8_east_mode=1
    if v:lang=~? '^zh_TW'
      let s:font_east=s:font_tchinese
    endif
  else
    let s:lang_east='zh_CN'
    let s:lang_west=matchstr(v:lang, '^[a-zA-Z_]*\ze\(\.\|$\)')
    let s:utf8_east_mode=0
  endif

  " Set a suitable GUI font and the ambiwidth option
  if &encoding == 'utf-8'
    if s:utf8_east_mode
      call UTF8_East()
    else
      call UTF8_West()
    endif
  endif
  if s:utf8_east_mode
    exec 'set guifont=' . s:font_east
  else
    exec 'set guifont=' . s:font_west
  endif

  " Key mapping to switch the east/west UTF-8 mode
  nmap <F8>      :call UTF8_SwitchMode()<CR>
  imap <F8> <C-O>:call UTF8_SwitchMode()<CR>

  if has('autocmd')
    " Set the appropriate GUI font according to the fileencoding
    au BufWinEnter *  call UTF8_SetFont()
    " Not if user manually changed it (when switching between windows)
    au WinEnter    *  call UTF8_CheckAndSetFont()
  endif
endif

" Key mapping to toggle spelling check
if has('syntax')
  nmap <silent> <F7>      :setlocal spell!<CR>
  imap <silent> <F7> <C-O>:setlocal spell!<CR>
  let spellfile_path=FindRuntimeFile('spell/en.' . &encoding . '.add', 'w')
  if spellfile_path != ''
    exec 'nmap <M-F7> :sp ' . spellfile_path . '<CR><bar><C-W>_'
  endif
endif

if has('autocmd')
  function! SetFileEncodings(encodings)
    let b:my_fileencodings_bak=&fileencodings
    let &fileencodings=a:encodings
  endfunction

  function! RestoreFileEncodings()
    let &fileencodings=b:my_fileencodings_bak
    unlet b:my_fileencodings_bak
  endfunction

  function! GnuIndent()
    setlocal cinoptions=>4,n-2,{2,^-2,:2,=2,g0,h2,p5,t0,+2,(0,u0,w1,m1
    setlocal shiftwidth=2
    setlocal tabstop=8
  endfunction

  function! UpdateLastChangeTime()
    let last_change_anchor='\(" Last Change:\s\+\)\d\{4}-\d\{2}-\d\{2} \d\{2}:\d\{2}:\d\{2}'
    let last_change_line=search('\%^\_.\{-}\(^\zs' . last_change_anchor . '\)', 'n')
    if last_change_line != 0
      let last_change_time=strftime('%Y-%m-%d %H:%M:%S', localtime())
      let last_change_text=substitute(getline(last_change_line), '^' . last_change_anchor, '\1', '') . last_change_time
      call setline(last_change_line, last_change_text)
    endif
  endfunction

  function! RemoveTrailingSpace()
    if $VIM_HATE_SPACE_ERRORS != '0' &&
          \(&filetype == 'c' || &filetype == 'cpp' || &filetype == 'vim')
      normal m`
      silent! :%s/\s\+$//e
      normal ``
    endif
  endfunction

  " Set default file encoding(s) to the legacy encoding
  exec 'set fileencoding=' . legacy_encoding
  let &fileencodings=substitute(
                    \&fileencodings, '\<default\>', legacy_encoding, '')

  " Use the legacy encoding for CVS in cvsmenu (Vim script #1245)
  let CVScmdencoding=legacy_encoding
  " but the encoding of files in CVS is still UTF-8
  let CVSfileencoding='utf-8'

  " File patterns for automatic encoding detection (Vim script #1708)
  let $FENCVIEW_TELLENC='tellenc'       " See <URL:http://wyw.dcweb.cn/>
  let fencview_auto_patterns='*.txt,*.tex,*.htm{l\=},*.asp'
  let fencview_html_filetypes='html,aspvbs'

  " File types to use function echoing (Vim script #1735)
  let EchoFuncTagsLanguages=['c', 'cpp']

  " Do not use menu for NERD Commenter
  let NERDMenuMode=0

  " Highlight space errors in C/C++ source files (Vim tip #935)
  if $VIM_HATE_SPACE_ERRORS != '0'
    let c_space_errors=1
  endif

  " Tune for C highlighting
  let c_gnu=1
  let c_no_curly_error=1

  " Load doxygen syntax file for c/cpp/idl files
  let load_doxygen_syntax=1

  " Use Bitstream Vera Sans Mono as special code font in doxygen, which
  " is available at
  " <URL:http://ftp.gnome.org/pub/GNOME/sources/ttf-bitstream-vera/1.10/>
  " let doxygen_use_bitsream_vera=1

  " Let TOhtml output <PRE> and style sheet
  let html_use_css=1

  " Show syntax highlighting attributes of character under cursor (Vim
  " script #383)
  map <Leader>a :call SyntaxAttr()<CR>

  " Automatically find scripts in the autoload directory
  au FuncUndefined Syn* exec 'runtime autoload/' . expand('<afile>') . '.vim'

  " File type related autosetting
  au FileType c,cpp      setlocal cinoptions=:0,g0,(0,w1 shiftwidth=4 tabstop=4
  au FileType diff       setlocal shiftwidth=4 tabstop=4
  au FileType changelog  setlocal textwidth=76
  au FileType cvs        setlocal textwidth=72
  au FileType html,xhtml setlocal indentexpr=
  au FileType mail       setlocal expandtab softtabstop=2 textwidth=70
  au FileType python     set tabstop=4|set shiftwidth=4|set expandtab
  au FileType php        source ~/.vim/myconf/yunt-php.vim

  " Detect file encoding based on file type
  au BufReadPre  *.gb               call SetFileEncodings('cp936')
  au BufReadPre  *.big5             call SetFileEncodings('cp950')
  au BufReadPre  *.nfo              call SetFileEncodings('cp437')
  au BufReadPost *.gb,*.big5,*.nfo  call RestoreFileEncodings()

  " Quickly exiting help files
  au BufRead *.txt      if &buftype=='help'|nmap <buffer> q <C-W>c|endif

  " Setting for files following the GNU coding standard
  "au BufEnter D:/WuYongwei/cvssrc/socket++/*  call GnuIndent()
  "au BufEnter D:/mingw*             call GnuIndent()

  " Automatically update change time
  au BufWritePre *vimrc,*.vim       call UpdateLastChangeTime()

  " Remove trailing spaces for C/C++ and Vim files
  au BufWritePre *                  call RemoveTrailingSpace()

" looks for DokuWiki headlines in the first 20 lines
" of the current buffer
  fun IsDokuWiki()
    if match(getline(1,20),'^ \=\(=\{2,6}\).\+\1 *$') >= 0
      set textwidth=0
      set wrap
      set linebreak
      set filetype=dokuwiki
    endif
  endfun
" check for dokuwiki syntax
  autocmd BufWinEnter *.txt call IsDokuWiki()
" user name with which you want to login at the remote wiki
let g:DokuVimKi_USER = 'yunt'

" password
let g:DokuVimKi_PASS = '23531760'

" url of the remote wiki (without trailing '/')
let g:DokuVimKi_URL  = 'http://shangyunt.3322.org'

" set to yes if you want to be connected to your remote wiki every time you start vim
let g:DokuVimKi_AUTOCONNECT = 'no'

" the width of the page tree window (default=35)
let g:DokuVimKi_TREEWIDTH = '40'

" the width of the fold column of the page tree window (default=3) see :help foldcolumn for further information
let g:DokuVimKi_FOLDCOLWIDTH = '10'

endif
map <F2> :w<CR>:call CleanupBuffer(1)<CR>:noh<CR>
function! CleanupBuffer(keep)
    " Skip binary files
    if (&bin > 0)
        return
    endif

    " Remove spaces and tabs from end of every line, if possible
    silent! %s/\s\+$//ge

    " Save current line number
    let lnum = line(".")

    " number of last line
    let lastline = line("$")
    let n        = lastline

    " while loop
    while (1)
        " content of last line
        let line = getline(n)

        " remove spaces and tab
        if (!empty(line))
            break
        endif

        let n = n - 1
    endwhile

    " Delete all empty lines at the end of file
    let start = n+1+a:keep
    if (start < lastline)
        execute n+1+a:keep . "," . lastline . "d"
    endif

    " after clean spaces and tabs, jump back
    exec "normal " . lnum . "G"
endfunction
