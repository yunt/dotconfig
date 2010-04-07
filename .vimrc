" vim:expandtab shiftwidth=2 tabstop=8 textwidth=72

" Wu Yongwei's _vimrc for Vim 7
" Last Change: 2010-04-07 21:51:20

" first the disabled features due to security concerns
set modelines=0                  " no modelines [http://www.guninski.com/vim1.html]
let g:secure_modelines_verbose=0 " securemodelines vimscript
let g:secure_modelines_modelines = 15 " 15 available modelines

" ---------------------------------------------------------------------------
" operational settings
syntax on
set ruler                     " show the line number on the bar
set more                      " use more prompt
set autoread                  " watch for file changes
set number                    " line numbers
set nohidden                  " close the buffer when I close a tab (I use tabs more than buffers)
set noautowrite               " don't automagically write on :next
set lazyredraw                " don't redraw when don't have to
set showmode                  " show the mode all the time
set showcmd                   " Show us the command we're typing
set nocompatible              " vim, not vi
set autoindent smartindent    " auto/smart indent
set expandtab                 " expand tabs to spaces (except java, see autocmd below)
set smarttab                  " tab and backspace are smart
set tabstop=6                 " 6 spaces
set shiftwidth=6              " shift width
set scrolloff=3               " keep at least 3 lines above/below
set sidescrolloff=5           " keep at least 5 lines left/right
set backspace=indent,eol,start  " backspace over all kinds of things
set showfulltag               " show full completion tags
set noerrorbells              " no error bells please
set linebreak                 " wrap at 'breakat' instead of last char
set tw=500                    " default textwidth is a max of 500
set cmdheight=1               " command line two lines high
set undolevels=500            " 500 undos
set updatecount=100           " switch every 100 chars
set complete=.,w,b,u,U,t,i,d  " do lots of scanning on tab completion
set ttyfast                   " we have a fast terminal
filetype on                   " Enable filetype detection
filetype indent on            " Enable filetype-specific indenting
filetype plugin on            " Enable filetype-specific plugins
compiler ruby                 " Enable compiler support for ruby
set wildmode=longest:full     " *wild* mode
set wildignore+=*.o,*~,.lo    " ignore object files
set wildmenu                  " menu has tab completion
let maplocalleader=','        " all my macros start with ,
" Deprecated, using SimpleFold with '\f' now. ,sf to revert
"set foldmethod=syntax         " fold on syntax automagically, always
"set foldcolumn=2              " 2 lines of column for fold showing, always
set whichwrap+=<,>,h,l        " backspaces and cursor keys wrap to
set magic                     " Enable the "magic"
set visualbell t_vb=          " Disable ALL bells
set cursorline                " show the cursor line
set matchpairs+=<:>           " add < and > to match pairs
set tags=tags;/               " search recursively up for tags
set helplang=cn
set fencs=ucs-bom,utf-8,chinese
set encoding=utf-8

" highlight over 80 columns
"highlight OverLength ctermbg=darkred ctermfg=white guibg=#FFD9D9
highlight OverLength cterm=reverse
match OverLength /\%81v.*/

if has('autocmd')
  " Remove ALL autocommands for the current group
  au!
endif

if !has("gui_running")
      "colorscheme candycode   " yum candy

      " I pretty much only like this scheme if I can use SIMBL with terminal
      " colors:
      " (http://www.culater.net/software/TerminalColors/TerminalColors.php)
      " to change the really hard-to-read dark blue into a lighter shade.
      " Or; Use iterm with Tango colors
      colorscheme desert256
      "colorscheme rdark
end
if has("gui_running")
      "colorscheme rdark
      colorscheme tango-desert
      let rdark_current_line=1  " highlight current line
      set background=dark
      set noantialias
      set guioptions-=T        " no toolbar
      set guioptions-=l        " no left scrollbar
      set guioptions-=L        " no left scrollbar
      set guioptions-=r        " no right scrollbar
      set guioptions-=R        " no right scrollbar
      set lines=64
      set columns=135
      "set transparency=0
      set gfn=Monaco\ 10
      set clipboard=unnamed
      let do_syntax_sel_menu=1 " Always show file types in menu
end

if has('multi_byte')
  " Legacy encoding is the system default encoding
  let legacy_encoding=&encoding
endif

"{{{插件设置
" Settings for NERD_Tree
let NERDTreeWinPos="left"
let NERDTreeWinSize=35

" Settings for taglist.vim
let Tlist_Use_Right_Window=1
let Tlist_Auto_Open=0
let Tlist_Enable_Fold_Column=0
let Tlist_Show_One_File = 1         " Only show tags for current buffer
let Tlist_Compact_Format=0
let Tlist_WinWidth=28
let Tlist_Exit_OnlyWindow=1
let Tlist_File_Fold_Auto_Close = 1

" Settings for :TOhtml
let html_number_lines=1
let html_use_css=1
let use_xhtml=1

" Settings for NeoComplCahce
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
"}}}

source $VIMRUNTIME/vimrc_example.vim
"if has('gui_running')
  "source $VIMRUNTIME/mswin.vim
  "unmap  <C-Y>|             " <C-Y> for Redo is kept in insert mode
  "iunmap <C-A>|             " <C-A> for Select-All is kept in normal mode
  "" Key mapping to switch windows quickly (<C-Tab> is already mapped)
  "nnoremap <C-S-Tab> <C-W>W
  "inoremap <C-S-Tab> <C-O><C-W>W
"endif

set formatoptions+=mM
set fileencodings=ucs-bom,utf-8,default,latin1          " default value

" ---------------------------------------------------------------------------
" status line
set laststatus=2
if has('statusline')
      " Status line detail: (from Rafael Garcia-Suarez)
      " %f              file path
      " %y              file type between braces (if defined)
      " %([%R%M]%)      read-only, modified and modifiable flags between braces
      " %{'!'[&ff=='default_file_format']}
      "                 shows a '!' if the file format is not the platform
      "                 default
      " %{'$'[!&list]}  shows a '*' if in list mode
      " %{'~'[&pm=='']} shows a '~' if in patchmode
      " (%{synIDattr(synID(line('.'),col('.'),0),'name')})
      "                 only for debug : display the current syntax item name
      " %=              right-align following items
      " #%n             buffer number
      " %l/%L,%c%V      line number, total number of lines, and column number

      function! SetStatusLineStyle()
            "let &stl="%f %y "                       .
            "\"%([%R%M]%)"                   .
            "\"%#StatusLineNC#%{&ff=='unix'?'':&ff.'\ format'}%*" .
            "\"%{'$'[!&list]}"               .
            "\"%{'~'[&pm=='']}"              .
            "\"%="                           .
            "\"#%n %l/%L,%c%V "              .
            "\""
            "      \"%#StatusLineNC#%{GitBranchInfoString()}%* " .
            let &stl="%F%m%r%h%w\ [%{&ff}]\ [%Y]\ %P\ %=[a=\%03.3b]\ [h=\%02.2B]\ [%l,%v]"
      endfunc
      " Not using it at the moment, using a different one
      call SetStatusLineStyle()

      if has('title')
            set titlestring=%t%(\ [%R%M]%)
      endif

      "highlight StatusLine    ctermfg=White ctermbg=DarkBlue cterm=bold
      "highlight StatusLineNC  ctermfg=White ctermbg=DarkBlue cterm=NONE
endif

"set statusline=%{GitBranch()}%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [%{(&fenc==\"\")?&enc:&fenc}%{(&bomb?\",BOM\":\"\")}]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]

" Personal setting for working with Windows NT/2000/XP (requires tee in path)
if &shell =~? 'cmd'
  "set shellxquote=\"
  set shellpipe=2>&1\|\ tee
endif

" Quote shell if it contains space and is not quoted
if &shell =~? '^[^"].* .*[^"]'
  let &shell='"' . &shell . '"'
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
let g:DokuVimKi_PASS = ''

" url of the remote wiki (without trailing '/')
let g:DokuVimKi_URL  = 'http://wiki.lazyhack.net'

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

"{{{ 特定语言相关/插件
    "{{{ php
    "-------------------------------------------------------------------------------------
        "{{{ main
        "-------------------------------------------------------------------------------------
        function! PHPsynCHK()
            let winnum =winnr() " get current window number
            silent make -l %
            cw " open the error window if it contains error
            " return to the window with cursor set on the line of the first error (if any)
            execute winnum . "wincmd w"
        endfunction

        function! SetCscope()
            if has("cscope")
                "set csprg=/usr/local/bin/cscope
                setl cscopequickfix=s-,c-,d-,i-,t-,e-
                setl csto=1
                setl cst
                setl nocsverb
                " add any database in current directory
                if filereadable("cscope.out")
                    cs add cscope.out
                    " else add database pointed to by environment
                elseif $CSCOPE_DB != ""
                    cs add $CSCOPE_DB
                endif
                set csverb
            endif

        endfunction

        function! SetPHP()
            "call SetCscope()
            :setl makeprg=php
            :setl errorformat=%m\ in\ %f\ on\ line\ %l
            ":setl dictionary=$VIMFILES/dict/php.dict
            let b:match_debug = 1

            " Map <CTRL>-P to check the file for syntax
            :noremap <C-P> :call PHPsynCHK()<CR>
            call TSkeletonMapGoToNextTag()
            "AutoComplPopDisable
        endfunction
        autocmd FileType php :call SetPHP()
        "----------------------------------------------------------------------------------}}}
        "{{{ php.vim 语法文件
        "-------------------------------------------------------------------------------------
        let php_large_file = 3000
        "let php_smart_members = 0
        "let php_smart_semicolon = 0
        "let php_show_pcre = 0
        let php_folding=1                   " 使用代码折叠
        let php_strict_blocks=1             "
        let php_fold_arrays=1               " 折叠数组
        let php_baselib=1                   " 高亮基础函数库
        let php_sql_query = 1               " 高亮字符串中的SQL关键字
        let php_htmlInStrings = 0           " 不高亮字符串中的HTML关键字
        let php_alt_properties = 0          "
        let php_highlight_quotes = 1
        let PHP_autoformatcomment = 1       " 自动格式注释
        let php_sync_method = -1
        "let g:AutoComplPop_NotEnableAtStartup = 1
        "----------------------------------------------------------------------------------}}}
    "----------------------------------------------------------------------------------}}}
