set nocompatible

" Vundle {{{1
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()
Bundle 'gmarik/vundle'

Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'
Bundle 'altercation/vim-colors-solarized'
Bundle 'majutsushi/tagbar'
Bundle 'Shougo/vimproc'
Bundle 'eagletmt/ghcmod-vim'

filetype plugin indent on

" Common Settings {{{1
set display=lastline
set number
set tabstop=4
set shiftwidth=4
set ts=4
set expandtab
set softtabstop=0
set autoindent
set autowrite
set scrolloff=3
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
"Patrick hates cursor line
"set cursorline
set nocursorline
set ruler
set ttyfast
set backspace=2
set noswapfile
set t_Co=16
set path=.,/usr/include,**,

let mapleader = "-"
let maplocalleader = ","

nnoremap / /\v
vnoremap / /\v

set ignorecase
set smartcase
set gdefault
set showmatch
set hlsearch
set incsearch
set lcs=trail:.,tab:>-
set list

" Global Keymap {{{1
"unhighlight search
nnoremap <leader><space> :noh<cr>

"j and k go by screen lines in normal mode
"(I like the default behavior in visual mode, usually)
nnoremap j gj
nnoremap k gk

"I need the functionality of : far more often than ;
nnoremap        ;       :
nnoremap        :       ;
vnoremap        ;       :
vnoremap        :       ;

"In case I can't use caps lock
inoremap kjh <esc>

"For when tab completion gets annoying
inoremap <C-j> <space><space><space><space>

"Toggle numbering mode
noremap	 <leader>m :set rnu!<CR>

"Re-source
nnoremap <leader>vv :source $MYVIMRC<CR>
nnoremap <leader>vo :new $MYVIMRC<CR>
nnoremap <leader>v; :execute "normal! :new $MYVIMRC\ro\e\":p"<CR>

" Better nav {{{2
nnoremap <leader>nt :NERDTreeToggle<CR>
let NERDTreeChDirMode = 2
let NERDChristmasTree = 1
nnoremap gh <C-W>h
nnoremap gl <C-W>l
nnoremap gj <C-W>j
nnoremap gk <C-W>k
nnoremap <leader>p :vsp<CR>
nnoremap <leader>b :bn<CR>
nnoremap <leader>q :q<CR>

"Calculator stuff
nnoremap <leader>a V:!bc<CR>
nnoremap <leader>A "zyy"zpV:!bc<CR>
vnoremap <leader>a :!bc<CR>
vnoremap <leader>A "zy"zPgv:!bc<CR>

"Block fold
nnoremap <leader>z zfiB<CR>

"Easy file search
nnoremap <leader>f :e **/

inoremap <c-u> <esc>gUiwea
nnoremap <c-u> gUiwe
vnoremap <leader>" <esc>`<i"<esc>`>la"<esc>

"Tags
nnoremap <leader>ta :ta<space>
nnoremap <leader>tl :TagbarToggle<CR>
let Tlist_File_Fold_Auto_Close = 1
let Tlist_File_Close_On_Select = 1

" Global Abbreviations {{{1
iabbrev @i jbarratt@indeed.com
iabbrev iab iabbrev
iabbrev @g joseph.m.barratt@gmail.com
iabbrev @@ joseph.barratt.us

let g:EclimLogLevel=2
let g:EclimSignLevel=2

" Filetype Java {{{1
augroup filetype_java
    autocmd!
    autocmd Filetype java setlocal smartindent
    autocmd Filetype java setlocal iskeyword=@,48-57,_,192-255,@-@
    autocmd Filetype java setlocal foldmethod=indent
    autocmd Filetype java setlocal nofoldenable
    "Eclim basics
    autocmd Filetype java nnoremap <buffer> <localleader>ei :JavaImport<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>es :JavaSearch -p<Space>
    autocmd Filetype java nnoremap <buffer> <localleader>ed :JavaSearchContext<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>ea :Ant -Divy_initialized=true -Dresolve_run=true<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>eai :Ant<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>ead :Ant deploy -Divy_initialized=true -Dresolve_run=true<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>eadi :Ant deploy<CR>
    "close preview window
    autocmd Filetype java nnoremap <buffer> <localleader>ep :pclose<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>el :lclose<CR>
    "Eclim code stuff
    "f for fix
    autocmd Filetype java nnoremap <buffer> <localleader>sf :JavaCorrect<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>sc :JavaConstructor<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>si :JavaImpl<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>sd :JavaDelegate<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>sg :JavaGet<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>ss :JavaSet<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>sb :JavaGetSet<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>st :JUnit<CR>
    "p for pretty
    autocmd Filetype java nnoremap <buffer> <localleader>sp :JavaFormat<CR>

    "Misc.
    autocmd Filetype java nnoremap <buffer> <localleader>vv :source $MYVIMRC<CR>:set ft=java<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>; A;<ESC>
    autocmd Filetype java nnoremap <buffer> <localleader>cf :call <SID>JavaConstructorField()<CR>
    autocmd Filetype java inoremap <buffer> <c-f> <ESC>:call <SID>JavaConstructorField()<CR>o
    autocmd Filetype java nnoremap <buffer> <localleader>t :vsp<CR>:execute "tag " . expand("%:t:r") . "Test"<CR>
    "Java words what what!
    autocmd Filetype java noremap <buffer> <localleader>w :<c-u>call <SID>JavaWord(1)<CR>
    autocmd Filetype java noremap <buffer> <localleader>b :<c-u>call <SID>JavaWord(0)<CR>
    "Parameters
    autocmd Filetype java nnoremap <buffer> <localleader>h :execute "normal! ?,\\\\|(\rnw:nohl\r"<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>l f,w
    "Move by methods
    autocmd Filetype java nnoremap <buffer> <localleader>m ]m
    autocmd Filetype java nnoremap <buffer> <localleader>p [m
    autocmd Filetype java onoremap <buffer> im :<c-u>execute "normal! [mwv]m[Mb"<CR>

    "Shortcuts
    autocmd Filetype java :iabbrev <buffer> fi final
    autocmd Filetype java :iabbrev <buffer> final NOPENOPENOPE
    autocmd Filetype java :iabbrev <buffer> pri private
    autocmd Filetype java :iabbrev <buffer> private NOPENOPENOPE
    autocmd Filetype java :iabbrev <buffer> pro protected
    autocmd Filetype java :iabbrev <buffer> protected NOPENOPENOPE
    autocmd Filetype java :iabbrev <buffer> pu public
    autocmd Filetype java :iabbrev <buffer> public NOPENOPENOPE
    autocmd Filetype java :iabbrev <buffer> st static
    autocmd Filetype java :iabbrev <buffer> static NOPENOPENOPE
    autocmd Filetype java :iabbrev <buffer> @nn @Nonnull
    autocmd Filetype java :iabbrev <buffer> @Nonnull NOPENOPENOPE
    autocmd Filetype java :iabbrev <buffer> @n @Nullable
    autocmd Filetype java :iabbrev <buffer> @Nullable NOPENOPENOPE
    autocmd Filetype java :iabbrev <buffer> St String
    autocmd Filetype java :iabbrev <buffer> String NOPENOPENOPE

    "Eclim insert mode
    autocmd Filetype java inoremap <buffer> <C-SPACE> <C-X><C-U>
    autocmd Filetype java let g:SuperTabDefaultCompletionType = "<c-x><c-u>"
augroup END

" Java Functions {{{1
function! <SID>JavaConstructorField()
    let temp = @"
    yank
    normal Ithis.
    normal A = 
    put
    normal k
    join
    normal A;
    let @" = temp
endfunction

function! <SID>JavaWord(forward)
    if a:forward
        let opts = ''
    else
        let opts = 'b'
    endif
    let s = search('\v[A-Z]|(<.)', opts)
endfunction

" Filetype Haskell {{{1
augroup filetype_haskell
    autocmd!
    "Haskell stuff
    autocmd Filetype haskell nnoremap <buffer> <localleader>gt :GhcModType<CR>
    autocmd Filetype haskell nnoremap <buffer> <localleader>gc :GhcModCheck<CR>
    autocmd Filetype haskell nnoremap <buffer> <localleader>gl :GhcModLint<CR>
    autocmd Filetype haskell iabbrev <buffer> da data<space>=<space><left><left><left>
    autocmd Filetype haskell iabbrev <buffer> data NOPENOPENOPE
augroup END

" Filetype Markdown {{{1
augroup filetype_markdown
    autocmd!
    autocmd Filetype markdown setlocal tw=72
    autocmd Filetype markdown setlocal spell
    autocmd Filetype markdown setlocal makeprg=markdown\ %\ >\ %:r.html
    autocmd Filetype markdown nnoremap <buffer> <localleader>sh "zyy"zpVr-
    autocmd Filetype markdown nnoremap <buffer> <localleader>h "zyy"zpVr=
    autocmd Filetype markdown inoremap <buffer> * *<space><space><space>
augroup END

" Filetype Mail {{{1
augroup filetype_mail
    autocmd!
    autocmd Filetype mail setlocal tw=72
    autocmd Filetype mail setlocal spell
    autocmd Filetype mail setlocal nonumber
augroup END

" FileType Vim {{{1
augroup filetype_vim
    autocmd!
    autocmd Filetype vim setlocal keywordprg=:help
    autocmd FileType vim setlocal foldmethod=marker
    autocmd FileType vim inoremap <buffer> <c-h> <esc>I"<space><esc>A<space><esc>3a{<esc>
    autocmd FileType vim nnoremap <buffer> <localleader>h1 I"<space><esc>A<space><esc>3a{<esc>a1<esc>
    autocmd FileType vim nnoremap <buffer> <localleader>h2 I"<space><esc>A<space><esc>3a{<esc>a2<esc>
augroup END

" Legacy {{{1
let g:user_zen_expandabbr_key = '<c-h>'

ab htt http://
"ab <? <?php<CR>?><ESC>O
"ab { {<CR>}<UP>

"Auto commands
autocmd! BufnewFile * silent! 0r ~/.vim/frames/frame.%:e

"Remember code folding
"au BufWinLeave * silent! mkview 
"au BufWinEnter * silent! loadview 

"For eclim:
let g:EclimHtmlValidate = 0
let g:EclimXmlValidate = 0

"this doesn't work in the jailshell... use messy style
"set background=dark
"colorscheme solarized
"Omnicompletion
"set omnifunc=javacomplete#Complete
"inoremap <C-SPACE> <C-X><C-O>
"nnoremap <C-SPACE> <C-X><C-O>

noh
