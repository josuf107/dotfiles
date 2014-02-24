set nocompatible
"let g:zenburn_high_Contrast=1
"colors zenburn
"colorscheme zenburn
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

"unhighlight search
nnoremap <leader><space> :noh<cr>

"j and k go by screen lines in normal mode
"(I like the default behavior in visual mode, usually)
nnoremap j gj
nnoremap k gk

"I need the functionality of : far more often than ;
nnoremap		;		:
nnoremap		:		;

"In case I can't use caps lock
inoremap kjh <esc>

"For when tab completion gets annoying
inoremap <C-j> <space><space><space><space>

"Toggle numbering mode
noremap	 <leader>m :set rnu!<CR>

nnoremap <leader>p :wall<CR>:!sh process<CR>
nnoremap <leader>b :bn<CR>
nnoremap <leader>nt :NERDTreeToggle<CR>
let NERDTreeChDirMode = 2
let NERDChristmasTree = 1
nnoremap <leader>tl :TlistToggle<CR>
let Tlist_File_Fold_Auto_Close = 1
let Tlist_File_Close_On_Select = 1
nnoremap <leader>g :!gnome-open

"Re-source
nnoremap <leader>vv :source $MYVIMRC<CR>
nnoremap <leader>vo :new $MYVIMRC<CR>
"OS stuff

nnoremap <leader>rt :!make run-forktree > out<CR>
nnoremap <leader>rg :!make grade<CR>

"Java stuff
nnoremap <leader>rm :!ant<CR>
nnoremap <leader>rj :!ant test<CR>

"Better window nav
nnoremap gh <C-W>h
nnoremap gl <C-W>l
nnoremap gj <C-W>j
nnoremap gk <C-W>k

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

iabbrev @i jbarratt@indeed.com
iabbrev iab iabbrev
iabbrev @g joseph.m.barratt@gmail.com
iabbrev @@ joseph.barratt.us

let g:EclimLogLevel=2
let g:EclimSignLevel=2
augroup filetype_java
    autocmd!
    autocmd Filetype java set smartindent
    "Eclim basics
    autocmd Filetype java nnoremap <buffer> <localleader>ei :JavaImportMissing<CR>
    autocmd Filetype java nnoremap <buffer> <localleader>ec :JavaImportClean<CR>
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
    autocmd Filetype java nnoremap <buffer> <localleader>st :JUnitExecute<CR>
    "p for pretty
    autocmd Filetype java nnoremap <buffer> <localleader>sp :JavaFormat<CR>

    autocmd Filetype java :iabbrev <buffer> fi final
    autocmd Filetype java :iabbrev <buffer> final NOPENOPENOPE

    "Eclim insert mode
    autocmd Filetype java inoremap <buffer> <localleader><space> <C-X><C-U>
    autocmd Filetype java let g:SuperTabDefaultCompletionType = "<c-x><c-u>"
augroup END

augroup filetype_haskell
    autocmd!
    "Haskell stuff
    autocmd Filetype haskell nnoremap <buffer> <localleader>gt :GhcModType<CR>
    autocmd Filetype haskell nnoremap <buffer> <localleader>gc :GhcModCheck<CR>
    autocmd Filetype haskell nnoremap <buffer> <localleader>gl :GhcModLint<CR>
    autocmd Filetype haskell iabbrev <buffer> da data<space>=<space><left><left><left>
    autocmd Filetype haskell iabbrev <buffer> data NOPENOPENOPE
augroup END

augroup filetype_mail
    autocmd!
    autocmd Filetype mail setlocal tw=72
    autocmd Filetype mail setlocal spell
    autocmd Filetype mail setlocal nonumber
augroup END

augroup filetype_vim
    autocmd!
    autocmd Filetype vim setlocal keywordprg=:help
augroup END

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
filetype plugin on
"call pathogen#helptags()
call pathogen#runtime_append_all_bundles()
syntax enable
set background=dark
colorscheme solarized
"Omnicompletion
"set omnifunc=javacomplete#Complete
"inoremap <C-SPACE> <C-X><C-O>
"nnoremap <C-SPACE> <C-X><C-O>

"This is for java projects
"set makeprg=ant\ -emacs

noh
