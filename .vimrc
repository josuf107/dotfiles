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

let mapleader = ","

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
noremap	 <leader>m :call g:ToggleNuMode()<CR>

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
nnoremap <leader>vv :source ~/.vimrc<CR>
nnoremap <leader>vo :new ~/.vimrc<CR>

"OS stuff
nnoremap <leader>rt :!make run-forktree > out<CR>
nnoremap <leader>rg :!make grade<CR>

"Java stuff
nnoremap <leader>rm :!ant<CR>
nnoremap <leader>rj :!ant test<CR>

"latex stuff ('d' for document)
nnoremap <leader>dw :!pdflatex *.tex<CR>
nnoremap <leader>do :!evince *.pdf &<CR>

"Better window nav
nnoremap gh <C-W>h
nnoremap gl <C-W>l
nnoremap gj <C-W>j
nnoremap gk <C-W>k
nnoremap gw <C-W>=

"Calculator stuff
nnoremap <leader>a V:!bc<CR>
nnoremap <leader>A "zyy"zpV:!bc<CR>
vnoremap <leader>a :!bc<CR>
vnoremap <leader>A "zy"zPgv:!bc<CR>

"Block fold
nnoremap <leader>z zfiB<CR>

"Easy file search
nnoremap <leader>f :e **/

"Eclim basics
let g:EclimLogLevel=2
let g:EclimSignLevel=2
autocmd Filetype java nnoremap <leader>ei :JavaImport<CR>
autocmd Filetype java nnoremap <leader>es :JavaSearch -p<Space>
autocmd Filetype java nnoremap <leader>ed :JavaSearchContext<CR>
autocmd Filetype java nnoremap <leader>ea :Ant -Divy_initialized=true -Dresolve_run=true<CR>
autocmd Filetype java nnoremap <leader>eai :Ant<CR>
autocmd Filetype java nnoremap <leader>ead :Ant deploy -Divy_initialized=true -Dresolve_run=true<CR>
autocmd Filetype java nnoremap <leader>eadi :Ant deploy<CR>
"close preview window
autocmd Filetype java nnoremap <leader>ep :pclose<CR>
autocmd Filetype java nnoremap <leader>el :lclose<CR>
autocmd Filetype java 
"Eclim code stuff
"f for fix
autocmd Filetype java nnoremap <leader>sf :JavaCorrect<CR>
autocmd Filetype java nnoremap <leader>sc :JavaConstructor<CR>
autocmd Filetype java nnoremap <leader>si :JavaImpl<CR>
autocmd Filetype java nnoremap <leader>sd :JavaDelegate<CR>
autocmd Filetype java nnoremap <leader>sg :JavaGet<CR>
autocmd Filetype java nnoremap <leader>ss :JavaSet<CR>
autocmd Filetype java nnoremap <leader>sb :JavaGetSet<CR>
autocmd Filetype java nnoremap <leader>st :JUnitExecute<CR>
"p for pretty
autocmd Filetype java nnoremap <leader>sp :JavaFormat<CR>

"Eclim insert mode
autocmd Filetype java inoremap <C-Space> <C-X><C-U>
autocmd Filetype java let g:SuperTabDefaultCompletionType = "<c-x><c-u>"

"Haskell stuff
nnoremap <leader>gt :GhcModType<CR>
nnoremap <leader>gc :GhcModCheck<CR>
nnoremap <leader>gl :GhcModLint<CR>

let g:user_zen_expandabbr_key = '<c-h>'

ab htt http://
"ab <? <?php<CR>?><ESC>O
"ab { {<CR>}<UP>

"Auto commands
autocmd! BufnewFile * silent! 0r ~/.vim/frames/frame.%:e
autocmd Filetype java set smartindent

"Remember code folding
"au BufWinLeave * silent! mkview 
"au BufWinEnter * silent! loadview 

"For eclim:
let g:EclimHtmlValidate = 0
let g:EclimXmlValidate = 0

"this doesn't work in the jailshell... use messy style
filetype plugin on
call pathogen#helptags()
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

"functions:
function! g:ToggleNuMode()
    if(&rnu == 1)
        set nu
    else
        set rnu
    endif
endfunc

noh
