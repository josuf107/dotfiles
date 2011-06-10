set nocompatible
"let g:zenburn_high_Contrast=1
"colors zenburn
"colorscheme zenburn
syntax on
set display=lastline
set number
set tabstop=2
set shiftwidth=2
set ts=2
set autoindent
set autowrite
set scrolloff=3
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
"set cursorline
set nocursorline
set ruler
set ttyfast
set rnu
set backspace=2

"updated protection
set noswapfile
set undodir=~/.vim/undodir
set undofile
set undolevels=1000
set undoreload=1000

let mapleader = ","

nnoremap / /\v
"vnoremap / /\v

set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>

nnoremap j gj
nnoremap k gk

noremap		;		:
noremap		:		;
inoremap kjh <esc>
inoremap <C-j> <C-v><TAB>
noremap	 <leader>m :call g:ToggleNuMode()<CR>
nnoremap <leader>p :wall<CR>:!sh process<CR>
nnoremap <leader>b :bn<CR>
nnoremap <leader>nt :NERDTreeToggle<CR>
nnoremap <leader>g :!gnome-open
nnoremap <leader>v :source ~/.vimrc<CR>
noremap <leader>rr :e ~/.vimrc<CR>gg/\v\<leader\>rt<CR>n2WcW
nnoremap <leader>rt :!make run-forktree > out<CR>
nnoremap <leader>rg :!make grade<CR>
nnoremap <leader>rm :make<CR>
nnoremap <leader>rj :!java strMatch --debug<CR>
nnoremap gw <C-W><C-W>
nnoremap gh <C-W>h
nnoremap gl <C-W>l
nnoremap gj <C-W>j
nnoremap gk <C-W>k
"Calculator stuff
nnoremap <leader>s V:!bc<CR>
nnoremap <leader>S "zyy"zpV:!bc<CR>
vnoremap <leader>s :!bc<CR>
vnoremap <leader>S "zy"zPgv:!bc<CR>
"LaTeX stuff (for now)
nnoremap <leader>dw :!pdflatex ./*.tex<CR>
nnoremap <leader>do :!evince ./*.pdf & <CR>

let g:user_zen_expandabbr_key = '<c-h>'

ab htt http://
ab <? <?php<CR><CR>?><UP>

"Auto commands
autocmd! BufnewFile * silent! 0r ~/.vim/frames/frame.%:e
autocmd Filetype java set smartindent
au BufWinLeave * mkview 
au BufWinEnter * loadview 
"For eclim:
let g:EclimHtmlValidate = 0
let g:EclimXmlValidate = 0

"this doesn't work in the jailshell...use messy style
filetype plugin on
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

"This is for java projects
set makeprg=ant\ -emacs\ -f\ ~/projects/javadev/build.xml

"functions:
function! g:ToggleNuMode()
	if(&rnu == 1)
		set nu
	else
		set rnu
	endif
endfunc

