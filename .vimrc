" 2015-06-29: Migrating to Neovim, starting vimrc from scratch.

" Use ~/.vim subdirectory even when on Windows
if has('win32') || has('win64')
    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

call plug#begin()

" Needed for changing font in Neovim-QT.
Plug 'equalsraf/neovim-gui-shim'

" Ergonomics
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-unimpaired'

" Files and stuff
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'

" Rust
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
Plug 'timonv/vim-cargo'

" Misc
Plug 'freitass/todo.txt-vim'
Plug 'tpope/vim-sensible'

call plug#end()

" TODO: Get this right somehow for both terminal-based and GUI nvim.
"colorscheme blue

let mapleader=" "

set relativenumber

set expandtab
set softtabstop=4
set shiftwidth=4

set nojoinspaces

set ruler
set hidden
set guioptions=ac
set guicursor+=a:blinkon0

set ignorecase
set smartcase
set hlsearch

filetype plugin indent on

autocmd FileType text setlocal textwidth=78

" Unify tabs and remove trailing whitespace.
command! WhiteClean retab | %s/\s\+$

" Timestamp abbreviation
iabbr tsp <C-r>=strftime("%Y-%m-%d")<cr>
iabbr tspt <C-r>=strftime("%Y-%m-%d %H:%M")<cr>

" Colemak navigation hack
" (Only remap vertical movement keys, there are smarter ways to move
" horizontally.)
" Langmap is nice and concise, but seems to break unimpaired somehow
"set langmap=ki,ik,KI,IK,ej,je
nnoremap ge j|xnoremap ge j|onoremap ge j|
nnoremap gi k|xnoremap gi k|onoremap gi k|
nnoremap e gj|xnoremap e gj|onoremap e gj|
nnoremap i gk|xnoremap i gk|onoremap i gk|
nnoremap I K|xnoremap I K|onoremap I K|
nnoremap j e|xnoremap j e|onoremap j e|
nnoremap k i|xnoremap k i|onoremap k i|
nnoremap K I|xnoremap K I|onoremap K I|

" Remap NERDtree stuff to enable our hacked navigation there.
let NERDTreeMapOpenExpl='j'
let NERDTreeMapOpenSplit='k'

" Faster window navigation
noremap <C-h> <C-W>h
noremap <C-l> <C-W>l
nnoremap <C-e> <C-W>j
nnoremap <C-i> <C-W>k
" langmap versions.
"noremap <C-e> <C-W>e
"noremap <C-i> <C-W>i


" Faster tab navigation
nnoremap <C-k> :tabp<cr>
nnoremap <C-m> :tabn<cr>

" No shift for command-line
map ; :

" Make the visual block mode the default
nnoremap v <C-v>|xnoremap v <C-v>
nnoremap <C-v> v|xnoremap <C-v> v
nnoremap <C-q> v|xnoremap <C-q> v
