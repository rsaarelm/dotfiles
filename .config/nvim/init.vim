" 2015-06-29: Migrating to Neovim, starting vimrc from scratch.

" Use ~/.vim subdirectory even when on Windows
" TODO: Fix this for nvim
"if has('win32') || has('win64')
"    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
"endif

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

set ignorecase
set smartcase

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
set langmap=ki,ik,KI,IK,ej,je

" Faster window navigation
nnoremap <C-e> <C-W>e
nnoremap <C-i> <C-W>i
nnoremap <C-l> <C-W>l
nnoremap <C-h> <C-W>h

" Faster tab navigation
nnoremap <C-k> :tabp<cr>
nnoremap <C-m> :tabn<cr>

" No shift for command-line
map ; :

" Make the visual block mode the default
nnoremap v <C-v>|xnoremap v <C-v>
nnoremap <C-v> v|xnoremap <C-v> v
