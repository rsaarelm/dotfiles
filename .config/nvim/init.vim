" 2015-06-29: Migrating to Neovim, starting vimrc from scratch.

call plug#begin()

" Needed for changing font in Neovim-QT.
Plug 'equalsraf/neovim-gui-shim'
Plug 'easymotion/vim-easymotion'
Plug 'scrooloose/nerdtree'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
Plug 'timonv/vim-cargo'

call plug#end()

colorscheme delek

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
