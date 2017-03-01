" 2015-06-29: Migrating to Neovim, starting vimrc from scratch.

" Use ~/.vim subdirectory even when on Windows
if has('win32') || has('win64')
    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

call plug#begin()

" Needed for changing font in Neovim-QT.
"Plug 'equalsraf/neovim-gui-shim'

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

" Input modes
Plug 'dhruvasagar/vim-table-mode'

" Misc
Plug 'freitass/todo.txt-vim'
Plug 'tpope/vim-sensible'
Plug 'altercation/vim-colors-solarized'
Plug 'lukaszkorecki/workflowish'

call plug#end()

" TODO: Get this right somehow for both terminal-based and GUI nvim.
"colorscheme blue

let mapleader=" "

set number

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

set lazyredraw " Don't update screen when running macros
set ttyfast

set visualbell

set nowrap

" Don't clobber working directory with swap files.
set directory=~/.vim/swap//
set backupdir=~/.vim/backup//

set virtualedit=block

filetype plugin indent on

autocmd FileType text setlocal textwidth=78
autocmd BufNewFile,BufRead *.md set filetype=markdown
autocmd FileType workflowish setlocal wrap linebreak breakindent breakindentopt+=shift:2

" Unify tabs and remove trailing whitespace.
command! WhiteClean retab | %s/\s\+$

" Timestamp abbreviation
iabbr tsp <C-r>=strftime("%Y-%m-%d")<cr>
iabbr tspt <C-r>=strftime("%Y-%m-%d %H:%M")<cr>

" Red error message
iabbr ansired \33[31;1m\33[0m<Left><Left><Left><Left><Left><Left>

" Navigate using neio, make Colemak layout work
nnoremap ge j|xnoremap ge j|onoremap ge j|
nnoremap e gj|xnoremap e gj|onoremap e gj|
nnoremap j e|xnoremap j e|onoremap j e|

nnoremap gi k|xnoremap gi k|onoremap gi k|
nnoremap i gk|xnoremap i gk|onoremap i gk|
nnoremap I K|xnoremap I K|onoremap I K|

nnoremap n h|xnoremap n h|onoremap n h|
nnoremap N H|xnoremap N H|onoremap N H|
" Force fold opening with zv to reproduce behavior of unmapped n
nnoremap h nzv|xnoremap h nzv|onoremap h nzv|
nnoremap H N|xnoremap H N|onoremap H N|

nnoremap o l|xnoremap o l|onoremap o l|
nnoremap O L|xnoremap O L|onoremap O L|

nnoremap k o|xnoremap k o|onoremap k o|
nnoremap K O|xnoremap K O|onoremap K O|
nnoremap l i|xnoremap l i|onoremap l i|
nnoremap L I|xnoremap L I|onoremap L I|

" Remap NERDtree stuff to enable our hacked navigation there.
let NERDTreeMapOpenExpl='j'
let NERDTreeMapOpenSplit='k'

" Faster window navigation
noremap <C-n> <C-W>h
noremap <C-o> <C-W>l
noremap <C-e> <C-W>j
noremap <C-i> <C-W>k

" Remap clobbered back-jumping.
noremap <C-h> <C-o>

" No shift for command-line
map ; :

" Make the visual block mode the default
nnoremap v <C-v>|xnoremap v <C-v>
nnoremap <C-v> v|xnoremap <C-v> v
nnoremap <C-q> v|xnoremap <C-q> v

" :E gets clobbered by some other commands.
cabbrev E Explore

" C-] is broken for some reason on Windows with default Colemak layout.
" Provide alternative
if has("win32")
    map <F3> <C-]>
    map <F4> g<C-]>
endif
