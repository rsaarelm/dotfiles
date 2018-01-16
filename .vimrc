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

if has('win32') || has('win64')
    Plug 'junegunn/fzf', { 'dir': '~/.fzf' }
else
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
endif

Plug 'junegunn/fzf.vim'

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
Plug 'vimoutliner/vimoutliner'

call plug#end()

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

" Nicer tab completion
set wildmode=longest,list,full
set wildmenu

filetype off
filetype plugin indent on

" Show trailing whitespace and physical tabs.
set encoding=utf-8
set listchars=tab:›…,trail:·
set list
" Highlight trailing whitespace.
match Error '\s\+$'

set colorcolumn=81

autocmd FileType text setlocal textwidth=78
autocmd BufNewFile,BufRead *.md set filetype=markdown

" Set up nice wrapping for long lines
autocmd FileType votl setlocal wrap linebreak breakindent breakindentopt+=shift:3 colorcolumn=0 foldlevel=0 textwidth=100 tabstop=2 shiftwidth=2

" todo.txt file niceties
autocmd FileType todo setlocal wrap linebreak colorcolumn=0 breakindent breakindentopt+=shift:2 formatoptions-=t

autocmd FileType text setlocal spell

" Rust cmds
autocmd FileType rust nmap gd <Plug>(rust-def)
autocmd FileType rust nmap gs <Plug>(rust-def-split)
autocmd FileType rust nmap gx <Plug>(rust-def-vertical)
autocmd FileType rust nmap <leader>gd <Plug>(rust-doc)
autocmd FileType rust setlocal colorcolumn=101
autocmd FileType rust nmap <F5> :CargoRun --release<cr>

" Fix for nightly rustfmt
let g:rustfmt_command = "cargo fmt -- "

" Save manual folds
" (Disabled, seems to cause more trouble than it's worth)
" autocmd BufWinLeave *.* mkview
" autocmd BufWinEnter *.* silent loadview

" Unify tabs and remove trailing whitespace.
command! WhiteClean retab | %s/\s\+$

" Expression-based folding in jrnl files
command! JrnlFold setlocal foldexpr=getline(v:lnum)=~'^\\d\\d\\d\\d-\\d\\d-\\d\\d\\s\\d\\d:\\d\\d\\s.*'?'>1':1 foldmethod=expr

autocmd FileType python setl noexpandtab softtabstop=0 shiftwidth=0 tabstop=4

autocmd BufRead,BufNewFile *.yaml setl softtabstop=2 shiftwidth=2

" Despite being .txt, CMakeLists shouldn't be line wrapped.
autocmd BufRead,BufNewFile CMakeLists.txt setl formatoptions-=t

autocmd FileType make setl noexpandtab

" Timestamp abbreviation
iabbr tsp <C-r>=strftime("%Y-%m-%d")<cr>
iabbr tspt <C-r>=strftime("%Y-%m-%d %H:%M")<cr>
iabbr wsp <C-r>=strftime("%G%V.%u")<cr>

" Red error message
iabbr ansired \x1b[31;1m\x1b[0m<Left><Left><Left><Left><Left><Left><Left>

" Up and down are used a lot, bind them to Colemak N and E.
" Also make the default behavior move by visual, not logical line,
" require the g modifier for logical lines (the opposite of the regular
" setting).
nnoremap gn j|xnoremap gn j|onoremap gn j|
nnoremap n gj|xnoremap n gj|onoremap n gj|
" Force fold opening with zv to reproduce behavior of unmapped n
nnoremap k nzv|xnoremap k nzv|onoremap k nzv|
nnoremap K Nzv|xnoremap K Nzv|onoremap K Nzv|

nnoremap ge k|xnoremap ge k|onoremap ge k|
nnoremap e gk|xnoremap e gk|onoremap e gk|
nnoremap j e|xnoremap j e|onoremap j e|

" Remap NERDtree stuff to enable our hacked navigation there.
let NERDTreeMapOpenExpl='j'

" Faster window navigation
noremap <C-h> <C-W>h
noremap <C-l> <C-W>l
noremap <C-n> <C-W>j
noremap <C-e> <C-W>k

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

" Fzf fuzzy find
nmap <Leader>B :Buffers<cr>

" More useful enter and backspace
nnoremap <BS> {
onoremap <BS> {
vnoremap <BS> {

nnoremap <expr> <CR> empty(&buftype) ? '}' : '<CR>'
onoremap <expr> <CR> empty(&buftype) ? '}' : '<CR>'
vnoremap <CR> }

" Easymotion navigation
nmap F <Plug>(easymotion-prefix)s

" Retain selection when indenting
vnoremap < <gv
vnoremap > >gv

"""""""""""""""""""""""""""""""" Color settings

colorscheme ron

highlight ColorColumn ctermbg=darkblue guibg=#2c2d27
highlight Error ctermbg=brown guibg=#331100
