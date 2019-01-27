let g:mapleader = "\<Space>"

" Plugin Manager
" ================================
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
call plug#begin('~/.nvim/plugged')

" Plugins
" ================================

" NerdTree
Plug 'scrooloose/nerdtree'
map <S-TAB> :NERDTreeFind<cr><c-w><c-p>
map <TAB> :call NERDTreeToggleAndFind()<cr>
function! NERDTreeToggleAndFind()
    if (exists('t:NERDTreeBufName') && bufwinnr(t:NERDTreeBufName) != -1)
        execute ':NERDTreeClose'
    else
        if filereadable(expand('%:p'))
            execute ':NERDTreeFind'
        else
            execute ':NERDTree'
        endif
        execute ':wincmd p'
    endif
    endfunction
let NERDTreeMapOpenExpl = 'j'  " Enable using Colemak vertical navigation
let NERDTreeIgnore = ['\.pyc$', '__pycache__'] " Don't show junk files
let NERDTreeShowHidden = 1  " Show dotfiles

" Fugitive for Git
Plug 'tpope/vim-fugitive'

" Table mode
Plug 'dhruvasagar/vim-table-mode'

" todo.txt
Plug 'freitass/todo.txt-vim'
" Make it recognize the third file in my todo workflow.
autocmd BufNewFile,BufRead [Ss]omeday.txt set filetype=todo

" Fuzzy Finder
if has('win32') || has('win64')
    Plug 'junegunn/fzf', { 'dir': '~/.fzf' }
else
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
endif
Plug 'junegunn/fzf.vim'
" Use FZF to change directory.
command! -nargs=* -complete=dir Cd call fzf#run(fzf#wrap({'source': 'find '.(empty(<q-args>) ? '.' : <q-args>).' -type d', 'sink': 'cd'}))
nnoremap <silent> <leader><space> :Files<CR>
nnoremap <silent> <leader>a :Buffers<CR>
nnoremap <silent> <leader>; :BLines<CR>
nnoremap <silent> <leader>o :BTags<CR>
nnoremap <silent> <leader>O :Tags<CR>
" Use git file list in git projects
let $FZF_DEFAULT_COMMAND = '
 \ (git ls-tree -r --name-only HEAD ||
 \ find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
    \ sed s/^..//) 2> /dev/null'

" Vimoutliner
Plug 'vimoutliner/vimoutliner'

" Rust support
Plug 'rust-lang/rust.vim'
let g:rustfmt_command = 'rustfmt +nightly'

" Nix file format
Plug 'LnL7/vim-nix'

Plug 'w0rp/ale'

" Language Client
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
let g:LanguageClient_serverCommands = {
    \ 'rust': ['env', 'rustup', 'run', 'nightly', 'rls'],
    \ 'python': ['env', 'pyls'],
    \ 'c': ['/home/rsaarelm/local/cquery/build/release/bin/cquery',
    \ '--log-file=/tmp/cq.log',
    \ '--init={"cacheDirectory":"/var/cquery/"}'],
    \ 'cpp': ['/home/rsaarelm/local/cquery/build/release/bin/cquery',
    \ '--log-file=/tmp/cq.log',
    \ '--init={"cacheDirectory":"/var/cquery/"}'],
    \ 'go': ['env', 'go-langserver'],
    \ }
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <silent> gD :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent> <leader>s :call LanguageClient#workspace_symbol()<CR>

" Asynchronous completion framework
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

call plug#end()


" Settings
" ================================
set virtualedit=block  " Free block placement in visual mode

set ignorecase      " Default search is case-insensitive
set smartcase       " ...except when you write caps in the search expression

set nojoinspaces    " Two spaces after a period is an abomination
set colorcolumn=81  " Show the forbidden zone
set wrap            " Show long lines by default
set textwidth=78

set listchars=tab:›…,trail:·  " Show trailing whitespace
set list

set lazyredraw      " Don't update screen when running macros
set hlsearch        " Highlight search results

set expandtab       " Don't use physical tabs as a rule
set softtabstop=-1  " Use shiftwidth everywhere
set shiftwidth=4
set shiftround      " Snap to shiftwidth

set noswapfile      " Don't create swap files

set guioptions=ac   " Minimize GUI cruft
set guicursor+=a:blinkon0

set hidden          " Allow unsaved stuff in background buffers

set wildmode=longest,list,full  " Don't tab complete wrong options

" Key mapping
" ================================

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

" Retain selection when indenting
vnoremap < <gv
vnoremap > >gv

" Fast buffer switching
nnoremap <BS> <C-^>

" Colemak navigation
"
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

" Buffer navigation hotkeys
noremap <unique> <M-1> 1gt
noremap <unique> <M-2> 2gt
noremap <unique> <M-3> 3gt
noremap <unique> <M-4> 4gt
noremap <unique> <M-5> 5gt
noremap <unique> <M-6> 6gt
noremap <unique> <M-7> 7gt
noremap <unique> <M-8> 8gt
noremap <unique> <M-9> 9gt
noremap <unique> <M-0> 10gt


" Abbreviations
" ================================

" Timestamp abbreviation
iabbr tsp <C-r>=strftime("%Y-%m-%d")<cr>
iabbr tspt <C-r>=strftime("%Y-%m-%d %H:%M")<cr>


" Filetype settings
" ================================

filetype plugin on
filetype indent on

autocmd FileType text setlocal textwidth=78

autocmd FileType votl setlocal linebreak breakindentopt+=shift:3 breakindent colorcolumn=0 foldlevel=0 textwidth=100 tabstop=2 shiftwidth=2 nolist

autocmd FileType todo setlocal linebreak breakindentopt+=shift:2 breakindent colorcolumn=0 formatoptions-=t foldexpr=getline(v:lnum)=~'^\|\\s.*'?'1':0 foldmethod=expr

" Haskell is indentation-sensitive, so set up the vertical sight-line. And the
" horizontal line too because otherwise it gets hard to see where the cursor
" is.
autocmd FileType haskell setlocal shiftwidth=2 cursorcolumn cursorline

autocmd FileType rust setlocal colorcolumn=100

autocmd FileType python setlocal shiftwidth=4 softtabstop=-1 formatoptions-=t colorcolumn=100

autocmd FileType go setlocal tabstop=4 listchars=tab:\ \ ,trail:· formatoptions-=t

autocmd FileType javascript setlocal shiftwidth=2

" Commands
" ================================

" Unify tabs and remove trailing whitespace.
command! WhiteClean retab | %s/\s\+$
