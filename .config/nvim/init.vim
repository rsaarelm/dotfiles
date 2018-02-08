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
map <TAB> :call NERDTreeToggleAndFind()<cr>
"map <C-TAB> :NERDTreeToggle<cr>
function! NERDTreeToggleAndFind()
    if (exists('t:NERDTreeBufName') && bufwinnr(t:NERDTreeBufName) != -1)
        execute ':NERDTreeClose'
    else
        if filereadable(expand('%:p'))
            execute ':NERDTreeFind'
        else
            execute ':NERDTree'
        endif
    endif
    endfunction
let NERDTreeMapOpenExpl='j'  " Enable using Colemak vertical navigation

" Fugitive for Git
Plug 'tpope/vim-fugitive'

" todo.txt
Plug 'freitass/todo.txt-vim'

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

" Vimoutliner
Plug 'vimoutliner/vimoutliner'

" Haskell Plugins
" --------------------------------

" Source code formatter
Plug 'nbouscal/vim-stylish-haskell'

call plug#end()


" Settings
" ================================
set virtualedit=block  " Free block placement in visual mode

set ignorecase      " Default search is case-insensitive
set smartcase       " ...except when you write caps in the search expression

set nojoinspaces    " Two spaces after a period is an abomination
set colorcolumn=81  " Show show the forbidden zone
set wrap            " Show long lines by default
set textwidth=78

set listchars=tab:›…,trail:·  " Show trailing whitespace
set list

set number          " Line numbering
set lazyredraw      " Don't update screen when running macros
set hlsearch        " Highlight search results

set expandtab       " Don't use physical tabs as a rule
set softtabstop=-1  " Use shiftwidth everywhere
set shiftwidth=4
set shiftround      " Snap to shiftwidth

set noswapfile      " Don't create swap files

set guioptions=ac   " Minimize GUI cruft
set guicursor+=a:blinkon0

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

autocmd FileType todo setlocal linebreak breakindentopt+=shift:2 breakindent colorcolumn=0 formatoptions-=t

autocmd FileType haskell setlocal shiftwidth=2


" Commands
" ================================

" Unify tabs and remove trailing whitespace.
command! WhiteClean retab | %s/\s\+$

" Expression-based folding in jrnl files
command! JrnlFold setlocal foldexpr=getline(v:lnum)=~'^\\d\\d\\d\\d-\\d\\d-\\d\\d\\s\\d\\d:\\d\\d\\s.*'?'>1':1 foldmethod=expr
