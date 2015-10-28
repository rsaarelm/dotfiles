" Use ~/.vim subdirectory even when on Windows
if has('win32') || has('win64')
   set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

execute pathogen#infect()

set expandtab
set softtabstop=4
set shiftwidth=4
set nojoinspaces
set backspace=indent,eol,start
set ignorecase
set smartcase
set formatoptions-=t
set modeline
set visualbell
set autoindent

set showmatch
set hlsearch
set smartcase
set ruler
set guioptions=ac
set guicursor+=a:blinkon0

set autochdir

" Don't clobber working directory with swap files.
set directory=~/.vim/swap//
set backupdir=~/.vim/backup//

" Find tags file recursively
set tags=./tags;/

" Better tab completion
set wildmenu
set wildmode=list:longest,full
set wildignore=*.o,*.so,*~

syntax enable
"set background=dark

" Timestamp abbreviation
iabbr tsp <C-r>=strftime("%Y-%m-%d")<cr>
iabbr tspt <C-r>=strftime("%Y-%m-%d %H:%M")<cr>

" Esc hack, actually "qk", but must use o here because of langmap.
imap qo <esc>

" Easier window and tab navigation
noremap <C-n> <C-w>h
noremap <C-e> <C-w>j
noremap <C-u> <C-w>k
noremap <C-i> <C-w>l
noremap <C-k> <C-PageUp>
noremap <C-m> <C-PageDown>

" Use line wrapping for these file types.
autocmd BufRead,BufNewFile *.txt,*.text,*.html,*.org,README,TODO,BUGS,COMMIT_EDITMSG setl formatoptions+=t formatoptions-=r tw=68

" Special mode for text Anki decks: Tab-separated single-line.
autocmd BufRead,BufNewFile *anki.txt setl formatoptions-=t noexpandtab softtabstop=0 showbreak=\ \  lbr wrap

" Use physical tabs with Go files
autocmd BufRead,BufNewFile *.go setl noexpandtab softtabstop=0 tabstop=4

autocmd BufRead,BufNewFile *.md setl syntax=markdown formatoptions+=tr tw=68

" Override the barbaric textwidth of the default Rust mode
autocmd BufRead,BufNewFile *.rs setl tw=78

autocmd FileType make setl noexpandtab

" Run cargo with ':make' in rust projects.
autocmd BufRead,BufNewFile Cargo.toml,Cargo.lock,*.rs compiler cargo

filetype indent on
filetype plugin indent on

" Unify tabs and remove trailing whitespace.
command! WhiteClean retab | %s/\s\+$

highlight BadWhitespace ctermbg=darkgray guibg=gray8
autocmd BufRead,BufNewFile * match BadWhitespace /\s\+$\| \+\zs\t\+\|\t\+\zs \+/
" | match physical tabs after space
" | match space after physical tab

" Instantly Better Vim stuff, from Damien Conway
" Highlight column 80.
highlight ColorColumn ctermbg=magenta
" call matchadd('ColorColumn', '\%81v', 100)
"autocmd BufRead,BufNewFile * call matchadd('ColorColumn', '\%81v', 100)
" XXX: This causes slowdowns with big files. Disabled for now.

" Swap ; and :, mostly using :, so shouldn't need shift for it.
nnoremap  ;  :
nnoremap  :  ;
vnoremap  ;  :
vnoremap  :  ;

" Swap v and CTRL-v, mostly using block mode not visual mode
nnoremap    v   <C-V>
nnoremap <C-V>     v
vnoremap    v   <C-V>
vnoremap <C-V>     v

" C-] doesn't work right in Windows gvim when using Colemak layout. Let's
" remap follow-tag to F3 and next-tag to F4.
nnoremap <f3> <C-]>
nnoremap <f4> :tn<cr>

" Buffer navigation
nnoremap <f1> :bp<cr>
nnoremap <f2> :bn<cr>

" Syntastic
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Fix navigation in directory view.
augroup netrw_colemak_fix
    autocmd!
    autocmd filetype netrw call Fix_netrw_maps_for_colemak()
augroup END
function! Fix_netrw_maps_for_colemak()
    noremap <buffer> n h
    noremap <buffer> e gj
    noremap <buffer> u gk
    noremap <buffer> i l
endfunction
