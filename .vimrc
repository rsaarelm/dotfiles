" colemak movement remapping.
set langmap=li,ko,hn,je,nh,ej,ik,ol,LI,KO,JE,EJ,IK,OL

set expandtab
set softtabstop=4
set shiftwidth=4
set nojoinspaces

set showmatch
set hlsearch
set smartcase
set ruler
set guioptions=ac
set autochdir

" Don't clobber working directory with swap files.
set directory=~/.vim/swap//
set backupdir=~/.vim/backup//

" Find tags file recursively
set tags=./tags;/

syntax enable
set background=dark
colo desert

" Timestamp abbreviation
iabbr tsp <<C-r>=strftime("%Y-%m-%d %H:%M")<cr>>

" Hardcore mode
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Esc hack, actually "qk", but must use o here because of langmap.
imap qo <esc>

" Use line wrapping for these file types.
autocmd BufRead,BufNewFile *.txt,*.text,*.html,*.org,README,TODO,BUGS,COMMIT_EDITMSG se formatoptions+=t formatoptions-=r tw=68

