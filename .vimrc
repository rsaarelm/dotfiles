" colemak movement remapping.
set langmap=li,ko,hn,je,nh,ej,ik,ol,LI,KO,JE,EJ,IK,OL

se expandtab
se softtabstop=4
se shiftwidth=4
se nojoinspaces

se showmatch
se hlsearch
se smartcase
se ruler
se guioptions=ac

syntax enable
se background=dark
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

autocmd BufRead,BufNewFile *.txt,*.org,README,TODO,BUGS,COMMIT_EDITMSG se formatoptions+=t formatoptions-=r tw=68

set directory=~/.vim/swap//
set backupdir=~/.vim/backup//
