" colemak movement remapping.
set langmap=li,ko,hn,je,nh,ej,ik,ol,LI,KO,HN,JE,NH,EJ,IK,OL

se expandtab
se softtabstop=4
se nojoinspaces

se showmatch
se hlsearch
se smartcase
se ruler
se guifont=terminus
se guioptions=ac

syntax on
colo torte

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

" Esc hack
imap qq <esc>

autocmd BufRead,BufNewFile *.txt,*.org,README,TODO,BUGS,COMMIT_EDITMSG se formatoptions+=t formatoptions-=r tw=68
