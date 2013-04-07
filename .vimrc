" colemak movement remapping.
set langmap=li,ko,hn,je,nh,ej,ik,ol,LI,KO,JE,EJ,IK,OL

set expandtab
set softtabstop=4
set shiftwidth=4
set nojoinspaces
set backspace=indent,eol,start
set ignorecase
set smartcase
set formatoptions-=t

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

highlight BadWhitespace ctermbg=darkgreen guibg=darkgreen
" Match trailing whitespace, except when you are typing at the end of the line
" | match physical tabs after space
" | match space after physical tab
match BadWhitespace /\s\+\%#\@<!$\| \+\zs\t\+\|\t\+\zs \+/

" Timestamp abbreviation
iabbr tsp <<C-r>=strftime("%Y-%m-%d")<cr>>
iabbr tspt <<C-r>=strftime("%Y-%m-%d %H:%M")<cr>>

" Esc hack, actually "qk", but must use o here because of langmap.
imap qo <esc>

" Use line wrapping for these file types.
autocmd BufRead,BufNewFile *.txt,*.text,*.html,*.org,README,TODO,BUGS,COMMIT_EDITMSG se formatoptions+=t formatoptions-=r tw=68

" Special mode for text Anki decks: Tab-separated single-line.
autocmd BufRead,BufNewFile *anki.txt se formatoptions-=t noexpandtab softtabstop=0 showbreak=\ \  lbr wrap

filetype indent on
