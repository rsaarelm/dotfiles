autocmd BufRead,BufNewFile *.wiki setlocal filetype=wiki sts=2 tw=78 fo+=tr fo-=r

autocmd BufRead,BufNewFile *.wiki setlocal foldexpr=getline(v:lnum)=~'\\C\\v^([A-Z][a-z0-9]+){2,}$'?'>1':1 foldmethod=expr

" Go to local file wiki definition of WikiWord with gd
autocmd BufRead,BufNewFile *.wiki map <buffer> gd yiw/^\<<C-f>pa\>$<cr>

" Open external file WikiWord.wiki with gf
autocmd BufRead,BufNewFile *.wiki setlocal suffixesadd=.wiki
