setlocal softtabstop=2
setlocal textwidth=78
setlocal formatoptions+=t

" Fold on titles
setlocal foldexpr=getline(v:lnum)=~'\\C\\v^([A-Z][a-z0-9]+){2,}$'?'>1':1 foldmethod=expr

" Go to local file wiki definition of WikiWord with gd
map <buffer> gd yiw/^\<<C-f>pa\>$<cr>

" Open external file WikiWord.wiki or WikiWord.wiki.html with gf
setlocal suffixesadd=.wiki,.wiki.html
