autocmd BufRead,BufNewFile *.wiki set filetype=wikilike

" Extension .wiki.html is for files using the HTML transforming shim that need
" to advertise a text/html MIME type to a static web server.
autocmd BufRead,BufNewFile *.wiki.html set filetype=wikilike

autocmd BufNewFile *.wiki.html 0r ~/.vim/skeleton.wiki.html
