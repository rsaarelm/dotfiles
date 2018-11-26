autocmd BufRead,BufNewFile *.wiki set filetype=wiki

" Extension .wiki.html is for files using the HTML transforming shim that need
" to advertise a text/html MIME type to a static web server.
autocmd BufRead,BufNewFile *.wiki.html set filetype=wiki
