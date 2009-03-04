" Prohibit physical tabs.
se expandtab
se softtabstop=2
" Prohibit audible bell.
se visualbell

se shiftwidth=2
se autoindent
se nosmartindent

" Automatically match closing parens you write
se showmatch

se encoding=utf-8
se fileencodings=
se nojoinspaces
se nobackup
se ignorecase
se smartcase

" The default textwidth, and automatically wrap comments in various
" programming languages.
se tw=78
" Don't wrap text by default. Most file extensions are source codes where
" we don't want this. Text files are specified separately in autocmds.
se formatoptions-=t

" I put some long lines in the text files. Make editing them without a mess a
" bit easier.
se formatoptions+=l

se hls

se ruler
se backspace=indent,eol,start

" Make markers easier with the otherwise useless ¤ char in the Finnish
" keyboard. And since I'm using these mostly in HTML-like Markdown documents,
" add the HTML comment around the marker.
abbrev ¤1 <!-- {{{1 -->
abbrev ¤2 <!-- {{{2 -->
abbrev ¤3 <!-- {{{3 -->
abbrev ¤4 <!-- {{{4 -->
abbrev ¤< }}}

map <F11> :tabp<cr>
map <F12> :tabn<cr>
map <F9> :bp<cr>
map <F10> :bn<cr>
map <S-left> :bp<cr>
map <S-right> :bn<cr>

" Autocmds for plain text, latex and html. Activate wrapping of normal text.
autocmd BufRead,BufNewFile *.txt,.followup,.article,.letter,README,TODO,BUGS se formatoptions+=t formatoptions-=r
autocmd BufRead,BufNewFile *.tex se formatoptions+=t
autocmd BufRead,BufNewFile *.html se formatoptions+=t
autocmd BufRead,BufNewFile *.m4 se formatoptions+=t
" Fold C# files by the '#region', '#endregion' tags.
autocmd BufRead,BufNewFile *.cs se foldmarker=#region,#endregion foldmethod=marker comments^=:///

" Abbrevs for C# documentation comments.
autocmd BufRead,BufNewFile *.cs abbrev ///p /// <param name=""> </param><Left><Left><Left><Left><Left><Left><Left><Left><Left>
autocmd BufRead,BufNewFile *.cs abbrev ///r /// <returns> </returns><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left>
autocmd BufRead,BufNewFile *.cs abbrev ///s /// <summary><cr>/// </summary><Up><cr>///
autocmd BufRead,BufNewFile *.cs abbrev ///t /// <typeparam name=""> </typeparam><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left>

autocmd BufRead,BufNewFile *.cs map <F6> :!xbuild<cr>
autocmd BufRead,BufNewFile *.cs map <F7> :!xbuild /t:ConsoleTest<cr>

"autocmd BufRead,BufNewFile *.cs se sts=4
autocmd BufRead,BufNewFile SConstruct,SConscript se syntax=python
autocmd BufRead,BufNewFile *.scala se syntax=scala
autocmd BufRead,BufNewFile *.clj se syntax=clojure

" Backquotes are difficult to type with a Finnish keyboard, fortunately it has
" some useless extra keys.
imap § `

" Most of the time I want to go up and down based on visual lines, not
" physical lines.

map j gj
map k gk

" And I want the visual lines to look pretty

set linebreak

" Console mode Vim colorscheme
colorscheme slate

syntax enable

" Highlight trailing whitespace
autocmd Syntax * syn match Underlined /\s\+$/

function! OCamlType()
  let col  = col('.')
  let line = line('.')
  let file = expand("%:p:r")
  echo system("annot -n -type ".line." ".col." ".file.".annot")
endfunction    
"map ,t :call OCamlType()<return>

map ,t :tabnew<cr>

" Quick switching between the first 10 tabs
map <M-1> 1gt
map <M-2> 2gt
map <M-3> 3gt
map <M-4> 4gt
map <M-5> 5gt
map <M-6> 6gt
map <M-7> 7gt
map <M-8> 8gt
map <M-9> 9gt
map <M-0> 10gt

" Window switching commands similar to the ones I have in Emacs
" map <C-,> <C-w>W
" map <C-.> <C-w>w

" Scrolling the other window
:map <C-M-B> <C-W>p<C-B><C-W>p
:map <C-M-F> <C-W>p<C-F><C-W>p

" Viki stuff

autocmd BufRead,BufNewFile *.viki se ft=viki formatoptions+=t
" This autocmd changes the viki files when they are opened. Maybe a bit strange.
autocmd BufRead *.viki call GrepBacklinkList()

" Propagate suffix to new files.
let g:vikiUseParentSuffix = 1
" Low-ascii only
let g:vikiLowerCharacters = "a-z"
let g:vikiUpperCharacters = "A-Z"
" Open HTML files in firefox.
let g:vikiOpenUrlWith_html  = "silent !firefox %{FILE}"

" Generate a list of wiki files linking to current file under a specific
" heading.
function! GrepBacklinkList()

  " !!! SPECIAL CONFIG SECTION !!!
  let links_section_pattern = '^\* Links to here$'
  let extension = '.viki'
  let link_prefix = '    - '

  " See if the link section header is found in this file.
  let [start_line, col] = searchpos(links_section_pattern)
  if col == 1
    " Get the filename of the current file without the extension
    let filename = expand('%:t:r')
    let dir = expand('%:p:h')
    " Do the grep query.
    let grep_cmd = 'grep -l "\<'.filename.'\>" '.dir.'/*'.extension
    let links = split(system(grep_cmd))
    "let links = []
    " Reverse the list since we'll be appending them in backwards order.
    let links = reverse(links)

    " Eat all non-empty indented lines under section heading line. These are
    " assumed to be the old links to this file.
    while getline(start_line + 1) =~ '^\s\+\S.*'
      execute (start_line + 1).'delete'
    endwhile

    " Print the new links properly indented and itemized.
    for link in links
      " Remove path and extension from the link
      let link = substitute(link, '.*\/\([^\/]\+\)\'.extension, '\1', '')

      " Add the link under the heading with proper indentation.
      call append(start_line, link_prefix.link)
    endfor
  endif
endfunction

" Remap hjkl for Colemak keyboard
" This doesn't reorder any other keys, so an excessive remapping for colemak
" isn't necessary.
function! ColemakHJKL()

  noremap K J
  noremap J K

  noremap h k
  noremap j h
  noremap k j

  noremap gh gk
  noremap gj gh
  noremap gk gj

  noremap zh zk
  "zK does not exist
  noremap zj zh
  noremap zJ zH
  noremap zk zj
  "zJ does not exist
  noremap z<Space> zl
  noremap z<S-Space> zL
  noremap z<BS> zh
  noremap z<S-BS> zH

  noremap <C-w>h <C-w>k
  noremap <C-w>H <C-w>K
  noremap <C-w>j <C-w>h
  noremap <C-w>J <C-w>H
  noremap <C-w>k <C-w>j
  noremap <C-w>K <C-w>J
  noremap <C-w><Space> <C-w>l
  noremap <C-w><S-Space> <C-w>L
  noremap <C-w><S-BS> <C-w>H

endfunction

call ColemakHJKL()
