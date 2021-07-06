" Colorscheme that should work on terminal even if background variable is set wrong.

hi clear

if &background == 'dark'
  highlight! Normal       guibg=#151505  guifg=#ffffea
  highlight! ColorColumn  guibg=gray25   guifg=darkred
  highlight! Folded       guibg=gray25   guifg=fg gui=italic
  highlight! FoldColumn   guibg=gray25   guifg=fg
else " is light background
  highlight! Normal       guibg=#ffffea  guifg=#111111
  highlight! ColorColumn  guibg=gray80   guifg=red
  highlight! Folded       guibg=gray80   guifg=fg gui=italic
  highlight! FoldColumn   guibg=gray80   guifg=fg
endif

highlight! Normal                                            ctermbg=none ctermfg=none    cterm=NONE
highlight! NonText        guibg=bg      guifg=red gui=NONE   ctermbg=none ctermfg=red     cterm=NONE
highlight! WildMenu       guibg=bg      guifg=fg  gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! VertSplit      guibg=bg      guifg=fg  gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! Folded                                            ctermbg=8    ctermfg=none    cterm=italic
highlight! FoldColumn                                        ctermbg=8    ctermfg=none
highlight! Conceal        guibg=bg      guifg=fg  gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! LineNr         guibg=bg      guifg=fg  gui=italic ctermbg=none ctermfg=none    cterm=italic
highlight! Visual         guibg=fg      guifg=bg             ctermbg=none ctermfg=none    cterm=reverse
highlight! CursorLine     guibg=bg      guifg=red gui=italic ctermbg=none ctermfg=red     cterm=italic
highlight! ColorColumn                                       ctermbg=none ctermfg=red
highlight! MatchParen     guibg=darkred guifg=fg             ctermbg=red  ctermfg=none

highlight! Statement      guibg=bg guifg=fg      gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! Identifier     guibg=bg guifg=fg      gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! Type           guibg=bg guifg=gray    gui=italic ctermbg=none ctermfg=8       cterm=italic
highlight! PreProc        guibg=bg guifg=gray    gui=italic ctermbg=none ctermfg=8       cterm=italic
highlight! Constant       guibg=bg guifg=fg      gui=italic ctermbg=none ctermfg=none    cterm=italic
highlight! Comment        guibg=bg guifg=gray    gui=italic ctermbg=none ctermfg=8       cterm=italic
highlight! SpecialComment guibg=bg guifg=gray    gui=italic ctermbg=none ctermfg=8       cterm=italic
highlight! Special        guibg=bg guifg=none    gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! SpecialKey     guibg=bg guifg=red     gui=bold   ctermbg=none ctermfg=darkred cterm=bold
highlight! Directory      guibg=bg guifg=fg      gui=bold   ctermbg=none ctermfg=none    cterm=bold

highlight! Todo           guibg=lightgray guifg=darkred gui=italic    ctermbg=none ctermfg=darkred cterm=reverse,italic
highlight! SpellCap       guibg=none      guifg=none    gui=underline ctermbg=none ctermfg=none    cterm=underline

highlight! link Title Directory
highlight! link MoreMsg Comment
highlight! link Question Comment

hi link vimFunction Identifier

let g:colors_name = "rubricate"
