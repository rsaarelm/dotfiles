" Colorscheme for when the background variable detection in terminal mode is
" bugged and can't distinguish light and dark terminal backgrounds.

hi clear

" Gui always gets a reliable background value, set gui normal color and some
" colors that rely on a discreet background color based on it.
if &background == 'dark'
  highlight! Normal       guibg=#111111  guifg=#ffffea
  highlight! ColorColumn  guibg=gray10   guifg=darkred
  highlight! Folded       guibg=gray10   guifg=fg gui=italic
else " is light background
  highlight! Normal       guibg=#888899  guifg=#111111
  highlight! ColorColumn  guibg=gray45   guifg=darkred
  highlight! Folded       guibg=gray45   guifg=fg gui=italic
endif

highlight! Normal                                                ctermbg=none ctermfg=none    cterm=NONE
highlight! NonText        guibg=bg      guifg=darkred gui=NONE   ctermbg=none ctermfg=red     cterm=NONE
highlight! WildMenu       guibg=bg      guifg=fg      gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! VertSplit      guibg=bg      guifg=fg      gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! Folded                                     gui=italic ctermbg=8    ctermfg=none    cterm=italic
highlight! link FoldColumn      Folded
highlight! Conceal        guibg=bg      guifg=fg      gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! Visual         guibg=fg      guifg=bg                 ctermbg=none ctermfg=none    cterm=reverse
highlight! CursorLine     guibg=bg      guifg=darkred gui=italic ctermbg=none ctermfg=red     cterm=italic
highlight! CursorColumn   guibg=bg      guifg=darkred gui=italic ctermbg=none ctermfg=red     cterm=italic
highlight! ColorColumn                                           ctermbg=none ctermfg=red
highlight! MatchParen     guibg=darkred guifg=fg                 ctermbg=red  ctermfg=none
highlight! link LineNr          Comment

" TODO: I want to use bold to highlight *specific important parts* in source
" code, but currently don't seem to be able to get a specific enough highlight
" group even with treesitter. More concretely, I'd like to bold function
" parameters, both in the function signature and when they occur in the
" function body, but treesitter doens't seem to have "variable in function
" body that is a function parameter" category.

highlight! Comment        guibg=bg guifg=gray30  gui=italic ctermbg=none ctermfg=8       cterm=italic
highlight! link Statement       Normal
highlight! link Identifier      Normal
highlight! link Type            Comment
highlight! link PreProc         Comment
highlight! Constant       guibg=bg guifg=fg      gui=italic ctermbg=none ctermfg=none    cterm=italic
highlight! link SpecialComment  Comment
highlight! Special        guibg=bg guifg=none    gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! Tag            guibg=bg guifg=none    gui=bold   ctermbg=none ctermfg=none    cterm=bold
highlight! SpecialKey     guibg=bg guifg=darkred gui=bold   ctermbg=none ctermfg=darkred cterm=bold
highlight! Directory      guibg=bg guifg=fg      gui=bold   ctermbg=none ctermfg=none    cterm=bold
highlight! link Title           Directory
highlight! link MoreMsg         Comment
highlight! link Question        Comment
highlight! Todo           guibg=gray45    guifg=darkred gui=italic    ctermbg=none ctermfg=darkred cterm=reverse,italic
highlight! SpellCap       guibg=none      guifg=none    gui=underline ctermbg=none ctermfg=none    cterm=underline

let g:colors_name = "rubricate"
