" Vim syntax file
" Language: jrnl

if exists("b:current_syntax")
  finish
endif

syn match jrnlHeading '\v^\d\d\d\d-\d\d-\d\d \d\d:\d\d .+$' contains=jrnlTag,jrnlDate
highlight def link jrnlHeading Function

syn match jrnlImportantHeading '\v^\d\d\d\d-\d\d-\d\d \d\d:\d\d .+ \*$' contains=jrnlTag,jrnlDate
highlight def link jrnlImportantHeading Constant

syn match jrnlTag '@[^ ]\+' contained
highlight def link jrnlTag Special

syn match jrnlDate '^\d\d\d\d-\d\d-\d\d \d\d:\d\d' contained
highlight def link jrnlDate Comment
