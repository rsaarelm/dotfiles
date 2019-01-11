" Fold at entry headings
setlocal foldexpr=getline(v:lnum)=~'^\\d\\d\\d\\d-\\d\\d-\\d\\d\\s\\d\\d:\\d\\d\\s.*'?'>1':1
setlocal foldmethod=expr
