(org-babel-do-load-languages
  'org-babel-load-languages
  '((R . t)
    (gnuplot . t)))

; I'll be running my own code, don't want a yes-confirm each time I
; re-eval things.
(setq org-confirm-babel-evaluate nil)

; Need this to get gnuplot working.
(ensure-installed 'gnuplot)
