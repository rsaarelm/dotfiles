(local { : dont-highlight-tabs } (require :util))

(vim.api.nvim_create_autocmd ["FileType"]
  {:pattern ["gdscript"]
   ; Don't highlight tabs in GDScript as they're the standard indentation.
   :callback dont-highlight-tabs
   })

(vim.api.nvim_create_autocmd ["FileType"]
  {:pattern ["votl"]
   ; Vimoutliner files
   :callback
    (λ []
      (dont-highlight-tabs)
      (set vim.opt_local.foldlevel 0)   ; Start with all folds closed
      (set vim.opt_local.tabstop 2)     ; Nice short tabs, folded items use shiftwidth instead
      (set vim.opt_local.shiftwidth 2)
      (set vim.opt_local.breakindent true)  ; Indent wrapped lines by 3 spaces
      (set vim.opt_local.breakindentopt [ :shift:3 ])
      )
   })
