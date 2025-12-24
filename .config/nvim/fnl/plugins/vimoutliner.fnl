(local util (require :util))

; TODO: I should just make my own plugin to replace VimOutliner completely...
(vim.pack.add ["https://github.com/rsaarelm/vimoutliner"] { :confirm false })

(vim.api.nvim_create_autocmd ["FileType"]
  {:pattern ["votl"]
   ; Vimoutliner files
   :callback
    (λ []
      (util.dont-highlight-tabs)
      (set vim.opt_local.foldlevel 0)   ; Start with all folds closed
      (set vim.opt_local.tabstop 2)     ; Nice short tabs, folded items use shiftwidth instead
      (set vim.opt_local.shiftwidth 2)
      (set vim.opt_local.linebreak true)    ; Wrap at word breaks
      (set vim.opt_local.breakindent true)  ; Indent wrapped lines by 3 spaces
      (set vim.opt_local.breakindentopt [ :shift:3 ])
      )
   })
