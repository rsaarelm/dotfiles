(local util (require :util))

(vim.api.nvim_create_autocmd ["FileType"]
  {:pattern ["gdscript"]
   :callback (λ []
               ; GDScript uses tabs for indentation, don't highlight.
               (util.dont-highlight-tabs)
               ; Support the fold comments in the language.
               (set vim.opt_local.foldmarker "#region,#endregion")
               (set vim.opt_local.foldmethod "marker"))
   })

(vim.api.nvim_create_autocmd ["FileType"]
  {:pattern ["text"]
   ; Make things look nice for long lines when using semantic linefeeds
   :callback (λ []
               (set vim.opt_local.breakindent true)
               (set vim.opt_local.breakindentopt [ :shift:2 ]))
   })

(vim.api.nvim_create_autocmd ["FileType"]
  {:pattern ["rust"]
   :callback
   (λ []
     ; Run cargo to get build errors.
     (vim.api.nvim_buf_set_keymap 0 "n" "<F5>" ":make check<cr>" {})
     ; Nice formatting width.
     (set vim.opt_local.textwidth 78)
     )
   })

(vim.api.nvim_create_autocmd "BufWritePre"
  {:pattern "*"
   ; Markdown has the stupid semantically significant trailing whitespace thing
   ; so add an exception for it in the auto-cleanup rule.

   ; Callbacks need to return a falsy value in the end or they runtime will delete them.
   :callback (λ [] (if (not= vim.bo.filetype "markdown") (util.clear-trailing-whitespace)) nil)
   })
