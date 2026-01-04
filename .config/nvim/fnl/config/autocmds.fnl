(local util (require :util))

(vim.api.nvim_create_autocmd ["FileType"]
  {:pattern ["gdscript"]
   ; Don't highlight tabs in GDScript as they're the standard indentation.
   :callback util.dont-highlight-tabs
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

(local white-clean (vim.api.nvim_create_augroup "WhitespaceCleanup" { :clear false }))

(vim.api.nvim_create_autocmd "BufWritePre"
  {:pattern "*"
   :group white-clean
   ; Markdown has the stupid semantically significant trailing whitespace thing
   ; so add an exception for it in the auto-cleanup rule.

   ; FIXME: For some reason using :callback here only works for the first save and then
   ; wipes out the autocmd, :command works fine.
   :command "Fnl (local util (require :util)) (if (not= vim.bo.filetype \"markdown\") (util.clear-trailing-whitespace))"
   ;:callback
   ;(λ [] (if (not= vim.bo.filetype "markdown") (util.clear-trailing-whitespace)))
   })
