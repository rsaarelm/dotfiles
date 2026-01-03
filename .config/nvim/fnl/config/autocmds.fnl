(local util (require :util))

(vim.api.nvim_create_autocmd ["FileType"]
  {:pattern ["gdscript"]
   ; Don't highlight tabs in GDScript as they're the standard indentation.
   :callback util.dont-highlight-tabs
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
