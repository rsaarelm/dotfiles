(local util (require :util))

(vim.api.nvim_create_autocmd ["FileType"]
  {:pattern ["gdscript"]
   ; Don't highlight tabs in GDScript as they're the standard indentation.
   :callback util.dont-highlight-tabs
   })
