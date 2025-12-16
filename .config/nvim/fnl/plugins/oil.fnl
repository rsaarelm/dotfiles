(vim.pack.add ["https://github.com/stevearc/oil.nvim"] { :confirm false })

(local oil (require :oil))

(oil.setup
 {
  :default_file_explorer true
  :view_options
   {
    :is_hidden_file
    (λ [name _]
      (or
        ; A bunch of binary files we don't want in the explorer.

        ; Programming stuff
        (name:match "%.cpcache$")
        (name:match "%.git$")
        (name:match "%.o$")
        (name:match "%.pyc$")
        (name:match ".direnv$")
        (name:match "__pycache__$")

        ; Media file stuff
        (name:match "%.bin$")
        (name:match "%.blend$")
        (name:match "%.glb$")
        (name:match "%.jpg$")
        (name:match "%.png$")
        (name:match "%.wav$")

        ; Godot and gamedev stuff
        (name:match "%.import$")
        (name:match "%.res$")
        (name:match "%.uid$")
        (name:match ".godot$")
      ))

   }
  })
