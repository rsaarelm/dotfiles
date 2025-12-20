(require :config.mappings)
(require :config.settings)
(require :config.autocmds)
(require :config.lsp)

; Automatically load all plugin specs under "fnl/plugins/"
(local plugin-paths
  (-> (vim.fn.stdpath "config")
      (.. "/fnl/plugins/*.fnl")
      (vim.fn.glob)
      (vim.split "\n")))

(each [_ path (ipairs plugin-paths)]
  (require (.. "plugins." (vim.fn.fnamemodify path ":t:r"))))
