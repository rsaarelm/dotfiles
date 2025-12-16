;; Utility functions

(λ dont-highlight-tabs []
  "Turn off tab listchar for buffer that uses a tab-based syntax."
  (set vim.opt_local.listchars { :tab "  " :trail "·" :nbsp "░" }))

(λ list-plugin-modules []
   "List all modules under fnl/plugins/ directory."
   (icollect [_ v (ipairs (vim.api.nvim_get_runtime_file "fnl/plugins/*.fnl" true))]
     (.. "plugins." (vim.fn.fnamemodify v ":t:r"))))

{
  : dont-highlight-tabs
  : list-plugin-modules
}
