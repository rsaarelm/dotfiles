;; Utility functions

(λ dont-highlight-tabs []
  "Turn off tab listchar for buffer that uses a tab-based syntax."
  (set vim.opt_local.listchars { :tab "  " :trail "·" :nbsp "░" }))

{
  : dont-highlight-tabs
}
