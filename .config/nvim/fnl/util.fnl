;; Utility functions

(λ dont-highlight-tabs []
  "Turn off tab listchar for buffer that uses a tab-based syntax."
  (set vim.opt_local.listchars { :tab "  " :trail "·" :nbsp "░" }))

(λ clear-trailing-whitespace []
  (let [current-view (vim.fn.winsaveview)
        current-search (vim.fn.getreg "/")]
    ; Trailing whitespace
    (vim.cmd "silent! %s/\\s\\+$//e")
    ; Whitespace-only lines at the end of file.
    (vim.cmd "silent! %s/\\n\\+\\%$//e")
    ; Restore view and search
    (vim.fn.winrestview current-view)
    (vim.fn.setreg "/" current-search)))

{
  : dont-highlight-tabs
  : clear-trailing-whitespace
}
