-- Shim to enable Fennel
vim.pack.add({"https://github.com/rktjmp/hotpot.nvim"}, { confirm = false })
require("hotpot").setup()
-- Switch to Fennel code
require("main")
