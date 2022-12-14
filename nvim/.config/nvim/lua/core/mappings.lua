local utils = require("utils")

utils.nmap("Q", ":cq<CR>") -- never use Ex useless mode

-- editing
utils.imap("AA", "<Esc>A") -- quick command in insert mode: go to line end
utils.imap("II", "<Esc>I") -- quick command in insert mode: go to line start
utils.imap("OO", "<Esc>O") -- quick command in insert mode: go to line above

-- clipboard
utils.vmap("<Leader><Leader>", "\"+") -- toggle + register
utils.nmap("<Leader><Leader>", "\"+") -- toggle + register

utils.vmap("<Leader>s", "\"*yggdG\"*pggdd")        -- keep only what's selectd

-- quickfix
utils.nmap("<Leader>q", ":copen<CR>")

-- formatter
utils.nmap("<Leader>f", ":Format<CR>") 
utils.nmap("<Leader>j", ":%!jq<CR>") 

-- git
utils.nmap("<Leader>gg", ":Git<CR><C-w>o") 
utils.nmap("<Leader>gl", ":Gclog<CR>") 

-- ease split navigation
utils.nmap("vv", "<C-w>v") 
utils.nmap("ss", "<C-w>s") 
utils.nmap("<C-l>", "<C-w>l") 
utils.nmap("<C-h>", "<C-w>h") 
utils.nmap("<C-j>", "<C-w>j") 
utils.nmap("<C-k>", "<C-w>k") 
utils.nmap("<C-k>", "<C-w>k") 


-- compilation
utils.nmap("<Leader>c", ":lua tscheck()<CR>")
--utils.nmap("<Leader>e", ":!%")
utils.nmap("<Leader>E", ":terminal bash %")
utils.nmap("<Leader>r", ":AsyncRun")

vim.cmd("command Exec set splitright | new | set filetype=sh | read !sh #")
utils.nmap("<leader>e", ":Exec<CR>")



-- fzf
utils.nmap("<C-p>", ":FzfLua files<CR>") -- c-p to open files
utils.nmap("<c-f>", ":FzfLua live_grep<CR>") -- fzf live grep
utils.nmap("<c-b>", ":FzfLua buffers<CR>") -- fzf live grep

-- directory editing
utils.nmap("<C-x><C-j>", ":Dirbuf %<CR>") -- directory editor

-- command mode
utils.cmap("<C-c><C-h>", "<C-c>:FzfLua commands<CR>") 


