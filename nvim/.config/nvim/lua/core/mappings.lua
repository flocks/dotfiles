local utils = require("utils")

utils.nmap("Q", "<ESC>") -- never use Ex useless mode

-- editing
utils.imap("AA", "<Esc>A") -- quick command in insert mode: go to line end
utils.imap("II", "<Esc>I") -- quick command in insert mode: go to line start
utils.imap("OO", "<Esc>O") -- quick command in insert mode: go to line above

-- clipboard
utils.vmap("<Leader><Leader>", "\"+") -- toggle + register
utils.nmap("<Leader><Leader>", "\"+") -- toggle + register

-- quickfix
utils.nmap("<Leader>q", ":copen<CR>")

-- formatter
utils.nmap("<Leader>f", ":Format<CR>") 

-- git
utils.nmap("<Leader>gg", ":Git<CR><C-w>T") 

-- ease split navigation
utils.nmap("vv", "<C-w>v") 
utils.nmap("ss", "<C-w>s") 
utils.nmap("<C-l>", "<C-w>l") 
utils.nmap("<C-h>", "<C-w>h") 
utils.nmap("<C-j>", "<C-w>j") 
utils.nmap("<C-k>", "<C-w>k") 

-- fzf
utils.nmap("<C-p>", ":FzfLua files<CR>") -- c-p to open files
utils.nmap("<c-f>", ":FzfLua live_grep<CR>") -- fzf live grep

-- directory editing
utils.nmap("<C-x><C-j>", ":Dirbuf %<CR>") -- directory editor
