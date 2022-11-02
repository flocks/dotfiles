require 'core.global'
require 'core.options'
require 'core.mappings'
require 'modules.plugins'

require('nvim-autopairs').setup()
require"gitlinker".setup({
  opts = {
    add_current_line_on_normal_mode = false
  }
})

local prettierd = {
   function()
      return {
        exe = "prettierd",
        args = {vim.api.nvim_buf_get_name(0)},
        stdin = true
      }
    end
}

require('formatter').setup({
  logging = true,
  filetype = {
    json = prettierd,
    jsonc = prettierd,
    css = prettierd,
    javascript = prettierd,
    javascriptreact = prettierd,
    html = prettierd,
    typescript = prettierd,
    typescriptreact = prettierd,
    yaml = prettierd,
    markdown = prettierd,
  }
})

local cmp = require'cmp'
cmp.setup({
  sources = cmp.config.sources({
    {name = "nvim_lsp"}
  })
})
local capabilities = require('cmp_nvim_lsp').default_capabilities()

local lsp_flags = {
  debounce_text_changes = 150,
}

local on_attach = function(client, bufnr)
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
end 

local lspconfig = require"lspconfig"
lspconfig.tsserver.setup({
  on_attach = on_attach,
  flags = lsp_flags,
  capabilities = capabilities,
  filetypes = { "typescript", "typescriptreact", "typescript.tsx" }

})

lspconfig.flow.setup({ 
  on_attach = on_attach,
  flags = lsp_flags,
  capabilities = capabilities,
  filetypes = { "javascript", "javascriptreact" }
})

lspconfig.bashls.setup({
  on_attach = on_attach,
  flags = lsp_flags,
  capabilities = capabilities,
  filetypes = { "sh" }
})


lspconfig.tailwindcss.setup({
  on_attach = on_attach,
  flags = lsp_flags,
  capabilities = capabilities,
})


