vim.cmd('source ~/.vimrc')

-- automatically install lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath })
end
vim.opt.rtp:prepend(lazypath)

local signs = {Error = "", Warn = "", Hint = "", Info = ""}
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, {text = icon, texthl = hl, numhl = hl})
end

vim.cmd("autocmd BufNewFile,BufRead tsconfig.json set filetype=jsonc") -- properly highlight json5 files
vim.cmd("autocmd FileType typescript,typescriptreact compiler tsc")


-- Diagnostic navigation
vim.keymap.set('n', '<leader>N', vim.diagnostic.goto_prev)
vim.keymap.set('n', '<leader>n', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setqflist)

vim.keymap.set('v', "<C-y>", "\"+y") -- yank in systemclipboard
vim.keymap.set('n', "<C-;>", ":Buffers<CR>") -- yank in systemclipboard
-- vim.keymap.set('n', "<C-c><C-f>", ":Prettier<CR>") -- reformat
vim.keymap.set('n', "<C-c><C-f>", ":lua vim.lsp.buf.format()<CR>") -- reformat
vim.keymap.set('n', "<C-c><C-r>", ":%s/<C-r><C-w>/") -- s/foo/bar current word


vim.keymap.set('n', "<C-c>c", ":RunAsync<Space>")
vim.keymap.set('n', "<C-c><C-c>", ":ReRunAsync<CR>")


-- add some roundness to neovim popup
local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
vim.lsp.util.open_floating_preview = function(contents, syntax, opts, ...)
  opts = opts or {}
  opts.border = opts.border or 'rounded'
  return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

-- set cmd height to 0 
vim.opt.cmdheight = 0

-- highlight on yank
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- LSP attach handler
local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end
    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')
  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
end

-- PLUGINS
-- =======

local lsp_servers = {
  'ts_ls',
  'tailwindcss',
  'lua'
}

require("lazy").setup({
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup({
        -- Configuration here, or leave empty to use defaults
      })
    end
  },
  {"skywind3000/asyncrun.vim"},
  {"prettier/vim-prettier"},
  {"tpope/vim-eunuch"},
  {"projekt0n/github-nvim-theme",
  config = function () 
    -- vim.cmd("colorscheme github_dark_high_contrast")
  end;
},
{"gbprod/nord.nvim",
config =  function() 
  vim.cmd("colorscheme nord")
end
},
{
  "nvim-lualine/lualine.nvim",
  lazy = false,
  priority = 1000,
  opts = {
    options = {
      icons_enabled = true,
      theme = 'nord',
      component_separators = '',
      section_separators = { left = '', right = '' },
    },
    sections = {
      lualine_a = { 'mode' },
      lualine_b = { 'diagnostics' },
      lualine_c = {
        { 'filename', path = 1 },
      },
      lualine_x = {},
      lualine_y = { 'branch' },
      lualine_z = { 'location' }
    },
    inactive_sections = {
      lualine_a = {},
      lualine_b = {},
      lualine_c = {
        { 'filename', path = 1 },
      },
      lualine_x = { 'location' },
      lualine_y = {},
      lualine_z = {}
    },
  },
},
{"tpope/vim-rsi"},
{"tpope/vim-repeat"},
{"tpope/vim-commentary"},
{ 
  "ruifm/gitlinker.nvim",
  config = function()
    require("gitlinker").setup()
  end
},
{"tpope/vim-fugitive"},
{"itchyny/vim-qfedit"},
{
  "windwp/nvim-autopairs",
  config = function()
    require("nvim-autopairs").setup({ map_cr = true })
  end
},
{"nvim-lua/plenary.nvim", build = 'make' },
{"nvim-telescope/telescope-fzf-native.nvim" },
{
  "nvim-telescope/telescope.nvim",
  config = function()
    local telescope = require('telescope')
    local ts_builtin = require('telescope.builtin')

    pcall(telescope.load_extension, 'fzf')

    telescope.setup({
      defaults = {
        preview_cutoff = 20,
        layout_config = {
          preview_width = 0.65,
          vertical = {
            width = 0.95,
          },
          horizontal = {
            width = 0.95,
          }
        },
        mappings = {
          i = {
            ['<C-u>'] = false,
            ['<C-d>'] = false,
          },
        },
      },
      pickers = {
        find_files = {
          previewer = false
        }
      },
    })

    vim.api.nvim_create_user_command(
      'FindCurrentDir',
      function ()
        ts_builtin.find_files({cwd = vim.fn.expand('%:p:h')})
      end,
      {}
    )

    vim.api.nvim_set_keymap('n', '<C-c>p', ':FindCurrentDir<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<C-c>f', ':Telescope diagnostics<CR>', { noremap = true, silent = true })
    vim.keymap.set('n', '<C-p>', ts_builtin.find_files, { desc = '[S]earch [F]iles' })
    vim.keymap.set('n', '<C-;>', ts_builtin.buffers )
    vim.keymap.set('n', '<C-b>', ts_builtin.buffers )
    -- vim.keymap.set('n', '<leader>f', ts_builtin.grep_string, { desc = '[S]earch current [W]ord' })
    vim.keymap.set('n', '<C-f>', ts_builtin.live_grep, { desc = '[S]earch by [G]rep' })
    vim.keymap.set('n', '<leader>sd', ts_builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
  end,
},
-- LSP
{ "williamboman/mason.nvim" },
{
  "williamboman/mason-lspconfig.nvim",
  config = function()
    require('mason').setup()
    require('mason-lspconfig').setup({
      ensure_installed = lsp_servers
    })
  end
},
{
  "neovim/nvim-lspconfig",
  dependencies = {  { 'j-hui/fidget.nvim', opts = {} }},
  config = function()
    -- local util = require('lspconfig.util')
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

    vim.lsp.enable("ts_ls")
    vim.lsp.config("ts_ls", {
      on_attach = on_attach,
      capabilities = capabilities,
      filetypes = {"typescript", "typescriptreact", "typescript.tsx"},
      settings = {
        typescript = {
          preferences = {
            importModuleSpecifier = "non-relative",
          }
        }
      }
    })

    vim.lsp.enable("tailwindcss")
    vim.lsp.config("tailwindcss", {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        tailwindCSS = {
          experimental = {
            classRegex = {
              { "cva\\(((?:[^()]|\\([^()]*\\))*)\\)", "[\"'`]([^\"'`]*).*?[\"'`]" },
              { "cn\\(((?:[^()]|\\([^()]*\\))*)\\)", "(?:'|\"|`)([^']*)(?:'|\"|`)" }
            },
          },
        },
      }
    })

    vim.lsp.config("lua_ls", {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        Lua = {
          diagnostics = {
            globals = { 'vim' },
          },
          workspace = {
            library = vim.api.nvim_get_runtime_file("", true),
            checkThirdParty = false,
          },
        },
      },
    })

  end
},
-- copilot
{
  'github/copilot.vim',
  config = function() 
    vim.keymap.set('i', '<C-J>', 'copilot#Accept("\\<CR>")', {
      expr = true,
      replace_keycodes = false,
    })
    vim.g.copilot_no_tab_map = true
  end
},
-- completion
{ "hrsh7th/cmp-nvim-lsp" },
{
  "L3MON4D3/LuaSnip",
  config = function()
    require("luasnip.loaders.from_snipmate").load()
  end
},
{ "saadparwaiz1/cmp_luasnip" },
{ "hrsh7th/cmp-buffer" },
{
  "hrsh7th/nvim-cmp",
  config = function()
    local cmp = require('cmp')
    local luasnip = require('luasnip')
    cmp.setup({
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },
      mapping = cmp.mapping.preset.insert {
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-M-i>'] = cmp.mapping.complete(),
        ['<CR>'] = cmp.mapping.confirm {
          behavior = cmp.ConfirmBehavior.Replace,
          select = true,
        },
        ['<Tab>'] = cmp.mapping(function(fallback)
          if luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
          elseif cmp.visible() then
            cmp.select_next_item()
          else
            fallback()
          end
        end, { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { 'i', 's' }),
      },
      sources = {
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
        { name = 'buffer'}
      },
      fallback = {
        enable = true,
        source = 'vim',
        -- You can specify additional sources here if desired.
      },
    })
  end
},
}, opts)



-- run :AsyncRun and store command in cache
function RunAsyncCommand(cmd)
  _G.last_async_command = cmd
  vim.cmd("AsyncRun " .. cmd)
end

-- re-run last :AsyncRun command
function ReRunAsync()
  if _G.last_async_command ~= "" then
    vim.cmd("AsyncRun " .. _G.last_async_command)
  else
    print("No command to re-run")
  end
end

vim.api.nvim_create_user_command("RunAsync", function(args)
  RunAsyncCommand(args.args)
end, { nargs = 1 })

vim.api.nvim_create_user_command("ReRunAsync", function()
  ReRunAsync()
end, {})

