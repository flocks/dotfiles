require('packer').startup(function()

  use 'wbthomason/packer.nvim'               -- plugin manager
  use 'lewis6991/impatient.nvim'             -- improve startup time

  use 'ggandor/leap.nvim'                    -- motions

  use 'elihunter173/dirbuf.nvim'
  --
  -- themes
  use 'morhetz/gruvbox'
  use 'ishan9299/modus-theme-vim'
  use 'arcticicestudio/nord-vim'

  use "SirVer/ultisnips"                     -- snipets
  use "hrsh7th/nvim-compe"                   -- completion
  use "ibhagwan/fzf-lua"                     -- fzf
  use "itchyny/lightline.vim"                -- lightline
  use "norcalli/nvim-colorizer.lua"          -- colorizer
  use "nvim-lua/plenary.nvim"                -- lib required by some plugins
  use "scrooloose/nerdcommenter"             -- line comments
  use "towolf/vim-helm"                      -- helm syntax highlight
  use "windwp/nvim-autopairs"                -- autopairs


  use "mhartington/formatter.nvim"           -- formatter

  use "itchyny/vim-qfedit"                   -- edit quickfix list

  use "ruifm/gitlinker.nvim"                 -- link to github

  -- Tim Pope section LOL
  use "tpope/vim-dispatch"                   -- make/compiler async helper
  use 'tpope/vim-fugitive'                   -- git wrapper
  use 'tpope/vim-rsi'                        -- emacs binding in prompt
  use 'tpope/vim-surround'
  use 'tpope/vim-unimpaired'
  use 'tpope/vim-repeat'

  -- LSP settings
  use "neovim/nvim-lspconfig"
  use "hrsh7th/nvim-cmp"                     -- completion
  use 'hrsh7th/cmp-nvim-lsp'

  -- async make
  use "skywind3000/asyncrun.vim"

  use "nvim-tree/nvim-web-devicons" 

end)
