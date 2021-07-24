-- HACK: see https://github.com/wbthomason/packer.nvim/issues/180
-- vim.fn.setenv('MACOSX_DEPLOYMENT_TARGET', '10.15')

return require('packer').startup {
  function()
    use { 'wbthomason/packer.nvim', opt = true }

    local fn = vim.fn
    -- Ref: jose-elias-alvarez/dotfiles
    -- Until #420 is merged, this  cannot be used OTHERWISE config is not loaded
    -- NOTE: if  you name a local var as `config`, it will not work,
    -- hence use another name to avoid any confict
    local config_of = function(name)
      pcall(require, 'plugins.' .. name)
    end
    local use_with_config = function(path, name)
      use { path, config = config_of(name) }
    end

    use { 'nvim-lua/popup.nvim' }
    use { 'nvim-lua/plenary.nvim' }

    -- ----------
    -- UI PLUGINS
    -- ----------
    -- Dark UI
    use {
      'https://gitlab.com/__tpb/monokai-pro.nvim',
      disable = true,
      config = function()
        vim.g.monokaipro_filter = 'spectrum'
        vim.g.monokaipro_terminal_colors = true
        vim.g.monokaipro_italic_functions = true
        vim.g.monokaipro_sidebars = { 'vista_kind', 'packer' }
        -- Change the "hint" color to the "orange" color, and make the "error" color bright red
        vim.g.monokaipro_colors = { hint = 'orange', error = '#ff0000' }
        -- Load the colorscheme
        -- vim.cmd[[colorscheme monokaipro]]
      end,
    }

    -- Light UI
    use {
      'sainnhe/gruvbox-material',
      disable = true,
      config = function()
        vim.o.background = 'light'
        vim.g.gruvbox_material_enable_italic = 1
        vim.g.gruvbox_material_diagnostic_virtual_text = 'colored'
        vim.g.gruvbox_material_better_performance = 1
        -- vim.cmd[[colorscheme gruvbox-material]]
      end,
    }
    use {
      'folke/tokyonight.nvim',
      as = 'theme',
      event = 'VimEnter',
      config = function()
        vim.g.tokyonight_style = 'night'
        -- Better performance
        require('tokyonight').colorscheme()
      end,
    }
    use { 'ishan9299/modus-theme-vim', disable = true }

    use_with_config('akinsho/nvim-bufferline.lua', 'nvim-bufferline')

    use {
      'hoob3rt/lualine.nvim',
      after = 'theme',
      config = function()
        require 'plugins.lualine'
      end,
      requires = { 'kyazdani42/nvim-web-devicons', opt = true },
    }

    use {
      'folke/which-key.nvim',
      event = 'BufRead',
      config = function()
        require 'plugins.which-key'
      end,
    }

    use {
      'lukas-reineke/indent-blankline.nvim',
      after = 'theme',
      config = function()
        require 'plugins.indent-blankline'
      end,
    }

    use {
      'lewis6991/gitsigns.nvim',
      event = { 'BufRead', 'BufNewFile' },
      config = function()
        require 'plugins.gitsigns'
      end,
    }

    -- --------------
    -- EDITOR PLUGINS
    -- --------------

    use {
      'akinsho/nvim-toggleterm.lua',
      event = 'BufRead',
      config = function()
        require 'plugins.toggleterm'
      end,
    }

    -- This is bad for Vimmer
    use {
      'ggandor/lightspeed.nvim',
      disable = true,
      config = function()
        require 'plugins.lightspeed'
      end,
    }

    -- use { 'marklcrns/vim-smartq', event = 'BufRead' }

    use {
      'karb94/neoscroll.nvim',
      event = 'WinScrolled',
      config = function()
        -- vim.cmd[[setlocal scrolloff=0]]
        require('neoscroll').setup {
          -- All these keys will be mapped to their corresponding default scrolling animation
          mappings = { '<C-u>', '<C-d>', '<C-b>', '<C-f>', '<C-y>', '<C-e>', 'zt', 'zz', 'zb' },
          -- hide_cursor = true,       -- Hide cursor while scrolling
          stop_eof = true, -- Stop at <EOF> when scrolling downwards
          use_local_scrolloff = false, -- Use the local scope of scrolloff instead of the global scope
          respect_scrolloff = false, -- Stop scrolling when the cursor reaches the scrolloff margin of the file
          cursor_scrolls_alone = true, -- The cursor will keep on scrolling even if the window cannot scroll further
          easing_function = nil, -- Default easing function
        }
      end,
    }

    use { 'tpope/vim-eunuch', event = 'BufRead' }
    use_with_config('b3nj5m1n/kommentary', 'kommentary')
    use {
      'junegunn/vim-easy-align',
      config = function()
        local map = require('core.utils').map
        -- Use <C-x> to switch to regex
        map('n', '<Leader>xa', '<Plug>(EasyAlign)', { noremap = false })
        map('x', '<Leader>xa', '<Plug>(EasyAlign)', { noremap = false })
      end,
    }
    use { 'tpope/vim-repeat' }
    use {
      'rhysd/accelerated-jk',
      event = 'BufRead',
      config = function()
        local map = require('core.utils').map
        map('n', 'j', 'v:lua.enhance_jk_move("j")', { noremap = false, expr = true })
        map('n', 'k', 'v:lua.enhance_jk_move("k")', { noremap = false, expr = true })
      end,
    }
    use { 'romainl/vim-qf', ft = 'qf' }
    use {
      'norcalli/nvim-colorizer.lua',
      ft = { 'html', 'css', 'sass', 'vim', 'typescript', 'typescriptreact' },
      config = function()
        require 'plugins.nvim-colorizer'
      end,
    }

    -- ------------------
    -- COMPLETION PLUGINS
    -- ------------------
    use {
      'neovim/nvim-lspconfig',
      event = 'BufReadPre',
      config = function()
        require 'plugins.lsp'
      end,
      requires = {
        'jose-elias-alvarez/null-ls.nvim',
        'jose-elias-alvarez/nvim-lsp-ts-utils',
        'folke/lua-dev.nvim',
      },
    }
    use {
      'folke/trouble.nvim',
      cmd = 'Trouble',
      config = "require('trouble').setup {}",
      requires = 'kyazdani42/nvim-web-devicons',
    }
    use { 'glepnir/lspsaga.nvim', disable = true, cmd = 'Lspsaga' }

    use_with_config('hrsh7th/nvim-compe', 'nvim-compe')

    use_with_config('hrsh7th/vim-vsnip', 'vim-vsnip')

    use {
      'junegunn/fzf',
      disable = true,
      run = function()
        fn['fzf#install']()
      end,
    }
    -- use {'junegunn/fzf.vim'}

    -- Cannot install rocks?
    -- `MACOSX_DEPLOYMENT_TARGET=10.15 nvim` then run `PackerSync`
    use {
      'camspiers/snap',
      disable = true,
      rocks = { 'fzy' },
      config = function()
        require 'plugins.snap'
      end,
    }

    use {
      'nvim-telescope/telescope.nvim',
      event = 'BufRead',
      config = function()
        require 'plugins.telescope'
      end,
      requires = {
        { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' },
      },
    }
    -- use {'rmagatti/session-lens',
    --   config = function()
    --     require('auto-session').setup {
    --       auto_session_enabled = true,
    --       auto_save_enabled = true,
    --       auto_session_root_dir = ttd.paths.CACHE_DIR..'session/'
    --     }
    --     require('session-lens').setup {
    --       shorten_path = false,
    --       previewer = false
    --     }
    --   end,
    --   requires = {'rmagatti/auto-session'}
    -- }

    use {
      'mattn/emmet-vim',
      event = 'InsertEnter',
      ft = {
        'html',
        'css',
        'javascript',
        'javascriptreact',
        'vue',
        'typescript',
        'typescriptreact',
      },
      config = function()
        require 'plugins.emmet'
      end,
    }

    -- ------------
    -- LANG PLUGINS
    -- ------------

    use {
      'nvim-treesitter/nvim-treesitter',
      as = 'treesitter',
      run = ':TSUpdate',
      event = 'BufRead',
      config = function()
        require 'plugins.nvim-treesitter'
      end,
    }
    use {
      'windwp/nvim-autopairs',
      after = 'treesitter',
      config = function()
        require 'plugins.nvim-autopairs'
      end,
    }
    use { 'windwp/nvim-ts-autotag', after = 'treesitter' }
    use { 'nvim-treesitter/nvim-treesitter-textobjects', after = 'treesitter' }

    -- TODO: Set it up
    -- use {'ray-x/lsp_signature.nvim'}

    -- --------------
    -- TOOLS PLUGINS
    -- --------------

    use {
      'editorconfig/editorconfig-vim',
      ft = { 'go', 'typescript', 'javascript', 'vim', 'rust', 'zig', 'c', 'cpp' },
    }
    -- TODO: Set it up
    -- use {'TimUntersberger/neogit'}

    use {
      'iamcco/markdown-preview.nvim',
      ft = 'markdown',
      config = function()
        vim.g.mkdp_auto_start = 0
      end,
    }
  end,
  config = {
    compile_path = ttd.paths.PACKER_COMPILED_PATH,
    git = { clone_timeout = 120 },
    profile = {
      enable = true,
      threshold = 0, -- The amount in ms that a plugins load time must be over for it to be included in the profile
    },
    display = {
      open_fn = function()
        return require('packer.util').float { border = 'single' }
      end,
    },
  },
}
