 (local {: use} (require :core.packer))

;; fnlfmt: skip
;; NOTE: lua use % as escape in pattern
(use
  ;;;; Dependencies
  :wbthomason/packer.nvim {:opt true}
  ;; :Olical/aniseed {:branch :develop}
  :rktjmp/hotpot.nvim {:branch :nightly}
  :babygau/luafun.nvim {}
  :antoinemadec/FixCursorHold.nvim {}
  :nvim-lua/plenary.nvim {:as :plenary :module_pattern "plenary.*"}
  :kyazdani42/nvim-web-devicons {:module_pattern "nvim.web.devicons" :mod :devicons}

  ;;;; UI plugins
  :rose-pine/neovim {:color :rose-pine}
  :lukas-reineke/indent-blankline.nvim {:after :themer :mod :indent-blankline}
  :akinsho/bufferline.nvim {:after :themer :mod :bufferline}
  :nvim-lualine/lualine.nvim {:after :themer :mod :lualine}
  :b4mbus/todo-comments.nvim {:event :BufRead :init :todo-comments} ;; fork version
  :kyazdani42/nvim-tree.lua {:event :BufRead :mod :nvim-tree}
  ;;;; Editor plugins
  :tpope/vim-eunuch {:event :BufRead}
  :tpope/vim-repeat {:event :BufRead}
  ;; :tpope/vim-sleuth {:event :BufRead}
  :NMAC427/guess-indent.nvim {:event :BufRead :init :guess-indent}
  :kylechui/nvim-surround {:event :BufRead :tag :* :init :nvim-surround}
  :ggandor/leap.nvim {:event :BufRead :mod :leap}
  :rktjmp/highlight-current-n.nvim {:event :BufRead :mod :highlight-current-n}
  :junegunn/vim-easy-align {:event :BufRead :mod :vim-easy-align}
  :xiyaowong/accelerated-jk.nvim {:event :BufRead :init :accelerated-jk}
  :romainl/vim-qf {:ft :qf}
  :NvChad/nvim-colorizer.lua {:ft [:html :css
                                   :sass :vim
                                   :typescript :typescriptreact]}
  ;;;; Fuzzy search engine
  :ThePrimeagen/harpoon {:event :BufRead :mod :harpoon}
  :ahmedkhalf/project.nvim {:module_pattern "project.*" :init :project_nvim}
  :nvim-telescope/telescope-fzf-native.nvim {:module_pattern "fzf.*" :run "make"}
  :nvim-telescope/telescope.nvim {:as :telescope
                                  :event :BufRead
                                  :module_pattern "telescope.*"
                                  :mod :telescope}
  ;;;; Lang plugins
  :folke/trouble.nvim {:cmd :Trouble}
  :jose-elias-alvarez/null-ls.nvim {:module_pattern "null.ls.*"}
  :onsails/lspkind-nvim {:module_pattern "lspkind"}
  :simrat39/rust-tools.nvim {:module "rust-tools"}
  :neovim/nvim-lspconfig {:as :lspconfig :module_pattern "lspconfig.*"}
  :williamboman/mason.nvim {:as :mason :module_pattern "mason"}
  :williamboman/mason-lspconfig.nvim {:event :BufRead :mod :lsp}
  :nvim-treesitter/nvim-treesitter {:event :BufRead
                                    :as :treesitter
                                    :mod :nvim-treesitter}
  :nvim-treesitter/nvim-treesitter-textobjects {:after :treesitter}
  :mfussenegger/nvim-dap {:ft [:rust :typescript :typescriptreact]}
  :p00f/nvim-ts-rainbow {:after :treesitter}
  :andymass/vim-matchup {:after :treesitter}
  :JoosepAlviste/nvim-ts-context-commentstring {:after :treesitter :as :ts-context}
  :numToStr/Comment.nvim {:after :ts-context :mod :comment}
  :nvim-neorg/neorg-telescope {:module_pattern "neorg.modules.*"}
  :nvim-neorg/neorg {:after [:treesitter :telescope] :mod :neorg :ft [:norg]}
  :Olical/conjure {:branch :develop :mod :conjure :ft [:clojure :fennel :hy]}
  ;;;; Completion plugins
  :L3MON4D3/LuaSnip {:module_pattern "luasnip.*"}
  :hrsh7th/nvim-cmp {:commit :706371f1300e7c0acb98b346f80dad2dd9b5f679
                     :event :BufRead
                     :module_pattern "cmp.*"
                     :mod :nvim-cmp}
  :hrsh7th/cmp-cmdline {:after :nvim-cmp}
  :hrsh7th/cmp-path {:after :nvim-cmp}
  :hrsh7th/cmp-buffer {:after :nvim-cmp}
  :hrsh7th/cmp-calc {:after :nvim-cmp}
  :hrsh7th/cmp-nvim-lsp {:after :nvim-cmp}
  :hrsh7th/cmp-nvim-lua {:after :nvim-cmp}
  :PaterJason/cmp-conjure {:after [:nvim-cmp :conjure]}
  :saadparwaiz1/cmp_luasnip {:after :nvim-cmp}
  ;;;; Git plugins
  :lewis6991/gitsigns.nvim {:after :themer :mod :gitsigns}
  :sindrets/diffview.nvim {:after :treesitter :mod :diffview}
  :tpope/vim-fugitive {:event :BufRead :mod :vim-fugitive}
  ;;;; Tool plugins
  :gpanders/editorconfig.nvim {:ft [:go :c :cpp :rust :typescript :javascript :vim :zig]}
  ;;;; Unused plugins
  ;; :lewis6991/impatient.nvim {}
  ;; :github/copilot.vim {}
  ;; :tzachar/cmp-tabnine {:mod :cmp-tabnine :after :nvim-cmp}
  ;; :mg979/vim-visual-multi {:event :BufRead}
  ;; :RRethy/nvim-base16 {:color :base16-onedark}
  ;; :tpope/vim-commentary {:event :BufRead :as :vim-commentary}
  ;; :tpope/vim-surround {:event :BufRead}
  ;; :tpope/vim-abolish {:event :BufWinEnter}
  ;; :akinsho/toggleterm.nvim {:event :BufRead :mod :toggleterm}
  ;; :folke/which-key.nvim {:event :BufRead :mod :which-key}
  )

(require :plugins.scratch)
