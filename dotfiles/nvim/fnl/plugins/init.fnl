 (local {: use} (require :core.packer))

;; fnlfmt: skip
;; NOTE: lua use % as escape in pattern
(use
  ;;;; Dependencies
  :lewis6991/impatient.nvim {}
  :wbthomason/packer.nvim {:opt true}
  ;; :Olical/aniseed {:branch :develop}
  :rktjmp/hotpot.nvim {:branch :nightly}
  :babygau/luafun.nvim {}
  :antoinemadec/FixCursorHold.nvim {}
  :nvim-lua/plenary.nvim {:as :plenary :module_pattern "plenary.*"}
  :kyazdani42/nvim-web-devicons {:module_pattern "nvim.web.devicons" :mod :devicons}

  ;;;; UI plugins
  :rose-pine/neovim {:color :rosepine}
  :lukas-reineke/indent-blankline.nvim {:after :colorscheme :mod :indent-blankline}
  :akinsho/bufferline.nvim {:after :colorscheme :mod :bufferline}
  :nvim-lualine/lualine.nvim {:after :colorscheme :mod :lualine}
  :folke/which-key.nvim {:event :BufRead :mod :which-key}
  :b4mbus/todo-comments.nvim {:event :BufRead :init :todo-comments}
  :kyazdani42/nvim-tree.lua {:event :BufRead :mod :nvim-tree}
  ;;;; Editor plugins
  :tpope/vim-commentary {:event :BufRead :as :vim-commentary}
  :tpope/vim-eunuch {:event :BufWinEnter}
  :tpope/vim-abolish {:event :BufWinEnter}
  :tpope/vim-repeat {:event :BufWinEnter}
  :tpope/vim-surround {:event :BufRead}
  :tpope/vim-sleuth {:event :BufRead}
  :ggandor/leap.nvim {:event :BufRead :mod :leap}
  :akinsho/toggleterm.nvim {:event :BufRead :mod :toggleterm}
  :rktjmp/highlight-current-n.nvim {:event :BufRead :mod :highlight-current-n}
  :junegunn/vim-easy-align {:event :BufRead :mod :vim-easy-align}
  :xiyaowong/accelerated-jk.nvim {:event :BufRead :init :accelerated-jk}
  :romainl/vim-qf {:ft :qf}
  :ahmedkhalf/project.nvim {:as :rooter :module_pattern "project.*" :init :project_nvim}
  :NvChad/nvim-colorizer.lua {:ft [:html :css
                                   :sass :vim
                                   :typescript :typescriptreact]}
  ;;;; Fuzzy search engine
  :ThePrimeagen/harpoon {:event :BufRead :mod :harpoon}
  :nvim-telescope/telescope-fzf-native.nvim {:as :fzf-native :module_pattern "fzf.*" :run "make"}
  :nvim-telescope/telescope.nvim {:as :telescope
                                  :module_pattern "telescope.*"
                                  :branch "0.1.x"
                                  :event :BufRead
                                  :cmd :Telescope
                                  :mod :telescope}
  ;;;; Git plugins
  :tpope/vim-fugitive {:event :BufRead :mod :vim-fugitive}
  :tpope/vim-rhubarb {:after :vim-fugitive :cmd :GBrowse}
  :sindrets/diffview.nvim {:event :BufRead :mod :diffview}
  :lewis6991/gitsigns.nvim {:after :colorscheme :mod :gitsigns}
  ;;;; Lang plugins
  :folke/trouble.nvim {:cmd :Trouble}
  :jose-elias-alvarez/null-ls.nvim {:module_pattern "null.ls.*"}
  :onsails/lspkind-nvim {:module_pattern "lspkind"}
  :simrat39/rust-tools.nvim {}
  :neovim/nvim-lspconfig {:as :lspconfig :module_pattern "lspconfig.*"}
  :williamboman/mason.nvim {:as :mason :module_pattern "mason"}
  :williamboman/mason-lspconfig.nvim {:event :BufReadPre :mod :lsp}
  :nvim-treesitter/nvim-treesitter {:event :BufRead
                                    :as :treesitter
                                    :mod :nvim-treesitter}
  ;; :mfussenegger/nvim-dap {:ft [:rust :typescript :typescriptreact]}
  :p00f/nvim-ts-rainbow {:after :treesitter}
  :andymass/vim-matchup {:after :treesitter}
  :JoosepAlviste/nvim-ts-context-commentstring {:after [:vim-commentary :treesitter] :as :ts-context}
  :nvim-neorg/neorg-telescope {:module_pattern "neorg.modules.*"}
  :nvim-neorg/neorg {:after [:treesitter :telescope] :mod :neorg :ft [:norg]}
<<<<<<< HEAD
  ;; :babygau/aniseed-playground {:cmd :AniseedPlayground}
=======
>>>>>>> 508d14b (Neovim(feat): make transition to `hotpot`)
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
  ;;;; Tool plugins
  :gpanders/editorconfig.nvim {:ft [:go :c :cpp :rust :typescript :javascript :vim :zig]}
  ;;;; Unused plugins
  ;; :github/copilot.vim {}
  ;; :tzachar/cmp-tabnine {:mod :cmp-tabnine :after :nvim-cmp}
  ;; :mg979/vim-visual-multi {:event :BufRead}
  ;; :RRethy/nvim-base16 {:color :base16-onedark}
  ;; :numToStr/Comment.nvim {:after :ts-context :mod :comment}
  )

(require :plugins.scratch)
