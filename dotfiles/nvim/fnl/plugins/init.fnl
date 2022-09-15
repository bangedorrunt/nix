(import-macros {: lazyreq} :core.macros)

(local {: use} (lazyreq :core.packer))

;; NOTE: lua use % as escape in pattern

;; fnlfmt: skip
(use
  ;;;; Dependencies
  :wbthomason/packer.nvim {:opt true}
  :rktjmp/hotpot.nvim {}
  :nvim-lua/plenary.nvim {}
  :kevinhwang91/promise-async {}
  :antoinemadec/FixCursorHold.nvim {}
  :kyazdani42/nvim-web-devicons {:mod :devicons}
  ;;;; UI plugins
  "~/workspace/rose-pine.nvim.git/main" {:color :rose-pine}
  :akinsho/bufferline.nvim {:mod :bufferline}
  :nvim-lualine/lualine.nvim {:mod :lualine}
  :kyazdani42/nvim-tree.lua {:mod :nvim-tree}
  ;;;; Editor plugins
  :tpope/vim-eunuch {}
  :tpope/vim-rsi {}
  :tpope/vim-repeat {}
  :kevinhwang91/nvim-ufo {:mod :ufo}
  :kylechui/nvim-surround {:init :nvim-surround}
  :ggandor/leap.nvim {:mod :leap}
  :romainl/vim-qf {}
  :NvChad/nvim-colorizer.lua {:init :colorizer} ;; fork version
  ;;;; Fuzzy search engine
  "~/workspace/son-of-harpoon.git/main" {:as :harpoon :mod :harpoon}
  :nvim-telescope/telescope-fzf-native.nvim {:run "make"}
  :nvim-telescope/telescope.nvim {:mod :telescope}
  ;;;; Lang plugins
  :neovim/nvim-lspconfig {}
  :williamboman/mason.nvim {}
  :williamboman/mason-lspconfig.nvim {:mod :lsp}
  :jaawerth/fennel.vim {}
  :folke/lua-dev.nvim {}
  :simrat39/rust-tools.nvim {}
  :jose-elias-alvarez/null-ls.nvim {}
  :nvim-treesitter/nvim-treesitter {:as :treesitter
                                    :mod :nvim-treesitter}
  :nvim-treesitter/playground {}
  :nvim-treesitter/nvim-treesitter-textobjects {}
  :p00f/nvim-ts-rainbow {}
  :andymass/vim-matchup {}
  :JoosepAlviste/nvim-ts-context-commentstring {}
  :numToStr/Comment.nvim {:mod :comment}
  :nvim-neorg/neorg-telescope {}
  :nvim-neorg/neorg {:mod :neorg}
  :Olical/conjure {:branch :develop :mod :conjure}
  ;;;; Completion plugins
  :L3MON4D3/LuaSnip {}
  :hrsh7th/nvim-cmp {:commit :913eb8599816b0b71fe959693080917d8063b26a
                     :mod :nvim-cmp}
  :hrsh7th/cmp-cmdline {}
  :hrsh7th/cmp-path {}
  :hrsh7th/cmp-buffer {}
  :hrsh7th/cmp-calc {}
  :hrsh7th/cmp-nvim-lsp {}
  :hrsh7th/cmp-nvim-lua {}
  :PaterJason/cmp-conjure {}
  :saadparwaiz1/cmp_luasnip {}
  ;;;; Git plugins
  :lewis6991/gitsigns.nvim {:mod :gitsigns}
  :tpope/vim-fugitive {:mod :vim-fugitive}
  ;;;; Tool plugins
  :ericpruitt/tmux.vim {}
  :aserowy/tmux.nvim {:mod :tmux}
  :jbyuki/venn.nvim {:cmd :VBox}
  :gpanders/editorconfig.nvim {:ft [:go :c :cpp :rust :typescript :javascript :vim :zig]}
  ;; :glacambre/firenvim {:run (fn [] ((. vim.fn "firenvim#install") 1))}
  ;;;; Unused plugins
  ;; :lewis6991/impatient.nvim {}
  ;; :Olical/aniseed {:branch :develop}
  ;; :github/copilot.vim {}
  ;; :tzachar/cmp-tabnine {:mod :cmp-tabnine :after :nvim-cmp}
  ;; :mg979/vim-visual-multi {:event :BufRead}
  ;; :RRethy/nvim-base16 {:color :base16-onedark}
  ;; :tpope/vim-commentary {:event :BufRead :as :vim-commentary}
  ;; :tpope/vim-surround {:event :BufRead}
  ;; :tpope/vim-abolish {:event :BufWinEnter}
  ;; :tpope/vim-sleuth {:event :BufRead}
  ;; :NMAC427/guess-indent.nvim {:event :BufRead :init :guess-indent}
  ;; :sindrets/diffview.nvim {:after :treesitter :mod :diffview}
  ;; :akinsho/toggleterm.nvim {:event :BufRead :mod :toggleterm}
  ;; :anuvyklack/hydra.nvim {:after :themer :mod :hydra}
  ;; :folke/which-key.nvim {:event :BufRead :mod :which-key}
  ;; :onsails/lspkind-nvim {:module_pattern "lspkind"}
  ;; :rktjmp/highlight-current-n.nvim {:event :BufRead :mod :highlight-current-n}
  ;; :lukas-reineke/indent-blankline.nvim {:after :treesitter :mod :indent-blankline}
  ;; :xiyaowong/accelerated-jk.nvim {:event :BufRead :init :accelerated-jk}
  ;; :declancm/cinnamon.nvim {:event :BufRead :init :cinnamon}
  ;; :vigoux/notifier.nvim {:event :BufEnter :init :notifier}
  ;; :hrsh7th/nvim-pasta {:event :BufReadPost}
  ;; :stevearc/dressing.nvim {:event :BufReadPost}
  ;; :j-hui/fidget.nvim {:event :BufReadPost :mod :fidget}
  ;; :folke/trouble.nvim {:cmd :Trouble}
  ;; :folke/todo-comments.nvim {:event :BufRead :init :todo-comments}
  ;; "babygau/rose-pine.nvim" {:color :rose-pine}
  ;; :junegunn/vim-easy-align {:event :BufRead :mod :vim-easy-align}
  ;; :ahmedkhalf/project.nvim {:init :project_nvim}
  ;; :mfussenegger/nvim-dap {:ft [:rust :typescript :typescriptreact]}
  ;; :lacygoill/vim-tmux {:ft :tmux} ;; not compatible with vim9script yet!
  ;; :babygau/luafun.nvim {}
  )

(require :plugins.scratch)
