(import-macros {: lazyreq} :core.macros)

(local {: use} (lazyreq :core.packer))

;; NOTE: lua use % as escape in pattern

;; fnlfmt: skip
(use
  ;;;; Dependencies
  :wbthomason/packer.nvim {:opt true}
  :rktjmp/hotpot.nvim {}
  :babygau/luafun.nvim {}
  :nvim-lua/plenary.nvim {:module :plenary}
  :kevinhwang91/promise-async {:module :promise}
  :antoinemadec/FixCursorHold.nvim {}
  :kyazdani42/nvim-web-devicons {:module "nvim-web-devicons" :mod :devicons}
  ;;;; UI plugins
  "~/workspace/rose-pine.nvim.git/main" {:color :rose-pine}
  :akinsho/bufferline.nvim {:after :themer :mod :bufferline}
  :nvim-lualine/lualine.nvim {:after :themer :mod :lualine}
  :kyazdani42/nvim-tree.lua {:after :treesitter :mod :nvim-tree}
  ;;;; Editor plugins
  :tpope/vim-eunuch {:event :BufRead}
  :tpope/vim-rsi {:event :InsertEnter}
  :tpope/vim-repeat {:event :BufRead}
  :kevinhwang91/nvim-ufo {:after :treesitter :mod :ufo}
  :kylechui/nvim-surround {:event :BufRead :tag :* :init :nvim-surround}
  :ggandor/leap.nvim {:event :BufRead :mod :leap}
  :romainl/vim-qf {:ft :qf}
  :NvChad/nvim-colorizer.lua {:init :colorizer
                              :cmd [:ColorizerToggle
                                    :ColorizerAttachToBuffer
                                    :ColorizerDetachFromBuffer
                                    :ColorizerReloadAllBuffers]} ;; fork version
  ;;;; Fuzzy search engine
  "~/workspace/son-of-harpoon.git/main" {:event :BufRead :as :harpoon :mod :harpoon}
  :ahmedkhalf/project.nvim {:module_pattern "project.*" :init :project_nvim}
  :nvim-telescope/telescope-fzf-native.nvim {:module_pattern "fzf.*" :run "make"}
  :nvim-telescope/telescope.nvim {:as :telescope
                                  :event :BufRead
                                  :module_pattern "telescope.*"
                                  :mod :telescope}
  ;;;; Lang plugins
  :jaawerth/fennel.vim {}
  :folke/lua-dev.nvim {:module "lua-dev"}
  :simrat39/rust-tools.nvim {:module "rust-tools"}
  :jose-elias-alvarez/null-ls.nvim {:module "null-ls"}
  :neovim/nvim-lspconfig {:as :lspconfig :module :lspconfig}
  :williamboman/mason.nvim {:as :mason :module :mason}
  :williamboman/mason-lspconfig.nvim {:event :BufRead :mod :lsp}
  :nvim-treesitter/nvim-treesitter {:event :BufRead
                                    :as :treesitter
                                    :mod :nvim-treesitter}
  :nvim-treesitter/playground {:after :treesitter}
  :nvim-treesitter/nvim-treesitter-textobjects {:commit :e63c2ff8e38fad77299dd74e14c7c9360e1b3181
                                                :after :treesitter}
  ;; :mfussenegger/nvim-dap {:ft [:rust :typescript :typescriptreact]}
  :p00f/nvim-ts-rainbow {:after :treesitter}
  :andymass/vim-matchup {:after :treesitter}
  :JoosepAlviste/nvim-ts-context-commentstring {:after :treesitter :as :ts-context}
  :numToStr/Comment.nvim {:after :ts-context :mod :comment}
  :nvim-neorg/neorg-telescope {:module_pattern "neorg.modules.*"}
  :nvim-neorg/neorg {:after [:treesitter :telescope] :mod :neorg}
  :Olical/conjure {:branch :develop :mod :conjure :ft [:clojure :fennel :hy]}
  ;;;; Completion plugins
  :L3MON4D3/LuaSnip {:module_pattern "luasnip.*"}
  :hrsh7th/nvim-cmp {:commit :913eb8599816b0b71fe959693080917d8063b26a
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
  :tpope/vim-fugitive {:event :BufRead :mod :vim-fugitive}
  ;;;; Tool plugins
  ;; :lacygoill/vim-tmux {:ft :tmux} ;; not compatible with vim9script yet!
  :ericpruitt/tmux.vim {:ft :tmux}
  :aserowy/tmux.nvim {:event :BufRead :mod :tmux}
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
  )

(require :plugins.scratch)
