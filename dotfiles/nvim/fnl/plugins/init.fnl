(module plugins.init
  {autoload {: packer}
   require-macros [core.macros]})

(defn assoc [tbl ...]
  "Adds any number of key/value pairs to `tbl`, returning `tbl`. Like [[tset]]
  but for multiple pairs."
  (for [i 1 (select "#" ...) 2]
    (let [(k v) (select i ...)]
      (tset tbl k v)))
  tbl)

(defn plugin-config [name]
  "A shortcut to building a require string for your plugin
  configuration. Intended for use with packer's config or setup
  configuration options. Will prefix the name with `plugins.`
  before requiring."
  (.. "require('plugins." name "')"))

(defn plugin-init [name]
  "A shortcut to building a require string for your plugin
  configuration. Intended for use with packer's config or setup
  configuration options."
  (.. "require('" name "').setup {}"))

(defn colorscheme-pick [name]
  "A shortcut to building a require string for your colorscheme
  configuration. Intended for use with packer's config or setup
  configuration options. Will prefix the name with `colorscheme.`
  before requiring."
  (.. "require('colorschemes." name "')"))

;;;; Courtesy of Olical with rocks changes
(defn use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup {1 (fn [use use-rocks]
                         (for [i 1 (length pkgs) 2]
                           (let [name (. pkgs i)
                                 opts (. pkgs (+ i 1))]
                             (if (. opts :rock)
                                 (use-rocks name)
                                 (. opts :colorscheme)
                                 (use (assoc opts 1 name :event :VimEnter :as :colorscheme :config (colorscheme-pick (. opts :colorscheme))))
                                 (. opts :mod)
                                 (use (assoc opts 1 name :config (plugin-config (. opts :mod))))
                                 (. opts :init)
                                 (use (assoc opts 1 name :config (plugin-init (. opts :init))))
                                 (use (assoc opts 1 name))))))
                     :config {:compile_path tdt.paths.PACKER_COMPILED_PATH
                              :git {:clone_timeout 120 :depth 1}
                              :profile {:enable true :threshold 0}}})))

; fnlfmt: skip
; NOTE: lua use % as escape in pattern
(use
  ;;;; Dependencies
  :wbthomason/packer.nvim {:opt true}
  :lewis6991/impatient.nvim {}
  :nathom/filetype.nvim {}
  :Olical/aniseed {:branch :develop}
  :antoinemadec/FixCursorHold.nvim {}
  :nvim-lua/plenary.nvim {:as :plenary :module_pattern "plenary.*"}
  :kyazdani42/nvim-web-devicons {:module_pattern "nvim.web.devicons"}

  ;;;; UI plugins
  :rose-pine/neovim {:colorscheme :rosepine}
  ; :folke/tokyonight.nvim {:colorscheme :tokyonight}
  ; :woodyZootopia/iceberg.vim {:branch :support_LSP
  ;                             :colorscheme :iceberg}
  ; :RRethy/nvim-base16 {:colorscheme :base16-onedark}
  :akinsho/nvim-bufferline.lua {:after :colorscheme :mod :nvim-bufferline}
  :lukas-reineke/indent-blankline.nvim {:after :colorscheme :mod :indent-blankline}
  :nvim-lualine/lualine.nvim {:after :colorscheme :mod :lualine}
  :folke/which-key.nvim {:event :BufRead :mod :which-key}
  :kyazdani42/nvim-tree.lua {:event :BufRead :mod :nvim-tree}
  ;;;; Editor plugins
  :tpope/vim-eunuch {:event :BufWinEnter}
  :tpope/vim-abolish {:event :BufWinEnter}
  :tpope/vim-repeat {:event :BufWinEnter}
  :tpope/vim-commentary {:event :BufRead}
  :tpope/vim-surround {:event :BufRead}
  :tpope/vim-unimpaired {:event :BufRead}
  :akinsho/nvim-toggleterm.lua {:event :BufRead :mod :nvim-toggleterm}
  :rktjmp/highlight-current-n.nvim {:event :BufRead :mod :highlight-current-n}
  :junegunn/vim-easy-align {:event :BufRead :mod :vim-easy-align}
  :xiyaowong/accelerated-jk.nvim {:event :BufRead :init :accelerated-jk}
  :romainl/vim-qf {:ft :qf}
  :ahmedkhalf/project.nvim {:as :rooter :module_pattern "project.*" :init :project_nvim}
  :norcalli/nvim-colorizer.lua {:ft [:html :css
                                     :sass :vim
                                     :typescript :typescriptreact]}
  ;;;; Git plugins
  :tpope/vim-fugitive {:event :BufRead :mod :vim-fugitive}
  :tpope/vim-rhubarb {:after :vim-fugitive :cmd :GBrowse}
  :sindrets/diffview.nvim {:event :BufRead :mod :diffview}
  :lewis6991/gitsigns.nvim {:after :colorscheme :mod :gitsigns}
  ;;;; Lang plugins
  :nvim-treesitter/nvim-treesitter {:event :BufRead
                                    :as :treesitter
                                    ; :run ":TSUpdate"
                                    :mod :nvim-treesitter}
  ; :nvim-treesitter/nvim-treesitter-textobjects {:after :treesitter}
  ; :RRethy/nvim-treesitter-textsubjects {:after :treesitter}
  :p00f/nvim-ts-rainbow {:after :treesitter}
  ; :windwp/nvim-autopairs {:after :treesitter
  ;                         :mod :nvim-autopairs}
  ; :windwp/nvim-ts-autotag {:after :treesitter}
  :andymass/vim-matchup {:after :treesitter}
  :JoosepAlviste/nvim-ts-context-commentstring {:after [:vim-commentary :treesitter]}
  ; :gpanders/nvim-parinfer {:ft [:clojure :fennel :lisp]}
  :Olical/conjure {:branch :develop
                   :mod :conjure
                   :ft [:clojure :fennel :hy]}
  :mattn/emmet-vim {:mod :emmet
                    :ft [:css
                         :html
                         :javascript
                         :jsx
                         :markdown
                         :typescriptreact
                         :xml]}
  :jose-elias-alvarez/null-ls.nvim {:module_pattern "null.ls.*"}
  :jose-elias-alvarez/nvim-lsp-ts-utils {:module_pattern "nvim.lsp.ts.utils.*"}
  :folke/lua-dev.nvim {:module_pattern "lua.dev.*"}
  :neovim/nvim-lspconfig {:event :BufReadPre
                          :mod :lsp}
  :folke/trouble.nvim {:cmd :Trouble}
  :folke/todo-comments.nvim {:after :treesitter :init :todo-comments}

  ;;;; Fuzzy search engine
  :nvim-telescope/telescope-fzf-native.nvim {:as :fzf-native :module_pattern "fzf.*" :run "make"}
  :nvim-telescope/telescope.nvim {:requires [:plenary :fzf-native :rooter]
                                  :event :BufRead
                                  :cmd :Telescope
                                  :mod :telescope}
  ;;;; Completion plugins
  :L3MON4D3/LuaSnip {:module_pattern "luasnip.*"}
  :hrsh7th/nvim-cmp {:event :BufRead
                     :module_pattern "cmp.*"
                     :mod :nvim-cmp}
  :hrsh7th/cmp-path {:after :nvim-cmp}
  :hrsh7th/cmp-buffer {:after :nvim-cmp}
  :hrsh7th/cmp-calc {:after :nvim-cmp}
  :hrsh7th/cmp-nvim-lsp {:after :nvim-cmp}
  :hrsh7th/cmp-nvim-lua {:after :nvim-cmp}
  :tzachar/cmp-tabnine {:mod :cmp-tabnine :after :nvim-cmp}
  :PaterJason/cmp-conjure {:after [:nvim-cmp :conjure]}
  :saadparwaiz1/cmp_luasnip {:after :nvim-cmp}

  ;;;; Tool plugins
  :ellisonleao/glow.nvim {:mod :glow :ft [:markdown :md]}
  :editorconfig/editorconfig-vim {:ft [:go :c :cpp :rust :typescript :javascript :vim :zig]})
