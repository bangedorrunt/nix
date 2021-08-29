(module plugins.init
  {autoload {: packer 
             : cljlib}
   require-macros [core.macros]})

(def {: count : assoc : nil?} cljlib)

;;;; HACK: see https://github.com/wbthomason/packer.nvim/issues/180
;; Only use it once, disable next time
;; (vim.fn.setenv :MACOSX_DEPLOYMENT_TARGET :10.15)

(defn- load-plugin-configs? []
  (nil? vim.env.NVIM_SKIP_PLUGIN_CONFIGS))

(defn- req [name] 
  "A shortcut to building a require string for your plugin
  configuration. Intended for use with packer's config or setup
  configuration options. Will prefix the name with `plugins.`
  before requiring." 
  (.. "require('plugins." name "')"))

(defn- default [name] 
  "A shortcut to building a require string for your plugin
  configuration. Intended for use with packer's config or setup
  configuration options. Will prefix the name with `plugins.`
  before requiring." 
  (.. "require('" name "').setup {}"))

;;;; Curtesy of Olical with rocks changes
(defn- use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup {1 (fn [use use-rocks]
                         (for [i 1 (count pkgs) 2]
                           (let [name (. pkgs i)
                                 opts (. pkgs (+ i 1))]
                             (if (. opts :rock)
                                  (use-rocks name)
                                 (and (load-plugin-configs?) (. opts :mod))
                                  (use (assoc opts 1 name :config (req (. opts :mod))))
                                 (use (assoc opts 1 name))))))
                     :config {:compile_path tdt.paths.PACKER_COMPILED_PATH
                              :git {:clone_timeout 120 :depth 1}
                              :profile {:enable true :threshold 0}}})))

;; fnlfmt: skip
(use
  ;;;; Dependencies
  :wbthomason/packer.nvim {:opt true}
  ;; :nvim-lua/popup.nvim {:as :popup :module_pattern "popup.*" :opt true}
  :nvim-lua/plenary.nvim {:as :plenary :module_pattern "plenary.*" :opt true}
  :kyazdani42/nvim-web-devicons {:module_pattern "nvim-web-devicons*" :opt true}
  :fzy {:rock true}
  :Olical/aniseed {}
  ;; :rktjmp/hotpot.nvim {}

  ;;;; UI plugins
  :babygau/tokyonight.nvim {:branch :tdt
                            :as :colorscheme
                            :event :VimEnter
                            :mod :tokyonight}
  :akinsho/nvim-bufferline.lua {:after :colorscheme :mod :nvim-bufferline}
  :lukas-reineke/indent-blankline.nvim {:after :colorscheme
                                        :mod :indent-blankline}
  :hoob3rt/lualine.nvim {:after :colorscheme
                         :mod :lualine}
  ;; :lewis6991/gitsigns.nvim {:cond false
  ;;                           :event [:BufRead :BufNewFile]
  ;;                           :mod :gitsigns}
  :folke/which-key.nvim {:event :BufRead
                         :mod :which-key}
  :kyazdani42/nvim-tree.lua {:event :BufEnter :mod :nvim-tree}
  ;;;; Editor plugins
  :akinsho/nvim-toggleterm.lua {:event :BufRead
                                :mod :nvim-toggleterm}
  :karb94/neoscroll.nvim {:event :WinScrolled
                          :mod :neoscroll}
  :b3nj5m1n/kommentary {:event :BufRead
                        :mod :kommentary}
  :junegunn/vim-easy-align {:event :BufRead
                            :mod :vim-easy-align}
  :rhysd/accelerated-jk {:event :BufRead
                         :mod :accelerated-jk}
  :machakann/vim-sandwich {:event :BufRead
                           :mod :vim-sandwich}
  :romainl/vim-qf {:ft :qf}
  :tpope/vim-eunuch {:event :BufRead}
  :tpope/vim-repeat {}
  :rktjmp/highlight-current-n.nvim {:event :BufRead}

  :norcalli/nvim-colorizer.lua {:ft [:html :css
                                     :sass :vim
                                     :typescript :typescriptreact]}
  :ahmedkhalf/project.nvim {:as :rooter :module_pattern "project*" :mod :project_nvim}
  ;;;; Git plugins
  :tpope/vim-fugitive {:event :BufRead :mod :vim-fugitive}
  :tpope/vim-rhubarb {:after :vim-fugitive :cmd ":Gbrowse"}
  :sindrets/diffview.nvim {:event :BufRead}
  ;;;; Lang plugins
  :nvim-treesitter/nvim-treesitter {:as :treesitter
                                    :run ":TSUpdate"
                                    :event :BufRead
                                    :mod :nvim-treesitter}
  :nvim-treesitter/nvim-treesitter-textobjects {:after :treesitter}
  :p00f/nvim-ts-rainbow {:after :treesitter}
  :windwp/nvim-autopairs {:event :BufRead
                          :module_pattern "nvim-autopairs*"
                          :mod :nvim-autopairs}
  :windwp/nvim-ts-autotag {:after :treesitter}
  :guns/vim-sexp {:ft [:clojure :fennel
                       :hy :lisp :scheme]
                  :mod :vim-sexp}
  :Olical/conjure {:mod :conjure
                   :ft [:clojure :fennel :hy]}
  :mattn/emmet-vim {:mod :emmet
                    :ft [:css
                         :html
                         :javascript
                         :jsx
                         :markdown
                         :typescriptreact
                         :xml]}
  ;;;; Completion plugins
  ;; :ms-jpq/coq_nvim {:branch :coq
  ;;                   :event :BufReadPre
  ;;                   :module_pattern "coq*"
  ;;                   :mod :coq_nvim
  ;;                   :run ":COQdeps"}
  ;; :ms-jpq/coq.artifacts {:branch :artifacts}
  :neovim/nvim-lspconfig {:event :BufReadPre
                          :mod :lsp
                          :requires [:jose-elias-alvarez/null-ls.nvim
                                     :jose-elias-alvarez/nvim-lsp-ts-utils
                                     :folke/lua-dev.nvim]}
  :folke/trouble.nvim {:cmd :Trouble}
  :folke/todo-comments.nvim {:after :treesitter :config (default :todo-comments)}
  ;; :ray-x/navigator.lua {:event :BufReadPre
  ;;                       :module_pattern "navigator*"
  ;;                       :requires [[:ray-x/guihua.lua {:run "cd lua/fzy && make" :opt true}]
  ;;                                  [:ray-x/lsp_signature.nvim {:opt true}]]}
  ;; :ray-x/lsp_signature.nvim {:after :nvim-lspconfig}
  ;; :glepnir/lspsaga.nvim {:cmd :Lspsaga :disable true}

  :nvim-telescope/telescope-fzf-native.nvim {:as :fzf-native :module_pattern "fzf*" :run "make" :opt false}
  :nvim-telescope/telescope.nvim {:requires [:plenary :fzf-native :rooter]
                                  :event :BufRead
                                  :module_pattern "telescope.*"
                                  :cmd :Telescope
                                  :mod :telescope}
  ;; :liuchengxu/vim-clap {:run ":Clap install-binary!"
  ;;                       :mod :vim-clap}
  ;; :camspiers/snap {:event :BufRead :mod :snap)}

  ;; :hrsh7th/nvim-compe {:after :treesitter :mod :nvim-compe}
  ;; :tami5/compe-conjure {:after [:nvim-compe :conjure] :ft [:clojure :fennel :hy]}
  ;; :hrsh7th/vim-vsnip {:after :nvim-compe
  ;;                     :setup (fn [] (let! :vsnip_snippet_dir (.. tdt.paths.VIM_PATH :/snippets)))}

  :hrsh7th/nvim-cmp {:module_pattern "cmp*" :mod :nvim-cmp}
  :hrsh7th/cmp-path {:after :nvim-cmp}
  :hrsh7th/cmp-buffer {:after :nvim-cmp}
  :hrsh7th/cmp-calc {:after :nvim-cmp}
  :hrsh7th/cmp-nvim-lsp {:after :nvim-cmp :module_pattern "cmp_nvim_lsp"}
  :hrsh7th/cmp-nvim-lua {:after :nvim-cmp}
  :PaterJason/cmp-conjure {:after [:nvim-cmp :conjure]}
  :abzcoding/cmp_luasnip {:after :nvim-cmp}
  :hrsh7th/cmp-emoji {:after :nvim-cmp}
  :L3MON4D3/LuaSnip {:after :nvim-cmp :module_pattern "luasnip"}
  ;; :rafamadriz/friendly-snippets {}

  ;;;; Tool plugins
  :editorconfig/editorconfig-vim {:ft [:go :c :cpp :rust
                                       :typescript :javascript
                                       :vim :zig ]})
