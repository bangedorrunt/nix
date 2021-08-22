(module plugins.init
  {autoload {packer packer
             cljfun bulb}})

(import-macros {: let!} :core.macros)

;;;; HACK: see https://github.com/wbthomason/packer.nvim/issues/180
;; Only use it once, disable next time
;; (vim.fn.setenv :MACOSX_DEPLOYMENT_TARGET :10.15)

(defn- safe-require-plugin-config [name]
       "Safely require a module under the plugins.* prefix. Will catch errors
  and print them while continuing execution, allowing other plugins to load
  even if one configuration module is broken."
       (let [(ok? val-or-err) (pcall require (.. :plugins. name))]
         (when (not ok?)
           (print (.. "Plugin config error: " val-or-err)))))

(defn- req [name] 
  "A shortcut to building a require string for your plugin
  configuration. Intended for use with packer's config or setup
  configuration options. Will prefix the name with `plugins.`
  before requiring." 
  (.. "require('plugins." name "')"))

;;;; Curtesy of Olical with rocks changes
(defn- use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup {1 (fn [use use-rocks]
                         (for [i 1 (cljfun.count pkgs) 2]
                           (let [name (. pkgs i)
                                 opts (. pkgs (+ i 1))]
                             (-?> (. opts :mod) (safe-require-plugin-config))
                             (if (. opts :rock)
                                 (use-rocks name)
                                 (use (cljfun.assoc! opts 1 name))))))
                     :config {:compile_path tdt.paths.PACKER_COMPILED_PATH
                              :git {:clone_timeout 120}
                              :profile {:enable true :threshold 0}}})))

;; fnlfmt: skip
(use
  ;;;; Dependencies
  :wbthomason/packer.nvim {:opt true}
  :nvim-lua/popup.nvim {:as :popup :module_pattern "popup.*" :opt true}
  :nvim-lua/plenary.nvim {:as :plenary :module_pattern "plenary.*" :opt true}
  :nvim-telescope/telescope-fzf-native.nvim {:as :fzf-native :module_pattern "fzf*" :run "make" :opt false}
  :fzy {:rock true}
  :Olical/aniseed {}
  ;; :rktjmp/hotpot.nvim {}

  ;;;; UI plugins
  :babygau/tokyonight.nvim {:branch :tdt
                            :as :colorscheme
                            :event :VimEnter
                            :config (req :tokyonight)}
  :akinsho/nvim-bufferline.lua {:after :colorscheme :config (req :nvim-bufferline)}
  :lukas-reineke/indent-blankline.nvim {:after :colorscheme
                                        :config (req :indent-blankline)}
  :hoob3rt/lualine.nvim {:after :colorscheme
                         :config (req :lualine)
                         :requires [:kyazdani42/nvim-web-devicons]}
  :lewis6991/gitsigns.nvim {:cond false
                            :event [:BufRead :BufNewFile]
                            :config (req :gitsigns)}
  :folke/which-key.nvim {:event :BufRead
                         :config (req :which-key)}
  :kyazdani42/nvim-tree.lua {:event :BufEnter :config (req :nvim-tree)}
  ;;;; Editor plugins
  :akinsho/nvim-toggleterm.lua {:event :BufRead
                                :config (req :nvim-toggleterm)}
  :karb94/neoscroll.nvim {:event :WinScrolled
                          :config (req :neoscroll)}
  :b3nj5m1n/kommentary {:event :BufRead
                        :config (req :kommentary)}
  :junegunn/vim-easy-align {:event :BufRead
                            :config (req :vim-easy-align)}
  :rhysd/accelerated-jk {:event :BufRead
                         :config (req :accelerated-jk)}
  :machakann/vim-sandwich {:event :BufRead
                           :config (req :vim-sandwich)}
  :romainl/vim-qf {:ft :qf}
  :tpope/vim-eunuch {:event :BufRead}
  :tpope/vim-repeat {}
  :rktjmp/highlight-current-n.nvim {:event :BufRead}
  :norcalli/nvim-colorizer.lua {:ft [:html :css
                                     :sass :vim
                                     :typescript :typescriptreact]}
  :ahmedkhalf/project.nvim {:as :rooter :module_pattern "project*" :config (req :project_nvim)}
  ;;;; Git plugins
  :tpope/vim-fugitive {:event :BufRead :config (req :vim-fugitive)}
  :tpope/vim-rhubarb {:after :vim-fugitive :cmd ":Gbrowse"}
  :sindrets/diffview.nvim {:event :BufRead}
  ;;;; Lang plugins
  :nvim-treesitter/nvim-treesitter {:as :treesitter
                                    :run ":TSUpdate"
                                    :event :BufRead
                                    :config (req :nvim-treesitter)}
  :nvim-treesitter/nvim-treesitter-textobjects {:after :treesitter}
  :p00f/nvim-ts-rainbow {:after :treesitter}
  :windwp/nvim-autopairs {:after :treesitter
                          :config (req :nvim-autopairs)}
  :windwp/nvim-ts-autotag {:after :treesitter}
  :guns/vim-sexp {:ft [:clojure :fennel
                       :hy :lisp :scheme]
                  :config (req :vim-sexp)}
  :Olical/conjure {:config (req :conjure)
                  :ft [:clojure :fennel :hy]
                  :event ["BufNewFile,BufRead *.clj"
                          "BufNewFile,BufRead *.fnl"
                          "BufNewFile,BufRead *.hy"]}
  :mattn/emmet-vim {:config (req :emmet)
                    :ft [:css
                         :html
                         :javascript
                         :jsx
                         :markdown
                         :typescriptreact
                         :xml]}
  ;;;; Completion plugins
  :ms-jpq/coq_nvim {:branch :coq
                    :event :BufReadPre
                    :module_pattern "coq*"
                    :config (req :coq_nvim)
                    :run ":COQdeps"}
  :ms-jpq/coq.artifacts {:branch :artifacts}
  :neovim/nvim-lspconfig {:event :BufReadPre
                          :config (req :lsp)
                          :requires [:jose-elias-alvarez/null-ls.nvim
                                     :jose-elias-alvarez/nvim-lsp-ts-utils
                                     :folke/lua-dev.nvim]}
  :folke/trouble.nvim {:cmd :Trouble}
  :ray-x/lsp_signature.nvim {}
  :glepnir/lspsaga.nvim {:cmd :Lspsaga :disable true}
  :nvim-telescope/telescope.nvim {:requires [:plenary :fzf-native :rooter]
                                  :event :BufRead
                                  :module_pattern "telescope.*"
                                  :cmd :Telescope
                                  :config (req :telescope)}
  ;; :camspiers/snap {:event :BufRead :config (req :snap)}
  ;; :hrsh7th/nvim-cmp {:after :treesitter :config (req :nvim-cmp)}
  ;; :hrsh7th/nvim-compe {:after :treesitter :config (req :nvim-compe)}
  ;; :hrsh7th/vim-vsnip {:after :nvim-compe
  ;;                     :setup (fn [] (let! :vsnip_snippet_dir (.. tdt.paths.VIM_PATH :/snippets)))}
  ;; :L3MON4D3/LuaSnip {}
  ;; :tami5/compe-conjure {:after [:nvim-compe :conjure] :ft [:clojure :fennel :hy]}

  ;;;; Tool plugins
  :editorconfig/editorconfig-vim {:ft [:go :c :cpp :rust
                                       :typescript :javascript
                                       :vim :zig ]})
