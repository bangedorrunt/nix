(module plugins.init {autoload {core aniseed.core
                                nvim aniseed.nvim
                                packer packer}
                      require-macros [core.macros]})

;; HACK: see https://github.com/wbthomason/packer.nvim/issues/180
;; Only use it once, disable next time
;; (vim.fn.setenv :MACOSX_DEPLOYMENT_TARGET :10.15)

(defn- safe-require-plugin-config [name]
       "Safely require a module under the plugins.* prefix. Will catch errors
  and print them while continuing execution, allowing other plugins to load
  even if one configuration module is broken."
       (let [(ok? val-or-err) (pcall require (.. :plugins. name))]
         (when (not ok?)
           (print (.. "Plugin config error: " val-or-err)))))

(defn- req [name] "A shortcut to building a require string for your plugin
  configuration. Intended for use with packer's config or setup
  configuration options. Will prefix the name with `plugins.`
  before requiring." (.. "require('plugins." name "')"))

;; Curtesy of Olical with rocks changes
(defn use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup {1 (fn [use use-rocks]
                         (for [i 1 (core.count pkgs) 2]
                           (let [name (. pkgs i)
                                 opts (. pkgs (+ i 1))]
                             (-?> (. opts :mod) (safe-require-plugin-config))
                             (if (. opts :rock)
                                 (use-rocks name)
                                 (use (core.assoc opts 1 name))))))
                     :config {:compile_path tdt.paths.PACKER_COMPILED_PATH
                              :git {:clone_timeout 120}
                              :profile {:enable true :threshold 0}}})))

;; fnlfmt: skip
(use
  ;; Dependencies
  :wbthomason/packer.nvim {:opt true}
  :nvim-lua/popup.nvim {:as :popup :module_pattern "popup.*"}
  :nvim-lua/plenary.nvim {:as :plenary :module_pattern "plenary.*"}
  :nvim-telescope/telescope-fzf-native.nvim {:as :fzf-native :run "make"}
  :fzy {:rock true}
  :Olical/aniseed {}
  ;; UI plugins
  :babygau/tokyonight.nvim {:branch :tdt
                            :as :theme
                            :event :VimEnter
                            :config (req :tokyonight)}
  :akinsho/nvim-bufferline.lua {:after :theme :config (req :nvim-bufferline)}
  :lukas-reineke/indent-blankline.nvim {:after :theme
                                        :config (req :indent-blankline)}
  :hoob3rt/lualine.nvim {:after :theme
                         :config (req :lualine)
                         :requires :kyazdani42/nvim-web-devicons}
  :lewis6991/gitsigns.nvim {:cond false
                            :event [:BufRead :BufNewFile]
                            :config (req :gitsigns)}
  :folke/which-key.nvim {:event :BufRead
                         :config (req :which-key)}
  :kyazdani42/nvim-tree.lua {:event :BufEnter :config (req :nvim-tree)}
  ;; Editor plugins
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
  :tpope/vim-fugitive {:event :BufRead :config (req :vim-fugitive)}
  :tpope/vim-rhubarb {:after :vim-fugitive :cmd ":Gbrowse"}
  :sindrets/diffview.nvim {:event :BufRead}
  :tpope/vim-repeat {}
  :norcalli/nvim-colorizer.lua {:ft [:html :css
                                     :sass :vim
                                     :typescript :typescriptreact]}
  ;; Completion plugins
  :neovim/nvim-lspconfig {:event :BufReadPre
                          :config (req :lsp)
                          :requires [[:jose-elias-alvarez/null-ls.nvim]
                                     [:jose-elias-alvarez/nvim-lsp-ts-utils]
                                     [:folke/lua-dev.nvim]]}
  :folke/trouble.nvim {:cmd :Trouble}
  :ray-x/lsp_signature.nvim {}
  :glepnir/lspsaga.nvim {:cmd :Lspsaga :disable true}
  ;; :camspiers/snap {:event :BufRead :config (req :snap)}
  :nvim-telescope/telescope.nvim {:requires [:popup :plenary :fzf-native]
                                  :event :BufRead
                                  :module_pattern "telescope.*"
                                  :cmd :Telescope
                                  :config (req :telescope)}
  :hrsh7th/nvim-compe {:after :treesitter :config (req :nvim-compe)}
  :hrsh7th/vim-vsnip {:after :nvim-compe
                      :setup (fn []
                              (let! :vsnip_snippet_dir (.. tdt.paths.VIM_PATH :/snippets)))}
  :tami5/compe-conjure {:after [:nvim-compe :conjure]
                        :ft [:clojure :fennel :hy]}
  ;; Lang plugins
  :nvim-treesitter/nvim-treesitter {:as :treesitter
                                    :run ":TSUpdate"
                                    :event :BufRead
                                    :config (req :nvim-treesitter)}
  :nvim-treesitter/nvim-treesitter-textobjects {:after :treesitter}
  :p00f/nvim-ts-rainbow {:after :treesitter}
  :windwp/nvim-autopairs {:after :nvim-compe
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
  ;; Tool plugins
  :editorconfig/editorconfig-vim {:ft [:go :c :cpp :rust
                                       :typescript :javascript
                                       :vim :zig ]})
