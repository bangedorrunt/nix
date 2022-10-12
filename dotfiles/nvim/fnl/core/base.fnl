(import-macros {: g : set!} :core.macros)

(let [os-name (. (vim.loop.os_uname) :sysname)
      path-sep (match os-name
                 :Windows "\\\\"
                 _ "/")
      share-path (vim.fn.stdpath :data)
      data-path (string.format "%s/site/" share-path)
      config-path (vim.fn.stdpath :config)
      cache-path (vim.fn.stdpath :cache)
      state-path (vim.fn.stdpath :state)]
  (tset _G :store
        {:ft {:conjure [:clojure :fennel :lisp :lua :rust]}
         :signs {:error " "
                 :warning " "
                 :hint " "
                 :information " "
                 :prompt "❯ "}
         :pallete {:moon {:base "#191724"
                          :surface "#1f1d2e"
                          :overlay "#26233a"
                          :subtle "#c0caf5"
                          :text "#ffffff"
                          :love "#ff5555"
                          :gold "#f1fa8c"
                          :rose "#ff79c6"
                          :pine "#50fa7b"
                          :foam "#8be9fd"
                          :iris "#bd93f9"}
                   :dark {:tokyonight "#1a1b26"
                          :monokaipro-spectrum "#222222"
                          :rose-pine "#191724"}
                   :light {:tokyonight "#e1e2e7"
                           :gruvbox "#fbf1c7"
                           :rose-pine "#faf4ed"}}
         :plugins []
         :lsp {:servers [:bashls
                         :clojure_lsp
                         :cssls
                         :diagnosticls
                         :dockerls
                         :emmet_ls
                         :eslint
                         :html
                         :jsonls
                         :marksman
                         :rust_analyzer
                         :sumneko_lua
                         :tailwindcss
                         :tsserver
                         :vimls
                         :yamlls]
               :icons {:Array " "
                       :Boolean " "
                       :Class " "
                       :Color " "
                       :Constant " "
                       :Constructor " "
                       :Enum " "
                       :EnumMember " "
                       :Event " "
                       :Field " "
                       :File " "
                       :Folder " "
                       :Function " "
                       :Interface " "
                       :Keyword " "
                       :Method " "
                       :Module " "
                       :Namespace " "
                       :Null "ﳠ "
                       :Number " "
                       :Object " "
                       :Operator " "
                       :Package " "
                       :Property " "
                       :Reference " "
                       :Snippet " "
                       :Struct " "
                       :String " "
                       :Text " "
                       :TypeParameter " "
                       :Unit " "
                       :Value " "
                       :Variable " "}}
         :border ["┌" "─" "┐" "│" "┘" "─" "└" "│"]
         :border-alt ["─" "│" "─" "│" "┌" "┐" "┘" "└"]
         :paths {:IS-MAC (= os-name :Darwin)
                 :IS-LINUX (= os-name :Linux)
                 :IS-WINDOWS (= os-name :Windows)
                 :PATH-SEP path-sep
                 :NVIM-PATH config-path
                 :HOME (os.getenv :HOME)
                 :CACHE-PATH cache-path
                 :DATA-PATH data-path
                 :STATE-PATH state-path
                 :TREESITTER-PATH (.. share-path :/treesitter)
                 :PACKER-PATH (.. data-path :pack/packer/opt/packer.nvim)
                 :PACKER-COMPILED-PATH (.. data-path :lua/packer_compiled.lua)}}))

(set! runtimepath+ store.paths.TREESITTER-PATH)

;; Disable built-in plugins and host providers
(g loaded_netrw 1)
(g loaded_netrwPlugin 1)
(g loaded_netrwSettings 1)
(g loaded_netrwFileHandlers 1)
(g loaded_gzip 1)
(g loaded_zip 1)
(g loaded_zipPlugin 1)
(g loaded_tar 1)
(g loaded_tarPlugin 1)
(g loaded_getscript 1)
(g loaded_getscriptPlugin 1)
(g loaded_vimball 1)
(g loaded_vimballPlugin 1)
(g loaded_2html_plugin 1)
(g loaded_logipat 1)
(g loaded_rrhelper 1)
(g loaded_spellfile_plugin 1)
(g loaded_matchit 1)

(g loaded_perl_provider 0)
(g loaded_python_provider 0)
(g loaded_python3_provider 0)
(g loaded_node_provider 0)
(g loaded_ruby_provider 0)
