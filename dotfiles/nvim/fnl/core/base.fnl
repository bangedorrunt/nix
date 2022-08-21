(module core.base
  {require-macros [core.macros]})

(def os_name (. (vim.loop.os_uname) :sysname))

;; fnlfmt: skip
(def path_sep (match os_name
                :Windows "\\\\"
                _ "/"))

(def data_path (string.format "%s/site/" (vim.fn.stdpath :data)))
(def config_path (vim.fn.stdpath :config))
(def cache_path (vim.fn.stdpath :cache))
(def state_path (vim.fn.stdpath :state))

(tset _G :tdt
      {:signs {:error " "
               :warning " "
               :hint " "
               :information " "
               :prompt "❯"}
       :pallete {:dark {:tokyonight :#1a1b26
                        :monokaipro_spectrum :#222222
                        :rose-pine :#191724}
                 :light {:tokyonight :#e1e2e7
                         :gruvbox :#fbf1c7
                         :rose-pine :#faf4ed}}
       :paths {:IS_MAC (= os_name :Darwin)
               :IS_LINUX (= os_name :Linux)
               :IS_WINDOWS (= os_name :Windows)
               :PATH_SEP path_sep
               :NVIM_PATH config_path
               :HOME (os.getenv :HOME)
               :CACHE_PATH cache_path
               :DATA_PATH data_path
               :STATE_PATH state_path
               :PACKER_PATH (.. data_path :pack/packer/opt/packer.nvim)
               :PACKER_COMPILED_PATH (.. data_path :lua/packer_compiled.lua)}})

;; Disable built-in plugins and host providers
;; (g loaded_netrw 1)
;; (g loaded_netrwPlugin 1)
;; (g loaded_netrwSettings 1)
;; (g loaded_netrwFileHandlers 1)
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
