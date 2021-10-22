(module core.base
   {require-macros [core.macros]})

(defn vim-cmd [prop]
  (fn [...]
    (let [args (accumulate [acc "" _ s (ipairs [...])] (.. acc " " s))]
      (vim.cmd (.. prop args)))))

(tset _G :nvim {:ex {:augroup (vim-cmd :augroup)
                     :autocmd (vim-cmd :autocmd)
                     :autocmd_ (vim-cmd :autocmd!)
                     :command_ (vim-cmd :command!)
                     :highlight (vim-cmd :highlight)
                     :colorscheme (vim-cmd :colorscheme)}
                :fn vim.fn
                :g vim.g
                :opt vim.opt
                :opt_local vim.opt_local
                :replace_termcodes vim.api.nvim_replace_termcodes
                :feedkeys vim.api.nvim_feedkeys
                :buf_set_keymap vim.api.nvim_buf_set_keymap
                :set_keymap vim.api.nvim_set_keymap})

(def os-name (. (vim.loop.os_uname) :sysname))
; fnlfmt: skip
(def path-sep (match os-name
                 :Windows :\\
                 _ :/))
(def data-path (string.format "%s/site/" (vim.fn.stdpath :data)))
(def config-path (vim.fn.stdpath :config))
(def cache-path (vim.fn.stdpath :cache))

(tset _G :tdt
        {:signs {:error " "
                 :warning " "
                 :hint " "
                 :information " "
                 :prompt "❯"}
         :pallete {:dark {:tokyonight "#1a1b26"
                          :monokaipro_spectrum "#222222"
                          :rose-pine "#191724"}
                   :light {:tokyonight "#e1e2e7"
                           :gruvbox "#fbf1c7"
                           :rose-pine "#faf4ed"}}
         :paths {:IS_MAC (= os-name :Darwin)
                 :IS_LINUX (= os-name :Linux)
                 :IS_WINDOWS (= os-name :Windows)
                 :PATH_SEP path-sep
                 :NVIM_PATH config-path
                 :HOME (os.getenv :HOME)
                 :CACHE_PATH cache-path
                 :DATA_PATH data-path
                 :PACKER_PATH (.. data-path :pack/packer/opt/packer.nvim)
                 :PACKER_COMPILED_PATH (.. data-path :lua/packer_compiled.lua)}})

; Disable built-in plugins and host providers
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

