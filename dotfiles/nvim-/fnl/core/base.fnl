(import-macros {: g} :core.macros)

(fn setup []
  (tset _G :store {:lazyadd []
                   :plugins []
                   :wks []
                   :paths
                   {:state (vim.fn.stdpath :state)
                    :data (vim.fn.stdpath :data)
                    :treesitter (.. (vim.fn.stdpath :data) "/site")
                    :luasnip (.. (vim.fn.stdpath :config) "/snippets")}
                   :luasnip
                   {:username :bangedorrunt
                    :email  "braden.truong@gmail.com"
                    :github :https://github.com/bangedorrunt
                    :real_name :Braden }})
  ;; Disable built-in plugins and host providers
  (g skip_ts_context_commentstring_module 1)
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
  (g loaded_ruby_provider 0))

{: setup}
