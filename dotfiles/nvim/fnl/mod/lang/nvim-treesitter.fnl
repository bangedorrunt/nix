(import-macros {: setup!} :core.macros)

(fn setup []
  (setup! nvim-treesitter.configs
    {:parser_install_dir store.paths.treesitter
     :ensure_installed store.treesitter.languages
     :highlight {:enable true}
     :indent {:enable true}
     :matchup {:enable true}
     :rainbow {:enable true :extended_mode true :max_file_lines nil}
     :incremental_selection {:enable true
                             :keymaps {:init_selection :<CR>
                                       :node_incremental :<CR>
                                       :node_decremental :<C-CR>}}
     :context_commentstring {:enable true
                             :enable_autocmd false
                             :config {:fish "# %s"
                                      :fennel ";; %s"
                                      :clojure ";; %s"}}}))
{: setup}
