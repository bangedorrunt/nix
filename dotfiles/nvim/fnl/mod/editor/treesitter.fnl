(import-macros {: setup!} :core.macros)
(local languages
  [
   :bash :clojure :commonlisp :cpp :diff :fennel :fish
   :help :html :css :javascript :json :json5 :jsonc :lua
   :markdown :markdown_inline :nix :org :python :rust
   :toml :tsx :typescript :vim :yaml :norg
   ])
(fn setup []
  (setup! nvim-treesitter.configs
    {
     ;; :parser_install_dir store.paths.treesitter
     :ensure_installed languages
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
                             :config {:fennel ";; %s"}}
     }))
{: setup}
