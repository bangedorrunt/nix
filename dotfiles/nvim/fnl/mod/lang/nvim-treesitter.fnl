(import-macros {: setup! : set!} :core.macros)
(local treesitter-path (.. (vim.fn.stdpath :data) :/treesitter))
(local languages [:bash :fish
                  :comment
                  :clojure
                  :commonlisp
                  :fennel
                  :html :css
                  :javascript :typescript :tsx :svelte
                  :rust
                  :cpp
                  :toml :yaml :json :json5 :jsonc
                  :nix
                  :python
                  :norg :markdown :markdown_inline])
(fn setup []
  (set! runtimepath+ treesitter-path)
  (setup! nvim-treesitter.configs
    {:parser_install_dir treesitter-path
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
                             :config {:fish "# %s"
                                      :fennel ";; %s"
                                      :clojure ";; %s"}}}))
{: setup}
