(module plugins.nvim-treesitter
  {autoload {treesitter nvim-treesitter.configs}})

(treesitter.setup
  {:ensure_installed [:bash :comment
                      :clojure :commonlisp :fennel
                      :html :css
                      :javascript :typescript :tsx :svelte
                      :toml :yaml :json :json5 :jsonc
                      :c :cpp
                      :lua :vim :nix :python]
  :highlight {:enable true
              :additional_vim_regex_highlighting false}
  :indent {:enable true}
  ; :autopairs {:enable false}
  ; :autotag {:enable true}
  :matchup {:enable true}
  :rainbow {:enable true
            :extended_mode true
            :max_file_lines nil}
  :context_commentstring {:enable true
                          :enable_autocmd false}})
