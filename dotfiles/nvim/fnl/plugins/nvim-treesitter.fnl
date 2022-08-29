(let [{: setup} (require :nvim-treesitter.configs)
      languages [:bash :comment
                 :clojure :commonlisp :fennel
                 :html :css
                 :javascript :typescript :tsx :svelte
                 :rust :c :cpp
                 :toml :yaml :json :json5 :jsonc
                 :lua :vim :nix :python
                 :markdown :markdown_inline :norg]]
  (setup {:ensure_installed languages
          :highlight {:enable true
                      :use_languagetree true}
          ;; :indent {:enable true}
          :matchup {:enable true}
          :rainbow {:enable true
                    :extended_mode true
                    :max_file_lines nil}
          :incremental_selection {:enable true
                                  :keymaps {:init_selection :gnn
                                            :node_incremental :grn
                                            :scope_incremental :grc
                                            :node_decremental :grm}}
          :textobjects {:select {:enable true}
                        :lookahead true
                        :keymaps {:af "@function.outer"
                                  :if "@function.inner"
                                  :ac "@class.outer"
                                  :ic "@class.inner"}
                        :move {:enable true
                              :set_jumps true
                              :goto_next_start {"]m" "@function.outer"
                                                "]]" "@class.outer"}
                              :goto_next_end {"]M" "@function.outer"
                                              "][" "@class.outer"}
                              :goto_previous_start {"[m" "@function.outer"
                                                    "[[" "@class.outer"}
                              :goto_previous_end {"[M" "@function.outer"
                                                  "[]" "@class.outer"}}}
          :context_commentstring {:enable true
                                  :enable_autocmd false
                                  :config {:fennel ";; %s"
                                  :clojure ";; %s"}}}))
