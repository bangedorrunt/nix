(module plugins.nvim-treesitter
  {autoload {{: setup} nvim-treesitter.configs
             parser nvim-treesitter.parsers}})

(let [parser_config (parser.get_parser_configs)]
  (set parser_config.norg_meta
       {:install_info
        {:url "https://github.com/nvim-neorg/tree-sitter-norg-meta"
         :files [:src/parser.c]
         :branch :main}})
  (set parser_config.norg_table
       {:install_info
        {:url "https://github.com/nvim-neorg/tree-sitter-norg-table"
         :files [:src/parser.c]
         :branch :main}}))

(def- languages
  [:bash :comment
   :clojure :commonlisp :fennel
   :html :css
   :javascript :typescript :tsx :svelte
   :rust :c :cpp
   :toml :yaml :json :json5 :jsonc
   :lua :vim :nix :python
   :markdown :norg :norg_meta :norg_table])

(setup {:ensure_installed languages
        :highlight {:enable true
                    :use_languagetree true}
        :indent {:enable true}
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
                                :config {:fennel ";; %s"}}})
