(module plugins.nvim-treesitter
  {autoload {treesitter nvim-treesitter.configs}})

(treesitter.setup
  {:ensure_installed [:bash
                      :clojure :commonlisp :fennel
                      :html :css
                      :javascript :typescript :tsx :svelte
                      :toml :yaml :json :json5 :jsonc
                      :c :cpp
                      :lua :vim :nix :python]
  :highlight {:enable true
              :additional_vim_regex_highlighting {:fennel false}}
  :indent {:enable true}
  :autopairs {:enable false}
  :autotag {:enable true}
  :matchup {:enable true}
  :rainbow {:enable true
            :extended_mode true
            :max_file_lines nil}
  :context_commentstring {:enable true
                          :config {:fennel ";; %s"}}
  ;; :incremental_selection {:enable true
  ;;                         :keymaps {:init_selection :<C-n>
  ;;                                   :node_incremental :<C-n>
  ;;                                   :scope_incremental :<C-s>
  ;;                                   :node_decremental :<C-r>}}
  ;; :textsubjects {:enable true
  ;;                :keymaps {:. :textsubjects-smart
  ;;                          ";" :textsubjects-container-outer}}
  ;; :textobjects {:select {:enable true
  ;;                        :keymaps {:oc "@class.outer"
  ;;                                  :ic "@class.inner"
  ;;                                  :of "@function.outer"
  ;;                                  :if "@function.inner"
  ;;                                  :ob "@block.outer"
  ;;                                  :ib "@block.inner"
  ;;                                  :ol "@loop.outer"
  ;;                                  :il "@loop.inner"
  ;;                                  :os "@statement.outer"
  ;;                                  :is "@statement.inner"
  ;;                                  :oC "@comment.outer"
  ;;                                  :iC "@comment.inner"
  ;;                                  :om "@call.outer"
  ;;                                  :im "@call.inner"}}
  ;;               :lookahead true
  ;;               :move {:enable true
  ;;                      :set_jumps true
  ;;                      :goto_next_start {"]m" "@function.outer"
  ;;                                        "]]" "@class.outer"}
  ;;                      :goto_next_end {"]M" "@function.outer"
  ;;                                      "][" "@class.outer"}
  ;;                      :goto_previous_start {"[m" "@function.outer"
  ;;                                            "[[" "@class.outer"}
  ;;                      :goto_previous_end {"[M" "@function.outer"
  ;;                                          "[]" "@class.outer}"}
  ;;                      :lsp_interop {:enable true
  ;;                                    :peek_definition_code {:gD "@function.outer"}}}}
  })
