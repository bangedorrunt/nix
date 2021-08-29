(module plugins.nvim-treesitter
  {autoload {treesitter nvim-treesitter.configs}
   require-macros [core.macros]})

(set! foldmethod :expr)
(set! foldexpr "nvim_treesitter#foldexpr()")

;; SEE: https://github.com/folke/dot/blob/master/config/nvim/lua/config/treesitter.lua
(treesitter.setup
 {;; NOTE: if neovim is unresponsive and slow
  ;; don't use `comment` lang
  ;; This cause startup delay, use
  ;;`TSInstall maintained` instead
  ;; ensure_installed = "maintained",
  :autopairs {:enable true}
  :autotag {:enable true}
  :highlight {:enable true
              ;; :use_languagetree true
              :additional_vim_regex_highlighting {:fennel false}}
  :indent {:enable true}
  :incremental_selection {:enable true
                          :keymaps {:init_selection :<C-n>
                                    :node_incremental :<C-n>
                                    :scope_incremental :<C-s>
                                    :node_decremental :<C-r>}}
  ;; :query_linter {:enable true
  ;;                :use_virtual_text true
  ;;                :lint_events {1 :BufWrite
  ;;                              2 :CursorHold}}
  :textobjects {:select {:enable true
                         :keymaps {:af "@function.outer"
                                   :if "@function.inner"
                                   :ac "@class.outer"
                                   :ic "@class.inner"}}
                :move {:enable true
                       :set_jumps true
                       :goto_next_start {"]m" "@function.outer"
                                         "]]" "@class.outer"}
                       :goto_next_end {"]M" "@function.outer"
                                       "][" "@class.outer"}
                       :goto_previous_start {"[m" "@function.outer"
                                             "[[" "@class.outer"}
                       :goto_previous_end {"[M" "@function.outer"
                                           "[]" "@class.outer}"}
                       :lsp_interop {:enable true
                                     :peek_definition_code {:gD "@function.outer"}}}}
  :rainbow {:enable true
            :extended_mode true
            :max_file_lines 1000}
  :context_commentstring {:enable true}})
