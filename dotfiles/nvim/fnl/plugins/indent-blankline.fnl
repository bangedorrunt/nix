(module plugins.indent-blankline
  {require-macros [core.macros]})

(let! indent_blankline_char "│"
      indent_blankline_filetype_exclude [:help :packer
                                         :NvimTree :fern
                                         :Trouble :neogitstatus
                                         :fennel :lisp :clojure
                                         :markdown]
      indent_blankline_buftype_exclude [:terminal :nofile]
      indent_blankline_use_treesitter true
      indent_blankline_show_trailing_blankline_indent false
      indent_blankline_show_current_context true
      indent_blankline_context_patterns
      [:class :return :function :method "^if" "^while" :jsx_element
       "^for" "^object" "^table" :block :arguments :if_statement
       :else_clause :jsx_element :jsx_self_closing_element :try_statement
       :catch_clause :import_statement])

;; Because lazy load indent-blankline so need readd this autocmd
;; (autocmd! CursorMoved * :IndentBlanklineRefresh)
