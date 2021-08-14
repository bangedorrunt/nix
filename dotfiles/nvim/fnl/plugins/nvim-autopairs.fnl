(module plugins.nvim-autopairs 
  {autoload {nvim-autopairs nvim-autopairs
             rule nvim-autopairs.rule
             ts-conds nvim-autopairs.ts-conds
             use-compe-with-autopairs nvim-autopairs.completion.compe}})

(nvim-autopairs.setup {:check_ts true
                       :ts_config {:lua :string}
                       :javascript :template_string
                       :enable_check_bracket_line false
                       :ignored_next_char "[%w%.]"
                       :fast_wrap {}})

(nvim-autopairs.add_rules {1 (: (rule "%" "%" :lua) :with_pair
                                (ts-conds.is_ts_node [:string :comment]))
                           2 (: (rule "$" "$" :lua) :with_pair
                                (ts-conds.is_not_ts_node :function))})

(use-compe-with-autopairs.setup {:map_cr true
                                 :map_complete true})
