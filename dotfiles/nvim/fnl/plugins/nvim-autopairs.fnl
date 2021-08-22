(module plugins.nvim-autopairs
        {autoload {npairs nvim-autopairs
                   rule nvim-autopairs.rule
                   ts-conds nvim-autopairs.ts-conds}})

(import-macros {: t : nmap! : noremap!} :core.macros)

(npairs.setup {:check_ts true
               :ts_config {:lua :string}
               :javascript :template_string
               :enable_check_bracket_line false
               :ignored_next_char "[%w%.]"
               :fast_wrap {}})

;; Add { | } spacing rule
(npairs.add_rules [(: (rule " " " ") :with_pair
                      (fn [opts]
                        (let [pair (opts.line:sub (- opts.col 1) opts.col)]
                          (vim.tbl_contains ["()" "[]" "{}"] pair))))
                   (: (: (: (rule "( " " )") :with_pair
                            (fn []
                              false)) :with_move
                         (fn [opts]
                           (not= (opts.prev_char:match ".%)") nil)))
                      :use_key ")")
                   (: (: (: (rule "{ " " }") :with_pair
                            (fn []
                              false)) :with_move
                         (fn [opts]
                           (not= (opts.prev_char:match ".%}") nil)))
                      :use_key "}")
                   (: (: (: (rule "[ " " ]") :with_pair
                            (fn []
                              false)) :with_move
                         (fn [opts]
                           (not= (opts.prev_char:match ".%]") nil)))
                      :use_key "]")])

;; See: https://github.com/ms-jpq/coq_nvim/issues/91#issuecomment-902292131
(set _G.on_enter (fn []
                   (if (not= (vim.fn.pumvisible) 0)
                       (if (not= (. (vim.fn.complete_info) :selected) (- 1))
                           (t :<C-y>)
                           (.. (t :<C-e>) (npairs.autopairs_cr)))
                       (npairs.autopairs_cr))))

(noremap! [is :expr :silent] :<CR> "v:lua.on_enter()")
