(module plugins.nvim-autopairs
        {autoload {npairs nvim-autopairs
                   npairs/cmp nvim-autopairs.completion.cmp}
         require-macros [core.macros]})

(npairs.setup {:check_ts true
               :ts_config {:lua :string}
               :javascript :template_string
               :enable_check_bracket_line false
               :ignored_next_char "[%w%.]"
               :fast_wrap {}})

;; NOTE: `nvim-cmp` only
(npairs/cmp.setup {:map_cr true
                   :map_complete true
                   :auto_select true})

;; NOTE: `coq_nvim` only
;; https://github.com/ms-jpq/coq_nvim/issues/91#issuecomment-902292131
;; (set _G.on_enter (fn []
;;                    (if (not= (vim.fn.pumvisible) 0)
;;                        (if (not= (. (vim.fn.complete_info) :selected) (- 1))
;;                            (t :<C-y>)
;;                            (.. (t :<C-e>) (npairs.autopairs_cr)))
;;                        (npairs.autopairs_cr))))

;; (noremap! [is :expr :silent] :<CR> "v:lua.on_enter()")

