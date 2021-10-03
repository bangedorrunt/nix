(module plugins.nvim-autopairs
        {autoload {npairs nvim-autopairs
                   npairs/cmp nvim-autopairs.completion.cmp}})

(npairs.setup {:check_ts true
               :ts_config {:lua :string}
               :javascript :template_string
               :enable_check_bracket_line false
               :ignored_next_char "[%w%.]"
               :fast_wrap {}
               :disable_filetype [:TelescopePrompt :vim]})

(npairs/cmp.setup {:map_cr true
                   :map_complete true
                   :insert true
                   :auto_select false})


