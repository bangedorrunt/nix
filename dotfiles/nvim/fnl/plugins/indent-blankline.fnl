(import-macros {: lazyreq} :core.macros)

(local {: setup} (lazyreq :indent_blankline))

(setup {:filetype_exclude [:help
                           :packer
                           :NvimTree
                           :fern
                           :Trouble
                           :neogitstatus
                           :fennel
                           :lisp
                           :clojure
                           :markdown
                           :norg]
        :buftype_exclude [:terminal]
        :use_treesitter true
        :show_current_context false
        :show_current_context_start false})
