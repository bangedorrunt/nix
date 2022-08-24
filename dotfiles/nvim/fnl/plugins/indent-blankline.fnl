(local {: setup} (require :indent_blankline))

(setup {:filetype_exclude [:help :packer
                           :NvimTree :fern
                           :Trouble :neogitstatus
                           :fennel :lisp :clojure
                           :markdown]
       :buftype_exclude [:terminal]
       :use_treesitter true
       :show_current_context true
       :show_current_context_start true})
