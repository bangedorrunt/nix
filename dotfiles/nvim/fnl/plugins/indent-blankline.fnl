(import-macros {: g} :core.macros)

(g indent_blankline_filetype_exclude [:help :packer
                                      :NvimTree :fern
                                      :Trouble :neogitstatus
                                      :fennel :lisp :clojure
                                      :markdown])
(g indent_blankline_use_treesitter true)
