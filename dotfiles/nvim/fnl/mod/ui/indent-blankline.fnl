(import-macros {: lazyreq} :core.macros)

(local indent-blankline (lazyreq :indent_blankline))

(fn setup []
  (indent-blankline.setup {:filetype_exclude [:help
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
                           :show_current_context_start false}))

{: setup}
