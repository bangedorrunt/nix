(import-macros {: lazyreq} :core.macros)

(let [{: setup} (lazyreq :ufo)]
  (setup
    {:provider_selector (fn [bufnr filetype buftype]
                          [:treesitter :indent])}))
