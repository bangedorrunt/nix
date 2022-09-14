(import-macros {: augroup : autocmd! : autocmd : noremap : lazyreq}
               :core.macros)

(local {: setup : detach : openAllFolds : closeAllFolds} (lazyreq :ufo))

;; fnlfmt: skip
(augroup disable-ufo-on-filetypes
         (autocmd!)
         (autocmd Filetype norg `(detach)))

(noremap n :zR openAllFolds)
(noremap n :zM closeAllFolds)

(setup {:open_fold_hl_timeout 0
        :provider_selector (fn [bufnr filetype buftype]
                             [:treesitter :indent])})
