(import-macros {: augroup : autocmd! : autocmd : noremap : lazyreq} :core.macros)

(local {: detach : openAllFolds : closeAllFolds &as ufo} (lazyreq :ufo))

(fn setup []
  (augroup disable-ufo-on-filetypes
           (autocmd!)
           (autocmd Filetype norg `(detach)))

  (noremap n :zR openAllFolds)
  (noremap n :zM closeAllFolds)

  (ufo.setup {:open_fold_hl_timeout 0
              :provider_selector (fn [bufnr filetype buftype]
                                   [:treesitter :indent])}))
{: setup}
