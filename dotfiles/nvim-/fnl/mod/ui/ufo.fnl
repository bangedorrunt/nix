(import-macros {: augroup : autocmd! : autocmd : noremap : setup!} :core.macros)

(local {: detach : openAllFolds : closeAllFolds} (require :ufo))

(fn setup []
  (augroup disable-ufo-on-filetypes
    (autocmd!)
    (autocmd Filetype norg `(detach)))
  (noremap n :zR openAllFolds)
  (noremap n :zM closeAllFolds)
  (setup! ufo
    {:open_fold_hl_timeout 0
     :provider_selector (fn [bufnr filetype buftype] [:treesitter :indent])}))

{: setup}
