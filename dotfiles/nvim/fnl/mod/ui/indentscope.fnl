(import-macros {: setup! : augroup : autocmd! : autocmd : b} :core.macros)

(fn setup []
  (setup! mini.indentscope {:symbol "│"})
  (augroup mini-indentscope-exclude-filetypes
    (autocmd!)
    (autocmd Filetype [help packer NvimTree norg fennel clojure]
             `(b miniindentscope_disable true))))

{: setup}
