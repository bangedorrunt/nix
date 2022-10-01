(import-macros {: lazyreq : lazymod : setup! : set!} :core.macros)

(local mod (lazyreq :mod))

(fn main []
  ;; Temporarily disable syntax and filetype to improve startup time
  (vim.cmd "syntax off")
  (vim.cmd "filetype off")
  (vim.cmd "filetype plugin indent off")
  (set! shadafile "NONE")

  (require :core.base)
  (require :core.options)
  (require :core.mappings)
  (require :core.autocmds)


  (setup! :mod)
  (setup! :core.packer mod.plugins))


(main)
