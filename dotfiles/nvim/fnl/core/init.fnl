(import-macros {: lazyreq : setup! : set!} :core.macros)

(fn main []
  (require :core.base)
  (require :core.options)
  (require :core.mappings)
  (require :core.autocmds)
  (setup! mod)
  (setup! core.packer))

(main)
