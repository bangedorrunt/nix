(import-macros {: lazyreq : lazymod : setup! : set!} :core.macros)

(local mod (lazyreq :mod))

(fn main []

  (require :core.base)
  (require :core.options)
  (require :core.mappings)
  (require :core.autocmds)

  (setup! :mod)
  (setup! :core.packer mod.plugins))

(main)
