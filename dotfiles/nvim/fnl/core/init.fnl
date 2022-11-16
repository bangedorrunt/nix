(import-macros {: setup!} :core.macros)

(fn main []
  (setup! core.base)
  (setup! core.options)
  (setup! core.keymaps)
  (setup! core.autocmds)
  (setup! core.packer))

(main)
