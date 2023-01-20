(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use jaawerth/fennel.vim :ft :fennel)
  (use folke/neodev.nvim)
  (use simrat39/rust-tools.nvim)
  (use nvim-neorg/neorg :ft :norg :mod :lang.neorg)
  (use nvim-orgmode/orgmode :ft :org :mod :lang.orgmode)
)

{: setup}
