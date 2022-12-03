(import-macros {: use : setup!} :core.macros)

(fn setup []
  (use jaawerth/fennel.vim :ft :fennel)
  (use folke/neodev.nvim :event "User LspLoaded")
  (use simrat39/rust-tools.nvim :event "User LspLoaded")
  (use Olical/conjure :start true :init+ :lang.conjure)
  (use nvim-neorg/neorg-telescope :event "User NeorgLoaded")
  (use nvim-neorg/neorg :ft :norg :init+ :lang.neorg))

{: setup}
