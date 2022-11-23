(import-macros {: use} :core.macros)

(fn setup []
  (use jaawerth/fennel.vim :ft :fennel)
  (use folke/neodev.nvim :module :neodev)
  (use simrat39/rust-tools.nvim :module :rust-tools)
  (use Olical/conjure :event "User PackerDefered" :module :conjure :init+ :lang.conjure)
  (use nvim-neorg/neorg-telescope :module :neorg.modules.core.integrations.telescope.module)
  (use nvim-neorg/neorg :after [:treesitter :telescope] :ft :norg :init+ :lang.neorg))

{: setup}
