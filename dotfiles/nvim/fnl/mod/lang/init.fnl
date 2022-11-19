(import-macros {: use} :core.macros)

(fn setup []
  (use jaawerth/fennel.vim :opt false)
  (use folke/neodev.nvim :module :neodev)
  (use simrat39/rust-tools.nvim :module :rust-tools)
  (use nvim-treesitter/nvim-treesitter :as :treesitter :event "User PackerDefered" :module :nvim-treesitter :init+ :lang.nvim-treesitter)
  (use p00f/nvim-ts-rainbow :after :treesitter)
  (use andymass/vim-matchup :after :treesitter)
  (use JoosepAlviste/nvim-ts-context-commentstring :module :ts_context_commentstring)
  (use AckslD/nvim-trevJ.lua :after :treesitter :init :trevj)
  (use Olical/conjure :event "User PackerDefered" :module :conjure :init+ :lang.conjure)
  (use nvim-neorg/neorg-telescope :module :neorg.modules.core.integrations.telescope.module)
  (use nvim-neorg/neorg :after [:treesitter :telescope.nvim] :init+ :lang.neorg))

{: setup}
