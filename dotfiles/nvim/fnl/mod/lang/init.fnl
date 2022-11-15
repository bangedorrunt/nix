(import-macros {: use} :core.macros)

(fn setup []
  (use jaawerth/fennel.vim)
  (use folke/neodev.nvim :module :neodev)
  (use simrat39/rust-tools.nvim :module :rust-tools)
  (use nvim-treesitter/nvim-treesitter :as :treesitter :mod :lang.nvim-treesitter)
  (use p00f/nvim-ts-rainbow :after :treesitter)
  (use andymass/vim-matchup :after :treesitter)
  (use JoosepAlviste/nvim-ts-context-commentstring :as :context-comment :after :treesitter)
  (use AckslD/nvim-trevJ.lua :init :trevj)
  (use Olical/conjure :branch :develop :mod :lang.conjure)
  (use nvim-neorg/neorg-telescope :module :neorg)
  (use nvim-neorg/neorg :after [:treesitter :telescope.nvim] :mod :lang.neorg))

{: setup}
