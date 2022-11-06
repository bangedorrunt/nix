(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use jaawerth/fennel.vim)
  (use folke/neodev.nvim :module :neodev)
  (use simrat39/rust-tools.nvim :module :rust-tools)
  (use nvim-treesitter/nvim-treesitter :as :treesitter (after-load mod.lang.nvim-treesitter))
  (use p00f/nvim-ts-rainbow :after :treesitter)
  (use andymass/vim-matchup :after :treesitter)
  (use JoosepAlviste/nvim-ts-context-commentstring :as :context-comment :after :treesitter)
  (use AckslD/nvim-trevJ.lua (after-load trevj))
  (use Olical/conjure :branch :develop (after-load mod.lang.conjure))
  (use nvim-neorg/neorg-telescope :module :neorg)
  (use nvim-neorg/neorg :after [:treesitter :telescope.nvim] (after-load mod.lang.neorg)))

{: setup}
