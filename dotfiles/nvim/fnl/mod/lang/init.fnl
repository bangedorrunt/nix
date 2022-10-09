(import-macros {: use : after-load : setup!} :core.macros)

(fn setup []
  (use jaawerth/fennel.vim)
  (use folke/lua-dev.nvim :module :lua-dev)
  (use simrat39/rust-tools.nvim :module :rust-tools)
  (use nvim-treesitter/nvim-treesitter :as :treesitter :run :TSUpdate (after-load mod.lang.nvim-treesitter))
  (use nvim-treesitter/nvim-treesitter-textobjects :after :treesitter)
  (use p00f/nvim-ts-rainbow :after :treesitter)
  (use andymass/vim-matchup :after :treesitter)
  (use JoosepAlviste/nvim-ts-context-commentstring :as :context-comment :after :treesitter)
  (use numToStr/Comment.nvim :after [:treesitter :context-comment] (after-load mod.lang.comment))
  (use AckslD/nvim-trevJ.lua (after-load trevj))
  (use Olical/conjure :branch :develop (after-load mod.lang.conjure))
  (use nvim-neorg/neorg-telescope)
  (use nvim-neorg/neorg :after :treesitter (after-load mod.lang.neorg)))

{: setup}
