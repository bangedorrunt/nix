(import-macros {: use : after-loaded : setup!} :core.macros)

(fn setup []
  (use jaawerth/fennel.vim)
  (use folke/lua-dev.nvim :module :lua-dev)
  (use simrat39/rust-tools.nvim :module :rust-tools)
  (use nvim-treesitter/nvim-treesitter :as :treesitter :run :TSUpdate (after-loaded mod.lang.nvim-treesitter))
  (use nvim-treesitter/nvim-treesitter-textobjects :after :treesitter)
  (use p00f/nvim-ts-rainbow :after :treesitter)
  (use andymass/vim-matchup :after :treesitter)
  (use JoosepAlviste/nvim-ts-context-commentstring :as :context-comment :after :treesitter)
  (use numToStr/Comment.nvim :after [:treesitter :context-comment] (after-loaded mod.lang.comment))
  (use AckslD/nvim-trevJ.lua (after-loaded trevj))
  (use Olical/conjure :branch :develop (after-loaded mod.lang.conjure))
  (use nvim-neorg/neorg-telescope)
  (use nvim-neorg/neorg :after :treesitter (after-loaded mod.lang.neorg)))

{: setup}
