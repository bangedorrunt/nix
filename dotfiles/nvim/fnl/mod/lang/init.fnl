(import-macros {: lazyreq : after! : before! : setup!} :core.macros)

(local plugins
  [[:jaawerth/fennel.vim]
   [:folke/lua-dev.nvim :module :lua-dev]
   [:simrat39/rust-tools.nvim :module :rust-tools]
   [:nvim-treesitter/nvim-treesitter :as :treesitter :run :TSUpdate]
   [:nvim-treesitter/playground :after :treesitter]
   [:nvim-treesitter/nvim-treesitter-textobjects :after :treesitter]
   [:p00f/nvim-ts-rainbow :after :treesitter]
   [:andymass/vim-matchup :after :treesitter]
   [:JoosepAlviste/nvim-ts-context-commentstring :as :context-comment :after :treesitter]
   [:numToStr/Comment.nvim :after [:treesitter :context-comment]]
   [:AckslD/nvim-trevJ.lua :after :treesitter]
   [:Olical/conjure :branch :develop]
   [:nvim-neorg/neorg-telescope]
   [:nvim-neorg/neorg]])

(fn setup []
  (after! :treesitter (setup! :mod.lang.nvim-treesitter))
  (after! :Comment.nvim (setup! :mod.lang.comment))
  (after! :nvim-trevJ.lua (setup! :trevj))
  (after! :conjure (setup! :mod.lang.conjure))
  (after! :neorg (setup! :mod.lang.neorg)))

{: plugins
 : setup}
