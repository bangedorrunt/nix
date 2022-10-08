(import-macros {: use : after-loaded : setup!} :core.macros)

(fn setup []
  (use "~/workspace/rose-pine.nvim.git/main" :as :themer (after-loaded mod.ui.rose-pine))
  (use kyazdani42/nvim-web-devicons :module :nvim-web-devicons (after-loaded mod.ui.devicons))
  (use akinsho/bufferline.nvim :after :themer (after-loaded mod.ui.bufferline))
  (use nvim-lualine/lualine.nvim :after :themer (after-loaded mod.ui.lualine))
  (use kyazdani42/nvim-tree.lua :after :themer (after-loaded mod.ui.nvim-tree)))

{: setup}
