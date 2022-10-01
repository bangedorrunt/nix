(import-macros {: lazyreq : after! : setup!} :core.macros)

(local plugins
  [["~/workspace/rose-pine.nvim.git/main" :as :themer]
   [:kyazdani42/nvim-web-devicons :module :nvim-web-devicons]
   [:akinsho/bufferline.nvim :after :themer]
   [:nvim-lualine/lualine.nvim :after :themer]
   [:kyazdani42/nvim-tree.lua :after :themer]])

(fn setup []
  (after! :themer (setup! :mod.ui.rose-pine))
  (after! :nvim-web-devicons (setup! :mod.ui.devicons))
  (after! :bufferline.nvim (setup! :mod.ui.bufferline))
  (after! :lualine.nvim (setup! :mod.ui.lualine))
  (after! :nvim-tree.lua (setup! :mod.ui.nvim-tree)))

{: plugins
 : setup}
