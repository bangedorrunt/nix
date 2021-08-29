(module plugins.vim-clap
  {autoload {: cljlib
             : clap}
   require-macros [core.macros]})

(let! g/clap_theme "tokyonight"
      g/clap_layout {:width "80%" :row "8%" :col "10%" :height "34%"}
      g/clap_preview_size 10
      g/clap_preview_direction "UD"
      g/clap_selected_sign {:text " ❯" :texthl "ClapSelectedSign" :linehl "ClapSelected"}
      g/clap_current_selection_sign {:text " ❯"
                                     :texthl "ClapCurrentSelectionSign"
                                     :linehl "ClapCurrentSelection"}
      ;; g/clap_provider_nix {
      ;; :source "fd --type f --hidden --follow --exclude .git --full-path ~/nix"
      ;; :sink "e"
      ;; :description "Nix Setup"
      ;; }
      ;; g/clap_provider_nvim {
      ;; :source "fd --type f --hidden --follow --full-path ~/nix/dotfiles/nvim"
      ;; :sink "e"
      ;; :description "Neovim Setup"
      ;; }
      ;; g/clap_provider_fennel {
      ;; :source "fd --type f --hidden --follow --exclude .git --full-path ~/workspace/fennel"
      ;; :sink "e"
      ;; :description "Fennel References"}
      )

;; `Clap!` -> `Clap ... +async`
;; `Clap! +ef=fzf` -> `Clap ... ++ef=fzf +asyn
;; TODO: doesn't open paths as expected
;; (noremap! [n :nowait] :<Leader>0 "<Cmd>Clap nvim<CR>")
;; (noremap! [n :nowait] :<Leader>9 "<Cmd>Clap nix<CR>")
;; (noremap! [n :nowait] :<Leader>8 "<Cmd>Clap fennel<CR>")

(noremap! [n :nowait] :<Leader><Leader> "<Cmd>Clap! files<CR>")
(noremap! [n :nowait] "<Leader>;" "<Cmd>Clap! grep2<CR>")
(noremap! [n :nowait] :<Leader>sg "<Cmd>Clap! gfiles<CR>")
(noremap! [n :nowait] :<Leader>sb "<Cmd>Clap! buffers<CR>")
(noremap! [n :nowait] :<Leader>so "<Cmd>Clap! history<CR>")
(noremap! [n :nowait] :<Leader>sc "<Cmd>Clap! command<CR>")
(noremap! [n :nowait] :<Leader>sC "<Cmd>Clap! command_history<CR>")
(noremap! [n :nowait] :<Leader>sk "<Cmd>Clap! maps<CR>")
