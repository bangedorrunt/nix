(import-macros {: set! : vim-has?} :core.macros)
;; https://github.com/folke/dot/blob/d5013a1c1f7f4e96b0d16e1c425941e39572271f/config/nvim/lua/util/status.lua
(local {: map} (require :core.funs))

(local M {})

(set _G.Status M)

(fn M.get_signs []
  (let [buf (vim.api.nvim_win_get_buf vim.g.statusline_winid)]
    (map (fn [sign]
           (. (vim.fn.sign_getdefined sign.name) 1))
         (. (. (vim.fn.sign_getplaced buf {:group "*" :lnum vim.v.lnum})
               1) :signs))))

(fn M.column []
  (var (sign git-sign) nil)
  (each [_ s (ipairs (M.get_signs))]
    (if (s.name:find :GitSign) (set git-sign s) (set sign s)))
  (var nu " ")
  (local number (vim.api.nvim_win_get_option vim.g.statusline_winid :number))
  (when (and (and number vim.wo.relativenumber) (= vim.v.virtnum 0))
    (set nu (or (and (= vim.v.relnum 0) vim.v.lnum) vim.v.relnum)))
  (local components [(or (and sign (.. "%#" sign.texthl "#" sign.text "%*"))
                         " ")
                     "%="
                     (.. nu " ")
                     (or (and git-sign
                              (.. "%#" git-sign.texthl "#" git-sign.text "%*"))
                         "  ")])
  (table.concat components ""))

(when (vim-has? :nvim-0.9.0)
  (set! statuscolumn "%!v:lua.Status.column()"))

M
