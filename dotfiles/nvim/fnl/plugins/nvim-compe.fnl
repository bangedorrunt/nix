(module plugins.nvim-compe
  {autoload {nvim-compe compe}
   require-macros [core.macros]})

(nvim-compe.setup
  {:enabled true
   :autocomplete true
   :debug false
   :min_lenght 1
   :preselect "enable"
   :throttle_time 80
   :source_timeout 200
   :incomplete_delay 400
   :max_abbr_width 100
   :max_kind_width 100
   :max_menu_width 100
   :documentation true
   :source {:path true
            :buffer true
            :treesitter true
            :nvim_lsp {:ignored_filetypes ["clojure" "fennel"]}
            :conjure true
            :vsnip true
            :calc true}})

(noremap! [i :expr :silent] "<C-Space>" "compe#complete()")
(noremap! [i :expr :silent] "<C-e>" "compe#close('<C-e>')")
;; (noremap! [i :expr :silent] "<CR>" "compe#confirm('<CR>')")
(noremap! [i :expr :silent] "<C-f>" "compe#scroll({'delta': +4})")
(noremap! [i :expr :silent] "<C-d>" "compe#scroll({'delta': -4})")

(defn- check-back-space []
  (let [col (- (vim.fn.col ".") 1)]
    (if (or (= col 0) (: (: (vim.fn.getline ".") :sub col col) :match "%s"))
        true false)))
;; luasnip 
;; (set _G.tab_complete
;;   (fn []
;;     (if (= (vim.fn.pumvisible) 1) (t :<C-n>)
;;       (and luasnip (luasnip.expand_or_jumpable)) (t :<Plug>luasnip-expand-or-jump)
;;       (check-back-space) (t :<Tab>)
;;       ((. vim.fn "compe#complete")))))

;; (set _G.s_tab_complete 
;;   (fn []
;;     (if (= (vim.fn.pumvisible) 1) (t :<C-p>)
;;       (and luasnip (luasnip.jumpable (- 1))) (t :<Plug>luasnip-jump-prev)
;;       (t :<S-Tab>))))

;; vs-snip
(set _G.tab_complete
  (fn []
    (if (= (vim.fn.pumvisible) 1)
          (t "<C-n>")
        (= (vim.fn.call "vsnip#available" { 1 1 }) 1)
          (t "<Plug>(vsnip-expand-or-jump)")
        (check-back-space)
          (t "<Tab>")
        ((. vim.fn "compe#complete")))))

(set _G.s_tab_complete
  (fn []
    (if (= (vim.fn.pumvisible) 1)
          (t "<C-p>")
        (= (vim.fn.call "vsnip#jumpable" { 1 -1 }) 1)
          (t "<Plug>(vsnip-jump-prev)")
        (t "<S-Tab>"))))

(noremap! [is :expr :silent] "<Tab>" "v:lua.tab_complete()")
(noremap! [is :expr :silent] "<S-Tab>" "v:lua.s_tab_complete()")

(vim.cmd "autocmd User CompeConfirmDone silent! lua vim.lsp.buf.signature_help()")
