(module plugins.accelerated-jk)

(import-macros {: nmap! : t} :core.macros)

(set _G.enhance_jk_move (fn [key]
                          (match key
                            :j (t "<Plug>(accelerated_jk_gj)")
                            :k (t "<Plug>(accelerated_jk_gk)"))))
(nmap! [n :expr] "j" "v:lua.enhance_jk_move('j')")
(nmap! [n :expr] "k" "v:lua.enhance_jk_move('k')")
