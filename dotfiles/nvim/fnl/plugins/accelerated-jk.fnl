(module plugins.accelerated-jk
        {require-macros [core.macros]})

(set _G.enhance_jk_move (fn [key]
                          (match key
                            :j (t "<Plug>(accelerated_jk_gj)")
                            :k (t "<Plug>(accelerated_jk_gk)"))))
(map! [n] "j" "v:lua.enhance_jk_move('j')" :expr)
(map! [n] "k" "v:lua.enhance_jk_move('k')" :expr)
