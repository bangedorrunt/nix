(module plugins.kommentary
        {autoload {kommentary kommentary.config} require-macros [core.macros]})

(let! kommentary_create_default_mappings false)

(kommentary.configure_language :default
                               {:prefer_single_line_comments true
                                :use_consistent_indentation true
                                :ignore_whitespace true})

(each [_ v (pairs [:fennel :clojure])]
  (-> v
      (kommentary.configure_language {:single_line_comment_string ";;"})))

;; SEE: https://github.com/b3nj5m1n/kommentary/issues/20#issuecomment-774664395
(map! [n] :gcc :<Plug>kommentary_line_default)
(map! [n] :gc :<Plug>kommentary_motion_default)
(map! [v] :gc :<Plug>kommentary_visual_default<C-c>)
