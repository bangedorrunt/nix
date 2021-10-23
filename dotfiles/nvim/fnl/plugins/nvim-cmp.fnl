(module plugins.nvim-cmp
  {autoload {: cmp
             : luasnip
             : lspkind}
   require-macros [core.macros]})

(def- cmp/menu-items {:cmp_tabnine "[T9]"
                     :nvim_lsp "[LSP]"
                     :luasnip "[LuaSnip]"
                     :conjure "[Conjure]"
                     :buffer "[Buffer]"
                     :calc "[Calc]"
                     :path "[Path]"})

(def- cmp/srcs [{:name :cmp_tabnine}
               {:name :nvim_lsp}
               {:name :conjure}
               {:name :luasnip}
               {:name :buffer :keyword_lengthen 5}
               {:name :path}
               {:name :nvim_lua}
               {:name :calc}])

; Menu display
; (defn- cmp/format [entry item]
;   (set item.menu
;        (or (. cmp/menu-items
;               entry.source.name)
;            ""))
;   item)

; Check backspace
(defn- has-words-before? []
  (let [(line col) (unpack (vim.api.nvim_win_get_cursor 0))]
    (and (not= col 0) (= (: (: (. (vim.api.nvim_buf_get_lines 0 (- line 1) line true) 1) :sub col col) :match "%s") nil))))

; Supertab
(defn- super-tab [fallback]
  (if (cmp.visible) (cmp.select_next_item)
      (luasnip.expand_or_jumpable) (luasnip.expand_or_jump)
      (has-words-before?) (cmp.complete)
      (fallback)))

(defn- super-s-tab [fallback]
  (if (cmp.visible) (cmp.select_prev_item)
      (luasnip.jumpable -1) (luasnip.jump -1)
      (fallback)))

(cmp.setup {:formatting {:format (lspkind.cmp_format {:with_text true
                                                      :menu cmp/menu-items})}
            :mapping {:<CR> (cmp.mapping.confirm {:behavior cmp.ConfirmBehavior.Replace
                                                  :select false})
                      :<C-Space> (cmp.mapping.complete)
                      :<C-e> (cmp.mapping.close)
                      :<Tab> (cmp.mapping super-tab [:i :s])
                      :<S-Tab> (cmp.mapping super-s-tab [:i :s])}
            :snippet {:expand (fn [args] (luasnip.lsp_expand args.body))}
            :experimental {:ghost_text true}
            :sources cmp/srcs})

