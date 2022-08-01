(module plugins.nvim-cmp
  {autoload {: cmp
             : luasnip
             : lspkind}})

(def- cmp-menu-items {:nvim_lsp :LSP
                      :luasnip :LuaSnip
                      :conjure :Conjure
                      :buffer :Buffer
                      :calc :Calc
                      :path :Path})

(def- cmp-srcs [{:name :nvim_lsp}
                {:name :conjure}
                {:name :luasnip}
                {:name :buffer :keyword_length 5}
                {:name :path}
                {:name :nvim_lua}
                {:name :calc}])

; Check backspace
(defn- has-words-before? []
  (let [col (- (nvim.fn.col ".") 1)
        ln (nvim.fn.getline ".")]
    (or (= col 0) (string.match (string.sub ln col col) "%s"))))

; Supertab
(defn- super-cn [fallback]
  (if (cmp.visible) (cmp.select_next_item)
      (luasnip.expand_or_jumpable) (luasnip.expand_or_jump)
      (has-words-before?) (cmp.complete)
      (fallback)))

(defn- super-cp [fallback]
  (if (cmp.visible) (cmp.select_prev_item)
      (luasnip.jumpable -1) (luasnip.jump -1)
      (fallback)))

(cmp.setup {:formatting {:format (lspkind.cmp_format {:with_text false
                                                      :menu cmp-menu-items})}
            :mapping (cmp.mapping.preset.insert
                       {:<CR> (cmp.mapping.confirm {:select true})
                        :<C-Space> (cmp.mapping.complete)
                        :<C-e> (cmp.mapping.abort)
                        :<C-n> (cmp.mapping super-cn [:i :s])
                        :<C-p> (cmp.mapping super-cp [:i :s])})
            :snippet {:expand #(luasnip.lsp_expand $.body)}
            :sources cmp-srcs})

; Cmdline completions
(cmp.setup.cmdline "/" {:mapping (cmp.mapping.preset.cmdline)
                        :sources [{:name :buffer}]})
(cmp.setup.cmdline ":" {:mapping (cmp.mapping.preset.cmdline)
                        :sources (cmp.config.sources
                                   [{:name :path}]
                                   [{:name :cmdline}])})
