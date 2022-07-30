(module plugins.nvim-cmp
  {autoload {: cmp
             : luasnip
             : lspkind}})

(def- cmp-compare cmp.config.compare)

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

(cmp.setup {:formatting {:format (lspkind.cmp_format {:with_text false
                                                      :menu cmp-menu-items})}
            :sorting {:comparators [cmp-compare.offset
                                    cmp-compare.exact
                                    cmp-compare.score
                                    cmp-compare.kind
                                    cmp-compare.sort_text
                                    cmp-compare.length
                                    cmp-compare.order]}
            :mapping (cmp.mapping.preset.insert
                       {:<CR> (cmp.mapping.confirm {:select true})
                        :<C-Space> (cmp.mapping.complete)
                        :<C-e> (cmp.mapping.abort)})
            :snippet {:expand #(luasnip.lsp_expand $.body)}
            :sources cmp-srcs})

; Cmdline completions
(cmp.setup.cmdline "/" {:mapping (cmp.mapping.preset.cmdline)
                        :sources [{:name :buffer}]})
(cmp.setup.cmdline ":" {:mapping (cmp.mapping.preset.cmdline)
                        :sources (cmp.config.sources
                                   [{:name :path}]
                                   [{:name :cmdline}])})
