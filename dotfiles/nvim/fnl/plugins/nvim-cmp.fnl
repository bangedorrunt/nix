(module plugins.nvim-cmp
  {autoload {: cmp
             : luasnip
             : lspkind}
   require-macros [core.macros]})

(def- cmp/menu-items {:cmp_tabnine :T9
                      :nvim_lsp :LSP
                      :luasnip :LuaSnip
                      :conjure :Conjure
                      :buffer :Buffer
                      :calc :Calc
                      :path :Path})

(def- cmp/srcs [{:name :cmp_tabnine}
                  {:name :nvim_lsp}
                {:name :conjure}
                {:name :luasnip}
                {:name :buffer :keyword_length 5}
                {:name :path}
                {:name :nvim_lua}
                {:name :calc}])

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

(cmp.setup {:completion {:completeopt "menuone,noinsert"}
            :formatting {:format (lspkind.cmp_format {:with_text false
                                                      :menu cmp/menu-items})}
            :mapping {:<CR> (cmp.mapping.confirm {:behavior cmp.ConfirmBehavior.Replace
                                                  :select false})
                      :<C-Space> (cmp.mapping.complete)
                      :<C-e> (cmp.mapping {:i (cmp.mapping.abort)
                                           :c (cmp.mapping.close)})
                      :<Tab> (cmp.mapping super-tab [:i :s])
                      :<S-Tab> (cmp.mapping super-s-tab [:i :s])}
            :snippet {:expand #(luasnip.lsp_expand $.body)}
            :experimental {:ghost_text true}
            :sources cmp/srcs})

; Cmdline completions
(cmp.setup.cmdline :/ {:sources [{:name :buffer}]
                       :mapping (cmp.mapping.preset.cmdline)})
(cmp.setup.cmdline ":" {:mapping (cmp.mapping.preset.cmdline)
                        :sources (cmp.config.sources
                                   [{:name :path}]
                                   [{:name :cmdline}])})
