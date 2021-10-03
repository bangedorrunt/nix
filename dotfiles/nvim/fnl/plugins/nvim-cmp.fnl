(module plugins.nvim-cmp
  {autoload {nvim aniseed.nvim
             luasnip luasnip
             luasnip/snippets luasnip.loaders.from_vscode
             cmp cmp}
   require-macros [core.macros]})

(def cmp/menu-items {:buffer "[Buffer]"
                     :calc "[Calc]"
                     :conjure "[Conjure]"
                     :emoji "[Emoji]"
                     :nvim_lsp "[LSP]"
                     :cmp_tabnine "[T9]"
                     :path "[Path]"
                     :luasnip "[LuaSnip]"})
(def cmp/srcs [{:name :cmp_tabnine}
               {:name :nvim_lsp}
               {:name :conjure}
               {:name :luasnip}
               {:name :buffer}
               {:name :path}
               {:name :nvim_lua}
               {:name :calc}
               {:name :emoji}])

;; Check backspace
(defn has-words-before? []
  (let [(line col) (unpack (nvim.win_get_cursor 0))]
    (and (not= col 0) (= (: (: (. (nvim.buf_get_lines 0 (- line 1) line true) 1) :sub col col) :match "%s") nil))))

(cmp.setup {
            :formatting {:format (fn [entry item]
                                   (set item.kind (.. "[" item.kind "]"))
                                   (set item.menu
                                        (or (. cmp/menu-items
                                                 entry.source.name)
                                            ""))
                                   item)}
            :mapping {:<C-p> (cmp.mapping.select_prev_item {:behavior cmp.SelectBehavior.Insert})
                      :<C-n> (cmp.mapping.select_next_item {:behavior cmp.SelectBehavior.Insert})
                      :<Up> (cmp.mapping.scroll_docs -4)
                      :<Down> (cmp.mapping.scroll_docs 4)
                      :<C-Space> (cmp.mapping.complete)
                      :<C-e> (cmp.mapping.close)
                      ;; :<CR> (cmp.mapping.confirm {:behavior cmp.ConfirmBehavior.Replace
                      ;;                             :select true})
                      :<Tab> (cmp.mapping (fn [fallback]
                                            (if (cmp.visible)
                                            (cmp.select_next_item {:behavior cmp.SelectBehavior.Insert})
                                                (luasnip.expand_or_jumpable) (luasnip.expand_or_jump)
                                                (has-words-before?) (cmp.complete)
                                                (fallback)))
                                          [:i :s])
                      :<S-Tab> (cmp.mapping (fn [fallback]
                                              (if (cmp.visible)
                                              (cmp.select_prev_item {:behavior cmp.SelectBehavior.Insert})
                                                (luasnip.jumpable -1) (luasnip.jump -1)
                                                (fallback)))
                                            [:i :s])}
            :snippet {:expand (fn [args]
                                (luasnip.lsp_expand args.body))}
            :experimental {:ghost_text false}
            :sources cmp/srcs})

;; Load friendly snippets
(luasnip/snippets.lazy_load {:paths tdt.paths.NVIM_PATH})

(augroup disable-cmp-on-filetype
          (autocmd FileType TelescopePrompt #(cmp.setup.buffer {:enabled false})))
