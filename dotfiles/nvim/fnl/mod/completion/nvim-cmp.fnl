(local {: get : into} (require :core.funs))
(local {: visible : complete : select_next_item : select_prev_item : mapping : config
        &as cmp} (require :cmp))
(local {: expand_or_jumpable : expand_or_jump : jumpable : jump : lsp_expand} (require :luasnip))
(local luasnip-vscode-snippets (require :luasnip.loaders.from_vscode))
(local cmp-menu-items {
                       :nvim_lsp :LSP
                       :luasnip :LuaSnip
                       ;; :conjure :Conjure
                       :buffer :Buffer
                       :calc :Calc
                       :path :Path
                       })
(local cmp-sources [
                    {:name :nvim_lsp}
                    ;; {:name :conjure}
                    {:name :luasnip}
                    {:name :buffer :keyword_length 3}
                    {:name :path}
                    {:name :neorg}
                    {:name :nvim_lua}
                    {:name :calc}
                    ])
(local lsp-icons {
                  :Array " "
                  :Boolean " "
                  :Class " "
                  :Color " "
                  :Constant " "
                  :Constructor " "
                  :Enum " "
                  :EnumMember " "
                  :Event " "
                  :Field " "
                  :File " "
                  :Folder " "
                  :Function " "
                  :Interface " "
                  :Keyword " "
                  :Method " "
                  :Module " "
                  :Namespace " "
                  :Null "ﳠ "
                  :Number " "
                  :Object " "
                  :Operator " "
                  :Package " "
                  :Property " "
                  :Reference " "
                  :Snippet " "
                  :Struct " "
                  :String " "
                  :Text " "
                  :TypeParameter " "
                  :Unit " "
                  :Value " "
                  :Variable " "
                  })

(fn cmp-fmt [entry item]
  (let [codicons (into item :kind
                       (.. (get lsp-icons item.kind "") item.kind))
        codicons-item (vim.split codicons.kind "%s" {:trimempty true})
        [codicons-kind codicons-menu] codicons-item
        cmp-menu (get cmp-menu-items entry.source.name "")]
    (into item :kind codicons-kind :menu cmp-menu)))

;; Check backspace
(fn has-words-before? []
  (let [[row col] (vim.api.nvim_win_get_cursor 0)
        line (- row 1)
        col* (math.max 0 (- col 1))
        [text] (vim.api.nvim_buf_get_text 0 line col* line col {})]
    (text:match "%w")))

;; Supertab
(fn super-cn [fallback]
  (if (visible) (select_next_item)
      (expand_or_jumpable) (expand_or_jump)
      (has-words-before?) (complete)
      (fallback)))

(fn super-cp [fallback]
  (if (visible) (select_prev_item)
      (jumpable -1) (jump -1)
      (fallback)))

(fn setup []
  (cmp.setup {:formatting {:fields [:kind :abbr :menu] :format cmp-fmt}
              :mapping (mapping.preset.insert
                         {:<CR> (mapping.confirm {:behavior cmp.ConfirmBehavior.Replace
                                                  :select true})
                          :<C-Space> (mapping.complete)
                          :<C-e> (mapping.abort)
                          :<C-n> (mapping super-cn [:i :s])
                          :<C-p> (mapping super-cp [:i :s])})
              :sorting {:comparators [
                                      config.compare.offset
                                      config.compare.exact
                                      config.compare.recently_used
                                      config.compare.kind
                                      config.compare.sort_text
                                      config.compare.length
                                      config.compare.order
                                      ]}
              :snippet {:expand #(lsp_expand $.body)}
              :sources (config.sources cmp-sources)})

  ;; Cmdline completions
  (cmp.setup.cmdline
    ["/" "?"]
    {:mapping (mapping.preset.cmdline)
     :sources [{:name :buffer}]})

  (cmp.setup.cmdline
    ":"
    {:mapping (mapping.preset.cmdline)
     :sources (config.sources [{:name :path}] [{:name :cmdline}])})

  ;; VS-Code snippets
  (luasnip-vscode-snippets.lazy_load)
  (luasnip-vscode-snippets.lazy_load {:paths [store.paths.luasnip]})
  )

{: setup}
