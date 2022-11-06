(local {: first : second : dec : get : into} (require :core.funs))
(local {: visible : complete : select_next_item : select_prev_item : mapping : config
        &as cmp} (require :cmp))
(local {: expand_or_jumpable : expand_or_jump : jumpable : jump : lsp_expand} (require :luasnip))

(fn cmp-fmt [entry item]
  (let [codicons (into item :kind
                       (.. (get store.lsp.icons item.kind "") item.kind))
        codicons-item (vim.split codicons.kind "%s" {:trimempty true})
        codicons-kind (first codicons-item)
        codicons-menu (second codicons-item)
        cmp-menu (get store.cmp.menu-items entry.source.name "")]
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
              :mapping (mapping.preset.insert {:<CR> (mapping.confirm {:select true})
                                               :<C-Space> (mapping.complete)
                                               :<C-e> (mapping.abort)
                                               :<C-n> (mapping super-cn [:i :s])
                                               :<C-p> (mapping super-cp [:i :s])})
              :sorting {:comparators [config.compare.offset
                                      config.compare.exact
                                      config.compare.recently_used
                                      config.compare.kind
                                      config.compare.sort_text
                                      config.compare.length
                                      config.compare.order]}
              :snippet {:expand #(lsp_expand $.body)}
              :sources (config.sources store.cmp.sources)})

  ;; Cmdline completions
  (cmp.setup.cmdline ["/" "?"] {:mapping (mapping.preset.cmdline)
                                :sources [{:name :buffer}]})

  (cmp.setup.cmdline ":" {:mapping (mapping.preset.cmdline)
                          :sources (config.sources [{:name :path}] [{:name :cmdline}])}))
{: setup}
