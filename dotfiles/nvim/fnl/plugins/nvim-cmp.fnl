(import-macros {: lazyreq : lazyfunc} :core.macros)

(local {: first : second : dec : get : into} (lazyfunc :core.funs))
(local {: setup
        : visible
        : complete
        : select_next_item
        : select_prev_item
        : mapping
        : config} (lazyreq :cmp))

(local {: expand_or_jumpable : expand_or_jump : jumpable : jump : lsp_expand}
       (lazyfunc :luasnip))

(local cmp-menu-items {:nvim_lsp :LSP
                       :luasnip :LuaSnip
                       :conjure :Conjure
                       :buffer :Buffer
                       :calc :Calc
                       :path :Path})

(local cmp-srcs [{:name :nvim_lsp}
                 {:name :conjure}
                 {:name :luasnip}
                 {:name :buffer :keyword_length 5}
                 {:name :path}
                 {:name :neorg}
                 {:name :nvim_lua}
                 {:name :calc}])

(local cmp-window
       {:border tdt.border
        :winhighlight "Normal:Normal,FloatBorder:FloatBorder,CursorLine:Visual,Search:None"})

(fn cmp-fmt [_ item]
  (let [codicons (into item :kind (.. (get tdt.lsp item.kind "") item.kind))
        codicons-item (vim.split codicons.kind "%s" {:trimempty true})
        codicons-kind (first codicons-item)
        codicons-menu (second codicons-item)]
    (into item :kind codicons-kind :menu codicons-menu)))

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

(setup {:window {:completion (config.window.bordered cmp-window)
                 :documentation (config.window.bordered cmp-window)}
        :formatting {:fields [:kind :abbr :menu] :format cmp-fmt}
        :mapping (mapping.preset.insert {:<CR> (mapping.confirm {:select true})
                                         :<C-Space> (mapping.complete)
                                         :<C-e> (mapping.abort)
                                         :<C-n> (mapping super-cn [:i :s])
                                         :<C-p> (mapping super-cp [:i :s])})
        :snippet {:expand #(lsp_expand $.body)}
        :sources (config.sources cmp-srcs)})

;; Cmdline completions
(setup.cmdline "/" {:mapping (mapping.preset.cmdline)
                    :sources [{:name :buffer}]})

(setup.cmdline "?" {:mapping (mapping.preset.cmdline)
                    :sources [{:name :buffer}]})

(setup.cmdline ":"
               {:mapping (mapping.preset.cmdline)
                :sources (config.sources [{:name :path}] [{:name :cmdline}])})
