(local {: setup
        : visible
        : complete
        : select_next_item
        : select_prev_item
        : mapping
        : config} (require :cmp))

(local {: expand_or_jumpable
        : expand_or_jump
        : jumpable
        : jump
        : lsp_expand} (require :luasnip))

(local {: cmp_format} (require :lspkind))

(local cmp_menu_items {:nvim_lsp :LSP
                       :luasnip :LuaSnip
                       :conjure :Conjure
                       :buffer :Buffer
                       :calc :Calc
                       :path :Path})

(local cmp_srcs [{:name :nvim_lsp}
                 {:name :conjure}
                 {:name :luasnip}
                 {:name :buffer :keyword_length 5}
                 {:name :path}
                 {:name :neorg}
                 {:name :nvim_lua}
                 {:name :calc}])

;; Check backspace
(fn has_words_before? []
  (let [(line col) (unpack (vim.api.nvim_win_get_cursor 0))]
    (and (not= col 0)
         (= (: (: (. (vim.api.nvim_buf_get_lines 0 (- line 1) line true)
                     1)
                  :sub col col)
               :match "%s")
            nil))))
;; Supertab
(fn super_cn [fallback]
  (if (visible) (select_next_item)
      (expand_or_jumpable) (expand_or_jump)
      (has_words_before?) (complete)
      (fallback)))

(fn super_cp [fallback]
  (if (visible) (select_prev_item)
      (jumpable -1) (jump -1)
      (fallback)))

(setup {:formatting {:format (cmp_format {:with_text false
                                          :menu cmp_menu_items})}
        :mapping (mapping.preset.insert
                   {:<CR> (mapping.confirm {:select true})
                    :<C-Space> (mapping.complete)
                    :<C-e> (mapping.abort)
                    :<C-n> (mapping super_cn [:i :s])
                    :<C-p> (mapping super_cp [:i :s])})
        :snippet {:expand #(lsp_expand $.body)}
        :sources (config.sources cmp_srcs)})

;; Cmdline completions
(setup.cmdline "/" {:mapping (mapping.preset.cmdline)
               :sources [{:name :buffer}]})
(setup.cmdline ":" {:mapping (mapping.preset.cmdline)
               :sources (config.sources
                          [{:name :path}]
                          [{:name :cmdline}])})
