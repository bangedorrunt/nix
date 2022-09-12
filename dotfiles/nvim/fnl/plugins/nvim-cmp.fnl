(import-macros {: lazyreq : lazyfunc} :core.macros)

(let
  [{: first : dec : get : concat} (lazyfunc :core.funs)
   {: setup : visible : complete
    : select_next_item : select_prev_item
    : mapping : config} (lazyreq :cmp)
   {: expand_or_jumpable : expand_or_jump
    : jumpable : jump : lsp_expand} (lazyfunc :luasnip)
   ;; VSCode icons
   vscode_kinds  {:Array " "
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
                  :Variable " "}
  cmp_menu_items {:nvim_lsp :LSP
                  :luasnip :LuaSnip
                  :conjure :Conjure
                  :buffer :Buffer
                  :calc :Calc
                  :path :Path}
  cmp_srcs      [{:name :nvim_lsp}
                 {:name :conjure}
                 {:name :luasnip}
                 {:name :buffer :keyword_length 5}
                 {:name :path}
                 {:name :neorg}
                 {:name :nvim_lua}
                 {:name :calc}]
  cmp_window {:border ["┌" "─" "┐" "│" "┘" "─" "└" "│"]}
  cmp_fmt
  (fn [_ item]
    (doto item (tset :kind (concat (get vscode_kinds item.kind "") item.kind))))
  ;; Check backspace
  has_words_before?
  (fn []
    (let [(line col) (unpack (vim.api.nvim_win_get_cursor 0))
          buf_get_lines vim.api.nvim_buf_get_lines]
      (and (not= col 0)
           (= (-> (buf_get_lines 0 (dec line) line true)
                  first
                  (string.sub col col)
                  (string.match "%s"))
              nil))))
  ;; Supertab
  super_cn
  (fn [fallback]
    (if (visible) (select_next_item)
        (expand_or_jumpable) (expand_or_jump)
        (has_words_before?) (complete)
        (fallback)))

  super_cp
  (fn [fallback]
    (if (visible) (select_prev_item)
        (jumpable -1) (jump -1)
        (fallback)))]

  (setup {:window {:completion (config.window.bordered cmp_window)
                   :documentation (config.window.bordered cmp_window)}
          :formatting {:format cmp_fmt}
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
  (setup.cmdline "?" {:mapping (mapping.preset.cmdline)
                      :sources [{:name :buffer}]})
  (setup.cmdline ":" {:mapping (mapping.preset.cmdline)
                      :sources (config.sources [{:name :path}]
                                               [{:name :cmdline}])}))
