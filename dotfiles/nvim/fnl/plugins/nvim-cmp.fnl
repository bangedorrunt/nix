(module plugins.nvim-cmp
  {autoload {{: setup
              : visible
              : complete
              : select_next_item
              : select_prev_item
              : mapping
              : config} cmp
             {: expand_or_jumpable
              : expand_or_jump
              : jumpable
              : jump
              : lsp_expand} luasnip
             {: cmp_format} lspkind}})

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
                {:name :neorg}
                {:name :nvim_lua}
                {:name :calc}])

;; Check backspace
(defn- has-words-before? []
  (let [col (- (vim.fn.col ".") 1)
        ln (vim.fn.getline ".")]
    (or (= col 0) (string.match (string.sub ln col col) "%s"))))

;; Supertab
(defn- super-cn [fallback]
  (if (visible) (select_next_item)
    (expand_or_jumpable) (expand_or_jump)
    (has-words-before?) (complete)
    (fallback)))

(defn- super-cp [fallback]
  (if (visible) (select_prev_item)
    (jumpable -1) (jump -1)
    (fallback)))

(setup {:formatting {:format (cmp_format {:with_text false
                                          :menu cmp-menu-items})}
        :mapping (mapping.preset.insert
                   {:<CR> (mapping.confirm {:select true})
                    :<C-Space> (mapping.complete)
                    :<C-e> (mapping.abort)
                    :<C-n> (mapping super-cn [:i :s])
                    :<C-p> (mapping super-cp [:i :s])})
        :snippet {:expand #(lsp_expand $.body)}
        :sources cmp-srcs})

;; Cmdline completions
(setup.cmdline "/" {:mapping (mapping.preset.cmdline)
                    :sources [{:name :buffer}]})
(setup.cmdline ":" {:mapping (mapping.preset.cmdline)
                    :sources (config.sources
                               [{:name :path}]
                               [{:name :cmdline}])})
