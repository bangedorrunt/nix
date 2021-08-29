(module plugins.nvim-cmp
  {autoload {cljlib cljlib
             icons core.icons
             luasnip luasnip
             snippet luasnip.loaders.from_vscode
             cmp cmp}
   require-macros [core.macros]})

(def- {: get} cljlib)

(def- icontab icons.tab)

(def- cmp-kinds
  {:Class icontab.cubes
   :Color icontab.color
   :Constant icontab.code-tags
   :Constructor icontab.stackoverflow
   :Enum icontab.enum
   :EnumMember icontab.atoz
   :Field icontab.buffer
   :File icontab.document-alt
   :Folder icontab.folder-open-alt
   :Function icontab.function-alt
   :Interface icontab.code-braces
   :Keyword icontab.key
   :Method icontab.code-parentheses
   :Module icontab.cubes
   :Property icontab.property
   :Snippet icontab.code-braces
   :Struct icontab.struct
   :Reference icontab.reference
   :Text icontab.text
   :Unit icontab.unit
   :Value icontab.one-two-three
   :Variable icontab.cube
   :Operator icontab.plus-minus
   :Event icontab.zap
   :TypeParameter icontab.package})

(def- cmp-src-menu-items
  {:buffer "[Buffer]"
   :calc "[Calc]"
   :conjure "[Conjure]"
   :emoji "[Emoji]"
   :nvim_lsp "[LSP]"
   :path "[Path]"
   :luasnip "[LuaSnip]"})

(def- cmp-srcs
  [{:name :nvim_lsp}
   {:name :conjure}
   {:name :luasnip}
   {:name :buffer}
   {:name :path}
   {:name :nvim_lua}
   {:name :calc}
   {:name :emoji}])

(defn- check-back-space []
  (let [col (- (vim.fn.col ".") 1)]
    (if (or (= col 0) (: (: (vim.fn.getline ".") :sub col col) :match "%s"))
        true false)))

(cmp.setup
  {:formatting
   {:format (fn [entry item]
              (set item.kind
                   (.. (or (get cmp-kinds item.kind) "")
                       " " item.kind))
              (set item.menu (or (get cmp-src-menu-items entry.source.name) ""))
              item)}
   :mapping
   {:<C-p> (cmp.mapping.select_prev_item)
    :<C-n> (cmp.mapping.select_next_item)
    :<Up> (cmp.mapping.scroll_docs -4)
    :<Down> (cmp.mapping.scroll_docs 4)
    :<C-s> (cmp.mapping.complete)
    :<C-e> (cmp.mapping.close)
    ;; :<CR> (cmp.mapping.confirm
    ;;         {:behavior cmp.ConfirmBehavior.Insert
    ;;          :select true})
    :<Tab> (cmp.mapping (fn [fallback]
              (if (= (vim.fn.pumvisible) 1) (vim.fn.feedkeys (t :<C-n>) "n")
                (and luasnip (luasnip.expand_or_jumpable)) (vim.fn.feedkeys (t :<Plug>luasnip-expand-or-jump) "")
                (check-back-space) (vim.fn.feedkeys (t :<Tab>) "n")
                (fallback))) ["i" "s"])
    :<S-Tab> (cmp.mapping (fn [fallback]
                (if (= (vim.fn.pumvisible) 1)
                      (vim.fn.feedkeys (t :<C-p>) "n")
                    (and luasnip (luasnip.jumpable (- 1))) 
                      (vim.fn.feedkeys (t :<Plug>luasnip-jump-prev) "")
                    (fallback)
                  )) ["i" "s"])}
   :snippet
   {:expand (fn [args]
              (luasnip.lsp_expand args.body))}
   :sources cmp-srcs})

;; Load friendly-snippets
(snippet.lazy_load {:paths [tdt.paths.VIM_PATH]})

