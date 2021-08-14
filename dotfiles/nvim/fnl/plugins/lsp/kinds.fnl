(module plugins.lsp.kinds)

(def- icons  {
  :Class " "
  :Color " "
  :Constant " "
  :Constructor " "
  :Enum "了 "
  :EnumMember " "
  :Field " "
  :File " "
  :Folder " "
  :Function " "
  :Interface "ﰮ "
  :Keyword " "
  :Method "ƒ "
  :Module " "
  :Property " "
  :Snippet "﬌ "
  :Struct " "
  :Text " "
  :Unit " "
  :Value " "
  :Variable " "
})

(defn setup []
  (let [kinds vim.lsp.protocol.CompletionItemKind]
    (each [i kind (ipairs kinds)]
      (tset kinds i (or (. icons kind) kind)))))

