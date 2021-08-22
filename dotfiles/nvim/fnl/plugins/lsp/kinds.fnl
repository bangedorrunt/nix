(module plugins.lsp.kinds)

(def- icons {:Class " "
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
             :Variable " "})

(def- kinds vim.lsp.protocol.CompletionItemKind)

(defn setup [] (each [i kind (ipairs kinds)]
                 (tset kinds i (or (. icons kind) kind))))
