(module plugins.lsp.kinds
  {autoload {icons core.icons}})

(def- icontab icons.tab)

(def- lsp-kinds {:Class icontab.cubes
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

(def- kinds vim.lsp.protocol.CompletionItemKind)

(defn setup [] (each [i kind (ipairs kinds)]
                 (tset kinds i (or (. lsp-kinds kind) kind))))
