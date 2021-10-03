(module plugins.conjure
  {require-macros [core.macros]})

(g conjure#client#fennel#aniseed#aniseed_module_prefix "aniseed.")
(g conjure#log#botright true)
(g conjure#extract#tree_sitter#enabled true)
