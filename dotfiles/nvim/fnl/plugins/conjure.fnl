(module plugins.conjure)

(import-macros {: let!} :core.macros)

(let! conjure#client#fennel#aniseed#aniseed_module_prefix :aniseed.
      conjure#extract#tree_sitter#enabled true)
