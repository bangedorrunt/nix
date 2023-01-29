(import-macros {: setup!} :core.macros)

(local {: formatting : diagnostics} (require :null-ls.builtins))
;; WARNING: when you experience any lag or unresponsive with Lsp,
;; make sure respective sources are installed
(local null-sources [formatting.prettier
                     formatting.nixpkgs_fmt
                     formatting.stylua
                     formatting.trim_whitespace
                     formatting.shfmt])
(fn setup []
  (setup! null-ls {:sources null-sources}))

{: setup}
