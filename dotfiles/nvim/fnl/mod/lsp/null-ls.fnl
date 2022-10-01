(import-macros {: lazyreq} :core.macros)

(local null-ls (lazyreq :null-ls))
(local {: formatting : diagnostics} (lazyreq :null-ls.builtins))
;; WARNING: when you experience any lag or unresponsive with Lsp,
;; make sure respective sources are installed
(local null-sources [formatting.prettier
                     formatting.stylua
                     formatting.trim_whitespace
                     formatting.shfmt])
(fn setup []
  (null-ls.setup {:sources null-sources}))

{: setup}
