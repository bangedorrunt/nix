(import-macros {: setup!} :core.macros)

(local {: run : merge} (require :core.funs))
(local cmp-lsp (require :cmp_nvim_lsp))
(local {:util {:default_config lsp-defaults} &as lspconfig} (require :lspconfig))
(local setup-server #(: (. lspconfig $) :setup {}))

(fn setup []
  (doto lsp-defaults (merge {:capabilities (cmp-lsp.default_capabilities)}))
  (setup! mod.lsp.diagnostics)
  (setup! mod.lsp.on-attach)
  (setup! mason)
  (setup! neodev)
  (setup! rust-tools)
  (run setup-server store.lsp.servers))

{: setup}
