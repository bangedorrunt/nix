(import-macros {: setup!
                : lazyfunc
                : lazyreq} :core.macros)

(local {: merge} (lazyfunc :core.funs))
(local cmp-lsp (lazyreq :cmp_nvim_lsp))
(local rust-tools (lazyreq :rust-tools))
(local neodev (lazyreq :neodev))
(local lspconfig (lazyreq :lspconfig))
(local setup-server #(: (. lspconfig $) :setup {}))

(local mason (lazyreq :mason))
(local mason-lspconfig (lazyreq :mason-lspconfig))

(local lsp-defaults {:flags {:debounce_text_changes 150}
                     :capabilities (cmp-lsp.default_capabilities)})

(fn setup []
  (setup! mod.lsp.ui)
  (setup! mod.lsp.diagnostics)
  (setup! mod.lsp.keymaps)
  (setup! mason)
  (setup! neodev)
  (set lspconfig.util.default_config (merge lspconfig.util.default_config lsp-defaults))
  (mason-lspconfig.setup {:ensure_installed store.lsp.servers})
  (mason-lspconfig.setup_handlers [setup-server])
  (setup! rust-tools))

{: setup}
