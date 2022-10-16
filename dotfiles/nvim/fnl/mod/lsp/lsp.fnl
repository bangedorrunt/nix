(import-macros {: setup!
                : lazyfunc
                : lazyreq} :core.macros)

(local {: merge} (lazyfunc :core.funs))
(local cmp-lsp (lazyreq :cmp_nvim_lsp))
(local rust-tools (lazyreq :rust-tools))
(local neodev (lazyreq :neodev))
(local {:util {:default_config lsp-defaults} &as lspconfig} (lazyreq :lspconfig))
(local setup-server #(: (. lspconfig $) :setup {}))

(local mason (lazyreq :mason))
(local mason-lspconfig (lazyreq :mason-lspconfig))

(fn setup []
  (setup! mod.lsp.ui)
  (setup! mod.lsp.diagnostics)
  (setup! mod.lsp.keymaps)
  (setup! mason)
  (setup! neodev)
  (doto lsp-defaults (merge {:flags {:debounce_text_changes 150}
                             :capabilities (cmp-lsp.default_capabilities)}))
  (mason-lspconfig.setup {:ensure_installed store.lsp.servers})
  (mason-lspconfig.setup_handlers [setup-server])
  (setup! rust-tools))

{: setup}
