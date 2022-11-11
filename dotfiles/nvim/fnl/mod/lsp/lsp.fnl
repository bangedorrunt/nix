(import-macros {: setup!} :core.macros)

(local {: run : merge} (require :core.funs))
(local cmp-lsp (require :cmp_nvim_lsp))
(local {:util {:default_config lsp-defaults} &as lspconfig} (require :lspconfig))
(local config #(: (. lspconfig $) :setup {}))
(local lsp-servers [:bashls
                    :clojure_lsp
                    :cssls
                    :diagnosticls
                    :dockerls
                    :emmet_ls
                    :eslint
                    :html
                    :jsonls
                    :marksman
                    ;; :rust_analyzer
                    :sumneko_lua
                    :tailwindcss
                    :tsserver
                    :vimls
                    :yamlls])
(fn setup []
  (doto lsp-defaults (merge {:capabilities (cmp-lsp.default_capabilities)}))
  (setup! mod.lsp.diagnostics)
  (setup! mod.lsp.on-attach)
  (setup! mason)
  (setup! neodev)
  (setup! rust-tools)
  (run config lsp-servers))

{: setup}
