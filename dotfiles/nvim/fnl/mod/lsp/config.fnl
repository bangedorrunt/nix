(import-macros {: setup!} :core.macros)
(vim.cmd.do "User LspLoaded")
(local {: run : merge} (require :core.funs))
(local cmp-lsp (require :cmp_nvim_lsp))
(local {:util {:default_config lsp-defaults}
        &as lspconfig} (require :lspconfig))
(local config #(: (. lspconfig $) :setup {}))
(local lsp-servers
  [
   :bashls
   :clojure_lsp
   :cssls
   :dockerls
   :emmet_ls
   :eslint
   ;; :fennel_language_server
   :html
   :jsonls
   :marksman
   :grammarly
   ;; :rust_analyzer
   :sumneko_lua
   :tailwindcss
   :tsserver
   ;; :vimls
   :yamlls
   ])
(fn setup []
  (doto lsp-defaults
    (merge {:capabilities (cmp-lsp.default_capabilities)}))
  (setup! mod.lsp.diagnostics)
  (setup! mod.lsp.on-attach)
  (setup! mod.lsp.null-ls)
  (setup! mason)
  (setup! neodev)
  (setup! rust-tools)
  (run config lsp-servers))

{: setup}
