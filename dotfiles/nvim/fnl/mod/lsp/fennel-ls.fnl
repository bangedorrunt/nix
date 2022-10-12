(import-macros {: lazyreq} :core.macros)

(local lspconfig (lazyreq :lspconfig))
(local server-configs (lazyreq :lspconfig.configs))
(local get-server #(. lspconfig $))

(local capabilities
  (let [c (vim.lsp.protocol.make_client_capabilities)]
    ;; NOTE: use `cmp_nvim_lsp.update_capabilities` is unneccessary
    ;; https://github.com/hrsh7th/cmp-nvim-lsp/blob/f6f471898bc4b45eacd36eef9887847b73130e0e/lua/cmp_nvim_lsp/init.lua#L23
    ;; Delegate snippet support to any completion engine such as `nvim-cmp`
    (set c.textDocument.completion.completionItem
         {:documentationFormat [:markdown :plaintext]
          :snippetSupport true
          :preselectSupport true
          :insertReplaceSupport true
          :labelDetailsSupport true
          :deprecatedSupport true
          :commitCharactersSupport true
          :tagSupport {:valueSet {1 1}}
          :resolveSupport {:properties [:documentation
                                        :detail
                                        :additionalTextEdits]}})))

(tset server-configs :fennel-ls
      {:default_config {:cmd [(.. (vim.fn.stdpath :data) :/mason/bin/fennel-ls)]
                        :filetypes [:fennel]
                        :root_dir (fn [dir] (lspconfig.util.find_git_ancestor dir))
                        :settings {}}})

(local fennel-ls (get-server :fennel-ls))

(fn setup []
  (fennel-ls.setup {: capabilities}))
