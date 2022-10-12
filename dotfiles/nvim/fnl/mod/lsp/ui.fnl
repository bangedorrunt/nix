(import-macros {: command} :core.macros)

;; More general LSP commands
(fn reload-lsp []
  (vim.lsp.stop_client (vim.lsp.get_active_clients))
  (vim.cmd.edit))

(fn open-lsp-log []
  (let [path (vim.lsp.get_log_path)]
    (vim.cmd.edit path)))

(fn setup []
  (let [{: with : handlers} vim.lsp
        open-floating-preview vim.lsp.util.open_floating_preview
        border store.border]

    (command LspLog open-lsp-log)
    (command LspRestart reload-lsp)

    (set vim.lsp.handlers.textDocument/signatureHelp
         (with handlers.signature_help {: border}))

    (set vim.lsp.handlers.textDocument/hover (with handlers.hover {: border}))

    (fn vim.lsp.util.open_floating_preview [...]
      (let [(bufnr winid) (open-floating-preview ...)]
        (vim.api.nvim_win_set_option winid :breakindentopt "")
        (vim.api.nvim_win_set_option winid :showbreak :NONE)
        (values bufnr winid)))))

{: setup}
