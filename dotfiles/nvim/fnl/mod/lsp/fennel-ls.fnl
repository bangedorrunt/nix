(import-macros {: autocmd} :core.macros)
(local lspconfig (require :lspconfig))
(local server-configs (require :lspconfig.configs))

(fn setup []
  (tset server-configs
        :fennel_language_server
        {:default_config
         {:cmd [:fennel-language-server]
          :filetypes [:fennel]
          :root_dir (lspconfig.util.root_pattern :fnl)
          :single_file_support true
          :settings
          {:fennel {:workspace {:library (vim.api.nvim_list_runtime_paths)}
                    :diagnostics {:globals [:vim]}}}}})
  ;; Temporarily disable fennel diagnostic
  ;; (autocmd FileType fennel (fn [args] (vim.diagnostic.disable args.buf)))
  )

{: setup}
