(import-macros {: lazyreq} :core.macros)

(local lspconfig (lazyreq :lspconfig))
(local server-configs (lazyreq :lspconfig.configs))
(local get-server #(. lspconfig $))

(tset server-configs :fennel-ls
      {:default_config {:cmd [(.. (vim.fn.stdpath :data) :/mason/bin/fennel-ls)]
                        :filetypes [:fennel]
                        :root_dir (fn [dir] (lspconfig.util.find_git_ancestor dir))
                        :settings {}}})

(local fennel-ls (get-server :fennel-ls))

(fn setup []
  (fennel-ls.setup {}))
