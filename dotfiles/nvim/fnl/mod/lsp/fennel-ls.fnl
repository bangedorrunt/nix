(local lspconfig (require :lspconfig))
(local server-configs (require :lspconfig.configs))

(fn setup []
  (tset server-configs
        :fennel-ls
        {:default_config {:cmd [(.. (vim.fn.stdpath :data) :/mason/bin/fennel-ls)]
                          :filetypes [:fennel]
                          :root_dir (fn [dir] (lspconfig.util.find_git_ancestor dir))
                          :settings {}}})
  ((. (. lspconfig :fennel-ls) :setup) {}))

{: setup}
