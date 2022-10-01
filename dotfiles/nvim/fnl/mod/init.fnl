(import-macros {: lazyreq : lazyfunc : setup!} :core.macros)

(local {: run! : concat+} (lazyfunc :core.funs))
(var plugins [])
(let [dependencies [[:wbthomason/packer.nvim :opt true]
                    [:rktjmp/hotpot.nvim]
                    [:nvim-lua/plenary.nvim]]
      ui (lazyreq :mod.ui)
      editor (lazyreq :mod.editor)
      lsp (lazyreq :mod.lsp)
      lang (lazyreq :mod.lang)
      completion (lazyreq :mod.completion)
      git (lazyreq :mod.git)
      tool (lazyreq :mod.tool)]
  (set plugins (concat+ dependencies
                        ui.plugins
                        editor.plugins
                        lsp.plugins
                        lang.plugins
                        completion.plugins
                        git.plugins
                        tool.plugins)))

(fn setup []
  (setup! :mod.ui)
  (setup! :mod.editor)
  (setup! :mod.lsp)
  (setup! :mod.lang)
  (setup! :mod.completion)
  (setup! :mod.git)
  (setup! :mod.tool))

{: plugins
 : setup}
