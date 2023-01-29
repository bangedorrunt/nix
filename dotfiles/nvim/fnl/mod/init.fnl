(import-macros {: setup!} :core.macros)

(fn setup []
  (setup! mod.deps)
  (setup! mod.ui)
  (setup! mod.editor)
  (setup! mod.lsp)
  (setup! mod.lang)
  (setup! mod.completion)
  (setup! mod.git)
  (setup! mod.util))

{: setup}
