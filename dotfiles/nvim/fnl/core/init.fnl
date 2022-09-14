(import-macros {: lazyfunc} :core.macros)

(require :core.base)
(require :core.options)
(require :core.mappings)
(require :core.autocmds)

(fn file-exist? [path]
  (= (vim.fn.filereadable path) 1))

(fn load-packer-plugins []
  (vim.cmd.packadd :packer.nvim)
  (require :plugins))

(if (file-exist? tdt.paths.PACKER-COMPILED-PATH)
    (do
      (require :packer_compiled)
      ;; REF: folke/dot
      ;; No need to load this immediately, since we have packer_compiled
      (vim.defer_fn load-packer-plugins 0))
    (load-packer-plugins))
