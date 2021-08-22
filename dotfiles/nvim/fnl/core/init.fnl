(module core.init)

(require :core.base)
(require :core.options)
(require :core.mappings)
(require :core.autocmds)

(if (= (vim.fn.filereadable tdt.paths.PACKER_COMPILED_PATH) 1)
  (do
    (require :packer_compiled)
    ;; REF: folke/dot
    ;; No need to load this immediately, since we have packer_compiled
    (vim.defer_fn (fn []
                          (vim.cmd "packadd packer.nvim")
                          (require :plugins))
                  0))
  (do
    (vim.cmd "packadd packer.nvim")
    (require :plugins)))
