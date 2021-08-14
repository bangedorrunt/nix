(module core.init
        {autoload {core aniseed.core
                   nvim aniseed.nvim}})

(require :core.base)
(require :core.options)
(require :core.mappings)
(require :core.events)

(if (= (vim.fn.filereadable tdt.paths.PACKER_COMPILED_PATH) 1)
  (do
    (require :packer_compiled)
    ;; REF: folke/dot
    ;; No need to load this immediately, since we have packer_compiled
    (vim.defer_fn (lambda []
                          (nvim.ex.packadd :packer.nvim)
                          (require :plugins))
                  0))
  (do
    (nvim.ex.packadd :packer.nvim)
    (require :plugins)))
