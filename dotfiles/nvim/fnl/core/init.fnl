(require :core.base)
(require :core.options)
(require :core.mappings)
(require :core.autocmds)

(fn file_exist? [path]
  (= (vim.fn.filereadable path) 1))

(fn load_packer_plugins []
  (vim.cmd.packadd "packer.nvim")
  (require :plugins))

(if (file_exist? tdt.paths.PACKER_COMPILED_PATH)
  (do
    (require :packer_compiled)
    ;; REF: folke/dot
    ;; No need to load this immediately, since we have packer_compiled
    (vim.defer_fn load_packer_plugins 0))
  (load_packer_plugins))
