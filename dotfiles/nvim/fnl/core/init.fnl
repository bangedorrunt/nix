(module core.init)

(require :core.base)
(require :core.options)
(require :core.mappings)
(require :core.autocmds)

(defn- load-packer-plugins [] 
       (vim.cmd "packadd packer.nvim")
       (require :plugins))

(if (= (vim.fn.filereadable tdt.paths.PACKER_COMPILED_PATH) 1)
    (do
      (require :packer_compiled)
      ;; REF: folke/dot
      ;; No need to load this immediately, since we have packer_compiled
      (vim.defer_fn load-packer-plugins 0))
    (load-packer-plugins))
