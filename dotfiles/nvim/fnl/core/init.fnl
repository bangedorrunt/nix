(module core.init
  {require [core.base
            core.options
            core.mappings
            core.autocmds]})

(defn- file-exist? [path]
  (= (vim.fn.filereadable path) 1))

(defn- load-packer-plugins []
       (vim.cmd.packadd "packer.nvim")
       (require :plugins))

(if (file-exist? tdt.paths.PACKER_COMPILED_PATH)
    (do
      (require :packer_compiled)
      ;; REF: folke/dot
      ;; No need to load this immediately, since we have packer_compiled
      (vim.defer_fn load-packer-plugins 0))
    (load-packer-plugins))
