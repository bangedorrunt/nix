(import-macros {: lazyfunc} :core.macros)

;; Automatically compile on the fly
;; NOTE: don't need this if using Aniseed
(let [{: build} (lazyfunc :hotpot.api.make)]
  (build "~/.config/nvim"
         {:verbosity 0}
         ;; ~/.config/nvim/fnl/*.fnl -> ~/.config/nvim/lua/*.lua
         "(.+)/fnl/(.+)"
         (fn [root path {: join-path}]
           (if (not (string.match path "macros%.fnl$"))
               (join-path root :lua path)))))

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
