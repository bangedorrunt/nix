(require :core.base)
(require :core.options)
(require :core.mappings)
(require :core.autocmds)

(fn file_exist? [path]
  (= (vim.fn.filereadable path) 1))

(fn load_packer_plugins []
  (vim.cmd.packadd "packer.nvim")
  (require :plugins))

;; Automatically compile on the fly
;; NOTE: don't need this if using Aniseed
(fn compile_aot []
  (let [{: build} (require :hotpot.api.make)]
    (build "~/.config/nvim"
           {:verbosity 0}
           ;; ~/.config/nvim/fnl/*.fnl -> ~/.config/nvim/lua/*.lua
           "(.+)/fnl/(.+)"
           (fn [root path {: join-path}] ;; root is the first match, path is the second
             ;; ignore our own macro file (init-macros.fnl is ignored by default)
             (if (not (string.match path "macros%.fnl$"))
                 ;; join-path automatically uses the os-appropriate path separator
                 (join-path root :lua path))))))

(if (file_exist? tdt.paths.PACKER_COMPILED_PATH)
    (do
      (require :packer_compiled)
      (compile_aot)
      ;; REF: folke/dot
      ;; No need to load this immediately, since we have packer_compiled
      (vim.defer_fn load_packer_plugins 0))
    (do
      (load_packer_plugins)
      (compile_aot)))
