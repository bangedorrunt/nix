(import-macros {: setup!} :core.macros)
(local packer (require :packer))
(local {: run} (require :core.funs))

(fn kvize [xs t]
  (match xs
    [k v] (kvize (doto xs (table.remove 1) (table.remove 1))
                 (match k
                   :init (doto t (tset :config (.. "require('" v "').setup()")))
                   :init+ (doto t (tset :config (.. "require('mod." v "').setup()")))
                   _ (doto t (tset k v))))
    _ t))

(fn use [[plug & args]]
  "feed a plugin to the plugin manager"
  (match #args
    0 (packer.use plug)
    _ (->> {1 plug}
           (kvize args)
           packer.use)))

(fn file-exist? [path]
  (= (vim.fn.filereadable path) 1))

(fn load-packer-plugins []
  ;; Load packer
  (vim.cmd.packadd :packer.nvim)
  (packer.init {:compile_path store.paths.packer-compiled
                :display {:compact true
                          :working_sym ""
                          :error_sym ""
                          :done_sym ""
                          :removed_sym ""
                          :moved_sym ""}
                :opt_default true
                :auto_reload_compiled false
                :preview_updates true
                :git {:clone_timeout 180 :depth 1}
                :max_jobs 60
                :profile {:enable true :threshold 0}})
  (packer.reset)
  (run use store.plugins))

(fn setup []
  (when (file-exist? store.paths.packer-compiled)
    (require :packer_compiled))
  (vim.defer_fn
    (fn []
      (setup! mod)
      (vim.api.nvim_exec_autocmds :User {:pattern :PackerDefered})
      (load-packer-plugins))
    100))

{: setup}
