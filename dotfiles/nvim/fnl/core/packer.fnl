(import-macros {: lazyfunc : lazyreq} :core.macros)
(local packer (lazyreq :packer))
(local {: kvize : run : concat+} (lazyfunc :core.funs))

(fn use [[plug & args]]
  "feed a plugin to the plugin manager"
  (if (= 0 #args)
    (packer.use plug)
    (->> {1 plug}
         (kvize args)
         packer.use)))

(fn file-exist? [path]
  (= (vim.fn.filereadable path) 1))

(fn load-packer-plugins []
    ;; Load packer
    (vim.cmd.packadd :packer.nvim)
    (packer.init {:compile_path store.paths.PACKER-COMPILED-PATH
                  :display {:compact true
                            :working_sym ""
                            :error_sym ""
                            :done_sym ""
                            :removed_sym ""
                            :moved_sym ""
                            :header_sym ""}
                  :auto_reload_compiled true
                  :preview_updates false
                  :git {:clone_timeout 180 :depth 1}
                  :max_jobs 60
                  :profile {:enable true :threshold 0}})
    (packer.reset)
    (run use store.plugins))

(fn setup []

  (if (file-exist? store.paths.PACKER-COMPILED-PATH)
    (do
      (require :packer_compiled)
      (vim.defer_fn load-packer-plugins 0))
    (load-packer-plugins)))

{: setup
 : use}
