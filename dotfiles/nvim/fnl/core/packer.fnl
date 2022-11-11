(local packer (require :packer))
(local {: kvize : run} (require :core.funs))
(local packer-compiled-path (.. (vim.fn.stdpath :data) :/site/lua/packer_compiled.lua))

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
    (packer.init {:compile_path packer-compiled-path
                  :display {:compact true
                            :working_sym ""
                            :error_sym ""
                            :done_sym ""
                            :removed_sym ""
                            :moved_sym ""
                            :header_sym "="}
                  :auto_reload_compiled false
                  :preview_updates true
                  :git {:clone_timeout 180 :depth 1}
                  :max_jobs 60
                  :profile {:enable true :threshold 0}})
    (packer.reset)
    (run use store.plugins))

(fn setup []
  (if (file-exist? packer-compiled-path)
    (do
      (require :packer_compiled)
      (vim.defer_fn load-packer-plugins 0))
    (load-packer-plugins)))

{: setup}
