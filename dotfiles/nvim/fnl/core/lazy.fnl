(import-macros {: setup!} :core.macros)
(local {: run} (require :core.funs))
(local packer (require :packer))
(local opts {:display {:compact true
                       :working_sym ""
                       :error_sym ""
                       :done_sym ""
                       :removed_sym ""
                       :moved_sym ""}
             ;; :opt_default true
             ;; :preview_updates true
             :git {:clone_timeout 180 :depth 1}
             :max_jobs 60})

(fn kvize [xs t]
  "Convert sequence [:a :b] to table {:a :b}"
  (match xs
    [k v] (kvize
            (doto xs (table.remove 1) (table.remove 1))
            (match k
              :init (doto t (tset :config #(: (require v) :setup)))
              :init+ (doto t (tset :config #(: (require (.. :mod. v)) :setup)))
              _ (doto t (tset k v))))
    _ t))
(fn packer-parser []
  "Feed a plugin to the plugin manager"
  (var res [])
  (run
    (fn [[plug & args]]
      (match #args
        0 (table.insert res plug)
        _ (->> {1 plug}
               (kvize args)
               (table.insert res))))
    store.plugins)
  res)

(fn load-packer-plugins []
  ;; Load packer
  (vim.cmd.packadd :packer.nvim)
  ;; NOTE: packer.startup will be deprecated
  ;; use packer.add, and packer.setup instead
  (packer.startup {1 (packer-parser) :config opts}))

(fn setup []
  (setup! mod)
  (load-packer-plugins)
  (vim.defer_fn #(vim.cmd.do "User PackerDefered") 100))

{: setup}
