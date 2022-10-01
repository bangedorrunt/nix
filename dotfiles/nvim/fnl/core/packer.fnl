(import-macros {: lazyfunc : lazyreq : setup! : before! : after! : set!} :core.macros)
(local packer (lazyreq :packer))
(local {: kvize : reduce : run!} (lazyfunc :core.funs))

(fn use [[name & args]]
  "feed a plugin to the plugin manager"
  (if (= 0 (length args))
    (packer.use name)
    (->> {1 name}
         (kvize args)
         packer.use)))

(fn plugin-present? [plug]
  "check if a plugin or list of plugins is on your system"
  (if (= :table (type plug))
    (reduce #(and $1 (plugin-present? $2)) true plug)
    (. packer_plugins plug)))

;; State
(var callbacks-for-after [])
(var callbacks-for-before [])

(fn before [plug callback]
  (table.insert callbacks-for-before [plug callback]))

(fn after [plug callback]
  (table.insert callbacks-for-after [plug callback]))

(fn run-callbacks [plugins]
  (each [_ [plug callback] (ipairs plugins)]
    (when (plugin-present? plug)
      (callback))))

(fn file-exist? [path]
  (= (vim.fn.filereadable path) 1))

(fn load-packer-plugins [plugins]
  ;; Load packer
  (vim.cmd.packadd :packer.nvim)
  (packer.init {:compile_path bangedorrunt.paths.PACKER-COMPILED-PATH
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
  (run! use plugins)
  )

(fn setup [plugins]

 ;; Re-enable syntax
 (vim.cmd "syntax on")
 (vim.cmd "filetype on")
 (vim.cmd "filetype plugin indent on")
 (set! shadafile "")
 (vim.cmd "rshada!")

  (if (file-exist? bangedorrunt.paths.PACKER-COMPILED-PATH)
    (do
      (require :packer_compiled)
      (run-callbacks callbacks-for-before)
      (run-callbacks callbacks-for-after)
      ;; REF: folke/dot
      ;; No need to load this immediately, since we have packer_compiled
      (vim.defer_fn (fn [] (load-packer-plugins plugins)) 0))
    (load-packer-plugins plugins))
  )

{: setup
 : use
 : before
 : after}
