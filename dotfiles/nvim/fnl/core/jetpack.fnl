(local jetpack (require :jetpack))

(fn add-args [obj args]
  "turn an [a b c d] into an {a b c d}"
  (for [i 1 (length args) 2]
    (let [k (. args i)
          v (. args (+ i 1))]
      (tset obj k v)))
  obj)

(fn use [[name & args]]
  "feed a plugin to the plugin manager"
  (vim.fn.jetpack#add name (if (= 0 (length args))
                               (vim.empty_dict)
                               (add-args [] args))))

(fn plugin-present? [plug]
  "check if a plugin or list of plugins is on your system"
  (if (= :table (type plug))
      (accumulate [ready true i plug (ipairs plug)]
        (and ready (plugin-present? plug)))
      (jetpack.tap plug)))

;; state
(var callbacks-for-after [])
(var callbacks-for-before [])
(var bootstrapped-plugins [])

(fn before [plug callback]
  (table.insert callbacks-for-before [plug callback]))

(fn after [plug callback]
  (table.insert callbacks-for-after [plug callback]))

(fn run-callbacks [plugins]
  (each [_ [plug callback] (ipairs plugins)]
    (when (or (. bootstrapped-plugins plug) (plugin-present? plug))
      (callback))))

(fn set-bootstrapped-plugins [b]
  (set bootstrapped-plugins b))

(fn setup [plugins]
  ;; basic setup
  (vim.cmd "syntax on")
  (vim.cmd "filetype plugin on")
  (vim.cmd "set path=$PWD/**")
  (vim.fn.jetpack#begin)
  (each [_ plugin (pairs plugins)]
    (use plugin))
  (run-callbacks callbacks-for-before)
  (vim.fn.jetpack#end)
  (run-callbacks callbacks-for-after))

{: setup : before : after : set-bootstrapped-plugins}
