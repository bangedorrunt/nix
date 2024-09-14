(import-macros {: setup!} :core.macros)

(fn kvize [xs t]
  "Convert sequence [:a :b] to table {:a :b}"
  (match xs
    [k v] (kvize
            (doto xs (table.remove 1) (table.remove 1))
            (match k
              :mod (doto t (tset :config #((. (require (.. :mod. v)) :setup))))
              _ (doto t (tset k v))))
    _ t))
(fn lazy-parser []
  "Feed a plugin to the plugin manager"
  (let [{: run} (require :core.funs)]
    (run
      (fn [[plug & args]]
        (match #args
          0 (table.insert store.plugins plug)
          _ (table.insert store.plugins (kvize args {1 plug}))))
      store.lazyadd)))


(fn setup []
  (let [lazy (require :lazy)
        opts {:defaults {:lazy true}
              :performance {:rtp {:paths [:store.paths.treesitter]}}}]
    (setup! mod)
    (lazy-parser)
    (lazy.setup store.plugins opts)))

{: setup}
