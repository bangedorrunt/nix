(module core.fun
  {autoload {{:length length_
              : for_each
              : reduce} aniseed.deps.fun}})


(def count length_)

(defn contains? [xs target]
  (var seen? false)
  (for_each #(when (= $ target)
               (set seen? true))
            xs)
  seen?)

(defn run! [f xs]
  "Calls `f` on each item in iterable."
  (reduce
    (fn [_ ...] (f ...) nil)
    nil
    xs))

(defn merge [...]
  (reduce
    (fn [acc v]
      (for_each #(tset acc $1 $2) v)
      acc)
    {}
    [...]))

(defn assoc [tbl ...]
  "Adds any number of key/value pairs to `tbl`, returning `tbl`. Like [[tset]]
  but for multiple pairs."
  (for [i 1 (select "#" ...) 2]
    (let [(k v) (select i ...)]
      (tset tbl k v)))
  tbl)
