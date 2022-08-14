(module core.funs
  {autoload {{:operator {: add : sub}
              :length length_
              : for_each
              : foldl} aniseed.deps.fun}})


(def count length_)

(defn inc [n]
  "Increment n by 1."
  (add n 1))

(defn dec [n]
  "Decrement n by 1."
  (sub n 1))

(defn contains? [xs target]
  (var seen? false)
  (for_each #(when (= $ target)
               (set seen? true))
            xs)
  seen?)

(def reduce foldl)

(def map map)

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
