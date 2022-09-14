(local {:operator {: add : sub : concat}
        :length count
        :tomap totable
        :totable tosequence
        :any any?
        :every every?
        : for_each
        : head
        : nth
        : map
        : reduce
        : filter
        : chain} (require :luafun.fun))

(fn inc [n]
  "Increment n by 1."
  (add n 1))

(fn dec [n]
  "Decrement n by 1."
  (sub n 1))

(local first head)

(fn second [xs]
  (nth 2 xs))

(fn last [xs]
  (nth (count xs) xs))

(fn llast [xs]
  (nth (dec (count xs)) xs))

(fn empty? [xs]
  (= 0 (count xs)))

(fn has? [xs v]
  (any? (fn [x]
          (= x v)) xs))

(fn nil? [x]
  "True if the value is equal to Lua `nil`."
  (= nil x))

(fn number? [x]
  "True if the value is of type 'number'."
  (= :number (type x)))

(fn boolean? [x]
  "True if the value is of type 'boolean'."
  (= :boolean (type x)))

(fn string? [x]
  "True if the value is of type 'string'."
  (= :string (type x)))

(fn table? [x]
  "True if the value is of type 'table'."
  (= :table (type x)))

(fn keys [t]
  "Get all keys of a table."
  (let [result []]
    (when t
      (for_each (fn [k _]
                  (table.insert result k)) t))
    result))

(fn vals [t]
  "Get all values of a table."
  (let [result []]
    (when t
      (for_each (fn [_ v]
                  (table.insert result v)) t))
    result))

(fn kv-pairs [t]
  "Get all values of a table."
  (let [result []]
    (when t
      (for_each (fn [k v]
                  (table.insert result [k v])) t))
    result))

(fn map-indexed [f xs]
  "Map xs to a new sequential table by calling (f [k v]) on each item. "
  (map f (kv-pairs xs)))

(fn run! [f xs]
  "Calls `f` on each item in iterable."
  (reduce (fn [_ ...]
            (f ...)
            nil) nil xs))

(fn merge [base ...]
  (reduce (fn [acc x]
            (for_each (fn [k v]
                        (tset acc k v)) x)
            acc) (or base {}) [...]))

(fn get [t k d]
  (let [res (when (table? t)
              (let [val (. t k)]
                (when (not (nil? val))
                  val)))]
    (if (nil? res)
        d
        res)))

(fn into [tbl ...]
  "Adds any number of key/value pairs to `tbl`, returning `tbl`. Like [[tset]]
  but for multiple pairs."
  (for [i 1 (select "#" ...) 2]
    (let [(k v) (select i ...)]
      (tset tbl k v)))
  tbl)

(fn concat+ [...]
  "Concatenates the sequential table arguments together."
  (let [result []]
    (run! (fn [xs]
            (run! (fn [x]
                    (table.insert result x)) xs)) [...])
    result))

(fn mapcat+ [f xs]
  (concat+ (unpack (map f xs))))

(fn pr-str [...]
  (let [{: view} (require :fennel)
        s (table.concat (map (fn [x]
                               (view.serialise x {:one-line true}))
                             [...]) " ")]
    (if (or (nil? s) (= "" s)) :nil s)))

(fn str [...]
  (->> [...]
       (map (fn [s]
              (if (string? s)
                  s
                  (pr-str s))))
       (reduce (fn [acc s]
                 (.. acc s)) "")))

(fn println [...]
  (->> [...]
       (map (fn [s]
              (if (string? s)
                  s
                  (pr-str s))))
       (map-indexed (fn [[i s]]
                      (if (= 1 i)
                          s
                          (.. " " s))))
       (reduce (fn [acc s]
                 (.. acc s)) "")
       print))

(fn pr [...]
  (println (pr-str ...)))

;;;; String
(fn join [...]
  "(join xs) (join sep xs)
  Joins all items of a table together with an optional separator.
  Separator defaults to an empty string.
  Values that aren't a string or nil will go through aniseed.core/pr-str."
  (let [args [...]
        [sep xs] (if (= 2 (count args))
                     args
                     ["" (first args)])
        len (count xs)]
    (var result [])
    (when (> len 0)
      (for [i 1 len]
        (let [x (. xs i)]
          (-?>> (if (= :string (type x)) x
                    (= nil x) x
                    (pr-str x))
                (table.insert result)))))
    (table.concat result sep)))

(fn split [s pat]
  "Split the given string into a sequential table using the pattern."
  (var done? false)
  (var acc [])
  (var index 1)
  (while (not done?)
    (let [(start end) (string.find s pat index)]
      (if (= :nil (type start))
          (do
            (table.insert acc (string.sub s index))
            (set done? true))
          (do
            (table.insert acc (string.sub s index (dec start)))
            (set index (inc end))))))
  acc)

(fn blank? [s]
  "Check if the string is nil, empty or only whitespace."
  (or (empty? s) (not (string.find s "[^%s]"))))

(fn triml [s]
  "Removes whitespace from the left side of string."
  (string.gsub s "^%s*(.-)" "%1"))

(fn trimr [s]
  "Removes whitespace from the right side of string."
  (string.gsub s "(.-)%s*$" "%1"))

(fn trim [s]
  "Removes whitespace from both ends of string."
  (string.gsub s "^%s*(.-)%s*$" "%1"))

{: count
 : inc
 : dec
 : first
 : second
 : nth
 : last
 : llast
 : empty?
 : any?
 : every?
 : has?
 : nil?
 : number?
 : boolean?
 : string?
 : table?
 : keys
 : vals
 : kv-pairs
 : totable
 : tosequence
 : for_each
 : map
 : reduce
 : filter
 : chain
 : run!
 : merge
 : into
 : get
 : concat+
 : mapcat+
 : join
 : concat
 : split
 : blank?
 : triml
 : trimr
 : trim}
