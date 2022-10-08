(fn inc [n]
  "Increment n by 1."
  (+ n 1))

(fn dec [n]
  "Decrement n by 1."
  (- n 1))

(fn first [xs]
  (when xs
    (. xs 1)))

(fn second [xs]
  (when xs
    (. xs 2)))

(fn last [xs]
  (when xs
    (. xs (length xs))))

(fn llast [xs]
  (. xs (dec (length xs))))

(fn empty? [xs]
  (= 0 (length xs)))

(fn any? [pred xs]
  (accumulate [any? false _ v (ipairs xs) :until any?]
    (pred v)))

(fn every? [pred xs]
  (not (any? #(not (pred $)) xs)))

(fn has? [xs x]
  (any? #(= $ x) xs))

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
      (each [k _ (pairs t)]
        (table.insert result k)))
    result))

(fn vals [t]
  "Get all values of a table."
  (let [result []]
    (when t
      (each [_ v (pairs t)]
        (table.insert result v)))
    result))

(fn kv-pairs [t]
  "Get all values of a table."
  (let [result []]
    (when t
      (each [k v (pairs t)]
        (table.insert result [k v])))
    result))

(fn kvize [xs t]
  (match xs
    [k v] (kvize (doto xs (table.remove 1) (table.remove 1))
                 (doto t (tset k v)))
    _ t))

(fn map [f xs ?default]
  "Map xs to a new sequential table by calling (f x) on each item."
  (if (nil? ?default)
      (icollect [_ v (ipairs xs)]
        (f v))
      (icollect [_ v (ipairs xs) &into ?default]
        (f v))))

(fn reduce [f init xs]
  "Reduce xs into a result by passing each subsequent value into the fn with
  the previous value as the first arg. Starting with init."
  (accumulate [acc init _ v (ipairs xs)]
    (f acc v)))

;; fnlfmt: skip
(fn run! [f xs]
  "Calls `f` on each item in iterable."
  (reduce (fn [_ ...] (f ...) nil) nil xs))

(fn filter [f xs]
  "Filter xs down to a new sequential table containing every value that (f x) returned true for."
  (icollect [_ v (ipairs xs)]
    (when (f v)
      v)))

(fn map-indexed [f xs]
  "Map xs to a new sequential table by calling (f [k v]) on each item. "
  (map f (kv-pairs xs)))

(fn identity [x]
  "Returns what you pass it."
  x)

(fn some [f xs]
  "Return the first truthy result from (f x) or nil."
  (var result nil)
  (var n 1)
  (while (and (nil? result) (<= n (length xs)))
    (let [candidate (f (. xs n))]
      (when candidate
        (set result candidate))
      (set n (inc n))))
  result)

(fn butlast [xs]
  (let [total (length xs)]
    (->> (kv-pairs xs)
         (filter (fn [[n v]]
                   (not= n total)))
         (map second))))

(fn rest [xs]
  (->> (kv-pairs xs)
       (filter (fn [[n v]]
                 (not= n 1)))
       (map second)))

(fn concat+ [...]
  "Concatenates the sequential table arguments together."
  (accumulate [acc [] _ xs (ipairs [...])]
    (icollect [_ v (ipairs xs) :into acc] v)))

(fn mapcat+ [f xs]
  (concat+ (unpack (map f xs))))

(fn merge! [base ...]
  (reduce (fn [acc m]
            (when m
              (each [k v (pairs m)]
                (tset acc k v)))
            acc) (or base {}) [...]))

(fn merge [...]
  (merge! {} ...))

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
        [sep xs] (if (= 2 (length args))
                     args
                     ["" (first args)])
        len (length xs)]
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

{: inc
 : dec
 : first
 : second
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
 : kvize
 : map
 : reduce
 : filter
 : run!
 : merge
 : merge!
 : into
 : get
 : concat+
 : mapcat+
 : join
 : split
 : blank?
 : triml
 : trimr
 : trim}
