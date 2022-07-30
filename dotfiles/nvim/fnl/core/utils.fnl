(module core.utils
  {autoload {view aniseed.view}})

;; Useful to have this set by someone.
(math.randomseed (os.time))

(defn rand [n]
  "Draw a random floating point number between 0 and `n`, where `n` is 1.0 if omitted."
  (* (math.random) (or n 1)))

(defn nil? [x]
  "True if the value is equal to Lua `nil`."
  (= nil x))

(defn number? [x]
  "True if the value is of type 'number'."
  (= "number" (type x)))

(defn boolean? [x]
  "True if the value is of type 'boolean'."
  (= "boolean" (type x)))

(defn string? [x]
  "True if the value is of type 'string'."
  (= "string" (type x)))

(defn table? [x]
  "True if the value is of type 'table'."
  (= "table" (type x)))

(defn function? [value]
  "True if the value is of type 'function'."
  (= "function" (type value)))

(defn keys [t]
  "Get all keys of a table."
  (let [result []]
    (when t
      (each [k _ (pairs t)]
        (table.insert result k)))
    result))

(defn count [xs]
  (if
    (table? xs) (let [maxn (table.maxn xs)]
                  ;; We only count the keys if maxn returns 0.
                  (if (= 0 maxn)
                    (table.maxn (keys xs))
                    maxn))
    (not xs) 0
    (length xs)))

(defn inc [n]
  "Increment n by 1."
  (+ n 1))

(defn dec [n]
  "Decrement n by 1."
  (- n 1))

(defn empty? [xs]
  (= 0 (count xs)))

(defn first [xs]
  (when xs
    (. xs 1)))

(defn second [xs]
  (when xs
    (. xs 2)))

(defn last [xs]
  (when xs
    (. xs (count xs))))

(defn last [xs]
  (when xs
    (. xs (length xs))))

(defn llast [xs]
  (when xs
    (. xs (dec (length xs)))))

(defn even? [n]
  (= (% n 2) 0))

(defn odd? [n]
  (not (even? n)))

(defn vals [t]
  "Get all values of a table."
  (let [result []]
    (when t
      (each [_ v (pairs t)]
        (table.insert result v)))
    result))

(defn kv-pairs [t]
  "Get all keys and values of a table zipped up in pairs."
  (let [result []]
    (when t
      (each [k v (pairs t)]
        (table.insert result [k v])))
    result))

(defn run! [f xs]
  "Execute the function (for side effects) for every xs."
  (when xs
    (let [nxs (count xs)]
      (when (> nxs 0)
        (for [i 1 nxs]
          (f (. xs i)))))))

(defn filter [f xs]
  "Filter xs down to a new sequential table containing every value that (f x) returned true for."
  (let [result []]
    (run!
      (fn [x]
        (when (f x)
          (table.insert result x)))
      xs)
    result))

(defn map [f xs]
  "Map xs to a new sequential table by calling (f x) on each item."
  (let [result []]
    (run!
      (fn [x]
        (let [mapped (f x)]
          (table.insert
            result
            (if (= 0 (select "#" mapped))
              nil
              mapped))))
      xs)
    result))

(defn map-indexed [f xs]
  "Map xs to a new sequential table by calling (f [k v]) on each item. "
  (map f (kv-pairs xs)))

(defn identity [x]
  "Returns what you pass it."
  x)

(defn reduce [f init xs]
  "Reduce xs into a result by passing each subsequent value into the fn with
  the previous value as the first arg. Starting with init."
  (var result init)
  (run!
    (fn [x]
      (set result (f result x)))
    xs)
  result)

(defn some [f xs]
  "Return the first truthy result from (f x) or nil."
  (var result nil)
  (var n 1)
  (while (and (nil? result) (<= n (count xs)))
    (let [candidate (f (. xs n))]
      (when candidate
        (set result candidate))
      (set n (inc n))))
  result)

(defn butlast [xs]
  (let [total (count xs)]
    (->> (kv-pairs xs)
         (filter
           (fn [[n v]]
             (not= n total)))
         (map second))))

(defn rest [xs]
  (->> (kv-pairs xs)
       (filter
         (fn [[n v]]
           (not= n 1)))
       (map second)))

(defn concat [...]
  "Concatenates the sequential table arguments together."
  (let [result []]
    (run! (fn [xs]
            (run!
              (fn [x]
                (table.insert result x))
              xs))
      [...])
    result))

(defn mapcat [f xs]
  (concat (unpack (map f xs))))

(defn any? [pred xs]
  (accumulate [any? false
               _ v (ipairs xs)
               :until any?]
    (pred v)))

(defn all? [pred xs]
  (not (any? #(not (pred $)) xs)))

(defn contains? [xs x]
  (any? #(= $ x) xs))

(defn pr-str [...]
  (let [s (table.concat
            (map (fn [x]
                   (view.serialise x {:one-line true}))
                 [...])
            " ")]
    (if (or (nil? s) (= "" s))
      "nil"
      s)))

(defn str [...]
  (->> [...]
       (map
         (fn [s]
           (if (string? s)
             s
             (pr-str s))))
       (reduce
         (fn [acc s]
           (.. acc s))
         "")))

(defn println [...]
  (->> [...]
       (map
         (fn [s]
           (if (string? s)
             s
             (pr-str s))))
       (map-indexed
         (fn [[i s]]
           (if (= 1 i)
             s
             (.. " " s))))
       (reduce
         (fn [acc s]
           (.. acc s))
         "")
       print))

(defn pr [...]
  (println (pr-str ...)))

(defn slurp [path silent?]
  "Read the file into a string."
  (match (io.open path "r")
    (nil msg) nil
    f (let [content (f:read "*all")]
        (f:close)
        content)))

(defn spit [path content]
  "Spit the string into the file."
  (match (io.open path "w")
    (nil msg) (error (.. "Could not open file: " msg))
    f (do
        (f:write content)
        (f:close)
        nil)))

(defn merge! [base ...]
  (reduce
    (fn [acc m]
      (when m
        (each [k v (pairs m)]
          (tset acc k v)))
      acc)
    (or base {})
    [...]))

(defn merge [...]
  (merge! {} ...))

(defn select-keys [t ks]
  (if (and t ks)
    (reduce
      (fn [acc k]
        (when k
          (tset acc k (. t k)))
        acc)
      {}
      ks)
    {}))

(defn get [t k d]
  (let [res (when (table? t)
              (let [val (. t k)]
                (when (not (nil? val))
                  val)))]
    (if (nil? res)
      d
      res)))

(defn get-in [t ks d]
  (let [res (reduce
              (fn [acc k]
                (when (table? acc)
                  (get acc k)))
              t ks)]
    (if (nil? res)
      d
      res)))

(defn assoc [t ...]
  (let [[k v & xs] [...]
        rem (count xs)
        t (or t {})]

    (when (odd? rem)
      (error "assoc expects even number of arguments after table, found odd number"))

    (when (not (nil? k))
      (tset t k v))

    (when (> rem 0)
      (assoc t (unpack xs)))

    t))

(defn assoc-in [t ks v]
  (let [path (butlast ks)
        final (last ks)
        t (or t {})]
    (assoc (reduce
             (fn [acc k]
               (let [step (get acc k)]
                 (if (nil? step)
                   (get (assoc acc k {}) k)
                   step)))
             t
             path)
           final
           v)
    t))

(defn update [t k f]
  (assoc t k (f (get t k))))

(defn update-in [t ks f]
  (assoc-in t ks (f (get-in t ks))))

(defn constantly [v]
  (fn [] v))

;;;; String
(defn join [...]
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
          (-?>> (if
                  (= :string (type x)) x
                  (= nil x) x
                  (pr-str x))
                (table.insert result)))))

    (table.concat result sep)))

(defn split [s pat]
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
          (table.insert acc (string.sub s index (- start 1)))
          (set index (+ end 1))))))
  acc)

(defn blank? [s]
  "Check if the string is nil, empty or only whitespace."
  (or (a.empty? s)
      (not (string.find s "[^%s]"))))

(defn triml [s]
  "Removes whitespace from the left side of string."
  (string.gsub s "^%s*(.-)" "%1"))

(defn trimr [s]
  "Removes whitespace from the right side of string."
  (string.gsub s "(.-)%s*$" "%1"))

(defn trim [s]
  "Removes whitespace from both ends of string."
  (string.gsub s "^%s*(.-)%s*$" "%1"))
