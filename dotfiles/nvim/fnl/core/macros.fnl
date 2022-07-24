;;;; Helper Functions Declaration
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
  (when xs
    (. xs (dec (length xs)))))

(fn mapt [f tbl]
  "Maps `f` over `tbl` in place."
  (for [i 1 (length tbl)]
    (tset tbl i (f (. tbl i))))
  tbl)

(fn ->str [x]
  "Convert a symbol to a string"
  (tostring x))

(fn ->bool [x]
  (if x true false))

(fn str->seq [s]
  "Convert an string into a sequence of characters."
  (icollect [c (string.gmatch s ".")]
    c))

(fn ->key-opts [xs]
  "Returns a set following the structure of `{:key true}` from a sequence"
  ; (reduce #(merge $1 {$2 true}) {} xs))
  (collect [_ v (ipairs xs)]
    (values v true)))

(fn expr->str [expr]
  `(macrodebug ,expr nil))

(fn nil? [x]
  (= nil x))

(fn str? [x]
  (= :string (type x)))

(fn begins-with? [chars str]
  "Return whether str begins with chars."
  (->bool (str:match (.. "^" chars))))

(fn num? [x]
  (= :number (type x)))

(fn bool? [x]
  (= :boolean (type x)))

(fn tbl? [x]
  (= :table (type x)))

(fn fn? [x]
  "Checks if `x` is a function definition.
  Cannot check if a symbol is a function in compile time."
  (and (list? x)
       (or (= 'fn (first x))
           (= 'hashfn (first x))
           (= 'lambda (first x))
           (= 'partial (first x)))))

(fn quoted? [x]
  "Check if `x` is a list that begins with `quote`."
  (and (list? x)
       (= 'quote (first x))))

(fn quoted->fn [expr]
  "Converts an expression like `(quote (+ 1 1))` into `(fn [] (+ 1 1))`."
  (let [non-quoted (second expr)]
    `(fn [] ,non-quoted)))

(fn quoted->fn? [expr]
  (if (quoted? expr) (quoted->fn expr) expr))

(fn quoted->str [expr]
  "Converts a quoted expression like `(quote (+ 1 1))` into an string with its shorthand form."
  (let [non-quoted (second expr)]
    (.. "'" (view non-quoted))))

(fn expand-exprs [exprs]
  "Converts a list of expressions into either an expression - if only one
  expression is in the list - or a do-expression containing the expressions."
  (if (> (length exprs) 1)
    `(do
       ,(unpack exprs))
    (first exprs)))

(fn any? [pred xs]
  (accumulate [any? false
               _ v (ipairs xs)
               :until any?]
    (pred v)))

(fn all? [pred xs]
  (not (any? #(not (pred $)) xs)))

(fn contains? [xs x]
  (any? #(= $ x) xs))

(fn cons [x xs]
  "Returns a sequence with the object appended as the first element of the sequence"
  [x (unpack xs)])

(fn conj [tbl ...]
  "Appends all values to `tbl`, returning `tbl`."
  (var end (length tbl)) ; a bit faster than table.insert
  (for [i 1 (select "#" ...)]
    (match (select i ...)
      x (do
          (set end (inc end))
          (tset tbl end x))))
  tbl)

(fn disj [tbl ...]
  "Removes any number of keys from `tbl`, returning `tbl`."
  (for [i 1 (select "#" ...)]
    (let [k (select i ...)]
      (tset tbl k nil)))
  tbl)

;;;; Macros Declaration

(fn command [...]
  "Define command"
  (match (select "#" ...)
    2 (let [(name expr) ...] `(nvim.ex.command_ ,(->str name) ,(quoted->fn? expr) {}))
    3 (let [(name _ expr) ...] `(nvim.ex.buf_command_ 0 ,(->str name) ,(quoted->fn? expr) {}))))

(fn opt [name ?value]
  "Set one vim.options using the `vim.opt` API
  The option name must be a symbol
  If the option doesn't have a corresponding value, if it begins with no the
  value becomes false, and if it doesn't it becomes true"
  (let [name (->str name)
        value (if (nil? ?value) (not (name:match :^no)) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (match (name:sub -1)
      "+" `(: (. nvim.opt ,(name:sub 1 -2)) :append ,value)
      "-" `(: (. nvim.opt ,(name:sub 1 -2)) :remove ,value)
      "^" `(: (. nvim.opt ,(name:sub 1 -2)) :prepend ,value)
      _ `(tset nvim.opt ,name ,value))))

(fn opt-local [name ?value]
  "Set a local vim.option using the `vim.opt_local` API"
  (let [name (->str name)
        value (if (nil? ?value) (not (name:match :^no)) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (match (name:sub -1)
      "+" `(: (. nvim.opt_local ,(name:sub 1 -2)) :append ,value)
      "-" `(: (. nvim.opt_local ,(name:sub 1 -2)) :remove ,value)
      "^" `(: (. nvim.opt_local ,(name:sub 1 -2)) :prepend ,value)
      _ `(tset nvim.opt_local ,name ,value))))

(fn g [name value]
  "Set value for global Vim variable"
  (let [name (->str name)]
    `(tset nvim.g ,name ,value)))

(fn autocmd [event pattern command ?options]
  "Create an autocommand using the nvim_create_autocmd API."
  (let [event (if (and (tbl? event) (not (sym? event)))
                (icollect [_ v (ipairs event)] (->str v))
                (->str event))
        pattern (if (and (tbl? pattern) (not (sym? pattern)))
                  (icollect [_ v (ipairs pattern)] (->str v))
                  (->str pattern))
        options (or ?options {})
        options (if (nil? options.buffer)
                  (if (= "<buffer>" pattern)
                    (doto options (tset :buffer 0))
                    (doto options (tset :pattern pattern)))
                  options)
        options (if (str? command)
                  (doto options (tset :command command))
                  (doto options (tset :callback (quoted->fn? command))))
        options (if (nil? options.desc)
                  (doto options (tset :desc (if (quoted? command) (quoted->str command)
                                              (str? command) command
                                              (view command))))
                  options)]
    `(nvim.ex.autocmd ,event ,options)))

(fn autocmd! [name ?options]
  "Defines an autocommand!"
  (let [name (->str name)
        options (or ?options {})
        options (doto options (tset :group name))]
    `(nvim.ex.autocmd_ ,options)))


(fn augroup [name ...]
  "Defines an autocommand group"
  (expand-exprs
    (let [name (->str name)]
      (icollect [_ expr (ipairs [...])
                 :into [`(nvim.ex.augroup ,name {:clear false})]]
        (if (= 'autocmd (first expr))
          (let [[_ event pattern command ?options] expr
                options (or ?options {})
                options (doto options (tset :group name))]
            `(autocmd ,event ,pattern ,command ,options))
          (let [[_ ?options] expr]
            `(autocmd! ,name ,?options)))))))

(fn map [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API
  If the `rhs` argument is a function then automatically includes the `:expr`
  option."
  (fn bind-to [mode lhs rhs options]
    `(nvim.keymap.set ,mode ,lhs ,rhs ,options))

  (let [args [...]
        modes (-> modes
                  ->str
                  str->seq)
        rhs (last args)
        rhs (quoted->fn? rhs)
        lhs (llast args)
        args-index-without-rlhs (- (length args) 2)
        options (mapt ->str [(unpack args 1 args-index-without-rlhs)])
        options (if (quoted? rhs)
                    (conj options :expr)
                    options)
        options (->key-opts options)
        exprs (mapt #(bind-to $ lhs rhs options) modes)]
    (if (> (length exprs) 1)
        `(do
           ,(unpack exprs))
        (unpack exprs))))

(fn noremap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API
  If the `rhs` argument is a function then automatically includes the `:expr`
  option.
  Automatically includes the `:noremap` option."
  `(map ,modes :noremap ,...))

(fn hi [name colors]
    `(nvim.ex.highlight 0 ,(->str name) ,colors))

(fn colorscheme [name]
  "Sets a vim colorscheme."
  `(nvim.ex.colorscheme ,(->str name)))

(fn t [key]
  "Returns the string with termcodes replaced"
  `(nvim.replace_termcodes ,(->str key) true true true))

(fn feedkeys [key]
  "Sends input-keys to Nvim, subject to various quirks
  controlled by `mode` flags."
  `(nvim.feedkeys ,(t key) :n true))

(fn has? [property]
  "Returns true if vim has a propety"
  `(match (nvim.fn.has ,property)
     1 true
     0 false
     _# nil))

{: any?
 : contains?
 : opt
 : opt-local
 : g
 : command
 : autocmd
 : autocmd!
 : augroup
 : map
 : noremap
 : hi
 : colorscheme
 : t
 : feedkeys
 : has?}
