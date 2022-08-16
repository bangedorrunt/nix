(local {:operator {: add : sub}
        : for_each
        : head
        : map
        : nth
        : reduce
        :tomap totbl
        :totable toseq
        :length count} (require :aniseed.deps.fun))

(fn inc [n]
  "Increment n by 1."
  (add n 1))

(fn dec [n]
  "Decrement n by 1."
  (sub n 1))

(fn first [xs]
  (head xs))

(fn second [xs]
  (nth 2 xs))

(fn last [xs]
  (nth (count xs) xs))

(fn llast [xs]
  (nth (dec (count xs)) xs))

(fn nil? [x]
  "True if the value is equal to Lua `nil`."
  (= nil x))

(fn str? [x]
  (= :string (type x)))

(fn num? [x]
  (= :number (type x)))

(fn bool? [x]
  (= :boolean (type x)))

(fn fn? [x]
  "Checks if `x` is a function definition.
  Cannot check if a symbol is a function in compile time."
  (and (list? x)
       (or (= 'fn (first x))
           (= 'hashfn (first x))
           (= 'lambda (first x))
           (= 'partial (first x)))))

(fn tbl? [x]
  (= :table (type x)))

(fn quoted? [x]
  "Check if `x` is a list that begins with `quote`."
  (and (list? x)
       (= 'quote (first x))))

(fn quoted->fn [expr]
  "Converts an expression like `(quote (+ 1 1))` into `(fn [] (+ 1 1))`."
  (assert-compile (quoted? expr) "expected quoted expression for expr" expr)
  (let [non-quoted (second expr)]
    `(fn [] ,non-quoted)))

(fn quoted->fn? [expr]
  (if (quoted? expr) (quoted->fn expr) expr))

;;;; Macros Declaration
;; SEE: https://github.com/rktjmp/hotpot.nvim/discussions/6
(fn pug [val prefix?]
  "Put Unique Global
  (val :any prefix? :string) -> (uuid :string)
  Takes any given value, generates a unique name (with optional prefix)
  and inserts value into _G. Returns unique name."
  (let [inter-compile-uid (os.date "%s")
        name (if prefix?
               (.. (tostring (gensym prefix?)) inter-compile-uid)
               (.. (tostring (gensym :pug)) inter-compile-uid))]
    `(do
       (tset _G ,name ,val)
       ,name)))

(fn vlua [what prefix?]
  "Wrap given in v:lua x pug call"
  `(.. "v:lua." ,(pug what prefix?) "()"))

(fn vlua->fn? [expr]
  ;; NOTE: only use with anonymous function
  "Evaluate expr
  If expr is a function, wrap expr with `vlua`"
  (if (quoted? expr)
    (values :call (vlua (quoted->fn expr)))
    expr))

(fn command [...]
  "Define command"
  (match (select "#" ...)
    1 (error "expected 2 or 3 arguments")
    2 (let [(name expr) ...] `(vim.api.nvim_create_user_command ,(tostring name) ,(quoted->fn? expr) {}))
    3 (let [(name _ expr) ...] `(vim.api.nvim_buf_create_user_command 0 ,(tostring name) ,(quoted->fn? expr) {}))
    _ (error "expected 2 or 3 arguments only")))

(fn opt [name ?value]
  "Set one vim.options using the `vim.opt` API
  The option name must be a symbol
  If the option doesn't have a corresponding value, if it begins with no the
  value becomes false, and if it doesn't it becomes true"
  (assert-compile (or (sym? name) (str? name)) "expected symbol for name" name)
  (let [name (tostring name)
        value (if (nil? ?value) (not (name:match :^no)) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (match (name:sub -1)
      "+" `(: (. vim.opt ,(name:sub 1 -2)) :append ,value)
      "-" `(: (. vim.opt ,(name:sub 1 -2)) :remove ,value)
      "^" `(: (. vim.opt ,(name:sub 1 -2)) :prepend ,value)
      _ `(tset vim.opt ,name ,value))))

(fn opt_local [name ?value]
  "Set a local vim.option using the `vim.opt_local` API"
  (assert-compile (or (sym? name) (str? name)) "expected symbol for name" name)
  (let [name (tostring name)
        value (if (nil? ?value) (not (name:match :^no)) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (match (name:sub -1)
      "+" `(: (. vim.opt_local ,(name:sub 1 -2)) :append ,value)
      "-" `(: (. vim.opt_local ,(name:sub 1 -2)) :remove ,value)
      "^" `(: (. vim.opt_local ,(name:sub 1 -2)) :prepend ,value)
      _ `(tset vim.opt_local ,name ,value))))

(fn g [name value]
  "Set value for global Vim variable"
  (assert-compile (or (sym? name) (str? name)) "expected symbol for name" name)
  (let [name (tostring name)]
    `(tset vim.g ,name ,value)))

(fn augroup [name ...]
  "Defines an autocommand group"
  (assert-compile (or (sym? name) (str? name)) "expected symbol for name" name)
  `(do
     (vim.cmd.augroup ,(tostring name))
     (do
       ,...)
     (vim.cmd.augroup "END")))

;; TODO: use new API but remain the same syntax
(fn autocmd! [...]
  "Defines an autocommand!"
  (match (select "#" ...)
    0 `(vim.cmd.autocmd {:bang true})
    1 `(vim.cmd.autocmd {:bang true :args ,(tostring ...)})
    2 (let [(x y) ...] `(vim.cmd.autocmd {:bang true :args [,(tostring x) ,(tostring y)]}))))

(fn autocmd [events pattern command]
  "Defines an autocommand"
  (assert-compile (or (sym? events) (str? events)) "expected symbol or string for events" events)
  (assert-compile (or (sym? pattern) (str? pattern)) "expected symbol or string for pattern" pattern)
  (assert-compile (or (sym? command) (str? command) (fn? command) (quoted? command))
                  "expected string, symbol, function or quoted expression for command" command)
  `(vim.cmd.autocmd {:args [,(tostring events) ,(tostring pattern) ,(vlua->fn? command)]}))

(fn nmap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API"
  ;; TODO: figure out why I cant use outer function in macro
  (assert-compile (sym? modes) "expected symbol for modes" modes)
  (let [args [...]
        modes (-> modes tostring toseq)
        rhs (-> args last quoted->fn?)
        lhs (llast args)
        options (->> [(unpack args 1 (- (count args) 2))]
                     (map tostring)
                     (map #(values $ true))
                     totbl)]
    (assert-compile (or (str? rhs) (sym? rhs) (fn? rhs) (quoted? rhs)) "expected string, symbol, function or quoted expression for rhs" rhs)
    (assert-compile (or (sym? lhs) (str? lhs)) "expected symbol or string for lhs" lhs)
    (assert-compile (or (nil? options) (tbl? options)) "expected table for options" options)
    `(vim.keymap.set ,modes ,lhs ,rhs ,options)))

(fn noremap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API
  Automatically includes the `:noremap` option."
  `(nmap ,modes :noremap ,...))

(fn hi [name colors]
  (assert-compile (or (sym? name) (str? name)) "expected symbol for name" name)
  `(vim.api.nvim_set_hl 0 ,(tostring name) ,colors))

(fn colorscheme [name]
  "Sets a vim colorscheme."
  (assert-compile (or (sym? name) (str? name)) "expected symbol for name" name)
  `(vim.cmd.colorscheme ,(tostring name)))

(fn t [key]
  "Returns the string with termcodes replaced"
  (assert-compile (or (sym? key) (str? key)) "expected symbol for key" key)
  `(vim.api.nvim_replace_termcodes ,(tostring key) true true true))

(fn feedkeys [key]
  "Sends input-keys to Nvim, subject to various quirks
  controlled by `mode` flags."
  (assert-compile (or (sym? key) (str? key)) "expected symbol for key" key)
  `(vim.api.nvim_feedkeys ,(t key) :n true))

(fn has? [property]
  "Returns true if vim has a propety"
  (assert-compile (or (sym? property) (str? property)) "expected symbol or string for property" property)
  `(match (vim.fn.has ,property)
     1 true
     0 false
     _# nil))

{: opt
 : opt_local
 : g
 : command
 : autocmd
 : autocmd!
 : augroup
 : nmap
 : noremap
 : hi
 : colorscheme
 : t
 : feedkeys
 : has?}
