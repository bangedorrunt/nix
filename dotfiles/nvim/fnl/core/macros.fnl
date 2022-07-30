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

(fn nil? [x]
  "True if the value is equal to Lua `nil`."
  (= nil x))

(fn ->str [x]
  "Convert a symbol to a string"
  (tostring x))

(fn ->bool [x]
  (if x true false))

(fn str->seq [s]
  "Convert an string into a sequence of characters."
  (icollect [c (string.gmatch s ".")]
    c))

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

;;;; Macros Declaration
; NOTE: https://github.com/rktjmp/hotpot.nvim/discussions/6
(fn pug [val prefix?]
  "Put Unique Global
  (val :any prefix? :string) -> (uuid :string)
  Takes any given value, generates a unique name (with optional prefix)
  and inserts value into _G. Returns unique name."
  (let [inter-compile-uid (os.date "%s")
        name (if prefix?
                 (.. (->str (gensym prefix?)) inter-compile-uid)
                 (.. (->str (gensym :pug)) inter-compile-uid))]
    `(do
       (tset _G ,name ,val)
       ,name)))

(fn vlua [what prefix?]
  "Wrap given in v:lua x pug call"
  `(.. "v:lua." ,(pug what prefix?) "()"))

(fn vlua->fn? [expr]
  ; WARNING: only use with anonymous function
  "Evaluate expr
  If expr is a function, wrap expr with `vlua`"
  (if (quoted? expr)
    (values :call (vlua (quoted->fn expr)))
    expr))

(fn viml->fn [name]
  ; WARNING: use with named function
  "Wrap given function in lua call"
  `(.. "lua require('" *module-name* "')['" ,(->str name) "']()"))

(fn viml->keymap [name]
  "Wrap given function in lua call which will be used for keybinding"
  `(.. "<Cmd>lua require('" *module-name* "')['" ,(->str name) "']()<CR>"))

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

(fn augroup [name ...]
  "Defines an autocommand group"
  `(do
     (nvim.ex.augroup ,(->str name))
     (do
       ,...)
     (nvim.ex.augroup "END")))

(fn autocmd! [...]
  "Defines an autocommand!"
  (match (select "#" ...)
    0 `(nvim.ex.autocmd_)
    1 `(nvim.ex.autocmd_ ,(->str ...))
    2 (let [(x y) ...] `(nvim.ex.autocmd_ ,(->str x) ,(->str y)))))

(fn autocmd [events pattern command]
  "Defines an autocommand"
    `(nvim.ex.autocmd ,(->str events) ,(->str pattern) ,(vlua->fn? command)))

(fn nmap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API"
  ; NOTE: If the `rhs` argument is a function, unlike old API, it's not neccessary
  ; to set `:expr` option.
  (fn ->opts [xs]
    "Returns a set following the structure of `{:key true}` from a sequence"
    (collect [_ v (ipairs xs)]
      (if (= :buffer v)
        (values v 0)
        (values v true))))

  (let [args [...]
        modes (-> modes
                  ->str
                  str->seq)
        rhs (last args)
        rhs (quoted->fn? rhs)
        lhs (llast args)
        args-index-without-rlhs (- (length args) 2)
        options (icollect [_ v (ipairs [(unpack args 1 args-index-without-rlhs)])]
                  (->str v))
        options (->opts options)]
    `(nvim.keymap.set ,modes ,lhs ,rhs ,options)))

(fn noremap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API
  If the `rhs` argument is a function then automatically includes the `:expr`
  option.
  Automatically includes the `:noremap` option."
  `(nmap ,modes :noremap ,...))

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

{: opt
 : opt-local
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
