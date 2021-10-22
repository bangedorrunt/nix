;;;; Helper Functions Declaration
(fn nil? [x]
  (= nil x))

(fn inc [n]
  "Increment n by 1."
  (+ n 1))

(fn dec [n]
  "Decrement n by 1."
  (- n 1))

(fn first [xs]
  (when xs
    (. xs 1)))

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

(fn sym->str [x]
  "Convert a symbol to a string"
  (tostring x))

(fn str->seq [s]
  "Convert an string into a sequence of characters."
  (icollect [c (string.gmatch s ".")]
    c))

(fn ->key-opts [xs]
  "Returns a set following the structure of `{:key true}` from a sequence"
  ; (reduce #(merge $1 {$2 true}) {} xs))
  (collect [_ v (ipairs xs)] (values v true)))

(fn fn? [obj]
  "Returns true if the object is a function
  This only works at compilation time"
  (and (list? obj) (or (= (sym->str (first obj)) :hashfn)
                       (= (sym->str (first obj)) :fn))))

(fn contains? [tbl x]
  ; Checks if `x' is stored in `tbl' in linear time.
  (var res false)
  (each [i v (ipairs tbl)]
    (if (= v x)
        (do
          (set res i)
          (lua :break))))
  res)

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

; NOTE: https://github.com/rktjmp/hotpot.nvim/discussions/6
(fn pug [val prefix?]
  "Put Unique Global

  (val :any prefix? :string) -> (uuid :string)

  Takes any given value, generates a unique name (with optional prefix)
  and inserts value into _G. Returns unique name."
  (let [inter-compile-uid (os.date "%s")
        name (if prefix?
                 (.. (sym->str (gensym prefix?)) inter-compile-uid)
                 (.. (sym->str (gensym :pug)) inter-compile-uid))]
    `(do
       (tset _G ,name ,val)
       ,name)))

(fn vlua [what prefix?]
  "Wrap given in v:lua x pug call"
  `(.. "v:lua." ,(pug what prefix?) "()"))

(fn vlua-maybe [expr ?in-keymap]
  ; WARNING: only use with anonymous function
  "Evaluate expr
  If expr is a function, wrap expr with `vlua`"
  (if (fn? expr)
    (if (nil? ?in-keymap)
      (values "call" (vlua expr))
      (vlua expr))
    expr))

(fn viml->fn [name]
  ; WARNING: use with named function
  "Wrap given function in lua call"
  `(.. "lua require('" *module-name* "')['" ,(sym->str name) "']()"))

(fn viml->keymap [name]
  "Wrap given function in lua call which will be used for keybinding"
  `(.. "<Cmd>lua require('" *module-name* "')['" ,(sym->str name) "']()<CR>"))

(fn command [...]
  "Define command"
  (match (select "#" ...)
    2 (let [(name expr) ...] `(nvim.ex.command_ ,(sym->str name) ,(vlua-maybe expr)))
    3 (let [(name _ expr) ...] `(nvim.ex.command_ "-buffer" ,(sym->str name) ,(vlua-maybe expr)))))

(fn opt [name ?value]
  "Set one vim.options using the `vim.opt` API
  The option name must be a symbol
  If the option doesn't have a corresponding value, if it begins with no the
  value becomes false, and if it doesn't it becomes true"
  (let [name (sym->str name)
        value (if (nil? ?value) (not (name:match :^no)) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (match (name:sub -1)
      "+" `(: (. nvim.opt ,(name:sub 1 -2)) :append ,value)
      "-" `(: (. nvim.opt ,(name:sub 1 -2)) :remove ,value)
      "^" `(: (. nvim.opt ,(name:sub 1 -2)) :prepend ,value)
      _ `(tset nvim.opt ,name ,value))))

(fn opt-local [name ?value]
  "Set a local vim.option using the `vim.opt_local` API"
  (let [name (sym->str name)
        value (if (nil? ?value) (not (name:match :^no)) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (match (name:sub -1)
      "+" `(: (. nvim.opt_local ,(name:sub 1 -2)) :append ,value)
      "-" `(: (. nvim.opt_local ,(name:sub 1 -2)) :remove ,value)
      "^" `(: (. nvim.opt_local ,(name:sub 1 -2)) :prepend ,value)
      _ `(tset nvim.opt_local ,name ,value))))

(fn g [name value]
  "Set value for global Vim variable"
  (let [name (sym->str name)]
    `(tset nvim.g ,name ,value)))

(fn augroup [name ...]
  "Defines an autocommand group"
  `(do
     (nvim.ex.augroup ,(sym->str name))
     (do
       ,...)
     (nvim.ex.augroup "END")))

(fn autocmd! [...]
  "Defines an autocommand!"
  (match (select "#" ...)
    0 `(nvim.ex.autocmd_)
    1 `(nvim.ex.autocmd_ ,(sym->str ...))
    2 (let [(x y) ...] `(nvim.ex.autocmd_ ,(sym->str x) ,(sym->str y)))))

(fn autocmd [events pattern command]
  "Defines an autocommand"
    `(nvim.ex.autocmd ,(sym->str events) ,(sym->str pattern) ,(vlua-maybe command)))

(fn map [modes ...]
  "Defines a vim mapping using the `vim.api.nvim_set_keymap` API or the
  `vim.api.nvim_buf_set_keymap` if the option `:buffer` was passed.
  Support all the options the API supports
  If the `rhs` argument is a function then automatically includes the `:expr`
  option."
  (fn bind-to [mode lhs rhs options]
    (let [buffer? (?. options :buffer)
          options (disj options :buffer)]
      (if buffer?
          `(nvim.buf_set_keymap 0 ,mode ,lhs ,rhs ,options)
          `(nvim.set_keymap ,mode ,lhs ,rhs ,options))))
  (let [args [...]
        modes (-> modes
                  sym->str
                  str->seq)
        rhs (last args)
        lhs (llast args)
        args-index-without-rlhs (- (length args) 2)
        options (mapt sym->str [(unpack args 1 args-index-without-rlhs)])
        options (if (fn? rhs)
                    (conj options :expr)
                    options)
        options (->key-opts options)
        exprs (mapt #(bind-to $ lhs (vlua-maybe rhs :in-keymap) options) modes)]
    (if (> (length exprs) 1)
        `(do
           ,(unpack exprs))
        (unpack exprs))))

(fn noremap [modes ...]
  "Defines a vim mapping using the `vim.api.nvim_set_keymap` API or the
  `vim.api.nvim_buf_set_keymap` if the option `:buffer` was passed.
  Support all the options the API supports.
  If the `rhs` argument is a function then automatically includes the `:expr`
  option.
  Automatically includes the `:noremap` option."
    `(map ,modes :noremap ,...))

(fn hi [name opts]
  "Defines a highlight"
  (let [f (fn [k v]
            (match k
              :fg (let [fg (. opts :fg)]
                    `(.. :ctermfg= ,fg " guifg=" ,fg))
              :bg (let [bg (. opts :bg)]
                    `(.. :ctermbg= ,bg " guibg=" ,bg))
              _ `(.. ,(sym->str k) "=" ,v)))
        args (accumulate [args (sym->str name) k v (pairs opts)] `(.. ,args " " ,(f k v)))]
    `(nvim.ex.highlight ,args)))

(fn color [name]
  "Sets a vim colorscheme."
  `(nvim.ex.colorscheme ,(sym->str name)))

(fn t [key]
  "Returns the string with termcodes replaced"
  `(nvim.replace_termcodes ,(sym->str key) true true true))

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

{: viml->fn
 : viml->keymap
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
 : color
 : t
 : feedkeys
 : has?}
