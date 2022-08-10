(local {: for_each
        : head
        : map
        : nth
        :tomap ->tbl
        :totable ->seq
        :length count} (require :aniseed.deps.fun))

(fn inc [n]
  "Increment n by 1."
  (+ n 1))

(fn dec [n]
  "Decrement n by 1."
  (- n 1))

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

(fn contains? [xs target]
  (var seen? false)
  (for_each #(when (= $ target)
               (set seen? true))
            xs)
  seen?)

(fn ->str [x]
  "Convert a symbol to a string"
  (tostring x))

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
;; NOTE: https://github.com/rktjmp/hotpot.nvim/discussions/6
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
  ;; WARNING: only use with anonymous function
  "Evaluate expr
  If expr is a function, wrap expr with `vlua`"
  (if (quoted? expr)
    (values :call (vlua (quoted->fn expr)))
    expr))

(fn command [...]
  "Define command"
  (match (select "#" ...)
    2 (let [(name expr) ...] `(vim.api.nvim_create_user_command ,(->str name) ,(quoted->fn? expr) {}))
    3 (let [(name _ expr) ...] `(vim.api.nvim_buf_create_user_command 0 ,(->str name) ,(quoted->fn? expr) {}))))

(fn opt [name ?value]
  "Set one vim.options using the `vim.opt` API
  The option name must be a symbol
  If the option doesn't have a corresponding value, if it begins with no the
  value becomes false, and if it doesn't it becomes true"
  (let [name (->str name)
        value (if (nil? ?value) (not (name:match :^no)) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (match (name:sub -1)
      "+" `(: (. vim.opt ,(name:sub 1 -2)) :append ,value)
      "-" `(: (. vim.opt ,(name:sub 1 -2)) :remove ,value)
      "^" `(: (. vim.opt ,(name:sub 1 -2)) :prepend ,value)
      _ `(tset vim.opt ,name ,value))))

(fn opt-local [name ?value]
  "Set a local vim.option using the `vim.opt_local` API"
  (let [name (->str name)
        value (if (nil? ?value) (not (name:match :^no)) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (match (name:sub -1)
      "+" `(: (. vim.opt_local ,(name:sub 1 -2)) :append ,value)
      "-" `(: (. vim.opt_local ,(name:sub 1 -2)) :remove ,value)
      "^" `(: (. vim.opt_local ,(name:sub 1 -2)) :prepend ,value)
      _ `(tset vim.opt_local ,name ,value))))

(fn g [name value]
  "Set value for global Vim variable"
  (let [name (->str name)]
    `(tset vim.g ,name ,value)))

(fn augroup [name ...]
  "Defines an autocommand group"
  `(do
     (vim.cmd.augroup ,(->str name))
     (do
       ,...)
     (vim.cmd.augroup "END")))

(fn autocmd! [...]
  "Defines an autocommand!"
  (match (select "#" ...)
    0 `(vim.cmd.autocmd {:bang true})
    1 `(vim.cmd.autocmd {:bang true :args ,(->str ...)})
    2 (let [(x y) ...] `(vim.cmd.autocmd {:bang true :args [,(->str x) ,(->str y)]}))))

(fn autocmd [events pattern command]
  "Defines an autocommand"
  `(vim.cmd.autocmd {:args [,(->str events) ,(->str pattern) ,(vlua->fn? command)]}))

(fn nmap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API"
  ;; TODO: figure out why I cant use outer function in macro
  (let [args [...]
        modes (-> modes ->str ->seq)
        rhs (-> args last quoted->fn?)
        lhs (llast args)
        options (->> [(unpack args 1 (- (count args) 2))]
                     (map ->str)
                     (map #(values $ true))
                     ->tbl)]
    `(vim.keymap.set ,modes ,lhs ,rhs ,options)))

(fn noremap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API
  Automatically includes the `:noremap` option."
  `(nmap ,modes :noremap ,...))

(fn hi [name colors]
  `(vim.api.nvim_set_hl 0 ,(->str name) ,colors))

(fn colorscheme [name]
  "Sets a vim colorscheme."
  `(vim.cmd.colorscheme ,(->str name)))

(fn t [key]
  "Returns the string with termcodes replaced"
  `(vim.api.nvim_replace_termcodes ,(->str key) true true true))

(fn feedkeys [key]
  "Sends input-keys to Nvim, subject to various quirks
  controlled by `mode` flags."
  `(vim.api.nvim_feedkeys ,(t key) :n true))

(fn has? [property]
  "Returns true if vim has a propety"
  `(match (vim.fn.has ,property)
     1 true
     0 false
     _# nil))

{: contains?
 : opt
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
