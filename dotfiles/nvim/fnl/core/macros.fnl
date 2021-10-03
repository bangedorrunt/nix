;;;; Helper Functions Declaration
(local {: inc
        : dec
        : first
        : last
        : rest
        : every?
        : table?
        : reduce
        : merge
        : map
        : filter
        : string?
        : number?
        : nil?
        : empty?} (require :aniseed.core))

(fn ->str [x]
  "Converts an object to its string representation"
  (tostring x))

(fn str->seq [s]
  "Convert an string into a sequence of characters."
  (icollect [c (string.gmatch s ".")]
    c))

(fn ->key-opts [xs]
  "Returns a set following the structure of `{:key true}` from a sequence"
  (reduce #(merge $1 {$2 true}) {} xs))

(fn fn? [obj]
  "Returns true if the object is a function
  This only works at compilation time"
  (and (list? obj) (or (= (->str (first obj)) :hashfn)
                       (= (->str (first obj)) :fn))))

(fn contains? [tbl x]
  ;; Checks if `x' is stored in `tbl' in linear time.
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
          (set end (+ 1 end))
          (tset tbl end x))))
  tbl)

(fn disj [tbl ...]
  "Removes any number of keys from `tbl`, returning `tbl`."
  (for [i 1 (select "#" ...)]
    (let [k (select i ...)]
      (tset tbl k nil)))
  tbl)


;;;; Macros Declaration

;; NOTE: https://github.com/rktjmp/hotpot.nvim/discussions/6
(fn pug [val prefix?]
  "Put Unique Global

  (val :any prefix? :string) -> (uuid :string)

  Takes any given value, generates a unique name (with optional prefix)
  and inserts value into _G. Returns unique name."
  (let [inter-compile-uid (_G.os.date "%s")
        name (if prefix?
                 (.. (->str (gensym prefix?)) inter-compile-uid)
                 (.. (->str (gensym :pug)) inter-compile-uid))]
    `(do
       (tset _G ,name ,val)
       ,name)))

(fn vlua [what prefix?]
  "Wrap given in v:lua x pug call"
  `(.. "v:lua." ,(pug what prefix?) "()"))

(fn command [name expr]
  (if (fn? expr)
      `(nvim.ex.command_ ,(->str name) :call ,(vlua expr))
      `(nvim.ex.command_ ,(->str name) ,expr)))

(fn buf-command [name expr]
  (if (fn? expr)
      `(nvim.ex.command_ :-buffer ,(->str name) :call ,(vlua expr))
      `(nvim.ex.command_ :-buffer ,(->str name) ,expr)))

(fn viml->lua-command [name expr]
  (let [expr (.. "lua " expr)]
    `(command ,name ,expr)))

(fn viml->lua-buf-command [name expr]
  (let [f (.. "lua " expr)]
    `(buf-command ,name ,expr)))

(fn unless [condition ...]
  "Takes a single condition and evaluates the rest as a body if it's nil or
  false. This is intended for side-effects."
  `(when (not ,condition)
     ,...))

(fn o [name ?value]
  "Set one vim options using the `vim.opt` API
  The option name must be a symbol
  If the option doesn't have a corresponding value, if it begins with no the
  value becomes false, and if it doesn't it becomes true"
    (let [name (->str name)
          value (if (nil? ?value) (not (name:match :^no)) ?value)
          name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
      (if (fn? value)
          `(tset vim.opt ,name (string.format "%s" ,(vlua value)))
          (match (name:sub -1)
            "+" `(: (. vim.opt ,(name:sub 1 -2)) :append ,value)
            "-" `(: (. vim.opt ,(name:sub 1 -2)) :remove ,value)
            "^" `(: (. vim.opt ,(name:sub 1 -2)) :prepend ,value)
            _ `(tset vim.opt ,name ,value)))))

(fn ol [name ?value]
  "Set a local vim option using the `vim.opt_local` API"
    (let [name (->str name)
          value (if (nil? ?value) (not (name:match :^no)) ?value)
          name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
      (if (fn? value)
          `(tset vim.opt_local ,name (string.format "%s" ,(vlua value)))
          (match (name:sub -1)
            "+" `(: (. vim.opt_local ,(name:sub 1 -2)) :append ,value)
            "-" `(: (. vim.opt_local ,(name:sub 1 -2)) :remove ,value)
            "^" `(: (. vim.opt_local ,(name:sub 1 -2)) :prepend ,value)
            _ `(tset vim.opt_local ,name ,value)))))

(fn g [name value]
  "Set value for global Vim variable"
  (let [name (->str name)]
    `(tset vim.g ,name ,value)))

(fn augroup [name ...]
  "Defines an autocommand group using the `vim.cmd` API."
  `(do
     ,(unpack (-> [`(nvim.ex.augroup ,(->str name)) `(nvim.ex.autocmd_)]
                  (conj ...)
                  (conj `(nvim.ex.augroup :END))))))

(fn buf-augroup [name ...]
  "Defines a buffer-local autocommand group using the `vim.cmd` API."
  `(do
     ,(unpack (-> [`(nvim.ex.augroup ,(->str name))
                   `(nvim.ex.autocmd_ "* <buffer>")]
                  (conj ...)
                  (conj `(nvim.ex.augroup :END))))))

(fn autocmd [events pattern command]
  "Defines an autocommand"
  (let [events (if (sequence? events) events [events])
        events (-> (map ->str events)
                   (table.concat ","))
        pattern (if (sequence? pattern) pattern [pattern])
        pattern (-> (map ->str pattern)
                    (table.concat ","))]
    (if (fn? command)
        `(nvim.ex.autocmd_ (string.format "%s %s call %s" ,events ,pattern
                                          ,(vlua command)))
        `(nvim.ex.autocmd_ (string.format "%s %s %s" ,events ,pattern ,command)))))

(fn buf-autocmd [events command]
  "Defines an autocommand"
  (let [events (if (sequence? events) events [events])
        events (-> (map ->str events)
                   (table.concat ","))]
    (if (fn? command)
        `(nvim.ex.autocmd_ (string.format "%s <buffer> call %s" ,events
                                          ,(vlua command)))
        `(nvim.ex.autocmd_ (string.format "%s <buffer> %s" ,events ,command)))))

(fn buf-set-opt [bufnr name value]
  `(nvim.buf_set_option bufnr ,name ,value))

(fn buf-set-var [bufnr name value]
  "Sets a buffer-scoped (b:) variable"
  `(nvim.buf_set_var bufnr ,name ,value))

(fn nmap [[modes & options] lhs rhs]
  "Defines a vim mapping using the `vim.api.nvim_set_keymap` API or the
  `vim.api.nvim_buf_set_keymap` if the option `:buffer` was passed.
  Support all the options the API supports.
  If the `rhs` argument is a function then automatically includes the `:expr`
  option."
  (fn nvim-set-keymap [mode lhs rhs options]
    (let [buffer? (?. options :buffer)
          options (disj options :buffer)]
      (if buffer?
          `(nvim.buf_set_keymap 0 ,mode ,lhs ,rhs ,options)
          `(nvim.set_keymap ,mode ,lhs ,rhs ,options))))

  (fn is-rhs-a-fn? [rhs]
    (if (fn? rhs)
        (vlua rhs)
        rhs))

  (let [modes (-> modes
                  ->str
                  str->seq)
        options (if (fn? rhs)
                    (conj options :expr)
                    options)
        options (->key-opts options)
        exprs (map #(nvim-set-keymap $ lhs (is-rhs-a-fn? rhs) options) modes)]
    (if (> (length exprs) 1)
        `(do
           ,(unpack exprs))
        (unpack exprs))))

(fn noremap [[modes & options] lhs rhs]
  "Defines a vim mapping using the `vim.api.nvim_set_keymap` API or the
  `vim.api.nvim_buf_set_keymap` if the option `:buffer` was passed.
  Support all the options the API supports.
  If the `rhs` argument is a function then automatically includes the `:expr`
  option.
  Automatically includes the `:noremap` option."
  (let [options (cons :noremap options)]
    `(nmap ,(cons modes options) ,lhs ,rhs)))

(fn buf-nmap [[modes & options] lhs rhs]
  "Defines a vim mapping using the `vim.api.nvim_buf_set_keymap` if the option
  `:buffer` was passed.
  Support all the options the API supports.
  If the `rhs` argument is a function then automatically includes the `:expr`
  option."
  (let [options (cons :buffer options)]
    `(nmap ,(cons modes options) ,lhs ,rhs)))

(fn buf-noremap [[modes & options] lhs rhs]
  "Defines a vim mapping using the `vim.api.nvim_buf_set_keymap` if the option
  `:buffer` was passed.
  Support all the options the API supports.
  If the `rhs` argument is a function then automatically includes the `:expr`
  option.
  Automatically includes the `:noremap` option."
  (let [options (->> options
                     (cons :buffer)
                     (cons :noremap))]
    `(nmap ,(cons modes options) ,lhs ,rhs)))

(fn hi [group opts]
  "Defines a highlight"
  (let [f (fn [k v]
            (match k
              :fg (let [fg (. opts :fg)]
                    `(.. :ctermfg= ,fg " guifg=" ,fg))
              :bg (let [bg (. opts :bg)]
                    `(.. :ctermbg= ,bg " guibg=" ,bg))
              _ `(.. ,(->str k) "=" ,v)))
        args (accumulate [args (->str group) k v (pairs opts)] `(.. ,args " " ,(f k v)))]
    `(nvim.ex.highlight ,args)))

(fn color [name]
  "Sets a vim colorscheme.
  The name can be either a symbol or a string."
  `(nvim.ex.colorscheme ,(->str name)))

(fn t [key]
  "Returns the string with termcodes replaced"
  `(nvim.replace_termcodes ,(->str key) true true true))

(fn feedkeys [key]
  `(nvim.feedkeys ,(t key) :n true))

(fn has? [property]
  "Returns true if vim has a propety"
  `(match (vim.fn.has ,property)
     1 true
     0 false
     _# nil))

{: pug
 : vlua
 : o
 : ol
 : g
 : command
 : buf-command
 : viml->lua-command
 : viml->lua-buf-command
 : augroup
 : buf-augroup
 : autocmd
 : buf-autocmd
 : buf-set-opt
 : buf-set-var
 : nmap
 : noremap
 : buf-nmap
 : buf-noremap
 : hi
 : color
 : t
 : feedkeys
 : has?
 : unless}
