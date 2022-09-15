(local {: inc
        : dec
        : first
        : second
        : last
        : llast
        : nil?
        : string?
        : number?
        : boolean?
        : map
        : reduce
        : into} (require :core.funs))

;;;; Helper functions
(fn str->seq [s]
  "Convert an string into a sequence of characters."
  (icollect [c (string.gmatch s ".")]
    c))

(fn fn? [x]
  "Checks if `x` is a function definition.
  Cannot check if a symbol is a function in compile time."
  (and (list? x)
       (or (= `fn (first x)) (= `hashfn (first x)) (= `lambda (first x))
           (= `partial (first x)))))

(fn quoted? [x]
  "Check if `x` is a list that begins with `quote`."
  (and (list? x) (= `quote (first x))))

(fn quoted->fn [expr]
  "Converts an expression like `(quote (+ 1 1))` into `(fn [] (+ 1 1))`."
  (let [non-quoted (second expr)]
    `(fn []
       ,non-quoted)))

(fn quoted->fn? [expr]
  (if (quoted? expr) (quoted->fn expr) expr))

(fn expand-exprs [exprs]
  "Converts a list of expressions into either an expression - if only one
  expression is in the list - or a do-expression containing the expressions."
  (if (> (length exprs) 1)
      `(do
         ,(unpack exprs))
      (first exprs)))

(fn into-sequence [xs iterable]
  (-> xs (chain iterable) tosequence))

(fn into-table [tbl iterable]
  (-> tbl (chain iterable) totable))

;;;; Neovim
;;;; ------

(fn command [...]
  "Define command"
  (match (select "#" ...)
    1 (error "expected 2 or 3 arguments")
    2 (let [(name expr) ...]
        `(vim.api.nvim_create_user_command ,(tostring name) ,(quoted->fn? expr)
                                           {}))
    3 (let [(name _ expr) ...]
        `(vim.api.nvim_buf_create_user_command 0 ,(tostring name)
                                               ,(quoted->fn? expr) {}))
    _ (error "expected 2 or 3 arguments only")))

(fn set! [name ?value]
  "Set one vim.options using the `vim.opt` API
  The option name must be a symbol
  If the option doesn't have a corresponding value, if it begins with no the
  value becomes false, and if it doesn't it becomes true"
  (let [name (tostring name)
        value (if (nil? ?value) (not (name:match :^no)) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (match (name:sub -1)
      "+" `(: (. vim.opt ,(name:sub 1 -2)) :append ,value)
      "-" `(: (. vim.opt ,(name:sub 1 -2)) :remove ,value)
      "^" `(: (. vim.opt ,(name:sub 1 -2)) :prepend ,value)
      _ `(tset vim.opt ,name ,value))))

(fn setl! [name ?value]
  "Set a local vim.option using the `vim.opt_local` API"
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
  (let [name (tostring name)]
    `(tset vim.g ,name ,value)))

(fn autocmd [event pattern command ?options]
  "Create an autocommand using the nvim_create_autocmd API. "
  (let [event (if (sequence? event)
                  (->> event (map tostring))
                  (tostring event))
        pattern (if (sequence? pattern)
                    (->> pattern (map tostring))
                    (tostring pattern))
        options (or ?options {})
        options (if (nil? options.buffer)
                    (if (= :<buffer> pattern)
                        (doto options (tset :buffer 0))
                        (doto options (tset :pattern pattern)))
                    options)
        options (if (string? command)
                    (doto options (tset :command command))
                    (doto options (tset :callback (quoted->fn? command))))]
    `(vim.api.nvim_create_autocmd ,event ,options)))

(fn autocmd! [name ?options]
  "Clears an augroup using the nvim_clear_autocmds API. "
  (let [name (tostring name)
        options (doto (or ?options {}) (tset :group name))]
    `(vim.api.nvim_clear_autocmds ,options)))

(fn augroup [name ...]
  "Create an augroup using the nvim_create_augroup API.
  Accepts either a name or a name and a list of autocmd statements. "
  (expand-exprs (let [name (tostring name)]
                  (icollect [_ expr (ipairs [...]) :into [`(vim.api.nvim_create_augroup ,name
                                                                                        {:clear false})]]
                    (if (= `autocmd (first expr))
                        (let [[_ event pattern command ?options] expr
                              options (or ?options {})
                              options (doto options (tset :group name))]
                          `(autocmd ,event ,pattern ,command ,options))
                        (let [[_ ?options] expr]
                          `(autocmd! ,name ,?options)))))))

(fn nmap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API"
  (let [args [...]
        modes (-> modes tostring str->seq)
        rhs (-> args last quoted->fn?)
        lhs (llast args)
        tmp {}
        options (->> [(unpack args 1 (- (length args) 2))]
                     (map tostring)
                     (map #(tset tmp $ true)))
        options (into tmp :remap true)]
    `(vim.keymap.set ,modes ,lhs ,rhs ,options)))

(fn noremap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API
  Automatically includes the `:noremap` option."
  (let [args [...]
        modes (-> modes tostring str->seq)
        rhs (-> args last quoted->fn?)
        lhs (llast args)
        tmp {}
        options (->> [(unpack args 1 (- (length args) 2))]
                     (map tostring)
                     (map #(tset tmp $ true)))
        options (into tmp :noremap true)]
    `(vim.keymap.set ,modes ,lhs ,rhs ,options)))

(fn hi [name colors]
  `(vim.api.nvim_set_hl 0 ,(tostring name) ,colors))

(fn colorscheme [name]
  "Sets a vim colorscheme."
  `(vim.cmd.colorscheme ,(tostring name)))

(fn termcodes [key]
  "Returns the string with termcodes replaced"
  `(vim.api.nvim_replace_termcodes ,(tostring key) true true true))

(fn feedkeys [key]
  "Sends input-keys to Nvim, subject to various quirks
  controlled by `mode` flags."
  `(vim.api.nvim_feedkeys ,(termcodes key) :n true))

(fn vim-has? [property]
  "Returns true if vim has a propety"
  `(match (vim.fn.has ,property)
     1 true
     0 false
     _# nil))

;;;; Lazy Loading Lib
;;;; ----------------
;; See: https://github.com/tjdevries/lazy.nvim/blob/master/lua/lazy.lua
;; Will only require the module after the first index of a module.
;; Only works for modules that export a table.
(fn lazyreq [module]
  `(setmetatable {}
                 {:__index (fn [_# key#]
                             (. (require ,module) key#))
                  :__newindex (fn [_# key# value#]
                                (tset (require ,module) key# value#))}))

;; Requires only when you call the _module_ itself.
(fn lazymod [module]
  `(setmetatable {} {:__call (fn [_# ...]
                               ((require ,module) ...))}))

;; Require when an exported method is called.
;; Creates a new function. Cannot be used to compare functions,
;; set new values, etc. Only useful for waiting to do the require until you
;; actually call the code.
(fn lazyfunc [module]
  `(setmetatable {}
                 {:__index (fn [_# k#]
                             (fn [...]
                               ((. (require ,module) k#) ...)))}))

;;;; Plugin Manager
;;;; --------------
(fn before! [pluginname ...]
  "Run code before a plugin loads"
  `((. (lazyreq :core.jetpack) :before) ,pluginname
                                        (fn []
                                          ,...)))

(fn after! [plugin-name ...]
  "Run code after a plugin loads"
  `((. (lazyreq :core.jetpack) :after) ,plugin-name
                                       (fn []
                                         ,...)))

(fn setup! [module ...]
  `((. (lazyreq ,module) :setup) ,...))

;;;; Clojure if-let and when-let
;;;; ---------------------------
(fn conditional-let [branch bindings ...]
  (assert (= 2 (length bindings)) "expected a single binding pair")
  (let [[bind-expr value-expr] bindings]
    (if ;; Simple symbols
        ;; [foo bar]
        (sym? bind-expr)
        `(let [,bind-expr ,value-expr]
           (,branch ,bind-expr ,...))
        ;; List / values destructure
        ;; [(a b) c]
        (list? bind-expr)
        (do
          ;; Even if the user isn't using the first slot, we will.
          ;; [(_ val) (pcall #:foo)]
          ;;  => [(bindGENSYM12345 val) (pcall #:foo)]
          (when (= `_ (first bind-expr))
            (tset bind-expr 1 (gensym :bind)))
          `(let [,bind-expr ,value-expr]
             (,branch ,(first bind-expr) ,...)))
        ;; Sequential and associative table destructure
        ;; [[a b] c]
        ;; [{: a : b} c]
        (table? bind-expr)
        `(let [value# ,value-expr
               ,bind-expr (or value# {})]
           (,branch value# ,...))
        ;; We should never get here, but just in case.
        (assert (.. "unknown bind-expr type: " (type bind-expr))))))

(fn if-let [bindings ...]
  (assert (<= (length [...]) 2)
          (.. "if-let does not support more than two branches"))
  (conditional-let `if bindings ...))

(fn when-let [bindings ...]
  (conditional-let `when bindings ...))

{: if-let
 : when-let
 : lazyreq
 : lazymod
 : lazyfunc
 : before!
 : after!
 : setup!
 : set!
 : setl!
 : g
 : command
 : autocmd
 : autocmd!
 : augroup
 : nmap
 : noremap
 : hi
 : colorscheme
 : termcodes
 : feedkeys
 : vim-has?}
