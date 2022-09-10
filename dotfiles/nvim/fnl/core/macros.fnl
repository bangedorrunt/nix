(local {: for_each
        : count
        : inc : dec
        : first : second : last : llast
        : nil? : string? : number? : boolean?
        : map : chain
        : totable : tosequence} (require :core.funs))

;;;; Helper functions
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

(fn expand_exprs [exprs]
  "Converts a list of expressions into either an expression - if only one
  expression is in the list - or a do-expression containing the expressions."
  (if (> (count exprs) 1)
    `(do
       ,(unpack exprs))
    (first exprs)))

(fn into_sequence [xs iterable]
  (-> xs (chain iterable) tosequence))

(fn into_table [tbl iterable]
  (-> tbl (chain iterable) totable))

;;;; Macros Declaration
(fn command [...]
  "Define command"
  (match (select "#" ...)
    1 (error "expected 2 or 3 arguments")
    2 (let [(name expr) ...]
        `(vim.api.nvim_create_user_command ,(tostring name) ,(quoted->fn? expr) {}))
    3 (let [(name _ expr) ...]
        `(vim.api.nvim_buf_create_user_command 0 ,(tostring name) ,(quoted->fn? expr) {}))
    _ (error "expected 2 or 3 arguments only")))

(fn opt [name ?value]
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

(fn opt_local [name ?value]
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
                (->> event (map tostring) tosequence)
                (tostring event))
        pattern (if (sequence? pattern)
                  (->> pattern (map tostring) tosequence)
                  (tostring pattern))
        options (or ?options {})
        options (if (nil? options.buffer)
                  (if (= "<buffer>" pattern)
                    (into_table options {:buffer 0})
                    (into_table options {:pattern pattern}))
                  options)
        options (if (string? command)
                  (into_table options {:command command})
                  (into_table options {:callback (quoted->fn? command)}))]
    `(vim.api.nvim_create_autocmd ,event ,options)))

(fn autocmd! [name ?options]
  "Clears an augroup using the nvim_clear_autocmds API. "
  (let [name (tostring name)
        options (into_table (or ?options {}) {:group name})]
    `(vim.api.nvim_clear_autocmds ,options)))

(fn augroup [name ...]
  "Create an augroup using the nvim_create_augroup API.
  Accepts either a name or a name and a list of autocmd statements. "
  (expand_exprs
    (let [name (tostring name)
          exprs
          (map
            (fn [expr]
              (if (= 'autocmd (first expr))
                (let [[_ event pattern command ?options] expr
                      options (into_table (or ?options {}) {:group name})]
                  `(autocmd ,event ,pattern ,command ,options))
                (let [[_ ?options] expr]
                  `(autocmd! ,name ,?options))))
            [...])]
      (into_sequence [`(vim.api.nvim_create_augroup ,name {:clear false})] exprs))))

(fn nmap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API"
  (let [args [...]
        modes (-> modes tostring tosequence)
        rhs (-> args last quoted->fn?)
        lhs (llast args)
        options (->> [(unpack args 1 (- (count args) 2))]
                     (map tostring)
                     (map #(values $ true)))
        options (into_table options {:remap true})]
    `(vim.keymap.set ,modes ,lhs ,rhs ,options)))

(fn noremap [modes ...]
  "Defines a vim mapping using the `vim.keymap.set` API
  Automatically includes the `:noremap` option."
  (let [args [...]
        modes (-> modes tostring tosequence)
        rhs (-> args last quoted->fn?)
        lhs (llast args)
        options (->> [(unpack args 1 (- (count args) 2))]
                     (map tostring)
                     (map #(values $ true)))
        options (into_table options {:noremap true})]
    `(vim.keymap.set ,modes ,lhs ,rhs ,options)))

(fn hi [name colors]
  `(vim.api.nvim_set_hl 0 ,(tostring name) ,colors))

(fn colorscheme [name]
  "Sets a vim colorscheme."
  `(vim.cmd.colorscheme ,(tostring name)))

(fn t [key]
  "Returns the string with termcodes replaced"
  `(vim.api.nvim_replace_termcodes ,(tostring key) true true true))

(fn feedkeys [key]
  "Sends input-keys to Nvim, subject to various quirks
  controlled by `mode` flags."
  `(vim.api.nvim_feedkeys ,(t key) :n true))

(fn vim_has? [property]
  "Returns true if vim has a propety"
  `(match (vim.fn.has ,property)
     1 true
     0 false
     _# nil))

;;; Clojure if-let and when-let
(fn conditional-let [branch bindings ...]
  (assert (= 2 (count bindings)) "expected a single binding pair")

  (let [[bind_expr value_expr] bindings]
    (if
      ;; Simple symbols
      ;; [foo bar]
      (sym? bind_expr)
      `(let [,bind_expr ,value_expr]
         (,branch ,bind_expr ,...))

      ;; List / values destructure
      ;; [(a b) c]
      (list? bind_expr)
      (do
        ;; Even if the user isn't using the first slot, we will.
        ;; [(_ val) (pcall #:foo)]
        ;;  => [(bindGENSYM12345 val) (pcall #:foo)]
        (when (= '_ (first bind_expr))
          (tset bind_expr 1 (gensym "bind")))

        `(let [,bind_expr ,value_expr]
           (,branch ,(first bind_expr) ,...)))

      ;; Sequential and associative table destructure
      ;; [[a b] c]
      ;; [{: a : b} c]
      (table? bind_expr)
      `(let [value# ,value_expr
             ,bind_expr (or value# {})]
         (,branch value# ,...))

      ;; We should never get here, but just in case.
      (assert (.. "unknown bind_expr type: " (type bind_expr))))))

(fn if-let [bindings ...]
  (assert (<= (count [...]) 2) (.. "if-let does not support more than two branches"))
  (conditional-let 'if bindings ...))

(fn when-let [bindings ...]
  (conditional-let 'when bindings ...))

{: if-let : when-let
 : opt
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
 : vim_has?}
