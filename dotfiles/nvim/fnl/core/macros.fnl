;;; Helper Functions Declaration
(local {: inc
        : dec
        : first
        : last
        : rest
        : any?
        : every?
        : keys
        : vals
        : reduce
        : merge
        :mapv map
        : filter
        : string?
        : number?
        : nil?
        : contains?
        :map? tbl?
        : set?
        :vector? seq?
        : empty?
        : cons
        : conj
        : dissoc} (require :cljlib))

(import-macros {: defn
                : into
                : empty
                : when-let
                : if-let
                : when-some
                : if-some} :cljlib.macros)

;; Reserve name for `into` macros
;; tbl->seq
;; seq->tbl
;; str->seq

(fn ->str [obj]
  "Converts an object to its string representation"
  (tostring obj))

(fn fn? [obj]
  "Returns true if the object is a function
  This only works at compilation time"
  (and (list? obj) (or (= (->str (first obj)) :hashfn)
                       (= (->str (first obj)) :fn))))

(fn ->key-opts [seq]
  "Returns a set following the structure of `{:key true}` from a sequence"
  (reduce #(merge $1 {$2 true}) {} seq))

(fn exists? [module-name]
  "Returns true if the module exists and false if it doesn't"
  (let [(ok? _) (pcall require module-name)]
    ok?))

(global __core_symfn_id 0)
(fn gensym-fn! []
  "Generates a new unique variable name following the structure `__core_symfn_#`"
  (.. :__core_symfn_ (do
                       (global __core_symfn_id (inc __core_symfn_id))
                       __core_symfn_id)))

;;; Macros Declaration

;; FIXME: it's not working as @datwaft `gensym-fn!`
;; NOTE: see https://github.com/rktjmp/hotpot.nvim/discussions/6
;; Put Unique Global
;;
;; (val :any prefix? :string) -> (uuid :string)
;;
;; Takes any given value, generates a unique name (with optional prefix)
;; and inserts value into _G. Returns unique name.
(fn pug [val prefix?]
  ;; gensym will generate a unique id across a compile pass, but hotpot/aniseed 
  ;; may compile files in separate passes as they are modified, so symbols may 
  ;; collide you can avoid this by passing a unique prefix per-file or using 
  ;; something like the "uid" below, based on compile time
  (local inter-compile-uid (_G.os.date "%s"))
  (local name
         (if prefix?
             (.. (->str (gensym prefix?)) inter-compile-uid)
             (.. (->str (gensym :pug)) inter-compile-uid)))
  `(do
     (tset _G ,name ,val)
     ,name))

;; Wrap given in v:lua x pug call
(fn vlua [what prefix?]
  `(.. "v:lua." ,(pug what prefix?) "()"))

(fn command! [name f]
  `(vim.cmd ,(string.format "command! %s %s" (->str name) (->str f))))

(fn buf-command! [name f]
  `(vim.cmd ,(string.format "command! -buffer %s %s" (->str name) (->str f))))

(fn lua-command! [name f]
  (let [f (.. "lua " (->str f))]
    `(command! ,name ,f)))

(fn lua-buf-command! [name f]
  (let [f (.. "lua " (->str f))]
    `(buf-command! ,name ,f)))

(fn unless [condition ...]
  "Takes a single condition and evaluates the rest as a body if it's nil or
  false. This is intended for side-effects."
  `(when (not ,condition)
     ,...))

(fn set! [...]
  "Set one or multiple vim options using the `vim.opt` API
  The option name must be a symbol
  If the option doesn't have a corresponding value, if it begins with no the
  value becomes false, and if it doesn't it becomes true"
  (fn expr [name ?value]
    (let [name (->str name)
          value (if (nil? ?value) (not (name:match "^no")) ?value)
          name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
      (if (fn? value)
        `(tset vim.opt ,name (string.format "%s" ,(vlua value)))
        (match (name:sub -1)
          :+ `(: (. vim.opt ,(name:sub 1 -2)) :append ,value)
          :- `(: (. vim.opt ,(name:sub 1 -2)) :remove ,value)
          :^ `(: (. vim.opt ,(name:sub 1 -2)) :prepend ,value)
          _ `(tset vim.opt ,name ,value)))))
  (fn exprs [...]
    (match [...]
      (where [& rest] (empty? rest)) []
      (where [name value & rest] (not (sym? value))) [(expr name value)
                                                      (unpack (exprs (unpack rest)))]
      [name & rest] [(expr name)
                     (unpack (exprs (unpack rest)))]
      _ []))
  (let [exprs (exprs ...)]
    (if (> (length exprs) 1)
      `(do ,(unpack exprs))
      (unpack exprs))))

(fn set-local! [...]
  "Set a local vim option using the `vim.opt_local` API"
  (fn expr [name ?value]
    (let [name (->str name)
          value (if (nil? ?value) (not (name:match "^no")) ?value)
          name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
      (if (fn? value)
        `(tset vim.opt_local ,name (string.format "%s" ,(vlua value)))
        (match (name:sub -1)
          "+" `(: (. vim.opt_local ,(name:sub 1 -2)) :append ,value)
          "-" `(: (. vim.opt_local ,(name:sub 1 -2)) :remove ,value)
          "^" `(: (. vim.opt_local ,(name:sub 1 -2)) :prepend ,value)
          _ `(tset vim.opt_local ,name ,value)))))
  (fn exprs [...]
    (match [...]
      (where [& rest] (empty? rest)) []
      (where [name value & rest] (not (sym? value))) [(expr name value)
                                                      (unpack (exprs (unpack rest)))]
      [name & rest] [(expr name)
                     (unpack (exprs (unpack rest)))]
      _ []))
  (let [exprs (exprs ...)]
    (if (> (length exprs) 1)
      `(do ,(unpack exprs))
      (unpack exprs))))

(fn let! [...]
  "Set a vim variable using the vim.[g b w t] API"
  (fn expr [name value]
    (let [name (->str name)
          scope (when (contains? [:g/ :b/ :w/ :t/] (name:sub 1 2))
                  (name:sub 1 1))
          name (if (nil? scope) name (name:sub 3))]
      `(tset ,(match scope
                :b `vim.b
                :w `vim.w
                :t `vim.t
                _ `vim.g) ,name ,value)))

  (fn exprs [...]
    (match [...]
      (where [& rest] (empty? rest)) []
      [name value & rest] [(expr name value) (unpack (exprs (unpack rest)))]
      _ []))

  (let [exprs (exprs ...)]
    (if (> (length exprs) 1)
        `(do
           ,(unpack exprs))
        (unpack exprs))))

(fn augroup! [name ...]
  `(do
     (vim.cmd ,(string.format "augroup %s\nautocmd!" (->str name)))
     (do
       ,...)
     (vim.cmd "augroup END")
     nil))

(fn buf-augroup! [name ...]
  `(do
     (vim.cmd ,(string.format "augroup %s\nautocmd! * <buffer>" (->str name)))
     (do
       ,...)
     (vim.cmd "augroup END")
     nil))

(fn autocmd! [events pattern command]
  "Defines an autocommand"
  (let [events (if (sequence? events) events [events])
        events (-> (map ->str events)
                   (table.concat ","))
        pattern (if (sequence? pattern) pattern [pattern])
        pattern (-> (map ->str pattern)
                    (table.concat ","))]
    (if (fn? command)
        `(vim.cmd (string.format "autocmd %s %s call %s" ,events ,pattern ,(vlua command)))
        `(vim.cmd ,(string.format "autocmd %s %s %s" events pattern command)))))

(fn buf-autocmd! [events command]
  "Defines an autocommand"
  (let [events (if (sequence? events) events [events])
        events (-> (map ->str events)
                   (table.concat ","))]
    (if (fn? command)
        `(vim.cmd (string.format "autocmd %s <buffer> call %s" ,events ,(vlua command)))
        `(vim.cmd ,(string.format "autocmd %s <buffer> %s" events command)))))

(fn wk-map! [mode lhs options description]
  "A mapping using which-key"
  `(let [(ok?# which-key#) (pcall require :which-key)]
     (when ok?#
       (which-key#.register {,lhs ,description}
                            {:mode ,mode
                             :buffer ,(if options.buffer 0)
                             :silent ,(if options.silent true)
                             :noremap ,(if (not options.noremap) false)
                             :nowait ,(if options.nowait true)}))))

;; TODO: use `pug` instead of `gensym-fn!`
(fn nmap! [[modes & options] lhs rhs ?description]
  "Defines a vim mapping
  Allows functions as right-hand side"
  (fn expr [buffer? mode lhs rhs options]
    (if buffer?
        `(vim.api.nvim_buf_set_keymap 0 ,mode ,lhs ,rhs ,options)
        `(vim.api.nvim_set_keymap ,mode ,lhs ,rhs ,options)))

  (let [modes (->> modes
                   (->str)
                   (into []))
        buffer? (contains? options :buffer)
        options (->key-opts options)
        options (if (fn? rhs)
                    (conj options [:expr true])
                    options)
        fn-name (when (fn? rhs)
                  (gensym-fn!))
        exprs (map #(expr buffer? $ lhs
                          (if (fn? rhs)
                              (string.format "v:lua.%s()" fn-name)
                              rhs)
                          (dissoc options :buffer)) modes)
        exprs (if (fn? rhs)
                  (cons `(global ,(sym fn-name) ,rhs) exprs)
                  exprs)
        exprs (if (and ?description (exists? :which-key))
                  (conj exprs
                        (unpack (map #(wk-map! $ lhs options ?description)
                                     modes)))
                  exprs)]
    (if (> (length exprs) 1)
        `(do
           ,(unpack exprs))
        (unpack exprs))))

(fn noremap! [[modes & options] lhs rhs ?description]
  "Defines a vim mapping
  Allows functions as right-hand side
  Appends :noremap to the options"
  (let [options (cons :noremap options)]
    `(nmap! ,(cons modes options) ,lhs ,rhs ,?description)))

(fn buf-nmap! [[modes & options] lhs rhs ?description]
  "Defines a vim mapping
  Allows functions as right-hand side
  Appends :buffer to the options"
  (let [options (cons :buffer options)]
    `(nmap! ,(cons modes options) ,lhs ,rhs ,?description)))

(fn buf-noremap! [[modes & options] lhs rhs ?description]
  "Defines a vim mapping
  Allows functions as right-hand side
  Appends :buffer and :noremap to the options"
  (let [options (->> options
                     (cons :buffer)
                     (cons :noremap))]
    `(nmap! ,(cons modes options) ,lhs ,rhs ,?description)))

(fn t [combination]
  "Returns the string with termcodes replaced"
  `(vim.api.nvim_replace_termcodes ,(->str combination) true true true))

(fn has? [property]
  "Returns true if vim has a propety"
  `(match (vim.fn.has ,property)
     1 true
     0 false
     _# nil))

{: set!
 : set-local!
 : let!
 : command!
 : buf-command!
 : lua-command!
 : lua-buf-command!
 : augroup!
 : buf-augroup!
 : autocmd!
 : buf-autocmd!
 : nmap!
 : noremap!
 : buf-nmap!
 : buf-noremap!
 : t
 : has?
 : unless}
