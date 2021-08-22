;;; Helper Functions Declaration

;; Import bulb
(local {: inc 
        : dec
        : first
        : last
        : rest
        : any?
        : every?
        : empty?
        : keys 
        : vals
        : string?
        : number?
        : nil?
        :table? tbl?
        :array? seq?
        :map! map
        :filter! filter
        :cons cons
        :assoc! conj
        :dissoc! disj}
  (require :bulb))

(fn ->str [obj]
  "Converts an object to its string representation"
  (tostring obj))

(fn seq->tbl [seq] ;; cljlib
  (let [tbl {}]
    (for [i 1 (length seq) 2]
      (tset tbl (. seq i) (. seq (+ i 1))))
    tbl))

;; Similar to `bulb.tomap`
(fn tbl->seq [tbl]
  "Converts a table into a sequence of key-value pairs"
  (icollect [key value (pairs tbl)] [key value]))

(fn str->seq [str]
  "Converts a string to a sequence of characters"
  (icollect [chr (string.gmatch str ".")] chr))

(fn seq->set [seq]
  "Returns a set following the structure of `{:key true}` from a sequence"
  (collect [_ value (ipairs seq)]
    (values value true)))

(fn contains? [tbl x] ;; cljlib
  "Returns true if the sequence contains the object"
  ;; Checks if `x' is stored in `tbl' in linear time.
  (var res false)
  (each [i v (ipairs tbl)]
    (if (= v x)
        (do (set res i)
            (lua :break))))
  res)

(fn fn? [obj]
  "Returns true if the object is a function
  This only works at compilation time"
  (and
    (list? obj)
    (or
      (= (->str (first obj)) :hashfn)
      (= (->str (first obj)) :fn))))

(global __core_symfn_id 0)
(fn gensym-fn! []
  "Generates a new unique variable name following the structure `__core_symfn_#`"
  (.. "__core_symfn_"
      (do
        (global __core_symfn_id (inc __core_symfn_id))
        __core_symfn_id)))

;; Module helper
(fn exists? [module-name]
  "Returns true if the module exists and false if it doesn't"
  (let [(ok? _) (pcall require module-name)]
    ok?))

;;; Macros Declaration

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

(fn set! [name ?value]
  "Set a vim option using the `vim.opt` API"
  (let [name (->str name)
        value (if (nil? ?value) (not (name:match "^no")) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (if (fn? value)
      (let [fn-name (gensym-fn!)]
        `(do
           (global ,(sym fn-name) ,value)
           (tset vim.opt ,name ,(string.format "v:lua.%s()" fn-name))))
      (match (name:sub -1)
        :+ `(: (. vim.opt ,(name:sub 1 -2)) :append ,value)
        :- `(: (. vim.opt ,(name:sub 1 -2)) :remove ,value)
        :^ `(: (. vim.opt ,(name:sub 1 -2)) :prepend ,value)
        _ `(tset vim.opt ,name ,value)))))

(fn set-local! [name ?value]
  "Set a local vim option using the `vim.opt_local` API"
  (let [name (->str name)
        value (if (nil? ?value) (not (name:match "^no")) ?value)
        name (if (nil? ?value) (or (name:match "^no(.*)$") name) name)]
    (if (fn? value)
      (let [fn-name (gensym-fn!)]
        `(do
           (global ,(sym fn-name) ,value)
           (tset vim.opt_local ,name ,(string.format "v:lua.%s()" fn-name))))
      (match (name:sub -1)
        :+ `(: (. vim.opt_local ,(name:sub 1 -2)) :append ,value)
        :- `(: (. vim.opt_local ,(name:sub 1 -2)) :remove ,value)
        :^ `(: (. vim.opt_local ,(name:sub 1 -2)) :prepend ,value)
        _ `(tset vim.opt_local ,name ,value)))))

(fn let! [...]
  "Set a vim variable using the vim.[g b w t] API"
  (fn expr [name value]
    (let [name (->str name)
          scope (when (contains? ["g/" "b/" "w/" "t/"] (name:sub 1 2))
                  (name:sub 1 1))
          name (if (nil? scope) name (name:sub 3))]
      `(tset ,(match scope
                :b 'vim.b
                :w 'vim.w
                :t 'vim.t
                _ 'vim.g) ,name ,value)))
  (fn exprs [...]
    (match [...]
      (where [& rest] (empty? rest)) []
      [name value & rest] [(expr name value)
                           (unpack (exprs (unpack rest)))]
      _ []))
  (let [exprs (exprs ...)]
    (if (> (length exprs) 1)
      `(do ,(unpack exprs))
      (unpack exprs))))

(fn augroup! [name ...]
  `(do
     (vim.cmd ,(string.format "augroup %s\nautocmd!"
                              (->str name)))
     (do
       ,...)
     (vim.cmd "augroup END")
     nil))

(fn buf-augroup! [name ...]
  `(do
     (vim.cmd ,(string.format "augroup %s\nautocmd! * <buffer>"
                              (->str name)))
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
      (let [fn-name (gensym-fn!)]
        `(do
           (global ,(sym fn-name) ,command)
           (vim.cmd ,(string.format "autocmd %s %s call v:lua.%s()"
                                    events pattern fn-name))))
      `(vim.cmd ,(string.format "autocmd %s %s %s"
                                events pattern command)))))

(fn buf-autocmd! [events command]
  "Defines an autocommand"
  (let [events (if (sequence? events) events [events])
        events (-> (map ->str events) 
                   (table.concat ","))]
    (if (fn? command)
      (let [fn-name (gensym-fn!)]
        `(do
           (global ,(sym fn-name) ,command)
           (vim.cmd ,(string.format "autocmd %s <buffer> call v:lua.%s()"
                                    events fn-name))))
      `(vim.cmd ,(string.format "autocmd %s <buffer> %s"
                                events command)))))

(fn wk-map! [mode lhs options description]
  "A mapping using which-key"
  `(let [(ok?# which-key#) (pcall require :which-key)]
     (when ok?#
       (which-key#.register
         {,lhs ,description}
         {:mode ,mode
          :buffer ,(if options.buffer 0)
          :silent ,(if options.silent true)
          :noremap ,(if (not options.noremap) false)
          :nowait ,(if options.nowait true)}))))

(fn nmap! [[modes & options] lhs rhs ?description]
  "Defines a vim mapping
  Allows functions as right-hand side"
  (fn expr [buffer? mode lhs rhs options]
    (if buffer?
      `(vim.api.nvim_buf_set_keymap 0 ,mode ,lhs ,rhs ,options)
      `(vim.api.nvim_set_keymap ,mode ,lhs ,rhs ,options)))
  (let [modes (-> modes
                  (->str)
                  (str->seq))
        buffer? (contains? options :buffer)
        options (seq->set options)
        options (if (fn? rhs)
                  (conj options [:expr true])
                  options)
        fn-name (when (fn? rhs)
                  (gensym-fn!))
        exprs (map #(expr buffer? $ lhs
                          (if (fn? rhs)
                            (string.format "v:lua.%s()" fn-name)
                            rhs)
                          (disj options :buffer))
                   modes)
        exprs (if (fn? rhs)
                (cons `(global ,(sym fn-name) ,rhs) exprs)
                exprs)
        exprs (if (and ?description (exists? :which-key))
                (conj exprs (unpack (map #(wk-map! $ lhs options ?description)
                                         modes)))
                exprs)]
    (if (> (length exprs) 1)
      `(do ,(unpack exprs))
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

;; These macros were inspired by https://github.com/tsbohc/zest.nvim.
;; In the documentation folder of this repo you can read an explanation about
;; how they work, but, if you want some macros that are plug-and-play you can
;; use zest, they are less opinionated than mine.

