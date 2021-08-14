(global __core_symfn_id 0)

(lambda gensym-fn! []
  "Generates a new unique variable name following the structure __core_symfn_#"
  (.. :__core_symfn_ (do
                       (global __core_symfn_id (+ __core_symfn_id 1))
                       __core_symfn_id)))

(lambda has? [property]
  `(match (vim.fn.has ,property)
     1 true
     0 false
     _# nil))

(lambda unless [condition ...]
  "Takes a single condition and evaluates the rest as a body if it's nil or
  false. This is intended for side-effects."
  `(when (not ,condition)
     ,...))

(lambda exists? [module-name]
  "This function verifies if a module exists returning nil if it doesn't and
  returning the module if it does"
  (let [(ok?# content#) (pcall require module-name)]
    (if ok?# content# nil)))

(lambda string? [obj]
  (= :string (type obj)))

(lambda nil? [obj]
  (= :nil (type obj)))

(lambda map [fun iter]
  (if (string? iter) (icollect [char (string.gmatch iter ".")]
                       (fun char))
      (icollect [_ value (ipairs iter)]
        (fun value))))

(lambda contains? [iter obj]
  (not= 0 (length (icollect [_ value (ipairs iter)]
                    (when (= obj value)
                      value)))))

(lambda function? [obj]
  (and (list? obj) (or (= (tostring (. obj 1)) :hashfn)
                       (= (tostring (. obj 1)) :fn))))

(lambda get? [name]
  "Returns the value of a vim option"
  (assert-compile (or (sym? name) (= :string (type name)))
                  "'name' must be either a symbol or a string" name)
  (let [name (tostring name)
        name (if (= (name:sub 1 2) :no) (name:sub 3) name)]
    `(let [(ok?# value#) (pcall #(: (. vim.opt ,name) :get))]
       (if ok?# value# nil))))

(lambda get-local? [name]
  "Returns the value of a vim local option"
  (assert-compile (or (sym? name) (= :string (type name)))
                  "'name' must be either a symbol or a string" name)
  (let [name (tostring name)
        name (if (= (name:sub 1 2) :no) (name:sub 3) name)]
    `(let [(ok?# value#) (pcall #(: (. vim.opt_local ,name) :get))]
       (if ok?# value# nil))))

(lambda set! [name ?value]
  "Set a vim option using the vim.opt api"
  (assert-compile (or (sym? name) (= :string (type name)))
                  "'name' must be either a symbol or a string" name)
  (let [name (tostring name)
        value-is-nil? (= nil ?value)
        name-begins-with-no? (= (name:sub 1 2) :no)
        name (if (and value-is-nil? name-begins-with-no?)
                 (name:sub 3)
                 name)
        value (if value-is-nil?
                  (not name-begins-with-no?)
                  ?value)
        name-last-character (name:sub -1)
        name-without-last-character (name:sub 1 -2)]
    (if (and (list? value)
             (or (= (tostring (. value 1)) :hashfn)
                 (= (tostring (. value 1)) :fn)))
        (let [symbol (gensym-fn!)]
          `(do
             (global ,(sym symbol) ,value)
             (tset vim.opt ,name ,(string.format "v:lua.%s()" symbol))))
        (match name-last-character
          "?" `(get? ,name-without-last-character)
          "!" `(tset vim.opt ,name-without-last-character
                     (not (get? ,name-without-last-character)))
          "+" `(: (. vim.opt ,name-without-last-character) :append ,value)
          "-" `(: (. vim.opt ,name-without-last-character) :remove ,value)
          "^" `(: (. vim.opt ,name-without-last-character) :prepend ,value)
          _ `(tset vim.opt ,name ,value)))))

(lambda set-local! [name ?value]
  "Set a local vim option using the vim.opt_local api"
  (assert-compile (or (sym? name) (= :string (type name)))
                  "'name' must be either a symbol or a string" name)
  (let [name (tostring name)
        value-is-nil? (= nil ?value)
        name-begins-with-no? (= (name:sub 1 2) :no)
        name (if (and value-is-nil? name-begins-with-no?)
                 (name:sub 3)
                 name)
        value (if value-is-nil?
                  (not name-begins-with-no?)
                  ?value)
        name-last-character (name:sub -1)
        name-without-last-character (name:sub 1 -2)]
    (if (and (list? value)
             (or (= (tostring (. value 1)) :hashfn)
                 (= (tostring (. value 1)) :fn)))
        (let [symbol (gensym-fn!)]
          `(do
             (global ,(sym symbol) ,value)
             (tset vim.opt_local ,name ,(string.format "v:lua.%s()" symbol))))
        (match name-last-character
          "?" `(get-local? ,name-without-last-character)
          "!" `(tset vim.opt_local ,name-without-last-character
                     (not (get-local? ,name-without-last-character)))
          "+" `(: (. vim.opt_local ,name-without-last-character) :append ,value)
          "-" `(: (. vim.opt_local ,name-without-last-character) :remove ,value)
          "^" `(: (. vim.opt_local ,name-without-last-character) :prepend
                  ,value)
          _ `(tset vim.opt_local ,name ,value)))))

(lambda let! [name value]
  "Set vim variable using the vim.[g b w t] api"
  (assert-compile (or (sym? name) (= :string (type name)))
                  "'name' must be either a symbol or a string" name)
  (let [name (tostring name)
        scope (if (> (length (icollect [_ v (ipairs [:g/ :b/ :w/ :t/])]
                               (when (= (name:sub 1 2) v)
                                 v))) 0)
                  (name:sub 1 1)
                  nil)
        name (if (= nil scope) name (name:sub 3))]
    (match scope
      :g `(tset vim.g ,name ,value)
      :b `(tset vim.b ,name ,value)
      :w `(tset vim.w ,name ,value)
      :t `(tset vim.t ,name ,value)
      _ `(tset vim.g ,name ,value))))

(lambda command! [name f]
  (vim.cmd (string.format "command! %s %s" (tostring name) (tostring f))))

(lambda lua-command! [name f]
  (let [f (.. "lua " (tostring f))]
    (command! (tostring name) f)))

(lambda augroup! [name ...]
  "Defines an augroup with a name and auto-commands"
  (assert-compile (or (sym? name) (= :string (type name)))
                  "'name' must be either a symbol or a string" name)
  `(do
     (vim.cmd ,(string.format "augroup %s\nautocmd!" (tostring name)))
     (do
       ,...)
     (vim.cmd "augroup END")
     nil))

(lambda autocmd! [events pattern command]
  "Defines an auto-command"
  (let [events (table.concat (icollect [_ v (ipairs (if (sequence? events)
                                                        events
                                                        [events]))]
                               (tostring v)) ",")
        pattern (table.concat (icollect [_ v (ipairs (if (sequence? pattern)
                                                         pattern
                                                         [pattern]))]
                                (tostring v)) ",")]
    (if (not (list? command))
        `(vim.cmd ,(string.format "autocmd %s %s %s" events pattern command))
        (let [name (gensym-fn!)]
          `(do
             (global ,(sym name) ,command)
             (vim.cmd ,(string.format "autocmd %s %s call v:lua.%s()" events
                                      pattern name)))))))

(lambda buf-augroup! [name ...]
  "Defines an buffer-only augroup with a name and auto-commands"
  (assert-compile (or (sym? name) (= :string (type name)))
                  "'name' must be either a symbol or a string" name)
  `(do
     (vim.cmd ,(string.format "augroup %s\nautocmd! * <buffer>" (tostring name)))
     (do
       ,...)
     (vim.cmd "augroup END")
     nil))

(lambda buf-autocmd! [events command]
  "Defines an auto-command"
  (let [events (table.concat (icollect [_ v (ipairs (if (sequence? events)
                                                        events
                                                        [events]))]
                               (tostring v)) ",")]
    (if (not (list? command))
        `(vim.cmd ,(string.format "autocmd %s <buffer> %s" events command))
        (let [name (gensym-fn!)]
          `(do
             (global ,(sym name) ,command)
             (vim.cmd ,(string.format "autocmd %s <buffer> call v:lua.%s()"
                                      events name)))))))

(lambda map! [mode-list combination command ...]
  "Maps a combination to a command in some modes"
  (let [mode-list (tostring (if (sequence? mode-list) (. mode-list 1) mode-list))
        combination (tostring combination)
        options (collect [_ option (ipairs [...])]
                  (values (tostring option) true))
        out []]
    (if (and (list? command)
             (or (= (tostring (. command 1)) :hashfn)
                 (= (tostring (. command 1)) :fn)))
        (let [name (gensym-fn!)]
          (table.insert out `(global ,(sym name) ,command))
          (each [mode (string.gmatch mode-list ".")]
            (table.insert out
                          `(vim.api.nvim_set_keymap ,mode ,combination
                                                    ,(string.format ":<C-u>call v:lua.%s()<CR>"
                                                                    name)
                                                    ,options))))
        (each [mode (string.gmatch mode-list ".")]
          (table.insert out
                        `(vim.api.nvim_set_keymap ,mode ,combination
                                                  ,(tostring command) ,options))))
    (if (> (length out) 1)
        `(do
           ,(unpack out))
        `,(unpack out))))

(lambda buf-map! [bufnr mode-list combination command ...]
  "Maps a combination to a command in some modes"
  (let [mode-list (tostring (if (sequence? mode-list) (. mode-list 1) mode-list))
        combination (tostring combination)
        options (collect [_ option (ipairs [...])]
                  (values (tostring option) true))
        out []]
    (if (and (list? command)
             (or (= (tostring (. command 1)) :hashfn)
                 (= (tostring (. command 1)) :fn)))
        (let [name (gensym-fn!)]
          (table.insert out `(global ,(sym name) ,command))
          (each [mode (string.gmatch mode-list ".")]
            (table.insert out
                          `(vim.api.nvim_buf_set_keymap ,(or bufnr 0) ,mode
                                                        ,combination
                                                        ,(string.format ":<C-u>call v:lua.%s()<CR>"
                                                                        name)
                                                        ,options))))
        (each [mode (string.gmatch mode-list ".")]
          (table.insert out
                        `(vim.api.nvim_buf_set_keymap ,(or bufnr 0) ,mode
                                                      ,combination
                                                      ,(tostring command)
                                                      ,options))))
    (if (> (length out) 1)
        `(do
           ,(unpack out))
        `,(unpack out))))

(lambda noremap! [mode-list combination command ...]
  "Maps a combination to a command in some modes with the noremap option"
  `(map! ,mode-list ,combination ,command :noremap ,...))

;; Experimenting
;; FIXME `which-key` popup doesn't show description
(lambda wk-map! [options lhs rhs ?name]
  (let [options (if (table? options) options [options])
        [modes & options] options
        modes (map #$ (tostring modes))
        options (map #(tostring $) options)
        lhs (tostring lhs)
        rhs (if (sym? rhs) (tostring rhs) rhs)]
    (if (exists? :which-key)
        (let [statements (icollect [_ mode (ipairs modes)]
                           `((. (require :which-key) :register) {,lhs [,rhs
                                                                       ,?name]}
                                                                {:prefix :<Leader>
                                                                 :mode ,mode
                                                                 :buffer ,(when (contains? options
                                                                                           :buffer)
                                                                            0)
                                                                 :silent ,(when (contains? options
                                                                                           :silent)
                                                                            true)
                                                                 :noremap ,(if (not (contains? options
                                                                                               :noremap))
                                                                               false)
                                                                 :nowait ,(when (contains? options
                                                                                           :nowait)
                                                                            true)}))]
          (if (> (length statements) 1)
              `(do
                 ,(unpack statements))
              `,(unpack statements)))
        (let [buffer? (contains? options :buffer)
              options (collect [_ option (ipairs options)]
                        (when (not= :buffer option)
                          (values (tostring option) true)))]
          (if (function? rhs)
              (let [fnsym (gensym-fn!)
                    statements (icollect [_ mode (ipairs modes)]
                                 (if buffer?
                                     `(vim.api.nvim_buf_set_keymap 0 ,mode ,lhs
                                                                   ,(string.format ":<C-u>call v:lua.%s()<CR>"
                                                                                   fnsym)
                                                                   ,options)
                                     `(vim.api.nvim_set_keymap ,mode ,lhs
                                                               ,(string.format ":<C-u>call v:lua.%s()<CR>"
                                                                               fnsym)
                                                               ,options)))
                    statements [`(global ,(sym fnsym) ,rhs)
                                (unpack statements)]]
                `(do
                   ,(unpack statements)))
              (let [statements (icollect [_ mode (ipairs modes)]
                                 (if buffer?
                                     `(vim.api.nvim_buf_set_keymap 0 ,mode ,lhs
                                                                   ,rhs ,options)
                                     `(vim.api.nvim_set_keymap ,mode ,lhs ,rhs
                                                               ,options)))]
                (if (> (length statements) 1)
                    `(do
                       ,(unpack statements))
                    `,(unpack statements))))))))

(lambda t [combination]
  "Returns the string with termcodes replaced"
  (assert-compile (or (sym? combination) (= :string (type combination)))
                  "'combination' must be either a symbol or a string"
                  combination)
  `(vim.api.nvim_replace_termcodes ,(tostring combination) true true true))

(lambda unless [condition ...]
  "Takes a single condition and evaluates the rest as a body if it's nil or
  false. This is intended for side-effects."
  `(when (not ,condition)
     ,...))

{: gensym-fn!
 : get?
 : get-local?
 : set!
 : set-local!
 : let!
 : command!
 : lua-command!
 : augroup!
 : buf-augroup!
 : autocmd!
 : buf-autocmd!
 : map!
 : buf-map!
 : noremap!
 : wk-map!
 : t
 : unless}

;; These macros were inspired by https://github.com/tsbohc/zest.nvim.
;; In the documentation folder of this repo you can read an explanation about
;; how they work, but, if you want some macros that are plug-and-play you can
;; use zest, they are less opinionated than mine.
