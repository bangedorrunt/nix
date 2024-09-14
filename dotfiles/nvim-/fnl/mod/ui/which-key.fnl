(import-macros {: setup*} :core.macros)

(fn setup []
  (let [wk (require :which-key)
        {: run : kvize} (require :core.funs)
        xs [[ :g  :group :+goto ]
            [ :gz :group :+surround ]
            [ "]" :group :+next ]
            [ "[" :group :+prev ]
            [ :<leader><tab> :group :+tabs ]
            [ :<leader>b     :group :+buffer ]
            [ :<leader>c     :group :+code ]
            [ :<leader>f     :group :+file/find ]
            [ :<leader>g     :group :+git ]
            [ :<leader>gh    :group :+hunks ]
            [ :<leader>gt    :group :+toggle ]
            [ :<leader>h     :group :+help ]
            [ :<leader>hl    :group :+lazy.nvim ]
            [ :<leader>q     :group :+quit/session ]
            [ :<leader>s     :group :+search ]
            [ :<leader>w     :group :+windows ]
            [ :<leader>t     :group :+toggle ]
            [ :<leader>x     :group :+text/edit ]]]
    (run
      (fn [[k & args]]
        (table.insert store.wks (kvize args {1 k})))
        xs)
    (setup* wk)
    (wk.add store.wks)))

{: setup}
