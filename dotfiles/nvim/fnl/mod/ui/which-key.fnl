(import-macros {: setup*} :core.macros)

(fn setup []
  (let [wk (require :which-key)]
    (setup* wk)
    (wk.register
      {:mode [:n :v]
       :g {:name :+goto}
       :gz {:name :+surround}
       "]" {:name :+next}
       "[" {:name :+prev}
       :<leader><tab> {:name :+tabs}
       :<leader>b {:name :+buffer}
       :<leader>c {:name :+code}
       :<leader>f {:name :+file/find}
       :<leader>g {:name :+git}
       :<leader>gh {:name :+hunks}
       :<leader>gt {:name :+toggle}
       :<leader>h {:name :+help}
       :<leader>q {:name :+quit/session}
       :<leader>s {:name :+search}
       :<leader>w {:name :+windows}
       :<leader>t {:name :+toggle}
       :<leader>x {:name :+text/edit}})))

{: setup}
