(import-macros {: setup*} :core.macros)

(local orgmode (require :orgmode))

(fn setup []
  (setup* orgmode)
  (orgmode.setup_ts_grammar))

{: setup}
