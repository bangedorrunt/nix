(import-macros {: setup!} :core.macros)

(fn setup []
  (setup! todo-comments
          {:keywords {:REVIEW {:icon "" :color :info}
                      :REFACTOR {:icon "" :color :hint}}
           :highlight {:keyword :wide_fg
                       :after ""
                       :pattern ".*<(KEYWORDS)\\s*"}
           :search {:pattern "\b(KEYWORDS)\b"}}))

{: setup}
