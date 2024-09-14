(import-macros {: setup!} :core.macros)

;; SEE: https://github.com/Trouble-Truffle/Perigord-Nvim/blob/main/fnl/UI/Startup.fnl
(fn setup []
  (setup! dashboard
          {:theme :hyper
           :config
           {:header ["                                                                                                "
                     "                                                                                                "
                     "                                                                                                "
                     "                                                                                                "
                     "                                                                                                "
                     "         -╲         '-                                                                          "
                     "       -' :╲        │ '-                                                                        "
                     "     -'   : ╲       │   '-                          │MMM│                                       "
                     "   -'·    :  ╲      │     '-                        │WWW│                                       "
                     "  '.-.·   :   ╲     │       │                                                                   "
                     "  │. .-·  :    ╲    │       │    MMM=         =MMM  │MMM│  │M│  +===+   +====+                  "
                     "  │ . .-· :     ╲   │       │    ╲HHB`       'BHH╱  │HHH│  │H│╲╱sMMMs╲_╱sMMMMs╲                 "
                     "  │. . .-·;      ╲  │       │     ╲HHH╲     ╱HHH╱   │HHH│  │HBBWWWWWHMMMHWWWW:B╲                "
                     "  │ . . .-│       ╲ │       │      ╲HHH╲   ╱HHH╱    │HHH│  │HK╱     ╲KYK╱    ╲KH│               "
                     "  │. . . .│╲       ╲│       │       ╲HHH╲ ╱HHH╱     │HHH│  │H│       │H│      │H│               "
                     "  │ . . . │ ╲       ;-      │        ╲HHHVHHH╱      │HHH│  │H│       │H│      │H│               "
                     "  │. . . .│  ╲      :·-     │         ╲HHHHH╱       │HHH│  │H│       │H│      │H│               "
                     "  │ . . . │   ╲     : .-    │          ╲HHH╱        │HHH│  │H│       │H│      │H│               "
                     "  │. . . .│    ╲    :. .-   │           ╲W╱         │WWW│  │W│       │W│      │W│               "
                     "  `- . . .│     ╲   : . .- -'                                                                   "
                     "    `- . .│      ╲  :. ..-'                                                                     "
                     "      `-. │       ╲ :..-'                                                                       "
                     "         `│        ╲;-'                                                                         "
                     "                                                                                                "
                     "                                                                                                "]}
           }))
{: setup}
