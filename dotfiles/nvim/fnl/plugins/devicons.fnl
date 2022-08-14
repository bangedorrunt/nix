(module plugins.devicons
  {autoload {{: tab} core.icons
             {: setup} nvim-web-devicons}})

(setup
  {:override {:fnl {:icon tab.lua
                    :color :#c2d94c
                    :name :Fennel}
              :hy {:icon tab.python
                   :color :#519aba
                   :name :Hy}
              :Makefile {:icon tab.large-m
                         :color :#6d8086
                         :name :Makefile}
              :norg {:icon tab.sticky-note
                     :color :#36a3d9
                     :name :norg}}})
