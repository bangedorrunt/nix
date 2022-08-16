(module plugins.devicons
  {autoload {{: tab} core.icons
             {: setup} nvim-web-devicons}})

(setup
  {:override {:fnl {:icon tab.lua
                    :color :#c2d94c
                    :name :fennel}
              :hy {:icon tab.python
                   :color :#519aba
                   :name :hy}
              :Makefile {:icon tab.large-m
                         :color :#6d8086
                         :name :makefile}
              :toml {:icon tab.document
                     :color :#c2d94c
                     :name :toml}
              :yml {:icon tab.document
                     :color :#c2d94c
                     :name :yaml}
              :conf {:icon tab.document
                     :color :#c2d94c
                     :name :conf}
              :norg {:icon tab.sticky-note
                     :color :#36a3d9
                     :name :norg}}})
