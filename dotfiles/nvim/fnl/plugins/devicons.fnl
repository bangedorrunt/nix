(local {: setup} (require :nvim-web-devicons))
(local {: tab} (require :core.icons))

(setup
  {:override {".zshrc" {:icon tab.terminal-alt
                        :color :#c2d94c
                        :name :Zsh}
              ".zshenv" {:icon tab.terminal-alt
                         :color :#c2d94c
                         :name :Zshenv}
              ".zprofile" {:icon tab.terminal-alt
                           :color :#c2d94c
                           :name :Zshprofile}
              :fnl {:icon tab.lua
                    :color :#c2d94c
                    :name :Fennel}
              :makefile {:icon tab.large-m
                         :color :#6d8086
                         :name :Makefile}
              :toml {:icon tab.document
                     :color :#c2d94c
                     :name :Toml}
              :yml {:icon tab.document
                    :color :#c2d94c
                    :name :Yaml}
              :conf {:icon tab.document
                     :color :#c2d94c
                     :name :conf}
              :norg {:icon tab.sticky-note
                     :color :#36a3d9
                     :name :Neorg}}})
