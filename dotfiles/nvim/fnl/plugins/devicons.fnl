(let [{: setup : set_default_icon} (require :nvim-web-devicons)]
  (set_default_icon "" :c2d94c)
  ;; Insert codicons using `C-v u {code}`
  (setup
    {:override {:.zshrc {:icon ""
                         :color :#c2d94c
                         :name :Zsh}
                :.zshenv {:icon ""
                          :color :#c2d94c
                          :name :Zshenv}
                :.zprofile {:icon ""
                            :color :#c2d94c
                            :name :Zshprofile}
                :.bashrc {:icon ""
                          :color :#c2d94c
                          :name :Bashrc}
                :.bash_profile {:icon ""
                                :color :#c2d94c
                                :name :BashProfile}
                :.gitconfig {:icon ""
                             :color :#c2d94c
                             :name :GitConfig}
                :.gitignore {:icon ""
                             :color :#c2d94c
                             :name :GitIgnore}
                :.gitmodules {:icon ""
                              :color :#c2d94c
                              :name :GitModules}
                :.gitattributes {:icon ""
                                 :color :#c2d94c
                                 :name :GitAttributes}
                :lock {:icon ""
                       :color :#c2d94c
                       :name :Lock}
                :fnl {:icon  ""
                      :color :#c2d94c
                      :name :Fennel}
                :makefile {:icon ""
                           :color :#c2d94c
                           :name :Makefile}
                :toml {:icon ""
                       :color :#c2d94c
                       :name :Toml}
                :yml {:icon ""
                      :color :#c2d94c
                      :name :Yaml}
                :conf {:icon ""
                       :color :#c2d94c
                       :name :conf}
                :markdown {:icon ""
                           :color :#c2d94c
                           :name :Markdown}
                :md {:icon ""
                     :color :#c2d94c
                     :name :Md}
                :mdx {:icon ""
                      :color :#c2d94c
                      :name :Mdx}
                :lua {:icon ""
                      :color :#c2d94c
                      :name :Lua}
                :rs {:icon ""
                     :color :#c2d94c
                     :name :Rust}
                :nix {:icon ""
                      :color :#c2d94c
                      :name :Nix}
                :norg {:icon ""
                       :color :#c2d94c
                       :name :Neorg}
                :log {:icon ""
                      :color :#c2d94c
                      :name :Log}
                :ps1 {:icon ""
                      :color :#c2d94c
                      :name :PromptPs1}
                :sh {:icon ""
                     :color :#c2d94c
                     :name :Sh}}}))
