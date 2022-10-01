(import-macros {: lazyreq} :core.macros)

(local {:set_default_icon set-default-icon
        &as devicons} (lazyreq :nvim-web-devicons))

(fn setup []
  (set-default-icon "" "#ff79c6")
  ;; Insert codicons using `C-v u {code}`
  (devicons.setup
    {:override
      {:.zshrc {:icon "" :color "#ff79c6" :name :Zsh}
       :.zshenv {:icon "" :color "#ff79c6" :name :Zshenv}
       :.zprofile {:icon "" :color "#ff79c6" :name :Zshprofile}
       :.bashrc {:icon "" :color "#ff79c6" :name :Bashrc}
       :.bash_profile {:icon "" :color "#ff79c6" :name :BashProfile}
       :.gitconfig {:icon "" :color "#ff79c6" :name :GitConfig}
       :.gitignore {:icon "" :color "#ff79c6" :name :GitIgnore}
       :.gitmodules {:icon "" :color "#ff79c6" :name :GitModules}
       :.gitattributes {:icon "" :color "#ff79c6" :name :GitAttributes}
       :lock {:icon "" :color "#ff79c6" :name :Lock}
       :fnl {:icon "" :color "#ff79c6" :name :Fennel}
       :makefile {:icon "" :color "#ff79c6" :name :Makefile}
       :toml {:icon "" :color "#ff79c6" :name :Toml}
       :yml {:icon "" :color "#ff79c6" :name :Yaml}
       :conf {:icon "" :color "#ff79c6" :name :conf}
       :markdown {:icon "" :color "#ff79c6" :name :Markdown}
       :md {:icon "" :color "#ff79c6" :name :Md}
       :mdx {:icon "" :color "#ff79c6" :name :Mdx}
       :lua {:icon "" :color "#ff79c6" :name :Lua}
       :rs {:icon "" :color "#ff79c6" :name :Rust}
       :nix {:icon "" :color "#ff79c6" :name :Nix}
       :norg {:icon "" :color "#ff79c6" :name :Neorg}
       :LICENSE {:icon "" :color "#ff79c6" :name :License}
       :log {:icon "" :color "#ff79c6" :name :Log}
       :ps1 {:icon "" :color "#ff79c6" :name :PromptPs1}
       :sh {:icon "" :color "#ff79c6" :name :Sh}}}))

{: setup}
