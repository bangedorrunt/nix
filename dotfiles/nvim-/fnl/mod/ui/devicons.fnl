(import-macros {: setup!} :core.macros)

(fn setup []
  ;; Insert codicons using `C-v u {code}`
  (setup! nvim-web-devicons
    {:color_icons false
     :override
      {:.zshrc {:icon "" :name :Zsh}
       :.zshenv {:icon "" :name :Zshenv}
       :.zprofile {:icon "" :name :Zshprofile}
       :.bashrc {:icon "" :name :Bashrc}
       :.bash_profile {:icon "" :name :BashProfile}
       :.gitconfig {:icon "" :name :GitConfig}
       :.gitignore {:icon "" :name :GitIgnore}
       :.gitmodules {:icon "" :name :GitModules}
       :.gitattributes {:icon "" :name :GitAttributes}
       :lock {:icon "" :name :Lock}
       :fnl {:icon "" :name :Fennel}
       :makefile {:icon "" :name :Makefile}
       :toml {:icon "" :name :Toml}
       :yml {:icon "" :name :Yaml}
       :conf {:icon "" :name :conf}
       :markdown {:icon "" :name :Markdown}
       :md {:icon "" :name :Md}
       :mdx {:icon "" :name :Mdx}
       :lua {:icon "" :name :Lua}
       :rs {:icon "" :name :Rust}
       :nix {:icon "" :name :Nix}
       :norg {:icon "" :name :Neorg}
       :LICENSE {:icon "" :name :License}
       :log {:icon "" :name :Log}
       :ps1 {:icon "" :name :PromptPs1}
       :sh {:icon "" :name :Sh}}}))

{: setup}
