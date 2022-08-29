;;;; Courtesy of Olical with rocks changes
(fn use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [packer (require :packer)
        {: count : inc : into} (require :core.funs)
        plugin_config (fn [name] (.. "require('plugins." name "')"))
        plugin_init (fn [name] (.. "require('" name "').setup {}"))
        theme_config (fn [name] (.. "require('themer." name "')"))
        pkgs [...]]
    (packer.startup
      {1 (fn [use use-rocks]
           (for [i 1 (count pkgs) 2]
             (let [name (. pkgs i)
                        opts (. pkgs (inc i))]
               (if (. opts :rock)
                   (use-rocks name)
                   (. opts :color)
                   (use (into opts 1 name
                              :event :VimEnter
                              :as :themer
                              :config (theme_config (. opts :color))))
                   (. opts :mod)
                   (use (into opts 1 name :config (plugin_config (. opts :mod))))
                   (. opts :init)
                   (use (into opts 1 name :config (plugin_init (. opts :init))))
                   (use (into opts 1 name))))))
      :config {:compile_path tdt.paths.PACKER_COMPILED_PATH
      :git {:clone_timeout 180 :depth 1}
      ;; FIXME: Temporarily disable this due to
      ;; https://github.com/wbthomason/packer.nvim/issues/751
      ;; :max_jobs 60
      :profile {:enable true :threshold 0}}})))

{: use}
