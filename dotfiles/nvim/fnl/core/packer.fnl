(module core.packer
  {autoload {{: assoc} aniseed.core
             : packer}
   require-macros [core.macros]})

;;;; Packer
(defn- plugin-config [name]
  "A shortcut to building a require string for your plugin
  configuration. Intended for use with packer's config or setup
  configuration options. Will prefix the name with `plugins.`
  before requiring."
  (.. "require('plugins." name "')"))

(defn- plugin-init [name]
  "A shortcut to building a require string for your plugin
  configuration. Intended for use with packer's config or setup
  configuration options."
  (.. "require('" name "').setup {}"))

(defn- colorscheme-init [name]
  "A shortcut to building a require string for your colorscheme
  configuration. Intended for use with packer's config or setup
  configuration options. Will prefix the name with `colorscheme.`
  before requiring."
  (.. "require('colorschemes." name "')"))

;;;; Courtesy of Olical with rocks changes
(defn use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup {1 (fn [use use-rocks]
                         (for [i 1 (length pkgs) 2]
                           (let [name (. pkgs i)
                                 opts (. pkgs (+ i 1))]
                             (if (. opts :rock)
                               (use-rocks name)
                               (. opts :colorscheme)
                               (use (assoc opts 1 name
                                           :event :VimEnter
                                           :as :colorscheme
                                           :config (colorscheme-init (. opts :colorscheme))))
                               (. opts :mod)
                               (use (assoc opts 1 name :config (plugin-config (. opts :mod))))
                               (. opts :init)
                               (use (assoc opts 1 name :config (plugin-init (. opts :init))))
                               (use (assoc opts 1 name))))))
                     :config {:compile_path tdt.paths.PACKER_COMPILED_PATH
                              :git {:clone_timeout 180 :depth 1}
                              ;; Temporarily disable this due to
                              ;; https://github.com/wbthomason/packer.nvim/issues/751
                              ;; :max_jobs 60
                              :profile {:enable true :threshold 0}}})))
