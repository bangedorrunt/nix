(module core.base)

;; (import-macros {: let!} :core.macros)

(def- os-name (. (vim.loop.os_uname) :sysname))

;; fnlfmt: skip
(def- path-sep (match os-name
                 :Windows :\\
                 _ :/))

(def- data-dir (string.format "%s/site/" (vim.fn.stdpath :data)))

(global tdt
        {:signs {:error " "
                 :warning " "
                 :hint " "
                 :information " "
                 :prompt "❯"}
         :pallete {:dark {:tokyonight "#1a1b26" :monokaipro_spectrum "#222222"}
                   :light {:tokyonight "#e1e2e7" :gruvbox "#fbf1c7"}}
         :paths {:IS_MAC (= os-name :Darwin)
                 :IS_LINUX (= os-name :Linux)
                 :IS_WINDOWS (= os-name :Windows)
                 :PATH_SEP path-sep
                 :VIM_PATH (vim.fn.stdpath :config)
                 :HOME (os.getenv :HOME)
                 :CACHE_DIR (vim.fn.stdpath :cache)
                 :DATA_DIR data-dir
                 :PACKER_DIR (.. data-dir :pack/packer/opt/packer.nvim)
                 :PACKER_COMPILED_PATH (.. data-dir :lua/packer_compiled.lua)}})

;; Because this is one-off setup so it's better to do with external shell script
;; KEEP for reference only
;; Create cache dir and subs dir
;; (fn createdir []
;;        (let [CACHE_DIR tdt.paths.CACHE_DIR
;;              NVIM_DATA_DIR [(.. CACHE_DIR path-sep :backup)
;;                             (.. CACHE_DIR path-sep :session)
;;                             (.. CACHE_DIR path-sep :swap)
;;                             (.. CACHE_DIR path-sep :tags)
;;                             (.. CACHE_DIR path-sep :undo)]]
;;          ;; Because `CACHE_DIR' is automatically created at first start
;;          ;; so we don't need to create this folder manually
;;          ;; (when (= (vim.fn.isdirectory CACHE_DIR) 0)
;;          ;;  (os.execute (.. "mkdir -p " CACHE_DIR)))
;;          (each [_ v (pairs NVIM_DATA_DIR)]
;;            (when (= (vim.fn.isdirectory v) 0)
;;              (os.execute (.. "mkdir -p " v))))))
;; (createdir)

;; Disable built-in plugins
((fn []
   (let [built-ins [:netrw
                    :netrwPlugin
                    :netrwSettings
                    :netrwFileHandlers
                    :gzip
                    :zip
                    :zipPlugin
                    :tar
                    :tarPlugin
                    :getscript
                    :getscriptPlugin
                    :vimball
                    :vimballPlugin
                    :2html_plugin
                    :logipat
                    :rrhelper
                    :spellfile_plugin
                    :matchit]
         providers [:perl
                    :python
                    :python3
                    :node
                    :ruby]]
     (each [_ v (ipairs built-ins)]
       (let [b (.. :loaded_ v)]
         (tset vim.g b 1)))
     (each [_ v (ipairs providers)]
       (let [p (.. :loaded_ v :_provider)]
         (tset vim.g p 0))))))

;; (let! g/python3_host_prog (.. tdt.paths.HOME "/.venv/bin/python"))
