(module plugins.telescope
  {autoload {{: run! : merge} core.utils
             : telescope
             telescope-utils telescope.utils
             telescope-actions telescope.actions
             telescope-builtin telescope.builtin}
   require-macros [core.macros]})


; TODO: move telescope settings to global table `tdt.plugins.telescope`
(telescope.setup {:defaults {:cache_picker {:num_pickers 20}
                             :prompt_prefix "❯ "
                             :selection_caret "❯ "
                             :borderchars ["─"
                                           "│"
                                           "─"
                                           "│"
                                           "┌"
                                           "┐"
                                           "┘"
                                           "└"]
                             :path_display ["truncate" "smart"]
                             :disable_devicons true
                             :winblend 0
                             :sorting_strategy :descending
                             :layout_strategy :cursor
                             :layout_config {:height 0.35}
                             :mappings {:i {:<ESC> telescope-actions.close}}
                             :file_ignore_patterns [:.git/
                                                    :node_modules/.*
                                                    :.neuron/.*
                                                    :alfred2/.*]}
                  :extensions {:fzf {:fuzzy true
                                     :override_generic_sorter false
                                     :override_file_sorter true
                                     :case_mode :smart_case}}})

; Load extensions
(run! telescope.load_extension [:fzf :projects])

; https://github.com/nvim-telescope/telescope.nvim/issues/938#issuecomment-916688222
(def- override-opts {:previewer false
                     :disable_devicons true})
(def- pickers
  (setmetatable {}
                {:__index (fn [_ key]
                            (when (= (. telescope-builtin key) nil)
                              (error "Invalid key, please check :h telescope.builtin")
                              (lua "return "))
                            (fn [opts]
                              (set-forcibly! opts (merge (or opts {}) override-opts ))
                              ((. telescope-builtin key) opts)))}))

(defn- project-files []
  (let [ok (pcall pickers.git_files)]
    (when (not ok) (pickers.find_files))))

; Telescope keymaps
; fnlfmt: skip
(noremap n nowait "<Leader>ht"       :<Cmd>Telescope<CR>)
(noremap n nowait "<Leader><Leader>" '(project-files))
(noremap n nowait "<Leader>;"        '(pickers.live_grep))
(noremap n nowait "<Leader>*"        '(pickers.grep_string))
(noremap n nowait "<Leader>sg"       '(pickers.git_files))
(noremap n nowait "<Leader>sb"       '(pickers.buffers))
(noremap n nowait "<Leader>so"       '(pickers.oldfiles))
(noremap n nowait "<Leader>sc"       '(pickers.commands))
(noremap n nowait "<Leader>sC"       '(pickers.command_history))
(noremap n nowait "<Leader>sk"       '(pickers.keymaps))
(noremap n nowait "<Leader>sr"       '(pickers.resume))
