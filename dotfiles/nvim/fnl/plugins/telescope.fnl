(module plugins.telescope
  {autoload {{: run! : merge} core.utils
             {: setup : load_extension} telescope
             {: get_os_command_output} telescope.utils
             {: close} telescope.actions
             builtin telescope.builtin}
   require-macros [core.macros]})


;; TODO: move telescope settings to global table `tdt.plugins.telescope`
(setup {:defaults {:cache_picker {:num_pickers 20}
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
                   :winblend 0
                   :sorting_strategy :descending
                   :layout_strategy :cursor
                   :layout_config {:height 0.35}
                   :mappings {:i {:<ESC> close}}
                   :file_ignore_patterns [:.git/
                                          :node_modules/.*
                                          :.neuron/.*
                                          :alfred2/.*]}
        :extensions {:fzf {:fuzzy true
                           :override_generic_sorter false
                           :override_file_sorter true
                           :case_mode :smart_case}}})

;; Load extensions
(run! load_extension [:fzf :projects])

;; https://github.com/nvim-telescope/telescope.nvim/issues/938#issuecomment-916688222
(defn- custom-opts [opts]
  (->> {:disable_devicons true} (merge (or opts {}))))

(defn- builtin-key? [_ key]
  (if-let [picker (. builtin key)]
    #(-> $ custom-opts picker)
    (error "Invalid key, please check :h telescope.builtin")))

(def- builtin-key (setmetatable {} {:__index builtin-key?}))

;; SEE: https://www.reddit.com/r/neovim/comments/p1xj92/make_telescope_git_files_revert_back_to_find
(defn- project []
  (let [(_ ret _) (get_os_command_output [:git :rev-parse :--is-inside-work-tree])]
    (if (= ret 0)
      (builtin-key.git_files)
      (builtin-key.find_files))))

;; Telescope keymaps
;; fnlfmt: skip
(noremap n nowait "<Leader>ht"       :<Cmd>Telescope<CR>)
(noremap n nowait "<Leader><Leader>" '(project))
(noremap n nowait "<Leader>;"        '(builtin-key.live_grep))
(noremap n nowait "<Leader>*"        '(builtin-key.grep_string))
(noremap n nowait "<Leader>sg"       '(builtin-key.git_files))
(noremap n nowait "<Leader>sb"       '(builtin-key.buffers))
(noremap n nowait "<Leader>so"       '(builtin-key.oldfiles))
(noremap n nowait "<Leader>sc"       '(builtin-key.commands))
(noremap n nowait "<Leader>sC"       '(builtin-key.command_history))
(noremap n nowait "<Leader>sk"       '(builtin-key.keymaps))
(noremap n nowait "<Leader>sr"       '(builtin-key.resume))
