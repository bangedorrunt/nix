(module plugins.telescope
  {autoload {: telescope
             telescope/utils telescope.utils
             telescope/actions telescope.actions
             telescope/builtin telescope.builtin}
   require-macros [core.macros]})

(defn merge [t1 t2]
  (vim.tbl_extend "keep" t1 t2))

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
                             :layout_strategy :bottom_pane
                             :layout_config {:prompt_position "bottom"
                                             :height 0.8}
                             :mappings {:i {:<ESC> telescope/actions.close}}
                             :file_ignore_patterns [:.git/
                                                    :node_modules/.*
                                                    :.neuron/.*
                                                    :alfred2/.*]}
                  :extensions {:fzf {:fuzzy true
                                     :override_generic_sorter false
                                     :override_file_sorter true
                                     :case_mode :smart_case}}})

; Load extensions
(telescope.load_extension :fzf)
(telescope.load_extension :projects)

; https://github.com/nvim-telescope/telescope.nvim/issues/938#issuecomment-916688222
(def override-opts {:disable_devicons true})
(def pickers
    (setmetatable {}
                  {:__index (fn [_ key]
                              (when (= (. telescope/builtin key) nil)
                                (error "Invalid key, please check :h telescope.builtin")
                                (lua "return "))
                              (fn [opts]
                                (set-forcibly! opts (merge (or opts {}) override-opts ))
                                ((. telescope/builtin key) opts)))}))

; https://www.reddit.com/r/neovim/comments/p1xj92/make_telescope_git_files_revert_back_to_find
(defn project-files []
  (let [(_ ret _) (telescope/utils.get_os_command_output [:git :rev-parse :--is-inside-work-tree])]
    (if (= ret 0) (pickers.git_files) (pickers.find_files))))


; fnlfmt: skip
(def live-grep       pickers.live_grep)
(def grep-string     pickers.grep_string)
(def find-files      pickers.find_files)
(def git-files       pickers.git_files)
(def oldfiles        pickers.oldfiles)
(def buffers         pickers.buffers)
(def commands        pickers.commands)
(def command-history pickers.command_history)
(def keymaps         pickers.keymaps)
(def resume          pickers.pickers)
(def nvim-files      #(pickers.find_files {:cwd tdt.paths.NVIM_PATH}))
(def nix-files       #(pickers.find_files {:cwd (.. tdt.paths.HOME "/nix")}))

; Telescope keymaps
; fnlfmt: skip
(noremap n nowait "<Leader>ht"       "<Cmd>Telescope<CR>")
(noremap n nowait "<Leader><Leader>" '(project-files))
(noremap n nowait "<Leader>;"        '(live-grep))
(noremap n nowait "<Leader>*"        '(grep-string))
(noremap n nowait "<Leader>0"        '(nvim-files))
(noremap n nowait "<Leader>9"        '(nix-files))
(noremap n nowait "<Leader>sg"       '(git-files))
(noremap n nowait "<Leader>sb"       '(buffers))
(noremap n nowait "<Leader>so"       '(oldfiles))
(noremap n nowait "<Leader>sc"       '(commands))
(noremap n nowait "<Leader>sC"       '(command-history))
(noremap n nowait "<Leader>sk"       '(keymaps))
(noremap n nowait "<Leader>sr"       '(resume))
