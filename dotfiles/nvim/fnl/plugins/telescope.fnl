(module plugins.telescope {autoload {core aniseed.core
                                     nvim aniseed.nvim
                                     : telescope 
                                     telescope/utils telescope.utils
                                     telescope/previewers telescope.previewers
                                     telescope/actions telescope.actions
                                     telescope/builtin telescope.builtin}
                           require-macros [core.macros]})


;; TODO: move telescope settings to global table `tdt.plugins.telescope`
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
                             :sorting_strategy :ascending
                             :layout_strategy :bottom_pane
                             :layout_config {:prompt_position "top"
                                             :height 0.8}
                             :mappings {:i {:<ESC> telescope/actions.close}}
                             :file_ignore_patterns [:.git/
                                                    :node_modules/.*
                                                    :.neuron/.*
                                                    :alfred2/.*]
                             :file_previewer telescope/previewers.vim_buffer_cat.new
                             :grep_previewer telescope/previewers.vim_buffer_vimgrep.new
                             :qflist_previewer telescope/previewers.vim_buffer_qflist.new}
                  :extensions {:fzf {:fuzzy true
                                     :override_generic_sorter false
                                     :override_file_sorter true
                                     :case_mode :smart_case}}})

;;;; Load extensions
;; (telescope.load_extension :fzf)
;; (telescope.load_extension :projects)
(->> [:fzf :projects]
     (core.run! telescope.load_extension))

;; https://github.com/nvim-telescope/telescope.nvim/issues/938#issuecomment-916688222
(def override-opts {:disable_devicons true})
(def pickers
    (setmetatable {}
                  {:__index (fn [_ key]
                              (when (= (. telescope/builtin key) nil)
                                (error "Invalid key, please check :h telescope.builtin")
                                (lua "return "))
                              (fn [opts]
                                (set-forcibly! opts (core.merge! (or opts {}) override-opts ))
                                ((. telescope/builtin key) opts)))}))

;; https://www.reddit.com/r/neovim/comments/p1xj92/make_telescope_git_files_revert_back_to_find
(defn project-files []
  (let [(_ ret _) (telescope/utils.get_os_command_output [:git :rev-parse :--is-inside-work-tree])]
    (if (= ret 0) (pickers.git_files) (pickers.find_files))))

;; fnlfmt: skip
(->> {:project_files   project-files
      :live_grep       pickers.live_grep
      :grep_string     pickers.grep_string
      :find_files      pickers.find_files
      :git_files       pickers.git_files
      :oldfiles        pickers.oldfiles
      :buffers         pickers.buffers
      :commands        pickers.commands
      :command_history pickers.command_history
      :keymaps         pickers.keymaps
      :pickers         pickers.pickers}
     (set tdt.telescope))

;;;; Telescope keymaps
;; fnlfmt: skip
(noremap [n :nowait] "<Leader><Leader>" "<Cmd>lua tdt.telescope.project_files()<CR>")
(noremap [n :nowait] "<Leader>ht"       "<Cmd>Telescope<CR>")
(noremap [n :nowait] "<Leader>;"        "<Cmd>lua tdt.telescope.live_grep()<CR>")
(noremap [n :nowait] "<Leader>*"        "<Cmd>lua tdt.telescope.grep_string()<CR>")
(noremap [n :nowait] "<Leader>0"        "<Cmd>lua tdt.telescope.find_files {cwd = os.getenv('HOME')..'/nix/dotfiles/nvim',}<CR>")
(noremap [n :nowait] "<Leader>9"        "<Cmd>lua tdt.telescope.find_files {cwd = os.getenv('HOME')..'/nix',}<CR>")
(noremap [n :nowait] "<Leader>sg"       "<Cmd>lua tdt.telescope.git_files()<CR>")
(noremap [n :nowait] "<Leader>sb"       "<Cmd>lua tdt.telescope.buffers()<CR>")
(noremap [n :nowait] "<Leader>so"       "<Cmd>lua tdt.telescope.oldfiles()<CR>")
(noremap [n :nowait] "<Leader>sc"       "<Cmd>lua tdt.telescope.commands()<CR>")
(noremap [n :nowait] "<Leader>sC"       "<Cmd>lua tdt.telescope.command_history()<CR>")
(noremap [n :nowait] "<Leader>sk"       "<Cmd>lua tdt.telescope.keymaps()<CR>")
(noremap [n :nowait] "<Leader>sr"       "<Cmd>lua tdt.telescope.pickers()<CR>")
