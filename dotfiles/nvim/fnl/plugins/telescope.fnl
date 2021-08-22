(module plugins.telescope {autoload {cljfun bulb
                                     telescope telescope
                                     telescope/utils telescope.utils
                                     telescope/themes telescope.themes
                                     telescope/previewers telescope.previewers
                                     telescope/actions telescope.actions
                                     telescope/builtin telescope.builtin}})

(import-macros {: nmap! : noremap!} :core.macros)

(def- {: run!} cljfun)

;; TODO move telescope settings to global table `tdt.plugins.telescope`
(telescope.setup {:defaults {:prompt_prefix "❯ "
                             :selection_caret "❯ "
                             :borderchars ["─"
                                           "│"
                                           "─"
                                           "│"
                                           "┌"
                                           "┐"
                                           "┘"
                                           "└"]
                             :disable_devicons true
                             :winblend 0
                             :sorting_strategy :descending
                             :layout_strategy :flex
                             :layout_config {:prompt_position "bottom"
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
     (run! telescope.load_extension))

;;;; Telescope keymaps  
;; SEE: https://www.reddit.com/r/neovim/comments/p1xj92/make_telescope_git_files_revert_back_to_find
;; (noremap! [n :nowait] "<Leader><Leader>" "<Cmd>Telescope find_files<CR>")
(defn- project_files []
       (let [(_ ret _) (telescope/utils.get_os_command_output [:git :rev-parse :--is-inside-work-tree])]
         (if (= ret 0) (telescope/builtin.git_files) (telescope/builtin.find_files))))

(->> {: project_files}
     (set tdt.telescope))

(noremap! [n :nowait] :<Leader><Leader>
          "<Cmd>lua tdt.telescope.project_files()<CR>")
(noremap! [n :nowait] :<Leader>ht :<Cmd>Telescope<CR>)
(noremap! [n :nowait] "<Leader>;" "<Cmd>Telescope live_grep<CR>")
(noremap! [n :nowait] :<Leader>* "<Cmd>Telescope grep_string<CR>")
(noremap! [n :nowait] :<Leader>0
          "<Cmd>lua require'telescope.builtin'.find_files{cwd = os.getenv('HOME')..'/nix/dotfiles/nvim'}<CR>")

(noremap! [n :nowait] :<Leader>9
          "<Cmd>lua require'telescope.builtin'.find_files{cwd = os.getenv('HOME')..'/nix'}<CR>")

(noremap! [n :nowait] :<Leader>sg "<Cmd>Telescope git_files<CR>")
(noremap! [n :nowait] :<Leader>sb "<Cmd>Telescope buffers<CR>")
(noremap! [n :nowait] :<Leader>so "<Cmd>Telescope oldfiles<CR>")
(noremap! [n :nowait] :<Leader>sc "<Cmd>Telescope commands<CR>")
(noremap! [n :nowait] :<Leader>sC "<Cmd>Telescope command_history<CR>")
(noremap! [n :nowait] :<Leader>sk "<Cmd>Telescope keymaps<CR>")
