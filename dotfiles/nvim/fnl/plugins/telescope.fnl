(module plugins.telescope
        {autoload {telescope telescope
                   telescope-themes telescope.themes
                   telescope-previewers telescope.previewers
                   telescope-actions telescope.actions
                   telescope-builtin telescope.builtin}
         require-macros [core.macros]})

;; SEE: https://github.com/folke/dot/blob/master/config/nvim/lua/config/telescope.lua#L70
(fn project_files [opts]
  (set-forcibly! opts (or opts {}))
  ;; (def- _git-pwd
  ;;   (. (vim.fn.systemlist "git rev-parse --show-toplevel") 1))
  (when (not= vim.v.shell_error 0)
    (def- client (. (vim.lsp.get_active_clients) 1))
    (when client
      (set opts.cwd client.config.root_dir))
    (telescope-builtin.find_files opts)
    (lua "return "))
  (telescope-builtin.git_files opts))

(set tdt.telescope {: project_files})

;; TODO move telescope settings to global table `tdt.plugins.telescope`
(telescope.setup {:defaults (telescope-themes.get_ivy {:prompt_prefix "❯ "
                                                       :selection_caret "❯ "
                                                       :disable_devicons true
                                                       :winblend 0
                                                       :sorting_strategy :ascending
                                                       :layout_strategy :bottom_pane
                                                       :mappings {:i {:<ESC> telescope-actions.close}}
                                                       :layout_config {:prompt_position :bottom
                                                                       :height 0.4}
                                                       :file_ignore_patterns [:.git/
                                                                              :node_modules/.*
                                                                              :.neuron/.*
                                                                              :alfred2/.*]
                                                       :file_previewer telescope-previewers.vim_buffer_cat.new
                                                       :grep_previewer telescope-previewers.vim_buffer_vimgrep.new
                                                       :qflist_previewer telescope-previewers.vim_buffer_qflist.new})
                  :extensions {:fzy_native {:override_generic_sorter false
                                            :override_file_sorter true}
                               :fzf {:fuzzy true
                                     :override_generic_sorter false
                                     :override_file_sorter true
                                     :case_mode :smart_case}}})

(telescope.load_extension :fzf)

(noremap! [n] :<Leader>ht :<Cmd>Telescope<CR> :nowait)
;; (noremap! [ n ] "<Leader><Leader>" "<Cmd>Telescope find_files<CR>" :nowait)
(noremap! [n] :<Leader><Leader> "<Cmd>lua tdt.telescope.project_files()<CR>"
          :nowait)

(noremap! [n] "<Leader>;" "<Cmd>Telescope live_grep<CR>" :nowait)
(noremap! [n] :<Leader>* "<Cmd>Telescope grep_string<CR>" :nowait)
(noremap! [n] :<Leader>0
          "<Cmd>lua require'telescope.builtin'.find_files{cwd = os.getenv('HOME')..'/nix/dotfiles/nvim'}<CR>"
          :nowait)

(noremap! [n] :<Leader>9
          "<Cmd>lua require'telescope.builtin'.find_files{cwd = os.getenv('HOME')..'/nix'}<CR>"
          :nowait)

(noremap! [n] :<Leader>sg "<Cmd>Telescope git_files<CR>" :nowait)
(noremap! [n] :<Leader>sb "<Cmd>Telescope buffers<CR>" :nowait)
(noremap! [n] :<Leader>so "<Cmd>Telescope oldfiles<CR>" :nowait)
(noremap! [n] :<Leader>sc "<Cmd>Telescope commands<CR>" :nowait)
(noremap! [n] :<Leader>sC "<Cmd>Telescope command_history<CR>" :nowait)
(noremap! [n] :<Leader>sk "<Cmd>Telescope keymaps<CR>" :nowait)
