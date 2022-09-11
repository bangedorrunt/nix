(import-macros {: nmap : noremap : if-let
                : lazyreq : lazyfunc} :core.macros)

(let [{: run! : merge} (lazyfunc :core.funs)
      {: setup : load_extension} (lazyfunc :telescope)
      {: close} (lazyfunc :telescope.actions)
      telescope_builtin (lazyfunc :telescope.builtin)
      ;; https://github.com/nvim-telescope/telescope.nvim/issues/938#issuecomment-916688222
      override_opts
      (fn [opts]
        (->> {:disable_devicons false
              :layout_strategy :bottom_pane
              :layout_config {:prompt_position :bottom}}
             (merge (or opts {}))))
      builtin?
      (fn [_ key]
        (if-let [picker (. telescope_builtin key)]
                (fn [opts] (-> opts override_opts picker))
                (error "Invalid key, please check :h telescope.builtin")))
      builtin (setmetatable {} {:__index builtin?})
      ;; SEE: https://www.reddit.com/r/neovim/comments/p1xj92/make_telescope_git_files_revert_back_to_find
      project
      (fn []
        (let [[ret _] (vim.fn.systemlist "git rev-parse --is-inside-work-tree")
              git? (= ret :true)]
          (if git? (builtin.git_files) (builtin.find_files {:cwd "%:h"}))))]

  (setup {:defaults {:cache_picker {:num_pickers 20}
                     :prompt_prefix "❯ "
                     :selection_caret "❯ "
                     :borderchars ["─" "│" "─" "│" "┌" "┐" "┘" "└"]
                     :path_display ["truncate" "smart"]
                     :winblend 0
                     :sorting_strategy :descending
                     :layout_strategy :cursor
                     :layout_config {:height 0.35}
                     :mappings {:i {:<ESC> close}}
                     :file_ignore_patterns [:.git/
                                           :node_modules/.*
                                           :alfred2/.*]}
                     :extensions {:fzf {:fuzzy true
                                       :override_generic_sorter false
                                       :override_file_sorter true
                                       :case_mode :smart_case}}})

  ;; Load extensions
  (run! load_extension [:fzf :projects])

  ;; Telescope keymaps
  (noremap n nowait "<Leader>ht"       :<Cmd>Telescope<CR>)
  (noremap n nowait "<Leader>p"        "<Cmd>Telescope projects<CR>")
  (noremap n nowait "<Leader><Leader>" project)
  (noremap n nowait "<Leader>;"        builtin.live_grep)
  (noremap n nowait "<Leader>*"        builtin.grep_string)
  (noremap n nowait "<Leader>sg"       builtin.git_files)
  (noremap n nowait "<Leader>sb"       builtin.buffers)
  (noremap n nowait "<Leader>so"       builtin.oldfiles)
  (noremap n nowait "<Leader>sc"       builtin.commands)
  (noremap n nowait "<Leader>sC"       builtin.command_history)
  (noremap n nowait "<Leader>sk"       builtin.keymaps)
  (noremap n nowait "<Leader>sr"       builtin.resume))
