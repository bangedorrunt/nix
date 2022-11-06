(import-macros {: noremap : setup!} :core.macros)

(local {: merge} (require :core.funs))
(local {: load_extension} (require :telescope))
(local {: close : select_horizontal} (require :telescope.actions))
(local builtin (require :telescope.builtin))

(local {:cache-prefix hotpot-cache-prefix} (require :hotpot.api.cache))

(fn project []
  (let [[ret _] (vim.fn.systemlist "git rev-parse --is-inside-work-tree")
        git? (= ret :true)]
    (if git?
      (builtin.git_files)
      (builtin.find_files {:cwd "%:h"}))))

(fn setup []
  (setup! telescope
    {:defaults {:cache_picker {:num_pickers 20}
                :prompt_prefix store.signs.prompt
                :selection_caret store.signs.prompt
                :border false
                :path_display [:truncate :smart]
                :sorting_strategy :ascending
                :layout_strategy :bottom_pane
                :layout_config {:height 0.35}
                :mappings {:i {:<ESC> close
                               :<C-s> select_horizontal}}
                :file_ignore_patterns [:.git/ :node_modules/.* :alfred2/.*]}
     :extensions {:fzf {:fuzzy true
                        :override_generic_sorter false
                        :override_file_sorter true
                        :case_mode :smart_case}}})

  ;; Load extensions
  (load_extension :fzf)

  ;; Telescope keymaps
  (noremap n nowait :<Leader>ht :<Cmd>Telescope<CR>)
  (noremap n nowait :<Leader><Leader> project)
  (noremap n nowait "<Leader>;" builtin.live_grep)
  (noremap n nowait :<Leader>* builtin.grep_string)
  (noremap n nowait :<Leader>sg builtin.git_files)
  (noremap n nowait :<Leader>sb builtin.buffers)
  (noremap n nowait :<Leader>so builtin.oldfiles)
  (noremap n nowait :<Leader>sc builtin.commands)
  (noremap n nowait :<Leader>sC builtin.command_history)
  (noremap n nowait :<Leader>sk builtin.keymaps)
  (noremap n nowait :<Leader>sr builtin.resume)
  ;; Open cached files
  (noremap n nowait :<Leader>hc `(builtin.find_files {:cwd (hotpot-cache-prefix) :hidden true})))

{: setup}
