(import-macros {: noremap : setup*} :core.macros)

(local {: run} (require :core.funs))
(local {: load_extension
        :extensions {:live_grep_args {: live_grep_args}}
        &as telescope} (require :telescope))
(local {: close : select_horizontal : to_fuzzy_refine} (require :telescope.actions))
(local {: grep_string : git_files : find_files : buffers : oldfiles
        : commands : command_history : keymaps : resume} (require :telescope.builtin))
(local {: cache-prefix} (require :hotpot.api.cache))

(fn project []
  (let [[ret _] (vim.fn.systemlist "git rev-parse --is-inside-work-tree")
        git? (= ret :true)]
    (if git?
      (git_files)
      (find_files {:cwd "%:h"}))))

(fn setup []
  (setup* telescope
    {:defaults {:prompt_prefix "❯ "
                :selection_caret "❯ "
                :color_devicons false
                :border false
                :path_display [:truncate :smart]
                :layout_strategy :bottom_pane
                :layout_config {:height 0.35 :prompt_position :bottom}
                :mappings {:i {:<ESC> close
                               :<C-s> select_horizontal
                               :<C-f> to_fuzzy_refine}}
                :file_ignore_patterns [:.git/ :node_modules/.* :alfred2/.*]}})

  ;; Load extensions
  (run load_extension [:fzf :live_grep_args])

  ;; Telescope keymaps
  (noremap n nowait :<Leader>ht :<Cmd>Telescope<CR>)
  (noremap n nowait :<Leader><Leader> project)
  (noremap n nowait "<Leader>;" live_grep_args)
  (noremap n nowait :<Leader>* grep_string)
  (noremap n nowait :<Leader>sg git_files)
  (noremap n nowait :<Leader>sb buffers)
  (noremap n nowait :<Leader>so oldfiles)
  (noremap n nowait :<Leader>sc commands)
  (noremap n nowait :<Leader>sC command_history)
  (noremap n nowait :<Leader>sk keymaps)
  (noremap n nowait :<Leader>sr resume)
  ;; Open cached files
  (noremap n nowait :<Leader>hc `(find_files {:cwd (cache-prefix) :hidden true})))

{: setup}
