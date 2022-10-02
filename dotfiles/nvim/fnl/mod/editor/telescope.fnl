(import-macros {: noremap : lazyreq : lazyfunc} :core.macros)

(local {: run! : merge} (lazyfunc :core.funs))
(local {: load_extension &as telescope} (lazyfunc :telescope))
(local {: close : select_horizontal} (lazyfunc :telescope.actions))
(local telescope-builtin (lazyfunc :telescope.builtin))

(local {:cache-prefix hotpot-cache-prefix} (lazyfunc :hotpot.api.cache))

;; https://github.com/nvim-telescope/telescope.nvim/issues/938#issuecomment-916688222
(fn override-opts [opts]
  (->> {:layout_strategy :bottom_pane
        :layout_config {:prompt_position :bottom}}
       (merge (or opts {}))))

(fn builtin? [_ key]
  (match (. telescope-builtin key)
    nil (error "Invalid key, please check :h telescope.builtin")
    picker (fn [opts]
             (-> opts override-opts picker))))

(local builtin (setmetatable {} {:__index builtin?}))

;; SEE: https://www.reddit.com/r/neovim/comments/p1xj92/make_telescope_git_files_revert_back_to_find
(fn project []
  (let [[ret _] (vim.fn.systemlist "git rev-parse --is-inside-work-tree")
        git? (= ret :true)]
    (if git? (builtin.git_files) (builtin.find_files {:cwd "%:h"}))))

(fn setup []
  (telescope.setup
    {:defaults {:cache_picker {:num_pickers 20}
                :prompt_prefix bangedorrunt.signs.prompt
                :selection_caret bangedorrunt.signs.prompt
                :borderchars bangedorrunt.border_alt
                :path_display [:truncate :smart]
                :winblend 0
                :sorting_strategy :descending
                :layout_strategy :cursor
                :layout_config {:height 0.35}
                :mappings {:i {:<ESC> close :<C-s> select_horizontal}}
                :file_ignore_patterns [:.git/ :node_modules/.* :alfred2/.*]}
     :extensions {:fzf {:fuzzy true
                        :override_generic_sorter false
                        :override_file_sorter true
                        :case_mode :smart_case}}})

  ;; Load extensions
  (run! load_extension [:fzf])

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
  (noremap n nowait :<Leader>hc
           `(builtin.find_files {:cwd (hotpot-cache-prefix) :hidden true})))

{: setup}
