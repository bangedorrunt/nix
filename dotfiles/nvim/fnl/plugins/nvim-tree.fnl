(module plugins.nvim-tree
  {autoload {: nvim-tree
             nvim-tree/events nvim-tree.events}
   require-macros [core.macros]})

; fnlfmt: skip
(g nvim_tree_icons {:default "●"
                    :symlink " "
                    :git {:unstaged ""
                          :staged :S
                          :unmerged ""
                          :renamed ""
                          :deleted ""
                          :untracked :U
                          :ignored "◌"}
                    :folder {:arrow_open ""
                             :arrow_closed ""
                             :default ""
                             :open ""
                             :empty ""
                             :empty_open ""
                             :symlink ""}})

; fnlfmt: skip
(nvim-tree.setup {:auto_close true
                  :hijack_cursor true
                  :update_cwd true
                  :update_focused_file {:enable true
                                        :update_cwd true}
                  :gitignore true
                  :ignore [:.git/ :node_modules]
                  :auto_ignore_ft [:dashboard :startify]
                  :diagnostics {:enable true
                                :hint " "
                                :info " "
                                :warning " "
                                :error " "}
                  :view {:width 35
                         :side :left
                         :auto_resize true}})

(nvim-tree/events.on_nvim_tree_ready #(vim.cmd :NvimTreeRefresh))

(noremap n :<Leader>tt :<Cmd>NvimTreeToggle<CR>)
