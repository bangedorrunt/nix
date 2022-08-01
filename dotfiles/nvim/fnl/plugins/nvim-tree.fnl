(module plugins.nvim-tree
  {autoload {{: setup} nvim-tree
             {: on_nvim_tree_ready} nvim-tree.events}
   require-macros [core.macros]})

;; fnlfmt: skip
(setup {:hijack_cursor true
                  :update_cwd true
                  :update_focused_file {:enable true
                                        :update_cwd true}
                  :respect_buf_cwd true
                  :update_focused_file {:enable true :update_cwd true}
                  :renderer {:icons {:show {:git false
                                            :folder true
                                            :folder_arrow false
                                            :file false}
                                     :glyphs {:default ""
                                              :symlink ""
                                              :folder {:default ""
                                                       :open ""
                                                       :empty ""
                                                       :empty_open ""
                                                       :symlink ""}}}}
                  :view {:width 30 :side :left :adaptive_size true}})

(on_nvim_tree_ready #(vim.cmd :NvimTreeRefresh))

(noremap n :<Leader>tt :<Cmd>NvimTreeToggle<CR>)
