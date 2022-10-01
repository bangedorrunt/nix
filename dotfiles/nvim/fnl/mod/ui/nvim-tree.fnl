(import-macros {: noremap : lazyreq : lazyfunc} :core.macros)

(local tree (lazyreq :nvim-tree))
(local {:on_nvim_tree_ready on-ready} (lazyfunc :nvim-tree.events))

(fn setup []
  (tree.setup {:hijack_cursor true
               :hijack_netrw true
               :sync_root_with_cwd true
               :update_focused_file {:enable true :update_root true}
               :respect_buf_cwd true
               :renderer {:icons {:show {:git false
                                         :folder true
                                         :folder_arrow false
                                         :file true}
                                  :glyphs {:default ""
                                           :symlink ""
                                           :bookmark ""
                                           :folder {:default ""
                                                    :open ""
                                                    :empty ""
                                                    :empty_open ""}}}}}
              :view {:width 30 :side :left})

  (on-ready #(vim.cmd :NvimTreeRefresh))

  (noremap n :<Leader>tt :<Cmd>NvimTreeToggle<CR>))

{: setup}
