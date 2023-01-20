(import-macros {: noremap : setup!} :core.macros)

(fn setup []
  (let [{:on_nvim_tree_ready on-ready} (require :nvim-tree.events)]
    (setup! nvim-tree
            {:hijack_cursor true
             :hijack_netrw true
             :sync_root_with_cwd true
             :update_focused_file {:enable true :update_root true}
             :respect_buf_cwd true
             :renderer {:icons {:webdev_colors false
                                :show {:git false
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

    (noremap n :<Leader>tt :<Cmd>NvimTreeToggle<CR>)))

{: setup}
