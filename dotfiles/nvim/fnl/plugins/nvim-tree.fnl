(import-macros {: nmap : noremap} :core.macros)

(local {: setup} (require :nvim-tree))
(local {: on_nvim_tree_ready} (require :nvim-tree.events))

(setup {:hijack_cursor true
        :update_cwd true
        :update_focused_file {:enable true
                              :update_cwd true}
        :respect_buf_cwd true
        :update_focused_file {:enable true :update_cwd true}
        :renderer {:icons {:show {:git false
                                  :folder true
                                  :folder_arrow false
                                  :file true}}}}
        :view {:width 30 :side :left})

(on_nvim_tree_ready #(vim.cmd :NvimTreeRefresh))

(noremap n :<Leader>tt :<Cmd>NvimTreeToggle<CR>)
