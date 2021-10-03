(module plugins.nvim-tree 
  {autoload {nvim aniseed.nvim
             : nvim-tree
             nvim-tree/events nvim-tree.events}
   require-macros [core.macros]})

;; fnlfmt: skip
(g nvim_tree_ignore [:.git/ :node_modules])
(g nvim_tree_gitignore 1)
(g nvim_tree_auto_ignore_ft [:dashboard :startify])
(g nvim_tree_indent_markers 0)
(g nvim_tree_git_hl 1)
(g nvim_tree_show_icons {:git 1
                           :folders 1
                           :files 1
                           :folder_arrows 1})
(g nvim_tree_icons {:default " "
                       :symlink " "
                       :git {:unstaged ""
                             :staged :S
                             :unmerged ""
                             :renamed ""
                             :deleted ""
                             :untracked :U
                             :ignored "◌"}
                       :lsp {:hint ""
                             :info ""
                             :warning ""
                             :error ""}
                       :folder {:arrow_open ""
                                :arrow_closed ""
                                :default ""
                                :open ""
                                :empty ""
                                :empty_open ""
                                :symlink ""}})
    
(nvim-tree.setup {})
(nvim-tree/events.on_nvim_tree_ready #(vim.cmd :NvimTreeRefresh))

(noremap [n] :<Leader>tt "<Cmd>NvimTreeToggle<CR>")
