(module plugins.nvim-tree 
  {autoload {icons core.icons
             nvim-tree/events nvim-tree.events}
   require-macros [core.macros]})

(def- icontab icons.tab)
;; fnlfmt: skip
(let! nvim_tree_ignore [:.git/ :node_modules]
      nvim_tree_gitignore 1
      nvim_tree_auto_open 1
      nvim_tree_auto_close 1
      nvim_tree_follow 1
      nvim_tree_auto_ignore_ft [:dashboard :startify]
      nvim_tree_indent_markers 0
      nvim_tree_git_hl 1
      nvim_tree_disable_netrw 1
      nvim_tree_lsp_diagnostics 1
      nvim_tree_show_icons {:git 1
                           :folders 1
                           :files 1
                           :folder_arrows 1
                           :tree_width 30}
      nvim_tree_icons {:default " "
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

(nvim-tree/events.on_nvim_tree_ready #(vim.cmd :NvimTreeRefresh))

(noremap! [n] :<Leader>tt :<Cmd>NvimTreeToggle<CR> :nowait)
