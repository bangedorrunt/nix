(module plugins.nvim-tree
  {autoload {nvim-tree/events nvim-tree.events}
   require-macros [core.macros]})

(let! nvim_tree_ignore [".git/" "node_modules"])
(let! nvim_tree_gitignore 1)
(let! nvim_tree_auto_open 1)
(let! nvim_tree_auto_close 1)
(let! nvim_tree_follow 1)
(let! nvim_tree_auto_ignore_ft [:dashboard :startify])
(let! nvim_tree_indent_markers 1)
(let! nvim_tree_git_hl 1)
(let! nvim_tree_disable_netrw 0)
(let! nvim_tree_lsp_diagnostics 1)

(nvim-tree/events.on_nvim_tree_ready #(vim.cmd "NvimTreeRefresh"))

(noremap! [n] "<Leader>tt" "<Cmd>NvimTreeToggle<CR>" :nowait)
