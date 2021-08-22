(module plugins.nvim-tree
  {autoload {nvim-tree/events nvim-tree.events}})

(import-macros {: let! : nmap! : noremap!} :core.macros)

(let! nvim_tree_ignore [".git/" "node_modules"]
      nvim_tree_gitignore 1
      nvim_tree_auto_open 1
      nvim_tree_auto_close 1
      nvim_tree_follow 1
      nvim_tree_auto_ignore_ft [:dashboard :startify]
      nvim_tree_indent_markers 1
      nvim_tree_git_hl 1
      nvim_tree_disable_netrw 0
      nvim_tree_lsp_diagnostics 1)

(nvim-tree/events.on_nvim_tree_ready #(vim.cmd "NvimTreeRefresh"))

(noremap! [n] "<Leader>tt" "<Cmd>NvimTreeToggle<CR>" :nowait)
