;;; tools/tree-sitter/config.el -*- lexical-binding: t; -*-

(use-package! tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
