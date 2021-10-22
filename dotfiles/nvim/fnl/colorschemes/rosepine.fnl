(module colorschemes.rosepine
  {require-macros [core.macros]})

; NOTE:
;; light: #faf4ed
;; dark:  #191724

; (g rose_pine_variant :dawn)
(g rose_pine_disable_background false)
(g rose_pine_disable_italics false)
(g rose_pine_bold_vertical_split_line true)

(colorscheme rose-pine)

(hi NvimInternalError {:guibg :None})

(hi CmpItemAbbr {:guifg "#c0caf5"})
(hi CmpItemMenu {:guifg "#db4b4b"})
(hi CmpItemKind {:guifg "#f7768e"})
(hi CmpItemAbbrDeprecated {:guifg "#3b4261"})
(hi CmpItemAbbrMatch {:guifg "#73daca"})
(hi CmpItemAbbrMatchFuzzy {:guifg "#73daca"})
(hi CmpDocumentation {:guifg "#c0caf5"})
(hi CmpDocumentationBorder {:guifg "#c0caf5" :guibg "#191724"})

(noremap n :<Leader>tc "<Cmd>lua require('rose-pine.functions').toggle_variant({'base', 'dawn'})<CR>")
