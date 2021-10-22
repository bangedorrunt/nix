(module colorschemes.rosepine
  {require-macros [core.macros]})

; NOTE:
; light: #faf4ed
; dark:  #191724
; (g rose_pine_variant :dawn)
(g rose_pine_disable_background false)
(g rose_pine_disable_italics false)
(g rose_pine_bold_vertical_split_line true)

(color rose-pine)

(hi NvimInternalError {:guibg :None})
; TODO: edit nvim-cmp highlight
; (hi Pmenu {:guibg "#191724" :guifg "#56949f"})
; (hi PmenuSel {:guifg "#56949f"})
; (hi PmenuThumb {:guibg "#191724"})
; (hi CmpItemAbbr {:guibg "#191724" :guifg "#56949f"})
; (hi CmpItemMenu {:guibg "#191724" :guifg "#56949f"})
; (hi CmpItemKind {:guibg "#191724" :guifg "#56949f"})
; (hi CmpItemAbbr {:guibg "#191724" :guifg "#56949f"})
; (hi CmpItemAbbrDeprecated {:guibg "#191724" :guifg "#907aa9"})
; (hi CmpItemAbbrMatch {:guibg "#191724" :guifg "#907aa9"})
; (hi CmpItemAbbrMatchFuzzy {:guibg "#191724" :guifg "#907aa9"})

(noremap n :<Leader>tc "<Cmd>lua require('rose-pine.functions').toggle_variant({'base', 'dawn'})<CR>")
