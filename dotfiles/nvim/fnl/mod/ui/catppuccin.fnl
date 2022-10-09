(import-macros {: lazyreq : autocmd : g} :core.macros)

(g catppuccin_flavour :mocha)

(local catppuccin (lazyreq :catppuccin))
(local colors ((. (lazyreq :catppuccin.palettes) :get_palette)))

(set colors.none :NONE)

(fn setup []
  (catppuccin.setup {:transparent_background true
                     :custom_highlights {:Comment {:fg colors.overlay1}
                                         :LineNr {:fg colors.overlay1}
                                         :CursorLine {:bg colors.none}
                                         :CursorLineNr {:fg colors.lavender}
                                         :DiagnosticVirtualTextError {:bg colors.none}
                                         :DiagnosticVirtualTextWarn {:bg colors.none}
                                         :DiagnosticVirtualTextInfo {:bg colors.none}
                                         :DiagnosticVirtualTextHint {:bg colors.none}}})
  (vim.cmd.colorscheme :catppuccin))

{: setup}
