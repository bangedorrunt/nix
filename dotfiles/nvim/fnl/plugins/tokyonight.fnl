(module plugins.tokyonight
  {autoload {tokyonight tokyonight}
  require-macros [core.macros]})

(let! tokyonight_style "night"
      tokyonight_cterm_colors false
      tokyonight_italic_comments true
      tokyonight_italic_keywords true
      tokyonight_italic_functions true
      tokyonight_italic_variables true
      tokyonight_sidebars ["NvimTree" "qf" "vista_kind" "terminal" "packer" "spectre_panel" "NeogitStatus"]
      tokyonight_hide_inactive_statusline true
      tokyonight_dark_sidebar false 
      tokyonight_dark_float false)

(tokyonight.colorscheme)
