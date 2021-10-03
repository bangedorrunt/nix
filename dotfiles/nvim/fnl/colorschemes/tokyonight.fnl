(module colorschemes.tokyonight
  {autoload {: tokyonight}
   require-macros [core.macros]})

(g tokyonight_style "night")
(g tokyonight_day_brightness 0.5)
(g tokyonight_transparent false)
(g tokyonight_cterm_colors true)
(g tokyonight_italic_comments true)
(g tokyonight_italic_keywords true)
(g tokyonight_italic_functions true)
(g tokyonight_italic_variables true)
(g tokyonight_sidebars ["NvimTree" "qf" "vista_kind" "terminal" "packer" "spectre_panel" "NeogitStatus"])
(g tokyonight_hide_inactive_statusline false)
(g tokyonight_dark_sidebar true)
(g tokyonight_dark_float true)
(g tokyonight_lualine_bold true)

(tokyonight.colorscheme)

(hi NvimInternalError {:bg :None})
