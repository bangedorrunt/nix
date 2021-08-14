(module plugins.tokyonight
        {autoload {tokyonight tokyonight}
         require-macros [core.macros]})

(let! tokyonight_style "night")
(let! tokyonight_cterm_colors false)
(let! tokyonight_italic_comments true)
(let! tokyonight_italic_keywords true)
(let! tokyonight_italic_functions true)
(let! tokyonight_italic_variables true)
(let! tokyonight_sidebars ["NvimTree" "qf" "vista_kind" "terminal" "packer" "spectre_panel" "NeogitStatus"])
(let! tokyonight_hide_inactive_statusline true)
(let! tokyonight_dark_sidebar false )
(let! tokyonight_dark_float false)
(tokyonight.colorscheme)
