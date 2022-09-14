(local {: setup} (require :rose-pine))

(setup {:dark_variant :moon
        :bold_vert_split true
        :dim_nc_background false
        :disable_background false
        :disable_float_background false
        :disable_italics false})

(vim.cmd.colorscheme :rose-pine)
