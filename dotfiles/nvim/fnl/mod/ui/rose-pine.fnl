(import-macros {: lazyreq} :core.macros)

(local rose-pine (lazyreq :rose-pine))

(fn setup []
  (rose-pine.setup {:dark_variant :moon
                    :bold_vert_split false
                    :dim_nc_background false
                    :disable_background true
                    :disable_float_background false
                    :disable_italics false})

  (vim.cmd.colorscheme :rose-pine))

{: setup}
