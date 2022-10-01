(import-macros {: lazyreq} :core.macros)
(local bufferline (lazyreq :bufferline))

(fn setup []
  (bufferline.setup {:options {:modified_icon "+"
                               :indicator {:style :underline}
                               :show_tab_indicators false
                               :show_close_icon false
                               :show_buffer_icons true
                               :show_buffer_close_icons false
                               :offsets [{:filetype :NvimTree :text "Neovim Tree"}]
                               :always_show_bufferline true}}))

{: setup}
