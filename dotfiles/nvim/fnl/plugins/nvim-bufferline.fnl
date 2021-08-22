(module plugins.nvim-bufferline
  {autoload {bufferline bufferline}})

(bufferline.setup {:options {:buffer_close_icon :x
                             :modified_icon "+"
                             :indicator_icon "▎"
                             :show_close_icon false
                             :show_buffer_close_icons false
                             :show_tab_indicators true
                             :mappings false
                             :always_show_bufferline true}})
