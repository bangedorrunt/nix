(let [{: setup} (require :bufferline)]
  (setup {:options {:buffer_close_icon :x
          :modified_icon "+"
          :show_close_icon false
          :show_buffer_icons true
          :show_buffer_close_icons false
          :show_tab_indicators true
          :offsets [{:filetype :NvimTree :text "" :padding 1}]
          :always_show_bufferline true}}))
