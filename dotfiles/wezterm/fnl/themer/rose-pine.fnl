(local palette {:base "#191724"
                :overlay "#26233a"
                :muted "#6e6a86"
                :text "#e0def4"
                :love "#eb6f92"
                :gold "#f6c177"
                :rose "#ebbcba"
                :pine "#31748f"
                :foam "#9ccfd8"
                :iris "#c4a7e7"
                :highlight_high "#524f67"})

(local active-tab {:bg_color palette.overlay :fg_color palette.text})

(local inactive-tab {:bg_color palette.base :fg_color palette.muted})

(fn colors []
  {:foreground palette.text
   :background palette.base
   :cursor_bg palette.highlight_high
   :cursor_border palette.highlight_high
   :cursor_fg palette.text
   :selection_bg "#2a283e"
   :selection_fg palette.text
   :ansi [palette.overlay
          palette.love
          palette.pine
          palette.gold
          palette.foam
          palette.iris
          palette.rose
          palette.text]
   :brights [palette.muted
             palette.love
             palette.pine
             palette.gold
             palette.foam
             palette.iris
             palette.rose
             palette.text]
   :tab_bar {:background palette.base
             :active_tab active-tab
             :inactive_tab inactive-tab
             :inactive_tab_hover active-tab
             :new_tab inactive-tab
             :new_tab_hover active-tab
             :inactive_tab_edge palette.muted}})

(fn window_frame []
  {:active_titlebar_bg palette.base
   :inactive_titlebar_bg palette.base})

{: colors
 : window_frame
}
