(local wezterm (require :wezterm))
(local {: colors
        : window_frame} (require :themer.rose-pine))

(fn is_windows? []
  (not= "/" (package.config:sub 1 1)))

{:default_prog (if (is_windows?)
                 ["wsl"]
                 ["/usr/local/bin/zsh"])
 :default_cwd (if (is_windows?)
                "\\\\wsl$\\Ubuntu\\home\\babygau"
                "~")
 :colors (colors)
 :window_frame (window_frame)
 :font (wezterm.font_with_fallback
         [{:family "OperatorMonoSSm Nerd Font" :weight "Light"}
          ;; {:family "Iosevka Nerd Font Mono" :weight "Regular"}
          {:family "codicon" :weight "Regular"
           :use_cap_height_to_scale_fallback_fonts true
           :scale 0.8}
          {:family "devicon" :weight "Regular"}])
 :font_size 19.5
 :freetype_load_target :Light
 :foreground_text_hsb {:hue 1.0 :saturation 1.0 :brightness 1.1}
 :line_height 1.5
 :cell_width 0.85
 :enable_tab_bar true
 :enable_scroll_bar false
 :exit_behavior "Close"
 :window_close_confirmation "NeverPrompt"
 :window_padding {:left :1cell
                  :right :1cell
                  :top :0.5cell
                  :bottom :0.5cell}
 :enable_csi_u_key_encoding true
 :adjust_window_size_when_changing_font_size false
 ;; TODO: custom keybindings
 ;; :disable_default_key_bindings true
}
