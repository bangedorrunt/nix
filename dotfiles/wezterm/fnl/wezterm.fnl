(local {: font_with_fallback
        : home_dir
        : target_triple} (require :wezterm))
(local {: colors
        : window_frame} (require :themer.rose-pine-moon))

(fn is_macos? [] (= target_triple :x86_64-apple-darwin))

{:term :wezterm
 :default_prog (if (is_macos?)
                 ["/usr/local/bin/zsh" "-l" "-c" "tmux attach || tmux"]
                 ["wsl.exe" "zsh" "-c" "-l" "tmux attach || tmux"])
 :default_cwd home_dir
 :colors (colors)
 :window_frame (window_frame)
 :font (font_with_fallback
         [{:family "OperatorMonoSSm Nerd Font" :weight "Light"}
          {:family "codicon" :weight "Regular" :scale 0.95}
          {:family "devicon" :weight "Regular"}])
 :font_size 22
 :use_cap_height_to_scale_fallback_fonts true
 :bold_brightens_ansi_colors: true
 :freetype_load_target :Light
 :foreground_text_hsb {:hue 1.0 :saturation 1.0 :brightness 1.0}
 :line_height 1.5
 :cell_width 0.85
 :window_decorations :RESIZE
 :enable_tab_bar true
 :hide_tab_bar_if_only_one_tab true
 :enable_scroll_bar false
 :exit_behavior "Close"
 :window_close_confirmation "NeverPrompt"
 :window_padding {:left :1cell
                  :right :1cell
                  :top :0.5cell
                  :bottom :0.5cell}
 :enable_csi_u_key_encoding true
 :adjust_window_size_when_changing_font_size false
 :check_for_updates false
}
