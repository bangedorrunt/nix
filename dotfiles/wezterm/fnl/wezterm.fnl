(local {: font_with_fallback : font
        : home_dir : target_triple} (require :wezterm))
(local {: colors
        : window_frame} (require :themer.rose-pine-moon))

(fn is_macos? [] (= target_triple :x86_64-apple-darwin))

(fn fallback [name]
  (font_with_fallback
    [name
     {:family "codicon" :scale 0.95}
     :neorg
     :devicon]))

{:term :wezterm
 :default_prog (if (is_macos?)
                 ["/usr/local/bin/zsh" "-l" "-c" "tmux attach || tmux"]
                 ["wsl.exe" "zsh" "-c" "-l" "tmux attach || tmux"])
 :default_cwd home_dir
 :colors (colors)
 :window_frame (window_frame)
 :font (fallback {:family "Operator Mono SSm" :weight 325})
 :font_rules [{:italic true
               :font (fallback {:family "Operator Mono SSm" :weight 325 :style :Italic})}
              {:intensity :Bold
               :font (fallback {:family "Operator Mono SSm" :weight :DemiLight})}
              {:italic true
               :intensity :Bold
               :font (fallback {:family "Operator Mono SSm" :weight :DemiLight :style :Italic})}]
 :font_size 22
 :freetype_load_target :Light
 :line_height 1.2
 :cell_width 0.85
 :underline_position "-0.15cell"
 :window_decorations :RESIZE
 :enable_tab_bar true
 :hide_tab_bar_if_only_one_tab true
 :enable_scroll_bar false
 :exit_behavior :Close
 :window_close_confirmation "NeverPrompt"
 :window_padding {:left "1cell"
                  :right "1cell"
                  :top "0.5cell"
                  :bottom "0.5cell"}
 :enable_csi_u_key_encoding true
 :adjust_window_size_when_changing_font_size false
 :check_for_updates false
}
