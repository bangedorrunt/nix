(local {: font_with_fallback : font : home_dir : target_triple : gui}
       (require :wezterm))

(fn is_macos? []
  (= target_triple :x86_64-apple-darwin))

;; fnlfmt: skip
(fn fallback [name]
  (font_with_fallback [name
                       :neorg
                       "Symbols Nerd Font 1000-em"
                       ]))

(fn scheme-for-appearance [appearance]
  (if (appearance:find :Dark)
    "Catppuccin Mocha"
    "Catppuccin Latte"))

{:term :wezterm
 :default_prog (if (is_macos?)
                   [:/usr/local/bin/fish :-l :-c "tmux attach -d || tmux"]
                   [:wsl.exe :zsh :-l :-c "tmux attach -d || tmux"])
 :default_cwd home_dir
 :color_scheme (scheme-for-appearance (gui.get_appearance))
 :window_background_opacity 0.95
 :font_dirs [:/Users/brunetdragon/Library/Fonts]
 :font (fallback {:family "Victor Mono" :weight :Medium})
 :font_size 21
 :freetype_load_target :Light
 ;; :line_height 1.4
 ;; :cell_width 0.85
 :underline_position :-0.15cell
 :window_decorations :RESIZE
 :enable_tab_bar true
 :hide_tab_bar_if_only_one_tab true
 :enable_scroll_bar false
 :exit_behavior :Close
 :window_close_confirmation :NeverPrompt
 :window_padding {:left :1cell :right :1cell :top :0.5cell :bottom :0.5cell}
 :enable_csi_u_key_encoding true
 :adjust_window_size_when_changing_font_size false
 :check_for_updates false}
