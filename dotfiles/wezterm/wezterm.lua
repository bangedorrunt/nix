local wezterm = require 'wezterm'

local function fallback(name)
  return wezterm.font_with_fallback { name, 'SF Compact Text', 'Symbols Nerd Font 1000-em' }
end

local function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'Tokyo Night'
  else
    return 'Catppuccin Latte'
  end
end

local function is_macos()
  if wezterm.target_triple == 'x86_64-apple-darwin' then
    return { '/run/current-system/sw/bin/fish', '-l', '-c', 'tmux attach -d || tmux' }
  else
    return { 'wsl.exe', 'zsh', '-l', '-c', 'tmux attach -d || tmux' }
  end
end

return {
  term = 'wezterm',
  default_prog = is_macos(),
  default_cwd = wezterm.home_dir,
  front_end = 'WebGpu',
  color_scheme = scheme_for_appearance(wezterm.gui.get_appearance()),
  colors = { background = '#14141f' },
  font_dirs = { '/Users/brunetdragon/Library/Fonts' },
  font = fallback 'SF Mono',
  font_size = 24,
  freetype_load_target = 'Light',
  line_height = 1.2,
  cell_width = 0.85,
  underline_position = '-0.15cell',
  window_decorations = 'RESIZE',
  enable_tab_bar = true,
  hide_tab_bar_if_only_one_tab = true,
  exit_behavior = 'Close',
  window_close_confirmation = 'NeverPrompt',
  window_padding = { left = '1cell', right = '1cell', top = '0.5cell', bottom = '0.5cell' },
  adjust_window_size_when_changing_font_size = false,
  check_for_updates = false,
  enable_scroll_bar = false,
}
