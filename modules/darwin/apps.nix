{
  config,
  lib,
  pkgs,
  ...
}: {
  homebrew = {
    casks = [
      "betterdiscord-installer"
      "bitwarden"
      "discord"
      "element-nightly"
      "firefox-developer-edition"
      "iina"
      "karabiner-elements"
      "microsoft-edge-beta"
      "neovide"
      "openkey"
      "via"
      "visual-studio-code-insiders"
      "wezterm-nightly"
    ];
  };
}
