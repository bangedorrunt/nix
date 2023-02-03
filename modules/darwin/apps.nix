{
  config,
  lib,
  pkgs,
  ...
}: {
  homebrew = {
    casks = [
      "basictex"
      "betterdiscord-installer"
      "bitwarden"
      "discord"
      "element-nightly"
      "firefox-developer-edition"
      "iina"
      "keycastr"
      "maccy"
      "microsoft-edge-beta"
      "neovide"
      "openkey"
      "via"
      "visual-studio-code-insiders"
      "wezterm-nightly"
    ];
  };
}
