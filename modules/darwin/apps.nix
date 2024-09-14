{
  config,
  lib,
  pkgs,
  ...
}: {
  homebrew = {
    casks = [
      "alt-tab"
      "aerospace"
      "betterdiscord-installer"
      "discord"
      "element@nightly"
      "iina"
      "karabiner-elements"
      "microsoft-edge@beta"
      "openkey"
      "via"
      "wezterm@nightly"
    ];
  };
}
